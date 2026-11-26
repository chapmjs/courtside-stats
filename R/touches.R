# =============================================================================
# CourtSide Stats v7 - Ball tracker: touches, passes, time on the ball
# The Ball tracker job records one PASS (passer -> receiver, in target_id) per tap,
# plus TOUCH (the ball reached a player another way: inbound, after a foul),
# DRIVE and POST markers, and the shots, rebounds and turnovers that end touches.
# A touch runs from the moment a player gets the ball to her next ball event.
# =============================================================================

migrate_v7 <- function(con) {
  if (!"target_id" %in% dbGetQuery(con, "PRAGMA table_info(events)")$name)
    dbExecute(con, "ALTER TABLE events ADD COLUMN target_id INTEGER")
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '7')")
}

BALL_TYPES <- c("PASS", "TOUCH", "DRIVE", "POST")
TOUCH_START <- c("PASS", "TOUCH", "OREB", "DREB", "STL")       # events that put the ball in a player's hands
TOUCH_END   <- c("PASS", "FG", "TO", "FD", "DEFL")                      # events by the ball handler that end her touch

# Same play entered by two keepers (e.g. the Ball tracker and the scorer both record a
# made 2 by #12): keep the first when the copies are a few seconds apart from different devices.
drop_cross_keeper_dupes <- function(ev, window_ms = 6000) {
  types <- c("FG", "FT", "TO", "OREB", "DREB", "AST", "STL")
  k <- which(ev$type %in% types & !is.na(ev$session_id))
  if (length(k) < 2) return(ev)
  key <- paste(ev$game_id, ev$team, ev$type, ev$player_id, ev$made, ev$points)[k]
  drop <- integer(0)
  for (g in unique(key[duplicated(key)])) {
    ii <- k[key == g]; ii <- ii[order(ev$ord[ii])]
    for (a in seq_along(ii)[-1]) {
      prev <- ii[seq_len(a - 1)]; prev <- setdiff(prev, drop)
      near <- prev[abs(ev$ord[prev] - ev$ord[ii[a]]) <= window_ms & ev$session_id[prev] != ev$session_id[ii[a]]]
      if (length(near)) drop <- c(drop, ii[a])
    }
  }
  if (length(drop)) ev[-drop, ] else ev
}

# With no substitutions recorded, guess each play's five from who touched the ball most recently.
infer_lineups <- function(ev) {
  for (gid in unique(ev$game_id)) {
    idx <- which(ev$game_id == gid)
    if (!any(ev$type[idx] %in% BALL_TYPES) || any(ev$on_court[idx] != "")) next
    recent <- integer(0)
    for (i in idx) {
      who <- c(if (ev$team[i] == "us") ev$player_id[i], if (ev$type[i] == "PASS") ev$target_id[i])
      for (p in who[!is.na(who)]) recent <- c(p, setdiff(recent, p))
      if (length(recent) >= 5) ev$on_court[i] <- paste(head(recent, 5), collapse = ",")
    }
  }
  ev
}

# One row per touch for the team that was ball-tracked
touch_table <- function(ev, team = "us") {
  empty <- data.frame(game_id = integer(0), poss_id = integer(0), player_id = integer(0), how = character(0),
                      start = numeric(0), secs = numeric(0), ended = character(0), drive = logical(0),
                      post = logical(0), pass_no = integer(0), to_kind = character(0), stringsAsFactors = FALSE)
  if (!nrow(ev) || !any(ev$type %in% BALL_TYPES)) return(empty)
  out <- list()
  for (gid in unique(ev$game_id[ev$type %in% BALL_TYPES])) {
    e <- ev[ev$game_id == gid & ((ev$team == team & ev$type %in% c(TOUCH_START, TOUCH_END, "DRIVE", "POST")) | ev$type %in% c("TIMEOUT", "PEND")), ]
    e <- e[order(e$ord), ]
    cur <- NULL; passes <- 0; last_poss <- NA
    close_touch <- function(t_end, ended, poss = NA, kind = NA_character_) {
      if (is.null(cur)) return()
      # a touch belongs to the possession its ending (pass, shot, turnover) is part of
      out[[length(out) + 1]] <<- data.frame(game_id = gid, poss_id = if (is.na(poss)) cur$poss else poss, player_id = cur$pid, how = cur$how,
        start = cur$start, secs = round((t_end - cur$start) / 1000, 1), ended = ended, drive = cur$drive,
        post = cur$post, pass_no = cur$pass_no, to_kind = kind, stringsAsFactors = FALSE)
      cur <<- NULL
    }
    for (i in seq_len(nrow(e))) {
      ty <- e$type[i]; pid <- e$player_id[i]; t <- e$ord[i]
      if (!identical(e$poss_id[i], last_poss)) { passes <- 0; last_poss <- e$poss_id[i] }
      if (ty == "TIMEOUT") { close_touch(t, "timeout"); next }
      if (ty == "PEND") { close_touch(t, "end of period"); next }    # the horn: nothing after the break counts as her time      # the clock stops; she inbounds or gets it back after
      if (ty == "DRIVE" && !is.null(cur)) { cur$drive <- TRUE; next }
      if (ty == "POST" && !is.null(cur)) { cur$post <- TRUE; next }
      if (ty == "PASS") {
        close_touch(t, "pass", e$poss_id[i]); passes <- passes + 1
        if (!is.na(e$target_id[i])) cur <- list(pid = e$target_id[i], how = "pass", start = t, drive = FALSE,
                                                post = FALSE, pass_no = passes, poss = e$poss_id[i])
      } else if (ty %in% c("FG", "TO", "FD", "DEFL")) {
        close_touch(t, if (ty == "FG") (if (e$made[i] %in% 1) "made shot" else "missed shot") else if (ty == "TO") "turnover"
                       else if (ty == "DEFL") "deflected out" else "fouled",
                    e$poss_id[i], if (ty == "TO") (e$tag[i] %||% "unspecified") else NA_character_)
      } else if (ty %in% c("TOUCH", "OREB", "DREB", "STL") && !is.na(pid)) {
        close_touch(t, "other")
        cur <- list(pid = pid, how = c(TOUCH = e$tag[i] %||% "inbound", OREB = "off rebound", DREB = "def rebound",
                                        STL = "steal")[[ty]] %||% "inbound",
                    start = t, drive = FALSE, post = FALSE, pass_no = passes, poss = e$poss_id[i])
      }
    }
  }
  if (!length(out)) return(empty)
  tt <- do.call(rbind, out); tt$how[is.na(tt$how)] <- "inbound"; tt
}

# ---- Ball movement report tables ------------------------------------------------
ball_player_table <- function(ev, players) {
  tt <- touch_table(ev); if (!nrow(tt)) return(data.frame())
  lab <- player_labels(players); us <- ev[ev$team == "us", ]
  do.call(rbind, lapply(split(tt, tt$player_id), function(d) {
    p <- d$player_id[1]; fg <- us[us$type == "FG" & us$player_id %in% p, ]
    data.frame(Player = unname(lab[as.character(p)]), Touches = nrow(d),
               `Avg sec on ball` = round(mean(d$secs), 1), `Median sec` = round(stats::median(d$secs), 1),
               `Quick (<2s) %` = pct(sum(d$secs < 2), nrow(d)),
               Passes = sum(d$ended == "pass"), Drives = sum(d$drive), `Post-ups` = sum(d$post),
               Shots = sum(d$ended %in% c("made shot", "missed shot")), TO = sum(d$ended == "turnover"),
               Fouled = sum(d$ended == "fouled"), `Passes deflected out` = sum(d$ended == "deflected out"),
               `Pts per touch` = round(sum(fg$pts) / nrow(d), 2), check.names = FALSE)
  })) -> out
  out[order(-out$Touches), ]
}

ball_possession_table <- function(ev, names) {
  tt <- touch_table(ev); if (!nrow(tt)) return(list(passes = data.frame(), first = data.frame(), speed = data.frame()))
  pt <- poss_table(ev); pt <- pt[pt$offense == "us", ]
  per <- do.call(rbind, lapply(split(tt, tt$poss_id), function(d) {
    d <- d[order(d$start), ]
    first <- if (any(d$drive)) "drive" else if (any(d$post)) "post-up" else if (any(d$ended == "pass")) "passing only" else "no pass"
    data.frame(poss_id = d$poss_id[1], passes = sum(d$ended == "pass"), touches = nrow(d), first = first,
               to_shot = { s <- which(d$ended %in% c("made shot", "missed shot"))
                           if (length(s)) round((d$start[s[1]] + d$secs[s[1]] * 1000 - d$start[1]) / 1000, 1) else NA },
               how = d$how[1])
  }))
  per <- merge(per, pt[, c("poss_id", "points", "trans")], by = "poss_id")
  if (!nrow(per)) return(list(passes = data.frame(), first = data.frame(), speed = data.frame()))
  grp <- function(by, lv = NULL) { g <- per[[by]]; if (!is.null(lv)) g <- factor(g, lv)
    do.call(rbind, lapply(split(per, g, drop = TRUE), function(d)
      data.frame(Group = as.character(d[[by]][1]), Possessions = nrow(d), Points = sum(d$points),
                 PPP = round(mean(d$points), 2), `Scored %` = pct(sum(d$points > 0), nrow(d)), check.names = FALSE))) }
  per$pass_grp <- ifelse(per$passes >= 4, "4+", as.character(per$passes))
  pa <- grp("pass_grp", c("0", "1", "2", "3", "4+")); names(pa)[1] <- "Passes before the end"
  fi <- grp("first", c("drive", "post-up", "passing only", "no pass")); names(fi)[1] <- "Possession included"
  sp <- data.frame(Measure = c("Touches per possession", "Passes per possession", "Seconds from first touch to shot (avg)",
                               "Shots within 8 s of a rebound or steal"),
                   Value = c(round(mean(per$touches), 1), round(mean(per$passes), 1), round(mean(per$to_shot, na.rm = TRUE), 1),
                             sum(per$how %in% c("def rebound", "steal") & !is.na(per$to_shot) & per$to_shot <= 8)))
  list(passes = pa, first = fi, speed = sp)
}

pass_matrix <- function(ev, players) {
  p <- ev[ev$type == "PASS" & ev$team == "us" & !is.na(ev$target_id), ]
  if (!nrow(p)) return(data.frame())
  lab <- player_labels(players); short <- function(id) sub("^(#\\S+).*", "\\1", lab[as.character(id)])
  m <- as.data.frame.matrix(table(From = short(p$player_id), To = short(p$target_id)))
  data.frame(From = rownames(m), m, Total = rowSums(m), check.names = FALSE, row.names = NULL)
}

# Passes picked off (Ball tracker "Pass picked off"): passer -> who the pass was meant for
picked_off_table <- function(ev, players) {
  p <- ev[ev$team == "us" & ((ev$type == "TO" & ev$tag %in% "pass") | ev$type == "DEFL"), ]
  if (!nrow(p)) return(data.frame())
  lab <- player_labels(players); nm <- function(id) ifelse(is.na(id), "(not sure)", unname(lab[as.character(id)]))
  d <- stats::aggregate(list(a = as.numeric(p$type == "TO"), b = as.numeric(p$type == "DEFL")),
                        list(From = nm(p$player_id), To = nm(p$target_id)), sum)
  names(d) <- c("Passer", "Meant for", "Picked off (turnover)", "Deflected out (kept the ball)")
  d <- d[order(-d[[3]], -d[[4]]), ]; rownames(d) <- NULL; d
}

# ---- Turnovers (Ball tracker asks what kind) ---------------------------------------
TO_KINDS <- c(pass = "Pass picked off", stripped = "Stripped / lost dribble", badpass = "Bad pass out of bounds",
              travel = "Travel", dribble = "Double dribble / carry", offfoul = "Offensive foul / charge",
              violation = "3-sec / 5-sec / 10-sec / backcourt", shotclock = "Shot clock violation", oob = "Stepped out of bounds", other = "Other",
              unspecified = "Not specified", auto = "Not specified")
TO_LIVE <- c("pass", "stripped")          # the other team gets the ball in play: these feed their fast break

turnover_tables <- function(ev, players) {
  to <- ev[ev$type == "TO" & ev$team == "us", ]
  if (!nrow(to)) return(list(kind = data.frame(), player = data.frame(), context = data.frame()))
  kind <- ifelse(is.na(to$tag) | !(to$tag %in% names(TO_KINDS)), "unspecified", to$tag)
  lab <- player_labels(players)
  # what the other team did with the ball next: points on the possession after each turnover
  pt <- poss_table(ev); nxt_pts <- vapply(seq_len(nrow(to)), function(i) {
    k <- which(pt$game_id == to$game_id[i] & pt$poss_id > to$poss_id[i] & pt$offense == "opp")
    if (length(k)) pt$points[k[1]] else NA_real_ }, 0)
  by_kind <- do.call(rbind, lapply(split(seq_len(nrow(to)), factor(kind, unique(c(names(TO_KINDS)[names(TO_KINDS) %in% kind])))), function(i)
    data.frame(Kind = unname(TO_KINDS[kind[i[1]]]), `Live or dead ball` = if (kind[i[1]] %in% TO_LIVE) "Live" else if (kind[i[1]] %in% c("unspecified", "auto")) "?" else "Dead",
               Turnovers = length(i), `% of all` = pct(length(i), nrow(to)),
               `Their points next possession` = if (any(!is.na(nxt_pts[i]))) round(mean(nxt_pts[i], na.rm = TRUE), 2) else NA,
               check.names = FALSE)))
  by_kind <- by_kind[order(-by_kind$Turnovers), ]; rownames(by_kind) <- NULL
  tt <- touch_table(ev)
  by_pl <- do.call(rbind, lapply(split(seq_len(nrow(to)), ifelse(is.na(to$player_id), -1, to$player_id)), function(i) {
    pid <- to$player_id[i[1]]; d <- tt[tt$player_id %in% pid, ]; tk <- table(unname(TO_KINDS[kind[i]]))
    data.frame(Player = if (is.na(pid)) "(team)" else unname(lab[as.character(pid)]), Turnovers = length(i),
               Touches = if (nrow(d)) nrow(d) else NA, `TO per 100 touches` = if (nrow(d)) round(100 * length(i) / nrow(d), 1) else NA,
               `Live-ball` = sum(kind[i] %in% TO_LIVE), `Most common` = names(tk)[which.max(tk)], check.names = FALSE) }))
  by_pl <- by_pl[order(-by_pl$Turnovers), ]; rownames(by_pl) <- NULL
  tot <- tt[tt$ended == "turnover", ]
  ctx <- if (nrow(tot)) data.frame(Measure = c("Turnovers with Ball tracker timing", "Average seconds she had the ball first",
                                               "Within 1 second of the catch", "Held 5+ seconds first", "On the first touch of the possession",
                                               "After 3+ passes", "Right after a drive"),
    Value = c(nrow(tot), round(mean(tot$secs), 1), sum(tot$secs < 1), sum(tot$secs >= 5), sum(tot$pass_no == 0 & tot$how != "pass"),
              sum(tot$pass_no >= 3), sum(tot$drive))) else data.frame()
  list(kind = by_kind, player = by_pl, context = ctx)
}
