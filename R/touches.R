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
TOUCH_END   <- c("PASS", "FG", "TO", "FD")                      # events by the ball handler that end her touch

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
                      post = logical(0), pass_no = integer(0), stringsAsFactors = FALSE)
  if (!nrow(ev) || !any(ev$type %in% BALL_TYPES)) return(empty)
  out <- list()
  for (gid in unique(ev$game_id[ev$type %in% BALL_TYPES])) {
    e <- ev[ev$game_id == gid & ev$team == team & ev$type %in% c(TOUCH_START, TOUCH_END, "DRIVE", "POST"), ]
    e <- e[order(e$ord), ]
    cur <- NULL; passes <- 0; last_poss <- NA
    close_touch <- function(t_end, ended, poss = NA) {
      if (is.null(cur)) return()
      # a touch belongs to the possession its ending (pass, shot, turnover) is part of
      out[[length(out) + 1]] <<- data.frame(game_id = gid, poss_id = if (is.na(poss)) cur$poss else poss, player_id = cur$pid, how = cur$how,
        start = cur$start, secs = round((t_end - cur$start) / 1000, 1), ended = ended, drive = cur$drive,
        post = cur$post, pass_no = cur$pass_no, stringsAsFactors = FALSE)
      cur <<- NULL
    }
    for (i in seq_len(nrow(e))) {
      ty <- e$type[i]; pid <- e$player_id[i]; t <- e$ord[i]
      if (!identical(e$poss_id[i], last_poss)) { passes <- 0; last_poss <- e$poss_id[i] }
      if (ty == "DRIVE" && !is.null(cur)) { cur$drive <- TRUE; next }
      if (ty == "POST" && !is.null(cur)) { cur$post <- TRUE; next }
      if (ty == "PASS") {
        close_touch(t, "pass", e$poss_id[i]); passes <- passes + 1
        if (!is.na(e$target_id[i])) cur <- list(pid = e$target_id[i], how = "pass", start = t, drive = FALSE,
                                                post = FALSE, pass_no = passes, poss = e$poss_id[i])
      } else if (ty %in% c("FG", "TO", "FD")) {
        close_touch(t, if (ty == "FG") (if (e$made[i] %in% 1) "made shot" else "missed shot") else if (ty == "TO") "turnover" else "fouled",
                    e$poss_id[i])
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
               Fouled = sum(d$ended == "fouled"),
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
