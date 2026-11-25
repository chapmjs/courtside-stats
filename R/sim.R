# =============================================================================
# CourtSide Stats v8 - Game simulator
# Possession-level Monte Carlo. Each simulated possession is a whole recorded
# possession, drawn at random (with replacement) from the team's logged games:
# its points, length, phase (transition / half-court) and how it ended stay
# together. Possessions are drawn by how they started (def rebound, turnover,
# made basket, other), and how one ends sets how the next one starts, using the
# start/end pattern seen in the logged games.
#
# Opponent adjustment: possessions against the chosen opponent are blended with
# possessions against every opponent. Against an opponent with n logged
# possessions, a draw comes from that opponent's games with weight n / (n + k).
# =============================================================================

SIM_K <- 60                                   # pseudo-possessions for the opponent blend
SIM_GROUPS <- c("dreb", "to", "made", "other")
SIM_GROUP_LABEL <- c(dreb = "After def rebound", to = "After turnover", made = "After made basket / FTs",
                     other = "Dead ball, jump ball, period start")

start_group <- function(reason) ifelse(reason %in% "def rebound", "dreb", ifelse(reason %in% "turnover", "to",
                                ifelse(reason %in% c("made basket", "after free throws"), "made", "other")))

# One row per possession with what the simulator needs, plus game time for the window checks
sim_possessions <- function(ev, games) {
  pt <- poss_table(ev); if (!nrow(pt)) return(NULL)
  pt <- pt[order(pt$game_id, pt$poss_id), ]
  g <- games[match(pt$game_id, games$game_id), ]
  pt$opponent <- g$opponent
  pt$grp <- start_group(pt$reason)
  pt$end <- ifelse(pt$points > 0, "made", ifelse(pt$had_to, "to", "miss"))
  # fill unknown lengths with the typical length for that side and phase
  med <- tapply(pt$dur, paste(pt$offense, pt$phase), stats::median, na.rm = TRUE)
  fill <- med[paste(pt$offense, pt$phase)]
  fill[is.na(fill)] <- ifelse(pt$phase[is.na(fill)] == "Transition", 8, 18)
  pt$dur_f <- ifelse(is.na(pt$dur) | pt$dur <= 0, fill, pt$dur)
  pt$timed <- !is.na(pt$dur) & pt$dur > 0
  # game time when each possession ends (regulation only), from the clock when there is one
  np <- ifelse(is.na(g$periods), 4, g$periods); plen <- ifelse(is.na(g$period_min), 8, g$period_min) * 60
  pidx <- period_index(pt$period, np)
  pt$reg <- !is.na(pidx) & pidx <= np
  pt$t_end <- NA_real_
  for (k in split(seq_len(nrow(pt)), paste(pt$game_id, pt$period))) {
    if (!pt$reg[k[1]]) next
    off0 <- (pidx[k[1]] - 1) * plen[k[1]]
    run <- off0 + cumsum(pt$dur_f[k])
    clk <- off0 + plen[k] - pt$start_secs[k] + pt$dur_f[k]
    pt$t_end[k] <- ifelse(!is.na(pt$start_secs[k]), clk, run)
  }
  pt$plen <- plen; pt$np <- np
  rownames(pt) <- NULL
  pt
}

# How the next possession starts, given how the previous one ended (other team's next possession)
next_start_matrix <- function(pt) {
  m <- matrix(0, 3, 4, dimnames = list(c("made", "to", "miss"), SIM_GROUPS))
  base <- rbind(made = c(0, 0, 9, 1), to = c(0, 9, 0, 1), miss = c(8, 0, 0, 2))   # prior counts
  if (!is.null(pt) && nrow(pt) > 1) {
    a <- pt[-nrow(pt), ]; b <- pt[-1, ]
    ok <- a$game_id == b$game_id & a$period == b$period & a$offense != b$offense
    if (any(ok)) { t <- table(factor(a$end[ok], rownames(m)), factor(b$grp[ok], SIM_GROUPS)); m <- m + unclass(t) }
  }
  m <- m + base
  m / rowSums(m)
}

# Indices of the possession pools: side x (opponent or all) x start group x phase
sim_model <- function(pt, opponent = NULL, k = SIM_K) {
  mk <- function(side, opp_only) {
    d <- which(pt$offense == side & (if (opp_only) pt$opponent %in% opponent else TRUE))
    lapply(setNames(SIM_GROUPS, SIM_GROUPS), function(gp) {
      i <- d[pt$grp[d] == gp]; if (!length(i)) i <- d       # no possessions started that way: use all of them
      list(all = i, trans = i[pt$trans[i]], half = i[!pt$trans[i]], p_trans = if (length(i)) mean(pt$trans[i]) else 0)
    })
  }
  n_opp <- if (is.null(opponent)) 0 else sum(pt$opponent %in% opponent)
  list(pt = pt, nxt = next_start_matrix(pt), w = n_opp / (n_opp + k), n_opp = n_opp,
       pools = list(us = list(all = mk("us", FALSE), opp = if (n_opp) mk("us", TRUE)),
                    opp = list(all = mk("opp", FALSE), opp = if (n_opp) mk("opp", TRUE))),
       plen = stats::median(pt$plen), np = stats::median(pt$np))
}

# Simulate n games. Levers (our offense only):
#   trans_shift: change in the chance a possession after a def rebound or turnover is a transition one
#   pace: multiplier on the length of our possessions
sim_games <- function(model, n = 2000, trans_shift = 0, pace = 1, keep_track = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pt <- model$pt; pts <- pt$points; dur <- pt$dur_f; trn <- pt$trans; endk <- pt$end
  nxt <- model$nxt; plen <- model$plen; np <- model$np
  draw <- function(side, gp) {
    pl <- if (!is.null(model$pools[[side]]$opp) && runif(1) < model$w) model$pools[[side]]$opp[[gp]] else model$pools[[side]]$all[[gp]]
    pr <- pl$p_trans
    if (side == "us" && gp %in% c("dreb", "to")) pr <- min(1, max(0, pr + trans_shift))
    cand <- if (runif(1) < pr) pl$trans else pl$half
    if (!length(cand)) cand <- pl$all
    if (!length(cand)) return(NA_integer_)
    cand[sample.int(length(cand), 1)]
  }
  res <- matrix(0, n, 4, dimnames = list(NULL, c("us", "opp", "poss_us", "poss_opp")))
  track <- vector("list", keep_track)
  for (s in seq_len(n)) {
    sc <- c(us = 0, opp = 0); pc <- c(us = 0, opp = 0); per <- 0; tl <- if (s <= keep_track) list() else NULL
    first <- sample(c("us", "opp"), 1)
    repeat {
      per <- per + 1; ot <- per > np
      if (ot && sc[["us"]] != sc[["opp"]]) break
      if (per > np + 3) { if (runif(1) < 0.5) sc[["us"]] <- sc[["us"]] + 1 else sc[["opp"]] <- sc[["opp"]] + 1; break }
      t <- if (ot) plen / 2 else plen; elapsed0 <- if (ot) np * plen + (per - np - 1) * plen / 2 else (per - 1) * plen
      side <- if (per %% 2 == 1) first else other(first); gp <- "other"
      while (t > 0) {
        i <- draw(side, gp); if (is.na(i)) break
        d <- dur[i] * (if (side == "us") pace else 1)
        t <- t - d
        sc[[side]] <- sc[[side]] + pts[i]; pc[[side]] <- pc[[side]] + 1
        if (!is.null(tl) && !ot) tl[[length(tl) + 1]] <- c(min(plen, plen - t) + elapsed0, pts[i], side == "us")
        gp <- SIM_GROUPS[sample.int(4, 1, prob = nxt[endk[i], ])]
        side <- other(side)
      }
      if (per >= np && sc[["us"]] != sc[["opp"]]) break
    }
    res[s, ] <- c(sc, pc)
    if (!is.null(tl)) { m <- do.call(rbind, tl); track[[s]] <- data.frame(sim = s, t_end = m[, 1], points = m[, 2],
                                                                           offense = ifelse(m[, 3] == 1, "us", "opp")) }
  }
  out <- as.data.frame(res); out$margin <- out$us - out$opp
  attr(out, "track") <- if (keep_track) do.call(rbind, track) else NULL
  out
}

sim_summary <- function(s, names, label = "") {
  data.frame(Scenario = label, `Win %` = round(100 * mean(s$margin > 0), 1),
             Score = sprintf("%.1f - %.1f", mean(s$us), mean(s$opp)),
             `Avg margin` = round(mean(s$margin), 1),
             `80% of margins between` = sprintf("%+d to %+d", as.integer(stats::quantile(s$margin, 0.1)),
                                                 as.integer(stats::quantile(s$margin, 0.9))),
             `Our poss` = round(mean(s$poss_us), 1), `Our PPP` = round(sum(s$us) / sum(s$poss_us), 2),
             `Their PPP` = round(sum(s$opp) / sum(s$poss_opp), 2), check.names = FALSE)
}

# ---- Validation 1: points in time windows, logged vs simulated --------------------
window_points <- function(tr, w_min, game_len) {
  # tr: data.frame(game, offense, t_end, points); returns points per (game, side, window)
  w <- w_min * 60; nwin <- floor(game_len / w); if (nwin < 1) return(NULL)
  tr <- tr[!is.na(tr$t_end) & tr$t_end <= nwin * w, ]
  win <- pmin(nwin, floor(pmax(tr$t_end - 1e-9, 0) / w) + 1)
  grid <- expand.grid(window = seq_len(nwin), offense = c("us", "opp"), game = unique(tr$game), stringsAsFactors = FALSE)
  agg <- stats::aggregate(list(points = tr$points), list(game = tr$game, offense = tr$offense, window = win), sum)
  grid <- merge(grid, agg, all.x = TRUE); grid$points[is.na(grid$points)] <- 0; grid
}

longest_drought <- function(tr, game_len) {
  # longest stretch (minutes) without a score, per game and side
  do.call(rbind, lapply(split(tr, list(tr$game, tr$offense), drop = TRUE), function(d) {
    t <- sort(c(0, d$t_end[d$points > 0], game_len))
    data.frame(game = d$game[1], offense = d$offense[1], drought = max(diff(t)) / 60)
  }))
}

window_check <- function(pt, sims, windows = c(1, 2, 3, 5, 7, 10)) {
  game_len <- stats::median(pt$plen * pt$np)
  obs <- pt[pt$reg & !is.na(pt$t_end), c("game_id", "offense", "t_end", "points")]; names(obs)[1] <- "game"
  tr <- attr(sims, "track"); if (is.null(tr) || !nrow(obs)) return(NULL)
  names(tr)[names(tr) == "sim"] <- "game"
  rows <- list(); dist <- list()
  for (w in windows) {
    o <- window_points(obs, w, game_len); s <- window_points(tr, w, game_len)
    if (is.null(o) || is.null(s)) next
    for (sd in c("us", "opp")) {
      a <- o$points[o$offense == sd]; b <- s$points[s$offense == sd]
      rows[[length(rows) + 1]] <- data.frame(side = sd, `Window (min)` = w, `Windows logged` = length(a),
        `Logged avg pts` = round(mean(a), 2), `Sim avg pts` = round(mean(b), 2),
        `Logged SD` = round(stats::sd(a), 2), `Sim SD` = round(stats::sd(b), 2),
        `Logged scoreless %` = pct(sum(a == 0), length(a)), `Sim scoreless %` = pct(sum(b == 0), length(b)),
        check.names = FALSE)
      dist[[length(dist) + 1]] <- rbind(data.frame(window = w, side = sd, source = "Logged games", points = a),
                                        data.frame(window = w, side = sd, source = "Simulated", points = b))
    }
  }
  dr_o <- longest_drought(obs, game_len); dr_s <- longest_drought(tr, game_len)
  drought <- do.call(rbind, lapply(c("us", "opp"), function(sd) data.frame(side = sd,
    `Logged: longest scoreless stretch (min, avg per game)` = round(mean(dr_o$drought[dr_o$offense == sd]), 1),
    `Simulated` = round(mean(dr_s$drought[dr_s$offense == sd]), 1), check.names = FALSE)))
  # calibration check: how long the logged games look from the possession lengths alone
  len <- stats::aggregate(list(t = pt$dur_f[pt$reg]), list(game = pt$game_id[pt$reg]), sum)
  list(table = do.call(rbind, rows), dist = do.call(rbind, dist), drought = drought,
       game_len = game_len / 60, logged_len = round(stats::median(len$t) / 60, 1),
       pct_timed = pct(sum(pt$timed), nrow(pt)))
}

# ---- Validation 2: predict each logged game from the others ------------------------
holdout_check <- function(pt, n = 400, min_poss = 20, progress = NULL) {
  ids <- unique(pt$game_id); out <- list()
  for (j in seq_along(ids)) {
    gid <- ids[j]; test <- pt[pt$game_id == gid, ]
    if (nrow(test) < min_poss || length(ids) < 3) next
    train <- pt[pt$game_id != gid, ]
    if (!any(train$offense == "us") || !any(train$offense == "opp")) next
    m <- sim_model(train, test$opponent[1]); s <- sim_games(m, n)
    act_us <- sum(test$points[test$offense == "us"]); act_op <- sum(test$points[test$offense == "opp"])
    am <- act_us - act_op; q <- stats::quantile(s$margin, c(0.1, 0.9))
    out[[length(out) + 1]] <- data.frame(game_id = gid, Opponent = test$opponent[1],
      `Actual` = sprintf("%d - %d", as.integer(act_us), as.integer(act_op)),
      `Predicted` = sprintf("%.0f - %.0f", mean(s$us), mean(s$opp)),
      `Actual margin` = am, `80% range` = sprintf("%+d to %+d", as.integer(q[1]), as.integer(q[2])),
      `Inside?` = ifelse(am >= q[1] & am <= q[2], "yes", "no"),
      `Win % (pred)` = round(100 * mean(s$margin > 0)), Won = am > 0,
      pit = mean(s$margin < am) + 0.5 * mean(s$margin == am),
      `Games vs this opp in fit` = length(unique(train$game_id[train$opponent %in% test$opponent[1]])),
      check.names = FALSE)
    if (is.function(progress)) progress(j / length(ids))
  }
  if (!length(out)) return(NULL)
  d <- do.call(rbind, out); rownames(d) <- NULL
  p <- d$`Win % (pred)` / 100
  list(games = d, summary = data.frame(
    Measure = c("Games tested", "Actual margin inside the 80% range", "Average miss on the margin (points)",
                "Brier score for win chance (0 = perfect, 0.25 = coin flip)", "Games won / predicted wins"),
    Value = c(nrow(d), sprintf("%d of %d (%.0f%%; about 80%% means well calibrated)", sum(d$`Inside?` == "yes"), nrow(d),
                               100 * mean(d$`Inside?` == "yes")),
              round(mean(abs(d$`Actual margin` - (as.numeric(sub(" - .*", "", d$Predicted)) -
                                                     as.numeric(sub(".* - ", "", d$Predicted))))), 1),
              round(mean((p - d$Won)^2), 3), sprintf("%d / %.1f", sum(d$Won), sum(p)))))
}
