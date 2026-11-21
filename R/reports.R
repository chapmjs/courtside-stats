# =============================================================================
# CourtSide Stats v2 - report tables (data frames for the Reports tab)
# =============================================================================

fmt_shoot <- function(m, a) ifelse(a > 0, sprintf("%d-%d (%.0f%%)", m, a, 100 * m / a), "0-0")

game_summary <- function(ev, w, g, names) {
  tm <- team_metrics(ev, w); fl <- game_flow(ev, g)
  rows <- c("Pts off TO", "2nd Effort Pts", "Bench Pts", "Paint Pts", "Layup Pts", "3PT Pts", "FT Pts",
            "Pts/Shot", "PPP", "OREB%", "DREB%", "BH Error %", "Def Stops", "Stop %")
  d <- data.frame(Stat = rows, Us = sapply(rows, function(r) tm_get(tm, r, "Us")),
                  Opp = sapply(rows, function(r) tm_get(tm, r, "Opp")))
  d$Us <- as.character(d$Us); d$Opp <- as.character(d$Opp)
  if (!is.null(fl)) d <- rbind(d,
    data.frame(Stat = c("Largest lead", "Longest run", "Lead changes", "Ties"),
               Us = c(fl$big_us, paste0(fl$run_us, "-0"), fl$lead_changes, fl$ties),
               Opp = c(fl$big_opp, paste0(fl$run_opp, "-0"), fl$lead_changes, fl$ties)))
  names(d) <- c("Stat", names); rownames(d) <- NULL; d
}

period_scores <- function(ev, names) {
  pers <- period_order(ev$period); if (!length(pers)) return(data.frame())
  u <- sapply(pers, function(p) sum(ev$pts[ev$team == "us" & ev$period == p]))
  o <- sapply(pers, function(p) sum(ev$pts[ev$team == "opp" & ev$period == p]))
  df <- as.data.frame(rbind(c(u, sum(u)), c(o, sum(o))))
  names(df) <- c(ifelse(grepl("OT", pers), pers, paste0("P", pers)), "Final")
  cbind(Team = names, df)
}

shooting_by_period <- function(ev, names) {
  pers <- period_order(ev$period)
  do.call(rbind, lapply(pers, function(p) {
    d <- ev[ev$period == p, ]
    f <- function(s, t, three = FALSE) { x <- d[d$team == s & d$type == t & (!three | d$points %in% 3), ]
      fmt_shoot(sum(x$made %in% 1), nrow(x)) }
    out <- data.frame(Period = p, f("us", "FG"), f("us", "FG", TRUE), f("us", "FT"),
                      f("opp", "FG"), f("opp", "FG", TRUE), f("opp", "FT"))
    names(out) <- c("Period", paste(names[1], c("FG", "3PT", "FT")), paste(names[2], c("FG", "3PT", "FT"))); out
  }))
}

LEADER_CATS <- c(Scoring = "PTS", `Field goals made` = "FGM", `3-point FG made` = "3PM",
                 `Free throws made` = "FTM", `Total rebounds` = "REB", `Offensive rebounds` = "OREB",
                 `Defensive rebounds` = "DREB", `Charges taken` = "CHG", Assists = "AST",
                 `Assist attempts` = "AST Att", Steals = "STL", Turnovers = "TO", `Blocked shots` = "BLK",
                 `Second effort points` = "2nd Pts", `Points in the paint` = "Paint Pts",
                 `Defensive assists` = "Def AST", `Fouls drawn` = "FD", Fouls = "PF", Outlets = "Outlets",
                 `Outlets to transition` = "Outlet Push",
                 Productivity = "Productivity", `Plus/minus` = "+/-")

leaders_table <- function(bx, top = 3) {
  if (is.null(bx) || !nrow(bx)) return(data.frame())
  out <- lapply(names(LEADER_CATS), function(cat) {
    v <- bx[[LEADER_CATS[[cat]]]]; o <- order(-v); o <- o[v[o] > 0 | LEADER_CATS[[cat]] == "+/-"][seq_len(top)]
    cells <- sapply(seq_len(top), function(i) { j <- o[i]
      if (is.na(j)) "" else sprintf("%s (%s) %s", bx$Player[j], bx$TeamName[j], v[j]) })
    data.frame(Category = cat, `1st` = cells[1], `2nd` = cells[2], `3rd` = cells[3], check.names = FALSE)
  })
  do.call(rbind, out)
}

PERIOD_STATS <- c("Transition %", "Transition PPP", "PTS", "FGM", "FGA", "FG%", "3PM", "3PA", "3P%", "FTM", "FTA", "FT%", "OREB", "DREB",
                  "REB", "AST", "TO", "STL", "BLK", "PF", "Possessions", "PPP", "Pts off TO",
                  "2nd Effort Pts", "Paint Pts", "Bench Pts", "Def Stops")

period_comparison <- function(ev, w) {
  pers <- c(period_order(ev$period), "Game"); out <- data.frame(Stat = PERIOD_STATS)
  for (p in pers) {
    tm <- team_metrics(if (p == "Game") ev else ev[ev$period == p, ], w)
    u <- sapply(PERIOD_STATS, function(s) tm_get(tm, s, "Us")); o <- sapply(PERIOD_STATS, function(s) tm_get(tm, s, "Opp"))
    d <- round(u - o, 2); dir <- ifelse(PERIOD_STATS %in% c(LOWER_BETTER), -1, 1)
    dir[PERIOD_STATS %in% c("FGA", "3PA", "FTA", "Possessions")] <- 0     # volume: neutral
    lb <- if (p == "Game") "Game" else if (grepl("OT", p)) p else paste0("P", p)
    out[[paste(lb, "Us")]] <- u; out[[paste(lb, "Opp")]] <- o; out[[paste(lb, "+/-")]] <- d
    out[[paste0(".g_", lb)]] <- ifelse(is.na(d) | d == 0, 0, sign(d) * dir)
  }
  out
}

goal_period_data <- function(ev, w, goals, n_reg) {
  pers <- period_order(ev$period); if (!nrow(goals) || !length(pers)) return(NULL)
  tms <- lapply(pers, function(p) team_metrics(ev[ev$period == p, ], w))
  do.call(rbind, lapply(seq_len(nrow(goals)), function(i) {
    gl <- goals[i, ]; rate <- gl$metric %in% RATE_METRICS
    tg <- if (rate) gl$target else round(gl$target / n_reg, 2)
    do.call(rbind, lapply(seq_along(pers), function(k) {
      v <- tm_get(tms[[k]], gl$metric, gl$side)
      data.frame(Goal = factor(gl$label, goals$label), Period = factor(pers[k], pers), Value = v, Target = tg,
                 Met = cmp_ok(v, gl$comparator, tg))
    }))
  }))
}

goal_trend_data <- function(ev_all, w, goals, games) {
  gids <- games$game_id[games$game_id %in% ev_all$game_id]
  if (!length(gids) || !nrow(goals)) return(NULL)
  gl <- games[match(gids, games$game_id), ]; gl <- gl[order(gl$game_date, gl$game_id), ]
  lv <- paste(gl$game_date, gl$opponent)
  do.call(rbind, lapply(seq_len(nrow(gl)), function(k) {
    tm <- team_metrics(ev_all[ev_all$game_id == gl$game_id[k], ], w); e <- eval_goals(tm, goals)
    data.frame(Goal = factor(goals$label, goals$label), Game = factor(lv[k], lv), Value = e$Actual, Target = goals$target, Met = e$met)
  }))
}

game_log <- function(ev_all, w, goals, games) {
  g <- games[games$game_id %in% ev_all$game_id, ]; g <- g[order(g$game_date, g$game_id), ]
  if (!nrow(g)) return(data.frame())
  do.call(rbind, lapply(seq_len(nrow(g)), function(i) {
    ev <- ev_all[ev_all$game_id == g$game_id[i], ]; tm <- team_metrics(ev, w); v <- function(m, s) tm_get(tm, m, s)
    gl <- eval_goals(tm, goals)
    data.frame(Date = g$game_date[i], Opponent = g$opponent[i], Score = paste0(v("PTS", "Us"), "-", v("PTS", "Opp")),
               Result = ifelse(v("PTS", "Us") > v("PTS", "Opp"), "W", ifelse(v("PTS", "Us") < v("PTS", "Opp"), "L", "T")),
               Poss = v("Possessions", "Us"), PPP = v("PPP", "Us"), `Opp PPP` = v("PPP", "Opp"),
               `eFG%` = v("eFG%", "Us"), `Opp eFG%` = v("eFG%", "Opp"), `BH Err%` = v("BH Error %", "Us"),
               `OREB%` = v("OREB%", "Us"), `DREB%` = v("DREB%", "Us"), `2nd Pts` = v("2nd Effort Pts", "Us"),
               `Pts off TO` = v("Pts off TO", "Us"), Stops = v("Def Stops", "Us"),
               `Trans %` = v("Transition %", "Us"), `Trans PPP` = v("Transition PPP", "Us"),
               `Half-ct PPP` = v("Half-court PPP", "Us"),
               Goals = if (is.null(gl)) "" else paste0(sum(gl$met, na.rm = TRUE), "/", nrow(gl)), check.names = FALSE)
  }))
}

pbp_report <- function(ev, lab, names, scoring_only = FALSE) {
  if (!nrow(ev)) return(data.frame())
  ev$us_sc <- cumsum(ifelse(ev$team == "us", ev$pts, 0)); ev$op_sc <- cumsum(ifelse(ev$team == "opp", ev$pts, 0))
  m <- ev$us_sc - ev$op_sc
  lead <- sign(m); prevlead <- 0; note <- character(nrow(ev))
  for (i in seq_len(nrow(ev))) if (ev$pts[i] != 0) {
    if (m[i] == 0) note[i] <- "Tie"
    else if (prevlead != 0 && lead[i] != prevlead) note[i] <- "Lead change"
    if (lead[i] != 0) prevlead <- lead[i]
  }
  d <- data.frame(Per = ev$period, Clock = ev$clock, Team = names[ifelse(ev$team == "us", 1, 2)],
                  Event = describe_event(ev, lab, setNames(names, c("us", "opp"))),
                  Score = ifelse(ev$pts != 0, paste0(ev$us_sc, "-", ev$op_sc), ""),
                  Margin = ifelse(ev$pts != 0, sprintf("%+d", m), ""), Note = note, keep = ev$pts != 0)
  # end-of-period summary rows
  ends <- which(c(ev$period[-1] != ev$period[-nrow(ev)], TRUE)); rows <- list(); last <- 0
  for (e in ends) {
    rows[[length(rows) + 1]] <- d[(last + 1):e, ]
    p <- ev$period[e]; pe <- ev[ev$period == p, ]
    f <- function(s) { x <- pe[pe$team == s & pe$type == "FG", ]; fmt_shoot(sum(x$made %in% 1), nrow(x)) }
    rows[[length(rows) + 1]] <- data.frame(Per = p, Clock = "END", Team = "",
      Event = sprintf("END OF PERIOD %s - %s FG %s | %s FG %s", p, names[1], f("us"), names[2], f("opp")),
      Score = paste0(ev$us_sc[e], "-", ev$op_sc[e]), Margin = sprintf("%+d", m[e]), Note = "", keep = TRUE)
    last <- e
  }
  d <- do.call(rbind, rows)
  if (scoring_only) d <- d[d$keep, ]
  d$keep <- NULL; d
}


# ---- Possession analysis (transition vs half-court, queueing rates) ---------
phase_summary <- function(pt, names, by = "phase") {
  if (!nrow(pt)) return(data.frame())
  key <- interaction(pt$offense, pt[[by]], drop = TRUE, sep = "|")
  out <- do.call(rbind, lapply(split(seq_len(nrow(pt)), key), function(i) {
    d <- pt[i, ]; known <- d[!is.na(d$dur) & d$dur > 0, ]; tsec <- sum(known$dur)
    data.frame(Team = names[ifelse(d$offense[1] == "us", 1, 2)],
               Group = d[[by]][1], Possessions = nrow(d), Points = sum(d$points),
               PPP = round(sum(d$points) / nrow(d), 2), `Scored %` = pct(sum(d$points > 0), nrow(d)),
               `TO %` = pct(sum(d$had_to), nrow(d)),
               `Transition %` = if (by == "phase") NA else pct(sum(d$trans), nrow(d)),
               `Timed poss` = nrow(known), `Avg sec` = if (nrow(known)) round(tsec / nrow(known), 1) else NA,
               `mu (score/min)` = if (tsec > 0) round(60 * sum(known$points > 0) / tsec, 2) else NA,
               `theta (empty/min)` = if (tsec > 0) round(60 * sum(known$points == 0) / tsec, 2) else NA,
               check.names = FALSE)
  }))
  names(out)[2] <- if (by == "phase") "Phase" else "Possession started by"
  if (by == "phase") out$`Transition %` <- NULL
  out <- out[order(out$Team != names[1], out[[2]]), ]; rownames(out) <- NULL; out
}

outlet_table <- function(bx) {
  if (is.null(bx)) return(data.frame())
  b <- bx[bx$Outlets > 0, ]
  if (!nrow(b)) return(data.frame())
  data.frame(Player = b$Player, Team = b$TeamName, Outlets = b$Outlets, `To transition` = b$`Outlet Push`,
             `Push %` = pct(b$`Outlet Push`, b$Outlets), DREB = b$DREB, STL = b$STL,
             check.names = FALSE)[order(-b$Outlets), ]
}
