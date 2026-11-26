# =============================================================================
# CourtSide Stats v8 - Player development
# Touch-level process metrics from the Ball tracker (many per game, so they move
# with skill sooner than shooting percentages), shrunk toward the team average so
# a player with few touches isn't flagged on noise; per-game learning curves;
# development cards; and a practice / workout log.
# =============================================================================

migrate_v8 <- function(con) {
  dbExecute(con, "CREATE TABLE IF NOT EXISTS dev_log (log_id INTEGER PRIMARY KEY AUTOINCREMENT, team_id INTEGER,
                  log_date TEXT, player_id INTEGER, focus TEXT, minutes REAL, notes TEXT, created TEXT)")
  # games where nobody recorded the quarters: left out of quarter-by-quarter reports
  if (!"periods_known" %in% dbGetQuery(con, "PRAGMA table_info(games)")$name)
    dbExecute(con, "ALTER TABLE games ADD COLUMN periods_known INTEGER DEFAULT 1")
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '8')")
}

DEV_K <- 30            # pseudo-touches pulling a player's rate toward the team rate

# metric key -> label, direction (+1 higher is better), the skill it points to, and a drill focus to discuss with the coach
DEV_METRICS <- data.frame(
  key   = c("to100", "cta", "quick", "paint", "ahead", "shoot"),
  label = c("Turnovers per 100 touches", "Catch-to-action (sec)", "Quick decisions on the catch %",
            "Paint touches %", "Pass-ahead after rebound/steal %", "Touches ending in a shot %"),
  better = c(-1, -1, 1, 1, 1, 0),
  skill = c("Ball security", "Decision speed", "Decision speed", "Attacking the paint", "Push in transition", "Shot hunting"),
  drill = c("Two-ball handling, pressure passing (2-on-1 keep-away), jump-stop and pivot under pressure",
            "Catch-ready footwork, 0.5-second rule (shoot, drive or pass on the catch), closeout reads",
            "Catch-ready footwork, 0.5-second rule (shoot, drive or pass on the catch), closeout reads",
            "1-on-1 attacks from closeouts, drive-and-kick, post seals and duck-ins",
            "Rebound-to-outlet in 2 seconds, 3-lane pass-ahead drill, outlet and sprint",
            "Shot selection: game-speed shooting from her best spots"),
  stringsAsFactors = FALSE)
DEV_FOCUS <- c(unique(DEV_METRICS$skill), "Shooting", "Rebounding", "Defense", "Conditioning", "Team offense", "Other")

# per player per game counts from touches
dev_counts <- function(ev) {
  tt <- touch_table(ev); if (!nrow(tt)) return(data.frame())
  tt$catch <- tt$how == "pass"
  tt$push <- tt$how %in% c("def rebound", "steal")
  tt$cta <- pmin(tt$secs, 8)
  stats::aggregate(cbind(touches = 1, to = tt$ended == "turnover", catches = tt$catch,
                         cta_sum = ifelse(tt$catch, tt$cta, 0), cta_sq = ifelse(tt$catch, tt$cta^2, 0),
                         quick = tt$catch & tt$secs < 2, paint = tt$drive | tt$post | tt$paint,
                         push_n = tt$push, ahead = tt$push & tt$ended == "pass" & tt$secs <= 3,
                         shots = tt$ended %in% c("made shot", "missed shot")) ~ game_id + player_id, data = tt, FUN = sum)
}

# rate, its count base, and the team rate for each metric (rows of counts summed already)
dev_rates <- function(d, team) {
  r <- function(x, n, tx, tn, k = DEV_K) { tr <- if (tn > 0) tx / tn else NA
    list(raw = ifelse(n > 0, x / n, NA), shrunk = (x + k * tr) / (n + k), n = n, team = tr) }
  out <- list(
    to100 = r(d$to, d$touches, team$to, team$touches), quick = r(d$quick, d$catches, team$quick, team$catches),
    paint = r(d$paint, d$touches, team$paint, team$touches), ahead = r(d$ahead, d$push_n, team$ahead, team$push_n),
    shoot = r(d$shots, d$touches, team$shots, team$touches), cta = r(d$cta_sum, d$catches, team$cta_sum, team$catches))
  for (m in c("to100", "quick", "paint", "ahead", "shoot")) out[[m]][c("raw", "shrunk", "team")] <-
    lapply(out[[m]][c("raw", "shrunk", "team")], function(v) 100 * v)
  out
}

# Season card table: one row per player x metric
dev_cards <- function(ev, players) {
  cn <- dev_counts(ev); if (!nrow(cn)) return(data.frame())
  num <- setdiff(names(cn), c("game_id", "player_id"))
  team <- as.list(colSums(cn[num]))
  pl <- stats::aggregate(cn[num], list(player_id = cn$player_id), sum)
  pl$games <- as.vector(table(cn$player_id)[as.character(pl$player_id)])
  lab <- player_labels(players)
  cta_sd <- sqrt(max(team$cta_sq / max(team$catches, 1) - (team$cta_sum / max(team$catches, 1))^2, 0.01))
  rows <- list()
  for (i in seq_len(nrow(pl))) {
    rt <- dev_rates(pl[i, ], team)
    for (m in DEV_METRICS$key) {
      x <- rt[[m]]; md <- DEV_METRICS[DEV_METRICS$key == m, ]
      se <- if (m == "cta") cta_sd / sqrt(x$n + DEV_K) else {
        p <- x$team / 100; 100 * sqrt(max(p * (1 - p), 0.0025) / (x$n + DEV_K)) }
      z <- if (is.na(x$team) || md$better == 0) 0 else md$better * (x$shrunk - x$team) / se
      rows[[length(rows) + 1]] <- data.frame(player_id = pl$player_id[i], Player = unname(lab[as.character(pl$player_id[i])]),
        Games = pl$games[i], Touches = pl$touches[i], key = m, Metric = md$label, Based_on = x$n,
        Raw = round(x$raw, 1), Estimate = round(x$shrunk, 1), Team = round(x$team, 1), z = round(z, 2),
        Read = if (x$n < 5) "too few" else if (z >= 1) "Strength" else if (z <= -1) "Work on" else "About team average",
        Skill = md$skill, Drill = md$drill, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

dev_card_view <- function(cards, pid, log = NULL) {
  d <- cards[cards$player_id == pid, ]; if (!nrow(d)) return(NULL)
  tab <- data.frame(Metric = d$Metric, `Her estimate` = d$Estimate, `Team average` = d$Team,
                    `Raw (this player)` = d$Raw, `Based on` = d$Based_on, Read = d$Read, check.names = FALSE)
  needs <- d[d$Read == "Work on", ]; needs <- needs[order(needs$z), ]; needs <- needs[!duplicated(needs$Skill), ]
  strengths <- d[d$Read == "Strength", ]; strengths <- strengths[!duplicated(strengths$Skill), ]
  logged <- if (!is.null(log) && nrow(log)) {
    l <- log[is.na(log$player_id) | log$player_id == pid, ]
    if (nrow(l)) stats::aggregate(list(Minutes = l$minutes), list(Focus = l$focus), sum, na.rm = TRUE) else NULL }
  list(table = tab, needs = head(needs, 2), strengths = strengths, logged = logged,
       header = sprintf("%s: %d games, %d touches", d$Player[1], d$Games[1], d$Touches[1]))
}

# Learning curve: one metric per game for one player, with a weighted trend line
dev_curve <- function(ev, games, pid, metric) {
  cn <- dev_counts(ev); if (!nrow(cn)) return(NULL)
  num <- setdiff(names(cn), c("game_id", "player_id")); team <- as.list(colSums(cn[num]))
  d <- cn[cn$player_id == pid, ]; if (!nrow(d)) return(NULL)
  rt <- dev_rates(d, team)[[metric]]
  out <- data.frame(game_id = d$game_id, n = rt$n, value = rt$raw, team = rt$team)
  out$date <- as.Date(games$game_date[match(out$game_id, games$game_id)])
  out$opponent <- games$opponent[match(out$game_id, games$game_id)]
  out <- out[!is.na(out$value) & out$n > 0, ]; out <- out[order(out$date, out$game_id), ]
  out$game_no <- seq_len(nrow(out))
  trend <- NULL
  if (nrow(out) >= 3) {
    f <- stats::lm(value ~ game_no, data = out, weights = n); cf <- summary(f)$coefficients
    if (nrow(cf) >= 2) trend <- list(slope = cf[2, 1], se = cf[2, 2], fit = stats::fitted(f))
  }
  list(data = out, trend = trend)
}

dev_curve_plot <- function(cv, metric, log = NULL, focus = NULL) {
  d <- cv$data; md <- DEV_METRICS[DEV_METRICS$key == metric, ]
  p <- ggplot(d, aes(date, value)) +
    geom_hline(yintercept = d$team[1], linetype = "dashed", colour = "grey55") +
    geom_point(aes(size = n), colour = COL_US, alpha = 0.85) +
    scale_size_area(max_size = 7, name = if (metric %in% c("cta", "quick")) "Catches" else if (metric == "ahead") "Chances" else "Touches")
  if (!is.null(cv$trend)) p <- p + geom_line(data = transform(d, fit = cv$trend$fit), aes(date, fit), colour = COL_US, linewidth = 0.9)
  if (!is.null(log) && nrow(log) && !is.null(focus)) {
    l <- log[log$focus == focus & !is.na(log$log_date), ]
    if (nrow(l)) p <- p + geom_vline(xintercept = as.Date(l$log_date), colour = COL_OPP, alpha = 0.5)
  }
  p + theme_cs() + labs(x = NULL, y = md$label,
    caption = "Dashed line = team average. Orange lines = practice/workout log entries for this skill.")
}

dev_trend_text <- function(cv, metric) {
  if (is.null(cv$trend)) return("Needs at least 3 games with this stat to show a trend.")
  md <- DEV_METRICS[DEV_METRICS$key == metric, ]; s <- cv$trend$slope; se <- cv$trend$se
  dir <- if (md$better == 0) "" else if (sign(s) == md$better) " (improving)" else " (getting worse)"
  sure <- if (is.na(se) || se == 0) "" else if (abs(s / se) >= 2) "This trend is larger than game-to-game noise."
          else "Not yet distinguishable from game-to-game noise; more games will tell."
  sprintf("Trend: %+.2f per game%s. %s", s, dir, sure)
}
