# =============================================================================
# CourtSide Stats v4 - full-court entry, shared game clock, team fouls/bonus
# Shots are still stored in half-court coordinates (x = -25..25 across the
# court, y = 0..47 from the baseline), so every report works unchanged.
# Full-court coordinates: X = 0..94 along the court, Y = 0..50 across it.
# =============================================================================

migrate_v4 <- function(con) {
  cols <- dbGetQuery(con, "PRAGMA table_info(game_state)")$name
  add <- c(us_left = "INTEGER DEFAULT 1", clock_run = "INTEGER DEFAULT 0", clock_at = "REAL", clock_t = "REAL")
  for (c in names(add)) if (!c %in% cols) dbExecute(con, sprintf("ALTER TABLE game_state ADD COLUMN %s %s", c, add[[c]]))
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '4')")
}

# Events that don't belong to either offense (they never change the possession)
NEUTRAL_TYPES <- c("TIMEOUT", "ADJ", "SHOTLOC", "PEND")

# ---- side of the floor --------------------------------------------------------
second_half <- function(p, np) period_index(p, np) > np / 2
# TRUE when our team attacks the LEFT basket in period p (base = left in the 1st half)
us_left_in <- function(p, np, base_left) xor(isTRUE(base_left), second_half(p, np))

full_to_half <- function(X, Y) {
  if (X < 47) list(side = "left", x = Y - 25, y = X) else list(side = "right", x = 25 - Y, y = 94 - X)
}
half_to_full <- function(x, y, side) {
  left <- rep_len(side == "left", length(x))
  data.frame(X = ifelse(left, y, 94 - y), Y = ifelse(left, x + 25, 25 - x))
}

# ---- drawing ------------------------------------------------------------------
court_full_layers <- function(r = 19.75, corner = 19.75, lane = 12, lw = 0.5) {
  ln <- "#55534f"
  one_end <- function(side) {
    th0 <- if (corner < r) acos(corner / r) else 0
    paths <- list(
      lane = data.frame(x = c(-lane / 2, -lane / 2, lane / 2, lane / 2), y = c(0, 19, 19, 0)),
      ftc  = circ(0, 19, 6, 0, pi), ra = circ(0, 5.25, 4, 0, pi),
      arc  = rbind(data.frame(x = corner, y = 0), circ(0, 5.25, r, th0, pi - th0), data.frame(x = -corner, y = 0)),
      bb   = data.frame(x = c(-3, 3), y = c(4, 4)), rim = circ(0, 5.25, 0.75))
    lane_poly <- half_to_full(paths$lane$x, paths$lane$y, side)
    c(list(annotate("polygon", x = lane_poly$X, y = lane_poly$Y, fill = "#ecdcbc", colour = ln, linewidth = lw)),
      lapply(names(paths)[-1], function(n) { f <- half_to_full(paths[[n]]$x, paths[[n]]$y, side)
        annotate("path", x = f$X, y = f$Y, colour = ln,
                 linewidth = if (n %in% c("arc", "rim")) lw * 1.6 else if (n == "bb") lw * 2 else lw) }))
  }
  mid <- circ(47, 25, 6)
  c(list(annotate("rect", xmin = 0, xmax = 94, ymin = 0, ymax = 50, fill = "#f6ecd9", colour = ln, linewidth = lw),
         annotate("segment", x = 47, xend = 47, y = 0, yend = 50, colour = ln, linewidth = lw),
         annotate("path", x = mid$x, y = mid$y, colour = ln, linewidth = lw)),
    one_end("left"), one_end("right"))
}

full_court_plot <- function(fg, g, us_left_now, names) {
  k <- geom_of(g)
  p <- ggplot() + court_full_layers(k$r, k$c, k$l)
  if (nrow(fg)) {
    f <- half_to_full(fg$x, fg$y, fg$side)
    d <- data.frame(X = f$X, Y = f$Y, Team = factor(names[ifelse(fg$team == "us", 1, 2)], names),
                    Result = factor(ifelse(fg$made %in% 1, "Made", "Missed"), c("Made", "Missed")))
    p <- p + geom_point(data = d, aes(X, Y, colour = Team, shape = Result), size = 2.6, stroke = 1.1) +
      scale_colour_manual(values = setNames(c(COL_US, COL_OPP), names), drop = FALSE) +
      scale_shape_manual(values = c(Made = 16, Missed = 4), drop = FALSE)
  }
  left_team <- if (us_left_now) names[1] else names[2]; right_team <- if (us_left_now) names[2] else names[1]
  p + annotate("label", x = 23.5, y = 47.6, label = paste(left_team, "shoots here"), size = 3.6, fontface = "bold",
               colour = if (us_left_now) COL_US else COL_OPP, fill = "#fcfcfb", label.size = 0) +
      annotate("label", x = 70.5, y = 47.6, label = paste(right_team, "shoots here"), size = 3.6, fontface = "bold",
               colour = if (us_left_now) COL_OPP else COL_US, fill = "#fcfcfb", label.size = 0) +
      coord_fixed(xlim = c(0, 94), ylim = c(0, 50), expand = FALSE) +
      theme_void(base_size = 12) + theme(legend.position = "bottom") + labs(colour = NULL, shape = NULL)
}

# ---- clock ----------------------------------------------------------------------
period_secs <- function(p, g) {
  np <- g$periods %||% 4
  if (period_index(p, np) <= np) (g$period_min %||% 8) * 60
  else if (grepl("NFHS|High", g$format %||% "")) 240 else 300
}
clock_rem <- function(gs, now = as.numeric(Sys.time())) {
  at <- gs$clock_at %||% NA
  if (is.na(at)) return(NA_real_)
  if (isTRUE(gs$clock_run == 1)) max(0, at - (now - (gs$clock_t %||% now))) else at
}
fmt_clock <- function(s) if (is.na(s)) "" else sprintf("%d:%02d", as.integer(s %/% 60), as.integer(floor(s %% 60)))

# ---- team fouls and bonus -------------------------------------------------------
# NCAA men: fouls reset by half; 7th = one-and-one, 10th = two shots.
# NFHS, NCAA women, NBA, FIBA: fouls reset by quarter; free throws from the 5th.
team_fouls <- function(ev, team, period, g) {
  np <- g$periods %||% 4
  pf <- ev[ev$type == "PF" & ev$team == team, ]
  if (grepl("College Men", g$format %||% "")) {
    sh <- second_half(pf$period, np)                    # overtime counts as part of the second half
    if (second_half(period, np)) sum(sh) else sum(!sh)
  } else sum(pf$period == period)
}
bonus_label <- function(opp_fouls, g) {
  if (grepl("College Men", g$format %||% "")) {
    if (opp_fouls >= 9) "DOUBLE BONUS" else if (opp_fouls >= 6) "BONUS" else ""
  } else if (opp_fouls >= 4) "BONUS" else ""
}
