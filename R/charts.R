# =============================================================================
# CourtSide Stats v2 - charts (ggplot2)
# Colors: Us = slot 1 blue, Opponent = slot 2 orange; made/missed also differ
# by shape so color never carries meaning alone.
# =============================================================================

theme_cs <- function(base = 12) theme_minimal(base_size = base) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e6e5e0", linewidth = 0.3),
        plot.title = element_text(face = "bold", colour = "#0b0b0b"),
        axis.text = element_text(colour = "#52514e"), legend.position = "bottom",
        strip.text = element_text(face = "bold", colour = "#0b0b0b"))

circ <- function(cx, cy, r, from = 0, to = 2 * pi, n = 120) {
  t <- seq(from, to, length.out = n); data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}

court_layers <- function(r = 19.75, corner = 19.75, lane = 12, lw = 0.5) {
  th0 <- if (corner < r) acos(corner / r) else 0
  arc <- circ(0, 5.25, r, th0, pi - th0); ycorner <- 5.25 + r * sin(th0)
  ftc <- circ(0, 19, 6, 0, pi); ra <- circ(0, 5.25, 4, 0, pi); rim <- circ(0, 5.25, 0.75); mid <- circ(0, 47, 6, pi, 2 * pi)
  ln <- "#55534f"
  list(
    annotate("rect", xmin = -25, xmax = 25, ymin = 0, ymax = 47, fill = "#f6ecd9", colour = ln, linewidth = lw),
    annotate("rect", xmin = -lane / 2, xmax = lane / 2, ymin = 0, ymax = 19, fill = "#ecdcbc", colour = ln, linewidth = lw),
    annotate("path", x = ftc$x, y = ftc$y, colour = ln, linewidth = lw),
    annotate("path", x = ra$x, y = ra$y, colour = ln, linewidth = lw),
    annotate("path", x = arc$x, y = arc$y, colour = ln, linewidth = lw * 1.6),
    annotate("segment", x = c(-corner, corner), xend = c(-corner, corner), y = 0, yend = ycorner,
             colour = ln, linewidth = lw * 1.6),
    annotate("segment", x = -3, xend = 3, y = 4, yend = 4, colour = ln, linewidth = lw * 2),
    annotate("path", x = rim$x, y = rim$y, colour = ln, linewidth = lw * 1.6),
    annotate("path", x = mid$x, y = mid$y, colour = ln, linewidth = lw)
  )
}

geom_of <- function(g) list(r = g$three_dist %||% 19.75, c = g$corner_dist %||% 19.75, l = g$lane_width %||% 12)

shot_chart <- function(fg, g, title = NULL, facet = NULL, size = 3, ncol = NULL) {
  k <- geom_of(g)
  p <- ggplot() + court_layers(k$r, k$c, k$l, lw = if (is.null(facet)) 0.5 else 0.35)
  fg <- fg[!is.na(fg$x), ]
  if (nrow(fg)) {
    fg$Result <- factor(ifelse(fg$made %in% 1, "Made", "Missed"), c("Made", "Missed"))
    p <- p + geom_point(data = fg, aes(x, y, shape = Result, colour = Result), size = size, stroke = 1.1) +
      scale_shape_manual(values = c(Made = 16, Missed = 4), drop = FALSE) +
      scale_colour_manual(values = c(Made = COL_US, Missed = COL_OPP), drop = FALSE)
    if (!is.null(facet)) p <- p + facet_wrap(facet, ncol = ncol)
  }
  p + coord_fixed(xlim = c(-25, 25), ylim = c(0, 47), expand = FALSE) +
    theme_void(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", margin = margin(4, 0, 4, 0))) +
    labs(title = title, shape = NULL, colour = NULL)
}

pie_chart <- function(fg, by, levels, title) {
  if (!nrow(fg) || sum(fg$pts) == 0) return(ggplot() + theme_void() + labs(title = title, subtitle = "No points scored"))
  d <- aggregate(pts ~ grp, data.frame(grp = fg[[by]], pts = fg$pts), sum)
  d <- d[d$pts > 0, ]; d$grp <- factor(d$grp, levels[levels %in% d$grp])
  d <- d[order(d$grp), ]; d$share <- d$pts / sum(d$pts)
  d$lab <- paste0(d$grp, "\n", d$pts, " pts (", round(100 * d$share), "%)")
  pal <- setNames(CAT_PAL[seq_along(levels)], levels)
  ggplot(d, aes(x = 1, y = pts, fill = grp)) +
    geom_col(width = 1, colour = "#fcfcfb", linewidth = 1) + coord_polar("y", direction = -1) +
    scale_fill_manual(values = pal, labels = d$lab, breaks = levels(d$grp)) +
    theme_void(base_size = 11) + theme(legend.position = "right", plot.title = element_text(face = "bold")) +
    labs(title = title, fill = NULL)
}

comparison_plot <- function(tm, metrics, us_name, opp_name) {
  d <- tm[tm$Metric %in% metrics, ]
  d <- rbind(data.frame(Metric = d$Metric, Team = us_name, Value = d$Us),
             data.frame(Metric = d$Metric, Team = opp_name, Value = d$Opp))
  d$Metric <- factor(d$Metric, metrics); d$Team <- factor(d$Team, c(us_name, opp_name))
  d$Value[is.na(d$Value)] <- 0
  ggplot(d, aes(x = Team, y = Value, fill = Team)) +
    geom_col(width = 0.7) + geom_text(aes(label = Value), vjust = -0.4, size = 3.4, colour = "#0b0b0b") +
    facet_wrap(~Metric, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = setNames(c(COL_US, COL_OPP), c(us_name, opp_name))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    theme_cs(11) + theme(axis.text.x = element_blank(), axis.title = element_blank(),
                         panel.grid.major.x = element_blank()) + labs(fill = NULL)
}

timeline_plot <- function(fl, us_name, opp_name, run_min = 8, period_marks = NULL) {
  s <- fl$s; s0 <- rbind(data.frame(t = 0, margin = 0), s[, c("t", "margin")])
  p <- ggplot(s0, aes(t, margin))
  big <- fl$runs[fl$runs$Points >= run_min, ]
  if (nrow(big)) {
    big$xmin <- c(0, s$t)[big$start]; big$xmax <- s$t[big$end]
    big$Run <- ifelse(big$Team == "us", paste(us_name, "run"), paste(opp_name, "run"))
    p <- p + geom_rect(data = big, inherit.aes = FALSE, alpha = 0.18,
                       aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Run)) +
      geom_text(data = big, inherit.aes = FALSE, size = 3.2, colour = "#52514e", vjust = 1.2,
                aes(x = (xmin + xmax) / 2, y = Inf, label = paste0(Points, "-0"))) +
      scale_fill_manual(values = setNames(c(COL_US, COL_OPP), c(paste(us_name, "run"), paste(opp_name, "run"))))
  }
  if (!is.null(period_marks)) p <- p + geom_vline(xintercept = period_marks, colour = "#a8a7a1", linetype = "dashed", linewidth = 0.4)
  p + geom_hline(yintercept = 0, colour = "#52514e", linewidth = 0.4) +
    geom_step(colour = "#0b0b0b", linewidth = 0.9) +
    annotate("text", x = -Inf, y = Inf, label = paste(us_name, "leads"), hjust = -0.1, vjust = 1.5, size = 3.3, colour = COL_US) +
    annotate("text", x = -Inf, y = -Inf, label = paste(opp_name, "leads"), hjust = -0.1, vjust = -0.8, size = 3.3, colour = COL_OPP) +
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    theme_cs() + labs(x = fl$xlab, y = paste0("Margin (", us_name, " minus opp)"), fill = NULL)
}

goal_period_plot <- function(d) {
  # d: Goal, Period, Value, Target, Met (TRUE/FALSE/NA)
  d$Status <- factor(ifelse(is.na(d$Met), "No data", ifelse(d$Met, "Met", "Missed")), c("Met", "Missed", "No data"))
  ggplot(d, aes(Period, Value, fill = Status)) + geom_col(width = 0.65) +
    geom_hline(aes(yintercept = Target), linetype = "dashed", colour = "#0b0b0b", linewidth = 0.5) +
    geom_text(aes(label = Value), vjust = -0.4, size = 3.2) +
    facet_wrap(~Goal, scales = "free_y", ncol = 4) +
    scale_fill_manual(values = c(Met = COL_GOOD, Missed = COL_BAD, `No data` = "#c3c2b7"), drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    theme_cs(11) + labs(x = NULL, y = NULL, fill = NULL,
                        caption = "Dashed line = target (count goals are pro-rated per period)")
}

goal_trend_plot <- function(d) {
  d$Status <- factor(ifelse(is.na(d$Met), "No data", ifelse(d$Met, "Met", "Missed")), c("Met", "Missed", "No data"))
  ggplot(d, aes(Game, Value, group = 1)) +
    geom_hline(aes(yintercept = Target), linetype = "dashed", colour = "#52514e", linewidth = 0.5) +
    (if (length(unique(d$Game)) > 1) geom_line(colour = COL_US, linewidth = 0.8)) +
    geom_point(aes(shape = Status, colour = Status), size = 3) +
    scale_colour_manual(values = c(Met = COL_GOOD, Missed = COL_BAD, `No data` = "#a8a7a1"), drop = FALSE) +
    scale_shape_manual(values = c(Met = 16, Missed = 4, `No data` = 1), drop = FALSE) +
    facet_wrap(~Goal, scales = "free_y", ncol = 3) +
    theme_cs(11) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL, colour = NULL, shape = NULL, caption = "Dashed line = target")
}
