# =============================================================================
# CourtSide Stats - demo data and XML import/export
# =============================================================================

ensure_roster <- function(con, squad, numbers, names = NULL) {
  have <- dbGetQuery(con, "SELECT * FROM players WHERE squad = ?", params = list(squad))
  for (i in seq_along(numbers)) if (!(as.character(numbers[i]) %in% have$number))
    dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,?,?)",
              params = list(as.character(numbers[i]), if (is.null(names)) "" else names[i], "", squad))
  dbGetQuery(con, "SELECT player_id FROM players WHERE squad = ? AND active = 1", params = list(squad))$player_id
}

new_game <- function(con, date, opp, loc, fmt_name, periods = NULL, pmin = NULL, three = NULL, notes = "",
                     team_id = NULL) {
  f <- FORMATS[[fmt_name]]; three <- three %||% f$three
  team_id <- team_id %||% dbGetQuery(con, "SELECT MIN(team_id) t FROM teams")$t
  dbExecute(con, "INSERT INTO games (game_date, opponent, location, format, periods, period_min, three_dist,
            corner_dist, lane_width, notes, team_id) VALUES (?,?,?,?,?,?,?,?,?,?,?)",
            params = list(as.character(date), opp, loc, fmt_name, periods %||% f$periods, pmin %||% f$min,
                          three, min(f$corner, three), f$lane, notes, as.integer(team_id)))
  last_id(con)
}

# Demo: a Varsity and a JV team whose rosters overlap (five JV players play up)
load_demo <- function(con, n_games = 4, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # a fresh install starts with one empty team; use it as Varsity rather than leaving it empty
  empty <- dbGetQuery(con, "SELECT team_id FROM teams t WHERE NOT EXISTS (SELECT 1 FROM games g WHERE g.team_id = t.team_id)
                            AND NOT EXISTS (SELECT 1 FROM team_players p WHERE p.team_id = t.team_id)")$team_id
  if (length(empty) && !nrow(dbGetQuery(con, "SELECT 1 FROM teams WHERE name = 'Varsity'")))
    dbExecute(con, "UPDATE teams SET name = 'Varsity' WHERE team_id = ?", params = list(empty[1]))
  vt <- team_id_by_name(con, "Varsity"); jt <- team_id_by_name(con, "JV")
  v_pl <- ensure_team_roster(con, vt, c(2, 4, 5, 10, 11, 12, 15, 20, 22, 24),
            c("Avery", "Brooke", "Camila", "Dani", "Elise", "Faith", "Grace", "Hallie", "Ivy", "Jada"))
  j_pl <- ensure_team_roster(con, jt, c(12, 15, 20, 22, 24, 3, 14, 21, 23, 30),
            c("Faith", "Grace", "Hallie", "Ivy", "Jada", "Kenzie", "Lily", "Mia", "Nora", "Olive"))
  opps <- c("Rigby", "Madison", "Skyline", "Bonneville", "Hillcrest", "Idaho Falls")
  for (gi in seq_len(n_games)) sim_game(con, vt, v_pl, opps[(gi - 1) %% 6 + 1], Sys.Date() - 7 * (n_games - gi), skill = 0.03)
  for (gi in 1:2) sim_game(con, jt, j_pl[c(6:10, 1:5)], opps[gi], Sys.Date() - 7 * (2 - gi) - 1, skill = 0)
}

sim_game <- function(con, team_id, us_pl, opp, date, skill = 0) {
  op_pl <- ensure_roster(con, opp, c(1, 3, 13, 14, 21, 23, 30, 32, 33))
  gid <- new_game(con, date, opp, sample(c("Home", "Away"), 1), "High School (NFHS)", notes = "Demo game", team_id = team_id)
  lu <- list(us = us_pl[1:5], opp = op_pl[1:5]); roster <- list(us = us_pl, opp = op_pl)
  for (per in 1:4) {
    clock <- 480; nxt <- FALSE
    for (poss in 1:30) {
      clock <- clock - sample(8:22, 1); if (clock < 0) break
      for (s in c("us", "opp")) if (runif(1) < 0.08) {
        out <- sample(lu[[s]], 1); lu[[s]][lu[[s]] == out] <- sample(setdiff(roster[[s]], lu[[s]]), 1)
      }
      off <- if (poss %% 2 == 1) "us" else "opp"; def <- other(off)
      A <- function(team, type, player = NA, ...) add_event(con, gid, team, type, player, period = per,
        clock = sprintf("%d:%02d", clock %/% 60, clock %% 60), on_court = paste(lu$us, collapse = ","),
        opp_on_court = paste(lu$opp, collapse = ","), ...)
      nxt <- sim_possession(A, off, def, lu, skill = if (off == "us") skill else 0, trans = nxt)
    }
  }
  gid
}

sim_possession <- function(A, off, def, lu, skill = 0, depth = 0, trans = FALSE) {
  trans <- isTRUE(trans); r <- runif(1) + if (trans) 0.04 else 0; sh <- sample(lu[[off]], 1); dfp <- sample(lu[[def]], 1)
  if (r < 0.08) { id <- A(def, "STL", dfp); A(off, "TO", sh, linked = id)
    if (runif(1) < 0.35) A(def, "DAST", sample(setdiff(lu[[def]], dfp), 1), linked = id)
    if (runif(1) < 0.5) { A(def, "TRANS", dfp, linked = id); return(TRUE) }; return(FALSE) }
  if (r < 0.15) { A(off, "TO", sh); return(FALSE) }
  if (r < 0.17) { id <- A(def, "CHG", dfp); A(off, "TO", sh, linked = id); A(off, "PF", sh, linked = id); return(FALSE) }
  if (r < 0.25) { id <- A(off, "FD", sh); A(def, "PF", dfp, linked = id)
    for (k in 1:2) A(off, "FT", sh, points = 1, made = runif(1) < 0.65 + skill); return(FALSE) }
  th <- runif(1, 0.2, pi - 0.2); three <- runif(1) < 0.3
  rad <- if (three) runif(1, 20.5, 24) else if (runif(1) < (if (trans) 0.75 else 0.45)) runif(1, 0.5, 4) else runif(1, 5, 18)
  x <- max(min(rad * cos(th), 24.5), -24.5); y <- max(5.25 + rad * sin(th), 0.5)
  rt <- sample(SHOT_RATINGS, 1, prob = c(.25, .33, .32, .10))
  mk <- runif(1) < c(EZ = .66, Open = .45, Contested = .31, Bad = .2)[[rt]] - three * 0.08 + skill
  st <- if (rad <= 4) sample(c("Layup", "Layup", "Putback"), 1) else if (three) "Catch & Shoot" else sample(c("Jump Shot", "Pull-Up", "Floater"), 1)
  if (trans) mk <- mk || runif(1) < 0.1
  id <- A(off, "FG", sh, points = 2 + three, made = mk, shot_type = st, rating = rt, x = x, y = y,
          tag = if (trans) "trans" else NA)
  if (mk && runif(1) < 0.55) A(off, "AST", sample(setdiff(lu[[off]], sh), 1), linked = id)
  if (mk) return(FALSE)
  if (!mk) {
    if (runif(1) < 0.25) A(off, "AST_ATT", sample(setdiff(lu[[off]], sh), 1), linked = id)
    if (runif(1) < 0.06) A(def, "BLK", dfp, linked = id)
    if (runif(1) < 0.3) { A(off, "OREB", sample(lu[[off]], 1), linked = id)
      if (depth < 2) return(sim_possession(A, off, def, lu, skill, depth + 1)) else return(FALSE) }
    else { rb <- sample(lu[[def]], 1); rid <- A(def, "DREB", rb, linked = id)
      if (runif(1) < 0.6) { push <- runif(1) < 0.45
        A(def, "OUTLET", rb, linked = rid, tag = if (push) "push" else "setup"); return(push) } }
  }
  FALSE
}

# ---- XML --------------------------------------------------------------------
export_game_xml <- function(con, gid, file) {
  g <- dbGetQuery(con, "SELECT * FROM games WHERE game_id = ?", params = list(gid))
  ev <- dbGetQuery(con, "SELECT * FROM events WHERE game_id = ? ORDER BY event_id", params = list(gid))
  ids <- unique(c(ev$player_id, as.integer(unlist(strsplit(c(ev$on_court, ev$opp_on_court), ",")))))
  ids <- ids[!is.na(ids)]
  pl <- dbGetQuery(con, sprintf("SELECT * FROM players WHERE player_id IN (%s)", paste(c(-1, ids), collapse = ",")))
  team <- dbGetQuery(con, "SELECT name FROM teams WHERE team_id = ?", params = list(g$team_id %||% -1))$name
  if (!length(team)) team <- "Our Team"
  doc <- xml_new_root("courtside_game", version = "2.0", team = team)
  ga <- as.list(g[1, setdiff(names(g), "game_id")]); ga <- ga[!sapply(ga, function(v) is.na(v) || identical(v, ""))]
  do.call(xml_add_child, c(list(doc, "game"), lapply(ga, as.character)))
  ro <- xml_add_child(doc, "roster")
  for (i in seq_len(nrow(pl)))
    xml_add_child(ro, "player", id = pl$player_id[i], number = pl$number[i], name = pl$name[i] %||% "",
                  position = pl$position[i] %||% "", side = ifelse(pl$squad[i] == "us", "us", "opp"))
  es <- xml_add_child(doc, "events")
  for (i in seq_len(nrow(ev))) {
    a <- as.list(ev[i, setdiff(names(ev), c("game_id", "created"))])
    a <- a[!sapply(a, function(v) is.na(v) || identical(v, ""))]
    do.call(xml_add_child, c(list(es, "event"), lapply(a, as.character)))
  }
  write_xml(doc, file)
}

import_game_xml <- function(con, file, team_id = NULL) {
  doc <- read_xml(file)
  ver <- xml_attr(doc, "version") %||% "1.0"
  g <- as.list(xml_attrs(xml_find_first(doc, "game")))
  num <- function(v) suppressWarnings(as.numeric(v %||% NA))
  dbExecute(con, "INSERT INTO games (game_date, opponent, location, format, periods, period_min,
            three_dist, corner_dist, lane_width, notes) VALUES (?,?,?,?,?,?,?,?,?,?)",
            params = list(g$game_date %||% "", g$opponent %||% "Imported", g$location %||% "",
                          g$format %||% "Custom", num(g$periods), num(g$period_min), num(g$three_dist),
                          num(g$corner_dist), num(g$lane_width), paste("Imported", g$notes %||% "")))
  gid <- last_id(con); opp <- g$opponent %||% "Imported"
  team_id <- team_id %||% dbGetQuery(con, "SELECT MIN(team_id) t FROM teams")$t
  dbExecute(con, "UPDATE games SET team_id = ? WHERE game_id = ?", params = list(as.integer(team_id), gid))
  pmap <- c()
  for (p in xml_find_all(doc, "roster/player")) {
    a <- as.list(xml_attrs(p)); squad <- if ((a$side %||% "us") == "us") "us" else opp
    hit <- dbGetQuery(con, "SELECT player_id FROM players WHERE squad = ? AND number = ? AND IFNULL(name,'') = ?",
                      params = list(squad, a$number, a$name %||% ""))
    if (nrow(hit)) new <- hit$player_id[1] else {
      dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,?,?)",
                params = list(a$number, a$name %||% "", a$position %||% "", squad))
      new <- last_id(con)
    }
    pmap[a$id] <- new
    if (squad == "us") add_to_team(con, team_id, new)
  }
  remap <- function(s) if (is.null(s)) "" else paste(pmap[strsplit(s, ",")[[1]]], collapse = ",")
  emap <- c()
  for (e in xml_find_all(doc, "events/event")) {
    a <- as.list(xml_attrs(e)); type <- a$type
    if (startsWith(type, "OPP_")) type <- substring(type, 5)      # version 1 files
    new <- add_event(con, gid, a$team, type,
                     player_id = if (!is.null(a$player_id)) pmap[[a$player_id]] else NA,
                     period = a$period %||% "1", clock = a$clock %||% "", points = num(a$points),
                     made = num(a$made), shot_type = a$shot_type %||% NA, rating = a$rating %||% NA,
                     x = num(a$x), y = num(a$y),
                     linked = if (!is.null(a$linked_event_id)) emap[[a$linked_event_id]] %||% NA else NA,
                     on_court = remap(a$on_court), opp_on_court = remap(a$opp_on_court),
                     tag = a$tag %||% NA, entered_by = a$entered_by %||% NA)
    emap[a$event_id] <- new
  }
  if (ver < "2") { dbExecute(con, "DELETE FROM settings WHERE key='schema'"); migrate_v2(con); migrate_v3(con); migrate_v4(con); migrate_v5(con) }
  gid
}
