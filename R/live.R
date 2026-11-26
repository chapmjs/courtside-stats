# =============================================================================
# CourtSide Stats v6 - Live Entry sync layer
# The Live Entry page (www/live.js) records plays in the browser first and sends
# them here in batches. Each play carries a client id (cid) so a batch that is
# sent twice (after a dropped connection) is only saved once. The server answers
# with an acknowledgement and every device gets a fresh snapshot of the game.
# =============================================================================

migrate_v6 <- function(con) {
  cols <- dbGetQuery(con, "PRAGMA table_info(events)")$name
  if (!"cid" %in% cols) dbExecute(con, "ALTER TABLE events ADD COLUMN cid TEXT")
  if (!"ts" %in% cols) dbExecute(con, "ALTER TABLE events ADD COLUMN ts REAL")
  dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS events_cid ON events(cid)")
  dbExecute(con, "INSERT OR IGNORE INTO settings VALUES ('track_lineups', '0')")
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '6')")
}

ensure_game_state <- function(con, gid) {
  gs <- dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
  if (nrow(gs)) return(gs)
  g <- dbGetQuery(con, "SELECT * FROM games WHERE game_id = ?", params = list(gid))
  us <- team_roster(con, g$team_id); op <- dbGetQuery(con, "SELECT player_id FROM players WHERE squad = ? AND active = 1
                                                          ORDER BY CAST(number AS INTEGER)", params = list(g$opponent))
  dbExecute(con, "INSERT OR IGNORE INTO game_state (game_id, us, opp, period, version, us_left, clock_run)
                  VALUES (?,?,?, '1', 0, 1, 0)",
            params = list(gid, paste(head(us$player_id, 5), collapse = ","), paste(head(op$player_id, 5), collapse = ",")))
  dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
}

bump_game <- function(con, gid)
  dbExecute(con, "UPDATE game_state SET version = version + 1 WHERE game_id = ?", params = list(gid))

game_rev <- function(con, gid) {
  r <- dbGetQuery(con, "SELECT (SELECT version FROM game_state WHERE game_id = ?) v,
                               (SELECT COUNT(*) FROM events WHERE game_id = ?) n,
                               (SELECT MAX(event_id) FROM events WHERE game_id = ?) m,
                               (SELECT COUNT(*) || '-' || MAX(player_id) || '-' || TOTAL(active) FROM players) p,
                               (SELECT value FROM settings WHERE key = 'roster_rev') r",
                  params = list(gid, gid, gid))
  paste(unlist(r), collapse = "|")
}

live_snapshot <- function(con, gid) {
  gs <- ensure_game_state(con, gid)
  g <- dbGetQuery(con, "SELECT g.*, t.name team_name FROM games g LEFT JOIN teams t USING (team_id) WHERE game_id = ?",
                  params = list(gid))
  ev <- dbGetQuery(con, "SELECT event_id, cid, team, type, player_id, target_id, points, made, x, y, tag, rating, period, clock, ts,
                                linked_event_id, entered_by, session_id, created FROM events WHERE game_id = ? ORDER BY event_id",
                   params = list(gid))
  cid <- ifelse(is.na(ev$cid), paste0("e", ev$event_id), ev$cid)
  ev$ts <- ifelse(is.na(ev$ts), as.numeric(as.POSIXct(ev$created, format = "%Y-%m-%d %H:%M:%OS")) * 1000, ev$ts)
  ev$linked_cid <- cid[match(ev$linked_event_id, ev$event_id)]; ev$cid <- cid
  ev$created <- NULL; ev$linked_event_id <- NULL
  us <- team_roster(con, g$team_id)
  op <- dbGetQuery(con, "SELECT * FROM players WHERE squad = ? AND active = 1 ORDER BY CAST(number AS INTEGER)",
                   params = list(g$opponent))
  slim <- function(r) data.frame(id = as.character(r$player_id), num = r$number,
                                 name = ifelse(is.na(r$name), "", r$name))
  track <- identical(dbGetQuery(con, "SELECT value FROM settings WHERE key = 'track_lineups'")$value, "1")
  list(gid = gid, rev = game_rev(con, gid), server_now = as.numeric(Sys.time()) * 1000,
       game = list(opponent = g$opponent, team = g$team_name %||% "Us", format = g$format, periods = g$periods,
                   period_min = g$period_min, three = g$three_dist, corner = g$corner_dist, lane = g$lane_width),
       roster = list(us = slim(us), opp = slim(op)),
       state = list(us = gs$us %||% "", opp = gs$opp %||% "", period = gs$period %||% "1",
                    us_left = isTRUE((gs$us_left %||% 1) == 1), run = isTRUE(gs$clock_run == 1),
                    rem = { r <- clock_rem(gs); if (is.na(r)) -1 else r }, track = track),
       events = ev)
}

# Apply a batch of operations from one device. Returns the op ids that are done.
live_apply <- function(con, gid, ops, keeper, kid) {
  done <- character(0)
  g <- dbGetQuery(con, "SELECT * FROM games WHERE game_id = ?", params = list(gid))
  if (!nrow(g)) return(done)
  gs <- ensure_game_state(con, gid)
  id_of <- function(cid) {
    if (is.null(cid) || is.na(cid) || !nzchar(cid)) return(NA_integer_)
    if (grepl("^e[0-9]+$", cid)) return(as.integer(substring(cid, 2)))
    r <- dbGetQuery(con, "SELECT event_id FROM events WHERE cid = ?", params = list(cid)); if (nrow(r)) r$event_id[1] else NA_integer_
  }
  num <- function(v) if (is.null(v) || length(v) == 0) NA else suppressWarnings(as.numeric(v))
  chr <- function(v) if (is.null(v) || length(v) == 0 || identical(v, "")) NA_character_ else as.character(v)
  now <- as.numeric(Sys.time())
  for (op in ops) {
    oid <- op$oid %||% ""
    tryCatch({
      kind <- op$op
      if (kind == "add") {
        e <- op$ev
        dbExecute(con, "INSERT OR IGNORE INTO events (game_id, period, clock, team, player_id, type, points, made,
                        shot_type, rating, x, y, linked_event_id, on_court, opp_on_court, created, tag, entered_by,
                        session_id, ts, cid, target_id) VALUES (?,?,?,?,?,?,?,?,NULL,NULL,?,?,?,?,?,?,?,?,?,?,?,?)",
          params = list(gid, chr(e$period) %||% "1", chr(e$clock) %||% "", e$team, as.integer(num(e$player_id)),
                        e$type, as.integer(num(e$points)), as.integer(num(e$made)), num(e$x), num(e$y),
                        id_of(e$linked_cid), e$on_court %||% "", e$opp_on_court %||% "",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%OS2"), chr(e$tag), keeper, kid, num(e$ts), e$cid,
                        as.integer(num(e$target_id))))
        if (identical(e$type, "TIMEOUT") && isTRUE(gs$clock_run == 1)) {
          dbExecute(con, "UPDATE game_state SET clock_run = 0, clock_at = ? WHERE game_id = ?",
                    params = list(clock_rem(gs), gid)); gs <- ensure_game_state(con, gid)
        }
      } else if (kind == "del") {
        id <- id_of(op$cid)
        if (!is.na(id)) dbExecute(con, "DELETE FROM events WHERE event_id = ? OR linked_event_id = ?", params = list(id, id))
      } else if (kind == "upd") {
        id <- id_of(op$cid)
        if (!is.na(id)) for (f in intersect(names(op$fields), c("player_id", "tag", "made", "points", "target_id", "rating", "ts")))
          dbExecute(con, sprintf("UPDATE events SET %s = ? WHERE event_id = ?", f),
                    params = list(if (f %in% c("tag", "rating")) chr(op$fields[[f]]) else if (f == "ts") num(op$fields[[f]]) else as.integer(num(op$fields[[f]])), id))
      } else if (kind == "lineup") {
        dbExecute(con, sprintf("UPDATE game_state SET %s = ? WHERE game_id = ?", if (op$team == "us") "us" else "opp"),
                  params = list(paste(unlist(op$ids), collapse = ","), gid))
      } else if (kind == "period") {
        dbExecute(con, "UPDATE game_state SET period = ?, clock_run = 0, clock_at = ?, clock_t = ? WHERE game_id = ?",
                  params = list(op$period, period_secs(op$period, as.list(g[1, ])), now, gid))
      } else if (kind == "clock") {
        gs <- ensure_game_state(con, gid); r <- clock_rem(gs)
        if (is.na(r)) r <- period_secs(gs$period, as.list(g[1, ]))
        if (op$action == "start") dbExecute(con, "UPDATE game_state SET clock_run = 1, clock_at = ?, clock_t = ? WHERE game_id = ?", params = list(r, now, gid))
        if (op$action == "stop")  dbExecute(con, "UPDATE game_state SET clock_run = 0, clock_at = ?, clock_t = ? WHERE game_id = ?", params = list(r, now, gid))
        if (op$action == "set")   dbExecute(con, "UPDATE game_state SET clock_at = ?, clock_t = ? WHERE game_id = ?", params = list(max(0, num(op$value)), now, gid))
        if (op$action == "adj")   dbExecute(con, "UPDATE game_state SET clock_at = ?, clock_t = ? WHERE game_id = ?", params = list(max(0, r + num(op$value)), now, gid))
      } else if (kind == "sides") {
        dbExecute(con, "UPDATE game_state SET us_left = 1 - IFNULL(us_left, 1) WHERE game_id = ?", params = list(gid))
      } else if (kind == "track") {
        dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('track_lineups', ?)", params = list(if (isTRUE(op$on)) "1" else "0"))
      } else if (kind == "addplayer") {
        squad <- if (op$team == "us") "us" else g$opponent
        dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,'',?)",
                  params = list(as.character(op$number), op$name %||% "", squad))
        if (op$team == "us") add_to_team(con, g$team_id, last_id(con))
      }
      done <- c(done, oid)
    }, error = function(err) {
      message("Live Entry op failed (", op$op, "): ", conditionMessage(err))
      done <<- c(done, oid)      # drop a bad op rather than retrying it forever
    })
  }
  bump_game(con, gid)
  done
}
