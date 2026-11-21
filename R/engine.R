# =============================================================================
# CourtSide Stats v2 - data layer and stats engine
# Every event type is shared by both teams; the `team` column ("us"/"opp")
# says who did it. Linked events (an assist on a shot, the turnover implied by
# a steal, etc.) point at their parent through linked_event_id.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
other <- function(s) ifelse(s == "us", "opp", "us")

COL_US  <- "#2a78d6"; COL_OPP <- "#eb6834"          # categorical slots 1 and 2
COL_GOOD <- "#0ca30c"; COL_BAD <- "#d03b3b"          # status: good / critical
CAT_PAL <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

FORMATS <- list(
  "High School (NFHS)"   = list(periods = 4, min = 8,  ot = 4, three = 19.75, corner = 19.75, lane = 12),
  "College Women (NCAA)" = list(periods = 4, min = 10, ot = 5, three = 22.15, corner = 21.65, lane = 12),
  "College Men (NCAA)"   = list(periods = 2, min = 20, ot = 5, three = 22.15, corner = 21.65, lane = 12),
  "Professional (NBA)"   = list(periods = 4, min = 12, ot = 5, three = 23.75, corner = 22.00, lane = 16),
  "International (FIBA)" = list(periods = 4, min = 10, ot = 5, three = 22.15, corner = 21.65, lane = 16),
  "Custom"               = list(periods = 4, min = 8,  ot = 4, three = 19.75, corner = 19.75, lane = 12)
)
SHOT_TYPES   <- c("Layup", "Jump Shot", "Pull-Up", "Catch & Shoot", "Post Move",
                  "Floater", "Putback", "Dunk", "Hook", "Tip-In")
LAYUP_TYPES  <- c("Layup", "Dunk", "Putback", "Tip-In")
SHOT_RATINGS <- c("EZ", "Open", "Contested", "Bad")
ZONES  <- c("Rim (0-4 ft)", "Paint (non-rim)", "Mid-Range", "Corner 3", "Above-Break 3", "Unknown")
RANGES <- c("0-4 ft", "5-9 ft", "10-14 ft", "15-19 ft", "20+ ft", "Unknown")

ACTIONS <- c(FG2M = "2 Made", FG2X = "2 Miss", FG3M = "3 Made", FG3X = "3 Miss",
             FT_M = "FT Made", FT_X = "FT Miss", OREB = "Off Reb", DREB = "Def Reb",
             AST = "Assist", AST_ATT = "Ast Att", STL = "Steal", BLK = "Block",
             DAST = "Def Ast", TO = "Turnover", PF = "Foul", FD = "Foul Drawn",
             CHG = "Charge", OUTLET = "Outlet", TRANS = "Fast Break")
# Which buttons each stats-keeper role sees (role "all" sees everything)
ROLES <- c("Everything (one keeper)" = "all",
           "Plays: every stat, no clock or subs" = "plays",
           "Clock & subs (game manager)" = "clock",
           "Scorer: shots, FTs, rebounds, assists" = "scorer",
           "Possession: turnovers, steals, fouls, subs" = "poss",
           "Our team only: every stat by our players" = "us",
           "Opponent only: every stat by their players" = "opp")
ROLE_ACTIONS <- list(
  plays  = names(ACTIONS), us = names(ACTIONS), opp = names(ACTIONS),
  scorer = c("FG2M","FG2X","FG3M","FG3X","FT_M","FT_X","OREB","DREB","AST","AST_ATT","BLK","OUTLET","TRANS"),
  poss   = c("STL","DAST","TO","PF","FD","CHG","OUTLET","TRANS"))

# Events that belong to the team on offense; everything else belongs to the defense
OFF_TYPES <- c("FG", "FT", "OREB", "AST", "AST_ATT", "TO", "FD", "OUTLET", "TRANS", "JUMP", "POSS",
               "PASS", "TOUCH", "DRIVE", "POST")
# These start the new offense, so they never inherit the offense of the event they link to
NO_INHERIT <- c("OUTLET", "TRANS", "JUMP", "POSS")

DEFAULT_WEIGHTS <- data.frame(
  key   = c("FG2M","FG3M","FTM","FG2X","FG3X","FTX","OREB","DREB","AST","AST_ATT",
            "STL","BLK","DAST","TO","PF","FD","CHG"),
  label = c("2PT made","3PT made","FT made","2PT miss","3PT miss","FT miss","Off rebound",
            "Def rebound","Assist","Assist attempt","Steal","Block","Defensive assist",
            "Turnover","Personal foul","Foul drawn","Charge taken"),
  value = c(2, 3, 1, -0.75, -0.75, -0.5, 1, 0.75, 1, 0.25, 1.5, 1, 1, -1.5, -0.5, 0.75, 2),
  stringsAsFactors = FALSE)

DEFAULT_GOALS <- data.frame(
  label = c("Points per possession 0.95+", "Turnovers 12 or fewer", "Offensive rebound % 30+",
            "65%+ free throws", "Opponent PPP under 0.85", "Hold opponent under 40% FG",
            "Defensive rebound % 70+", "Second-effort points 8+"),
  side = c("Us", "Us", "Us", "Us", "Opp", "Opp", "Us", "Us"),
  metric = c("PPP", "TO", "OREB%", "FT%", "PPP", "FG%", "DREB%", "2nd Effort Pts"),
  comparator = c(">=", "<=", ">=", ">=", "<", "<", ">=", ">="),
  target = c(0.95, 12, 30, 65, 0.85, 40, 70, 8), stringsAsFactors = FALSE)

# ---- Database ---------------------------------------------------------------
db_connect <- function(path) {
  con <- dbConnect(SQLite(), path)
  dbExecute(con, "PRAGMA busy_timeout = 5000")          # several keepers writing at once
  try(dbGetQuery(con, "PRAGMA journal_mode = WAL"), silent = TRUE)
  dbExecute(con, "CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS players (player_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  number TEXT, name TEXT, position TEXT, active INTEGER DEFAULT 1, squad TEXT DEFAULT 'us')")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS games (game_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  game_date TEXT, opponent TEXT, location TEXT, format TEXT, periods INTEGER,
                  period_min REAL, three_dist REAL, corner_dist REAL, lane_width REAL, notes TEXT)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS events (event_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  game_id INTEGER, period TEXT, clock TEXT, team TEXT, player_id INTEGER,
                  type TEXT, points INTEGER, made INTEGER, shot_type TEXT, rating TEXT,
                  x REAL, y REAL, linked_event_id INTEGER, on_court TEXT, created TEXT,
                  opp_on_court TEXT DEFAULT '')")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS goals (goal_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  label TEXT, side TEXT, metric TEXT, comparator TEXT, target REAL)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS weights (key TEXT PRIMARY KEY, label TEXT, value REAL)")
  if (dbGetQuery(con, "SELECT COUNT(*) n FROM settings WHERE key='team_name'")$n == 0)
    dbExecute(con, "INSERT INTO settings VALUES ('team_name', 'Our Team')")
  migrate_v2(con); migrate_v3(con); migrate_v4(con); migrate_v5(con); migrate_v6(con); migrate_v7(con)
  if (dbGetQuery(con, "SELECT COUNT(*) n FROM weights")$n == 0) dbAppendTable(con, "weights", DEFAULT_WEIGHTS)
  if (dbGetQuery(con, "SELECT COUNT(*) n FROM goals")$n == 0) dbAppendTable(con, "goals", DEFAULT_GOALS)
  if (dbGetQuery(con, "SELECT COUNT(*) n FROM settings WHERE key='team_name'")$n == 0)
    dbExecute(con, "INSERT INTO settings VALUES ('team_name', 'Our Team')")
  con
}

# Upgrade a version-1 database (opponent stats were team-level with OPP_ prefixes)
migrate_v2 <- function(con) {
  v <- dbGetQuery(con, "SELECT value FROM settings WHERE key='schema'")$value
  if (length(v) && v %in% c("2", "3", "4", "5", "6", "7")) return(invisible())
  cols <- function(t) dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name
  if (!"squad" %in% cols("players")) dbExecute(con, "ALTER TABLE players ADD COLUMN squad TEXT DEFAULT 'us'")
  if (!"opp_on_court" %in% cols("events")) dbExecute(con, "ALTER TABLE events ADD COLUMN opp_on_court TEXT DEFAULT ''")
  dbExecute(con, "UPDATE events SET type = substr(type, 5) WHERE type LIKE 'OPP\\_%' ESCAPE '\\'")
  # v1 implied the opponent turnover on a steal or charge; v2 stores it explicitly
  old <- dbGetQuery(con, "SELECT * FROM events e WHERE type IN ('STL','CHG') AND NOT EXISTS
                          (SELECT 1 FROM events c WHERE c.linked_event_id = e.event_id AND c.type = 'TO')")
  for (i in seq_len(nrow(old))) {
    o <- old[i, ]
    ins <- function(type) dbExecute(con, "INSERT INTO events (game_id, period, clock, team, type, linked_event_id,
             on_court, opp_on_court, created) VALUES (?,?,?,?,?,?,?,?,?)",
             params = list(o$game_id, o$period, o$clock, other(o$team), type, o$event_id,
                           o$on_court %||% "", "", format(Sys.time())))
    ins("TO"); if (o$type == "CHG") ins("PF")
  }
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '2')")
}

# v3: outlet/transition tags, who entered each event, shared game state, keeper presence
migrate_v3 <- function(con) {
  cols <- function(t) dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name
  for (c in c("tag", "entered_by", "session_id"))
    if (!c %in% cols("events")) dbExecute(con, sprintf("ALTER TABLE events ADD COLUMN %s TEXT", c))
  dbExecute(con, "CREATE TABLE IF NOT EXISTS game_state (game_id INTEGER PRIMARY KEY, us TEXT, opp TEXT,
                  period TEXT, version INTEGER DEFAULT 0)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS keepers (session_id TEXT PRIMARY KEY, game_id INTEGER,
                  name TEXT, role TEXT, last_seen REAL)")
  dbExecute(con, "INSERT OR IGNORE INTO settings VALUES ('auto8', '0')")
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '3')")
}

last_id <- function(con) dbGetQuery(con, "SELECT last_insert_rowid() id")$id

add_event <- function(con, game_id, team, type, player_id = NA, period = "1", clock = "",
                      points = NA, made = NA, shot_type = NA, rating = NA, x = NA, y = NA,
                      linked = NA, on_court = "", opp_on_court = "", tag = NA,
                      entered_by = NA, session_id = NA, ts = as.numeric(Sys.time()) * 1000) {
  pid <- suppressWarnings(as.integer(player_id %||% NA))
  dbExecute(con, "INSERT INTO events (game_id, period, clock, team, player_id, type, points, made,
                  shot_type, rating, x, y, linked_event_id, on_court, opp_on_court, created,
                  tag, entered_by, session_id, ts) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
            params = list(as.integer(game_id), as.character(period), as.character(clock %||% ""),
                          team, pid, type, as.integer(points %||% NA), as.integer(made %||% NA),
                          as.character(shot_type %||% NA), as.character(rating %||% NA),
                          as.numeric(x %||% NA), as.numeric(y %||% NA), as.integer(linked %||% NA),
                          on_court %||% "", opp_on_court %||% "", format(Sys.time(), "%Y-%m-%d %H:%M:%OS2"),
                          as.character(tag %||% NA), as.character(entered_by %||% NA),
                          as.character(session_id %||% NA), as.numeric(ts)))
  last_id(con)
}

delete_event <- function(con, id)
  dbExecute(con, "DELETE FROM events WHERE event_id = ? OR linked_event_id = ?", params = list(id, id))

get_events <- function(con, game_ids) {
  game_ids <- game_ids[!is.na(game_ids)]
  auto8 <- identical(dbGetQuery(con, "SELECT value FROM settings WHERE key='auto8'")$value, "1")
  if (!length(game_ids)) return(annotate_events(dbGetQuery(con, "SELECT e.*, NULL lane_width, NULL periods,
    NULL period_min, NULL format, 0 auto8 FROM events e WHERE 0")))
  q <- sprintf("SELECT e.*, g.lane_width, g.periods, g.period_min, g.format, %d auto8 FROM events e JOIN games g USING (game_id)
                WHERE game_id IN (%s) ORDER BY game_id, event_id", as.integer(auto8), paste(as.integer(game_ids), collapse = ","))
  annotate_events(dbGetQuery(con, q))
}

# ---- Geometry ---------------------------------------------------------------
is_three <- function(x, y, r, corner) {
  ycut <- 5.25 + sqrt(max(r^2 - corner^2, 0))
  (abs(x) >= corner & y <= ycut) | (sqrt(x^2 + (y - 5.25)^2) > r & y > ycut)
}
shot_dist <- function(x, y) sqrt(x^2 + (y - 5.25)^2)
shot_zone <- function(x, y, points, lane = 12) {
  lane <- ifelse(is.na(lane), 12, lane); d <- shot_dist(x, y)
  ifelse(is.na(x), "Unknown",
  ifelse(points %in% 3 & y <= 14, "Corner 3",
  ifelse(points %in% 3, "Above-Break 3",
  ifelse(d <= 4, "Rim (0-4 ft)",
  ifelse(abs(x) <= lane / 2 & y <= 19, "Paint (non-rim)", "Mid-Range")))))
}
shot_range <- function(x, y) {
  d <- shot_dist(x, y)
  r <- as.character(cut(d, c(-Inf, 4.5, 9.5, 14.5, 19.5, Inf), labels = RANGES[1:5]))
  ifelse(is.na(r), "Unknown", r)
}

# ---- Annotation: possessions, second effort, bench, zones -------------------
oc_ids <- function(s) { s <- s[!is.na(s) & s != ""]; if (!length(s)) integer(0) else as.integer(strsplit(s[1], ",")[[1]]) }

annotate_events <- function(ev) {
  n <- nrow(ev)
  # ADJ = a score correction: it changes the team score but no player's stats
  ev$pts <- ifelse(ev$type %in% c("FG", "FT") & ev$made %in% 1, ev$points,
                   ifelse(ev$type == "ADJ" & !is.na(ev$points), ev$points, 0))
  if (is.null(ev$ts)) ev$ts <- rep(NA_real_, n)
  if (!n) { for (c in c("zone","range","offense","poss_reason")) ev[[c]] <- character(0)
    for (c in c("paint","layup","second_eff","bench")) ev[[c]] <- logical(0)
    ev$poss_id <- integer(0); ev$ord <- numeric(0); return(ev) }
  ev$opp_on_court[is.na(ev$opp_on_court)] <- ""; ev$on_court[is.na(ev$on_court)] <- ""
  # Time order: the entry device's timestamp (Live Entry), else when the server saved it
  ev$ord <- ifelse(!is.na(ev$ts), ev$ts,
                   as.numeric(as.POSIXct(ev$created, format = "%Y-%m-%d %H:%M:%OS")) * 1000)
  ev$ord[is.na(ev$ord)] <- ev$event_id[is.na(ev$ord)]
  # Keep each play's linked entries together (a steal with its turnover, a miss with its rebound),
  # even when two keepers entered them a few seconds apart; events that start the new offense go last.
  ev <- ev[order(ev$game_id, ev$ord, ev$event_id), ]
  root <- ev$ord; names(root) <- ev$event_id
  for (i in seq_len(n)) { lk <- ev$linked_event_id[i]
    if (!is.na(lk) && !is.na(root[as.character(lk)])) root[i] <- root[as.character(lk)] }
  ev <- ev[order(ev$game_id, root, ev$type %in% NO_INHERIT, ev$ord, ev$event_id), ]
  if (is.null(ev$session_id)) ev$session_id <- NA_character_
  if (is.null(ev$target_id)) ev$target_id <- NA_integer_
  ev <- drop_cross_keeper_dupes(ev); ev <- infer_lineups(ev)
  ev <- merge_shot_spots(ev)
  isfg <- ev$type == "FG"
  ev$zone  <- ifelse(isfg, shot_zone(ev$x, ev$y, ev$points, ev$lane_width), NA)
  ev$range <- ifelse(isfg, shot_range(ev$x, ev$y), NA)
  ev$paint <- isfg & ev$zone %in% ZONES[1:2]
  ev$layup <- isfg & (ev$shot_type %in% LAYUP_TYPES | (is.na(ev$shot_type) & ev$zone %in% ZONES[1]))
  ev$offense <- NA_character_; ev$poss_id <- NA_integer_; ev$poss_reason <- NA_character_
  ev$second_eff <- FALSE; ev$bench <- FALSE
  for (gid in unique(ev$game_id)) {
    idx <- which(ev$game_id == gid)
    st <- list(us = oc_ids(ev$on_court[idx]), opp = oc_ids(ev$opp_on_court[idx]))
    sc <- idx[ev$pts[idx] > 0 & !is.na(ev$player_id[idx])]
    if (length(sc)) ev$bench[sc] <- mapply(function(t, p) length(st[[t]]) > 0 && !(p %in% st[[t]]),
                           ev$team[sc], ev$player_id[sc])
    # which team is on offense for each event (linked entries inherit their play's offense)
    off_of <- list(); cur <- NA
    for (i in idx) {
      tm <- ev$team[i]; ty <- ev$type[i]; lk <- ev$linked_event_id[i]
      off <- if (ty %in% NEUTRAL_TYPES) (if (is.na(cur)) tm else cur)
             else if (!is.na(lk) && !(ty %in% NO_INHERIT) && !is.null(off_of[[as.character(lk)]])) off_of[[as.character(lk)]]
             else if (ty %in% OFF_TYPES) tm else other(tm)
      off_of[[as.character(ev$event_id[i])]] <- off; ev$offense[i] <- off
      if (!(ty %in% NEUTRAL_TYPES)) cur <- off
    }
    if (any(ev$type[idx] == "POSS")) ev[idx, ] <- possessions_from_markers(ev[idx, ], gid)
    else ev[idx, ] <- possessions_inferred(ev[idx, ], gid)
  }
  ev
}

# Possessions inferred from the play sequence (no possession keeper)
possessions_inferred <- function(e, gid) {
  cur <- NA; per <- NA; pc <- 0; had_oreb <- FALSE; reason <- "period start"; pending <- "other"; ended <- FALSE
  for (i in seq_len(nrow(e))) {
    tm <- e$team[i]; ty <- e$type[i]; off <- e$offense[i]
    # A score, turnover or defensive rebound ends a possession even when the other team's
    # possession in between wasn't recorded (e.g. only one team is being ball-tracked).
    restart <- ended && off == cur && ty %in% c("PASS", "TOUCH", "FG", "TO", "DRIVE", "POST", "OREB") &&
               is.na(e$linked_event_id[i])
    if (is.na(cur) || off != cur || e$period[i] != per || restart) {
      ended <- FALSE
      reason <- if (is.na(per) || e$period[i] != per) "period start" else pending
      pc <- pc + 1; cur <- off; per <- e$period[i]; had_oreb <- FALSE; pending <- "other"
    }
    e$poss_id[i] <- gid * 100000L + pc; e$poss_reason[i] <- reason
    if (ty %in% NEUTRAL_TYPES) next
    if (ty == "JUMP") pending <- "jump ball"
    if (ty == "OREB" && tm == off) had_oreb <- TRUE
    if (e$pts[i] > 0 && tm == off && had_oreb) e$second_eff[i] <- TRUE
    if (ty == "TO" && tm == off) { pending <- "turnover"; ended <- TRUE }
    else if (ty == "DREB" && tm != off) { pending <- "def rebound"; ended <- TRUE }
    else if (e$pts[i] > 0 && tm == off && ty == "FG") { pending <- "made basket"; ended <- TRUE }
    else if (e$pts[i] > 0 && tm == off) pending <- "made basket"
  }
  e
}

POSS_START <- c(dreb = "def rebound", to = "turnover", made = "made basket", ft = "after free throws",
                deadball = "dead ball", jump = "jump ball", period = "period start")
# Possessions set by a possession keeper: each POSS marker starts one. Every other entry joins the
# latest possession of its offense that started before it (allowing a few seconds of entry lag).
possessions_from_markers <- function(e, gid, lag_ms = 4000) {
  mk <- which(e$type == "POSS"); mk <- mk[order(e$ord[mk])]
  seg_id <- gid * 100000L + seq_along(mk); seg_team <- e$team[mk]; seg_ord <- e$ord[mk]
  seg_reason <- ifelse(is.na(POSS_START[e$tag[mk]]), "other", POSS_START[e$tag[mk]])
  for (i in seq_len(nrow(e))) {
    if (e$type[i] == "POSS") { k <- match(i, mk) }
    else {
      cand <- which(seg_ord <= e$ord[i] + lag_ms & (seg_team == e$offense[i] | e$type[i] %in% NEUTRAL_TYPES))
      if (!length(cand)) cand <- which(seg_team == e$offense[i])
      k <- if (length(cand)) max(cand) else NA
    }
    if (!is.na(k)) { e$poss_id[i] <- seg_id[k]; e$poss_reason[i] <- seg_reason[k]; e$offense[i] <- seg_team[k] }
  }
  # Plays with no matching possession tap (the keeper started late or missed a change)
  # fall back to the automatic splitting, so nothing drops out of the stats.
  miss <- which(is.na(e$poss_id))
  if (length(miss)) {
    sub <- possessions_inferred(e[miss, ], gid)
    e$poss_id[miss] <- sub$poss_id + 50000L; e$poss_reason[miss] <- sub$poss_reason; e$second_eff[miss] <- sub$second_eff
  }
  for (pid in unique(na.omit(e$poss_id[e$type == "POSS" | !(seq_len(nrow(e)) %in% miss)]))) {      # second-effort points within each possession
    ii <- which(e$poss_id %in% pid); ii <- ii[order(e$ord[ii])]; o <- e$offense[ii[1]]; had <- FALSE
    for (i in ii) { if (e$type[i] == "OREB" && e$team[i] == o) had <- TRUE
      if (e$pts[i] > 0 && e$team[i] == o && had) e$second_eff[i] <- TRUE }
  }
  e
}

# Shot spots from the shot-location keeper (SHOTLOC) give a location to the scorer's matching
# made/missed shot: same team, same player (or unknown), entered within 30 seconds, nearest first.
merge_shot_spots <- function(ev, window_ms = 30000) {
  loc <- which(ev$type == "SHOTLOC"); if (!length(loc)) return(ev)
  used <- integer(0)
  for (j in loc[order(ev$ord[loc])]) {
    cand <- which(ev$type == "FG" & ev$game_id == ev$game_id[j] & ev$team == ev$team[j] & is.na(ev$x) &
                  abs(ev$ord - ev$ord[j]) <= window_ms & !(seq_len(nrow(ev)) %in% used) &
                  (is.na(ev$player_id) | is.na(ev$player_id[j]) | ev$player_id %in% ev$player_id[j]))
    if (!length(cand)) next
    k <- cand[which.min(abs(ev$ord[cand] - ev$ord[j]))]
    ev$x[k] <- ev$x[j]; ev$y[k] <- ev$y[j]; used <- c(used, k)
    if (is.na(ev$player_id[k])) ev$player_id[k] <- ev$player_id[j]
  }
  ev
}

TRANSITION_SECS <- 8

poss_table <- function(ev) {
  empty <- data.frame(poss_id = integer(0), game_id = integer(0), period = character(0), offense = character(0),
                      points = numeric(0), reason = character(0), lu_us = character(0), lu_opp = character(0),
                      trans = logical(0), manual_trans = logical(0), had_to = logical(0), dur = numeric(0),
                      phase = character(0))
  if (!nrow(ev)) return(empty)
  secs <- clock_secs(ev$clock)
  sp <- split(seq_len(nrow(ev)), ev$poss_id)
  pt <- do.call(rbind, lapply(sp, function(i) {
    o <- ev$offense[i[1]]; mine <- ev$team[i] == o; ty <- ev$type[i]
    term <- i[mine & ty %in% c("FG", "FT", "TO")]
    withclk <- i[!is.na(secs[i])]
    data.frame(poss_id = ev$poss_id[i[1]], game_id = ev$game_id[i[1]], period = ev$period[i[1]],
               offense = o, points = sum(ev$pts[i][mine & ev$type[i] != "ADJ"]), reason = ev$poss_reason[i[1]],
               lu_us = lineup_key(ev$on_court[i[1]]), lu_opp = lineup_key(ev$opp_on_court[i[1]]),
               manual_trans = any(mine & (ty == "TRANS" | (ty == "OUTLET" & ev$tag[i] %in% "push") |
                                          (ty == "FG" & ev$tag[i] %in% "trans"))),
               had_to = any(mine & ty == "TO"),
               first_term = if (length(term)) secs[term[1]] else NA_real_,
               end_secs = if (length(withclk)) secs[withclk[length(withclk)]] else NA_real_,
               plen = (if (is.na(ev$period_min[i[1]] %||% NA)) 8 else ev$period_min[i[1]]) * 60,
               otlen = if (grepl("NFHS|High", ev$format[i[1]] %||% "")) 240 else 300,
               auto8 = isTRUE(ev$auto8[i[1]] == 1),
               start_ord = min(ev$ord[i]), first_term_ord = if (length(term)) min(ev$ord[term]) else NA_real_,
               marked = any(ty == "POSS"))
  }))
  pt <- pt[order(pt$poss_id), ]
  prev_same <- c(FALSE, pt$game_id[-1] == pt$game_id[-nrow(pt)] & pt$period[-1] == pt$period[-nrow(pt)])
  per_len <- ifelse(grepl("OT", pt$period), pt$otlen, pt$plen)
  pt$start_secs <- ifelse(prev_same, c(NA, pt$end_secs[-nrow(pt)]), per_len)
  pt$dur <- ifelse(!is.na(pt$start_secs) & !is.na(pt$end_secs) & pt$start_secs >= pt$end_secs,
                   pt$start_secs - pt$end_secs, NA)
  # No game clock: estimate possession length from real time between possession starts.
  # Gaps over 60 seconds include stoppages (free throws, timeouts), so they are left out.
  next_ord <- c(pt$start_ord[-1], NA); next_same <- c(prev_same[-1], FALSE)
  rt <- ifelse(next_same, (next_ord - pt$start_ord) / 1000, NA)
  pt$dur <- ifelse(is.na(pt$dur) & !is.na(rt) & rt > 0 & rt <= 60, rt, pt$dur)
  quick <- pt$start_secs - pt$first_term
  quick <- ifelse(is.na(quick), (pt$first_term_ord - pt$start_ord) / 1000, quick)
  auto <- pt$auto8 & pt$reason %in% c("def rebound", "turnover") & !is.na(quick) & quick >= 0 & quick <= TRANSITION_SECS
  pt$trans <- pt$manual_trans | auto
  pt$phase <- ifelse(pt$trans, "Transition", "Half-court")
  rownames(pt) <- NULL
  pt[, c(names(empty), "start_secs")]
}
lineup_key <- function(s) vapply(strsplit(ifelse(is.na(s), "", s), ","), function(z)
  paste(sort(as.integer(z)), collapse = ","), "")

# ---- Labels -----------------------------------------------------------------
player_labels <- function(players) setNames(
  trimws(paste0("#", players$number, " ", ifelse(is.na(players$name), "", players$name))), players$player_id)

pct <- function(a, b, d = 1) ifelse(b > 0, round(100 * a / b, d), NA)
ppp <- function(p) if (length(p)) round(sum(p) / length(p), 2) else NA
ev_key <- function(ev) ifelse(ev$type == "FG", paste0("FG", ev$points, ifelse(ev$made %in% 1, "M", "X")),
                       ifelse(ev$type == "FT", ifelse(ev$made %in% 1, "FTM", "FTX"), ev$type))
ev_value <- function(ev, weights) { w <- setNames(weights$value, weights$key); v <- w[ev_key(ev)]; ifelse(is.na(v), 0, v) }

# ---- Team metrics -----------------------------------------------------------
METRIC_GROUPS <- list(
  Scoring = c("PTS", "PPP", "Layup Pts", "3PT Pts", "FT Pts", "Paint Pts", "2nd Effort Pts",
              "Pts off TO", "Bench Pts"),
  Shooting = c("FG%", "3P%", "FT%", "eFG%", "TS%", "Pts/Shot", "EZ+Open Shot %"),
  Rebounding = c("OREB%", "DREB%", "REB%", "OREB", "DREB", "REB"),
  `Ball Handling` = c("AST", "AST Att", "TO", "BH Error %"),
  Defense = c("Def AST", "STL", "Def Stops", "Stop %", "CHG", "PF", "BLK"),
  Transition = c("Transition %", "Transition PPP", "Half-court PPP", "Transition Pts", "Transition TO %",
                 "PPP after DREB", "Outlets", "Outlet Push %"),
  Miscellaneous = c("Productivity", "Off Eff", "Def Eff", "Possessions", "FD", "FT Rate"))
METRICS <- c("FGM", "FGA", "3PM", "3PA", "FTM", "FTA", unlist(METRIC_GROUPS, use.names = FALSE))
LOWER_BETTER <- c("TO", "BH Error %", "PF", "Def Eff", "Transition TO %")
RATE_METRICS <- c("Transition %", "Transition PPP", "Half-court PPP", "Transition TO %", "PPP after DREB",
                  "Outlet Push %", "FG%", "3P%", "FT%", "eFG%", "TS%", "Pts/Shot", "EZ+Open Shot %", "OREB%", "DREB%",
                  "REB%", "BH Error %", "Stop %", "Off Eff", "Def Eff", "PPP", "FT Rate")

side_stats <- function(ev, s, weights, pt = poss_table(ev)) {
  m <- ev[ev$team == s, ]; o <- ev[ev$team == other(s), ]
  n <- function(t) sum(m$type == t)
  fg <- m[m$type == "FG", ]; ft <- m[m$type == "FT", ]
  FGM <- sum(fg$made %in% 1); FGA <- nrow(fg); TPM <- sum(fg$made %in% 1 & fg$points == 3)
  TPA <- sum(fg$points == 3); FTM <- sum(ft$made %in% 1); FTA <- nrow(ft)
  PTS <- sum(m$pts); PTSr <- sum(m$pts[m$type != "ADJ"]); OREB <- n("OREB"); DREB <- n("DREB"); TO <- n("TO")
  oD <- sum(o$type == "DREB"); oO <- sum(o$type == "OREB")
  POSS <- FGA - OREB + TO + 0.44 * FTA
  dposs <- pt[pt$offense == other(s), ]; stops <- sum(dposs$points == 0)
  op <- pt[pt$offense == s, ]; tp <- op[op$trans, ]
  rated <- fg[!is.na(fg$rating) & fg$rating != "", ]
  c(PTS = PTS, FGM = FGM, FGA = FGA, `FG%` = pct(FGM, FGA), `3PM` = TPM, `3PA` = TPA,
    `3P%` = pct(TPM, TPA), FTM = FTM, FTA = FTA, `FT%` = pct(FTM, FTA),
    PPP = ifelse(POSS > 0, round(PTSr / POSS, 2), NA),
    `Layup Pts` = sum(m$pts[m$layup %in% TRUE]), `3PT Pts` = 3 * TPM, `FT Pts` = FTM,
    `Paint Pts` = sum(m$pts[m$paint %in% TRUE]), `2nd Effort Pts` = sum(m$pts[m$second_eff]),
    `Pts off TO` = sum(m$pts[m$poss_reason %in% "turnover" & m$offense == s & m$type != "ADJ"]),
    `Bench Pts` = sum(m$pts[m$bench]),
    `eFG%` = pct(FGM + 0.5 * TPM, FGA), `TS%` = pct(PTSr, 2 * (FGA + 0.44 * FTA)),
    `Pts/Shot` = ifelse(FGA > 0, round(sum(fg$pts) / FGA, 2), NA),
    `EZ+Open Shot %` = pct(sum(rated$rating %in% c("EZ", "Open")), nrow(rated)),
    `OREB%` = pct(OREB, OREB + oD), `DREB%` = pct(DREB, DREB + oO),
    `REB%` = pct(OREB + DREB, OREB + DREB + oO + oD), OREB = OREB, DREB = DREB, REB = OREB + DREB,
    AST = n("AST"), `AST Att` = n("AST_ATT"), TO = TO, `BH Error %` = pct(TO, POSS),
    `Def AST` = n("DAST"), STL = n("STL"), `Def Stops` = stops, `Stop %` = pct(stops, nrow(dposs)),
    CHG = n("CHG"), PF = n("PF"), BLK = n("BLK"),
    Productivity = round(sum(ev_value(m, weights)), 1),
    `Off Eff` = ifelse(POSS > 0, round(100 * PTSr / POSS, 1), NA), `Def Eff` = NA,
    Possessions = round(POSS, 1), FD = n("FD"), `FT Rate` = ifelse(FGA > 0, round(FTA / FGA, 2), NA),
    `Transition %` = pct(nrow(tp), nrow(op)), `Transition PPP` = ppp(tp$points),
    `Half-court PPP` = ppp(op$points[!op$trans]), `Transition Pts` = sum(tp$points),
    `Transition TO %` = pct(sum(tp$had_to), nrow(tp)), `PPP after DREB` = ppp(op$points[op$reason == "def rebound"]),
    Outlets = n("OUTLET"), `Outlet Push %` = pct(sum(m$type == "OUTLET" & m$tag %in% "push"), n("OUTLET")))
}

team_metrics <- function(ev, weights) {
  pt <- poss_table(ev)
  us <- side_stats(ev, "us", weights, pt); op <- side_stats(ev, "opp", weights, pt)
  us["Def Eff"] <- op["Off Eff"]; op["Def Eff"] <- us["Off Eff"]
  data.frame(Metric = METRICS, Us = unname(us[METRICS]), Opp = unname(op[METRICS]), check.names = FALSE)
}
tm_get <- function(tm, m, s) tm[tm$Metric == m, s]

# ---- Player box score -------------------------------------------------------
plus_minus <- function(ev, side) {
  col <- if (side == "us") "on_court" else "opp_on_court"
  s <- ev[ev$pts > 0 & ev$type != "ADJ" & ev[[col]] != "", ]
  if (!nrow(s)) return(numeric(0))
  ids <- strsplit(s[[col]], ","); v <- ifelse(s$team == side, 1, -1) * s$pts
  tapply(rep(v, lengths(ids)), as.integer(unlist(ids)), sum)
}

box_score <- function(ev, players, weights, side = "us") {
  if (!nrow(ev)) return(NULL)
  lab <- player_labels(players); col <- if (side == "us") "on_court" else "opp_on_court"
  m <- ev[ev$team == side & !is.na(ev$player_id), ]
  oc <- unique(as.integer(unlist(strsplit(ev[[col]][ev[[col]] != ""], ","))))
  pids <- sort(unique(c(m$player_id, oc))); pids <- pids[as.character(pids) %in% names(lab)]
  if (!length(pids)) return(NULL)
  pm <- plus_minus(ev, side); m$val <- ev_value(m, weights)
  rows <- lapply(pids, function(p) {
    d <- m[m$player_id == p, ]; fg <- d[d$type == "FG", ]; ft <- d[d$type == "FT", ]
    k <- function(t) sum(d$type == t)
    on <- grepl(paste0("(^|,)", p, "(,|$)"), ev[[col]])
    gp <- length(unique(c(d$game_id, ev$game_id[on])))
    FGM <- sum(fg$made %in% 1); FGA <- nrow(fg); TPM <- sum(fg$made %in% 1 & fg$points == 3)
    TPA <- sum(fg$points == 3); FTM <- sum(ft$made %in% 1); FTA <- nrow(ft)
    data.frame(Player = unname(lab[as.character(p)]), player_id = p, Team = side, GP = gp,
               PTS = sum(d$pts), FGM = FGM, FGA = FGA, `FG%` = pct(FGM, FGA), `3PM` = TPM, `3PA` = TPA,
               `3P%` = pct(TPM, TPA), FTM = FTM, FTA = FTA, `FT%` = pct(FTM, FTA),
               `Pts/Shot` = ifelse(FGA > 0, round(sum(fg$pts) / FGA, 2), NA),
               OREB = k("OREB"), DREB = k("DREB"), REB = k("OREB") + k("DREB"),
               AST = k("AST"), `AST Att` = k("AST_ATT"), TO = k("TO"), STL = k("STL"),
               BLK = k("BLK"), `Def AST` = k("DAST"), PF = k("PF"), FD = k("FD"), CHG = k("CHG"),
               Outlets = k("OUTLET"), `Outlet Push` = sum(d$type == "OUTLET" & d$tag %in% "push"),
               `Paint Pts` = sum(d$pts[d$paint %in% TRUE]), `2nd Pts` = sum(d$pts[d$second_eff]),
               `+/-` = if (any(on)) unname(pm[as.character(p)] %||% 0) else NA, Productivity = round(sum(d$val), 1),
               check.names = FALSE)
  })
  out <- do.call(rbind, rows); out[order(-out$PTS, -out$Productivity), ]
}

box_totals <- function(bx, ev = NULL, side = "us", weights = NULL) {
  if (is.null(bx)) return(NULL)
  pctc <- grep("%$|/Shot", names(bx), value = TRUE)
  num <- setdiff(names(bx)[sapply(bx, is.numeric)], c("player_id", "GP", "+/-", pctc))
  tot <- as.list(colSums(bx[num]))
  if (!is.null(ev)) {   # add team-level stats (team rebounds, unattributed events)
    tm <- team_metrics(ev, weights); s <- if (side == "us") "Us" else "Opp"
    for (c in intersect(c("PTS","FGM","FGA","3PM","3PA","FTM","FTA","OREB","DREB","REB","AST","TO",
                          "STL","BLK","PF","FD","CHG","Paint Pts"), names(tot)))
      tot[[c]] <- tm_get(tm, c, s)
    tot[["AST Att"]] <- tm_get(tm, "AST Att", s); tot[["Def AST"]] <- tm_get(tm, "Def AST", s)
    tot$Outlets <- tm_get(tm, "Outlets", s)
    tot[["2nd Pts"]] <- tm_get(tm, "2nd Effort Pts", s); tot$Productivity <- tm_get(tm, "Productivity", s)
  }
  tot$`FG%` <- pct(tot$FGM, tot$FGA); tot$`3P%` <- pct(tot$`3PM`, tot$`3PA`); tot$`FT%` <- pct(tot$FTM, tot$FTA)
  tot$`Pts/Shot` <- ifelse(tot$FGA > 0, round((tot$PTS - tot$FTM) / tot$FGA, 2), NA)
  tot$Player <- "TEAM TOTALS"; tot$player_id <- NA; tot$Team <- side; tot$GP <- NA; tot$`+/-` <- NA
  rbind(bx, as.data.frame(tot, check.names = FALSE)[names(bx)])
}

per_game <- function(bx) {
  if (is.null(bx)) return(NULL)
  cnt <- setdiff(names(bx)[sapply(bx, is.numeric)], c("player_id", "GP", grep("%$|/Shot", names(bx), value = TRUE)))
  bx[cnt] <- round(bx[cnt] / pmax(bx$GP, 1), 1); bx
}

# ---- Game flow: running score, leads, runs ----------------------------------
clock_secs <- function(cl) {
  cl <- trimws(cl %||% ""); out <- rep(NA_real_, length(cl))
  ok <- grepl("^[0-9]+(:[0-9]{1,2}(\\.[0-9]+)?)?$", cl)
  p <- strsplit(cl[ok], ":")
  out[ok] <- vapply(p, function(z) as.numeric(z[1]) * 60 + (if (length(z) > 1) as.numeric(z[2]) else 0), 0)
  out
}
period_index <- function(p, periods) {
  num <- suppressWarnings(as.numeric(p))
  ifelse(!is.na(num), num, periods + suppressWarnings(as.numeric(sub("OT", "", p))))
}
period_order <- function(p) { p <- unique(p); p[order(period_index(p, 99))] }

game_flow <- function(ev, g) {
  s <- ev[ev$pts != 0, ]
  if (!nrow(s)) return(NULL)
  s$us <- cumsum(ifelse(s$team == "us", s$pts, 0)); s$opp <- cumsum(ifelse(s$team == "opp", s$pts, 0))
  s$margin <- s$us - s$opp
  sg <- sign(s$margin); nz <- sg[sg != 0]
  lead_changes <- if (length(nz) > 1) sum(diff(nz) != 0) else 0
  ties <- sum(s$margin == 0)
  r <- rle(s$team); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
  runs <- data.frame(Team = r$values, Points = mapply(function(a, b) sum(s$pts[a:b]), starts, ends),
                     start = starts, end = ends,
                     From = paste0(s$us[starts] - ifelse(s$team[starts] == "us", s$pts[starts], 0), "-",
                                   s$opp[starts] - ifelse(s$team[starts] == "opp", s$pts[starts], 0)),
                     To = paste0(s$us[ends], "-", s$opp[ends]), Period = s$period[starts])
  # x axis: elapsed game minutes when every scoring play has a clock, else scoring-play number
  plen <- g$period_min %||% 8; np <- g$periods %||% 4
  otlen <- if (grepl("NFHS|High", g$format %||% "")) 4 else 5
  rem <- clock_secs(s$clock); pi <- period_index(s$period, np)
  if (all(!is.na(rem)) && all(!is.na(pi))) {
    before <- ifelse(pi <= np, (pi - 1) * plen, np * plen + (pi - np - 1) * otlen)
    len <- ifelse(pi <= np, plen, otlen)
    s$t <- before + (len * 60 - rem) / 60; xlab <- "Game minute"
  } else { s$t <- seq_len(nrow(s)); xlab <- "Scoring play #" }
  list(s = s, runs = runs, lead_changes = lead_changes, ties = ties, xlab = xlab,
       big_us = max(c(0, s$margin)), big_opp = max(c(0, -s$margin)),
       run_us = max(c(0, runs$Points[runs$Team == "us"])), run_opp = max(c(0, runs$Points[runs$Team == "opp"])))
}

# ---- Lineups ----------------------------------------------------------------
lineup_report <- function(ev, players, side, weights, min_poss = 3) {
  lab <- player_labels(players); col <- if (side == "us") "on_court" else "opp_on_court"
  ev <- ev[ev[[col]] != "", ]
  if (!nrow(ev)) return(data.frame())
  key <- lineup_key(ev[[col]]); sp <- split(seq_len(nrow(ev)), key)
  pt <- poss_table(ev); ptkey <- if (side == "us") pt$lu_us else pt$lu_opp
  S <- if (side == "us") "Us" else "Opp"; O <- if (side == "us") "Opp" else "Us"
  out <- do.call(rbind, lapply(names(sp), function(k) {
    d <- ev[sp[[k]], ]; tm <- team_metrics(d, weights); g <- function(m, s) tm_get(tm, m, s)
    dp <- pt[ptkey == k & pt$offense == other(side), ]
    ids <- strsplit(k, ",")[[1]]
    data.frame(Lineup = paste(sub(" .*", "", lab[ids]), collapse = " "),
               Players = paste(lab[ids], collapse = ", "),
               `Off Poss` = g("Possessions", S), `Def Poss` = g("Possessions", O),
               `Pts For` = g("PTS", S), `Pts Agst` = g("PTS", O), `+/-` = g("PTS", S) - g("PTS", O),
               `Off PPP` = g("PPP", S), `Def PPP` = g("PPP", O),
               `Net /100` = round(100 * (g("PTS", S) / max(g("Possessions", S), 1) -
                                         g("PTS", O) / max(g("Possessions", O), 1)), 1),
               `REB%` = g("REB%", S), Stops = sum(dp$points == 0), `Stop %` = pct(sum(dp$points == 0), nrow(dp)),
               `Forced TO%` = g("BH Error %", O), check.names = FALSE)
  }))
  out[(out$`Off Poss` + out$`Def Poss`) >= min_poss, ]
}
LINEUP_SORTS <- list("Most used" = function(d) -(d$`Off Poss` + d$`Def Poss`),
                     "Most productive (net per 100)" = function(d) -d$`Net /100`,
                     "Best rebounding (REB%)" = function(d) -d$`REB%`,
                     "Best defensive (lowest Def PPP)" = function(d) d$`Def PPP`,
                     "Best at defensive stops" = function(d) -d$`Stop %`,
                     "Best at forcing turnovers" = function(d) -d$`Forced TO%`)

# ---- Goals ------------------------------------------------------------------
eval_goals <- function(tm, goals) {
  if (!nrow(goals)) return(NULL)
  vals <- mapply(function(m, s) { v <- tm_get(tm, m, s); if (length(v)) v else NA }, goals$metric, goals$side)
  met <- mapply(cmp_ok, vals, goals$comparator, goals$target)
  data.frame(Goal = goals$label, Target = paste(goals$side, goals$metric, goals$comparator, goals$target),
             Actual = unname(vals), Met = ifelse(is.na(met), "-", ifelse(met, "\u2714 Met", "\u2718 Missed")),
             met = unname(met), check.names = FALSE)
}
cmp_ok <- function(v, cmp, tg) if (is.na(v)) NA else
  switch(cmp, ">=" = v >= tg, "<=" = v <= tg, ">" = v > tg, "<" = v < tg, "=" = v == tg)

# ---- Shot tables ------------------------------------------------------------
shot_table <- function(fg, by, levels = NULL) {
  if (!nrow(fg)) return(data.frame())
  g <- fg[[by]]; g[is.na(g) | g == ""] <- "Unrated"
  lv <- unique(c(levels, sort(unique(g)))); lv <- lv[lv %in% g]
  out <- do.call(rbind, lapply(lv, function(k) {
    d <- fg[g == k, ]
    data.frame(Group = k, FGA = nrow(d), FGM = sum(d$made %in% 1), `FG%` = pct(sum(d$made %in% 1), nrow(d)),
               Points = sum(d$pts), `Pts/Shot` = round(sum(d$pts) / nrow(d), 2),
               `Share of FGA %` = pct(nrow(d), nrow(fg)), `Share of Pts %` = pct(sum(d$pts), sum(fg$pts)),
               check.names = FALSE)
  }))
  names(out)[1] <- c(rating = "Rating", zone = "Zone", range = "Range", shot_type = "Shot type")[by] %||% by
  out
}

describe_event <- function(ev, lab, team_names) {
  who <- ifelse(is.na(ev$player_id), paste(team_names[ev$team], "(team)"), lab[as.character(ev$player_id)])
  what <- ifelse(ev$type == "FG",
                 paste0(ev$points, "PT ", ifelse(ev$made %in% 1, "made", "miss"),
                        ifelse(is.na(ev$shot_type), "", paste0(" | ", ev$shot_type)),
                        ifelse(is.na(ev$rating), "", paste0(" | ", ev$rating))),
          ifelse(ev$type == "FT", paste("FT", ifelse(ev$made %in% 1, "made", "miss")),
          c(OREB = "off rebound", DREB = "def rebound", AST = "assist", AST_ATT = "assist attempt",
            STL = "steal", BLK = "block", DAST = "defensive assist", TO = "turnover", PF = "foul",
            FD = "foul drawn", CHG = "charge taken", OUTLET = "outlet pass", TRANS = "fast break", TIMEOUT = "timeout", JUMP = "jump ball - gains possession",
            ADJ = "score correction", POSS = "possession starts", SHOTLOC = "shot spot")[ev$type]))
  if (!is.null(ev$target_id))
    what <- ifelse(ev$type == "PASS", paste("pass to", ifelse(is.na(ev$target_id), "?", lab[as.character(ev$target_id)])), what)
  what <- ifelse(ev$type == "TOUCH", paste("gets the ball", ifelse(is.na(ev$tag), "", paste0("(", ev$tag, ")"))), what)
  what <- ifelse(ev$type == "DRIVE", "drives", ifelse(ev$type == "POST", "posts up", what))
  what <- ifelse(ev$type == "POSS", paste("ball -", ifelse(is.na(POSS_START[ev$tag]), "possession", POSS_START[ev$tag])), what)
  what <- ifelse(ev$type == "ADJ", sprintf("score correction %+d", as.integer(ev$points)), what)
  what <- ifelse(ev$type == "JUMP" & ev$tag %in% "tip", "wins the opening tip", what)
  what <- paste0(what, ifelse(ev$type == "OUTLET" & ev$tag %in% "push", " -> transition push",
                       ifelse(ev$type == "OUTLET" & ev$tag %in% "setup", " -> set up half-court",
                       ifelse(ev$type == "FG" & ev$tag %in% "trans", " | transition", ""))))
  paste(who, "-", what)
}
