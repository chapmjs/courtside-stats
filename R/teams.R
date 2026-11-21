# =============================================================================
# CourtSide Stats v5 - several teams of your own (Varsity, JV, a team you scout)
# A player record is one person; team_players says which of your teams they are
# on, so a JV player called up to varsity is the same player on both rosters.
# Each game belongs to one of your teams (games.team_id). Goals can apply to one
# team or to all (goals.team_id NULL). Opponent rosters are shared by name.
# =============================================================================

migrate_v5 <- function(con) {
  dbExecute(con, "CREATE TABLE IF NOT EXISTS teams (team_id INTEGER PRIMARY KEY AUTOINCREMENT,
                  name TEXT, active INTEGER DEFAULT 1)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS team_players (team_id INTEGER, player_id INTEGER,
                  PRIMARY KEY (team_id, player_id))")
  for (t in c("games", "goals"))
    if (!"team_id" %in% dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name)
      dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN team_id INTEGER", t))
  if (dbGetQuery(con, "SELECT COUNT(*) n FROM teams")$n == 0) {
    nm <- dbGetQuery(con, "SELECT value FROM settings WHERE key = 'team_name'")$value
    dbExecute(con, "INSERT INTO teams (name) VALUES (?)", params = list(if (length(nm) && nzchar(nm)) nm else "Our Team"))
    tid <- last_id(con)
    dbExecute(con, "UPDATE games SET team_id = ? WHERE team_id IS NULL", params = list(tid))
    dbExecute(con, "INSERT OR IGNORE INTO team_players SELECT ?, player_id FROM players WHERE squad = 'us'",
              params = list(tid))
  }
  dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('schema', '5')")
}

team_roster <- function(con, team_id, active_only = TRUE) {
  dbGetQuery(con, sprintf("SELECT p.* FROM players p JOIN team_players tp USING (player_id)
                           WHERE tp.team_id = ? %s ORDER BY CAST(p.number AS INTEGER)",
                          if (active_only) "AND p.active = 1" else ""), params = list(as.integer(team_id)))
}

add_team <- function(con, name) {
  dbExecute(con, "INSERT INTO teams (name) VALUES (?)", params = list(name)); last_id(con)
}

add_to_team <- function(con, team_id, player_id)
  dbExecute(con, "INSERT OR IGNORE INTO team_players VALUES (?, ?)", params = list(as.integer(team_id), as.integer(player_id)))

# Find-or-create our players by number+name and put them on a team
ensure_team_roster <- function(con, team_id, numbers, names) {
  ids <- integer(0)
  for (i in seq_along(numbers)) {
    hit <- dbGetQuery(con, "SELECT player_id FROM players WHERE squad = 'us' AND number = ? AND IFNULL(name,'') = ?",
                      params = list(as.character(numbers[i]), names[i]))
    pid <- if (nrow(hit)) hit$player_id[1] else {
      dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,'','us')",
                params = list(as.character(numbers[i]), names[i])); last_id(con) }
    add_to_team(con, team_id, pid); ids <- c(ids, pid)
  }
  ids
}

team_id_by_name <- function(con, name) {
  t <- dbGetQuery(con, "SELECT team_id FROM teams WHERE name = ?", params = list(name))
  if (nrow(t)) t$team_id[1] else add_team(con, name)
}
