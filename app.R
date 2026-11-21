# =============================================================================
# CourtSide Stats v5 - Shiny basketball statistics (several teams, both sides tracked, several keepers)
# Files: app.R (UI + server), R/engine.R, R/charts.R, R/reports.R, R/data_io.R
# app.R sources everything in R/ itself, so older Shiny versions work too.
# Data: bball.sqlite next to this file (override with env var COURTSIDE_DB).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- Startup checks: write a clear message to the server log instead of a bare failure ----
need <- c("shiny", "DBI", "RSQLite", "DT", "ggplot2", "htmltools")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("CourtSide Stats: missing R packages: ", paste(missing, collapse = ", "),
                          ". Install them as root so the shiny user can see them, e.g. sudo Rscript -e ",
                          "'install.packages(c(\"", paste(missing, collapse = "\", \""), "\"), repos = \"https://cloud.r-project.org\")'", call. = FALSE)
if (getRversion() < "3.6.0") stop("CourtSide Stats needs R 3.6 or newer (this server has ", getRversion(), ").", call. = FALSE)
if (packageVersion("ggplot2") < "3.4.0")
  warning("CourtSide Stats: ggplot2 ", packageVersion("ggplot2"), " is older than 3.4; charts may fail. Update ggplot2.", call. = FALSE)
suppressPackageStartupMessages({
  library(shiny); library(DBI); library(RSQLite); library(DT); library(ggplot2)
})
# xml2 is only needed for XML import/export; the app runs without it
HAS_XML <- suppressWarnings(suppressPackageStartupMessages(require(xml2, quietly = TRUE)))
if (!HAS_XML) message("CourtSide Stats: xml2 could not be loaded, so XML import/export is turned off. ",
                      "Fix with: sudo apt install libxml2-dev, then reinstall xml2 as root.")
# Load helper files explicitly (older Shiny versions do not auto-load the R/ folder)
for (f in sort(list.files("R", pattern = "[.][Rr]$", full.names = TRUE))) source(f, encoding = "UTF-8")
DB_PATH <- Sys.getenv("COURTSIDE_DB", file.path(getwd(), "bball.sqlite"))

css <- "
.pick .radio-inline, .pick .checkbox-inline {padding-left:0; margin:0 4px 6px 0;}
.pick input {position:absolute; opacity:0; width:0; height:0;}
.pick input + span {display:inline-block; min-width:52px; padding:9px 10px; border:2px solid #52514e;
  border-radius:8px; text-align:center; font-weight:600; cursor:pointer; background:#fff;}
.pick input:checked + span {background:#0b0b0b; color:#fff; border-color:#0b0b0b;}
.keepers {padding-top:24px; color:#52514e; font-size:12px;} .keepers b {color:#0b0b0b;}
/* scoreboard */
.sb {background:#0b0b0b; color:#fff; border-radius:10px; padding:8px 10px; text-align:center; margin-bottom:8px;}
.sb .tname {font-size:16px; font-weight:700;} .sb .score {font-size:44px; font-weight:800; line-height:1.05;}
.sb-us .tname {color:#8fbcf0;} .sb-opp .tname {color:#f5a47f;}
.sb .info {font-size:13px; color:#d6d5d0;} .sb .bonus {color:#ffd166; font-weight:800; margin-left:6px;}
.sb .btn {padding:3px 8px; font-size:12px; margin-top:4px;}
.sb-mid {background:#1f1f1e;} .sb-mid .form-group {margin-bottom:4px;}
.sb-mid select {background:#2b2b29; color:#fff; border-color:#52514e; height:30px; padding:2px 6px;}
#clock_disp {font-family:monospace; font-size:40px; font-weight:800; color:#ffd166; line-height:1.1;}
#clock_toggle.running {background:#d03b3b; border-color:#d03b3b; color:#fff;}
#clock_toggle {background:#0ca30c; border-color:#0ca30c; color:#fff; font-weight:700; min-width:110px;}
/* roster tiles */
.roster h4 {font-weight:800; margin:4px 0;} .roster-us h4 {color:#2a78d6;} .roster-opp h4 {color:#c4501f;}
.tiles {display:flex; flex-wrap:wrap; gap:5px; margin-bottom:6px;}
.tile {width:58px; padding:5px 2px; border:2px solid #a8a7a1; border-radius:8px; text-align:center;
  cursor:pointer; background:#fff; user-select:none; line-height:1.1;}
.tile .num {display:block; font-size:20px; font-weight:800;} .tile .nm {display:block; font-size:10px; color:#52514e; overflow:hidden; white-space:nowrap;}
.tile.bench {opacity:0.55; width:50px;}
.plist .tile {width:100%; display:flex; align-items:baseline; gap:8px; text-align:left; padding:6px 8px;}
.plist .tile .num {display:inline; font-size:18px; min-width:30px;} .plist .tile .nm {display:inline; font-size:13px;}
.plist {display:flex; flex-direction:column; gap:4px;} .tile.bench .num {font-size:15px;}
.roster-us .tile.on {border-color:#2a78d6;} .roster-opp .tile.on {border-color:#eb6834;}
.roster-us .tile.sel {background:#2a78d6; color:#fff;} .roster-opp .tile.sel {background:#eb6834; color:#fff;}
.tile.sel .nm {color:#fff;}
.submode .tile {border-style:dashed;}
.tile.outpending {border:3px solid #d03b3b !important; background:#f6d4d4;}
.dimside {opacity:0.4; pointer-events:none;}
.mgr {background:#f0efec; border-radius:8px; padding:10px 14px; margin-bottom:8px;} .mgr li {margin-bottom:4px;} .subnote {font-size:12px; color:#d03b3b; font-weight:700;}
.recent {font-size:11.5px; color:#52514e; border-top:1px solid #e6e5e0; padding-top:4px; margin-top:4px;}
.recent div {white-space:nowrap; overflow:hidden; text-overflow:ellipsis;}
.lineup-warn {color:#d03b3b; font-weight:600; font-size:12px;}
/* action bar */
.actbar {display:flex; flex-wrap:wrap; gap:5px; margin-top:6px;}
.actbar > .shiny-panel-conditional {display:contents;}
.actbar .btn {min-width:82px; padding:9px 6px; font-weight:700;}
.status {font-size:14px; font-weight:600; padding:5px 8px; background:#f0efec; border-radius:6px; margin-top:4px;}
.ctrlrow {display:flex; flex-wrap:wrap; gap:14px; align-items:center;} .ctrlrow .form-group {margin-bottom:0;}
"

js <- "
function csPick(team, pid) {
  document.querySelectorAll('.tile').forEach(function(e) { e.classList.remove('sel'); });
  var el = document.getElementById('tile_' + team + '_' + pid); if (el) el.classList.add('sel');
  Shiny.setInputValue('pick', {team: team, pid: pid, n: Date.now()}, {priority: 'event'});
}
window.csClock = null;
document.addEventListener('keydown', function(e) {
  if (e.code !== 'Space' && e.key !== ' ') return;
  var t = e.target, tag = t.tagName;
  if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || t.isContentEditable) return;
  var r = document.getElementById('role'); if (!r || ['all', 'clock'].indexOf(r.value) < 0) return;
  var b = document.getElementById('clock_toggle'); if (!b) return;
  e.preventDefault(); if (document.activeElement) document.activeElement.blur(); b.click();
});
Shiny.addCustomMessageHandler('cs_clock', function(m) { window.csClock = {rem: m.rem, run: m.run, t0: performance.now()}; });
setInterval(function() {
  var c = window.csClock, el = document.getElementById('clock_disp'), b = document.getElementById('clock_toggle');
  if (!c || !el) return;
  if (b) { b.textContent = c.run ? 'Stop clock' : 'Start clock'; b.classList.toggle('running', !!c.run); }
  if (c.rem < 0) { el.textContent = '--:--'; return; }
  var r = c.run ? Math.max(0, c.rem - (performance.now() - c.t0) / 1000) : c.rem;
  var m = Math.floor(r / 60), s = Math.floor(r % 60);
  el.textContent = (r < 60 && c.run) ? (r.toFixed(1)) : (m + ':' + (s < 10 ? '0' : '') + s);
}, 100);
"

roster_col <- function(s) div(class = paste0("roster roster-", s),
  h4(textOutput(paste0(s, "_title"), inline = TRUE)),
  uiOutput(paste0("tiles_", s)),
  conditionalPanel(sprintf("['all','poss','%s'].indexOf(input.role) >= 0 && input.track_lineups", s),
    actionButton(paste0("sub_", s), "Substitute", icon = icon("right-left"), class = "btn-sm")),
  if (s == "opp") div(style = "margin-top:6px;",
    textInput("opp_add", NULL, placeholder = "Add #s: 3, 11 Jones"),
    actionButton("opp_add_btn", "Add to roster", class = "btn-sm")),
  div(class = "recent", uiOutput(paste0("recent_", s))))

scoreboard_side <- function(s) div(class = paste0("sb sb-", s),
  div(class = "tname", textOutput(paste0("sb_name_", s), inline = TRUE)),
  div(class = "score", textOutput(paste0("sb_score_", s), inline = TRUE)),
  div(class = "info", uiOutput(paste0("sb_info_", s), inline = TRUE)),
  actionButton(paste0("timeout_", s), "Timeout", class = "btn-default"))

live_tab <- tabPanel("Live Entry",
  tags$head(tags$link(rel = "stylesheet", href = "live.css")),
  div(id = "cs-live"), tags$script(src = "live.js"))

entry_tab <- tabPanel("Classic Entry",
  tags$head(tags$script(HTML(js))),
  fluidRow(
    column(3, selectInput("game_id", "Game", choices = NULL, width = "100%")),
    column(1, br(), actionButton("new_game", "New", icon = icon("plus"))),
    column(2, textInput("keeper", "Your name", placeholder = "e.g. John")),
    column(3, selectInput("role", "Your job", ROLES, width = "100%")),
    column(3, div(class = "keepers", uiOutput("keepers_ui")))),
  fluidRow(
    column(4, scoreboard_side("us")),
    column(4, div(class = "sb sb-mid",
      fluidRow(column(6, selectInput("period", NULL, choices = as.character(1:4))),
               column(6, div(class = "info", style = "padding-top:6px;", textOutput("period_lbl", inline = TRUE)))),
      div(id = "clock_disp", "--:--"),
      conditionalPanel("['all','clock'].indexOf(input.role) >= 0",
        actionButton("clock_toggle", "Start clock"),
        actionButton("clock_minus", "-1s", class = "btn-default"), actionButton("clock_plus", "+1s", class = "btn-default"),
        div(style = "display:inline-block; width:70px; vertical-align:top;", textInput("clock_set_txt", NULL, placeholder = "m:ss")),
        actionButton("clock_set", "Set", class = "btn-default")),
      div(style = "margin-top:6px;",
        actionButton("jump_ball", "Jump ball", icon = icon("arrows-up-down"), class = "btn-default"),
        actionButton("adjust_score", "Fix score", icon = icon("pen-to-square"), class = "btn-default")),
      div(class = "info", style = "margin-top:4px;", textOutput("arrow_txt", inline = TRUE)))),
    column(4, scoreboard_side("opp"))),
  fluidRow(
    column(2, roster_col("us")),
    column(8,
      conditionalPanel("input.role == 'clock'", div(class = "mgr",
        h4("Game manager: clock and substitutions"),
        tags$ul(
          tags$li(tags$b("Space bar"), " (or the green button) starts and stops the clock. Stop it on every whistle; start it when play resumes."),
          tags$li("Timeout buttons stop the clock for you. Changing the period resets it."),
          tags$li(tags$b("Substitutions:"), " tap the player coming out, then the player going in. Tap a highlighted player again to send her to the bench."),
          tags$li("At timeouts and quarter breaks, glance at the gym scoreboard and use Set if the time has drifted."),
          tags$li("Jump ball and Fix score are under the clock if the other keeper is busy.")),
          )),
      conditionalPanel("input.role != 'clock'",
      div(class = "ctrlrow",
        div(class = "pick", radioButtons("court_view", NULL, c("Full court" = "full", "Half court" = "half"), inline = TRUE)),
        conditionalPanel("input.court_view == 'half'",
          div(class = "pick", radioButtons("rec_team", NULL, c("Us" = "us", "Opponent" = "opp"), inline = TRUE))),
        conditionalPanel("input.court_view == 'full'", actionButton("switch_sides", "Switch baskets", icon = icon("arrows-left-right"), class = "btn-sm")),
        checkboxInput("prompts", "Statistician prompts", TRUE),
        checkboxInput("track_lineups", "Track who's on the court (for +/- and lineups)", FALSE)),
      uiOutput("court_ui"),
      div(class = "status", textOutput("status", inline = TRUE)),
      div(class = "actbar", lapply(names(ACTIONS), function(k) {
        roles <- names(ROLE_ACTIONS)[sapply(ROLE_ACTIONS, function(a) k %in% a)]
        conditionalPanel(sprintf("input.role == 'all' || ['%s'].indexOf(input.role) >= 0", paste(roles, collapse = "','")),
          actionButton(paste0("act_", k), ACTIONS[[k]], class = "btn-default"))
      }))),
      div(style = "margin-top:8px;",
        actionButton("undo", "Undo my last", icon = icon("rotate-left"), class = "btn-warning"),
        actionButton("edit_sel", "Edit selected play", icon = icon("pen")),
        actionButton("del_sel", "Delete selected play", icon = icon("trash"), class = "btn-danger"))),
    column(2, roster_col("opp"))),
  hr(), h4("Play-by-play"), DTOutput("pbp"))

reports_tab <- tabPanel("Reports",
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("rep_type", "Report", c("Game", "Season / cumulative", "Player")),
      conditionalPanel("input.rep_type == 'Game'", selectInput("rep_game", "Game", choices = NULL)),
      conditionalPanel("input.rep_type != 'Game'",
        selectizeInput("rep_games", "Games included", choices = NULL, multiple = TRUE),
        helpText("Leave empty to include every game.")),
      conditionalPanel("input.rep_type == 'Player'", selectInput("rep_player", "Player", choices = NULL),
        checkboxInput("pl_all_teams", "Include games for all your teams (e.g. JV and varsity)", FALSE)),
      hr(), downloadButton("dl_box", "Box scores (CSV)"), br(), br(),
      downloadButton("dl_events", "Event log (CSV)"),
      helpText("To print a report, use your browser's Print command.")),
    mainPanel(width = 9, uiOutput("report_ui"))))

settings_tab <- tabPanel("Rosters & Settings",
  fluidRow(
    column(5, h4("Your teams"),
      helpText("Switch teams with the Team menu at the top. Games, rosters, goals and reports follow the team you pick."),
      fluidRow(column(8, textInput("team_name", "Rename this team")), column(4, br(), actionButton("save_team", "Save"))),
      fluidRow(column(8, textInput("new_team", "Add a team", placeholder = "e.g. JV, Sophomore, a team you scout")),
               column(4, br(), actionButton("add_team_btn", "Add team", icon = icon("plus")))),
      hr(), h4("Rosters"), selectInput("roster_squad", "Roster", choices = c("This team" = "us")),
      fluidRow(column(3, textInput("p_num", "#")), column(6, textInput("p_name", "Name")), column(3, textInput("p_pos", "Pos"))),
      actionButton("add_player", "Add new player", icon = icon("user-plus")),
      actionButton("toggle_player", "Activate / deactivate selected"),
      actionButton("delete_player", "Delete selected", icon = icon("trash"), class = "btn-danger btn-sm"),
      helpText("To fix a number, name or position, double-click it in the table, type the change, and press Enter."),
      conditionalPanel("input.roster_squad == 'us'",
        fluidRow(style = "margin-top:8px;",
          column(8, selectInput("add_existing", "Add a player from another of your teams", choices = NULL)),
          column(4, br(), actionButton("add_existing_btn", "Add to this team"))),
        actionButton("remove_from_team", "Remove selected from this team", class = "btn-sm")),
      br(), br(), DTOutput("roster_tbl"), hr(),
      actionButton("demo", "Load demo data (Varsity + JV)", class = "btn-info"), hr(),
      h4("Transition"),
      checkboxInput("auto8", "Also count a possession as transition when its first shot or turnover comes within 8 seconds of a defensive rebound or steal (needs clock times on every play)", FALSE),
      helpText("Otherwise a possession is transition when a keeper taps Fast Break, marks an outlet as a push, or marks a shot as transition.")),
    column(7, h4("Team goals"),
      fluidRow(column(4, textInput("g_label", "Goal label")), column(2, selectInput("g_side", "Side", c("Us", "Opp"))),
               column(3, selectInput("g_metric", "Metric", METRICS)),
               column(1, selectInput("g_cmp", "", c(">=", "<=", ">", "<", "="))),
               column(2, numericInput("g_target", "Target", 0))),
      div(class = "pick", radioButtons("g_scope", NULL, c("This team only" = "team", "All your teams" = "all"), inline = TRUE)),
      actionButton("add_goal", "Add goal"), actionButton("del_goal", "Delete selected goal"),
      helpText("Side = whose stat it is (Opp FG% = opponent's shooting). Percentages use a 0-100 scale."),
      DTOutput("goals_tbl"), hr(), h4("Productivity (+/- value) weights"),
      helpText("A player's Productivity is the sum of these values over their stats. Double-click a value to edit."),
      DTOutput("weights_tbl"))))

io_tab <- tabPanel("Import / Export",
  uiOutput("xml_note"),
  fluidRow(
    column(6, h4("Export"), selectInput("io_game", "Game", choices = NULL),
      downloadButton("dl_xml", "Export game (XML)"), br(), br(),
      downloadButton("dl_db", "Back up entire database (.sqlite)")),
    column(6, h4("Import"), fileInput("xml_in", "Import a game XML file from CourtSide Stats (v1 or v2)", accept = ".xml"),
      helpText("Players are matched by team, number, and name; anyone not found is added."))))

includeMarkdown_safe <- function() HTML("
<h4>How stats are derived</h4>
<ul>
<li><b>Possessions</b> = FGA - OREB + TO + 0.44 x FTA. <b>PPP</b> = points / possessions. <b>Off Eff</b> = points per 100 possessions; <b>Def Eff</b> = opponent points per 100.</li>
<li><b>Ball-handling error %</b> = turnovers / possessions.</li>
<li><b>OREB%</b> = OREB / (OREB + opponent DREB). <b>DREB%</b> = DREB / (DREB + opponent OREB).</li>
<li>The play sequence is split into possessions (they change on a defensive rebound, turnover, or made basket).
<b>Defensive stops</b> = opponent possessions that scored 0. <b>Second-effort points</b> = points scored after an offensive rebound in the same possession.
<b>Points off turnovers</b> = points in possessions that began with an opponent turnover.</li>
<li><b>Bench points</b> = points by players who were not in the first lineup of the game.</li>
<li><b>Paint points</b> = made shots in the rim or paint zones. <b>Layup points</b> = shot type Layup/Dunk/Putback/Tip-In (or rim zone if no type).</li>
<li><b>Productivity</b> = sum of the weights set under Rosters &amp; Settings. <b>+/-</b> = score margin while on the court.</li>
<li><b>Steal</b> also records the other team's turnover; <b>charge</b> records their turnover and foul; <b>foul drawn</b> records their foul.
Prompts let you name the player; if skipped, the stat goes to the team.</li>
<li><b>Transition</b> possession: a keeper tapped <i>Fast Break</i>, marked an <i>Outlet</i> as pushed, or marked a shot as transition.
Optionally (Settings) also any possession whose first shot or turnover comes within 8 seconds of a defensive rebound or steal, using clock times.
<b>Outlet push %</b> = outlets that led to a transition push. <b>PPP after DREB</b> = points per possession that began with a defensive rebound.</li>
<li><b>mu / theta</b> (Transition &amp; Possessions report): scoring possessions and empty possessions per minute of possession time,
the two competing exponential rates in the possession queueing model. Average length = 1 / (mu + theta). Needs clock times.</li>
<li><b>Live Entry</b> runs in the browser: taps show at once and are saved on the device, then sent to the server.
If the connection drops, plays wait on the device (the badge says how many) and are sent when it reconnects. Don't reload the page while offline.
Pick one or more jobs at the top: <i>Actions</i> (tap a player, then an action; keyboard codes like 12+2, 12-3, 5r, o24f),
<i>Shot spots</i> (tap the court, then the shooter's number; matched to the scorer's made/missed shot),
<i>Possessions</i> (tap who gets the ball and how; this sets the possessions and their length in real time),
and <i>Clock &amp; subs</i>.</li>
<li><b>Ast Att</b> (assist attempt): a pass that set up a good shot that missed. It would have been an assist if the shot went in,
so it credits good passing that the box score normally misses.</li>
<li><b>Def Ast</b> (defensive assist): the defensive play that led to a teammate's steal or forced turnover, such as a
deflection, a trap or a help rotation. The steal goes to the player who got the ball; the defensive assist goes to the player who caused it.</li>
<li><b>Jump ball</b>: the opening tip and held balls. Held balls follow the possession arrow shown under the clock.
<b>Fix score</b> adds a score correction for each team; it changes team points only, not player stats.</li>
<li><b>Track who's on the court</b> (checkbox under the scoreboard): off by default, so each team is one list of players to tap.
Turn it on to keep lineups with the Substitute button; that is what +/-, the lineup report and bench points need.</li>
<li><b>Teams</b>: pick the team at the top of the page. Each game belongs to one team; a player can be on several
of your teams (a JV player who also plays varsity) and keeps one set of stats, which season reports split by team.
Goals can apply to one team or to all of them.</li>
<li><b>Jobs for several keepers</b>: <i>Plays</i> records every stat but not the clock or substitutions;
<i>Clock &amp; subs</i> runs the clock (space bar starts and stops it), timeouts, the period and substitutions, and turns lineup tracking on;
<i>Scorer</i> and <i>Possession</i> split the plays between two people for a three-person crew.</li>
<li><b>One keeper per team</b> (<i>Our team only</i> / <i>Opponent only</i>): each keeper records only what their own players do,
including their turnovers, fouls and rebounds. The app does not add the other team's half of a play (a steal does not add a turnover),
because the other keeper enters it. Possession stats depend on plays being entered in order, so enter plays as they happen.</li>
<li><b>Several keepers</b>: each person opens the app, enters a name and picks a job. Lineups and the period are shared; plays from
everyone appear within about 2 seconds; <i>Undo my last</i> only removes your own plays.</li>
<li><b>Game clock</b>: Start/Stop is shared by every keeper; each play is stamped with the time left. Changing the period resets the clock to that period's length.
A timeout stops the clock. With the clock running, the 8-second transition rule and mu/theta work without typing times.</li>
<li><b>Team fouls / bonus</b>: NFHS, NCAA women, NBA and FIBA count team fouls by quarter; BONUS shows when the next opponent foul gives free throws (5th foul).
NCAA men count by half: BONUS from the 7th foul (one-and-one), DOUBLE BONUS from the 10th. Overtime counts with the second half.</li>
<li><b>Baskets</b>: on the full court each team attacks one basket in the first half and switches at halftime. Use <i>Switch baskets</i> if a game starts the other way.</li>
<li>Timeline uses game minutes when every scoring play has a clock time; otherwise scoring-play order.</li>
</ul>")

help_tab <- tabPanel("Definitions", fluidRow(column(8, includeMarkdown_safe())))


ui <- navbarPage("CourtSide Stats",
  header = tagList(tags$head(tags$style(HTML(paste0(css, "
.teambar {display:flex; align-items:center; gap:10px; margin:-8px 0 8px 0;}
.teambar label {margin:0; font-weight:700;} .teambar .form-group {margin:0;} .teambar select {min-width:200px; font-weight:700;}
")))), div(class = "teambar", tags$label("Team:"), selectInput("team_id", NULL, choices = NULL, width = "240px"))),
                 live_tab, entry_tab, reports_tab, settings_tab, io_tab, help_tab)

# =============================================================================
server <- function(input, output, session) {
  con <- db_connect(DB_PATH); sid <- session$token %||% paste0("s", as.integer(runif(1, 1, 1e9)))
  role <- reactive(input$role %||% "all")
  session$allowReconnect(TRUE)

  # ---- Live Entry sync: snapshots out, batches of plays in ----------------
  live_gid <- reactiveVal(NULL)
  send_snap <- function(gid) if (!is.null(gid) && !is.na(gid))
    session$sendCustomMessage("cs_snap", live_snapshot(con, gid))
  live_games_sig <- reactive({ g <- games(); paste(cur_team(), paste(g$game_id, g$opponent, g$game_date, collapse = ";")) })
  observeEvent(live_games_sig(), {
    g <- games(); lbl <- paste(g$game_date, ifelse(g$location == "Away", "@", "vs"), g$opponent)
    session$sendCustomMessage("cs_games", list(games = data.frame(id = g$game_id, label = lbl), team = team_name()))
  })
  observeEvent(input$live_hello, {
    g <- games(); lbl <- paste(g$game_date, ifelse(g$location == "Away", "@", "vs"), g$opponent)
    session$sendCustomMessage("cs_games", list(games = data.frame(id = g$game_id, label = lbl), team = team_name()))
  })
  observeEvent(input$live_game, {
    gid <- suppressWarnings(as.integer(input$live_game))
    if (length(gid) != 1 || is.na(gid) || !(gid %in% all_games()$game_id)) { live_gid(NULL); return() }
    live_gid(gid); send_snap(gid)
  })
  observeEvent(input$cs_batch, {
    b <- input$cs_batch; gid <- suppressWarnings(as.integer(b$gid))
    if (length(gid) != 1 || is.na(gid)) return()
    done <- live_apply(con, gid, b$ops, b$keeper %||% "Keeper", b$kid %||% sid)
    if (identical(gid, live_gid())) send_snap(gid)
    session$sendCustomMessage("cs_ack", list(oids = I(done)))
    bump()
  })
  live_poll <- reactivePoll(1500, session, checkFunc = function() {
    gid <- isolate(live_gid()); if (is.null(gid)) "" else game_rev(con, gid)
  }, valueFunc = function() isolate(live_gid()))
  observeEvent(live_poll(), send_snap(live_gid()), ignoreInit = TRUE)
  onStop(function() { try(dbExecute(con, "DELETE FROM keepers WHERE session_id = ?", params = list(sid)), silent = TRUE)
                      dbDisconnect(con) })
  rv <- reactiveValues(v = 0, shot = NULL, prompt = NULL)
  bump <- function() rv$v <- rv$v + 1

  players <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM players ORDER BY squad, CAST(number AS INTEGER)") })
  teams <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM teams WHERE active = 1 ORDER BY team_id") })
  observe({ t <- teams(); sel <- isolate(input$team_id); ch <- setNames(as.character(t$team_id), t$name)
    updateSelectInput(session, "team_id", choices = ch, selected = if (!is.null(sel) && sel %in% ch) sel else ch[1]) })
  cur_team <- reactive({ t <- input$team_id; if (is.null(t) || !nzchar(t)) teams()$team_id[1] else as.integer(t) })
  all_games <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM games ORDER BY game_date DESC, game_id DESC") })
  games   <- reactive({ g <- all_games(); g[g$team_id %in% cur_team(), ] })
  goals   <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM goals WHERE team_id IS NULL OR team_id = ?",
                                         params = list(as.integer(cur_team()))) })
  weights <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM weights") })
  team_name <- reactive({ n <- teams()$name[teams()$team_id == cur_team()]; if (length(n)) n else "Our Team" })
  lab <- reactive(player_labels(players()))
  has_game <- reactive(!is.null(input$game_id) && nzchar(input$game_id))
  cur_game <- reactive({ req(has_game()); games()[games()$game_id == as.integer(input$game_id), ] })
  cur_ev   <- reactive({ rv$v; req(has_game()); get_events(con, as.integer(input$game_id)) })
  roster <- function(squad) {
    if (identical(squad, "us")) { rv$v; return(team_roster(con, cur_team())) }
    p <- players(); p[p$squad == squad & p$active == 1, ] }
  opp_squad <- reactive(if (has_game() && nrow(cur_game())) cur_game()$opponent else "")
  names2 <- reactive(c(team_name(), if (has_game() && nrow(cur_game())) cur_game()$opponent else "Opponent"))


  # ---- choice lists -----------------------------------------------------
  game_choices <- reactive({ g <- games()
    setNames(g$game_id, paste(g$game_date, ifelse(g$location == "Away", "@", "vs"), g$opponent)) })
  observe({
    ch <- game_choices(); sel <- isolate(input$game_id); if (is.null(sel) || !(sel %in% ch)) sel <- ch[1]
    updateSelectInput(session, "game_id", choices = ch, selected = sel)
    rs <- isolate(input$rep_game); updateSelectInput(session, "rep_game", choices = ch, selected = if (!is.null(rs) && rs %in% ch) rs else ch[1])
    updateSelectizeInput(session, "rep_games", choices = ch, selected = isolate(input$rep_games))
    updateSelectInput(session, "io_game", choices = ch)
    sq <- unique(players()$squad); sq <- sq[sq != "us"]
    updateSelectInput(session, "roster_squad", choices = c(setNames("us", paste("Our team:", team_name())), setNames(sq, sq)),
                      selected = isolate(input$roster_squad))
    pl <- players(); mine <- team_roster(con, cur_team(), active_only = FALSE)$player_id
    pl <- pl[(pl$squad == "us" & pl$player_id %in% mine) | pl$squad %in% unique(games()$opponent), ]
    grp <- split(setNames(pl$player_id, lab()[as.character(pl$player_id)]),
                 ifelse(pl$squad == "us", team_name(), pl$squad))
    updateSelectInput(session, "rep_player", choices = lapply(grp, as.list), selected = isolate(input$rep_player))
  })
  observe({ updateTextInput(session, "team_name", value = team_name()) })
  # ---- shared game state: lineups, period, clock and baskets live in the database ----
  gstate <- reactive({
    rv$v; req(has_game()); gid <- as.integer(input$game_id)
    gs <- dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
    if (!nrow(gs)) {
      f5 <- function(r) paste(head(r$player_id, 5), collapse = ",")
      dbExecute(con, "INSERT OR IGNORE INTO game_state (game_id, us, opp, period, version, us_left, clock_run)
                      VALUES (?,?,?,?,0,1,0)", params = list(gid, f5(roster("us")), f5(roster(opp_squad())), "1"))
      gs <- dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
    }
    gs
  })
  set_state <- function(...) {
    a <- list(...)
    dbExecute(con, sprintf("UPDATE game_state SET %s, version = version + 1 WHERE game_id = ?",
                           paste(sprintf("%s = ?", names(a)), collapse = ", ")),
              params = c(unname(a), list(as.integer(input$game_id)))); bump()
  }
  # Lineup tracking is optional. When it's off, every player on the roster is available to pick
  # and plays are saved without an on-court lineup (so +/- and lineup reports skip them).
  tracking <- reactive({ rv$v; identical(dbGetQuery(con, "SELECT value FROM settings WHERE key='track_lineups'")$value, "1") })
  observe(updateCheckboxInput(session, "track_lineups", value = tracking()))
  observeEvent(input$track_lineups, { v <- if (isTRUE(input$track_lineups)) "1" else "0"
    if (!identical(isolate(tracking()), v == "1")) {
      dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('track_lineups', ?)", params = list(v)); bump() } }, ignoreInit = TRUE)
  lineup_ids <- function(s) { v <- if (has_game()) gstate()[[s]] else ""; ids <- strsplit(v %||% "", ",")[[1]]; ids[nzchar(ids)] }
  oc <- function(s) if (tracking()) lineup_ids(s) else
    as.character(roster(if (s == "us") "us" else opp_squad())$player_id)
  lineup_str <- function(s) if (tracking()) paste(lineup_ids(s), collapse = ",") else ""
  plist <- function(ids) setNames(ids, lab()[ids])
  tn <- reactive(setNames(names2(), c("us", "opp")))
  now <- function() as.numeric(Sys.time())

  # period (changing it resets and stops the clock)
  observe({
    g <- cur_game(); req(nrow(g)); np <- g$periods %||% 4
    updateSelectInput(session, "period", choices = setNames(c(as.character(seq_len(np)), "OT1", "OT2", "OT3"),
                      c(paste(if (np == 2) "Half" else "Period", seq_len(np)), "OT1", "OT2", "OT3")),
                      selected = isolate(gstate()$period))
  })
  observe({ p <- gstate()$period; if (!identical(isolate(input$period), p)) updateSelectInput(session, "period", selected = p) })
  observeEvent(input$period, {
    if (has_game() && !identical(input$period, gstate()$period))
      set_state(period = input$period, clock_run = 0, clock_at = period_secs(input$period, cur_game()), clock_t = now())
  })
  output$period_lbl <- renderText({ g <- cur_game(); req(nrow(g)); g$format })

  # game clock (shared; every keeper's screen counts down locally between updates)
  observe({ gs <- gstate(); r <- clock_rem(gs)
    session$sendCustomMessage("cs_clock", list(rem = if (is.na(r)) -1 else r, run = isTRUE(gs$clock_run == 1))) })
  cur_rem <- function() { gs <- isolate(gstate()); r <- clock_rem(gs); if (is.na(r)) period_secs(gs$period, cur_game()) else r }
  observeEvent(input$clock_toggle, { req(has_game()); gs <- gstate(); r <- cur_rem()
    if (isTRUE(gs$clock_run == 1)) set_state(clock_run = 0, clock_at = r) else set_state(clock_run = 1, clock_at = r, clock_t = now()) })
  observeEvent(input$clock_minus, { req(has_game()); set_state(clock_at = max(0, cur_rem() - 1), clock_t = now()) })
  observeEvent(input$clock_plus,  { req(has_game()); set_state(clock_at = cur_rem() + 1, clock_t = now()) })
  observeEvent(input$clock_set, { req(has_game()); v <- clock_secs(input$clock_set_txt)
    if (is.na(v)) return(showNotification("Type the time as m:ss, e.g. 6:45", type = "warning"))
    set_state(clock_at = v, clock_t = now()); updateTextInput(session, "clock_set_txt", value = "") })
  clock_text <- function() { gs <- isolate(gstate()); fmt_clock(clock_rem(gs)) }

  # which basket each team attacks right now
  np_cur <- reactive(cur_game()$periods %||% 4)
  base_left <- reactive(isTRUE((gstate()$us_left %||% 1) == 1))
  us_left_now <- reactive(us_left_in(gstate()$period, np_cur(), base_left()))
  observeEvent(input$switch_sides, { req(has_game()); set_state(us_left = if (base_left()) 0L else 1L) })

  # ---- live refresh: notice plays and changes made by other keepers ----
  poll <- reactivePoll(1500, session, checkFunc = function() {
    gid <- isolate(input$game_id); if (is.null(gid) || gid == "") return("")
    paste(unlist(dbGetQuery(con, "SELECT COUNT(*), MAX(event_id), TOTAL(player_id), TOTAL(made), TOTAL(points)
                                  FROM events WHERE game_id = ?", params = list(as.integer(gid)))),
          dbGetQuery(con, "SELECT version FROM game_state WHERE game_id = ?", params = list(as.integer(gid)))$version,
          unlist(dbGetQuery(con, "SELECT COUNT(*), MAX(player_id), TOTAL(active) FROM players")),
          dbGetQuery(con, "SELECT value FROM settings WHERE key = 'roster_rev'")$value, collapse = "|")
  }, valueFunc = function() TRUE)
  observeEvent(poll(), bump(), ignoreInit = TRUE)
  keeper_name <- reactive(if (nzchar(trimws(input$keeper %||% ""))) trimws(input$keeper) else "Keeper")
  observe({
    invalidateLater(5000); gid <- if (has_game()) as.integer(input$game_id) else NA
    dbExecute(con, "INSERT OR REPLACE INTO keepers VALUES (?,?,?,?,?)",
              params = list(sid, gid, keeper_name(), role(), now()))
  })
  output$keepers_ui <- renderUI({
    invalidateLater(5000); req(has_game())
    k <- dbGetQuery(con, "SELECT * FROM keepers WHERE game_id = ? AND session_id != ? AND last_seen > ?",
                    params = list(as.integer(input$game_id), sid, now() - 20))
    rl <- setNames(c("everything", "scorer", "possession"), c("all", "scorer", "poss"))
    if (!nrow(k)) return(span("No one else is keeping this game."))
    span("Also keeping: ", HTML(paste(sprintf("<b>%s</b> (%s)", htmltools::htmlEscape(k$name), rl[k$role]), collapse = ", ")))
  })

  # ---- scoreboard ---------------------------------------------------------
  for (s0 in c("us", "opp")) local({ s <- s0
    output[[paste0("sb_name_", s)]] <- renderText(tn()[[s]])
    output[[paste0(s, "_title")]] <- renderText(tn()[[s]])
    output[[paste0("sb_score_", s)]] <- renderText({ if (!has_game()) return("0"); ev <- cur_ev(); sum(ev$pts[ev$team == s]) })
    output[[paste0("sb_info_", s)]] <- renderUI({
      req(has_game()); ev <- cur_ev(); g <- as.list(cur_game()[1, ]); p <- gstate()$period
      f <- team_fouls(ev, s, p, g); b <- bonus_label(team_fouls(ev, other(s), p, g), g)
      tagList(sprintf("Fouls %d | Timeouts %d", f, sum(ev$type == "TIMEOUT" & ev$team == s)),
              if (nzchar(b)) span(class = "bonus", b))
    })
    observeEvent(input[[paste0("timeout_", s)]], {
      if (!need_game()) return()
      E(s, "TIMEOUT")
      if (isTRUE(gstate()$clock_run == 1)) set_state(clock_run = 0, clock_at = cur_rem()) else bump()
      showNotification(paste(tn()[[s]], "timeout"))
    })
  })

  # ---- jump balls and the alternating-possession arrow ----------------------
  # After the opening tip the arrow points to the team that lost it; each held ball
  # goes to the arrow team and the arrow flips. JUMP is recorded for the team that gains the ball.
  arrow_team <- reactive({ req(has_game()); j <- cur_ev(); j <- j[j$type == "JUMP", ]
    if (!nrow(j)) NA else other(j$team[nrow(j)]) })
  output$arrow_txt <- renderText({ a <- tryCatch(arrow_team(), error = function(e) NA)
    if (is.na(a)) "Possession arrow: set by the opening tip" else paste("Possession arrow →", tn()[[a]]) })
  observeEvent(input$jump_ball, {
    if (!need_game()) return()
    a <- arrow_team(); first <- is.na(a)
    showModal(modalDialog(title = if (first) "Opening tip" else "Held ball / jump ball",
      div(class = "pick", radioButtons("jb_kind", NULL, c("Opening tip (jump)" = "tip", "Held ball (alternating possession)" = "held"),
                                       selected = if (first) "tip" else "held", inline = TRUE)),
      div(class = "pick", radioButtons("jb_team", "Possession goes to", setNames(c("us", "opp"), names2()),
                                       selected = if (first) character(0) else a, inline = TRUE)),
      if (!first) helpText(paste("The arrow points to", tn()[[a]], "- they get this one, then the arrow flips.")),
      uiOutput("jb_player_ui"),
      footer = tagList(modalButton("Cancel"), actionButton("save_jump", "Save", class = "btn-primary"))))
  })
  output$jb_player_ui <- renderUI({ t <- input$jb_team; req(t)
    selectInput("jb_player", if (identical(input$jb_kind, "tip")) "Who won the tip (optional)" else "Who gets the ball (optional)",
                c("Not recorded" = "", plist(oc(t)))) })
  observeEvent(input$save_jump, {
    if (is.null(input$jb_team)) return(showNotification("Choose which team gets the ball.", type = "warning"))
    E(input$jb_team, "JUMP", if (nzchar(input$jb_player %||% "")) input$jb_player else NA, tag = input$jb_kind)
    removeModal(); bump()
  })

  # ---- fix the score: adds a correction that changes team points only ------
  observeEvent(input$adjust_score, {
    if (!need_game()) return()
    ev <- cur_ev()
    showModal(modalDialog(title = "Fix the score",
      p("Type the correct score. The app adds a score correction for the difference. It changes the team scores only,",
        "not any player's points or shooting stats, and it shows in the play-by-play, where you can delete it."),
      fluidRow(column(6, numericInput("adj_us", tn()[["us"]], sum(ev$pts[ev$team == "us"]), min = 0)),
               column(6, numericInput("adj_opp", tn()[["opp"]], sum(ev$pts[ev$team == "opp"]), min = 0))),
      footer = tagList(modalButton("Cancel"), actionButton("save_adj", "Save", class = "btn-primary"))))
  })
  observeEvent(input$save_adj, {
    ev <- cur_ev(); n <- 0
    for (s in c("us", "opp")) {
      want <- input[[paste0("adj_", s)]]; if (is.null(want) || is.na(want)) next
      d <- as.integer(want) - sum(ev$pts[ev$team == s])
      if (d != 0) { E(s, "ADJ", points = d); n <- n + 1 }
    }
    removeModal(); bump(); if (n) showNotification("Score corrected.") else showNotification("Score unchanged.")
  })

  # ---- roster tiles: tap to select a player, or substitute in sub mode ----
  rv$sel <- NULL; rv$sub <- c(us = FALSE, opp = FALSE); rv$out <- list(us = NULL, opp = NULL)
  submode <- function(s) isTRUE(rv$sub[[s]]) || identical(role(), "clock")
  # Team-split keeping: each keeper records only what their own team does, so nothing is
  # entered twice and the app does not add the other team's half of a play automatically.
  my_side <- reactive(if (role() %in% c("us", "opp")) role() else NA)
  is_split <- function() !is.na(my_side())
  # The game manager needs lineups, so picking that job turns lineup tracking on
  observeEvent(role(), { if (role() == "clock" && !isolate(tracking())) {
    dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('track_lineups', '1')"); bump()
    showNotification("Lineup tracking is on so substitutions are recorded.") } })
  sel_team <- function() rv$sel$team %||% NA
  sel_pid  <- function() { p <- rv$sel$pid %||% "T"; if (p == "T") NA else p }
  observeEvent(input$pick, {
    pk <- input$pick; s <- pk$team; p <- as.character(pk$pid)
    if (is_split() && s != my_side()) return(showNotification(paste("You keep", tn()[[my_side()]], "- the other keeper records", tn()[[s]], "plays."), type = "warning"))
    if (submode(s)) {
      if (p == "T") return()
      ids <- lineup_ids(s); out <- rv$out[[s]]
      if (p %in% ids) {                       # a player on the court
        if (identical(out, p)) { ids <- setdiff(ids, p); rv$out[[s]] <- NULL }   # tapped twice: to the bench
        else { rv$out[[s]] <- p; return() }                                       # mark as coming out
      } else if (!is.null(out)) { ids[ids == out] <- p; rv$out[[s]] <- NULL }     # swap in
      else if (length(ids) < 5) ids <- c(ids, p)
      else return(showNotification("Tap the player coming out first, then the player going in.", type = "warning"))
      do.call(set_state, setNames(list(paste(ids, collapse = ",")), s))
    } else rv$sel <- list(team = s, pid = p)
  })
  for (s0 in c("us", "opp")) local({ s <- s0
    observeEvent(input[[paste0("sub_", s)]], {
      rv$sub[[s]] <- !rv$sub[[s]]
      updateActionButton(session, paste0("sub_", s), label = if (rv$sub[[s]]) "Done substituting" else "Substitute")
    })
    output[[paste0("tiles_", s)]] <- renderUI({
      req(has_game()); r <- roster(if (s == "us") "us" else opp_squad()); on <- lineup_ids(s); sel <- rv$sel
      sm <- submode(s); out <- rv$out[[s]]
      if (is_split() && s != my_side()) return(div(class = "dimside", div("Kept by the other keeper"),
        div(class = "plist", lapply(seq_len(nrow(r)), function(i) div(class = "tile on", span(class = "num", r$number[i]))))))
      tile <- function(pid, num, nm, cls) {
        is_sel <- !sm && !is.null(sel) && identical(sel$team, s) && identical(as.character(sel$pid), as.character(pid))
        div(id = paste0("tile_", s, "_", pid), class = paste("tile", cls, if (is_sel) "sel",
                                                              if (sm && identical(out, as.character(pid))) "outpending"),
            onclick = sprintf("csPick('%s','%s')", s, pid), span(class = "num", num), span(class = "nm", nm))
      }
      onr <- r[as.character(r$player_id) %in% on, ]; onr <- onr[order(match(as.character(onr$player_id), on)), ]
      benchr <- r[!(as.character(r$player_id) %in% on), ]
      nm <- function(x) ifelse(is.na(x), "", x)
      if (!tracking()) return(div(class = "plist", tile("T", "TEAM", "team stat", "on"),
        lapply(seq_len(nrow(r)), function(i) tile(r$player_id[i], r$number[i], nm(r$name[i]), "on"))))
      div(class = if (sm) "submode",
        if (sm) div(class = "subnote", if (is.null(out)) "Sub: tap the player coming out, then the player going in."
                                       else paste(lab()[out], "coming out - now tap who goes in (or tap her again to bench her).")),
        div(class = "tiles", if (!sm) tile("T", "TEAM", "team stat", "on"),
            lapply(seq_len(nrow(onr)), function(i) tile(onr$player_id[i], onr$number[i], nm(onr$name[i]), "on"))),
        if (length(on) != 5) div(class = "lineup-warn", sprintf("%d on court (expected 5)", length(on))),
        div(class = "tiles", lapply(seq_len(nrow(benchr)), function(i) tile(benchr$player_id[i], benchr$number[i], nm(benchr$name[i]), "bench"))))
    })
    output[[paste0("recent_", s)]] <- renderUI({
      req(has_game()); ev <- cur_ev(); ev <- ev[ev$team == s & is.na(ev$linked_event_id), ]
      ev <- tail(ev, 5); if (!nrow(ev)) return(div("No plays yet"))
      lapply(rev(seq_len(nrow(ev))), function(i) div(paste(ev$clock[i], sub("^.*? - ", "", describe_event(ev[i, ], lab(), tn())),
        if (!is.na(ev$player_id[i])) paste0("(", sub(" .*", "", lab()[as.character(ev$player_id[i])]), ")") else "")))
    })
  })
  output$status <- renderText({
    if (!has_game()) return("Create or select a game to start.")
    s <- sel_team()
    if (is_split() && is.na(s)) return(paste0("You keep ", tn()[[my_side()]], ": record only what your players do. Tap a player, then an action, or click your team's basket for a shot."))
    if (is.na(s)) return("Tap a player (or TEAM) on either side, then tap an action or click the court for a shot.")
    who <- if (is.na(sel_pid())) paste(tn()[[s]], "team") else lab()[sel_pid()]
    side <- if (!identical(input$court_view, "half")) paste0(" | ", tn()[[s]], " shoots at the ", if ((s == "us") == us_left_now()) "left" else "right", " basket") else ""
    paste0("Selected: ", who, side)
  })

  need_game <- function() { if (!has_game()) { showNotification("Create or select a game first.", type = "error"); FALSE } else TRUE }
  # With one keeper per team, link an entry to the other team's half of the same play when it was
  # entered in the last few seconds (their steal -> our turnover, their foul -> our foul drawn, their miss -> our rebound).
  COUNTERPART <- list(TO = c("STL", "CHG"), PF = c("FD", "CHG"), FD = "PF", STL = "TO", CHG = c("TO", "PF"),
                      DREB = "FG_MISS", BLK = "FG_MISS")
  find_counterpart <- function(team, type) {
    want <- COUNTERPART[[type]]; if (is.null(want)) return(NA)
    r <- dbGetQuery(con, "SELECT * FROM events WHERE game_id = ? AND team = ? AND linked_event_id IS NULL
                          ORDER BY event_id DESC LIMIT 8", params = list(as.integer(input$game_id), other(team)))
    if (!nrow(r)) return(NA)
    t <- as.POSIXct(r$created, format = "%Y-%m-%d %H:%M:%OS")
    ok <- !is.na(t) & difftime(Sys.time(), t, units = "secs") < 30 &
          (r$type %in% want | ("FG_MISS" %in% want & r$type == "FG" & r$made %in% 0))
    for (i in which(ok)) {
      kids <- dbGetQuery(con, "SELECT type, team FROM events WHERE linked_event_id = ?", params = list(r$event_id[i]))
      taken <- if (type %in% c("DREB", "BLK")) any(kids$type == type | kids$type == "OREB" & type == "DREB")
               else any(kids$team == team & kids$type == type)
      if (!taken) return(r$event_id[i])
    }
    NA
  }
  E <- function(team, type, player = NA, ...) {
    args <- list(...)
    if (is_split() && is.null(args$linked)) {
      lk <- find_counterpart(team, type)
      if (!is.na(lk)) return(E(team, type, player, linked = lk, ...))
    }
    if (is.null(args$linked) || is.na(args$linked)) dup_check(team, type, player)
    add_event(con, input$game_id, team, type, player,
      period = isolate(gstate()$period) %||% input$period, clock = clock_text(),
      on_court = lineup_str("us"), opp_on_court = lineup_str("opp"),
      entered_by = keeper_name(), session_id = sid, ...)
  }
  # Warn when another keeper entered the same kind of play for the same player in the last few seconds
  dup_check <- function(team, type, player) {
    if (type == "TIMEOUT") return()
    r <- dbGetQuery(con, "SELECT * FROM events WHERE game_id = ? AND session_id IS NOT NULL AND session_id != ?
                          AND team = ? AND type = ? AND linked_event_id IS NULL ORDER BY event_id DESC LIMIT 5",
                    params = list(as.integer(input$game_id), sid, team, type))
    if (!nrow(r)) return()
    t <- as.POSIXct(r$created, format = "%Y-%m-%d %H:%M:%OS")
    pid <- suppressWarnings(as.integer(player %||% NA))
    hit <- r[!is.na(t) & difftime(Sys.time(), t, units = "secs") < 8 &
             ((is.na(r$player_id) & is.na(pid)) | (!is.na(r$player_id) & !is.na(pid) & r$player_id == pid)), ]
    if (nrow(hit)) showNotification(sprintf("Possible duplicate: %s entered the same play a few seconds ago. Use Undo if you both recorded it.",
                                            hit$entered_by[1] %||% "another keeper"), type = "warning", duration = 8)
  }
  set_player <- function(id, p) dbExecute(con, "UPDATE events SET player_id = ? WHERE event_id = ?",
                                          params = list(as.integer(p), id))

  # ---- court and shot entry -------------------------------------------
  output$court_ui <- renderUI(
    if (identical(input$court_view, "half")) plotOutput("court", click = "court_click", height = "470px")
    else plotOutput("court", click = "court_click", height = "430px"))
  output$court <- renderPlot({
    g <- if (has_game() && nrow(cur_game())) as.list(cur_game()[1, ]) else list()
    ev <- if (length(g)) cur_ev() else data.frame(type = character(0), team = character(0))
    fg <- ev[ev$type == "FG", , drop = FALSE]
    if (identical(input$court_view, "half")) {
      fg <- fg[fg$team == input$rec_team, , drop = FALSE]
      return(shot_chart(fg, g, paste("Shots this game:", tn()[[input$rec_team]])))
    }
    if (!length(g)) return(ggplot() + court_full_layers() + coord_fixed(xlim = c(0, 94), ylim = c(0, 50), expand = FALSE) + theme_void())
    fg <- fg[!is.na(fg$x) & second_half(fg$period, np_cur()) == second_half(gstate()$period, np_cur()), , drop = FALSE]
    if (nrow(fg)) fg$side <- ifelse((fg$team == "us") == us_left_in(fg$period, np_cur(), base_left()), "left", "right")
    full_court_plot(fg, g, us_left_now(), names2())
  })

  open_shot <- function(team, shooter, x = NA, y = NA, pts = NULL, made = NULL) {
    o <- other(team); g <- cur_game()
    if (is.null(pts)) pts <- if (is_three(x, y, g$three_dist %||% 19.75, g$corner_dist %||% 19.75)) 3 else 2
    rv$shot <- list(x = x, y = y, team = team)
    mates <- oc(team); defs <- oc(o)
    reb <- if (is_split()) c("Other team or not recorded" = "", setNames(paste0("S:", oc(team)), paste(lab()[oc(team)], "(off)")),
                          setNames("S:T", paste(tn()[[team]], "team (off)")))
      else c("Not recorded" = "", setNames(paste0("S:", oc(team)), paste(lab()[oc(team)], "(off)")),
             setNames(paste0("D:", defs), paste(lab()[defs], "(def)")),
             setNames("S:T", paste(tn()[[team]], "team (off)")), setNames("D:T", paste(tn()[[o]], "team (def)")))
    near <- !is.na(x) && shot_dist(x, y) < 4
    shooter_ch <- c(if (team == "opp") c("Unknown" = "T"), plist(mates))
    showModal(modalDialog(title = paste(tn()[[team]], "shot"),
      selectInput("shot_shooter", "Shooter", shooter_ch,
                  selected = if (!is.na(shooter) && shooter %in% mates) shooter else shooter_ch[1]),
      div(class = "pick", radioButtons("shot_made", NULL, c("Made", "Missed"), inline = TRUE,
                                       selected = if (is.null(made)) character(0) else if (made) "Made" else "Missed")),
      div(class = "pick", radioButtons("shot_pts", "Points", c("2", "3"), selected = as.character(pts), inline = TRUE)),
      div(class = "pick", radioButtons("shot_rating", "Shot rating", SHOT_RATINGS, inline = TRUE, selected = character(0))),
      selectInput("shot_type", "Shot type", c("", SHOT_TYPES), selected = if (near) "Layup" else ""),
      checkboxInput("shot_trans", "Transition / fast-break shot", FALSE),
      conditionalPanel("input.shot_made == 'Made'", selectInput("shot_ast", "Assisted by", c("None" = "", plist(mates)))),
      conditionalPanel("input.shot_made == 'Missed'",
        selectInput("shot_reb", "Rebound", reb),
        if (!is_split()) selectInput("shot_blk", "Blocked by", c("None" = "", plist(defs))),
        selectInput("shot_astatt", "Assist attempt (pass that set up the miss)", c("None" = "", plist(mates))),
        conditionalPanel("input.shot_reb && input.shot_reb.indexOf('D:') == 0",
          selectInput("shot_outlet", "Outlet pass by", c("No outlet / not recorded" = "", plist(defs))),
          div(class = "pick", radioButtons("shot_push", "After the outlet", c("Pushed in transition" = "push",
                                           "Set up half-court" = "setup"), inline = TRUE, selected = character(0))))),
      footer = tagList(modalButton("Cancel"), actionButton("save_shot", "Save", class = "btn-primary"))))
  }

  observeEvent(input$court_click, {
    if (!need_game()) return()
    if (role() %in% c("poss", "clock")) return(showNotification("Shots are entered by the scorer. Switch your job to record shots.", type = "warning"))
    ck <- input$court_click
    if (identical(input$court_view, "half")) { t <- input$rec_team; x <- ck$x; y <- ck$y }
    else {
      h <- full_to_half(ck$x, ck$y); x <- h$x; y <- h$y
      t <- if ((h$side == "left") == us_left_now()) "us" else "opp"
    }
    if (is_split() && t != my_side()) return(showNotification(paste("That basket is", tn()[[t]], "- the other keeper records their shots."), type = "warning"))
    shooter <- if (identical(sel_team(), t)) sel_pid() else NA
    open_shot(t, shooter, x, y)
  })

  observeEvent(input$save_shot, {
    s <- rv$shot; req(s)
    if (is.null(input$shot_made)) return(showNotification("Choose Made or Missed.", type = "warning"))
    shooter <- input$shot_shooter %||% "T"; shooter <- if (shooter == "T") NA else shooter
    made <- input$shot_made == "Made"; t <- s$team; o <- other(t)
    id <- E(t, "FG", shooter, points = as.integer(input$shot_pts), made = made,
            shot_type = if (nzchar(input$shot_type %||% "")) input$shot_type else NA,
            rating = input$shot_rating %||% NA, x = s$x, y = s$y, tag = if (isTRUE(input$shot_trans)) "trans" else NA)
    if (made && nzchar(input$shot_ast %||% "") && !identical(input$shot_ast, shooter)) E(t, "AST", input$shot_ast, linked = id)
    if (!made) {
      if (nzchar(input$shot_astatt %||% "") && !identical(input$shot_astatt, shooter)) E(t, "AST_ATT", input$shot_astatt, linked = id)
      if (nzchar(input$shot_blk %||% "")) E(o, "BLK", input$shot_blk, linked = id)
      r <- input$shot_reb %||% ""
      if (nzchar(r)) {
        side <- if (startsWith(r, "S:")) t else o; p <- sub("^[SD]:", "", r)
        rid <- E(side, if (side == t) "OREB" else "DREB", if (p == "T") NA else p, linked = id)
        if (side == o && nzchar(input$shot_outlet %||% ""))
          E(o, "OUTLET", input$shot_outlet, linked = rid, tag = input$shot_push %||% NA)
      }
    }
    removeModal(); rv$shot <- NULL; bump()
  })

  # ---- action bar + statistician prompts ---------------------------------
  ask <- function(kind, side, ids, title, choices, dast = NULL) {
    if (!isTRUE(input$prompts)) return()
    rv$prompt <- list(kind = kind, side = side, ids = ids)
    showModal(modalDialog(title = title,
      if (length(choices)) div(class = "pick", radioButtons("prompt_pick", NULL, choices, inline = TRUE, selected = character(0))),
      if (length(dast)) tagList(h5("Defensive assist?"),
        div(class = "pick", radioButtons("prompt_dast", NULL, c("None" = "", dast), inline = TRUE, selected = "")),
        h5("Pushed in transition?"),
        div(class = "pick", radioButtons("prompt_push", NULL, c("Yes" = "yes", "No" = "no"), inline = TRUE, selected = "no"))),
      footer = tagList(modalButton("Skip"), actionButton("save_prompt", "Save", class = "btn-primary"))))
  }

  handle_action <- function(k) {
    if (!need_game()) return()
    s <- sel_team()
    if (is.na(s)) return(showNotification("Tap a player (or TEAM) first.", type = "warning"))
    p <- sel_pid(); o <- other(s); them <- plist(oc(o))
    who <- if (is.na(p)) paste(tn()[[s]], "(team)") else lab()[p]
    if (k %in% c("FG2M", "FG2X", "FG3M", "FG3X"))
      return(open_shot(s, p, pts = as.integer(substr(k, 3, 3)), made = substr(k, 4, 4) == "M"))
    if (is_split()) {
      if (k %in% c("FT_M", "FT_X")) E(s, "FT", p, points = 1, made = k == "FT_M")
      else if (k == "OUTLET") { id <- E(s, "OUTLET", p)
        ask("outlet", s, list(outlet = id), paste(who, "outlet pass - what happened next?"),
            c("Pushed in transition" = "push", "Set up half-court" = "setup")) }
      else if (k == "STL") { id <- E(s, "STL", p)
        ask("stl", s, list(stl = id, to = NULL, stealer = p), paste(who, "steal"), NULL, dast = plist(setdiff(oc(s), p))) }
      else E(s, k, p)
      return(bump())
    }
    if (k %in% c("FT_M", "FT_X")) E(s, "FT", p, points = 1, made = k == "FT_M")
    else if (k == "OUTLET") { id <- E(s, "OUTLET", p)
      ask("outlet", s, list(outlet = id), paste(who, "outlet pass - what happened next?"),
          c("Pushed in transition" = "push", "Set up half-court" = "setup")) }
    else if (k == "STL") { id <- E(s, "STL", p); tid <- E(o, "TO", linked = id)
      ask("stl", s, list(stl = id, to = tid, stealer = p), paste(who, "steal - who turned it over?"),
          c("Unknown" = "", them), dast = plist(setdiff(oc(s), p))) }
    else if (k == "CHG") { id <- E(s, "CHG", p); tid <- E(o, "TO", linked = id); fid <- E(o, "PF", linked = id)
      ask("chg", s, list(to = tid, pf = fid), paste(who, "took a charge - offensive foul on?"), c("Unknown" = "", them)) }
    else if (k == "FD") { id <- E(s, "FD", p); fid <- E(o, "PF", linked = id)
      ask("fd", s, list(pf = fid), paste(who, "drew a foul - fouled by?"), c("Unknown" = "", them)) }
    else if (k == "TO") { id <- E(s, "TO", p)
      ask("to", s, list(parent = id), paste(who, "turnover - stolen by?"), c("No steal" = "", them)) }
    else if (k == "PF") { id <- E(s, "PF", p)
      ask("pf", s, list(parent = id), paste(who, "foul - who drew it?"), c("Not recorded" = "", them)) }
    else E(s, k, p)
    bump()
  }
  for (k0 in names(ACTIONS)) local({ k <- k0; observeEvent(input[[paste0("act_", k)]], handle_action(k)) })

  observeEvent(input$save_prompt, {
    pr <- rv$prompt; req(pr); v <- input$prompt_pick %||% ""; o <- other(pr$side)
    if (nzchar(v)) switch(pr$kind,
      stl = if (!is.null(pr$ids$to)) set_player(pr$ids$to, v),
      outlet = dbExecute(con, "UPDATE events SET tag = ? WHERE event_id = ?", params = list(v, pr$ids$outlet)),
      chg = { set_player(pr$ids$to, v); set_player(pr$ids$pf, v) },
      fd  = set_player(pr$ids$pf, v),
      to  = E(o, "STL", v, linked = pr$ids$parent),
      pf  = E(o, "FD", v, linked = pr$ids$parent))
    if (pr$kind == "stl" && nzchar(input$prompt_dast %||% "")) E(pr$side, "DAST", input$prompt_dast, linked = pr$ids$stl)
    if (pr$kind == "stl" && identical(input$prompt_push, "yes")) E(pr$side, "TRANS", pr$ids$stealer, linked = pr$ids$stl)
    removeModal(); rv$prompt <- NULL; bump()
  })

  observeEvent(input$opp_add_btn, {
    if (!need_game() || !nzchar(input$opp_add)) return()
    for (tok in trimws(strsplit(input$opp_add, ",")[[1]])) if (nzchar(tok)) {
      num <- sub("^#?([0-9]+).*$", "\\1", tok); nm <- trimws(sub("^#?[0-9]+", "", tok))
      dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,?,?)",
                params = list(num, nm, "", opp_squad()))
    }
    updateTextInput(session, "opp_add", value = ""); bump()
  })

  # ---- play-by-play + editing -------------------------------------------
  pbp_df <- reactive({
    ev <- cur_ev(); if (!nrow(ev)) return(data.frame())
    ev <- ev[order(-ev$event_id), ]; tn <- setNames(names2(), c("us", "opp"))
    data.frame(ID = ev$event_id, Per = ev$period, Clock = ev$clock, Team = tn[ev$team],
               Event = describe_event(ev, lab(), tn),
               Linked = ifelse(is.na(ev$linked_event_id), "", paste0("-> ", ev$linked_event_id)),
               By = ifelse(is.na(ev$entered_by), "", ev$entered_by))
  })
  output$pbp <- renderDT(datatable(pbp_df(), selection = "single", rownames = FALSE,
                                   options = list(pageLength = 10, dom = "tip")))
  sel_id <- reactive({ i <- input$pbp_rows_selected; if (length(i)) pbp_df()$ID[i] else NULL })
  observeEvent(input$undo, {
    ev <- cur_ev(); mine <- ev[ev$session_id %in% sid, ]
    if (!nrow(mine)) return(showNotification("You haven't entered any plays in this game yet.", type = "warning"))
    last <- mine[which.max(mine$event_id), ]
    delete_event(con, if (!is.na(last$linked_event_id)) last$linked_event_id else last$event_id)
    showNotification(paste("Undid your last play:", describe_event(last, lab(), setNames(names2(), c("us", "opp"))))); bump()
  })
  observeEvent(input$del_sel, { id <- sel_id(); if (is.null(id)) return(showNotification("Select a row first.", type = "warning"))
    delete_event(con, id); bump() })
  observeEvent(input$edit_sel, {
    id <- sel_id(); if (is.null(id)) return(showNotification("Select a row first.", type = "warning"))
    e <- cur_ev()[cur_ev()$event_id == id, ]
    r <- roster(if (e$team == "us") "us" else opp_squad())
    showModal(modalDialog(title = paste("Edit event", id),
      selectInput("ed_player", "Player", c("Team" = "T", setNames(r$player_id, lab()[as.character(r$player_id)])),
                  selected = if (is.na(e$player_id)) "T" else e$player_id),
      textInput("ed_period", "Period", e$period), textInput("ed_clock", "Clock", e$clock),
      if (!is.na(e$made)) radioButtons("ed_made", "Result", c("Made" = 1, "Missed" = 0), selected = e$made, inline = TRUE),
      if (e$type == "FG") tagList(
        radioButtons("ed_pts", "Points", c("2", "3"), selected = e$points, inline = TRUE),
        selectInput("ed_rating", "Rating", c("", SHOT_RATINGS), selected = e$rating %||% ""),
        selectInput("ed_type", "Shot type", c("", SHOT_TYPES), selected = e$shot_type %||% "")),
      footer = tagList(modalButton("Cancel"), actionButton("save_edit", "Save", class = "btn-primary"))))
  })
  observeEvent(input$save_edit, {
    id <- sel_id(); e <- cur_ev()[cur_ev()$event_id == id, ]
    blank <- function(v, old) if (e$type != "FG") old else if (is.null(v) || !nzchar(v)) NA else v
    dbExecute(con, "UPDATE events SET player_id=?, period=?, clock=?, made=?, points=?, rating=?, shot_type=? WHERE event_id=?",
      params = list(if (input$ed_player == "T") NA_integer_ else as.integer(input$ed_player), input$ed_period, input$ed_clock,
                    as.integer(input$ed_made %||% e$made), as.integer(input$ed_pts %||% e$points),
                    blank(input$ed_rating, e$rating), blank(input$ed_type, e$shot_type), id))
    removeModal(); bump()
  })

  # ---- new game -------------------------------------------------------
  observeEvent(input$new_game, showModal(modalDialog(title = paste("New game for", team_name()),
    dateInput("ng_date", "Date", Sys.Date()), textInput("ng_opp", "Opponent"),
    radioButtons("ng_loc", "Location", c("Home", "Away", "Neutral"), inline = TRUE),
    selectInput("ng_fmt", "Format", names(FORMATS)),
    fluidRow(column(4, numericInput("ng_per", "Periods", 4)), column(4, numericInput("ng_min", "Minutes/period", 8)),
             column(4, numericInput("ng_three", "3PT distance (ft)", 19.75, step = 0.05))),
    textInput("ng_roster", "Opponent jersey numbers (optional)", placeholder = "e.g. 1, 3, 13 Smith, 14"),
    helpText("Players already on file for this opponent are reused."),
    footer = tagList(modalButton("Cancel"), actionButton("save_game", "Create", class = "btn-primary")))))
  observeEvent(input$ng_fmt, { f <- FORMATS[[input$ng_fmt]]
    updateNumericInput(session, "ng_per", value = f$periods); updateNumericInput(session, "ng_min", value = f$min)
    updateNumericInput(session, "ng_three", value = f$three) })
  observeEvent(input$save_game, {
    if (!nzchar(trimws(input$ng_opp))) return(showNotification("Enter the opponent's name.", type = "warning"))
    opp <- trimws(input$ng_opp)
    gid <- new_game(con, input$ng_date, opp, input$ng_loc, input$ng_fmt, input$ng_per, input$ng_min, input$ng_three,
                    team_id = cur_team())
    toks <- trimws(strsplit(input$ng_roster %||% "", ",")[[1]]); toks <- toks[nzchar(toks)]
    if (length(toks)) ensure_roster(con, opp, sub("^#?([0-9]+).*$", "\\1", toks), trimws(sub("^#?[0-9]+", "", toks)))
    removeModal(); bump()
    session$onFlushed(function() updateSelectInput(session, "game_id", selected = gid))
  })

  # =========================== REPORTS ==================================
  rep_pool <- reactive(if (identical(input$rep_type, "Player") && isTRUE(input$pl_all_teams)) all_games() else games())
  rep_ids <- reactive({
    if ((input$rep_type %||% "Game") == "Game") as.integer(input$rep_game %||% NA)
    else if (identical(input$rep_type, "Player") && isTRUE(input$pl_all_teams)) all_games()$game_id
    else if (length(input$rep_games)) as.integer(input$rep_games) else games()$game_id
  })
  rep_ev <- reactive({ rv$v; get_events(con, rep_ids()) })
  rep_g <- reactive({ g <- rep_pool()[rep_pool()$game_id %in% rep_ids(), ]; if (nrow(g)) as.list(g[1, ]) else list() })
  rep_names <- reactive(c(team_name(), if ((input$rep_type %||% "Game") == "Game" && length(rep_g())) rep_g()$opponent else "Opponents"))
  rep_tm <- reactive(team_metrics(rep_ev(), weights()))
  side_name <- function(s) rep_names()[ifelse(s == "us", 1, 2)]
  box_side <- function(s) box_score(rep_ev(), players(), weights(), s)
  all_box <- reactive({
    b <- rbind(box_side("us"), box_side("opp")); if (is.null(b)) return(NULL)
    sq <- players()$squad[match(b$player_id, players()$player_id)]
    b$TeamName <- ifelse(sq == "us", team_name(), sq); b
  })
  dt <- function(df, page = 100, ...) datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = page, scrollX = TRUE, ...))
  clean_box <- function(b) { if (is.null(b)) return(data.frame()); b$player_id <- NULL; b$Team <- NULL; b$TeamName <- NULL; b }

  # Box score report
  output$rep_period  <- renderDT(dt(period_scores(rep_ev(), rep_names())))
  output$rep_summary <- renderDT(dt(game_summary(rep_ev(), weights(), rep_g(), rep_names())))
  output$rep_shoot_period <- renderDT(dt(shooting_by_period(rep_ev(), rep_names())))
  output$rep_box_us  <- renderDT(dt(clean_box(box_totals(box_side("us"), rep_ev(), "us", weights()))))
  output$rep_box_opp <- renderDT(dt(clean_box(box_totals(box_side("opp"), rep_ev(), "opp", weights()))))
  output$box_us_title <- renderText(side_name("us")); output$box_opp_title <- renderText(side_name("opp"))
  output$rep_box_pg <- renderDT(dt(clean_box(per_game(box_side("us")))))
  # Leaders
  output$rep_leaders <- renderDT(dt(leaders_table(all_box())))
  # Play-by-play
  output$rep_pbp <- renderDT({
    t <- datatable(pbp_report(rep_ev(), lab(), rep_names(), isTRUE(input$pbp_scoring)),
                   rownames = FALSE, options = list(pageLength = 50, scrollX = TRUE))
    t <- formatStyle(t, "Note", fontWeight = "bold")
    formatStyle(t, "Clock", target = "row", fontWeight = styleEqual("END", "bold"),
                backgroundColor = styleEqual("END", "#f0efec"))
  })
  # Shot charts
  fg_of <- function(s = NULL) { ev <- rep_ev(); ev[ev$type == "FG" & (is.null(s) | ev$team %in% s), ] }
  chart_side <- reactive(input$chart_side %||% "us")
  output$rep_chart_half <- renderPlot({
    fg <- fg_of(); req(nrow(fg)); np <- rep_g()$periods %||% 4
    pi <- period_index(fg$period, np)
    fg$Half <- factor(ifelse(pi > np, "Overtime", ifelse(pi <= np / 2, "1st Half", "2nd Half")), c("1st Half", "2nd Half", "Overtime"))
    fg$Team <- factor(side_name(fg$team), rep_names())
    shot_chart(fg, rep_g(), NULL, facet = ~Team + Half, size = 2.2, ncol = length(unique(fg$Half)) * 2)
  })
  output$rep_team_chart <- renderPlot(shot_chart(fg_of(chart_side()), rep_g(), paste(side_name(chart_side()), "shots")))
  output$rep_pie_zone  <- renderPlot(pie_chart(fg_of(chart_side()), "zone", ZONES, "Points by court location"))
  output$rep_pie_range <- renderPlot(pie_chart(fg_of(chart_side()), "range", RANGES, "Points by shot range"))
  output$rep_tbl_zone   <- renderDT(dt(shot_table(fg_of(chart_side()), "zone", ZONES)))
  output$rep_tbl_range  <- renderDT(dt(shot_table(fg_of(chart_side()), "range", RANGES)))
  output$rep_tbl_rating <- renderDT(dt(shot_table(fg_of(chart_side()), "rating", SHOT_RATINGS)))
  output$rep_tbl_type   <- renderDT(dt(shot_table(fg_of(chart_side()), "shot_type", SHOT_TYPES)))
  output$rep_players_chart <- renderPlot({
    fg <- fg_of(chart_side()); fg <- fg[!is.na(fg$player_id) & !is.na(fg$x), ]; req(nrow(fg))
    tot <- tapply(fg$pts, fg$player_id, sum); att <- table(fg$player_id); mk <- tapply(fg$made %in% 1, fg$player_id, sum)
    fg$Player <- sprintf("%s  %d-%d, %d pts", lab()[as.character(fg$player_id)], mk[as.character(fg$player_id)],
                         att[as.character(fg$player_id)], tot[as.character(fg$player_id)])
    num <- suppressWarnings(as.numeric(players()$number[match(fg$player_id, players()$player_id)]))
    fg$Player <- factor(fg$Player, unique(fg$Player[order(num)]))
    shot_chart(fg, rep_g(), NULL, facet = ~Player, size = 1.8, ncol = 4)
  }, height = function() players_chart_h())
  players_chart_h <- reactive({ fg <- fg_of(chart_side()); n <- length(unique(fg$player_id[!is.na(fg$player_id)])); 60 + 250 * ceiling(max(n, 1) / 4) })
  # Graphical comparison
  for (i0 in seq_along(METRIC_GROUPS)) local({ i <- i0
    output[[paste0("rep_cmp_", i)]] <- renderPlot(comparison_plot(rep_tm(), METRIC_GROUPS[[i]], rep_names()[1], rep_names()[2]))
  })
  # Period comparison
  output$rep_period_cmp <- renderDT({
    d <- period_comparison(rep_ev(), weights()); hid <- grep("^\\.g_", names(d)) - 1
    t <- datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE,
                   columnDefs = list(list(visible = FALSE, targets = hid))))
    for (gc in grep("^\\.g_", names(d), value = TRUE)) {
      dc <- paste(sub("^\\.g_", "", gc), "+/-")
      t <- formatStyle(t, dc, valueColumns = gc, fontWeight = "bold",
                       backgroundColor = styleEqual(c(-1, 0, 1), c("#f6d4d4", "transparent", "#cfeccf")))
    }
    t
  })
  # Timeline
  rep_flow <- reactive(game_flow(rep_ev(), rep_g()))
  output$rep_timeline <- renderPlot({
    fl <- rep_flow(); req(fl); g <- rep_g(); np <- g$periods %||% 4
    marks <- if (fl$xlab == "Game minute") (g$period_min %||% 8) * seq_len(np - 1) else
      which(c(FALSE, fl$s$period[-1] != fl$s$period[-nrow(fl$s)])) - 0.5
    timeline_plot(fl, rep_names()[1], rep_names()[2], input$run_min %||% 8, marks)
  })
  output$rep_runs <- renderDT({
    fl <- rep_flow(); req(fl); r <- fl$runs[fl$runs$Points >= (input$run_min %||% 8), ]
    dt(data.frame(Team = side_name(r$Team), Run = paste0(r$Points, "-0"), Period = r$Period,
                  `Score before` = r$From, `Score after` = r$To, check.names = FALSE))
  })
  output$rep_flow_stats <- renderText({ fl <- rep_flow(); req(fl)
    sprintf("Lead changes: %d | Ties: %d | Largest lead: %s %d, %s %d | Longest run: %s %d-0, %s %d-0",
            fl$lead_changes, fl$ties, rep_names()[1], fl$big_us, rep_names()[2], fl$big_opp,
            rep_names()[1], fl$run_us, rep_names()[2], fl$run_opp) })
  # Lineups
  output$rep_lineups <- renderDT({
    s <- input$lu_side %||% "us"; d <- lineup_report(rep_ev(), players(), s, weights(), input$lu_min %||% 3)
    req(nrow(d)); d <- d[order(LINEUP_SORTS[[input$lu_sort %||% "Most used"]](d)), ]
    datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE,
              columnDefs = list(list(visible = FALSE, targets = 1))))
  })
  # Goals
  output$rep_goals <- renderDT({
    gl <- eval_goals(rep_tm(), goals()); req(gl); gl$met <- NULL
    formatStyle(datatable(gl, rownames = FALSE, options = list(dom = "t")), "Met", color = styleEqual(c("\u2714 Met", "\u2718 Missed"), c(COL_GOOD, COL_BAD)), fontWeight = "bold")
  })
  output$rep_goal_period <- renderPlot({
    d <- goal_period_data(rep_ev(), weights(), goals(), rep_g()$periods %||% 4); req(d); goal_period_plot(d) }, height = function() goal_h())
  output$rep_goal_trend <- renderPlot({ d <- goal_trend_data(rep_ev(), weights(), goals(), games()); req(d); goal_trend_plot(d) }, height = function() goal_h())
  output$rep_goal_rate <- renderDT({
    d <- goal_trend_data(rep_ev(), weights(), goals(), games()); req(d)
    a <- aggregate(Met ~ Goal, d, function(x) sum(x, na.rm = TRUE), na.action = na.pass)
    n <- length(unique(d$Game)); a <- a[match(goals()$label, a$Goal), ]
    dt(data.frame(Goal = a$Goal, `Games met` = a$Met, Games = n, `Hit rate %` = pct(a$Met, n), check.names = FALSE))
  })
  goal_h <- reactive(80 + 230 * ceiling(max(nrow(goals()), 1) / 4))
  # Season
  output$rep_gamelog <- renderDT(dt(game_log(rep_ev(), weights(), goals(), games())))
  output$rep_compare <- renderDT({ tm <- rep_tm(); n <- max(length(unique(rep_ev()$game_id)), 1)
    tm$`Us / game` <- ifelse(tm$Metric %in% RATE_METRICS, NA, round(tm$Us / n, 1))
    tm$`Opp / game` <- ifelse(tm$Metric %in% RATE_METRICS, NA, round(tm$Opp / n, 1))
    names(tm)[2:3] <- rep_names(); dt(tm) })
  # Player
  pl_id <- reactive(as.integer(input$rep_player %||% NA))
  pl_side <- reactive({ sq <- players()$squad[players()$player_id == pl_id()]; if (length(sq) && sq == "us") "us" else "opp" })
  pl_fg <- reactive({ ev <- rep_ev(); ev[ev$type == "FG" & ev$player_id %in% pl_id(), ] })
  output$pl_summary <- renderDT({
    bx <- box_side(pl_side()); req(bx); r <- bx[bx$player_id == pl_id(), ]; req(nrow(r))
    pg <- per_game(r); r$Player <- "Totals"; pg$Player <- "Per game"; dt(clean_box(rbind(r, pg)))
  })
  output$pl_log <- renderDT({
    g <- rep_pool()[rep_pool()$game_id %in% rep_ids(), ]; g <- g[order(g$game_date), ]; ev_all <- rep_ev()
    tnm <- setNames(teams()$name, teams()$team_id)
    rows <- lapply(seq_len(nrow(g)), function(i) {
      b <- box_score(ev_all[ev_all$game_id == g$game_id[i], ], players(), weights(), pl_side())
      if (is.null(b)) return(NULL); b <- b[b$player_id == pl_id(), ]; if (!nrow(b)) return(NULL)
      cbind(Date = g$game_date[i], Team = unname(tnm[as.character(g$team_id[i])]), Opponent = g$opponent[i], clean_box(b)[, -(1:2)])
    })
    out <- do.call(rbind, rows); req(out); dt(out)
  })
  output$pl_chart <- renderPlot(shot_chart(pl_fg(), rep_g(), lab()[as.character(pl_id())]))
  output$pl_pie_zone  <- renderPlot(pie_chart(pl_fg(), "zone", ZONES, "Points by court location"))
  output$pl_pie_range <- renderPlot(pie_chart(pl_fg(), "range", RANGES, "Points by shot range"))
  output$pl_tbl_zone   <- renderDT(dt(shot_table(pl_fg(), "zone", ZONES)))
  output$pl_tbl_rating <- renderDT(dt(shot_table(pl_fg(), "rating", SHOT_RATINGS)))
  output$pl_tbl_type   <- renderDT(dt(shot_table(pl_fg(), "shot_type", SHOT_TYPES)))

  # Transition & possessions
  rep_pt <- reactive(poss_table(rep_ev()))
  output$rep_phase <- renderDT(dt(phase_summary(rep_pt(), rep_names())))
  output$rep_start <- renderDT(dt(phase_summary(rep_pt(), rep_names(), "reason")))
  output$rep_outlets <- renderDT(dt(outlet_table(all_box())))
  output$rep_phase_plot <- renderPlot({
    d <- phase_summary(rep_pt(), rep_names()); req(nrow(d) > 0, "Phase" %in% names(d))
    d$Team <- factor(d$Team, rep_names())
    ggplot(d, aes(Phase, PPP, fill = Team)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_text(aes(label = sprintf("%.2f\n(%d poss)", PPP, Possessions)), position = position_dodge(width = 0.75),
                vjust = -0.25, size = 3.4, lineheight = 0.9) +
      scale_fill_manual(values = setNames(c(COL_US, COL_OPP), rep_names())) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
      theme_cs() + labs(x = NULL, y = "Points per possession", fill = NULL)
  })
  trans_tab <- function() tabPanel("Transition & Possessions",
    helpText("Possessions are split from the play sequence. Transition = a keeper tapped Fast Break, marked an outlet as a push, or marked a shot as transition",
             "(plus the 8-second clock rule if it's turned on in Settings). mu and theta are scoring and empty-possession rates per minute, from possessions with clock times."),
    fluidRow(column(5, plotOutput("rep_phase_plot", height = "300px")), column(7, h5("By phase"), DTOutput("rep_phase"))),
    h5("By how the possession started"), DTOutput("rep_start"), h5("Outlet passes"), DTOutput("rep_outlets"))

  # ---- report layout ----------------------------------------------------
  shot_tabs <- function(game) tabPanel("Shot Charts",
    div(class = "pick", radioButtons("chart_side", NULL, setNames(c("us", "opp"), rep_names()), inline = TRUE,
                                     selected = isolate(input$chart_side) %||% "us")),
    tabsetPanel(
      if (game) tabPanel("Game (by half)", plotOutput("rep_chart_half", height = "360px")),
      tabPanel("Team", fluidRow(column(6, plotOutput("rep_team_chart", height = "430px")),
                                column(6, plotOutput("rep_pie_zone", height = "215px"), plotOutput("rep_pie_range", height = "215px"))),
               fluidRow(column(6, h5("By court location"), DTOutput("rep_tbl_zone")), column(6, h5("By range"), DTOutput("rep_tbl_range"))),
               fluidRow(column(6, h5("By shot rating"), DTOutput("rep_tbl_rating")), column(6, h5("By shot type"), DTOutput("rep_tbl_type")))),
      tabPanel("Team - individual players", plotOutput("rep_players_chart", height = "auto"))))
  cmp_tab <- function() tabPanel("Graphical Comparison", lapply(seq_along(METRIC_GROUPS), function(i)
    tagList(h4(names(METRIC_GROUPS)[i]), plotOutput(paste0("rep_cmp_", i), height = "230px"))))
  lu_tab <- function() tabPanel("Lineups",
    fluidRow(column(3, div(class = "pick", radioButtons("lu_side", "Team", setNames(c("us", "opp"), rep_names()), inline = TRUE))),
             column(5, selectInput("lu_sort", "Analysis", names(LINEUP_SORTS))),
             column(4, numericInput("lu_min", "Minimum possessions (off + def)", 3, min = 1))),
    DTOutput("rep_lineups"))

  output$report_ui <- renderUI({
    if (!length(rep_ids()) || all(is.na(rep_ids()))) return(h4("No games yet. Create one, or load demo data under Rosters & Settings."))
    if (input$rep_type == "Game") tabsetPanel(
      tabPanel("Box Score", h5("Score by period"), DTOutput("rep_period"),
        fluidRow(column(5, h5("Game summary"), DTOutput("rep_summary")),
                 column(7, h5("Shooting by period"), DTOutput("rep_shoot_period"))),
        h4(textOutput("box_us_title")), DTOutput("rep_box_us"), h4(textOutput("box_opp_title")), DTOutput("rep_box_opp")),
      tabPanel("Leaders", DTOutput("rep_leaders")),
      tabPanel("Play-by-Play", checkboxInput("pbp_scoring", "Scoring plays only", FALSE), DTOutput("rep_pbp")),
      shot_tabs(TRUE), cmp_tab(),
      tabPanel("Period Comparison", helpText("Green = period won on that stat, red = lost (lower is better for TO and fouls)."),
               DTOutput("rep_period_cmp")),
      trans_tab(),
      tabPanel("Timeline", fluidRow(column(3, numericInput("run_min", "Highlight runs of at least", 8, min = 2))),
               textOutput("rep_flow_stats"), plotOutput("rep_timeline", height = "380px"), h5("Runs"), DTOutput("rep_runs")),
      lu_tab(),
      tabPanel("Team Goals", DTOutput("rep_goals"), h5("By period"),
               plotOutput("rep_goal_period", height = "auto")))
    else if (input$rep_type == "Player") tabsetPanel(
      tabPanel("Summary", h5("Totals and per-game averages"), DTOutput("pl_summary"), h5("Game log"), DTOutput("pl_log")),
      tabPanel("Shot Chart", fluidRow(column(6, plotOutput("pl_chart", height = "430px")),
                                      column(6, plotOutput("pl_pie_zone", height = "215px"), plotOutput("pl_pie_range", height = "215px"))),
               fluidRow(column(4, h5("By location"), DTOutput("pl_tbl_zone")), column(4, h5("By rating"), DTOutput("pl_tbl_rating")),
                        column(4, h5("By type"), DTOutput("pl_tbl_type")))))
    else tabsetPanel(
      tabPanel("Game Log", DTOutput("rep_gamelog")),
      tabPanel("Team Totals", DTOutput("rep_compare")),
      tabPanel("Player Totals", h5("Totals"), DTOutput("rep_box_us"), h5("Per game"), DTOutput("rep_box_pg")),
      tabPanel("Leaders", DTOutput("rep_leaders")),
      trans_tab(), shot_tabs(FALSE), cmp_tab(), lu_tab(),
      tabPanel("Team Goals", h5("Hit rate"), DTOutput("rep_goal_rate"), h5("Trends"),
               plotOutput("rep_goal_trend", height = "auto")))
  })

  output$dl_box <- downloadHandler(function() paste0("box_scores_", Sys.Date(), ".csv"), function(f) {
    b <- all_box(); b$player_id <- NULL; b$Team <- NULL; write.csv(b, f, row.names = FALSE) })
  output$dl_events <- downloadHandler(function() paste0("events_", Sys.Date(), ".csv"), function(f) {
    ev <- rep_ev(); ev$player <- lab()[as.character(ev$player_id)]; write.csv(ev, f, row.names = FALSE) })

  # =========================== SETTINGS =================================
  observeEvent(input$save_team, { req(nzchar(trimws(input$team_name)))
    dbExecute(con, "UPDATE teams SET name = ? WHERE team_id = ?", params = list(trimws(input$team_name), cur_team())); bump() })
  observeEvent(input$add_team_btn, {
    nm <- trimws(input$new_team); if (!nzchar(nm)) return(showNotification("Type a team name.", type = "warning"))
    if (nm %in% teams()$name) return(showNotification("You already have a team with that name.", type = "warning"))
    tid <- add_team(con, nm); updateTextInput(session, "new_team", value = ""); bump()
    session$onFlushed(function() updateSelectInput(session, "team_id", selected = as.character(tid)))
    showNotification(paste("Added", nm, "- add players to its roster below, or pick them from your other teams."))
  })
  squad_tbl <- reactive({ rv$v; sq <- input$roster_squad %||% "us"
    if (sq == "us") team_roster(con, cur_team(), active_only = FALSE) else { p <- players(); p[p$squad == sq, ] } })
  observe({  # players on your other teams who aren't on this one (call-ups)
    rv$v; mine <- team_roster(con, cur_team(), active_only = FALSE)$player_id
    o <- dbGetQuery(con, "SELECT DISTINCT p.*, t.name team FROM players p JOIN team_players tp USING (player_id)
                          JOIN teams t USING (team_id) WHERE p.squad = 'us' ORDER BY CAST(p.number AS INTEGER)")
    o <- o[!(o$player_id %in% mine), ]; o <- o[!duplicated(o$player_id), ]
    updateSelectInput(session, "add_existing", choices = setNames(o$player_id, sprintf("#%s %s (%s)", o$number, o$name, o$team)))
  })
  observeEvent(input$add_existing_btn, { req(input$add_existing); add_to_team(con, cur_team(), input$add_existing); bump() })
  observeEvent(input$remove_from_team, { i <- input$roster_tbl_rows_selected; req(i, (input$roster_squad %||% "us") == "us")
    dbExecute(con, "DELETE FROM team_players WHERE team_id = ? AND player_id = ?",
              params = list(cur_team(), squad_tbl()$player_id[i])); bump() })
  output$roster_tbl <- renderDT({ p <- squad_tbl()
    datatable(data.frame(`#` = p$number, Name = p$name, Pos = p$position, Active = ifelse(p$active == 1, "Yes", "No"),
                         check.names = FALSE), rownames = FALSE, selection = "single",
              editable = list(target = "cell", disable = list(columns = 3)), options = list(dom = "t", pageLength = 50)) })
  roster_changed <- function() {
    dbExecute(con, "INSERT OR REPLACE INTO settings VALUES ('roster_rev', ?)", params = list(format(Sys.time(), "%OS3"))); bump() }
  # Fix a number, name or position by editing the cell
  observeEvent(input$roster_tbl_cell_edit, {
    e <- input$roster_tbl_cell_edit; p <- squad_tbl()[e$row, ]; req(nrow(p))
    col <- c("number", "name", "position")[e$col + 1]; v <- trimws(as.character(e$value))
    if (col == "number" && !grepl("^[0-9]{1,3}$", v)) return(showNotification("A jersey number is 0 to 3 digits.", type = "warning"))
    dbExecute(con, sprintf("UPDATE players SET %s = ? WHERE player_id = ?", col), params = list(v, p$player_id))
    roster_changed()
  })
  # Delete a player who has no stats; anyone with stats can only be deactivated (so past games stay correct)
  observeEvent(input$delete_player, {
    i <- input$roster_tbl_rows_selected; if (!length(i)) return(showNotification("Select a player first.", type = "warning"))
    p <- squad_tbl()[i, ]
    n <- dbGetQuery(con, "SELECT COUNT(*) n FROM events WHERE player_id = ?", params = list(p$player_id))$n
    if (n > 0) return(showNotification(sprintf("#%s %s has %d recorded plays, so deleting would change past games. Use Activate / deactivate to hide them instead.",
                                               p$number, p$name %||% "", n), type = "warning", duration = 8))
    dbExecute(con, "DELETE FROM team_players WHERE player_id = ?", params = list(p$player_id))
    dbExecute(con, "DELETE FROM players WHERE player_id = ?", params = list(p$player_id))
    showNotification(sprintf("Deleted #%s %s", p$number, p$name %||% "")); roster_changed()
  })
  observeEvent(input$add_player, { req(nzchar(input$p_num) || nzchar(input$p_name))
    dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,?,?)",
              params = list(input$p_num, input$p_name, input$p_pos, input$roster_squad %||% "us"))
    if ((input$roster_squad %||% "us") == "us") add_to_team(con, cur_team(), last_id(con))
    for (i in c("p_num", "p_name", "p_pos")) updateTextInput(session, i, value = ""); bump() })
  observeEvent(input$toggle_player, { i <- input$roster_tbl_rows_selected; req(i)
    dbExecute(con, "UPDATE players SET active = 1 - active WHERE player_id = ?", params = list(squad_tbl()$player_id[i])); roster_changed() })
  output$goals_tbl <- renderDT({ g <- goals()
    datatable(data.frame(Goal = g$label, Side = g$side, Metric = g$metric, Rule = g$comparator, Target = g$target,
                         `Applies to` = ifelse(is.na(g$team_id), "All teams", team_name()), check.names = FALSE),
              rownames = FALSE, selection = "single", options = list(dom = "t")) })
  observeEvent(input$add_goal, {
    lbl <- if (nzchar(input$g_label)) input$g_label else paste(input$g_side, input$g_metric, input$g_cmp, input$g_target)
    dbExecute(con, "INSERT INTO goals (label, side, metric, comparator, target, team_id) VALUES (?,?,?,?,?,?)",
              params = list(lbl, input$g_side, input$g_metric, input$g_cmp, input$g_target,
                            if (identical(input$g_scope, "all")) NA_integer_ else as.integer(cur_team()))); bump() })
  observeEvent(input$del_goal, { i <- input$goals_tbl_rows_selected; req(i)
    dbExecute(con, "DELETE FROM goals WHERE goal_id = ?", params = list(goals()$goal_id[i])); bump() })
  output$weights_tbl <- renderDT(datatable(weights()[, c("label", "value")], rownames = FALSE, colnames = c("Stat", "Value"),
    selection = "none", editable = list(target = "cell", disable = list(columns = 0)), options = list(dom = "t", pageLength = 50)))
  observeEvent(input$weights_tbl_cell_edit, { e <- input$weights_tbl_cell_edit
    dbExecute(con, "UPDATE weights SET value = ? WHERE key = ?", params = list(as.numeric(e$value), weights()$key[e$row])); bump() })
  observe({ rv$v; updateCheckboxInput(session, "auto8",
    value = identical(dbGetQuery(con, "SELECT value FROM settings WHERE key='auto8'")$value, "1")) })
  observeEvent(input$auto8, { v <- if (isTRUE(input$auto8)) "1" else "0"
    if (!identical(dbGetQuery(con, "SELECT value FROM settings WHERE key='auto8'")$value, v)) {
      dbExecute(con, "UPDATE settings SET value = ? WHERE key = 'auto8'", params = list(v)); bump() } }, ignoreInit = TRUE)
  observeEvent(input$demo, { withProgress(message = "Generating demo games", load_demo(con)); bump()
    vt <- dbGetQuery(con, "SELECT team_id FROM teams WHERE name = 'Varsity'")$team_id[1]
    session$onFlushed(function() updateSelectInput(session, "team_id", selected = as.character(vt)))
    showNotification("Demo Varsity and JV teams loaded. Five JV players also play varsity.") })

  # =========================== IMPORT / EXPORT ==========================
  output$dl_xml <- downloadHandler(
    function() { g <- games()[games()$game_id == as.integer(input$io_game), ]
      paste0("game_", g$game_date, "_", gsub("[^A-Za-z0-9]", "", g$opponent), ".xml") },
    function(f) { if (!HAS_XML) stop("XML export needs the xml2 package on the server."); export_game_xml(con, as.integer(input$io_game), f) })
  output$xml_note <- renderUI(if (!HAS_XML) div(class = "alert alert-warning",
    "XML import/export is off because the xml2 package can't be loaded on this server. CSV and database backups still work."))
  output$dl_db <- downloadHandler(function() paste0("courtside_", Sys.Date(), ".sqlite"), function(f) file.copy(DB_PATH, f))
  observeEvent(input$xml_in, {
    if (!HAS_XML) return(showNotification("XML import needs the xml2 package, which isn't available on this server.", type = "error"))
    gid <- tryCatch(import_game_xml(con, input$xml_in$datapath, cur_team()), error = function(e) {
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(gid)) { bump(); showNotification("Game imported.") }
  })
}

shinyApp(ui, server)
