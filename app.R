# =============================================================================
# CourtSide Stats v3 - Shiny basketball statistics (both teams, several keepers)
# Files: app.R (UI + server), R/engine.R, R/charts.R, R/reports.R, R/data_io.R
# app.R sources everything in R/ itself, so older Shiny versions work too.
# Data: bball.sqlite next to this file (override with env var COURTSIDE_DB).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- Startup checks: write a clear message to the server log instead of a bare failure ----
need <- c("shiny", "DBI", "RSQLite", "DT", "ggplot2", "xml2", "htmltools")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("CourtSide Stats: missing R packages: ", paste(missing, collapse = ", "),
                          ". Install them as root so the shiny user can see them, e.g. sudo Rscript -e ",
                          "'install.packages(c(\"", paste(missing, collapse = "\", \""), "\"), repos = \"https://cloud.r-project.org\")'", call. = FALSE)
if (getRversion() < "3.6.0") stop("CourtSide Stats needs R 3.6 or newer (this server has ", getRversion(), ").", call. = FALSE)
if (packageVersion("ggplot2") < "3.4.0")
  warning("CourtSide Stats: ggplot2 ", packageVersion("ggplot2"), " is older than 3.4; charts may fail. Update ggplot2.", call. = FALSE)
suppressPackageStartupMessages({
  library(shiny); library(DBI); library(RSQLite); library(DT); library(ggplot2); library(xml2)
})
# Load helper files explicitly (older Shiny versions do not auto-load the R/ folder)
for (f in sort(list.files("R", pattern = "[.][Rr]$", full.names = TRUE))) source(f, encoding = "UTF-8")
DB_PATH <- Sys.getenv("COURTSIDE_DB", file.path(getwd(), "bball.sqlite"))

css <- "
.pick .radio-inline, .pick .checkbox-inline {padding-left:0; margin:0 4px 6px 0;}
.pick input {position:absolute; opacity:0; width:0; height:0;}
.pick input + span {display:inline-block; min-width:52px; padding:9px 10px; border:2px solid #52514e;
  border-radius:8px; text-align:center; font-weight:600; cursor:pointer; background:#fff;}
.pick input:checked + span {background:#0b0b0b; color:#fff; border-color:#0b0b0b;}
.us-panel .pick input:checked + span {background:#2a78d6; border-color:#2a78d6;}
.opp-panel .pick input:checked + span {background:#eb6834; border-color:#eb6834;}
.us-panel h4 {color:#2a78d6; font-weight:700;} .opp-panel h4 {color:#c4501f; font-weight:700;}
.actgrid > .shiny-panel-conditional {display:contents;}
.keepers {padding-top:24px; color:#52514e;} .keepers b {color:#0b0b0b;}
.actgrid .btn {width:48%; margin:0 1% 5px 0; padding:8px 2px; font-weight:600; white-space:normal;}
.us-panel .actgrid .btn {border-color:#2a78d6;} .opp-panel .actgrid .btn {border-color:#eb6834;}
.scorebox {font-size:26px; font-weight:700; text-align:center; padding:6px; background:#0b0b0b;
  color:#fff; border-radius:8px; margin-top:4px;}
.lineup-warn {color:#d03b3b; font-weight:600;}
.oncourt .checkbox-inline {margin-left:0; margin-right:8px;}
"

side_panel <- function(s, title_out) div(class = paste0(s, "-panel"),
  h4(textOutput(title_out, inline = TRUE)),
  div(class = "pick", uiOutput(paste0("pick_", s, "_ui"))),
  div(class = "actgrid", lapply(names(ACTIONS), function(k) {
    roles <- names(ROLE_ACTIONS)[sapply(ROLE_ACTIONS, function(a) k %in% a)]
    conditionalPanel(sprintf("input.role == 'all' || ['%s'].indexOf(input.role) >= 0", paste(roles, collapse = "','")),
      actionButton(paste0("act_", s, "_", k), ACTIONS[[k]], class = "btn-default btn-sm"))
  })),
  conditionalPanel("input.role != 'scorer'",
    div(class = "oncourt", checkboxGroupInput(paste0("oc_", s), "On court", choices = NULL, inline = TRUE))),
  conditionalPanel("input.role == 'scorer'", tags$small("On court (set by the possession keeper): ",
                                                         textOutput(paste0("octxt_", s), inline = TRUE))),
  uiOutput(paste0("lumsg_", s)))

entry_tab <- tabPanel("Game Entry",
  fluidRow(
    column(2, textInput("keeper", "Your name", placeholder = "e.g. John")),
    column(4, selectInput("role", "Your job", ROLES, width = "100%")),
    column(6, div(class = "keepers", uiOutput("keepers_ui")))),
  fluidRow(
    column(3, selectInput("game_id", "Game", choices = NULL, width = "100%")),
    column(1, br(), actionButton("new_game", "New", icon = icon("plus"))),
    column(2, selectInput("period", "Period", choices = as.character(1:4))),
    column(2, textInput("clock", "Clock (mm:ss, optional)")),
    column(4, div(class = "scorebox", textOutput("scoreline")))),
  fluidRow(
    column(3, side_panel("us", "us_title")),
    column(6,
      fluidRow(column(7, div(class = "pick", radioButtons("rec_team", "Court clicks record a shot for:",
                                c("Us" = "us", "Opponent" = "opp"), inline = TRUE))),
               column(5, br(), checkboxInput("prompts", "Statistician prompts", TRUE))),
      plotOutput("court", click = "court_click", height = "470px"),
      div(style = "margin-top:6px;",
        actionButton("undo", "Undo my last", icon = icon("rotate-left"), class = "btn-warning"),
        actionButton("edit_sel", "Edit selected", icon = icon("pen")),
        actionButton("del_sel", "Delete selected", icon = icon("trash"), class = "btn-danger"))),
    column(3, side_panel("opp", "opp_title"),
      textInput("opp_add", "Add opponent players", placeholder = "e.g. 3, 11 Jones, 24"),
      actionButton("opp_add_btn", "Add to opponent roster", class = "btn-sm"))),
  hr(), h4("Play-by-play"), DTOutput("pbp"))

reports_tab <- tabPanel("Reports",
  sidebarLayout(
    sidebarPanel(width = 3,
      radioButtons("rep_type", "Report", c("Game", "Season / cumulative", "Player")),
      conditionalPanel("input.rep_type == 'Game'", selectInput("rep_game", "Game", choices = NULL)),
      conditionalPanel("input.rep_type != 'Game'",
        selectizeInput("rep_games", "Games included", choices = NULL, multiple = TRUE),
        helpText("Leave empty to include every game.")),
      conditionalPanel("input.rep_type == 'Player'", selectInput("rep_player", "Player", choices = NULL)),
      hr(), downloadButton("dl_box", "Box scores (CSV)"), br(), br(),
      downloadButton("dl_events", "Event log (CSV)"),
      helpText("To print a report, use your browser's Print command.")),
    mainPanel(width = 9, uiOutput("report_ui"))))

settings_tab <- tabPanel("Rosters & Settings",
  fluidRow(
    column(5, h4("Team"), textInput("team_name", "Our team name"), actionButton("save_team", "Save"), hr(),
      h4("Rosters"), selectInput("roster_squad", "Roster", choices = c("Our team" = "us")),
      fluidRow(column(3, textInput("p_num", "#")), column(6, textInput("p_name", "Name")), column(3, textInput("p_pos", "Pos"))),
      actionButton("add_player", "Add player", icon = icon("user-plus")),
      actionButton("toggle_player", "Activate / deactivate selected"),
      br(), br(), DTOutput("roster_tbl"), hr(),
      actionButton("demo", "Load demo data (4 games)", class = "btn-info"), hr(),
      h4("Transition"),
      checkboxInput("auto8", "Also count a possession as transition when its first shot or turnover comes within 8 seconds of a defensive rebound or steal (needs clock times on every play)", FALSE),
      helpText("Otherwise a possession is transition when a keeper taps Fast Break, marks an outlet as a push, or marks a shot as transition.")),
    column(7, h4("Team goals"),
      fluidRow(column(4, textInput("g_label", "Goal label")), column(2, selectInput("g_side", "Side", c("Us", "Opp"))),
               column(3, selectInput("g_metric", "Metric", METRICS)),
               column(1, selectInput("g_cmp", "", c(">=", "<=", ">", "<", "="))),
               column(2, numericInput("g_target", "Target", 0))),
      actionButton("add_goal", "Add goal"), actionButton("del_goal", "Delete selected goal"),
      helpText("Side = whose stat it is (Opp FG% = opponent's shooting). Percentages use a 0-100 scale."),
      DTOutput("goals_tbl"), hr(), h4("Productivity (+/- value) weights"),
      helpText("A player's Productivity is the sum of these values over their stats. Double-click a value to edit."),
      DTOutput("weights_tbl"))))

io_tab <- tabPanel("Import / Export",
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
<li><b>Several keepers</b>: each person opens the app, enters a name and picks a job. Lineups and the period are shared; plays from
everyone appear within about 2 seconds; <i>Undo my last</i> only removes your own plays.</li>
<li>Timeline uses game minutes when every scoring play has a clock time; otherwise scoring-play order.</li>
</ul>")

help_tab <- tabPanel("Definitions", fluidRow(column(8, includeMarkdown_safe())))


ui <- navbarPage("CourtSide Stats", header = tags$head(tags$style(HTML(css))),
                 entry_tab, reports_tab, settings_tab, io_tab, help_tab)

# =============================================================================
server <- function(input, output, session) {
  con <- db_connect(DB_PATH); sid <- session$token %||% paste0("s", as.integer(runif(1, 1, 1e9)))
  role <- reactive(input$role %||% "all")
  onStop(function() { try(dbExecute(con, "DELETE FROM keepers WHERE session_id = ?", params = list(sid)), silent = TRUE)
                      dbDisconnect(con) })
  rv <- reactiveValues(v = 0, shot = NULL, prompt = NULL)
  bump <- function() rv$v <- rv$v + 1

  players <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM players ORDER BY squad, CAST(number AS INTEGER)") })
  games   <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM games ORDER BY game_date DESC, game_id DESC") })
  goals   <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM goals") })
  weights <- reactive({ rv$v; dbGetQuery(con, "SELECT * FROM weights") })
  team_name <- reactive({ rv$v; dbGetQuery(con, "SELECT value FROM settings WHERE key='team_name'")$value })
  lab <- reactive(player_labels(players()))
  has_game <- reactive(!is.null(input$game_id) && nzchar(input$game_id))
  cur_game <- reactive({ req(has_game()); games()[games()$game_id == as.integer(input$game_id), ] })
  cur_ev   <- reactive({ rv$v; req(has_game()); get_events(con, as.integer(input$game_id)) })
  roster <- function(squad) { p <- players(); p[p$squad == squad & p$active == 1, ] }
  opp_squad <- reactive(if (has_game() && nrow(cur_game())) cur_game()$opponent else "")
  names2 <- reactive(c(team_name(), if (has_game() && nrow(cur_game())) cur_game()$opponent else "Opponent"))

  output$us_title <- renderText(team_name())
  output$opp_title <- renderText(names2()[2])

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
    updateSelectInput(session, "roster_squad", choices = c("Our team" = "us", setNames(sq, sq)),
                      selected = isolate(input$roster_squad))
    pl <- players(); grp <- split(setNames(pl$player_id, lab()[as.character(pl$player_id)]),
                                  ifelse(pl$squad == "us", team_name(), pl$squad))
    updateSelectInput(session, "rep_player", choices = lapply(grp, as.list), selected = isolate(input$rep_player))
  })
  observe({ updateTextInput(session, "team_name", value = team_name()) })
  # ---- shared game state: lineups + period live in the database so every keeper sees the same ----
  gstate <- reactive({
    rv$v; req(has_game()); gid <- as.integer(input$game_id)
    gs <- dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
    if (!nrow(gs)) {
      f5 <- function(r) paste(head(r$player_id, 5), collapse = ",")
      dbExecute(con, "INSERT OR IGNORE INTO game_state (game_id, us, opp, period, version) VALUES (?,?,?,?,0)",
                params = list(gid, f5(roster("us")), f5(roster(opp_squad())), "1"))
      gs <- dbGetQuery(con, "SELECT * FROM game_state WHERE game_id = ?", params = list(gid))
    }
    gs
  })
  set_state <- function(col, val) {
    dbExecute(con, sprintf("UPDATE game_state SET %s = ?, version = version + 1 WHERE game_id = ?", col),
              params = list(val, as.integer(input$game_id))); bump()
  }
  last_ch <- new.env()
  update_oc <- function(s, r) {
    ch <- setNames(as.character(r$player_id), lab()[as.character(r$player_id)]); sel <- oc(s)
    if (!identical(ch, last_ch[[s]])) {
      last_ch[[s]] <- ch
      updateCheckboxGroupInput(session, paste0("oc_", s), choices = ch, selected = sel, inline = TRUE)
    } else if (!setequal(isolate(input[[paste0("oc_", s)]]) %||% character(0), sel))
      updateCheckboxGroupInput(session, paste0("oc_", s), selected = sel)
  }
  observe({ req(has_game()); update_oc("us", roster("us")) })
  observe({ req(has_game()); update_oc("opp", roster(opp_squad())) })
  for (s0 in c("us", "opp")) local({ s <- s0
    observeEvent(input[[paste0("oc_", s)]], {
      if (role() == "scorer" || !has_game()) return()
      v <- paste(input[[paste0("oc_", s)]], collapse = ",")
      if (!identical(v, gstate()[[s]] %||% "")) set_state(s, v)
    })
    output[[paste0("octxt_", s)]] <- renderText(paste(lab()[oc(s)], collapse = ", "))
  })
  observe({
    g <- cur_game(); req(nrow(g)); np <- g$periods %||% 4
    updateSelectInput(session, "period", choices = c(as.character(seq_len(np)), "OT1", "OT2", "OT3"),
                      selected = isolate(gstate()$period))
  })
  observe({ p <- gstate()$period; if (!identical(isolate(input$period), p)) updateSelectInput(session, "period", selected = p) })
  observeEvent(input$period, { if (has_game() && !identical(input$period, gstate()$period)) set_state("period", input$period) })

  # ---- live refresh: notice plays and lineup changes made by other keepers ----
  poll <- reactivePoll(1500, session, checkFunc = function() {
    gid <- isolate(input$game_id); if (is.null(gid) || gid == "") return("")
    paste(unlist(dbGetQuery(con, "SELECT COUNT(*), MAX(event_id), TOTAL(player_id), TOTAL(made), TOTAL(points)
                                  FROM events WHERE game_id = ?", params = list(as.integer(gid)))),
          dbGetQuery(con, "SELECT version FROM game_state WHERE game_id = ?", params = list(as.integer(gid)))$version,
          unlist(dbGetQuery(con, "SELECT COUNT(*), MAX(player_id), TOTAL(active) FROM players")), collapse = "|")
  }, valueFunc = function() TRUE)
  observeEvent(poll(), bump(), ignoreInit = TRUE)
  observe({
    invalidateLater(5000); gid <- if (has_game()) as.integer(input$game_id) else NA
    dbExecute(con, "INSERT OR REPLACE INTO keepers VALUES (?,?,?,?,?)",
              params = list(sid, gid, keeper_name(), role(), as.numeric(Sys.time())))
  })
  keeper_name <- reactive(if (nzchar(trimws(input$keeper %||% ""))) trimws(input$keeper) else "Keeper")
  output$keepers_ui <- renderUI({
    invalidateLater(5000); req(has_game())
    k <- dbGetQuery(con, "SELECT * FROM keepers WHERE game_id = ? AND session_id != ? AND last_seen > ?",
                    params = list(as.integer(input$game_id), sid, as.numeric(Sys.time()) - 20))
    rl <- setNames(c("everything", "scorer", "possession"), c("all", "scorer", "poss"))
    if (!nrow(k)) return(span("No one else is keeping this game."))
    span("Also keeping this game: ", HTML(paste(sprintf("<b>%s</b> (%s)", htmltools::htmlEscape(k$name), rl[k$role]), collapse = ", ")))
  })

  # ---- pickers (Team = team-level stat) ----------------------------------
  oc <- function(s) { v <- if (has_game()) gstate()[[s]] else ""; ids <- strsplit(v %||% "", ",")[[1]]; ids[nzchar(ids)] }
  pick <- function(s) { v <- input[[paste0("pick_", s)]]; if (is.null(v) || v == "T") NA else v }
  plist <- function(ids) setNames(ids, lab()[ids])
  for (s0 in c("us", "opp")) local({
    s <- s0
    output[[paste0("pick_", s, "_ui")]] <- renderUI({
      ids <- oc(s); sel <- isolate(input[[paste0("pick_", s)]])
      if (is.null(sel) || !(sel %in% c("T", ids))) sel <- if (length(ids)) ids[1] else "T"
      radioButtons(paste0("pick_", s), NULL, c("Team" = "T", plist(ids)), selected = sel, inline = TRUE)
    })
    output[[paste0("lumsg_", s)]] <- renderUI({ n <- length(oc(s))
      if (n != 5) div(class = "lineup-warn", sprintf("%d checked (expected 5)", n)) })
  })

  need_game <- function() { if (!has_game()) { showNotification("Create or select a game first.", type = "error"); FALSE } else TRUE }
  E <- function(team, type, player = NA, ...) {
    args <- list(...)
    if (is.null(args$linked) || is.na(args$linked)) dup_check(team, type, player)
    add_event(con, input$game_id, team, type, player,
      period = isolate(gstate()$period) %||% input$period, clock = input$clock,
      on_court = paste(oc("us"), collapse = ","), opp_on_court = paste(oc("opp"), collapse = ","),
      entered_by = keeper_name(), session_id = sid, ...)
  }
  # Warn when another keeper entered the same kind of play for the same player in the last few seconds
  dup_check <- function(team, type, player) {
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

  output$scoreline <- renderText({
    if (!has_game() || !nrow(cur_game())) return("No game selected")
    ev <- cur_ev(); sprintf("%s %d - %d %s", team_name(), sum(ev$pts[ev$team == "us"]),
                            sum(ev$pts[ev$team == "opp"]), cur_game()$opponent)
  })

  # ---- court and shot entry -------------------------------------------
  output$court <- renderPlot({
    g <- if (has_game() && nrow(cur_game())) as.list(cur_game()[1, ]) else list()
    ev <- if (length(g)) cur_ev() else data.frame(type = character(0))
    fg <- ev[ev$type == "FG" & ev$team == input$rec_team, , drop = FALSE]
    shot_chart(fg, g, paste("Shots this game:", if (input$rec_team == "us") team_name() else names2()[2]))
  })

  open_shot <- function(team, shooter, x = NA, y = NA, pts = NULL, made = NULL) {
    if (team == "us" && is.na(shooter)) return(showNotification("Pick one of our players for a shot.", type = "warning"))
    o <- other(team); g <- cur_game()
    if (is.null(pts)) pts <- if (is_three(x, y, g$three_dist %||% 19.75, g$corner_dist %||% 19.75)) 3 else 2
    rv$shot <- list(x = x, y = y, team = team, shooter = shooter)
    mates <- setdiff(oc(team), shooter); defs <- oc(o)
    tn <- setNames(names2(), c("us", "opp"))
    reb <- c("Not recorded" = "", setNames(paste0("S:", oc(team)), paste(lab()[oc(team)], "(off)")),
             setNames(paste0("D:", defs), paste(lab()[defs], "(def)")),
             setNames("S:T", paste(tn[team], "team (off)")), setNames("D:T", paste(tn[o], "team (def)")))
    near <- !is.na(x) && shot_dist(x, y) < 4
    showModal(modalDialog(title = paste("Shot:", if (is.na(shooter)) paste(tn[team], "(team)") else lab()[shooter]),
      div(class = "pick", radioButtons("shot_made", NULL, c("Made", "Missed"), inline = TRUE,
                                       selected = if (is.null(made)) character(0) else if (made) "Made" else "Missed")),
      div(class = "pick", radioButtons("shot_pts", "Points", c("2", "3"), selected = as.character(pts), inline = TRUE)),
      div(class = "pick", radioButtons("shot_rating", "Shot rating", SHOT_RATINGS, inline = TRUE, selected = character(0))),
      selectInput("shot_type", "Shot type", c("", SHOT_TYPES), selected = if (near) "Layup" else ""),
      checkboxInput("shot_trans", "Transition / fast-break shot", FALSE),
      conditionalPanel("input.shot_made == 'Made'", selectInput("shot_ast", "Assisted by", c("None" = "", plist(mates)))),
      conditionalPanel("input.shot_made == 'Missed'",
        selectInput("shot_reb", "Rebound", reb),
        selectInput("shot_blk", "Blocked by", c("None" = "", plist(defs))),
        selectInput("shot_astatt", "Assist attempt (pass that set up the miss)", c("None" = "", plist(mates))),
        conditionalPanel("input.shot_reb && input.shot_reb.indexOf('D:') == 0",
          selectInput("shot_outlet", "Outlet pass by", c("No outlet / not recorded" = "", plist(defs))),
          div(class = "pick", radioButtons("shot_push", "After the outlet", c("Pushed in transition" = "push",
                                           "Set up half-court" = "setup"), inline = TRUE, selected = character(0))))),
      footer = tagList(modalButton("Cancel"), actionButton("save_shot", "Save", class = "btn-primary"))))
  }

  observeEvent(input$court_click, {
    if (!need_game()) return()
    if (role() == "poss") return(showNotification("Shots are entered by the scorer. Switch your job to record shots.", type = "warning"))
    t <- input$rec_team; open_shot(t, pick(t), input$court_click$x, input$court_click$y)
  })

  observeEvent(input$save_shot, {
    s <- rv$shot; req(s)
    if (is.null(input$shot_made)) return(showNotification("Choose Made or Missed.", type = "warning"))
    made <- input$shot_made == "Made"; t <- s$team; o <- other(t)
    id <- E(t, "FG", s$shooter, points = as.integer(input$shot_pts), made = made,
            shot_type = if (nzchar(input$shot_type)) input$shot_type else NA,
            rating = input$shot_rating %||% NA, x = s$x, y = s$y, tag = if (isTRUE(input$shot_trans)) "trans" else NA)
    if (made && nzchar(input$shot_ast %||% "")) E(t, "AST", input$shot_ast, linked = id)
    if (!made) {
      if (nzchar(input$shot_astatt %||% "")) E(t, "AST_ATT", input$shot_astatt, linked = id)
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

  # ---- action buttons + statistician prompts ---------------------------
  ask <- function(kind, side, ids, title, choices, dast = NULL) {
    if (!isTRUE(input$prompts)) return()
    rv$prompt <- list(kind = kind, side = side, ids = ids)
    showModal(modalDialog(title = title,
      div(class = "pick", radioButtons("prompt_pick", NULL, choices, inline = TRUE, selected = character(0))),
      if (length(dast)) tagList(h5("Defensive assist?"),
        div(class = "pick", radioButtons("prompt_dast", NULL, c("None" = "", dast), inline = TRUE, selected = "")),
        h5("Pushed in transition?"),
        div(class = "pick", radioButtons("prompt_push", NULL, c("Yes" = "yes", "No" = "no"), inline = TRUE, selected = "no"))),
      footer = tagList(modalButton("Skip"), actionButton("save_prompt", "Save", class = "btn-primary"))))
  }

  handle_action <- function(s, k) {
    if (!need_game()) return()
    p <- pick(s); o <- other(s); them <- plist(oc(o)); tn <- setNames(names2(), c("us", "opp"))
    who <- if (is.na(p)) paste(tn[s], "(team)") else lab()[p]
    if (k %in% c("FG2M", "FG2X", "FG3M", "FG3X"))
      return(open_shot(s, p, pts = as.integer(substr(k, 3, 3)), made = substr(k, 4, 4) == "M"))
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
  for (s0 in c("us", "opp")) for (k0 in names(ACTIONS)) local({
    s <- s0; k <- k0; observeEvent(input[[paste0("act_", s, "_", k)]], handle_action(s, k))
  })

  observeEvent(input$save_prompt, {
    pr <- rv$prompt; req(pr); v <- input$prompt_pick %||% ""; o <- other(pr$side)
    if (nzchar(v)) switch(pr$kind,
      stl = set_player(pr$ids$to, v),
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
  observeEvent(input$new_game, showModal(modalDialog(title = "New game",
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
    gid <- new_game(con, input$ng_date, opp, input$ng_loc, input$ng_fmt, input$ng_per, input$ng_min, input$ng_three)
    toks <- trimws(strsplit(input$ng_roster %||% "", ",")[[1]]); toks <- toks[nzchar(toks)]
    if (length(toks)) ensure_roster(con, opp, sub("^#?([0-9]+).*$", "\\1", toks), trimws(sub("^#?[0-9]+", "", toks)))
    removeModal(); bump()
    session$onFlushed(function() updateSelectInput(session, "game_id", selected = gid))
  })

  # =========================== REPORTS ==================================
  rep_ids <- reactive({
    if ((input$rep_type %||% "Game") == "Game") as.integer(input$rep_game %||% NA)
    else if (length(input$rep_games)) as.integer(input$rep_games) else games()$game_id
  })
  rep_ev <- reactive({ rv$v; get_events(con, rep_ids()) })
  rep_g <- reactive({ g <- games()[games()$game_id %in% rep_ids(), ]; if (nrow(g)) as.list(g[1, ]) else list() })
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
    g <- games()[games()$game_id %in% rep_ids(), ]; g <- g[order(g$game_date), ]; ev_all <- rep_ev()
    rows <- lapply(seq_len(nrow(g)), function(i) {
      b <- box_score(ev_all[ev_all$game_id == g$game_id[i], ], players(), weights(), pl_side())
      if (is.null(b)) return(NULL); b <- b[b$player_id == pl_id(), ]; if (!nrow(b)) return(NULL)
      cbind(Date = g$game_date[i], Opponent = g$opponent[i], clean_box(b)[, -(1:2)])
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
    d <- phase_summary(rep_pt(), rep_names()); req(nrow(d))
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
  cmp_tab <- tabPanel("Graphical Comparison", lapply(seq_along(METRIC_GROUPS), function(i)
    tagList(h4(names(METRIC_GROUPS)[i]), plotOutput(paste0("rep_cmp_", i), height = "230px"))))
  lu_tab <- tabPanel("Lineups",
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
      shot_tabs(TRUE), cmp_tab,
      tabPanel("Period Comparison", helpText("Green = period won on that stat, red = lost (lower is better for TO and fouls)."),
               DTOutput("rep_period_cmp")),
      trans_tab(),
      tabPanel("Timeline", fluidRow(column(3, numericInput("run_min", "Highlight runs of at least", 8, min = 2))),
               textOutput("rep_flow_stats"), plotOutput("rep_timeline", height = "380px"), h5("Runs"), DTOutput("rep_runs")),
      lu_tab,
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
      trans_tab(), shot_tabs(FALSE), cmp_tab, lu_tab,
      tabPanel("Team Goals", h5("Hit rate"), DTOutput("rep_goal_rate"), h5("Trends"),
               plotOutput("rep_goal_trend", height = "auto")))
  })

  output$dl_box <- downloadHandler(function() paste0("box_scores_", Sys.Date(), ".csv"), function(f) {
    b <- all_box(); b$player_id <- NULL; b$Team <- NULL; write.csv(b, f, row.names = FALSE) })
  output$dl_events <- downloadHandler(function() paste0("events_", Sys.Date(), ".csv"), function(f) {
    ev <- rep_ev(); ev$player <- lab()[as.character(ev$player_id)]; write.csv(ev, f, row.names = FALSE) })

  # =========================== SETTINGS =================================
  observeEvent(input$save_team, { dbExecute(con, "UPDATE settings SET value=? WHERE key='team_name'",
                                            params = list(input$team_name)); bump() })
  squad_tbl <- reactive({ p <- players(); p[p$squad == (input$roster_squad %||% "us"), ] })
  output$roster_tbl <- renderDT({ p <- squad_tbl()
    datatable(data.frame(`#` = p$number, Name = p$name, Pos = p$position, Active = ifelse(p$active == 1, "Yes", "No"),
                         check.names = FALSE), rownames = FALSE, selection = "single", options = list(dom = "t", pageLength = 50)) })
  observeEvent(input$add_player, { req(nzchar(input$p_num) || nzchar(input$p_name))
    dbExecute(con, "INSERT INTO players (number, name, position, squad) VALUES (?,?,?,?)",
              params = list(input$p_num, input$p_name, input$p_pos, input$roster_squad %||% "us"))
    for (i in c("p_num", "p_name", "p_pos")) updateTextInput(session, i, value = ""); bump() })
  observeEvent(input$toggle_player, { i <- input$roster_tbl_rows_selected; req(i)
    dbExecute(con, "UPDATE players SET active = 1 - active WHERE player_id = ?", params = list(squad_tbl()$player_id[i])); bump() })
  output$goals_tbl <- renderDT(datatable(goals()[, -1], rownames = FALSE, selection = "single", options = list(dom = "t")))
  observeEvent(input$add_goal, {
    lbl <- if (nzchar(input$g_label)) input$g_label else paste(input$g_side, input$g_metric, input$g_cmp, input$g_target)
    dbExecute(con, "INSERT INTO goals (label, side, metric, comparator, target) VALUES (?,?,?,?,?)",
              params = list(lbl, input$g_side, input$g_metric, input$g_cmp, input$g_target)); bump() })
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
    showNotification("Demo rosters and games loaded.") })

  # =========================== IMPORT / EXPORT ==========================
  output$dl_xml <- downloadHandler(
    function() { g <- games()[games()$game_id == as.integer(input$io_game), ]
      paste0("game_", g$game_date, "_", gsub("[^A-Za-z0-9]", "", g$opponent), ".xml") },
    function(f) export_game_xml(con, as.integer(input$io_game), f))
  output$dl_db <- downloadHandler(function() paste0("courtside_", Sys.Date(), ".sqlite"), function(f) file.copy(DB_PATH, f))
  observeEvent(input$xml_in, {
    gid <- tryCatch(import_game_xml(con, input$xml_in$datapath), error = function(e) {
      showNotification(paste("Import failed:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(gid)) { bump(); showNotification("Game imported.") }
  })
}

shinyApp(ui, server)
