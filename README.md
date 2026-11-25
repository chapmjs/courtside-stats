# CourtSide Stats

A Shiny app for keeping basketball statistics courtside and producing post-game reports, for one or several of your teams (Varsity, JV, a team you scout). Its feature set is modeled on CyberSports for Basketball and the HoopTactics game reports. It tracks individual players on **both** teams and supports **several stats keepers on one game at once**.

## Features

**Live Entry** (the main entry screen; runs in the browser, built for tablets)
- Every tap shows instantly and is saved on the device, then sent to the server in the background. If the Wi-Fi drops, plays wait on the device (a badge shows how many) and are sent when it reconnects.
- Each keeper picks one or more jobs:
  - *Actions*: both teams' full rosters, with each team's action buttons beside its list. Tap a player, then an action (2 ✓, 2 ✗, 3 ✓, FT ✓, Reb, Ast, TO, Stl, Foul, Fouled, Blk, More). Rebounds are marked offensive or defensive from the last miss, and the other team's half of a steal, charge or drawn foul is added automatically and takes the player if someone taps it within a few seconds. Optional keyboard codes: `12+2`, `12-3`, `12+1`, `5r`, `3t`, `o24f` (opponent #24 foul), `tr` (team rebound).
  - *Shot spots*: tap where the shot was taken on the full court, then the shooter's number. Each spot is matched to the scorer's made/missed shot for the shot charts.
  - *Possessions*: tap who gets the ball and how (defensive rebound, steal/turnover, after a score, after free throws, dead ball, jump ball), plus Fast break. These taps set the possessions and their length in real time, so no game clock is needed for possession stats.
  - *Ball tracker* (your team): the whole roster as big jersey-number buttons in a fixed order, with the five most recent ball-touchers highlighted. Tap the player who gets each pass (one tap = the pass and the new ball handler); tap Drives, Posts up, 2 ✓/✗, 3 ✓/✗, Turnover or Fouled for the ball handler. When your team gets the ball, pick how (def rebound, steal, inbound or jump ball) before tapping the player. Rebounds, free throws and "their ball" screens follow automatically, assists and assist attempts are credited to the last passer when the shot comes within 3 seconds, and lineups are inferred from who touches the ball. Free throws: shooting fouls go to the line for 2 (tap 1 or 3 to change), *Bonus* and *And-1* buttons cover the other trips; after the last free throw a make gives them the ball and a miss asks who rebounded. Turnovers: *Pass picked off* (then tap who it was meant for) or *Turnover (other)* with its kind. A running log of the latest plays sits under the buttons. Feeds the *Ball Movement* report: touches, seconds on the ball, quick decisions (under 2 s), drives and post-ups by player; points per possession by number of passes and by what the possession included; pace; who passes to whom; turnovers by kind (live or dead ball), by player per 100 touches, and when they happen; picked-off passes by who they were meant for.
  - *Clock & subs*: game clock (space bar on a keyboard), period, timeouts, jump balls with the possession arrow, score fixes, and lineups.

**Classic Entry** (the earlier Shiny screen, still available)
 (laid out like CyberSports: rosters down each side, full court in the middle, one action bar)
- Scoreboard with team fouls, bonus / double bonus, timeouts, and a shared Start/Stop game clock that time-stamps every play.
- Full court: each team attacks its own basket, so where you click says who shot. Baskets switch at halftime automatically, with a manual "Switch baskets" button. A half-court view is still available.
- Jersey-number tiles for both teams (on court on top, bench below). Tap a player, then tap an action, or click the court for a shot. Substitute mode moves players on and off with taps.
- Recent plays listed above each roster to catch mistakes quickly.
- 2 vs. 3 points is set from the shot location for the chosen game format (NFHS, NCAA, NBA, FIBA, custom).
- Shot rating (EZ / Open / Contested / Bad), shot type, and a transition flag on each shot.
- Statistician prompts: assist, rebound, block, assist attempt, outlet pass, steal ↔ turnover, charge ↔ offensive foul, foul ↔ foul drawn, defensive assist.
- Outlet passes and fast breaks, so transition possessions can be measured.
- On-court lineups for both teams (used for +/- and lineup reports), and quick-add for opponent jersey numbers.
- Edit and delete any play. **Undo my last** removes only your own plays.

**Several teams**
- Pick the team at the top of the page. Games, rosters, goals and reports follow that team.
- A player can be on more than one of your teams (a JV player who also plays varsity). Season reports split by team, and a player report can include all of her games.
- Goals can apply to one team or to all of them. Opponent rosters are shared across your teams.

**Several keepers** (each person enters a name and picks a job)
- *Everything*: one person keeps the whole game.
- *Plays* + *Clock & subs*: one person records every stat, and the other runs the game clock (space bar starts and stops it), timeouts, the period and substitutions (tap the player coming out, then the player going in).
- *Our team only* + *Opponent only*: each keeper records only what their own team's players do, so each needs to know only one team's numbers. The app links the two halves of a play (a steal and the other team's turnover, a miss and the other team's rebound) when they're entered within a few seconds.
- *Scorer* + *Possession*: split the plays between two people, for a three-person crew.
- Lineups, the clock and the period are shared. Plays from every keeper appear on all screens within about 2 seconds, *Undo my last* removes only your own plays, and a warning appears when two keepers seem to enter the same play.
- Lineup tracking is optional (off by default). Turn it on, or use the Clock & subs job, for +/-, lineup reports and bench points.

**Reports** (game, season/cumulative, player)
- Box scores for both teams, including points off turnovers, second-effort, bench, paint, layup, 3-point, and free-throw points, defensive stops, largest lead, longest run, lead changes, and ties
- Leaders in 22 categories, across both teams
- Play-by-play with running score, lead changes, ties, and period summaries
- Shot charts: game by half, team (with pie charts by location and range), every player, and individual players
- Graphical comparison by category
- Period comparison, color-coded
- Score timeline with scoring runs marked
- Lineup analysis: most used, net rating, rebounding, defense, stops, forced turnovers
- Team goals, with by-period bars for a game and trend lines for a season
- **Transition & Possessions:**
  - Points per possession for transition vs. half-court
  - Results by how the possession started (defensive rebound, turnover, made basket)
  - Outlet-pass stats
  - Queueing-model rates: μ (scoring) and θ (empty possessions) per minute

**Simulator** (new in v8)
- Plays out thousands of games from your logged possessions: win chance, expected score, and the spread of final margins, against an average opponent or a named one (blended with all your games so one game doesn't decide it).
- *What if*: change the transition rate after defensive rebounds and steals, or the length of your possessions, and compare with the games as logged.
- *Check: points in time windows*: points per 1, 2, 3, 5, 7 and 10 minutes and longest scoring droughts, logged vs. simulated, plus a clock check (do the logged possession lengths add up to a 32-minute game?).
- *Check: predict each game*: predicts each logged game from the others and reports how often the real margin fell in the 80% range, the average miss, and a Brier score for the win chance.

**Player Development** (new in v8; from the Ball tracker)
- Touch-level process stats for each player: turnovers per 100 touches, catch-to-action time, quick decisions, paint touches, pass-ahead after a rebound or steal, and touches ending in a shot. Estimates are pulled toward the team average for players with few touches.
- Development cards: strengths, two things to work on with drill ideas to discuss with the coach, and logged minutes by skill.
- Learning curves: one stat game by game with a trend line and a note on whether the trend is bigger than game-to-game noise; practice and workout entries for that skill show as lines.
- Team overview with strengths and needs colour-coded.
- Practice & workout log: what the team practiced and what each player worked on outside practice.

**Other**
- Customizable team goals and productivity (+/-) weights
- Game export/import as XML, CSV downloads, full database backup
- Demo data generator (now with Ball tracker data and players who speed up over the season)

## Running it

Required R packages: `shiny`, `DBI`, `RSQLite`, `DT`, `ggplot2`, `xml2`.

```r
install.packages(c("shiny", "DBI", "RSQLite", "DT", "ggplot2", "xml2"))
shiny::runApp("path/to/courtside-stats")
```

On Shiny Server, copy the folder into the apps directory. The `shiny` user needs write access to that folder, because the app saves its data to `bball.sqlite` there. To keep the database somewhere else, set `COURTSIDE_DB`. Older databases are upgraded automatically when the app starts.

## Files

| File | Contents |
|---|---|
| `app.R` | UI and server |
| `R/engine.R` | Database schema and migrations, event model, possession segmentation, team/player metrics |
| `R/charts.R` | Court, shot charts, pies, comparison, timeline, goal charts (ggplot2) |
| `R/reports.R` | Report tables |
| `R/data_io.R` | Demo data, XML import/export |
| `R/teams.R` | Several teams and shared rosters |
| `R/fullcourt.R` | Full-court geometry, clock, team fouls/bonus |
| `R/live.R` | Live Entry sync: snapshots and batched plays |
| `R/touches.R` | Ball tracker: touches, passes, Ball Movement report |
| `R/sim.R` | Game simulator and its two checks |
| `R/develop.R` | Player development stats, cards, learning curves, practice/workout log |
| `www/live.js`, `www/live.css` | Live Entry browser app |

Stat definitions are in the app's **Definitions** tab.
