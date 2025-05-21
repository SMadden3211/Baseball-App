# ---- Libraries ----
library(tidyverse)
library(shiny)
library(shinyMobile)
library(DT)
library(readxl)
library(DBI)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(webshot2)
library(htmltools)
library(base64enc)
library(RPostgres)

# ---- App Configuration ----
app_config <- list(
  title = "Statesmen Statcast",
  theme = "auto",
  dark = TRUE,
  color = "#007AFF"
)

# ---- Constants ----
player_type <- c("","Pitcher", "Hitter", "Two-Way")
batter_handed <- c("","Right", "Left", "Switch")
throwing_handed <- c("","Right", "Left")
player_status <- c("","Active", "Graduated", "Transferred", "Redshirt", "Inactive")
the_pitch_types <- c("FB", "CB", "SL", "CH", "SP", "CT", "OTH")
ball_flight_types <- c("Ground Ball", "Line Drive", "Fly Ball", "Pop-Up", "Bunt")
contact_quality_types <- c("Weak", "Average", "Hard", "Bunt")
available_outcomes <- c("Out", "Single", "Double", "Triple", "Homerun", "Error")
# Finding Runner Positions
position_map <- c("Stay on First" = 1, "First" = 1, "Stay on Second" = 2, "Second" = 2,
                  "Stay on Third" = 3, "Third" = 3, "Home" = 4, "Out" = 5, "Single" = 1, "Double" = 2, 
                  "Triple" = 3, "Homerun" = 4, "Error" = 1, "Fielder's Choice" = 1, "Double Play" = 5, 
                  "Triple Play" = 5, "Sacrifice Fly" = 5, "Choose Option" = 5, "None" = 5)

# ---- Dynamic UI Pages ----
# Start Page
startPage <- function() {
  f7Page(
    f7Block(
      f7BlockHeader(
        div(
          img(
            src = "logo.png",
            style = "max-width: 250px; height: auto;",
            alt = "School Logo"
          )
        )
      )
    ),
    f7Block(
      f7Button(inputId = "create_new_event", "Play", fill = TRUE, color = "primary"),
      f7Button(inputId = "stats", "Statistics", fill = TRUE),
      f7Button(inputId = "go_to_list", "Manage Players", fill = TRUE),
      f7Button(inputId = "settings", "Tables", fill = TRUE)
    )
  )
}

# Full Data Tables Page
settingsPage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Data Tables",
      leftPanel = TRUE,
      rightPanel = FALSE,
      hairline = TRUE,
      subNavbar = NULL
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Home")
    ),
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        tabName = "Data",
        icon = f7Icon("table_fill"),
        active = TRUE,
        f7Block(
          f7Button(inputId = "add_team_database", "Add Team to Database", color = "primary", fill = TRUE)
        ),
        f7Block(
          uiOutput("table_select_ui"),
          f7Button(inputId = "download_selected_table", "Download CSV", color = "success", fill = TRUE),
          DT::dataTableOutput("table_display")
        )
      ),
      f7Tab(
        tabName = "Help",
        icon = f7Icon("question_circle_fill")
      )
    )
  )
}

# Input Single Player Page
inputPage <- function(current_year) {
  f7Page(
    navbar = f7Navbar(
      title = "Enter Player Information",
      leftPanel = TRUE,
      hairline = TRUE
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Back")
    ),
    f7Block(
      uiOutput("team_selection_ui"),
      f7Text(inputId = "first_name", label = "First Name", placeholder = "Enter first name"),
      f7Text(inputId = "last_name", label = "Last Name", placeholder = "Enter last name"),
      f7Stepper(inputId = "jersey_number", label = "Jersey Number", min = 0, max = 99, step = 1),
      f7Select(inputId = "player_type", label = "Player Type", choices = player_type),
      f7Stepper(inputId = "grad_year", label = "Graduation Year", value = current_year, min = current_year, step = 1),
      f7Select(inputId = "batter_handedness", label = "Batter Handedness", choices = batter_handed),
      f7Select(inputId = "throwing_handedness", label = "Throwing Handedness", choices = throwing_handed),
      f7Button(inputId = "confirm_player", "Confirm", color = "primary", fill = TRUE)
    )
  )
}

# Full List of Players Page
listPage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "List of Players",
      leftPanel = TRUE,
      hairline = TRUE
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Home")
    ),
    f7Block(
      f7Button(inputId = "add_player", "Add Player", color = "primary", fill = TRUE),
      f7Button(inputId = "player_info", "Update Player", color = "primary", fill = TRUE),
      f7Button(inputId = "import_excel_page", "Import Excel", color = "primary", fill = TRUE)
    ),
    f7Block(
      DTOutput("player_list")
    )
  )
}

# Bulk Import Page For Excel File
importPage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Import Players",
      leftPanel = TRUE,
      hairline = TRUE
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Back")
    ),
    f7Block(
      f7File(inputId = "excel_file", label = "Choose Excel File", accept = ".xlsx"),
      f7Button(inputId = "import_players", "Import", color = "primary", fill = TRUE)
    ),
    f7Block(
      DTOutput("imported_player_preview")
    )
  )
}

# Update Player Page
updatePlayerPage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Update Player",
      leftPanel = TRUE,
      rightPanel = TRUE,
      hairline = TRUE
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Back")
    ),
    f7Panel(
      id = "right-panel",
      side = "right",
      f7Link(href = "#", "Home")
    ),
    f7Tabs(
      animated = TRUE,
      id = "update-tabs",
      f7Tab(
        tabName = "Edit",
        icon = f7Icon("pencil"),
        active = TRUE,
        f7Block(
          f7BlockHeader("Edit Player Information"),
          f7AutoComplete(
            inputId = "edit_player_name",
            label = "Search Player to Edit",
            placeholder = "Start typing a player's name..."
          ),
          verbatimTextOutput("player_info_display_edit"),
          f7Text(inputId = "edit_first_name", label = "First Name"),
          f7Text(inputId = "edit_last_name", label = "Last Name"),
          f7Stepper(inputId = "edit_jersey_number", label = "Jersey Number", min = 0, max = 99, step = 1),
          f7Select(inputId = "edit_player_type", label = "Player Type", choices = player_type),
          f7Stepper(inputId = "edit_grad_year", label = "Graduation Year", min = 2020, step = 1),
          f7Select(inputId = "edit_batter_handedness", label = "Batting Hand", choices = batter_handed),
          f7Select(inputId = "edit_throwing_handedness", label = "Throwing Hand", choices = throwing_handed),
          f7Select(inputId = "edit_status", label = "Status", choices = player_status),
          f7Button(inputId = "update_player", "Confirm Update", color = "primary", fill = TRUE)
        )
      ),
      f7Tab(
        tabName = "Delete",
        icon = f7Icon("trash"),
        f7Block(
          f7BlockHeader("Delete Player"),
          f7AutoComplete(
            inputId = "delete_player_id",
            label = "Search Player to Delete",
            placeholder = "Start typing a player's name..."
          ),
          verbatimTextOutput("player_info_display_delete"),
          f7Button(inputId = "remove_player", "Delete Player", color = "danger", fill = TRUE)
        )
      )
    )
  )
}

# Stats Page
statPage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Statistics",
      leftPanel = TRUE,
      hairline = TRUE
    ),
    f7Panel(
      id = "left-panel",
      side = "left",
      f7Navbar(href = NULL, "Home")
    ),
    f7Tabs(
      animated = TRUE,
      id = "stats-tabs",
      f7Tab(
        tabName = "Pitcher",
        icon = f7Icon("sportscourt"),
        active = TRUE,
        f7Panel(
          id = "pitcher-panel",
          side = "left",
          effect = "reveal",
          f7Block(
            f7Select(inputId = "pitcher_status_of_player", label = "Status", choices = c("Active", "Graduated"), selected = "Active"),
            f7Select(inputId = "pitcher_type_of_event", label = "Event Type", choices = c("Both", "Game", "Practice"), selected = "Both"),
            f7Select(inputId = "pitcher_team", label = "Select Team:", choices = NULL),
            f7AutoComplete(
              inputId = "pitcher_dates",
              label = "Select Dates",
              choices = c("All Dates")
            ),
            f7CheckboxGroup(inputId = "pitcher_players", label = "Select Pitchers:", choices = NULL),
            f7Button(
              inputId = "export_pitcher_reports",
              "Export Reports",
              color = "primary",
              fill = TRUE
            )
          )
        ),
        uiOutput("pitcher_report_output")
      ),
      f7Tab(
        tabName = "Hitter",
        icon = f7Icon("person_crop_circle"),
        f7Panel(
          id = "hitter-panel",
          side = "left",
          effect = "reveal",
          f7Block(
            f7Select(inputId = "hitter_status_of_player", label = "Status", choices = c("Active", "Graduated"), selected = "Active"),
            f7Select(inputId = "hitter_type_of_event", label = "Event Type", choices = c("Both", "Game", "Practice"), selected = "Both"),
            f7Button(
              inputId = "export_hitter_reports",
              "Export Reports",
              color = "primary",
              fill = TRUE
            )
          )
        ),
        uiOutput("hitter_report_output")
      ),
      f7Tab(
        tabName = "Leaderboards",
        icon = f7Icon("chart_bar"),
        f7Block(
          f7Radio(
            inputId = "leaderboard_choice",
            label = "Player Types",
            choices = c("Pitchers", "Hitters"),
            selected = "Pitchers"
          ),
          uiOutput("leaderboard_calculations")
        )
      )
    )
  )
}

# Selecting New Event (Game or Practice) Page - User Interface
newEventPage <- function() {  f7Page(    f7Navbar(
  title = "Game Type",
  left_panel = TRUE,
  hairline = TRUE,
  left_panel = tags$div(
    f7Back("Back")
  )
),    f7Block(
  f7Button(inputId = "type_game", "Game", color = "primary", fill = TRUE),
  f7Button(inputId = "type_practice", "Practice", color = "primary", fill = TRUE)
)
)
}

# Practice Input Page - User Interface
practicePage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Practice",
      hairline = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      id = "practice-tabs",
      f7Tab(
        tabName = "Input",
        icon = f7Icon("sportscourt"),
        active = TRUE,
        f7Block(
          f7BlockHeader(uiOutput("full_inning")),
          uiOutput("practice_pitch_count_and_bf"),
          uiOutput("practice_score"),
          uiOutput("count_and_outs"),
          uiOutput("having_runners"),
          uiOutput("changing_score_ui"),
          f7Button(inputId = "set_the_count_outs", "Change Count / Outs", fill = TRUE),
          uiOutput("next_inning_button"),
          f7Button(inputId = "end_practice", "End Practice", color = "red", fill = TRUE)
        ),
        f7Block(
          f7Row(
            f7Col(
              f7Select(inputId = "practice_pitcher", label = "Current Pitcher", choices = NULL)
            ),
            f7Col(
              f7Select(inputId = "practice_batter", label = "Current Batter", choices = NULL)
            )
          ),
          f7Row(
            f7Col(
              size = 50,
              div(style = "position: relative;",
                  plotOutput("strike_zone", click = "strike_zone_click"),
                  uiOutput("floating_buttons")
              ),
              f7Slider(
                inputId = "pitch_speed",
                label = "Pitch Speed",
                min = 65,
                max = 95,
                value = 65,
                scale = TRUE
              ),
              f7RadioGroup(
                inputId = "pitch_type",
                label = "",
                choices = the_pitch_types,
                selected = character(0)
              )
            ),
            f7Col(
              size = 50,
              f7Block(
                f7Button(inputId = "practice_called_strike", "Called Strike", fill = TRUE),
                f7Button(inputId = "practice_swing_strike", "Swinging Strike", fill = TRUE),
                f7Button(inputId = "practice_foul_ball", "Foul Ball", fill = TRUE),
                f7Button(inputId = "practice_ball", "Ball", fill = TRUE),
                f7Button(inputId = "practice_hit_by_pitch", "Hit By Pitch", fill = TRUE),
                f7Button(inputId = "practice_ball_in_play", "Ball in Play", fill = TRUE),
                uiOutput("the_practice_runners"),
                f7Button(inputId = "practice_undo_pitch", "Undo Last Pitch", color = "red", fill = TRUE)
              )
            )
          )
        )
      ),
      f7Tab(
        tabName = "Pitches",
        icon = f7Icon("chart_bar"),
        f7Block(
          DTOutput("pitch_table_current_game")
        )
      ),
      f7Tab(
        tabName = "Help",
        icon = f7Icon("question_circle"),
        f7Block(
          f7BlockHeader("Contact Us"),
          f7BlockFooter(
            "If you are in need of help, contact our IT department at:",
            tags$br(),
            "andrew.gennaro@hws.edu"
          )
        )
      )
    )
  )
}

# Practice Options Page - (What kind of practice is it?)
beforePracticePage <- function() {
  f7Page(    f7Navbar(
    title = "Practice Settings",
    leftPanel = TRUE,
    hairline = TRUE,
    left = tags$div(
      f7Back("Home")
    )
  ),f7Block(
    f7Select(inputId = "option_innings", label = "Track Innings?", choices = c("Choose", "Yes", "No"), selected = "Choose"),
    f7Select(inputId = "option_score", label = "Track Score?", choices = c("Choose", "Yes", "No"), selected = "Choose"),
    f7Select(inputId = "option_runners", label = "Track Base Runners?", choices = c("Choose", "Yes", "No"), selected = "Choose"),
    f7Select(inputId = "option_bip_outcome", label = "Track Ball in Play Outcome?", help = "If Live BP in batting cage, select No", choices = c("Choose", "Yes", "No"), selected = "Choose"),
    f7Select(inputId = "practice_team", label = "Opponent Team", help = "If this is just an intersquad, choose Hobart College", choices = NULL),
    f7Button(inputId = "go_practice", "Confirm", color = "primary", fill = TRUE)
  )
  )
}

# Setting Lineups for Game Page - User Interface 
lineupPage <- function() {
  f7Page(
    f7Navbar(
      title = "Set Your Lineups",
      left_panel = TRUE,
      hairline = TRUE,
      left_panel = tags$div(f7Back("Home"))
    ),
    f7Block(
      uiOutput("inning_specifier_ui")  # optional: dynamic rendering for innings
    ),
    f7Tabs(
      animated = TRUE,
      f7Tab(
        tabName = "Home",
        icon = "house",
        active = TRUE,
        f7Block(
          f7BlockHeader("Home Team Lineup"),
          uiOutput("home_team_table")  # ⬅️ this now renders full team lineup (team dropdown + all 10 spots)
        )
      ),
      f7Tab(
        tabName = "Away",
        icon = "airplane",
        f7Block(
          f7BlockHeader("Away Team Lineup"),
          uiOutput("away_team_table")  # ⬅️ same here
        )
      )
    ),
    f7Block(
      f7Button(inputId = "to_game", "Confirm Lineups", color = "primary", fill = TRUE)
    )
  )
}

# Game Input Page - User Interface
gamePage <- function() {
  f7Page(
    navbar = f7Navbar(
      title = "Game",
      hairline = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      id = "game-tabs",
      f7Tab(
        tabName = "Pitch",
        icon = f7Icon("sportscourt"),
        active = TRUE,
        f7Block(
          f7Row(
            f7Col(
              size = 25,
              htmlOutput("current_matchup_label")
            ),
            f7Col(
              size = 25,
              div(class = "score-team", textOutput("away_name")),
              div(class = "score-runs", textOutput("away_score")),
              div(class = "score-team", textOutput("home_name")),
              div(class = "score-runs", textOutput("home_score"))
            ),
            f7Col(
              size = 25,
              plotOutput("bases_graphic", height = "95px")
            ),
            f7Col(
              size = 25,
              div(class = "inning-display",
                  textOutput("inning_number"),
                  uiOutput("inning_arrow")
              ),
              uiOutput("count_and_outs")
            )
          )
        ),
        f7Block(
          f7Row(
            f7Col(
              size = 50,
              div(style = "position: relative;",
                  plotOutput("strike_zone", click = "strike_zone_click"),
                  uiOutput("floating_buttons")
              ),
              f7Slider(
                inputId = "pitch_speed",
                label = "Pitch Speed",
                min = 65,
                max = 95,
                value = 65,
                scale = TRUE
              ),
              f7RadioGroup(
                inputId = "pitch_type",
                label = "",
                choices = the_pitch_types,
                selected = character(0)
              )
            ),
            f7Col(
              size = 50,
              f7Button(inputId = "called_strike", "Called Strike", fill = TRUE),
              f7Button(inputId = "swing_strike", "Swinging Strike", fill = TRUE),
              f7Button(inputId = "foul_ball", "Foul Ball", fill = TRUE),
              f7Button(inputId = "ball", "Ball", fill = TRUE),
              f7Button(inputId = "hit_by_pitch", "Hit By Pitch", fill = TRUE),
              f7Button(inputId = "ball_in_play", "Ball in Play", fill = TRUE),
              f7Button(inputId = "runners_advance", "Runners Advance", fill = TRUE),
              f7Button(inputId = "undo_pitch", "Undo Last Pitch", color = "red", fill = TRUE)
            )
          )
        )
      ),
      f7Tab(
        tabName = "Inning",
        icon = f7Icon("chart_bar"),
        f7Block(
          f7Button(inputId = "add_inning", "Add Inning", color = "primary", fill = TRUE),
          uiOutput("all_half_inning_table")
        )
      ),
      f7Tab(
        tabName = "Lineups",
        icon = f7Icon("person_2"),
        f7Block(
          f7Row(
            f7Col(
              size = 50,
              f7BlockHeader("Home Team Lineup"),
              uiOutput("home_team_table")
            ),
            f7Col(
              size = 50,
              f7BlockHeader("Away Team Lineup"),
              uiOutput("away_team_table")
            )
          )
        )
      ),
      f7Tab(
        tabName = "Help",
        icon = f7Icon("question_circle"),
        f7Block(
          f7BlockHeader("Contact Us"),
          f7BlockFooter(
            "If you are in need of help, contact our IT department at:",
            tags$br(),
            "andrew.gennaro@hws.edu"
          )
        )
      ),
      f7Tab(
        tabName = "End",
        icon = f7Icon("xmark"),
        f7Block(
          f7Button(inputId = "go_home", "End Game Early", color = "red", fill = TRUE)
        )
      )
    )
  )
}

# ---- Drawing Field Functions ----
# Baseball field gamplay look
draw_baseball_field <- function() {
  
  # Pitcher's mound location
  mound_x <- 60.5 * cos(pi/4)  # ≈ 42.8
  mound_y <- 60.5 * sin(pi/4)  # ≈ 42.8
  # Infield Dirt Full Circle
  full_dirt <- data.frame(
    x = 100 + mound_x + 95 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + mound_y + 95 * sin(seq(0, 2 * pi, length.out = 100))
  )
  #Foul Territory
  foul_area_third <- data.frame(
    x = c(40, 96, 96, 40),
    y = c(40, 40, 250, 250)
  )
  foul_area_first <- data.frame(
    x = c(40, 250, 250, 40),
    y = c(40, 40, 96, 96)
  )
  # Infield Grass
  diamond <- data.frame(
    x = c(103, 187, 187, 103),
    y = c(103, 103, 187, 187)
  )
  #Different areas of dirt
  home_plate_dirt <- data.frame(
    x = 102 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 102 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  first_base_dirt <- data.frame(
    x = 190 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  second_base_dirt <- data.frame(
    x = 190 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 190 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  third_base_dirt <- data.frame(
    x = 100 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 190 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  # Bases as points
  bases <- data.frame(
    x = c(102, 192, 192, 102),
    y = c(102, 102, 192, 192)
  )
  # Foul lines from home plate
  foul_line1 <- data.frame(x = c(100, 350), y = c(100, 100))
  foul_line2 <- data.frame(x = c(100, 100), y = c(100, 350))
  # Mount dirt
  mound_dirt <- data.frame(
    x = 100 + mound_x + 9 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + mound_y + 9 * sin(seq(0, 2 * pi, length.out = 100))
  )
  # Outfield fence as an arc (using polar coordinates)
  # Define the points
  p1_bsblfd <- c(100, 350)
  p2_bsblfd <- c(278.78,278.78)
  p3_bsblfd <- c(350, 100)
  
  # Center of the circle
  center_bsblfd <- c((p1_bsblfd[1] + p3_bsblfd[1]) / 2, (p1_bsblfd[2] + p3_bsblfd[2]) / 2)
  # Calculating the radius of the circle
  radius_bsblfd <- sqrt((p1_bsblfd[1] - center_bsblfd[1])^2 + (p1_bsblfd[2] - center_bsblfd[2])^2)
  # Generate the angles for the arc (between p1 and p3)
  theta_bsblfd <- seq(atan2(p1_bsblfd[2] - center_bsblfd[2], p1_bsblfd[1] - center_bsblfd[1]),
                      atan2(p3_bsblfd[2] - center_bsblfd[2], p3_bsblfd[1] - center_bsblfd[1]), length.out = 100)
  # Calculate the coordinates along the arc
  arc_points_bsblfd <- data.frame(
    x = center_bsblfd[1] + radius_bsblfd * cos(theta_bsblfd),
    y = center_bsblfd[2] + radius_bsblfd * sin(theta_bsblfd)
  )
  
  # Build the plot
  p <- ggplot() +
    theme_void() +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "#009000", color = NA)) +
    # Dirt as tan, grass as green
    geom_polygon(data = full_dirt, aes(x = x, y =y),
                 fill = "tan", size = 1) +
    geom_polygon(data = diamond, aes(x = x, y = y),
                 fill = "#009000", size = 1) +
    geom_polygon(data = first_base_dirt, aes(x = x, y = y),
                 fill = "tan", size = 1) +
    geom_polygon(data = second_base_dirt, aes(x = x, y = y),
                 fill = "tan", size = 1) +
    geom_polygon(data = third_base_dirt, aes(x = x, y = y),
                 fill = "tan", size = 1) +
    geom_polygon(data = foul_area_third, aes(x = x, y = y), 
                 fill = "#009000", size = 1) +
    geom_polygon(data = foul_area_first, aes(x = x, y = y), 
                 fill = "#009000", size = 1) +
    geom_polygon(data = home_plate_dirt, aes(x = x, y = y),
                 fill = "tan", size = 1) +
    geom_polygon(data = mound_dirt, aes(x = x, y = y),
                 fill = "tan", size = 1) +
    geom_point(data = bases, aes(x = x, y = y),
               color = "white", size = 2, shape = 15) +
    geom_line(data = foul_line1, aes(x = x, y = y), color = "white") +
    geom_line(data = foul_line2, aes(x = x, y = y), color = "white") +
    geom_path(data = arc_points_bsblfd, aes(x = x, y = y), color = "yellow", size = 1) +
    
    xlim(40, 420) + ylim(40,420) +
    coord_fixed()
  
  return(p)
}

# Same field but used for spray charts in hitter and pitcher outputs
draw_spraychart_field <- function() {
  
  # Pitcher's mound location
  mound_x <- 60.5 * cos(pi/4)  # ≈ 42.8
  mound_y <- 60.5 * sin(pi/4)  # ≈ 42.8
  # Infield Dirt Full Circle
  full_dirt <- data.frame(
    x = 100 + mound_x + 95 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + mound_y + 95 * sin(seq(0, 2 * pi, length.out = 100))
  )
  #Foul Territory
  foul_area_third <- data.frame(
    x = c(40, 96, 96, 40),
    y = c(40, 40, 250, 250)
  )
  foul_area_first <- data.frame(
    x = c(40, 250, 250, 40),
    y = c(40, 40, 96, 96)
  )
  # Infield Grass
  diamond <- data.frame(
    x = c(103, 187, 187, 103),
    y = c(103, 103, 187, 187)
  )
  #Different places of dirt
  home_plate_dirt <- data.frame(
    x = 102 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 102 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  first_base_dirt <- data.frame(
    x = 190 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  second_base_dirt <- data.frame(
    x = 190 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 190 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  third_base_dirt <- data.frame(
    x = 100 + 13 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 190 + 13 * sin(seq(0, 2 * pi, length.out = 100))
  )
  # Bases as points
  bases <- data.frame(
    x = c(102, 192, 192, 102),
    y = c(102, 102, 192, 192)
  )
  # Foul lines from home plate
  foul_line1 <- data.frame(x = c(100, 350), y = c(100, 100))
  foul_line2 <- data.frame(x = c(100, 100), y = c(100, 350))
  # Mount dirt
  mound_dirt <- data.frame(
    x = 100 + mound_x + 9 * cos(seq(0, 2 * pi, length.out = 100)),
    y = 100 + mound_y + 9 * sin(seq(0, 2 * pi, length.out = 100))
  )
  # Outfield fence as an arc (using polar coordinates)
  p1_bsblfd <- c(100, 350)
  p2_bsblfd <- c(278.78,278.78)
  p3_bsblfd <- c(350, 100)
  
  # Find the center of the circle
  center_bsblfd <- c((p1_bsblfd[1] + p3_bsblfd[1]) / 2, (p1_bsblfd[2] + p3_bsblfd[2]) / 2)
  # Calculate the radius of the circle
  radius_bsblfd <- sqrt((p1_bsblfd[1] - center_bsblfd[1])^2 + (p1_bsblfd[2] - center_bsblfd[2])^2)
  # Generate the angles for the arc (between p1 and p3)
  theta_bsblfd <- seq(atan2(p1_bsblfd[2] - center_bsblfd[2], p1_bsblfd[1] - center_bsblfd[1]),
                      atan2(p3_bsblfd[2] - center_bsblfd[2], p3_bsblfd[1] - center_bsblfd[1]), length.out = 100)
  # Calculate the coordinates along the arc
  arc_points_bsblfd <- data.frame(
    x = center_bsblfd[1] + radius_bsblfd * cos(theta_bsblfd),
    y = center_bsblfd[2] + radius_bsblfd * sin(theta_bsblfd)
  )
  
  
  # Build the plot
  p <- ggplot() +
    theme_void() +
    coord_fixed() +
    theme(panel.background = element_rect(fill = "white", color = NA)) +
    # They did not want color on the field, only for the data points
    geom_polygon(data = full_dirt, aes(x = x, y =y),
                 fill = "lightgray", size = 1) +
    geom_polygon(data = diamond, aes(x = x, y = y),
                 fill = "white", size = 1) +
    geom_polygon(data = first_base_dirt, aes(x = x, y = y),
                 fill = "lightgray", size = 1) +
    geom_polygon(data = second_base_dirt, aes(x = x, y = y),
                 fill = "lightgray", size = 1) +
    geom_polygon(data = third_base_dirt, aes(x = x, y = y),
                 fill = "lightgray", size = 1) +
    geom_polygon(data = foul_area_third, aes(x = x, y = y), 
                 fill = "white", size = 1) +
    geom_polygon(data = foul_area_first, aes(x = x, y = y), 
                 fill = "white", size = 1) +
    geom_polygon(data = home_plate_dirt, aes(x = x, y = y),
                 fill = "lightgray", size = 1) +
    geom_polygon(data = mound_dirt, aes(x = x, y = y),
                 fill = "lightgray", size = 1) +
    geom_point(data = bases, aes(x = x, y = y),
               color = "black", size = 2, shape = 15) +
    geom_line(data = foul_line1, aes(x = x, y = y), color = "black") +
    geom_line(data = foul_line2, aes(x = x, y = y), color = "black") +
    geom_path(data = arc_points_bsblfd, aes(x = x, y = y), color = "#e1891e", size = 1) +
    
    xlim(40, 420) + ylim(40,420) +
    coord_fixed()
  
  return(p)
}

ui <- f7Page(
  title = app_config$title,
  options = list(
    theme = "auto",
    dark = TRUE,
    filled = TRUE,
    color = "#007AFF",
    touch = list(
      tapHold = TRUE,
      tapHoldDelay = 750,
      iosTouchRipple = FALSE
    ),
    navbar = list(
      hideOnPageScroll = TRUE
    ),
    pullToRefresh = FALSE
  ),  panels = tagList(
    f7Panel(
      id = "main-panel",
      side = "left",
      theme = "light",
      f7PanelMenu(
        id = "menu",
        f7PanelItem(tabName = "home", "Play!", icon = f7Icon("house_fill")),
        f7PanelItem(tabName = "stats", "Statistics", icon = f7Icon("chart_bar_square")),
        f7PanelItem(tabName = "players", "Players", icon = f7Icon("person_2_fill")),
        f7PanelItem(tabName = "settings", "Settings", icon = f7Icon("gear"))
      )
    )
  ),
  navbar = f7Navbar(
    title = "Statesmen Statcast",
    leftPanel = TRUE,
    hairline = TRUE
  ),
  main = uiOutput("dynamicUI")
)