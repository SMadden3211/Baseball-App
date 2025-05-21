server <- function(input, output, session) {
  # Current dates
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_date <- format(Sys.Date(), "%m/%d/%Y")
  app_state <- reactiveValues(page = "new_event") # Makes app to start on new event page
  
  # ---- SQL setup ----
  # Function to connect to database and set search path
  connect_to_db <- dbConnect(
    Postgres(),
    dbname = "data353_s25",
    user = "data353_s25_gvegasexpos",
    password = "tHFjz28VQyeY",
    host = "postgres.hws.edu",
    port = 5432,
    options = "-c search_path=gvegasexpos"
  )
  dbExecute(connect_to_db, "SET search_path TO gvegasexpos")
  
  # GraduationUpdates table
  dbExecute(connect_to_db, "
    CREATE TABLE IF NOT EXISTS graduation_updates (
    year INTEGER PRIMARY KEY,
    ran_on TEXT NOT NULL
  )")
  
  # Create teams table
  dbExecute(connect_to_db, "
  CREATE TABLE IF NOT EXISTS teams (
    college_name TEXT PRIMARY KEY,
    college_abbreviation TEXT,
    conference TEXT,
    region NUMERIC
  )")
  
  # Create players table
  dbExecute(connect_to_db, "
  CREATE TABLE IF NOT EXISTS players (
    player_id NUMERIC PRIMARY KEY,
    college_name TEXT,
    first_name TEXT,
    last_name TEXT,
    full_name TEXT,
    jersey_number NUMERIC,
    player_type TEXT,
    grad_year NUMERIC,
    batter_handedness TEXT,
    throwing_handedness TEXT,
    status TEXT,
    FOREIGN KEY (college_name) REFERENCES teams(college_name)
  )")
  
  # Create events table
  dbExecute(connect_to_db, "
  CREATE TABLE IF NOT EXISTS events (
    event_id NUMERIC PRIMARY KEY,
    date TEXT,
    event_type TEXT,
    home_team TEXT,
    away_team TEXT,
    FOREIGN KEY (home_team) REFERENCES teams(college_name),
    FOREIGN KEY (away_team) REFERENCES teams(college_name)
  )")
  
  # Update global_pitch_data table
  dbExecute(connect_to_db, "
  CREATE TABLE IF NOT EXISTS global_pitch_data (
    event_id NUMERIC,
    game_pitch_number NUMERIC,
    is_pitch_event BOOLEAN,
    hitter_id NUMERIC,
    pitcher_id NUMERIC,
    pitchers_pitch_count NUMERIC,
    at_bat_pitch_number NUMERIC,
    half_inning TEXT,
    inning NUMERIC,
    outs NUMERIC,
    strikes NUMERIC,
    balls NUMERIC,
    pitch_outcome TEXT,
    pitch_velo NUMERIC,
    pitch_type TEXT,
    pitch_location_x REAL,
    pitch_location_y REAL,
    ball_in_play_outcome TEXT,
    batter_outcome TEXT,
    ball_flight TEXT,
    contact_type TEXT,
    spray_chart_x REAL,
    spray_chart_y REAL,
    first_base_runner NUMERIC,    
    first_base_outcome TEXT,
    first_base_reason TEXT,
    second_base_runner NUMERIC,    
    second_base_outcome TEXT,   
    second_base_reason TEXT,
    third_base_runner NUMERIC,    
    third_base_outcome TEXT,
    third_base_reason TEXT,
    FOREIGN KEY (event_id) REFERENCES events(event_id)
  )")
  
  # If teams table is empty this will automcatically fill it
  excel_path <- "updated_d3_teams.xlsx"
  if (dbGetQuery(connect_to_db, "SELECT COUNT(*) FROM teams")[1, 1] == 0) {
    if (file.exists(excel_path)) {
      division3teams <- readxl::read_xlsx(excel_path)
      teams_data <- division3teams %>%
        select(college_name, college_abbreviation, conference, region) %>%
        mutate(
          college_name = as.character(ifelse(is.na(college_name), "", college_name)),
          college_abbreviation = as.character(ifelse(is.na(college_abbreviation), "", college_abbreviation)),
          conference = as.character(ifelse(is.na(conference), "", conference)),
          region = as.numeric(ifelse(is.na(region), "", region))
        )
      dbWriteTable(connect_to_db, "teams", teams_data, append = TRUE)
      message("Seeded teams table with ", nrow(teams_data), " teams")
    }
  }
  
  # Automatic Updating of player status' in server
  observe({
    today <- Sys.Date()
    current_year <- as.numeric(format(today, "%Y"))
    
    # This only works on July 1st
    if (format(today, "%m-%d") == "07-01") {
      
      # Get all the player data
      already_run <- dbGetQuery(connect_to_db, 
                                "SELECT * FROM GraduationUpdates WHERE year = $1", 
                                params = list(current_year))
      
      # Check if update for this year has already run (That way it does not do it infinity times on July 1st)
      if (nrow(already_run) == 0) {
        # Only updates players that are not inactive or transferred
        dbExecute(connect_to_db, "
        UPDATE players
        SET status = 'Graduated'
        WHERE grad_year <= $1
          AND status NOT IN ('Inactive', 'Transferred')", 
                  params = list(current_year))
        
        # Logs the update run
        dbExecute(connect_to_db, "
        INSERT INTO graduation_updates (year, ran_on) 
        VALUES ($1, $2)", 
                  params = list(current_year, as.character(today)))
        
        message("Graduation status updated for year: ", current_year)
      }
    }
  })
  
  # Clean up database connection when app stops
  onStop(function() {
    dbDisconnect(connect_to_db)
  })
  
  
  # ---- Reactive Databases ----
  ## ---- SQL into Reactive Values  ----
  # teams data from database into reactive value
  teams <- reactiveValues(data = tryCatch({
    data <- dbReadTable(connect_to_db, "teams")
    data$college_name <- as.character(ifelse(is.na(data$college_name), "", data$college_name))
    message("Loaded ", nrow(data), " teams from database")
    data
  }, error = function(e) {
    message("Error loading teams table: ", e$message)
    data.frame(college_name = character(), college_abbreviation = character(), 
               conference = character(), region = numeric())
  }))
  
  # Initialize stored_players directly with open 
  stored_players <- reactiveValues(data = dbReadTable(connect_to_db, "players"))
  # This makes stored_players reactive
  observe({
    stored_players$data <- dbReadTable(connect_to_db, "players")
    message("Refreshed stored_players$data. Rows: ", nrow(stored_players$data))
  })
  
  # This outputs messages about the types of players in stored_players, it looks like it is not necessary
  observe({
    req(stored_players$data)
    if ("batter_handedness" %in% colnames(stored_players$data)) {
      batter_handedness <- stored_players$data$batter_handedness
      message("batter_handedness values (first 10): ", paste(head(batter_handedness, 10), collapse = ", "))
      message("Unique batter_handedness values: ", paste(unique(batter_handedness), collapse = ", "))
      message("NA count: ", sum(is.na(batter_handedness)))
      message("Empty string count: ", sum(batter_handedness == "", na.rm = TRUE))
      # Replace empty strings with "Empty" for summary
      batter_handedness_summary <- ifelse(batter_handedness == "", "Empty", batter_handedness)
      summary_data <- table(batter_handedness_summary, useNA = "always")
      message("batter_handedness summary:\n", paste(capture.output(summary_data), collapse = "\n"))
      # Add player_type summary
      if ("player_type" %in% colnames(stored_players$data)) {
        player_type_summary <- table(stored_players$data$player_type, useNA = "always")
        message("player_type summary:\n", paste(capture.output(player_type_summary), collapse = "\n"))
        # Cross-tabulate player_type and batter_handedness
        cross_table <- table(
          player_type = stored_players$data$player_type,
          batter_handedness = batter_handedness_summary,
          useNA = "always"
        )
        message("player_type vs batter_handedness:\n", paste(capture.output(cross_table), collapse = "\n"))
      }
    } else {
      message("batter_handedness column not found in stored_players$data")
    }
  })
  
  # Define global_pitch_data as a reactive that pulls from the database
  global_pitch_data <- reactive({
    # Invalidate when pitch_update_trigger changes
    pitch_update_trigger()
    
    # Query the database
    data <- dbGetQuery(connect_to_db, "SELECT * FROM global_pitch_data")
    message("global_pitch_data refreshed. Rows: ", nrow(data))
    data
  })
  
  # Only updates when a row is added to events sql database, entire events table
  events <- reactive({
    req(input$create_new_event)
    data <- dbGetQuery(connect_to_db, "SELECT * FROM events")
    message("global_pitch_data refreshed. Rows: ", nrow(data))
    data
  })
  
  # Events database for statistics page - without req() in order to update when options change for stats page
  all_events <- reactive({
    data <- dbGetQuery(connect_to_db, "SELECT * FROM events")
    message("global_pitch_data refreshed. Rows: ", nrow(data))
    data
  })
  
  # Last Event Id, in order to calculate what the next event id should be when setting up game or practice
  latest_event_id <- reactive({
    req(input$type_game || input$type_practice)
    if (length(events()$event_id) == 0) {
      0
    } else {
      max(events()$event_id, na.rm = TRUE)
    }
  })
  
  # Values needs throughout the code that need to update with the global_pitch_data 
  observe({
    req(game_info$event_id > 0)
    # Will update every pitch or undo pitch
    pitch_update_trigger()
    pitches <- global_pitch_data() # Grabs all pitches
    
    pitch_values$current_event_id <- game_info$event_id # Takes the current event so it can't change throughout the game
    
    # game_pitch_number updates every time there is a pitch whether is_pitch_event == TRUE or FALSE
    #   last_pitch_num and last_non_pitch_event are sometimes different
    pitch_values$last_pitch_num <- pitches %>%    # Highest game_pitch_number from the current event
      filter(
        is_pitch_event == TRUE,
        event_id == pitch_values$current_event_id
      ) %>%
      summarise(last = max(game_pitch_number, na.rm = TRUE)) %>%
      pull(last)
    pitch_values$last_non_pitch_num <- pitches %>%    # Highest non pitch game_pitch_number
      filter(event_id == pitch_values$current_event_id) %>%
      summarise(last = max(game_pitch_number, na.rm = TRUE)) %>%
      pull(last)
    
    # Because the game_pitch_number updates every time, only one of these will populate depending on what the last game_pitch_number was...
    # If is_pitch_event == TRUE, this will populate
    last_pitch_data <- pitches %>%
      filter(
        event_id == pitch_values$current_event_id,
        game_pitch_number == pitch_values$last_pitch_num,
        is_pitch_event == TRUE
      )
    last_pitch_reactive(last_pitch_data) #update values
    
    # If is_pitch_event == FALSE, this will populate
    last_record_data <- pitches %>%
      filter(
        event_id == pitch_values$current_event_id,
        game_pitch_number == pitch_values$last_non_pitch_num,
        is_pitch_event == FALSE
      )
    last_record_reactive(last_record_data)#update values
  })
  
  
  ## ---- Initializing Reactive Tables ----
  # Initialize reactiveValues for stored_players
  stored_players <- reactiveValues(
    data = data.frame(
      player_id = numeric(),
      college_name = character(),
      first_name = character(),
      last_name = character(),
      full_name = character(),
      jersey_number = numeric(),
      player_type = character(),
      grad_year = numeric(),
      batter_handedness = character(),
      throwing_handedness = character(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  
  # Reactive Values for Game State (changes pitch to pitch for both game and practice events)
  game_state <- reactiveValues(
    home_lineup = NULL,  # Vector of PlayerIDs (9 hitters)
    away_lineup = NULL,  # Vector of PlayerIDs (9 hitters)
    home_pitcher = NULL, # Single player_id
    away_pitcher = NULL, # Single player_id
    inning = 1,
    inning_specifier = 9,
    half = "Top",
    outs = 0,
    balls = 0,
    strikes = 0,
    pitch_count = 0,
    home_score = 0,
    away_score = 0,
    runners = list(first = NA_real_, second = NA_real_, third = NA_real_),
    pitcher_pitch_count = list(),
    current_batter_idx = 1,
    away_next_batter_idx = 1,
    home_next_batter_idx = 1,
    current_pitcher = NULL,
    current_hitter = NULL,
    available_pitchers = list(),
    available_hitters = list(),
    at_bat_pitch_number = 0,
    hitting_team = NULL,
    pitching_team = NULL
  )
  
  # Practice State (does not change after practice set up, only used for practice)
  practice_state <- reactiveValues(
    have_innings = NULL,
    have_runners = NULL,
    have_score = NULL,
    have_bip_outcome = NULL
  )
  
  # Reactive Values for Runners
  prev_runners <- reactiveValues(
    first = NA_real_,
    second = NA_real_,
    third = NA_real_,
    outcome_first = NA_character_,
    outcome_second = NA_character_,
    outcome_third = NA_character_,
    reason_first = NA_character_,
    reason_second = NA_character_,
    reason_third = NA_character_
  )
  
  # Pitch Values
  pitch_values <- reactiveValues()
  
  # Reactive Values for Home Team Lineup
  home_lineup <- reactiveValues(
    ht_1spot = NULL,
    ht_2spot = NULL,
    ht_3spot = NULL,
    ht_4spot = NULL,
    ht_5spot = NULL,
    ht_6spot = NULL,
    ht_7spot = NULL,
    ht_8spot = NULL,
    ht_9spot = NULL,
  )
  home_pitcher <- reactiveValues(
    ht_Pspot = NULL,
  )
  # Reactive Values for Away Team Lineup
  away_lineup <- reactiveValues(
    at_1spot = NULL,
    at_2spot = NULL,
    at_3spot = NULL,
    at_4spot = NULL,
    at_5spot = NULL,
    at_6spot = NULL,
    at_7spot = NULL,
    at_8spot = NULL,
    at_9spot = NULL,
  )
  away_pitcher <- reactiveValues(
    at_Pspot = NULL,
  )
  
  # reactiveValues object to store game-wide data (does not change pitch to pitch)
  game_info <- reactiveValues(
    date = NULL,
    event_type = NULL,
    event_id = NULL,
    home_team = NULL,
    away_team = NULL
  )
  
  # reactiveValues for balls In Play
  ball_in_play_data <- reactiveValues(
    ball_in_play_outcome = "None",
    ball_flight = "None",
    contact_type = "None",
    spray_chart_x = NA_real_,
    spray_chart_y = NA_real_
  )
  
  #Last pitch and last non pitch events
  last_pitch_reactive <- reactiveVal(NULL)
  last_record_reactive <- reactiveVal(NULL)
  
  # Reactive for selected runner (score board)
  runner_selection <- reactiveValues(base = NULL)  
  
  # Initialize a trigger for manual updates (used in many parts of reactive functioning)
  pitch_update_trigger <- reactiveVal(0)
  
  
  ## ---- Resetting Reactive Tables ----
  reset_the_game_state <- function() {
    # Game State
    game_state$home_lineup <- NULL
    game_state$away_lineup <- NULL
    game_state$home_pitcher <- NULL
    game_state$away_pitcher <- NULL
    game_state$inning <- 1
    game_state$inning_specifier <- 9
    game_state$half <- "Top"
    game_state$outs <- 0
    game_state$balls <- 0
    game_state$strikes <- 0
    game_state$pitch_count <- 0
    game_state$home_score <- 0
    game_state$away_score <- 0
    game_state$runners$first <- NA_real_
    game_state$runners$second <- NA_real_
    game_state$runners$third <- NA_real_
    game_state$pitcher_pitch_count <- list()
    game_state$current_batter_idx <- 1
    game_state$away_next_batter_idx <- 1
    game_state$home_next_batter_idx <- 1
    game_state$current_pitcher <- NA_real_
    game_state$current_hitter <- NA_real_
    game_state$available_pitchers <- list()
    game_state$available_hitters <- list()
    game_state$at_bat_pitch_number <- 0
    game_state$hitting_team <- NULL
    game_state$pitching_team <- NULL
    
    # Reset lineups
    game_state$home_lineup <- rep(NA, 9)  # 9 spots
    game_state$away_lineup <- rep(NA, 9)  # 9 spots
    game_state$home_pitcher <- NA_real_
    game_state$away_pitcher <- NA_real_
    
    # Practice State
    practice_state$have_innings <- NULL
    practice_state$have_score <- NULL
    practice_state$have_runners <- NULL
    practice_state$have_bip_outcome <- NULL
    
    # Prev Runners
    prev_runners$first <- NA_real_
    prev_runners$second <- NA_real_
    prev_runners$third <- NA_real_
    prev_runners$outcome_first <- NA_character_
    prev_runners$outcome_second <- NA_character_
    prev_runners$outcome_third <- NA_character_
    prev_runners$reason_first <- NA_character_
    prev_runners$reason_second <- NA_character_
    prev_runners$reason_third <- NA_character_
    
    # Pitch Values
    pitch_values <- reactiveValues()
    
    # Home Team
    home_lineup$ht_1spot <- NULL
    home_lineup$ht_2spot <- NULL
    home_lineup$ht_3spot <- NULL
    home_lineup$ht_4spot <- NULL
    home_lineup$ht_5spot <- NULL
    home_lineup$ht_6spot <- NULL
    home_lineup$ht_7spot <- NULL
    home_lineup$ht_8spot <- NULL
    home_lineup$ht_9spot <- NULL
    home_pitcher$ht_Pspot <- NULL
    
    # Away Team
    away_lineup$at_1spot <- NULL
    away_lineup$at_2spot <- NULL
    away_lineup$at_3spot <- NULL
    away_lineup$at_4spot <- NULL
    away_lineup$at_5spot <- NULL
    away_lineup$at_6spot <- NULL
    away_lineup$at_7spot <- NULL
    away_lineup$at_8spot <- NULL
    away_lineup$at_9spot <- NULL
    away_pitcher$at_Pspot <- NULL
    
    # Game Info
    game_info$date <- NULL
    game_info$event_type <- NULL
    game_info$event_id <- NULL
    game_info$home_team <- NULL
    game_info$away_team <- NULL
    
    # balls In Play
    ball_in_play_data$ball_in_play_outcome <- "None"
    ball_in_play_data$ball_flight <- "None"
    ball_in_play_data$contact_type <- "None"
    ball_in_play_data$spray_chart_x <- NA_real_
    ball_in_play_data$spray_chart_y <- NA_real_
    
    # Last pitch and last non pitch events
    last_pitch_reactive <- reactiveVal(NULL)
    last_record_reactive <- reactiveVal(NULL)
    
    # Will this work?
    at_bat_pitch_number <- 0
  }
  
  # ---- Start Page Functioning ----
  # Start of new event creation 
  observeEvent(input$create_new_event, { 
    game_state$pitcher_pitch_count <- list()
    app_state$page <- "new_event" 
    reset_the_game_state() #reset all of the reactive tables
  })
  observeEvent(input$stats, {app_state$page <- "stat"}) # Go to Statistics page
  observeEvent(input$go_to_list, { app_state$page <- "list" }) # Go to All Players page
  observeEvent(input$settings, { app_state$page <- "settings" }) # Go to Full Data Tables page
  
  # ---- Sidebar Navigation ----
  # Moving the app to mobile it is easier to use a sidebar to navigate the app, these are the options on the sidebar
  observeEvent(input$menu, {
    page_map <- list(
      home = "new_event",
      stats = "stat",
      players = "list",
      settings = "settings"
    )
    if (!is.null(input$menu) && input$menu %in% names(page_map)) {
      app_state$page <- page_map[[input$menu]]
    }
  })
  
  # ---- Full Data Tables Page ----
  # Reactive list of tables - This is a really important change from standard shiny practices as the observe function won't work to update the dropdowns
  available_tables <- reactive({
    req(app_state$page == "settings")
    
    excluded <- c("graduation_updates")
    setdiff(dbListTables(connect_to_db), excluded)
  })
  
  # Render f7Select dynamically
  output$table_select_ui <- renderUI({
    req(available_tables())
    f7Select(
      inputId = "table_select",
      label = "Select Table to View",
      choices = available_tables(),
      selected = available_tables()[1]
    )
  })
  
  
  # Render selected table
  output$table_display <- DT::renderDT({
    req(input$table_select)
    table_data <- dbReadTable(connect_to_db, input$table_select)
    datatable(table_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Download handler for csv output of selected table
  output$download_selected_table <- downloadHandler(
    filename = function() {
      paste0(input$table_select, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$table_select)
      data_to_download <- dbReadTable(connect_to_db, input$table_select)
      readr::write_csv(data_to_download, file)
    }
  )
  
  # add new college to database options (pop up)
  observeEvent(input$add_team_database, {
    showModal(modalDialog(
      title = "Add College Information",
      textInput("college_name_input", "College Name"),
      textInput("college_abbreviation_input", "College Abbreviation"),
      textInput("conference_input", "Conference"),
      textInput("region_input", "Region"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_college", "Confirm")
      ),
      easyClose = FALSE
    ))
  })
  
  # Confirm button for addition to teams table in SQL
  observeEvent(input$confirm_add_college, {
    req(input$college_name_input, input$college_abbreviation_input)
    
    tryCatch({
      dbExecute(connect_to_db, "
      INSERT INTO teams (college_name, college_abbreviation, conference, region)
      VALUES ($1, $2, $3, $4)",
                params = list(
                  input$college_name_input,
                  input$college_abbreviation_input,
                  input$conference_input,
                  input$region_input
                )
      )
      
      removeModal()
      showNotification("College info added successfully!", type = "message")
    })
  })
  
  # ---- Stats Page ----
  ## ---- All Pitchers Functioning ----
  # For the pitcher_team dropdowns, only show teams that have pitchers with data in global_pitch_data
  observe({
    req(teams$data, global_pitch_data(), stored_players$data, app_state$page == "stat")  # Only update when on Stats page
    # Pitchers with data
    pitchers_with_data <- global_pitch_data() %>%
      pull(pitcher_id) %>%
      unique()
    # Unique teams of these pitches
    pitcher_teams <- stored_players$data %>%
      filter(player_id %in% pitchers_with_data) %>%
      pull(college_name) %>%
      unique()
    valid_pitcher_teams <- teams$data$college_name[teams$data$college_name %in% pitcher_teams]
    # Update the team options
    updateSelectInput(session, "pitcher_team",
                      choices = c("Choose Team", valid_pitcher_teams),
                      selected = "Choose Team")
  })
  
  observe({
    req(
      input$pitcher_team,
      input$pitcher_status_of_player,
      input$pitcher_type_of_event,
      global_pitch_data(),
      stored_players$data,
      all_events(),
      app_state$page == "stat"
    )
    
    if (input$pitcher_team == "Choose Team") return()
    
    # Get pitcher IDs for selected team and status
    eligible_pitchers <- stored_players$data %>%
      filter(
        college_name == input$pitcher_team,
        status == input$pitcher_status_of_player
      ) %>%
      pull(player_id)
    
    # Get pitch data for those pitchers
    pitcher_events <- global_pitch_data() %>%
      filter(pitcher_id %in% eligible_pitchers) %>%
      left_join(all_events(), by = "event_id")
    
    # Filter by event type
    if (input$pitcher_type_of_event != "Both") {
      pitcher_events <- pitcher_events %>%
        filter(event_type == input$pitcher_type_of_event)
    }
    
    # Get sorted unique dates
    available_dates <- pitcher_events %>%
      pull(date) %>%
      unique() %>%
      sort(decreasing = TRUE)
    
    # Add "All Dates" at the top
    date_choices <- c("All Dates", available_dates)
    
    # Update selectizeInput (single select)
    updateSelectizeInput(
      session, "pitcher_date",
      choices = date_choices,
      selected = "All Dates",
      server = TRUE
    )
  })
  
  # Reactive values of pitchers for stats check box selection (Changes when any selectInputs or databases change)
  valid_pitchers <- reactive({
    req(input$pitcher_team, input$pitcher_type_of_event, input$pitcher_status_of_player, global_pitch_data(), stored_players$data)
    
    if (input$pitcher_team == "Choose Team" || nrow(global_pitch_data()) == 0) {
      return(character(0))
    }
    
    # Filter pitcher IDs based on event type
    if (input$pitcher_type_of_event == "Both") {
      active_pitcher_ids <- global_pitch_data() %>%
        pull(pitcher_id) %>%
        unique()
    } else {
      joined_data <- global_pitch_data() %>%
        left_join(all_events(), by = "event_id")
      active_pitcher_ids <- joined_data %>%
        filter(event_type == input$pitcher_type_of_event) %>%
        pull(pitcher_id) %>%
        unique()
    }
    
    # Get player names from stored_players
    result <- stored_players$data %>%
      filter(
        college_name == input$pitcher_team,
        status == input$pitcher_status_of_player,
        player_id %in% active_pitcher_ids
      ) %>%
      arrange(full_name) %>%
      pull(full_name)
    
    result
  })
  
  # Update the selection
  observeEvent(valid_pitchers(), {
    pitcher_choices <- valid_pitchers()
    
    label_text <- if (length(pitcher_choices) == 0) {
      "Select Pitchers: (No pitchers available for this team)"
    } else {
      "Select Pitchers:"
    }
    
    updateCheckboxGroupInput(session, "pitcher_players",
                             choices = pitcher_choices,
                             selected = NULL,
                             label = label_text)
  })
  
  # This is where all of the pitcher stats are calculated
  calculate_pitcher_stats <- function(pitcher_id_calculator) {
    # All of the data that has the selected pitcher involved
    all_pitcher_data <- global_pitch_data() %>%
      filter(pitcher_id == pitcher_id_calculator)
    
    # Getting stored_players reactive data
    player_info <- tryCatch({
      stored_players$data
    }, error = function(e) {
      message("Failed to access stored_players$data: ", e$message)
      data.frame(player_id = character(), batter_handedness = character(), player_type = character())
    })
    
    # Getting events reactive data
    player_events <- tryCatch({
      isolate(all_events()) #need to use all_events() because it does not have req() in the observe
    }, error = function(e) {
      message("Failed to access events(): ", e$message)
      data.frame(event_id = numeric(), date = character(), event_type = character(),
                 home_team = character(), away_team = character())
    })
    
    # Getting all the data onto one tables
    pitch_data <- all_pitcher_data %>%
      left_join(player_info, by = c("hitter_id" = "player_id")) %>% # getting hitter handedness information from join
      left_join(player_events, by = "event_id") %>% # getting the event type information from join
      filter(
        if (input$pitcher_type_of_event != "Both") event_type == input$pitcher_type_of_event else TRUE # filter by event type if needed
      ) %>%
      arrange(event_id, game_pitch_number) %>%
      mutate(
        pa_id = cumsum(at_bat_pitch_number == 1) # Plate Apparenece ID, this will give every plate appearance an ascending numbers
      )
    message("Rows with non-NA batter_handedness: ", sum(!is.na(pitch_data$batter_handedness)))
    print(unique(pitch_data$event_type))
    
    unmatched <- pitch_data$hitter_id[!pitch_data$hitter_id %in% player_info$player_id]
    if (length(unmatched) > 0) {
      message("Unmatched HitterIDs: ", paste(unique(unmatched), collapse = ", "))
    }
    
    # Filter out any events that were not pitches (between pitches)
    just_pitch_events <- pitch_data %>% filter(is_pitch_event == TRUE)
    
    # Every row is the game situation at the end of that plate appearance. The game state, what happened, and the runners too
    # Defines every plate appearances (PA's) this pitcher has pitched
    pa_all <- pitch_data %>% 
      group_by(event_id, pa_id) %>%
      summarise(
        pitcher_id = pitcher_id_calculator,
        hitter_id = last(hitter_id),
        hitter_side = last(batter_handedness),
        pitchers_pitch_count = last(pitchers_pitch_count),
        pitches_in_at_bat = max(at_bat_pitch_number),
        pitch_outcome = last(pitch_outcome),
        outcome = last(ball_in_play_outcome),
        ball_flight = last(ball_flight),
        contact_type = last(contact_type),
        balls = last(balls), 
        strikes = last(strikes),
        batter_base = last(batter_outcome),
        first_base = last(coalesce(first_base_outcome, "None")),
        second_base = last(coalesce(second_base_outcome, "None")),
        third_base = last(coalesce(third_base_outcome, "None")),
        first_pitch_outcome = first(pitch_outcome) # First() because is only used for first pitch strike(fps)% and fps% obp
      ) %>%
      ungroup() %>%
      arrange(event_id, pitchers_pitch_count)
    
    # Filter out plate appearances to only show the at bats (AB's)
    at_bats_all <- pa_all %>%
      filter(!outcome %in% c("Walk", "HBP", "Sacrifice Bunt", "Sacrifice Fly", "Cage Ball", "None", "Other (Interference)"))
    
    # Total Number of PA's and AB's
    total_pa_against_all <- if (nrow(pa_all) == 0) 1 else {nrow(pa_all)}
    total_abs_against_all <- if (nrow(at_bats_all) == 0) 1 else {nrow(at_bats_all)}
    message("All-time at-bats calculated: ", total_abs_against_all, "Plate Appearances All Time Calculated: ", total_pa_against_all)
    
    # Total number of outs recorded while this player was pitching
    outs_recorded_all <- 
      sum(pitch_data$ball_in_play_outcome %in% c("Out", "SO Looking", "SO Swinging"), na.rm = TRUE) +
      sum(pitch_data$first_base_outcome == "Out", na.rm = TRUE) +
      sum(pitch_data$second_base_outcome == "Out", na.rm = TRUE) +
      sum(pitch_data$third_base_outcome == "Out", na.rm = TRUE) +
      sum(pitch_data$ball_in_play_outcome == "Double Play" & ((pitch_data$first_base_outcome == "Out") + (pitch_data$second_base_outcome == "Out") + (pitch_data$third_base_outcome == "Out") < 2), na.rm = TRUE) +
      sum(pitch_data$ball_in_play_outcome == "Triple Play" & ((pitch_data$first_base_outcome == "Out") + (pitch_data$second_base_outcome == "Out") + (pitch_data$third_base_outcome == "Out") < 3), na.rm = TRUE)
    
    innings_all <- (outs_recorded_all %/% 3) + ((outs_recorded_all %% 3) / 10) #used to show number of outs (will mess up any calculations)
    inning_calculator <- if(outs_recorded_all == 0) .001 else {outs_recorded_all / 3} #used when something is divided by number of outs (use for calculations)
    message("All-time outs recorded: ", outs_recorded_all, "; Innings: ", innings_all)
    
    # Basic stats for all time (most are self explanatory)
    # Notes for calculations (Cage Balls should not be involoved in any calculations, I did the more of filtering them out, Cage Balls do not have a ball_in_play_outcome or
    #   location on the spray chart, that is on purpose because sometimes we have practice indoors and cannot calculate these things)
    hits_allowed_all <- sum(pa_all$outcome %in% c("Single", "Double", "Triple", "Homerun"), na.rm = TRUE)
    walks_all <- sum(pa_all$outcome == "Walk", na.rm = TRUE)
    hbps_all <- sum(pa_all$outcome == "HBP", na.rm = TRUE)
    strikeouts_all <- sum(pa_all$outcome %in% c("SO Looking", "SO Swinging"), na.rm = TRUE)
    #Cage Ball is not considered a PA because there is no way to get on base (Only helps pitchers and hurts hitters if you leave it)
    total_pa_minus_cage <- if (nrow(pa_all) == 0) 1 else {pa_all %>% filter(batter_base != "Cage Ball") %>% nrow()} 
    on_base_against <- sum(!pa_all$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / total_pa_minus_cage
    slg_against <- (sum(pitch_data$ball_in_play_outcome == "Single") + 
                      (2 * sum(pitch_data$ball_in_play_outcome == "Double")) +
                      (3 * sum(pitch_data$ball_in_play_outcome == "Triple")) +
                      (4 * sum(pitch_data$ball_in_play_outcome == "Homerun"))) / total_abs_against_all
    
    
    # All of this is used for QABs against
    # Total runs given up while pitching
    runs_allowed_all <- sum(pitch_data$batter_outcome == "Scored", na.rm = TRUE) +
      sum(pitch_data$first_base_outcome == "Scored", na.rm = TRUE) +
      sum(pitch_data$second_base_outcome == "Scored", na.rm = TRUE) +
      sum(pitch_data$third_base_outcome == "Scored", na.rm = TRUE)
    # Runs Batted In
    rbi_allowed <- (sum(pa_all$batter_base == "Scored", na.rm = TRUE) +
                      sum(pa_all$first_base == "Scored", na.rm = TRUE) +
                      sum(pa_all$second_base == "Scored", na.rm = TRUE) +
                      sum(pa_all$third_base == "Scored", na.rm = TRUE))
    # Sacrifice hits are not considered at-bats
    num_of_sacs_allowed <- sum(pa_all$outcome %in% c("Sacrifice Bunt", "Sacrifice Fly"), na.rm = TRUE)
    contact_total_against <- sum(pa_all$contact_type == "Weak", na.rm = TRUE) +
      (sum(pa_all$contact_type == "Average", na.rm = TRUE) * 2) +
      (sum(pa_all$contact_type == "Hard", na.rm = TRUE) * 3)
    so_swinging_against <- sum(pa_all$outcome == "SO Swinging", na.rm = TRUE)
    so_looking_against <- sum(pa_all$outcome == "SO Looking", na.rm = TRUE)
    six_plus_pitches <- sum(pa_all$pitches_in_at_bat >= 6 & !pa_all$outcome %in% c("None", "Cage Ball", "SO Looking", "SO Swinging"), na.rm = TRUE)
    message("All-time hits allowed: ", hits_allowed_all, "; Walks: ", walks_all, "; HBPs: ", hbps_all, "; Strikeouts: ", strikeouts_all, "; Runs allowed: ", runs_allowed_all)
    
    qab_against <- round((
      (hits_allowed_all + walks_all + rbi_allowed + hbps_all + num_of_sacs_allowed + six_plus_pitches) * 3
      + contact_total_against
      - so_swinging_against * 3
      - so_looking_against * 2
    ) / total_pa_against_all, 2)
    
    
    # Last 50 Throws of each pitch (Used to find speed of each pitches)
    last_50 <- list(
      fb = list(
        pitch_data %>%
          filter(pitch_type == "FB") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      cb = list(
        pitch_data %>%
          filter(pitch_type == "CB") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      sl = list(
        pitch_data %>%
          filter(pitch_type == "SL") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      ch = list(
        pitch_data %>%
          filter(pitch_type == "CH") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      sp = list(
        pitch_data %>%
          filter(pitch_type == "SP") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      ct = list(
        pitch_data %>%
          filter(pitch_type == "CT") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      ),
      oth = list(
        pitch_data %>%
          filter(pitch_type == "OTH") %>%
          arrange(desc(event_id), desc(pitchers_pitch_count)) %>%
          slice_head(n = 50)
      )
    )
    
    # Number and percentage each pitch is used
    pitch_counts <- pitch_data %>%
      filter(!is.na(pitch_type)) %>%
      count(pitch_type, name = "times") %>%
      arrange(desc(times)) %>%
      mutate(
        pct_label = paste0(round(times / sum(times) * 100, 1), "%")
      )
    
    # Last 15 plate appearances
    pa_last_15 <- pa_all %>%
      slice_tail(n = 15)
    
    # Of last 15 pa, these are the at bats (it is okay if these are less than 15)
    at_bats_last_15 <- pa_last_15 %>%
      filter(!outcome %in% c("Walk", "HBP", "Sacrifice Bunt", "Sacrifice Fly", "Cage Ball", "None", "Other (Interference)"))
    
    # Total number of PA's and AB's
    total_pa_against_last_15 <- if(nrow(pa_last_15) == 0) 1 else {nrow(pa_last_15)}
    total_abs_against_last_15 <- if(nrow(at_bats_last_15) == 0) 1 else {nrow(at_bats_last_15)}
    message("Last 15 at-bats calculated: ", total_abs_against_last_15, "; Last 15 plate appearances calculated: ", total_pa_against_last_15)
    
    # All of the pitches from the Last 15 plate appearances
    last_15 <- pitch_data %>%
      semi_join(pa_last_15, by = c("event_id", "pa_id"))
    
    # Number of outs recorded from the last 15 PA
    last_15_outs <- sum(last_15$ball_in_play_outcome %in% c("Out","SO Looking","SO Swinging"), na.rm = TRUE) +
      sum(last_15$first_base_outcome == "Out", na.rm = TRUE) +
      sum(last_15$second_base_outcome == "Out", na.rm = TRUE) +
      sum(last_15$third_base_outcome == "Out", na.rm = TRUE)
    innings_last_15 <- if(last_15_outs == 0) .001 else {last_15_outs / 3}
    message("Last 15 outs recorded: ", last_15_outs, "; Innings: ", innings_last_15)
    
    
    # Same as above but calculated for the last 15 PA instead
    hits_allowed_last_15 <- sum(pa_last_15$outcome %in% c("Single", "Double", "Triple", "Homerun"), na.rm = TRUE)
    walks_last_15 <- sum(pa_last_15$outcome == "Walk", na.rm = TRUE)
    hbps_last_15 <- sum(pa_last_15$outcome == "HBP", na.rm = TRUE)
    strikeouts_last_15 <- sum(pa_last_15$outcome %in% c("SO Looking", "SO Swinging"), na.rm = TRUE)
    total_pa_minus_cage_last_15 <- if (nrow(pa_last_15) == 0) 1 else {pa_all %>% filter(batter_base != "Cage Ball") %>% nrow()}
    on_base_against_last_15 <- sum(!pa_last_15$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / total_pa_minus_cage_last_15
    runs_allowed_last_15 <- sum(last_15$batter_outcome == "Scored", na.rm = TRUE) +
      sum(last_15$first_base_outcome == "Scored", na.rm = TRUE) +
      sum(last_15$second_base_outcome == "Scored", na.rm = TRUE) +
      sum(last_15$third_base_outcome == "Scored", na.rm = TRUE)
    rbi_allowed_last_15 <- (sum(pa_last_15$batter_base == "Scored", na.rm = TRUE) +
                              sum(pa_last_15$first_base == "Scored", na.rm = TRUE) +
                              sum(pa_last_15$second_base == "Scored", na.rm = TRUE) +
                              sum(pa_last_15$third_base == "Scored", na.rm = TRUE))
    num_of_sacs_allowed_last_15 <- sum(pa_last_15$outcome %in% c("Sacrifice Bunt", "Sacrifice Fly"), na.rm = TRUE)
    contact_total_against_last_15 <- sum(pa_last_15$contact_type == "Weak", na.rm = TRUE) +
      (sum(pa_last_15$contact_type == "Average", na.rm = TRUE) * 2) +
      (sum(pa_last_15$contact_type == "Hard", na.rm = TRUE) * 3)
    so_swinging_against_last_15 <- sum(pa_last_15$outcome == "SO Swinging", na.rm = TRUE)
    so_looking_against_last_15 <- sum(pa_last_15$outcome == "SO Looking", na.rm = TRUE)
    six_plus_pitches_last_15 <- sum(pa_last_15$pitches_in_at_bat >= 6 & !pa_last_15$outcome %in% c("None", "Cage Ball", "SO Looking", "SO Swinging"), na.rm = TRUE)
    
    qab_against_last_15 <- round((
      (hits_allowed_last_15 + walks_last_15 + rbi_allowed_last_15 + hbps_last_15 + num_of_sacs_allowed_last_15 + six_plus_pitches_last_15) * 3
      + contact_total_against_last_15
      - so_swinging_against_last_15 * 3
      - so_looking_against_last_15 * 2
    ) / total_pa_against_last_15, 2)
    message("Last 15 hits allowed: ", hits_allowed_last_15, "; Walks: ", walks_last_15, "; Hit By Pitches: ", hbps_last_15, "; Strikeouts: ", strikeouts_last_15, "; Runs allowed: ", runs_allowed_last_15)
    
    # Total Number of Pitches all time (_all) and last 15 PA (_last_15)
    total_pitches_all <- if(nrow(just_pitch_events) == 0) 1 else {nrow(just_pitch_events)}
    total_pitches_last_15 <- if(nrow(last_15) == 0) 1 else {nrow(last_15)}
    
    # Strike percentage for last 15
    strike_pct_last_15 <-  round(sum(last_15$pitch_outcome %in% c("Called Strike", "Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / total_pitches_last_15 * 100, 1)
    
    # Some more calculations about all time batters faced
    # Of the Batters faced that had a first pitch strike, what percentage of them got on base
    first_k_obp <- if (nrow(pa_all) > 0) {
      round(
        sum(
          pa_all$first_pitch_outcome %in% c("Swinging Strike", "Ball In Play", "Foul Ball") &
            !pa_all$batter_base %in% c("None", "Cage Ball"),
          na.rm = TRUE
        ) / nrow(pa_all), 3
      )
    } else {
      0
    }
    whip_all <- round((hits_allowed_all + walks_all) / inning_calculator, 2)
    ba_against_all <- round(hits_allowed_all / total_abs_against_all, 3)
    obp_against_all <- round(on_base_against, 3)
    
    # Some more calculations about last 15 batters faced
    first_k_obp_last_15 <- if (nrow(pa_last_15) > 0) {
      round(
        sum(
          pa_last_15$first_pitch_outcome %in% c("Swinging Strike", "Ball In Play", "Foul Ball") &
            !pa_last_15$batter_base %in% c("None", "Cage Ball"),
          na.rm = TRUE
        ) / nrow(pa_last_15), 3
      )
    } else {
      0
    }
    whip_last_15 <- round((hits_allowed_last_15 + walks_last_15) / innings_last_15, 2)
    ba_against_last_15 <- round(hits_allowed_last_15 / total_abs_against_last_15, 3)
    obp_against_last_15 <- round(on_base_against_last_15, 3)
    
    # figuring out what pitches the pitcher has
    has_pitch_type <- "pitch_type" %in% colnames(pitch_data)
    pitch_types <- unique(pitch_data$pitch_type[!is.na(pitch_data$pitch_type)])
    message("Starting pitch selection; Pitch types: ", paste(pitch_types, collapse = ", "))
    
    # This splits up the data into counts (the bottom table on our outputs) and
    #   this puts the percentage of each pitch thrown
    pitch_selection <- list(
      early = list(
        LHH = lapply(c("0-0","1-0","0-1","1-1"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Left")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        }),
        RHH = lapply(c("0-0","1-0","0-1","1-1"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Right")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        })
      ),
      behind = list(
        LHH = lapply(c("2-0","2-1","3-0","3-1"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Left")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        }),
        RHH = lapply(c("2-0","2-1","3-0","3-1"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Right")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        })
      ),
      ahead = list(
        LHH = lapply(c("0-2","1-2"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Left")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        }),
        RHH = lapply(c("0-2","1-2"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Right")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        })
      ),
      other = list(
        LHH = lapply(c("2-2","3-2"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Left")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        }),
        RHH = lapply(c("2-2","3-2"), function(count) {
          b <- as.numeric(strsplit(count,"-")[[1]][1])
          s <- as.numeric(strsplit(count,"-")[[1]][2])
          p <- pitch_data %>% filter(balls==b, strikes==s, batter_handedness=="Right")
          tbl <- table(p$pitch_type)
          pct <- round(tbl / sum(tbl) * 100)
          paste0(names(pct)," ",pct,"%", collapse=", ")
        })
      )
    )
    
    names(pitch_selection$early$LHH) <- c("0-0","1-0","0-1","1-1")
    names(pitch_selection$early$RHH) <- c("0-0","1-0","0-1","1-1")
    names(pitch_selection$behind$LHH) <- c("2-0","2-1","3-0","3-1")
    names(pitch_selection$behind$RHH) <- c("2-0","2-1","3-0","3-1")
    names(pitch_selection$ahead$LHH) <- c("0-2","1-2")
    names(pitch_selection$ahead$RHH) <- c("0-2","1-2")
    names(pitch_selection$other$LHH) <- c("2-2","3-2")
    names(pitch_selection$other$RHH) <- c("2-2","3-2")
    
    message("Pitch selection completed")
    
    # Honestly not sure what this is used for, looks like these are just for messages and is not put anywhere else
    has_batter_handedness <- "batter_handedness" %in% colnames(pitch_data) && 
      any(pitch_data$batter_handedness %in% c("Left", "Right", "Switch"), na.rm = TRUE)
    message("pitch_type present: ", has_pitch_type, "; batter_handedness present: ", has_batter_handedness)
    message("Pitch types: ", paste(pitch_types, collapse = ", "))
    
    # This function is called after the data is split up by hitter handedness and pitch type
    compute_metrics <- function(data, total_pitches) {
      pitches <- if(nrow(data) == 0) 1 else {nrow(data)}
      strike_pct <- round(sum(data$pitch_outcome %in% c("Called Strike", "Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / pitches * 100, 1)
      called_strike_pct <- round(sum(data$pitch_outcome == "Called Strike", na.rm = TRUE) / pitches * 100, 1)
      swing_miss_pct <- round(sum(data$pitch_outcome == "Swinging Strike", na.rm = TRUE) / pitches * 100, 1)
      strikeouts <- sum(data$ball_in_play_outcome %in% c("SO Looking", "SO Swinging"), na.rm = TRUE)
      at_bs <- data %>%
        group_by(event_id, pa_id) %>% 
        slice_max(at_bat_pitch_number, n = 1, with_ties = FALSE) %>% 
        transmute(
          outcome = ball_in_play_outcome
        ) %>% 
        ungroup() %>%
        filter(!outcome %in% c("None", "Cage Ball")) %>%
        nrow()
      abs <- if (length(at_bs) == 0 || at_bs == 0) 1 else {at_bs}
      strikeout_pct <- round(strikeouts / abs * 100, 1)
      # pressure is a location of the strike zone, it is the area that is just inside the strikezone and just outside of it
      is_pressure <- (data$pitch_location_x >= -11.3 & data$pitch_location_x <= -5.7 |
                        data$pitch_location_x >= 5.7 & data$pitch_location_x <= 11.3) &
        (data$pitch_location_y >= 14 & data$pitch_location_y <= 22 |
           data$pitch_location_y >= 38 & data$pitch_location_y <= 46)
      pressure_pitch <- ifelse(is.na(is_pressure), TRUE, is_pressure)
      pressure_swing_pct <- if (sum(pressure_pitch, na.rm = TRUE) == 0) {0} else {
        round(sum(data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch, na.rm = TRUE) /
                sum(pressure_pitch, na.rm = TRUE) * 100, 1)}
      # ball in play (bip)
      bip <- data %>% filter(pitch_outcome == "Ball In Play")
      hard_hit_pct <- round(sum(bip$contact_type == "Hard", na.rm = TRUE) / if(nrow(bip) == 0) 1 else {nrow(bip)} * 100, 1)
      weak_hit_pct <- round(sum(bip$contact_type == "Weak", na.rm = TRUE) / if(nrow(bip) == 0) 1 else {nrow(bip)} * 100, 1)
      ground_ball_pct <- round(sum(bip$ball_flight == "Ground Ball", na.rm = TRUE) / if(nrow(bip) == 0) 1 else {nrow(bip)} * 100, 1)
      
      list(
        strike_pct = strike_pct,
        called_strike_pct = called_strike_pct,
        swing_miss_pct = swing_miss_pct,
        strikeout_pct = strikeout_pct,
        pressure_swing_pct = pressure_swing_pct,
        hard_hit_pct = hard_hit_pct,
        weak_hit_pct = weak_hit_pct,
        ground_ball_pct = ground_ball_pct
      )
    }
    
    # this calculates the usage percentage of each pitch
    pitch_types_stats <- lapply(pitch_types, function(pt) {
      total_mean <- mean(pitch_data$pitch_type == pt, na.rm = TRUE)
      message("Pitch type ", pt, ": Total mean = ", total_mean)
      list(Total = round(total_mean * 100, 1))
    })
    names(pitch_types_stats) <- pitch_types
    
    # This function calls compute_metrics() so it will calculate all of the metrics for each pitch type
    pitch_types_metrics <- lapply(pitch_types, function(pt) {
      # Gets just the pitch type data
      pt_data <- pitch_data %>% filter(pitch_type == pt, is_pitch_event == TRUE)
      total_pt_pitches <- nrow(pt_data)
      #splits up by batter handedness
      lhh_data <- pt_data %>% filter(batter_handedness == "Left")
      rhh_data <- pt_data %>% filter(batter_handedness == "Right")
      message("Pitch type ", pt, ": LHH rows = ", nrow(lhh_data), ", RHH rows = ", nrow(rhh_data))
      list(
        # calls compute_metrics for each hitter types 
        LHH = compute_metrics(lhh_data, nrow(lhh_data)),
        RHH = compute_metrics(rhh_data, nrow(rhh_data))
      )
    })
    names(pitch_types_metrics) <- pitch_types
    
    # this lists out all of the calculations in a clean way
    #   everything is called by stats$_____, it is easier to understand it that way 
    stats <- list(
      innings = innings_all,
      last_50 = last_50,
      
      pitch_counts = pitch_counts,
      pitch_types = pitch_types_stats,
      pitch_types_metrics = pitch_types_metrics,
      
      pitches_per_inning = round(total_pitches_all / inning_calculator, 1),
      pitches_per_inning_last_15 = round(nrow(last_15) / innings_last_15, 1),
      first_pitch_strike_pct = round(sum(pitch_data$pitch_outcome %in% c("Called Strike", "Swinging Strike", "Ball In Play", "Foul Ball", "Cage Ball") & pitch_data$at_bat_pitch_number == 1, na.rm = TRUE) / total_pa_against_all * 100, 1),
      first_pitch_strike_pct_last_15 = round(sum(last_15$pitch_outcome %in% c("Called Strike", "Swinging Strike", "Ball In Play", "Foul Ball", "Cage Ball") & last_15$at_bat_pitch_number == 1, na.rm = TRUE) / total_pa_against_last_15 * 100, 1),
      first_pitch_k_obp = first_k_obp,
      first_pitch_k_obp_last_15 = first_k_obp_last_15,
      whip = whip_all,
      whip_last_15 = whip_last_15,
      runs_per_9 = round(runs_allowed_all * 9 / inning_calculator, 1),
      runs_per_9_last_15 = round(runs_allowed_last_15 * 9 / innings_last_15, 1),
      hits_per_9 = round(hits_allowed_all * 9 / inning_calculator, 1),
      hits_per_9_last_15 = round(hits_allowed_last_15 * 9 / innings_last_15, 1),
      walks_per_9 = round(walks_all * 9 / inning_calculator, 1),
      walks_per_9_last_15 = round(walks_last_15 * 9 / innings_last_15, 1),
      strikeouts_per_9 = round(strikeouts_all * 9 / inning_calculator, 1),
      strikeouts_per_9_last_15 = round(strikeouts_last_15 * 9 / innings_last_15, 1),
      free_br_per_9 = round((hbps_all + walks_all) * 9 / inning_calculator, 1),
      free_br_per_9_last_15 = round((hbps_last_15 + walks_last_15) * 9 / innings_last_15, 1),
      ba_against = ba_against_all,
      ba_against_last_15 = ba_against_last_15,
      obp_against = obp_against_all,
      obp_against_last_15 = obp_against_last_15,
      qabs_against = qab_against,
      qabs_against_last_15 = qab_against_last_15,
      
      pitch_selection = pitch_selection,
      summary = list(last_15 = strike_pct_last_15)
    )
    message("Calculated stats for pitcher ID: ", pitcher_id_calculator)
    return(stats)
  }
  
  # Pitcher Report UI - This shows up inside the app on the stats page, strictly will call the data into neat output
  pitcherReportUI <- function(player_data, stats, pitcher_id) {
    # Determine top 4 pitch types by Total usage
    pitch_usage <- sapply(stats$pitch_types, function(pt) as.numeric(pt$Total))
    pitch_names <- names(stats$pitch_types)
    ordered_pitches <- pitch_names[order(pitch_usage, decreasing = TRUE)]
    top_pitches <- head(ordered_pitches, 4)
    
    tagList(
      fluidRow(
        column(5,
               h4(style = "text-align: center;", "LHH"),
               div(
                 style = "width: 400px; margin: 0 auto;",
                 plotOutput(paste0("pitcher_spray_chart_ui_LHH", pitcher_id), height = "350px", width = "350px")
               )
        ),
        column(2,
               h3(player_data$full_name),
               p(paste("#", player_data$jersey_number)),
               p(ifelse(player_data$throwing_handedness == "Left", "LHP", "RHP")),
               p(paste("Innings Recorded: ", stats$innings)),
               div(
                 # If the pitch type is present, it will then show the data that is related to it
                 tags$table(
                   if (nrow(stats$last_50$fb[[1]]) > 0) {
                     tags$tr(
                       tags$th("FB: "),
                       tags$td(min(stats$last_50$fb[[1]]$pitch_velo), " - ", max(stats$last_50$fb[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$cb[[1]]) > 0) {
                     tags$tr(
                       tags$th("CB: "),
                       tags$td(min(stats$last_50$cb[[1]]$pitch_velo), " - ", max(stats$last_50$cb[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$sl[[1]]) > 0) {
                     tags$tr(
                       tags$th("SL: "),
                       tags$td(min(stats$last_50$sl[[1]]$pitch_velo), " - ", max(stats$last_50$sl[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$ch[[1]]) > 0) {
                     tags$tr(
                       tags$th("CH: "),
                       tags$td(min(stats$last_50$ch[[1]]$pitch_velo), " - ", max(stats$last_50$ch[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$sp[[1]]) > 0) {
                     tags$tr(
                       tags$th("SP: "),
                       tags$td(min(stats$last_50$sp[[1]]$pitch_velo), " - ", max(stats$last_50$sp[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$ct[[1]]) > 0) {
                     tags$tr(
                       tags$th("CT: "),
                       tags$td(min(stats$last_50$ct[[1]]$pitch_velo), " - ", max(stats$last_50$ct[[1]]$pitch_velo))
                     )
                   },
                   if (nrow(stats$last_50$oth[[1]]) > 0) {
                     tags$tr(
                       tags$th("OTH: "),
                       tags$td(min(stats$last_50$oth[[1]]$pitch_velo), " - ", max(stats$last_50$oth[[1]]$pitch_velo))
                     )
                   }
                 )
               )
        ),
        column(5,
               h4(style = "text-align: center;", "RHH"),
               div(
                 style = "width: 400px; margin: 0 auto;",
                 plotOutput(paste0("pitcher_spray_chart_ui_RHH", pitcher_id), height = "350px", width = "350px")
               )
        )
      ),
      fluidRow(
        column(12,
               h4("Pitch Type Metrics"),
               div(
                 style = "display: flex; align-items: flex-start;",
                 # Metric labels
                 div(
                   style = "width: 150px; margin-right: 10px;",
                   tags$div(
                     style = "padding-top: 48px;",
                     lapply(c("Usage:", "", "Strike %", "Called Strike %", "Swing & Miss %", "% of Total Strikeouts",
                              "Pressure Pitch Swing %", "Hard Hit %", "Weak Hit %", "Ground Ball %"),
                            function(metric) {
                              tags$p(
                                style = "margin: 0; padding: 5px 0; padding-bottom: 32px; text-align: right; line-height: 26px; height: 26px; font-size: 14px;",
                                metric
                              )
                            })
                   )
                 ),
                 # Tables for each pitch type
                 div(
                   style = "flex-grow: 1;",
                   fluidRow(
                     lapply(top_pitches, function(pt) {
                       metrics <- stats$pitch_types_metrics[[pt]]
                       column(3,
                              tags$table(
                                style = "width: 100%; border-collapse: collapse;",
                                tags$tr(
                                  tags$th(colspan = 2, style = "border-top:1px solid black; border-left:1px solid black; border-right:1px solid black;
                                          border-bottom:none; padding: 5px; text-align: center; line-height: 35px; font-size: 28px", pt)
                                ),
                                tags$tr(
                                  tags$th(colspan = 2, style = "border-top: none; border-left:1px solid black; border-right:1px solid black;
                                          border-bottom:1px solid black; padding: 5px; text-align: center; line-height: 26px;",
                                          stats$pitch_counts %>% filter(pitch_type == pt) %>% pull(pct_label))
                                ),
                                tags$tr(
                                  tags$th(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", "LHH"),
                                  tags$th(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", "RHH")
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$strike_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$strike_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$called_strike_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$called_strike_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$swing_miss_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$swing_miss_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$strikeout_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$strikeout_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$pressure_swing_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$pressure_swing_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26ミュージ2px;", paste(metrics$LHH$hard_hit_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$hard_hit_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$weak_hit_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$weak_hit_pct, "%"))
                                ),
                                tags$tr(
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$LHH$ground_ball_pct, "%")),
                                  tags$td(style = "border: 1px solid black; padding: 5px; text-align: center; line-height: 26px;", paste(metrics$RHH$ground_ball_pct, "%"))
                                )
                              )
                       )
                     })
                   )
                 )
               )
        )
      ),
      h4("Pitching Efficiency"), # Table for all time and last 15 batters data
      tags$table(
        style = "width: 100%; border-collapse: collapse; text-align: center;",
        tags$tr(
          tags$th(style = "border: 1px solid black; padding: 5px; width:4%; text-align: center;", "Batters"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "Pitches Per Inn"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "First Pitch K%"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "First Pitch K OBP%"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "WHIP"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "Runs Per 9"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "Hits Per 9"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "Walks Per 9"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "SO Per 9"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "Free 90s Per 9"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "BA Against"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "OBP Against"),
          tags$th(style = "border: 1px solid black; padding: 5px; width:8%; text-align: center;", "QABs Against")
        ),
        tags$tr(
          tags$td(style = "border: 1px solid black; padding: 5px;", "Last 15"),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$pitches_per_inning_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", paste(stats$first_pitch_strike_pct_last_15, "%")),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$first_pitch_k_obp_last_15, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$whip_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$runs_per_9_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$hits_per_9_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$walks_per_9_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$strikeouts_per_9_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$free_br_per_9_last_15),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$ba_against_last_15, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$obp_against_last_15, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$qabs_against_last_15)
        ),
        tags$tr(
          tags$td(style = "border: 1px solid black; padding: 5px;", "All Time"),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$pitches_per_inning),
          tags$td(style = "border: 1px solid black; padding: 5px;", paste(stats$first_pitch_strike_pct, "%")),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$first_pitch_k_obp, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$whip),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$runs_per_9),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$hits_per_9),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$walks_per_9),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$strikeouts_per_9),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$free_br_per_9),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$ba_against, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", formatC(stats$obp_against, format = "f", digits = 3)),
          tags$td(style = "border: 1px solid black; padding: 5px;", stats$qabs_against)
        )
      ),
      h4("Pitch Selection"), # Table for the percentage of each pitch thrown in every count, depending on batter type
      tags$table(
        style = "width: 100%; border-collapse: collapse; table-layout: fixed; text-align: center;",
        tags$tr(
          tags$th(colspan = 1),
          tags$th(colspan = 4, style = "border:1px solid black; padding:5px; width:32%;", "Early"),
          tags$th(colspan = 4, style = "border:1px solid black; padding:5px; width:32%;", "Behind"),
          tags$th(colspan = 2, style = "border:1px solid black; padding:5px; width:16%;", "Ahead"),
          tags$th(colspan = 2, style = "border:1px solid black; padding:5px; width:16%;", "Other")
        ),
        tags$tr(
          tags$th(),
          lapply(c("0-0","1-0","0-1","1-1","2-0","2-1","3-0","3-1","0-2","1-2","2-2","3-2"), function(cnt) {
            tags$th(style = "border:1px solid black; padding:5px; width:8%;", cnt)
          })
        ),
        tags$tr(
          tags$th(style = "border:1px solid black; padding:5px; width:4%;", "RHH"),
          lapply(list(
            stats$pitch_selection$early$RHH,
            stats$pitch_selection$behind$RHH,
            stats$pitch_selection$ahead$RHH,
            stats$pitch_selection$other$RHH
          ) %>% unlist(recursive = FALSE), function(val) {
            tags$td(style = "border:1px solid black; padding:5px; width:8%;", val)
          })
        ),
        tags$tr(
          tags$th(style = "border:1px solid black; padding:5px; width:4%;", "LHH"),
          lapply(list(
            stats$pitch_selection$early$LHH,
            stats$pitch_selection$behind$LHH,
            stats$pitch_selection$ahead$LHH,
            stats$pitch_selection$other$LHH
          ) %>% unlist(recursive = FALSE), function(val) {
            tags$td(style = "border:1px solid black; padding:5px; width:8%;", val)
          })
        )
      )
    )
  }
  
  # Pitcher Report Rendering
  output$pitcher_report_output <- renderUI({
    message("Rendering pitcher_report_output")
    req(input$pitcher_players)
    message("Selected pitchers for report: ", paste(input$pitcher_players, collapse = ", "))
    
    pitcher_ids <- stored_players$data %>% 
      filter(full_name %in% input$pitcher_players) %>% 
      pull(player_id)
    message("Pitcher IDs for report: ", paste(pitcher_ids, collapse = ", "))
    
    tagList(
      lapply(pitcher_ids, function(pid) {
        player_data <- stored_players$data %>% 
          filter(player_id == pid) %>% 
          slice(1)
        stats <- calculate_pitcher_stats(pid)
        pitcherReportUI(player_data, stats, pitcher_id = pid)
      })
    )
  })
  
  # Render spray charts for pitchers
  observe({
    pitch_data <- global_pitch_data()
    
    # Pitcher spray chart rendering (unchanged)
    pitcher_ids <- stored_players$data %>%
      filter(full_name %in% input$pitcher_players) %>%
      pull(player_id)
    for (pid in pitcher_ids) {
      local({
        pid_local <- pid
        output[[paste0("pitcher_spray_chart_ui_RHH", pid_local)]] <- renderPlot({
          message("Rendering pitcher spray chart for ID: ", pid_local)
          generate_spray_chart(pid_local, role = "pitcher", pitch_data = pitch_data, player_side = "RHH")
        }, height = 300, width = 350)
        output[[paste0("pitcher_spray_chart_ui_LHH", pid_local)]] <- renderPlot({
          message("Rendering pitcher spray chart for ID: ", pid_local)
          generate_spray_chart(pid_local, role = "pitcher", pitch_data = pitch_data, player_side = "LHH")
        }, height = 300, width = 350)
      })
    }
  })
  
  # After clicking the download button, this is what runs to create the one page pdf's of our outputs
  output$export_pitcher_reports <- downloadHandler(
    filename = function() { paste("Pitcher_Reports_", Sys.Date(), ".pdf", sep = "") }, # could change the name of file if wanted
    content = function(file) {
      req(input$pitcher_players)
      pitcher_ids <- stored_players$data %>% 
        filter(full_name %in% input$pitcher_players) %>% 
        pull(player_id)
      
      # Temporary directory for images
      temp_dir <- tempdir()
      temp_html <- file.path(temp_dir, "pitcher_report.html")
      
      # Generate HTML content
      html_content <- lapply(pitcher_ids, function(pid) {
        player_data <- stored_players$data %>% 
          filter(player_id == pid) %>% 
          slice(1)
        # Fill in the blanks for players with no data
        stats <- calculate_pitcher_stats(pid) %||% list(
          innings = "No Data",
          pitches_per_inning = "No Data", 
          first_pitch_strike_pct = "No Data", 
          first_pitch_k_obp = "No Data",
          whip = "No Data", 
          runs_per_9 = "No Data", 
          hits_per_9 = "No Data", 
          walks_per_9 = "No Data",
          strikeouts_per_9 = "No Data", 
          free_br_per_9 = "No Data", 
          ba_against = "No Data",
          obp_against = "No Data", 
          qabs_against = "No Data",
          pitches_per_inning_last_15 = "No Data", 
          first_pitch_strike_pct_last_15 = "No Data", 
          first_pitch_k_obp_last_15 = "No Data",
          whip_last_15 = "No Data", 
          runs_per_9_last_15 = "No Data",
          hits_per_9_last_15 = "No Data",
          walks_per_9_last_15 = "No Data", 
          strikeouts_per_9_last_15 = "No Data", 
          free_br_per_9_last_15 = "No Data",
          ba_against_last_15 = "No Data", 
          obp_against_last_15 = "No Data", 
          qabs_against_last_15 = "No Data",
          pitch_types = list(),
          pitch_types_metrics = list(Other = list(
            LHH = list(strike_pct = "No Data", called_strike_pct = "No Data", swing_miss_pct = "No Data",
                       strikeout_pct = "No Data", pressure_swing_pct = "No Data", hard_hit_pct = "No Data",
                       weak_hit_pct = "No Data", ground_ball_pct = "No Data"),
            RHH = list(strike_pct = "No Data", called_strike_pct = "No Data", swing_miss_pct = "No Data",
                       strikeout_pct = "No Data", pressure_swing_pct = "No Data", hard_hit_pct = "No Data",
                       weak_hit_pct = "No Data", ground_ball_pct = "No Data")
          )),
          pitch_counts = data.frame(pitch_type = character(), times = integer(), pct_label = character()),
          pitch_selection = list(
            early = list(RHH = list("0-0" = "No Data", "1-0" = "No Data", "0-1" = "No Data", "1-1" = "No Data"),
                         LHH = list("0-0" = "No Data", "1-0" = "No Data", "0-1" = "No Data", "1-1" = "No Data")),
            behind = list(RHH = list("2-0" = "No Data", "2-1" = "No Data", "3-0" = "No Data", "3-1" = "No Data"),
                          LHH = list("2-0" = "No Data", "2-1" = "No Data", "3-0" = "No Data", "3-1" = "No Data")),
            ahead = list(RHH = list("0-2" = "No Data", "1-2" = "No Data"),
                         LHH = list("0-2" = "No Data", "1-2" = "No Data")),
            other = list(RHH = list("2-2" = "No Data", "3-2" = "No Data"),
                         LHH = list("2-2" = "No Data", "3-2" = "No Data"))
          ),
          last_50 = list(
            fb = list(data.frame(pitch_velo = numeric())),
            cb = list(data.frame(pitch_velo = numeric())),
            sl = list(data.frame(pitch_velo = numeric())),
            ch = list(data.frame(pitch_velo = numeric())),
            sp = list(data.frame(pitch_velo = numeric())),
            ct = list(data.frame(pitch_velo = numeric())),
            oth = list(data.frame(pitch_velo = numeric()))
          )
        )
        
        # Determine top pitch types
        top_pitches <- if (length(stats$pitch_types) > 0) {
          pitch_usage <- sapply(stats$pitch_types, function(pt) as.numeric(pt$Total) %||% 0)
          pitch_names <- names(stats$pitch_types)
          ordered_pitches <- pitch_names[order(pitch_usage, decreasing = TRUE)]
          head(ordered_pitches, 4)
        } else {
          character(0)
        }
        
        # Render spray charts as base64
        spray_base64_lhh <- {
          temp_plot <- file.path(temp_dir, paste0("spray_chart_lhh_", pid, ".png"))
          spray_chart <- generate_spray_chart(pid, role = "pitcher", pitch_data = global_pitch_data(), player_side = "LHH")
          if (!is.null(spray_chart)) {
            ggsave(temp_plot, plot = spray_chart, width = 4, height = 4, dpi = 300, limitsize = FALSE)
            base64enc::base64encode(temp_plot)
          } else {
            png(temp_plot, width = 300, height = 300, units = "px", res = 75)
            plot.new()
            text(0.5, 0.5, "No LHH spray chart data", cex = 1.5)
            dev.off()
            base64enc::base64encode(temp_plot)
          }
        }
        spray_base64_rhh <- {
          temp_plot <- file.path(temp_dir, paste0("spray_chart_rhh_", pid, ".png"))
          spray_chart <- generate_spray_chart(pid, role = "pitcher", pitch_data = global_pitch_data(), player_side = "RHH")
          if (!is.null(spray_chart)) {
            ggsave(temp_plot, plot = spray_chart, width = 4, height = 4, dpi = 300, limitsize = FALSE)
            base64enc::base64encode(temp_plot)
          } else {
            png(temp_plot, width = 300, height = 300, units = "px", res = 75)
            plot.new()
            text(0.5, 0.5, "No RHH spray chart data", cex = 1.5)
            dev.off()
            base64enc::base64encode(temp_plot)
          }
        }
        
        # Velocity ranges
        velocity_html <- paste0(
          '<table style="width: 100%; font-size: 10px;">',
          if (nrow(stats$last_50$fb[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">FB:</th><td style="padding: 3px;">', 
                   min(stats$last_50$fb[[1]]$pitch_velo), " - ", max(stats$last_50$fb[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$cb[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">CB:</th><td style="padding: 3px;">', 
                   min(stats$last_50$cb[[1]]$pitch_velo), " - ", max(stats$last_50$cb[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$sl[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">SL:</th><td style="padding: 3px;">', 
                   min(stats$last_50$sl[[1]]$pitch_velo), " - ", max(stats$last_50$sl[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$ch[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">CH:</th><td style="padding: 3px;">', 
                   min(stats$last_50$ch[[1]]$pitch_velo), " - ", max(stats$last_50$ch[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$sp[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">SP:</th><td style="padding: 3px;">', 
                   min(stats$last_50$sp[[1]]$pitch_velo), " - ", max(stats$last_50$sp[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$ct[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">CT:</th><td style="padding: 3px;">', 
                   min(stats$last_50$ct[[1]]$pitch_velo), " - ", max(stats$last_50$ct[[1]]$pitch_velo), '</td></tr>')
          },
          if (nrow(stats$last_50$oth[[1]]) > 0) {
            paste0('<tr><th style="padding: 3px; text-align: left;">OTH:</th><td style="padding: 3px;">', 
                   min(stats$last_50$oth[[1]]$pitch_velo), " - ", max(stats$last_50$oth[[1]]$pitch_velo), '</td></tr>')
          },
          '</table>'
        )
        
        # Construct HTML
        html <- paste0(
          '<div class="row" style="flex-wrap: nowrap; align-items: center;">',
          '  <div class="column" style="flex: 1; min-width: 250px; max-width: 250px;">',
          '    <h4 style="text-align: center; margin-bottom: 3px; font-size: 14px;">LHH</h4>',
          '    <div style="width: 250px; margin: 0 auto;">',
          '      <img src="data:image/png;base64,', spray_base64_lhh, '" width="250px" height="250px" style="display: block; margin: 0 auto;"/>',
          '    </div>',
          '  </div>',
          '  <div class="column" style="flex: 1; min-width: 180px; max-width: 180px; text-align: center;">',
          '    <h3 style="font-size: 16px; margin-bottom: 2px;">', player_data$full_name, '</h3>',
          '    <p style="font-size: 10px; margin: 1px 0;">#', player_data$jersey_number, '</p>',
          '    <p style="font-size: 10px; margin: 1px 0;">', 
          ifelse(player_data$throwing_handedness == "Left", "LHP", ifelse(player_data$throwing_handedness == "Right", "RHP", "")), '</p>',
          '    <p style="font-size: 10px; margin: 1px 0;">Innings: ', stats$innings %||% "No Data", '</p>',
          '    <div style="font-size: 10px;">', velocity_html, '</div>',
          '  </div>',
          '  <div class="column" style="flex: 1; min-width: 250px; max-width: 250px;">',
          '    <h4 style="text-align: center; margin-bottom: 3px; font-size: 14px;">RHH</h4>',
          '    <div style="width: 250px; margin: 0 auto;">',
          '      <img src="data:image/png;base64,', spray_base64_rhh, '" width="250px" height="250px" style="display: block; margin: 0 auto;"/>',
          '    </div>',
          '  </div>',
          '</div>',
          '<div class="row">',
          '  <div class="column" style="flex: 12;">',
          '    <h4 style="font-size: 14px; margin: 5px 0;">Pitch Type Metrics</h4>',
          if (length(top_pitches) == 0 && is.null(stats$pitch_types_metrics$Other)) {
            '    <p style="font-size: 10px; margin: 2px 0;">No pitch type data available</p>'
          } else {
            paste0(
              '    <div style="display: flex; align-items: flex-start;">',
              '      <div style="width: 120px; margin-right: 5px;">',
              '        <div style="padding-top: 40px;">',
              paste(sapply(c("Usage:", "", "Strike %", "Called Strike %", "Swing & Miss %", "% of Total Strikeouts",
                             "Pressure Pitch Swing %", "Hard Hit %", "Weak Hit %", "Ground Ball %"), 
                           function(metric) paste0('<p style="margin: 0; padding: 3px 0; text-align: right; line-height: 20px; height: 20px; font-size: 10px;">', metric, '</p>')), 
                    collapse = "\n"),
              '        </div>',
              '      </div>',
              '      <div style="flex-grow: 1;">',
              '        <div class="row">',
              paste(sapply(head(top_pitches, 4), function(pt) {
                metrics <- stats$pitch_types_metrics[[pt]] %||% list(
                  LHH = list(strike_pct = "No Data", called_strike_pct = "No Data", swing_miss_pct = "No Data",
                             strikeout_pct = "No Data", pressure_swing_pct = "No Data", hard_hit_pct = "No Data",
                             weak_hit_pct = "No Data", ground_ball_pct = "No Data"),
                  RHH = list(strike_pct = "No Data", called_strike_pct = "No Data", swing_miss_pct = "No Data",
                             strikeout_pct = "No Data", pressure_swing_pct = "No Data", hard_hit_pct = "No Data",
                             weak_hit_pct = "No Data", ground_ball_pct = "No Data")
                )
                paste0(
                  '          <div class="column" style="flex: 3; padding: 3px;">',
                  '            <table style="width: 100%; border-collapse: collapse;">',
                  '              <tr><th colspan="2" style="border: 1px solid black; padding: 3px; text-align: center; line-height: 25px; font-size: 20px;">', pt, '</th></tr>',
                  '              <tr><th colspan="2" style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', 
                  (stats$pitch_counts %>% filter(pitch_type == pt) %>% pull(pct_label) %||% "N/A"), '</th></tr>',
                  '              <tr><th style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">LHH</th>',
                  '                  <th style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">RHH</th></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$strike_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$strike_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$called_strike_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$called_strike_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$swing_miss_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$swing_miss_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$strikeout_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$strikeout_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$pressure_swing_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$pressure_swing_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$hard_hit_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$hard_hit_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$weak_hit_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$weak_hit_pct, "%"), '</td></tr>',
                  '              <tr><td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$LHH$ground_ball_pct, "%"), '</td>',
                  '                  <td style="border: 1px solid black; padding: 3px; text-align: center; line-height: 20px; font-size: 10px;">', paste(metrics$RHH$ground_ball_pct, "%"), '</td></tr>',
                  '            </table>',
                  '          </div>'
                )
              }), collapse = "\n"),
              '        </div>',
              '      </div>',
              '    </div>'
            )
          },
          '  </div>',
          '</div>',
          '<div class="row">',
          '  <div class="column" style="flex: 12;">',
          '    <h4 style="font-size: 14px; margin: 1px 0;">Pitching Efficiency</h4>',
          '    <table style="width: 100%; border-collapse: collapse; text-align: center;">',
          '      <tr>',
          '        <th style="border: 1px solid black; padding: 3px; width: 5%; font-size: 10px;">Batters</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">Pitches Per Inn</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">First Pitch K%</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">First Pitch K OBP%</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">WHIP</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">Runs Per 9</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">Hits Per 9</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">Walks Per 9</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">SO Per 9</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">Free 90s Per 9</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">BA Against</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">OBP Against</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7%; font-size: 10px;">QABs Against</th>',
          '      </tr>',
          '      <tr>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">Last 15</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitches_per_inning_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', paste(stats$first_pitch_strike_pct_last_15 %||% "No Data", "%"), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$first_pitch_k_obp_last_15 %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$whip_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$runs_per_9_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$hits_per_9_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$walks_per_9_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$strikeouts_per_9_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$free_br_per_9_last_15 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$ba_against_last_15 %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$obp_against_last_15 %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$qabs_against_last_15 %||% "No Data", '</td>',
          '      </tr>',
          '      <tr>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">All Time</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitches_per_inning %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', paste(stats$first_pitch_strike_pct %||% "No Data", "%"), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$first_pitch_k_obp %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$whip %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$runs_per_9 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$hits_per_9 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$walks_per_9 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$strikeouts_per_9 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$free_br_per_9 %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$ba_against %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', formatC(stats$obp_against %||% 0, format = "f", digits = 3), '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$qabs_against %||% "No Data", '</td>',
          '      </tr>',
          '    </table>',
          '  </div>',
          '</div>',
          '<div class="row">',
          '  <div class="column" style="flex: 12;">',
          '    <h4 style="font-size: 14px; margin: 1px 0;">Pitch Selection</h4>',
          '    <table style="width: 100%; border-collapse: collapse; table-layout: fixed; text-align: center;">',
          '      <tr>',
          '        <th style="border: 1px solid black; padding: 3px; width: 5%; font-size: 10px;"></th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 35%; font-size: 10px;" colspan="4">Early</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 35%; font-size: 10px;" colspan="4">Behind</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 15%; font-size: 10px;" colspan="2">Ahead</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 15%; font-size: 10px;" colspan="2">Other</th>',
          '      </tr>',
          '      <tr>',
          '        <th style="border: 1px solid black; padding: 3px; font-size: 10px;"></th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">0-0</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">1-0</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">0-1</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">1-1</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">2-0</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">2-1</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">3-0</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 8.75%; font-size: 10px;">3-1</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7.5%; font-size: 10px;">0-2</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7.5%; font-size: 10px;">1-2</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7.5%; font-size: 10px;">2-2</th>',
          '        <th style="border: 1px solid black; padding: 3px; width: 7.5%; font-size: 10px;">3-2</th>',
          '      </tr>',
          '      <tr>',
          '        <th style="border: 1px solid black; padding: 3px; font-size: 10px;">RHH</th>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$RHH$`0-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$RHH$`1-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$RHH$`0-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$RHH$`1-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$RHH$`2-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$RHH$`2-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$RHH$`3-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$RHH$`3-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$ahead$RHH$`0-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$ahead$RHH$`1-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$other$RHH$`2-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$other$RHH$`3-2` %||% "No Data", '</td>',
          '      </tr>',
          '      <tr>',
          '        <th style="border: 1px solid black; padding: 3px; font-size: 10px;">LHH</th>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$LHH$`0-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$LHH$`1-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$LHH$`0-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$early$LHH$`1-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$LHH$`2-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$LHH$`2-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$LHH$`3-0` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$behind$LHH$`3-1` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$ahead$LHH$`0-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$ahead$LHH$`1-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$other$LHH$`2-2` %||% "No Data", '</td>',
          '        <td style="border: 1px solid black; padding: 3px; font-size: 10px;">', stats$pitch_selection$other$LHH$`3-2` %||% "No Data", '</td>',
          '      </tr>',
          '    </table>',
          '  </div>',
          '</div>'
        )
      })
      
      # Create full HTML
      html_full <- tagList(
        tags$head(
          tags$style(HTML("
    body { font-family: Helvetica, sans-serif; margin: 5px; }
    h3 { color: #000; margin-bottom: 2px; font-size: 16px; }
    h4 { color: #000; font-size: 14px; margin: 5px 0; }
    table { width: 100%; border-collapse: collapse; margin-bottom: 1px; }
    th, td { border: 1px solid black; padding: 3px; text-align: center; }
    th { background-color: #e0e0e0; }
    .row { display: flex; margin-bottom: 5px; }
    .column { padding: 3px; }
    img { max-width: 250px; height: auto; display: block; margin: 0 auto; }
    p { margin: 1px 0; line-height: 14px; }
  "))
        ),
        tags$body(
          tags$h2("Pitcher Reports", style="font-size: 18px; margin: 5px 0;"),
          lapply(html_content, function(content) HTML(content))
        )
      )
      
      # Save HTML and convert to PDF
      htmltools::save_html(html_full, temp_html)
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1000,
        vheight = 1400,
        zoom = 1,
        expand = c(5, 5, 5, 5)
      )
      
      # Clean up
      unlink(c(temp_html, file.path(temp_dir, "*.png")))
    }
  )
  
  
  ## ---- All Hitters Functioning ----
  # For the hitter_team dropdowns, only show teams that have hitters with data in global_pitch_data
  observe({
    req(teams$data, global_pitch_data(), stored_players$data, app_state$page == "stat")  # Only update when on Stats page
    # Hitters with data
    hitters_with_data <- global_pitch_data() %>%
      pull(hitter_id) %>%
      unique()
    # Unique teams of these hitters
    hitter_teams <- stored_players$data %>%
      filter(player_id %in% hitters_with_data) %>%
      pull(college_name) %>%
      unique()
    valid_hitter_teams <- teams$data$college_name[teams$data$college_name %in% hitter_teams]
    # Update the team options
    updateSelectInput(session, "hitter_team",
                      choices = c("Choose Team", valid_hitter_teams),
                      selected = "Choose Team")
  })
  observe({
    req(
      input$hitter_team,
      input$hitter_status_of_player,
      input$hitter_type_of_event,
      global_pitch_data(),
      stored_players$data,
      all_events(),
      app_state$page == "stat"
    )
    
    if (input$hitter_team == "Choose Team") return()
    
    # Get hitters from selected team and status
    eligible_hitters <- stored_players$data %>%
      filter(
        college_name == input$hitter_team,
        status == input$hitter_status_of_player
      ) %>%
      pull(player_id)
    
    # Join pitch data with events and filter by team hitters
    pitch_events <- global_pitch_data() %>%
      filter(hitter_id %in% eligible_hitters) %>%
      left_join(all_events(), by = "event_id")
    
    # Filter by selected event type
    if (input$hitter_type_of_event != "Both") {
      pitch_events <- pitch_events %>%
        filter(event_type == input$hitter_type_of_event)
    }
    
    # Get unique sorted dates
    available_dates <- pitch_events %>%
      pull(date) %>%
      unique() %>%
      sort(decreasing = TRUE)
    
    # Add "All Dates" to beginning of list
    date_choices <- c("All Dates", available_dates)
    
    # Update the selectize input
    updateSelectizeInput(
      session, "hitter_date",
      choices = date_choices,
      selected = "All Dates",
      server = TRUE
    )
  })
  
  # Reactive values of hitters for stats check box selection (Changes when any selectInputs or databases change)
  valid_hitters <- reactive({
    req(input$hitter_team, input$hitter_type_of_event, input$hitter_status_of_player, global_pitch_data(), stored_players$data)
    
    if (input$hitter_team == "Choose Team" || nrow(global_pitch_data()) == 0) {
      return(character(0))
    }
    
    # Filter hitter IDs based on event type
    if (input$hitter_type_of_event == "Both") {
      active_hitter_ids <- global_pitch_data() %>%
        pull(hitter_id) %>%
        unique()
    } else {
      joined_data <- global_pitch_data() %>%
        left_join(all_events(), by = "event_id")
      active_hitter_ids <- joined_data %>%
        filter(event_type == input$hitter_type_of_event) %>%
        pull(hitter_id) %>%
        unique()
    }
    
    # Get player names from stored_players
    result2 <- stored_players$data %>%
      filter(
        college_name == input$hitter_team,
        status == input$hitter_status_of_player,
        player_id %in% active_hitter_ids
      ) %>%
      arrange(full_name) %>%
      pull(full_name)
    
    result2
  })
  
  # Update the check box options 
  observeEvent(valid_hitters(), {
    hitter_choices <- valid_hitters()
    
    label_text <- if (length(hitter_choices) == 0) {
      "Select Hitters: (No hitters available for this team)"
    } else {
      "Select Hitters:"
    }
    
    updateCheckboxGroupInput(session, "hitter_players",
                             choices = hitter_choices,
                             selected = NULL,
                             label = label_text)
  })
  
  # Calculating the statistics for hitters
  calculate_hitter_stats <- function(hitter_id_calculator) {
    all_hitter_data <- global_pitch_data() %>%
      filter(hitter_id == hitter_id_calculator)
    
    # Fetch stored_players reactive data
    player_info <- tryCatch({
      stored_players$data
    }, error = function(e) {
      message("Failed to access stored_players$data: ", e$message)
      data.frame(player_id = character(), batter_handedness = character(), player_type = character())
    })
    
    # Fetch events reactive data
    player_events <- tryCatch({
      isolate(all_events())
    }, error = function(e) {
      message("Failed to access events(): ", e$message)
      data.frame(event_id = numeric(), date = character(), event_type = character(),
                 home_team = character(), away_team = character())
    })
    
    # Combine hitter data with handedness and event type
    hitter_data <- all_hitter_data %>%
      left_join(player_info, by = c("hitter_id" = "player_id")) %>%
      left_join(player_events, by = "event_id") %>%
      filter(
        if (input$hitter_type_of_event != "Both") event_type == input$hitter_type_of_event else TRUE
      ) %>%
      arrange(event_id, game_pitch_number) %>%
      mutate(
        pa_id = cumsum(at_bat_pitch_number == 1)
      )
    if (nrow(all_hitter_data) == 0) {
      message("No pitch data in database for hitter ID: ", hitter_id_calculator)
      return(NULL)
    }
    
    # plate_appearances (PA) stats / info
    plate_appearances <- hitter_data %>% 
      group_by(event_id, pa_id) %>% 
      slice_max(at_bat_pitch_number, n = 1, with_ties = FALSE) %>% 
      transmute(
        pitcher_id = pitcher_id,
        pitcher_side = throwing_handedness,
        pitches_in_at_bat = at_bat_pitch_number,
        pitchers_pitch_count = pitchers_pitch_count,
        pitch_outcome = pitch_outcome,
        outcome = ball_in_play_outcome,
        ball_flight = ball_flight,
        contact_type = contact_type,
        balls = balls, 
        strikes = strikes,
        batter_base = batter_outcome,
        first_base = coalesce(first_base_outcome,  "None"),
        second_base = coalesce(second_base_outcome, "None"),
        third_base = coalesce(third_base_outcome,  "None")
      ) %>% 
      ungroup() %>%
      arrange(event_id, pitchers_pitch_count)
    
    # Use PA to filter for just times they counted as At Bats (AB)
    at_bats <- plate_appearances %>%
      filter(!outcome %in% c("Walk", "HBP", "Sacrifice Bunt", "Sacrifice Fly", "Cage Ball", "None", "Other (Interference)"))
    
    # Total AB's and PA's
    total_pas <- if (nrow(plate_appearances) == 0) {1} else {nrow(plate_appearances)}
    total_abs <- if (nrow(at_bats) == 0) {1} else {nrow(at_bats)}
    
    # Totals of basic stats
    hits <- sum(plate_appearances$outcome %in% c("Single", "Double", "Triple", "Homerun"), na.rm = TRUE)
    singles <- sum(at_bats$outcome == "Single", na.rm = TRUE)
    doubles <- sum(at_bats$outcome == "Double", na.rm = TRUE)
    triples <- sum(at_bats$outcome == "Triple", na.rm = TRUE)
    homeruns <- sum(at_bats$outcome == "Homerun", na.rm = TRUE)
    walks <- sum(plate_appearances$outcome == "Walk", na.rm = TRUE)
    hbp <- sum(plate_appearances$outcome == "Hit By Pitch", na.rm = TRUE)
    num_of_sacs <- sum(plate_appearances$outcome %in% c("Sacrifice Bunt", "Sacrifice Fly"), na.rm = TRUE)
    so_swinging <- sum(plate_appearances$outcome == "SO Swinging", na.rm = TRUE)
    so_looking <- sum(plate_appearances$outcome == "SO Looking", na.rm = TRUE)
    on_base <- sum(!plate_appearances$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE)
    total_bases <- singles + (2 * doubles) + (3 * triples) + (4 * homeruns)
    
    # Percentage stats
    ba <- sprintf("%.3f", if (total_abs > 0) hits / total_abs else 0) #batting average
    obp <- sprintf("%.3f", if (total_pas > 0) on_base / sum(!plate_appearances$batter_base %in% c("Cage Ball"), na.rm = TRUE) else 0) #on base percentage
    lob <- if (total_abs > 0)
      round((sum(at_bats$first_base == "LOB", na.rm = TRUE) +
               sum(at_bats$second_base == "LOB", na.rm = TRUE) +
               sum(at_bats$third_base == "LOB", na.rm = TRUE)) / nrow(at_bats) * 100, 1) else 0 # Left on base percentage
    slg <- sprintf("%.3f", if (total_abs > 0) total_bases / total_abs else 0) # slugging percentage
    
    # Other stats used for calculating Quality At Bats (QAB)
    rbi <- (sum(at_bats$batter_base == "Scored", na.rm = TRUE) +
              sum(at_bats$first_base == "Scored", na.rm = TRUE) +
              sum(at_bats$second_base == "Scored", na.rm = TRUE) +
              sum(at_bats$third_base == "Scored", na.rm = TRUE))
    contact_total <- sum(plate_appearances$contact_type == "Weak", na.rm = TRUE) +
      (sum(plate_appearances$contact_type == "Average", na.rm = TRUE) * 2) +
      (sum(plate_appearances$contact_type == "Hard", na.rm = TRUE) * 3)
    six_plus_pitches <- sum(plate_appearances$pitches_in_at_bat >= 6 & !at_bats$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE)
    
    # QAB's for hitters
    qab_hitter <- round(
      ((hits + walks + rbi + hbp + num_of_sacs + six_plus_pitches) * 3
       + contact_total
       - so_swinging * 3
       - so_looking   * 2
      ) / ifelse(total_pas > 0, total_pas, 1), 1)
    
    
    # Zone definitions
    is_heart <- hitter_data$pitch_location_x >= -5.7 & hitter_data$pitch_location_x <= 5.7 &
      hitter_data$pitch_location_y >= 22 & hitter_data$pitch_location_y <= 38
    heart_pitch <- ifelse(is.na(is_heart), TRUE, is_heart)
    is_pressure <- (hitter_data$pitch_location_x >= -11.3 & hitter_data$pitch_location_x <= -5.7 |
                      hitter_data$pitch_location_x >= 5.7 & hitter_data$pitch_location_x <= 11.3) &
      (hitter_data$pitch_location_y >= 14 & hitter_data$pitch_location_y <= 22 |
         hitter_data$pitch_location_y >= 38 & hitter_data$pitch_location_y <= 46)
    pressure_pitch <- ifelse(is.na(is_pressure), TRUE, is_pressure)
    
    # Spliting hitter data by pitchers throwing handedness
    lhp_data <- hitter_data %>% filter(throwing_handedness == "Left")
    rhp_data <- hitter_data %>% filter(throwing_handedness == "Right")
    
    
    # (FOR THE EXAMPLE BELOW) - Talking about calculations below this line
    
    # Left Handed Pitchers pressure pitches
    heart_pitch_left <- lhp_data$pitch_location_x >= -5.7 & lhp_data$pitch_location_x <= 5.7 &
      lhp_data$pitch_location_y >= 22 & lhp_data$pitch_location_y <= 38
    pressure_pitch_left <- (lhp_data$pitch_location_x >= -11.3 & lhp_data$pitch_location_x <= -5.7 |
                              lhp_data$pitch_location_x >= 5.7 & lhp_data$pitch_location_x <= 11.3) &
      (lhp_data$pitch_location_y >= 14 & lhp_data$pitch_location_y <= 22 |
         lhp_data$pitch_location_y >= 38 & lhp_data$pitch_location_y <= 46)
    
    # Right Handed Pitchers pressure pitches
    heart_pitch_right <- rhp_data$pitch_location_x >= -5.7 & rhp_data$pitch_location_x <= 5.7 &
      rhp_data$pitch_location_y >= 22 & rhp_data$pitch_location_y <= 38
    pressure_pitch_right <- (rhp_data$pitch_location_x >= -11.3 & rhp_data$pitch_location_x <= -5.7 |
                               rhp_data$pitch_location_x >= 5.7 & rhp_data$pitch_location_x <= 11.3) &
      (rhp_data$pitch_location_y >= 14 & rhp_data$pitch_location_y <= 22 |
         rhp_data$pitch_location_y >= 38 & rhp_data$pitch_location_y <= 46)
    
    # Heart table lhp stats
    lhp_heart_swing_pct <- if (sum(heart_pitch_left & hitter_data$throwing_handedness == "Left", na.rm = TRUE) > 0)
      round(sum(hitter_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & heart_pitch_left & 
                  hitter_data$throwing_handedness == "Left", na.rm = TRUE) / 
              sum(heart_pitch_left & hitter_data$throwing_handedness == "Left", na.rm = TRUE) * 100, 1) else 0
    lhp_heart_miss_pct <- if (sum(heart_pitch_left & hitter_data$throwing_handedness == "Left", na.rm = TRUE) > 0)
      round(sum(hitter_data$pitch_outcome == "Swinging Strike" & heart_pitch_left & 
                  hitter_data$throwing_handedness == "Left", na.rm = TRUE) / 
              sum(heart_pitch_left & hitter_data$throwing_handedness == "Left", na.rm = TRUE) * 100, 1) else 0
    lhp_heart_hard_pct <- {
      denom <- sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch_left & 
                     hitter_data$throwing_handedness == "Left", na.rm = TRUE)
      num <- sum(hitter_data$contact_type == "Hard" & heart_pitch_left & 
                   hitter_data$throwing_handedness == "Left", na.rm = TRUE)
      if (denom > 0) round(num / denom * 100, 1) else 0
    }
    lhp_heart_weak_pct <- {
      denom <- sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch_left & 
                     hitter_data$throwing_handedness == "Left", na.rm = TRUE)
      num <- sum(hitter_data$contact_type == "Weak" & heart_pitch_left & 
                   hitter_data$throwing_handedness == "Left", na.rm = TRUE)
      if (denom > 0) round(num / denom * 100, 1) else 0
    }
    
    # Heart table rhp stats
    rhp_heart_swing_pct <- if (sum(heart_pitch_right & hitter_data$throwing_handedness == "Right", na.rm = TRUE) > 0)
      round(sum(hitter_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & heart_pitch_right & 
                  hitter_data$throwing_handedness == "Right", na.rm = TRUE) / 
              sum(heart_pitch_right & hitter_data$throwing_handedness == "Right", na.rm = TRUE) * 100, 1) else 0
    rhp_heart_miss_pct <- if (sum(heart_pitch_right & hitter_data$throwing_handedness == "Right", na.rm = TRUE) > 0)
      round(sum(hitter_data$pitch_outcome == "Swinging Strike" & heart_pitch_right & 
                  hitter_data$throwing_handedness == "Right", na.rm = TRUE) / 
              sum(heart_pitch_right & hitter_data$throwing_handedness == "Right", na.rm = TRUE) * 100, 1) else 0
    rhp_heart_denom <- sum(
      hitter_data$pitch_outcome == "Ball In Play" & heart_pitch_right & 
        hitter_data$throwing_handedness == "Right", 
      na.rm = TRUE
    )
    rhp_heart_hard_pct <- if (rhp_heart_denom > 0) round(
      sum(hitter_data$contact_type == "Hard" & heart_pitch_right & 
            hitter_data$throwing_handedness == "Right", na.rm = TRUE) / 
        rhp_heart_denom * 100, 1) else 0
    
    rhp_heart_weak_pct <- if (rhp_heart_denom > 0) round(
      sum(hitter_data$contact_type == "Weak" & heart_pitch_right & 
            hitter_data$throwing_handedness == "Right", na.rm = TRUE) / 
        rhp_heart_denom * 100, 1) else 0
    
    # Pressure table lhp stats
    lhp_pressure_swing_pct <- if (sum(pressure_pitch_left & lhp_data$throwing_handedness == "Left", na.rm = TRUE) > 0) {
      round(
        sum(lhp_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") &
              pressure_pitch_left & lhp_data$throwing_handedness == "Left", na.rm = TRUE) /
          sum(pressure_pitch_left & lhp_data$throwing_handedness == "Left", na.rm = TRUE) * 100,
        1
      ) } else {0}
    # Logical filter for left-handed hitters in the pressure zone
    pressure_left_filter <- pressure_pitch_left & lhp_data$throwing_handedness == "Left" # example lhp_data already only has throwing_handedness == "Left"
    lhp_pressure_miss_pct <- if (sum(pressure_left_filter, na.rm = TRUE) > 0) {
      round(
        sum(lhp_data$pitch_outcome == "Swinging Strike" & pressure_left_filter, na.rm = TRUE) /
          sum(pressure_left_filter, na.rm = TRUE) * 100, 1
      )} else {0}
    contact_filter <- lhp_data$pitch_outcome == "Ball In Play" & pressure_left_filter
    lhp_pressure_hard_pct <- if (sum(contact_filter, na.rm = TRUE) > 0) {
      round(
        sum(lhp_data$contact_type == "Hard" & contact_filter, na.rm = TRUE) /
          sum(contact_filter, na.rm = TRUE) * 100, 1
      )} else {0}
    lhp_pressure_weak_pct <- if (sum(contact_filter, na.rm = TRUE) > 0) {
      round(
        sum(lhp_data$contact_type == "Weak" & contact_filter, na.rm = TRUE) /
          sum(contact_filter, na.rm = TRUE) * 100, 1
      )} else {0}
    
    # Pressure table rhp stats
    # Logical filter for right-handed hitters in the pressure zone
    pressure_right_filter <- pressure_pitch_right & rhp_data$throwing_handedness == "Right"
    # Swing %
    rhp_pressure_swing_pct <- if (sum(pressure_right_filter, na.rm = TRUE) > 0) {
      round(
        sum(rhp_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_right_filter, na.rm = TRUE) /
          sum(pressure_right_filter, na.rm = TRUE) * 100, 1
      )
    } else {
      0
    }
    # Miss %
    rhp_pressure_miss_pct <- if (sum(pressure_right_filter, na.rm = TRUE) > 0) {
      round(
        sum(rhp_data$pitch_outcome == "Swinging Strike" & pressure_right_filter, na.rm = TRUE) /
          sum(pressure_right_filter, na.rm = TRUE) * 100, 1
      )
    } else {
      0
    }
    # Filter for balls in play
    contact_filter_rhp <- rhp_data$pitch_outcome == "Ball In Play" & pressure_right_filter
    
    # Hard Contact %
    rhp_pressure_hard_pct <- if (sum(contact_filter_rhp, na.rm = TRUE) > 0) {
      round(
        sum(rhp_data$contact_type == "Hard" & contact_filter_rhp, na.rm = TRUE) /
          sum(contact_filter_rhp, na.rm = TRUE) * 100, 1
      )
    } else {
      0
    }
    # Weak Contact %
    rhp_pressure_weak_pct <- if (sum(contact_filter_rhp, na.rm = TRUE) > 0) {
      round(
        sum(rhp_data$contact_type == "Weak" & contact_filter_rhp, na.rm = TRUE) /
          sum(contact_filter_rhp, na.rm = TRUE) * 100, 1
      )
    } else {
      0
    }
    
    # Overall table stats from lhp
    lhp_overall_swing_pct <- if (nrow(lhp_data) > 0)
      round(sum(lhp_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / 
              nrow(lhp_data) * 100, 1) else 0
    lhp_overall_miss_pct <- if (nrow(lhp_data) > 0)
      round(sum(lhp_data$pitch_outcome == "Swinging Strike", na.rm = TRUE) / nrow(lhp_data) * 100, 1) else 0
    lhp_overall_hard_pct <- if (sum(lhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(lhp_data$contact_type == "Hard", na.rm = TRUE) / 
              sum(lhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    lhp_overall_weak_pct <- if (sum(lhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(lhp_data$contact_type == "Weak", na.rm = TRUE) / 
              sum(lhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    
    # Overall table stats from rhp
    rhp_overall_swing_pct <- if (nrow(rhp_data) > 0)
      round(sum(rhp_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / 
              nrow(rhp_data) * 100, 1) else 0
    rhp_overall_miss_pct <- if (nrow(rhp_data) > 0)
      round(sum(rhp_data$pitch_outcome == "Swinging Strike", na.rm = TRUE) / nrow(rhp_data) * 100, 1) else 0
    rhp_overall_hard_pct <- if (sum(rhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(rhp_data$contact_type == "Hard", na.rm = TRUE) / 
              sum(rhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    rhp_overall_weak_pct <- if (sum(rhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(rhp_data$contact_type == "Weak", na.rm = TRUE) / 
              sum(rhp_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    
    # Heart table, all column stats
    heart_swing_pct <- if (nrow(hitter_data) > 0 && any(heart_pitch))
      round(sum(hitter_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & heart_pitch, na.rm = TRUE) / 
              sum(heart_pitch, na.rm = TRUE) * 100, 1) else 0
    heart_miss_pct <- if (nrow(hitter_data) > 0 && any(heart_pitch))
      round(sum(hitter_data$pitch_outcome == "Swinging Strike" & heart_pitch, na.rm = TRUE) / 
              sum(heart_pitch, na.rm = TRUE) * 100, 1) else 0
    heart_hard_pct <- if (sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch, na.rm = TRUE) > 0)
      round(sum(hitter_data$contact_type == "Hard" & heart_pitch, na.rm = TRUE) / 
              sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch, na.rm = TRUE) * 100, 1) else 0
    heart_weak_pct <- if (sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch, na.rm = TRUE) > 0)
      round(sum(hitter_data$contact_type == "Weak" & heart_pitch, na.rm = TRUE) / 
              sum(hitter_data$pitch_outcome == "Ball In Play" & heart_pitch, na.rm = TRUE) * 100, 1) else 0
    
    # Pressure table all column stats
    if (nrow(hitter_data) > 0 && any(pressure_pitch, na.rm = TRUE)) {
      
      pressure_filter <- pressure_pitch
      bip_filter <- hitter_data$pitch_outcome == "Ball In Play" & pressure_filter
      
      pressure_swing_pct <- round(
        sum(hitter_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_filter, na.rm = TRUE) /
          sum(pressure_filter, na.rm = TRUE) * 100, 1
      )
      
      pressure_miss_pct <- round(
        sum(hitter_data$pitch_outcome == "Swinging Strike" & pressure_filter, na.rm = TRUE) /
          sum(pressure_filter, na.rm = TRUE) * 100, 1
      )
      
      pressure_hard_pct <- if (sum(bip_filter, na.rm = TRUE) > 0) {
        round(
          sum(hitter_data$contact_type == "Hard" & bip_filter, na.rm = TRUE) /
            sum(bip_filter, na.rm = TRUE) * 100, 1
        )
      } else { 0 }
      
      pressure_weak_pct <- if (sum(bip_filter, na.rm = TRUE) > 0) {
        round(
          sum(hitter_data$contact_type == "Weak" & bip_filter, na.rm = TRUE) /
            sum(bip_filter, na.rm = TRUE) * 100, 1
        )
      } else { 0 }
      
    } else {
      pressure_swing_pct <- 0
      pressure_miss_pct <- 0
      pressure_hard_pct <- 0
      pressure_weak_pct <- 0
    }
    
    # Overall table all column stats
    overall_swing_pct <- if (nrow(hitter_data) > 0)
      round(sum(hitter_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / 
              nrow(hitter_data) * 100, 1) else 0
    overall_miss_pct <- if (nrow(hitter_data) > 0)
      round(sum(hitter_data$pitch_outcome == "Swinging Strike", na.rm = TRUE) / nrow(hitter_data) * 100, 1) else 0
    overall_hard_pct <- if (sum(hitter_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(hitter_data$contact_type == "Hard", na.rm = TRUE) / 
              sum(hitter_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    overall_weak_pct <- if (sum(hitter_data$pitch_outcome == "Ball In Play", na.rm = TRUE) > 0)
      round(sum(hitter_data$contact_type == "Weak", na.rm = TRUE) / 
              sum(hitter_data$pitch_outcome == "Ball In Play", na.rm = TRUE) * 100, 1) else 0
    
    
    # RISP Data and Stats for table
    # RISP pressure pitches: runners on 2nd and/or 3rd and pitch is in pressure zone
    risp_pressure_data <- hitter_data %>%
      filter(
        second_base_runner > 10 | third_base_runner > 10,
        (pitch_location_x >= -11.3 & pitch_location_x <= -5.7 |
           pitch_location_x >= 5.7 & pitch_location_x <= 11.3),
        (pitch_location_y >= 14 & pitch_location_y <= 22 |
           pitch_location_y >= 38 & pitch_location_y <= 46)
      )
    
    # RISP Number of at bats
    risp_abs <- at_bats %>% filter(second_base != "None" | third_base != "None")
    
    # Risp BA and OBP
    risp_ba <- formatC(if (nrow(risp_abs) > 0) sum(risp_abs$outcome %in% c("Single","Double","Triple","Homerun"), na.rm = TRUE) / nrow(risp_abs) else 0, format = "f", digits = 3)
    risp_obp <- formatC(if (nrow(risp_abs) > 0) sum(risp_abs$batter_base != "None", na.rm = TRUE) / nrow(risp_abs) else 0, format = "f", digits = 3)
    
    # RISP pressure swing pct
    pressure_pct_risp <- if (nrow(risp_pressure_data) > 0)
      round(sum(risp_pressure_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / 
              nrow(risp_pressure_data) * 100, 1) else 0
    
    # RISP Number of runners that advanced because of the hitter, and the percentage is called success rate
    risp_advance <- risp_abs %>% filter(second_base %in% c("Third", "Scored") | third_base == "Scored")
    success_rate_risp <- if (nrow(risp_abs) > 0) round(nrow(risp_advance) / nrow(risp_abs) * 100, 1) else 0
    
    
    # RISP Data and Stats for table
    # No RISP pressure pitches: no runners on 2nd or 3rd and pitch is in pressure zone
    no_risp_pressure_data <- hitter_data %>%
      filter(
        is.na(second_base_runner) & is.na(third_base_runner),
        (pitch_location_x >= -11.3 & pitch_location_x <= -5.7 |
           pitch_location_x >= 5.7 & pitch_location_x <= 11.3),
        (pitch_location_y >= 14 & pitch_location_y <= 22 |
           pitch_location_y >= 38 & pitch_location_y <= 46)
      )
    
    # NO RISP Number of at bats
    no_risp_abs <- at_bats %>% filter(second_base == "None" & third_base == "None")
    
    # No Risp BA and OBP
    no_risp_ba <- formatC(if (nrow(no_risp_abs) > 0) sum(no_risp_abs$outcome %in% c("Single","Double","Triple","Homerun"), na.rm = TRUE) / nrow(no_risp_abs) else 0, format = "f", digits = 3)
    no_risp_obp <- formatC(if (nrow(no_risp_abs) > 0) sum(no_risp_abs$batter_base != "None", na.rm = TRUE) / nrow(no_risp_abs) else 0, format = "f", digits = 3)
    
    # NO RISP pressure swing pct
    pressure_pct_no_risp <- if (nrow(no_risp_pressure_data) > 0)
      round(sum(no_risp_pressure_data$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball"), na.rm = TRUE) / 
              nrow(no_risp_pressure_data) * 100, 1) else 0
    
    # NO RISP Number of runners that advanced because of the hitter, and the percentage is called success rate
    no_risp_advance <- no_risp_abs %>% filter(first_base %in% c("Second", "Third", "Scored"))
    success_rate_no_risp <- if (nrow(no_risp_abs) > 0) round(nrow(no_risp_advance) / nrow(no_risp_abs) * 100, 1) else 0
    
    
    # Splitting up lhp and rhp stats by at bats ending in early or two strike counts
    lhp_early <- lhp_data %>% filter(strikes < 2, pitch_outcome != "None")
    rhp_early <- rhp_data %>% filter(strikes < 2, pitch_outcome != "None")
    lhp_two_strike <- lhp_data %>% filter(strikes >= 2, pitch_outcome != "None")
    rhp_two_strike <- rhp_data %>% filter(strikes >= 2, pitch_outcome != "None")
    
    
    # Splitting Plate Appearances Data by Pitcher Handedness and Early vs Two strike
    lhp_early_pa <- plate_appearances %>% 
      filter(pitcher_side == "Left") %>%
      filter(strikes < 2)
    rhp_early_pa <- plate_appearances %>% 
      filter(pitcher_side == "Right") %>%
      filter(strikes < 2)
    lhp_two_strike_pa <- plate_appearances %>% 
      filter(pitcher_side == "Left") %>%
      filter(strikes >= 2)
    rhp_two_strike_pa <- plate_appearances %>% 
      filter(pitcher_side == "Right") %>%
      filter(strikes >= 2)
    
    # Splitting At Bat Data by Pitcher Handedness and Early vs Two strike
    lhp_early_at_bats <- at_bats %>% 
      filter(pitcher_side == "Left") %>%
      filter(strikes < 2)
    rhp_early_at_bats <- at_bats %>% 
      filter(pitcher_side == "Right") %>%
      filter(strikes < 2)
    lhp_two_strike_at_bats <- at_bats %>% 
      filter(pitcher_side == "Left") %>%
      filter(strikes >= 2)
    rhp_two_strike_at_bats <- at_bats %>% 
      filter(pitcher_side == "Right") %>%
      filter(strikes >= 2)
    
    # Putting all calculated stats into a list to call them cleanly
    stats <- list(
      total_abs = total_abs,
      ba = ba,
      obp = obp,
      slg = slg,
      lob_per = lob,
      qab_score = qab_hitter,
      heart = list(
        swing_pct = heart_swing_pct,
        miss_pct = heart_miss_pct,
        hard_pct = heart_hard_pct,
        weak_pct = heart_weak_pct,
        lhp_swing_pct = lhp_heart_swing_pct,
        lhp_miss_pct = lhp_heart_miss_pct,
        lhp_hard_pct = lhp_heart_hard_pct,
        lhp_weak_pct = lhp_heart_weak_pct,
        rhp_swing_pct = rhp_heart_swing_pct,
        rhp_miss_pct = rhp_heart_miss_pct,
        rhp_hard_pct = rhp_heart_hard_pct,
        rhp_weak_pct = rhp_heart_weak_pct
      ),
      pressure = list(
        swing_pct = pressure_swing_pct,
        miss_pct = pressure_miss_pct,
        hard_pct = pressure_hard_pct,
        weak_pct = pressure_weak_pct,
        lhp_swing_pct = lhp_pressure_swing_pct,
        lhp_miss_pct = lhp_pressure_miss_pct,
        lhp_hard_pct = lhp_pressure_hard_pct,
        lhp_weak_pct = lhp_pressure_weak_pct,
        rhp_swing_pct = rhp_pressure_swing_pct,
        rhp_miss_pct = rhp_pressure_miss_pct,
        rhp_hard_pct = rhp_pressure_hard_pct,
        rhp_weak_pct = rhp_pressure_weak_pct
      ),
      overall = list(
        swing_pct = overall_swing_pct,
        miss_pct = overall_miss_pct,
        hard_pct = overall_hard_pct,
        weak_pct = overall_weak_pct,
        lhp_swing_pct = lhp_overall_swing_pct,
        lhp_miss_pct = lhp_overall_miss_pct,
        lhp_hard_pct = lhp_overall_hard_pct,
        lhp_weak_pct = lhp_overall_weak_pct,
        rhp_swing_pct = rhp_overall_swing_pct,
        rhp_miss_pct = rhp_overall_miss_pct,
        rhp_hard_pct = rhp_overall_hard_pct,
        rhp_weak_pct = rhp_overall_weak_pct
      ),
      risp = list(
        abs = nrow(risp_abs),
        ba = risp_ba,
        obp = risp_obp,
        pressure_swing_pct = pressure_pct_risp,
        success_rate = success_rate_risp
      ),
      no_risp = list(
        abs = nrow(no_risp_abs),
        ba = no_risp_ba,
        obp = no_risp_obp,
        pressure_pct = pressure_pct_no_risp,
        success_rate = success_rate_no_risp
      ),
      lhp = list(
        early = list(
          pitches = nrow(lhp_early),
          ab = nrow(lhp_early_at_bats),
          obp = sprintf("%.3f", if (nrow(lhp_early_pa) > 0) 
            round(sum(!lhp_early_pa$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / sum(lhp_early_pa$batter_base != "Cage Ball", na.rm = TRUE), 3) else 0),
          hard_hit_pct = {
            denom <- sum(lhp_early_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            num <- sum(lhp_early_pa$contact_type == "Hard", na.rm = TRUE)
            if (denom > 0) round(num / denom * 100, 1) else 0
          },
          weak_hit_pct = {
            denom <- sum(lhp_early_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            num <- sum(lhp_early_pa$contact_type == "Weak", na.rm = TRUE)
            if (denom > 0) round(num / denom * 100, 1) else 0
          },
          strike_take_pct = if (nrow(lhp_early) > 0) 
            round(sum(lhp_early$pitch_outcome == "Called Strike", na.rm = TRUE) / sum(lhp_early$pitch_outcome != "None", na.rm = TRUE) * 100, 1) else 0,
          pressure_pct = if (sum(lhp_early$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_early$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) / 
                    sum(pressure_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_take_pct = if (sum(lhp_early$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_early$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_barrel_pct = if (sum(lhp_early$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_early$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0
        ),
        two_strike = list(
          pitches = nrow(lhp_two_strike),
          ab = nrow(lhp_two_strike_at_bats),
          obp = sprintf("%.3f", if (nrow(lhp_two_strike_pa) > 0) 
            round(sum(!lhp_two_strike_pa$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / sum(lhp_two_strike_pa$batter_base != "Cage Ball", na.rm = TRUE), 3) else 0),
          hard_hit_pct = {
            denom <- sum(lhp_two_strike_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(lhp_two_strike_pa$contact_type == "Hard", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          weak_hit_pct = {
            denom <- sum(lhp_two_strike_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(lhp_two_strike_pa$contact_type == "Weak", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          strike_take_pct = if (nrow(lhp_two_strike) > 0) 
            round(sum(lhp_two_strike$pitch_outcome == "Called Strike", na.rm = TRUE) / sum(lhp_two_strike$pitch_outcome != "None", na.rm = TRUE) * 100, 1) else 0,
          pressure_pct = if (sum(lhp_two_strike$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_two_strike$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) / 
                    sum(pressure_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_take_pct = if (sum(lhp_two_strike$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_two_strike$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_barrel_pct = if (sum(lhp_two_strike$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(lhp_two_strike$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0
        )
      ),
      rhp = list(
        early = list(
          pitches = nrow(rhp_early),
          ab = nrow(rhp_early_at_bats),
          obp = sprintf("%.3f", if (nrow(rhp_early_pa) > 0) 
            round(sum(!rhp_early_pa$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / sum(rhp_early_pa$batter_base != "Cage Ball", na.rm = TRUE), 3) else 0),
          hard_hit_pct = {
            denom <- sum(rhp_early_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(rhp_early_pa$contact_type == "Hard", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          weak_hit_pct = {
            denom <- sum(rhp_early_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(rhp_early_pa$contact_type == "Weak", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          strike_take_pct = if (nrow(rhp_early) > 0) 
            round(sum(rhp_early$pitch_outcome == "Called Strike", na.rm = TRUE) / sum(rhp_early$pitch_outcome != "None", na.rm = TRUE) * 100, 1) else 0,
          pressure_pct = if (sum(rhp_early$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_early$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) / 
                    sum(pressure_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_take_pct = if (sum(rhp_early$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_early$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_barrel_pct = if (sum(rhp_early$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_early$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0
        ),
        two_strike = list(
          pitches = nrow(rhp_two_strike),
          ab = nrow(rhp_two_strike_at_bats),
          obp = sprintf("%.3f", if (nrow(rhp_two_strike_pa) > 0) 
            round(sum(!rhp_two_strike_pa$batter_base %in% c("None", "Cage Ball"), na.rm = TRUE) / sum(rhp_two_strike_pa$batter_base != "Cage Ball", na.rm = TRUE), 3) else 0),
          hard_hit_pct = {
            denom <- sum(rhp_two_strike_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(rhp_two_strike_pa$contact_type == "Hard", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          weak_hit_pct = {
            denom <- sum(rhp_two_strike_pa$pitch_outcome == "Ball In Play", na.rm = TRUE)
            if (denom > 0) round(
              sum(rhp_two_strike_pa$contact_type == "Weak", na.rm = TRUE) / denom * 100, 1
            ) else 0
          },
          strike_take_pct = if (nrow(rhp_two_strike) > 0) 
            round(sum(rhp_two_strike$pitch_outcome == "Called Strike", na.rm = TRUE) / sum(rhp_two_strike$pitch_outcome != "None", na.rm = TRUE) * 100, 1) else 0,
          pressure_pct = if (sum(rhp_two_strike$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_two_strike$pitch_outcome %in% c("Swinging Strike", "Foul Ball", "Ball In Play", "Cage Ball") & pressure_pitch_left, na.rm = TRUE) / 
                    sum(pressure_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_take_pct = if (sum(rhp_two_strike$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_two_strike$pitch_outcome == "Called Strike" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0,
          heart_barrel_pct = if (sum(rhp_two_strike$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) > 0) 
            round(sum(rhp_two_strike$contact_type == "Hard" & heart_pitch_left, na.rm = TRUE) / 
                    sum(heart_pitch_left, na.rm = TRUE) * 100, 1) else 0
        )
      )
    )
    return(stats)
  }
  
  # Hitter Report UI - this shows up in the app
  hitterReportUI <- function(stats, hitter_id) {
    tagList(
      fluidRow(
        column(5,
               h4("LHP", style = "text-align: center;"),
               plotOutput(paste0("hitter_spray_chart_LHP", hitter_id), height = "300px", width = "300px")
        ),
        column(2,
               h3(stats$full_name, style = "text-align: center;"),
               p(paste("#", stats$jersey_number)),
               tags$table(
                 style = "border-collapse: collapse; text-align: left;",
                 tags$tr(
                   tags$th("Total ABs: "),
                   tags$td(stats$total_abs)
                 ),
                 tags$tr(
                   tags$th("OBP: "),
                   tags$td(stats$obp)
                 ),
                 tags$tr(
                   tags$th("BA: "),
                   tags$td(stats$ba)
                 ),
                 tags$tr(
                   tags$th("SLG: "),
                   tags$td(stats$slg)
                 ),
                 tags$tr(
                   tags$th("QAB Score: "),
                   tags$td(stats$qab_score)
                 ),
                 tags$tr(
                   tags$th("LOB %: "),
                   tags$td(stats$lob)
                 )
               )
        ),
        column(5,
               h4("RHP", style = "text-align: center;"),
               plotOutput(paste0("hitter_spray_chart_RHP", hitter_id), height = "300px", width = "300px")
        )
      ),
      tags$div(style = "margin-bottom: 70px;"),
      fluidRow(
        column(12,
               div(
                 style = "display: flex; align-items: flex-start;",
                 # Metric labels
                 div(
                   style = "width: 150px; margin-right: 10px;",
                   tags$div(
                     style = "padding-top: 65px;",
                     lapply(c("Swing %", "Miss %", "Hard Hit %", "Weak Hit %"),
                            function(metric) {
                              tags$p(
                                style = "margin: 0; padding: 5px 0; padding-bottom: 24px; text-align: right; line-height: 26px; height: 26px; font-size: 14px;",
                                metric
                              )
                            })
                   )
                 ),
                 # 
                 div(
                   style = "flex-grow: 1;",
                   fluidRow( # Table for all of stats pitches in the Heart zone
                     column(4,
                            h4("Heart", style = "text-align: center;"),
                            tags$table(
                              style = "width: 100%; border-collapse: collapse; text-align: center;",
                              tags$tr(
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "LHP"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "ALL"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "RHP")
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$lhp_swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$rhp_swing_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$lhp_miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$rhp_miss_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$lhp_hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$rhp_hard_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$lhp_weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$heart$rhp_weak_pct)
                              )
                            )
                     ),
                     column(4, # Table for all of stats pitches in the Pressure zone
                            h4("Pressure", style = "text-align: center;"),
                            tags$table(
                              style = "width: 100%; border-collapse: collapse; text-align: center;",
                              tags$tr(
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "LHP"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "ALL"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "RHP")
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$lhp_swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$rhp_swing_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$lhp_miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$rhp_miss_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$lhp_hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$rhp_hard_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$lhp_weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$pressure$rhp_weak_pct)
                              )
                            )
                     ),
                     column(4, # Table for stats of all of the pitches
                            h4("Overall", style = "text-align: center;"),
                            tags$table(
                              style = "width: 100%; border-collapse: collapse; text-align: center;",
                              tags$tr(
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "LHP"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "ALL"),
                                tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "RHP")
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$lhp_swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$swing_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$rhp_swing_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$lhp_miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$miss_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$rhp_miss_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$lhp_hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$hard_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$rhp_hard_pct)
                              ),
                              tags$tr(
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$lhp_weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$weak_pct),
                                tags$td(style = "border: 1px solid black; padding: 4px;", stats$overall$rhp_weak_pct)
                              )
                            )
                     )
                   )
                 )
               )
               
        )
      ),
      fluidRow( # RISP and No RISP data table
        column(5,
               h4("With Runners", style = "text-align: center;"),
               tags$table(
                 style = "width: 100%; border-collapse: collapse; text-align: center;",
                 tags$tr(
                   tags$th(),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "RISP"),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "NO RISP")
                 ),
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px;", "Total AB's"),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$risp$abs),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$no_risp$abs)
                 ),
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px;", "BA"),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$risp$ba),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$no_risp$ba)
                 ),
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px;", "OBP"),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$risp$obp),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$no_risp$obp)
                 ),
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px;", "Pressure Swing %"),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$risp$pressure_swing_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$no_risp$pressure_pct)
                 ),
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px;", "Success %"),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$risp$success_rate),
                   tags$td(style = "border: 1px solid black; padding: 4px;", stats$no_risp$success_rate)
                 )
               ),
               h4("Heat Map", style = "text-align: center;"),
               div(style="text-align: center;",
                   div(style="display: inline-block;",
                       plotOutput(paste0("player_heatmap_ui_", hitter_id), height="330px", width="280px")
                   )
               )
        ),
        column(7, # Big table dividing stats of characteristics of pitcher throwing hand and what type of cound the pitch happened in
               h4("Difference in Counts", style = "text-align: center;"),
               tags$table(
                 style = "width: 100%; border-collapse: collapse; text-align: center;",
                 tags$tr(
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "Before 2 strikes"),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "VS"),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "With 2 strikes")
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", paste(stats$rhp$early$pitches), " Pitches Seen"),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "RHP"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", paste(stats$rhp$two_strike$pitches), " Pitches Seen")
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$ab),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "# of AB's"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$ab)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$obp),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "OBP"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$obp)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$hard_hit_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Hard Hit %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$hard_hit_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$weak_hit_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Weak Hit %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$weak_hit_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$strike_take_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Strike Take %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$strike_take_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$pressure_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Pressure Swing %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$pressure_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$heart_take_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Heart Zone Take %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$heart_take_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$early$heart_barrel_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Heart Zone Barrel %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$rhp$two_strike$heart_barrel_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", paste(stats$lhp$early$pitches), " Pitches Seen"),
                   tags$th(style = "border: 1px solid black; padding: 4px; text-align: center;", "LHP"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", paste(stats$lhp$two_strike$pitches), " Pitches Seen")
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$ab),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "# of AB's"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$ab)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$obp),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "OBP"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$obp)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$hard_hit_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Hard Hit %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$hard_hit_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$weak_hit_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Weak Hit %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$weak_hit_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$strike_take_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Strike Take %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$strike_take_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$pressure_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Pressure Swing %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$pressure_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$heart_take_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Heart Zone Take %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$heart_take_pct)
                 ),
                 tags$tr(
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$early$heart_barrel_pct),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", "Heart Zone Barrel %"),
                   tags$td(style = "border: 1px solid black; padding: 4px; text-align: center;", stats$lhp$two_strike$heart_barrel_pct)
                 )
               )
        )
      )
    )
  }
  
  # Hitter Report Rendering
  output$hitter_report_output <- renderUI({
    message("Rendering hitter_report_output")
    req(input$hitter_players)
    message("Selected hitters for report: ", paste(input$hitter_players, collapse = ", "))
    
    hitter_ids <- stored_players$data %>% 
      filter(full_name %in% input$hitter_players) %>% 
      pull(player_id)
    message("Hitter IDs for report: ", paste(hitter_ids, collapse = ", "))
    
    tagList(
      lapply(hitter_ids, function(hid) {
        message("Generating hitterReportUI for hitter ID: ", hid)
        player_data <- stored_players$data %>% 
          filter(player_id == hid) %>% 
          slice(1)
        stats <- calculate_hitter_stats(hid)
        if (is.null(stats)) {
          stats <- list(
            full_name = player_data$full_name,  # Pass through from player_data
            jersey_number = player_data$jersey_number,
            total_abs = "No Data", obp = "No Data", ba = "No Data", slg = "No Data", qab_score = "No Data", lob = "No Data",
            heart = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data"),
            pressure = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data"),
            overall = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data"),
            risp = list(abs = "No Data", ba = "No Data", obp = "No Data", pressure_pct = "No Data", success_rate = "No Data"),
            no_risp = list(abs = "No Data", ba = "No Data", obp = "No Data", pressure_pct = "No Data", success_rate = "No Data"),
            lhp = list(
              early = list(ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", strike_take_pct = "No Data", 
                           pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data"),
              two_strike = list(ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", strike_take_pct = "No Data", 
                                pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data")
            ),
            rhp = list(
              early = list(ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", strike_take_pct = "No Data", 
                           pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data"),
              two_strike = list(ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", strike_take_pct = "No Data", 
                                pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data")
            )
          )
        } else {
          # ADD: Combine player_data with stats for passing to UI
          stats$full_name <- player_data$full_name
          stats$jersey_number <- player_data$jersey_number
        }
        hitterReportUI(stats, hitter_id = hid)
      })
    )
  })
  
  # Render hitters plots, spray charts and heat map
  observe({
    pitch_data <- global_pitch_data()
    
    # Debug hitter data
    hitter_ids <- stored_players$data %>%
      filter(full_name %in% input$hitter_players) %>%
      pull(player_id)
    for (hid in hitter_ids) {
      hitter_spray <- pitch_data %>%
        filter(hitter_id == hid, ball_in_play_outcome %in% c("Single", "Double", "Triple", "Homerun"))
      message("Hitter ID: ", hid, " - Rows with hits: ", nrow(hitter_spray))
      message("Hitter ID: ", hid, " - Non-NA spray_chart_x/y rows: ", 
              nrow(hitter_spray %>% filter(!is.na(spray_chart_x), !is.na(spray_chart_y))))
    }
    
    # Hitter spray chart rendering (with debug message)
    for (hid in hitter_ids) {
      local({
        hid_local <- hid
        output[[paste0("hitter_spray_chart_LHP", hid_local)]] <- renderPlot({
          message("Rendering hitter spray chart for ID: ", hid_local)
          generate_spray_chart(hid_local, role = "hitter", pitch_data = pitch_data, player_side = "LHP")
        }, height = 300, width = 350)
        output[[paste0("hitter_spray_chart_RHP", hid_local)]] <- renderPlot({
          message("Rendering hitter spray chart for ID: ", hid_local)
          generate_spray_chart(hid_local, role = "hitter", pitch_data = pitch_data, player_side = "RHP")
        }, height = 300, width = 350)
        
        output[[paste0("player_heatmap_ui_", hid_local)]] <- renderPlot({
          message("Rendering hitter heatmap for ID: ", hid_local)
          message("Pitch data rows for heatmap before filtering: ", nrow(pitch_data))
          generate_strikezone_heatmap(hid_local, pitch_data = pitch_data)
        }, height = 330, width = 280)
      })
    }
  })
  
  # After clicking the download button, this is what runs to create the one page pdf's of our outputs
  output$export_hitter_reports <- downloadHandler(
    filename = function() { paste("Hitter_Reports_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      req(input$hitter_players)
      hitter_ids <- stored_players$data %>% 
        filter(full_name %in% input$hitter_players) %>% 
        pull(player_id)
      
      # Temporary directory for images
      temp_dir <- tempdir()
      temp_html <- file.path(temp_dir, "hitter_report.html")
      
      # Generate HTML content
      html_content <- lapply(hitter_ids, function(hid) {
        player_data <- stored_players$data %>% 
          filter(player_id == hid) %>% 
          slice(1)
        stats <- calculate_hitter_stats(hid) %||% list(
          total_abs = "No Data", obp = "No Data", ba = "No Data", slg = "No Data", qab_score = "No Data", lob = "No Data",
          heart = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data",
                       lhp_swing_pct = "No Data", lhp_miss_pct = "No Data", lhp_hard_pct = "No Data", lhp_weak_pct = "No Data",
                       rhp_swing_pct = "No Data", rhp_miss_pct = "No Data", rhp_hard_pct = "No Data", rhp_weak_pct = "No Data"),
          pressure = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data",
                          lhp_swing_pct = "No Data", lhp_miss_pct = "No Data", lhp_hard_pct = "No Data", lhp_weak_pct = "No Data",
                          rhp_swing_pct = "No Data", rhp_miss_pct = "No Data", rhp_hard_pct = "No Data", rhp_weak_pct = "No Data"),
          overall = list(swing_pct = "No Data", miss_pct = "No Data", hard_pct = "No Data", weak_pct = "No Data",
                         lhp_swing_pct = "No Data", lhp_miss_pct = "No Data", lhp_hard_pct = "No Data", lhp_weak_pct = "No Data",
                         rhp_swing_pct = "No Data", rhp_miss_pct = "No Data", rhp_hard_pct = "No Data", rhp_weak_pct = "No Data"),
          risp = list(abs = "No Data", ba = "No Data", obp = "No Data", pressure_swing_pct = "No Data", success_rate = "No Data"),
          no_risp = list(abs = "No Data", ba = "No Data", obp = "No Data", pressure_pct = "No Data", success_rate = "No Data"),
          lhp = list(
            early = list(pitches = "No Data", ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", 
                         strike_take_pct = "No Data", pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data"),
            two_strike = list(pitches = "No Data", ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", 
                              strike_take_pct = "No Data", pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data")
          ),
          rhp = list(
            early = list(pitches = "No Data", ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", 
                         strike_take_pct = "No Data", pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data"),
            two_strike = list(pitches = "No Data", ab = "No Data", obp = "No Data", hard_hit_pct = "No Data", weak_hit_pct = "No Data", 
                              strike_take_pct = "No Data", pressure_pct = "No Data", heart_take_pct = "No Data", heart_barrel_pct = "No Data")
          )
        )
        
        # Render plots as PNGs and encode as base64
        temp_heatmap <- file.path(temp_dir, paste0("heatmap_", hid, ".png"))
        temp_spray_lhp <- file.path(temp_dir, paste0("spray_chart_lhp_", hid, ".png"))
        temp_spray_rhp <- file.path(temp_dir, paste0("spray_chart_rhp_", hid, ".png"))
        
        heatmap <- tryCatch({
          plot <- generate_strikezone_heatmap(hid, pitch_data = global_pitch_data())
          if (!is.null(plot)) plot else NULL
        }, error = function(e) NULL)
        
        spray_chart_lhp <- tryCatch({
          plot <- generate_spray_chart(hid, role = "hitter", pitch_data = global_pitch_data(), player_side = "LHP")
          if (!is.null(plot)) plot else NULL
        }, error = function(e) NULL)
        
        spray_chart_rhp <- tryCatch({
          plot <- generate_spray_chart(hid, role = "hitter", pitch_data = global_pitch_data(), player_side = "RHP")
          if (!is.null(plot)) plot else NULL
        }, error = function(e) NULL)
        
        heatmap_base64 <- if (!is.null(heatmap)) {
          ggsave(temp_heatmap, plot = heatmap, width = 2.67, height = 3.20, dpi = 300) # 200x240px
          base64enc::base64encode(temp_heatmap)
        } else {
          png(temp_heatmap, width = 200, height = 240, units = "px", res = 72)
          plot.new()
          text(0.5, 0.5, "No heatmap data", cex = 1)
          dev.off()
          base64enc::base64encode(temp_heatmap)
        }
        
        spray_lhp_base64 <- if (!is.null(spray_chart_lhp)) {
          ggsave(temp_spray_lhp, plot = spray_chart_lhp, width = 3.20, height = 3.20, dpi = 300) # 240x240px
          base64enc::base64encode(temp_spray_lhp)
        } else {
          png(temp_spray_lhp, width = 240, height = 240, units = "px", res = 72)
          plot.new()
          text(0.5, 0.5, "No LHP spray chart data", cex = 1)
          dev.off()
          base64enc::base64encode(temp_spray_lhp)
        }
        
        spray_rhp_base64 <- if (!is.null(spray_chart_rhp)) {
          ggsave(temp_spray_rhp, plot = spray_chart_rhp, width = 3.20, height = 3.20, dpi = 300) # 240x240px
          base64enc::base64encode(temp_spray_rhp)
        } else {
          png(temp_spray_rhp, width = 240, height = 240, units = "px", res = 72)
          plot.new()
          text(0.5, 0.5, "No RHP spray chart data", cex = 1)
          dev.off()
          base64enc::base64encode(temp_spray_rhp)
        }
        
        # Construct HTML to match hitterReportUI
        html <- paste0(
          '<div class="row">',
          '  <div class="column" style="width: 41.67%; padding: 3px;">',
          '    <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">LHP</h4>',
          '    <img src="data:image/png;base64,', spray_lhp_base64, '" width="240px" height="240px" style="display: block; margin: auto;"/>',
          '  </div>',
          '  <div class="column" style="width: 16.67%; padding: 3px;">',
          '    <h3 style="text-align: center; font-size: 14px; margin: 5px 0;">', player_data$full_name, '</h3>',
          '    <p style="text-align: center; font-size: 10px; margin: 5px 0;">#', player_data$jersey_number, '</p>',
          '    <table class="stats-table" style="width: 100%; border-collapse: collapse; text-align: left; font-size: 10px;">',
          '      <tr><th style="padding: 2px;">Total ABs:</th><td style="padding: 2px;">', stats$total_abs, '</td></tr>',
          '      <tr><th style="padding: 2px;">OBP:</th><td style="padding: 2px;">', stats$obp, '</td></tr>',
          '      <tr><th style="padding: 2px;">BA:</th><td style="padding: 2px;">', stats$ba, '</td></tr>',
          '      <tr><th style="padding: 2px;">SLG:</th><td style="padding: 2px;">', stats$slg, '</td></tr>',
          '      <tr><th style="padding: 2px;">QAB Score:</th><td style="padding: 2px;">', stats$qab_score, '</td></tr>',
          '      <tr><th style="padding: 2px;">LOB %:</th><td style="padding: 2px;">', stats$lob, '</td></tr>',
          '    </table>',
          '  </div>',
          '  <div class="column" style="width: 41.67%; padding: 3px;">',
          '    <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">RHP</h4>',
          '    <img src="data:image/png;base64,', spray_rhp_base64, '" width="240px" height="240px" style="display: block; margin: auto;"/>',
          '  </div>',
          '</div>',
          '<div style="margin-bottom: 10px;"></div>',
          '<div class="row">',
          '  <div class="column" style="width: 100%;">',
          '    <div style="display: flex; align-items: flex-start;">',
          '      <div style="width: 150px; margin-right: 5px;">',
          '        <table style="width: 100%; border-collapse: collapse; font-size: 10px; margin-top: 22px;">',
          '          <tr><td style="height: 20px;"></td></tr>',  # Empty row to align with column headers
          '          <tr><td style="padding: 4px 0; text-align: right; height: 20px; line-height: 20px;">Swing %</td></tr>',
          '          <tr><td style="padding: 4px 0; text-align: right; height: 20px; line-height: 20px;">Miss %</td></tr>',
          '          <tr><td style="padding: 4px 0; text-align: right; height: 20px; line-height: 20px;">Hard Hit %</td></tr>',
          '          <tr><td style="padding: 4px 0; text-align: right; height: 20px; line-height: 20px;">Weak Hit %</td></tr>',
          '        </table>',
          '      </div>',
          '      <div style="flex-grow: 1;">',
          '        <div class="row">',
          '          <div class="column" style="width: 33.33%; padding: 3px;">',
          '            <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">Heart</h4>',
          '            <table class="data-table" style="width: 100%; border-collapse: collapse; text-align: center; font-size: 10px;">',
          '              <tr><th style="border: 1px solid black; padding: 4px;">LHP</th><th style="border: 1px solid black; padding: 4px;">ALL</th><th style="border: 1px solid black; padding: 4px;">RHP</th></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$lhp_swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$rhp_swing_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$lhp_miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$rhp_miss_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$lhp_hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$rhp_hard_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$lhp_weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$heart$rhp_weak_pct, '</td></tr>',
          '            </table>',
          '          </div>',
          '          <div class="column" style="width: 33.33%; padding: 3px;">',
          '            <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">Pressure</h4>',
          '            <table class="data-table" style="width: 100%; border-collapse: collapse; text-align: center; font-size: 10px;">',
          '              <tr><th style="border: 1px solid black; padding: 4px;">LHP</th><th style="border: 1px solid black; padding: 4px;">ALL</th><th style="border: 1px solid black; padding: 4px;">RHP</th></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$lhp_swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$rhp_swing_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$lhp_miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$rhp_miss_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$lhp_hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 18px;">', stats$pressure$hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 18px;">', stats$pressure$rhp_hard_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$pressure$lhp_weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 18px;">', stats$pressure$weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 18px;">', stats$pressure$rhp_weak_pct, '</td></tr>',
          '            </table>',
          '          </div>',
          '          <div class="column" style="width: 33.33%; padding: 3px;">',
          '            <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">Overall</h4>',
          '            <table class="data-table" style="width: 100%; border-collapse: collapse; text-align: center; font-size: 10px;">',
          '              <tr><th style="border: 1px solid black; padding: 4px;">LHP</th><th style="border: 1px solid black; padding: 4px;">ALL</th><th style="border: 1px solid black; padding: 4px;">RHP</th></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$lhp_swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$swing_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$rhp_swing_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$lhp_miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$miss_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$rhp_miss_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$lhp_hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$hard_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$rhp_hard_pct, '</td></tr>',
          '              <tr><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$lhp_weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$weak_pct, '</td><td style="border: 1px solid black; padding: 4px; height: 20px;">', stats$overall$rhp_weak_pct, '</td></tr>',
          '            </table>',
          '          </div>',
          '        </div>',
          '      </div>',
          '    </div>',
          '  </div>',
          '</div>',
          '<div style="margin-bottom: 5px;"></div>',
          '<div class="row">',
          '  <div class="column" style="width: 41.67%; padding: 3px;">',
          '    <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">With Runners</h4>',
          '    <table class="data-table" style="width: 100%; border-collapse: collapse; text-align: center; font-size: 10px;">',
          '      <tr><th style="border: 1px solid black; padding: 4px;"></th><th style="border: 1px solid black; padding: 4px;">RISP</th><th style="border: 1px solid black; padding: 4px;">NO RISP</th></tr>',
          '      <tr><th style="border: 1px solid black; padding: 4px;">Total ABs</th><td style="border: 1px solid black; padding: 4px;">', stats$risp$abs, '</td><td style="border: 1px solid black; padding: 4px;">', stats$no_risp$abs, '</td></tr>',
          '      <tr><th style="border: 1px solid black; padding: 4px;">BA</th><td style="border: 1px solid black; padding: 4px;">', stats$risp$ba, '</td><td style="border: 1px solid black; padding: 4px;">', stats$no_risp$ba, '</td></tr>',
          '      <tr><th style="border: 1px solid black; padding: 4px;">OBP</th><td style="border: 1px solid black; padding: 4px;">', stats$risp$obp, '</td><td style="border: 1px solid black; padding: 4px;">', stats$no_risp$obp, '</td></tr>',
          '      <tr><th style="border: 1px solid black; padding: 4px;">Pressure Swing %</th><td style="border: 1px solid black; padding: 4px;">', stats$risp$pressure_swing_pct, '</td><td style="border: 1px solid black; padding: 4px;">', stats$no_risp$pressure_pct, '</td></tr>',
          '      <tr><th style="border: 1px solid black; padding: 4px;">Success %</th><td style="border: 1px solid black; padding: 4px;">', stats$risp$success_rate, '</td><td style="border: 1px solid black; padding: 4px;">', stats$no_risp$success_rate, '</td></tr>',
          '    </table>',
          '    <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">Heat Map</h4>',
          '    <div style="text-align: center;">',
          '      <div style="display: inline-block;">',
          '        <img src="data:image/png;base64,', heatmap_base64, '" width="200px" height="240px" style="display: block; margin: auto;"/>',
          '      </div>',
          '    </div>',
          '  </div>',
          '  <div class="column" style="width: 58.33%; padding: 3px;">',
          '    <h4 style="text-align: center; font-size: 12px; margin: 5px 0;">Difference in Counts</h4>',
          '    <table class="data-table" style="width: 100%; border-collapse: collapse; text-align: center; font-size: 10px;">',
          '      <tr><th style="border: 1px solid black; padding: 4px;">Before 2 Strikes</th><th style="border: 1px solid black; padding: 4px;">VS</th><th style="border: 1px solid black; padding: 4px;">With 2 Strikes</th></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$pitches, ' Pitches Seen</td><th style="border: 1px solid black; padding: 4px;">RHP</th><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$pitches, ' Pitches Seen</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$ab, '</td><td style="border: 1px solid black; padding: 4px;"># of ABs</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$ab, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$obp, '</td><td style="border: 1px solid black; padding: 4px;">OBP</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$obp, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$hard_hit_pct, '</td><td style="border: 1px solid black; padding: 4px;">Hard Hit %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$hard_hit_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$weak_hit_pct, '</td><td style="border: 1px solid black; padding: 4px;">Weak Hit %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$weak_hit_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$strike_take_pct, '</td><td style="border: 1px solid black; padding: 4px;">Strike Take %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$strike_take_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$pressure_pct, '</td><td style="border: 1px solid black; padding: 4px;">Pressure %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$pressure_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$heart_take_pct, '</td><td style="border: 1px solid black; padding: 4px;">Heart Zone Take %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$heart_take_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$rhp$early$heart_barrel_pct, '</td><td style="border: 1px solid black; padding: 4px;">Heart Zone Barrel %</td><td style="border: 1px solid black; padding: 4px;">', stats$rhp$two_strike$heart_barrel_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$pitches, ' Pitches Seen</td><th style="border: 1px solid black; padding: 4px;">LHP</th><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$pitches, ' Pitches Seen</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$ab, '</td><td style="border: 1px solid black; padding: 4px;"># of ABs</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$ab, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$obp, '</td><td style="border: 1px solid black; padding: 4px;">OBP</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$obp, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$hard_hit_pct, '</td><td style="border: 1px solid black; padding: 4px;">Hard Hit %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$hard_hit_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$weak_hit_pct, '</td><td style="border: 1px solid black; padding: 4px;">Weak Hit %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$weak_hit_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$strike_take_pct, '</td><td style="border: 1px solid black; padding: 4px;">Strike Take %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$strike_take_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$pressure_pct, '</td><td style="border: 1px solid black; padding: 4px;">Pressure %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$pressure_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$heart_take_pct, '</td><td style="border: 1px solid black; padding: 4px;">Heart Zone Take %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$heart_take_pct, '</td></tr>',
          '      <tr><td style="border: 1px solid black; padding: 4px;">', stats$lhp$early$heart_barrel_pct, '</td><td style="border: 1px solid black; padding: 4px;">Heart Zone Barrel %</td><td style="border: 1px solid black; padding: 4px;">', stats$lhp$two_strike$heart_barrel_pct, '</td></tr>',
          '    </table>',
          '  </div>',
          '</div>'
        )
        
        return(html)
      })
      
      # Create full HTML with CSS to match hitterReportUI
      html_full <- tagList(
        tags$head(
          tags$style(HTML("
          body { font-family: Arial, sans-serif; margin: 10px; font-size: 10px; }
          h3 { color: #333; font-size: 14px; margin: 5px 0; }
          h4 { color: #333; font-size: 12px; margin: 5px 0; }
          table { border-collapse: collapse; }
          th, td { padding: 4px; }
          .row { display: flex; flex-wrap: nowrap; margin-bottom: 10px; }
          .column { padding: 3px; }
          img { max-width: 100%; height: auto; }
          p { margin: 5px 0; }
          .stats-table th, .stats-table td { border: none; }
          .data-table th, .data-table td { border: 1px solid black; text-align: center; }
          .data-table th { background-color: #f5f5f5; }
        "))
        ),
        tags$body(
          tags$h2("Hitter Reports", style="font-size: 14px; margin-bottom: 10px;"),
          lapply(html_content, function(content) HTML(content))
        )
      )
      
      # Save HTML and convert to PDF
      htmltools::save_html(html_full, temp_html)
      webshot2::webshot(
        url = temp_html,
        file = file,
        vwidth = 1200,
        vheight = 1200,
        zoom = 1.5,
        expand = c(5, 5, 5, 5)
      )
      
      # Clean up temporary files
      unlink(c(temp_html, file.path(temp_dir, "*.png")))
    }
  )
  
  ## ---- Heat Map Functioning ----
  # Strike zone for eat map
  draw_detailed_strike_zone <- function() {
    ggplot() +
      geom_rect(aes(xmin = -28, xmax = 28, ymin = -10, ymax = 45),  # Full strike zone
                fill = "transparent", color = "black", linewidth = 1) +
      geom_rect(aes(xmin = -18, xmax = 18, ymin = 0, ymax = 35),     # Heart zone
                fill = "transparent", color = "grey", linewidth = 1, linetype = "dashed") +
      geom_rect(aes(xmin = -38, xmax = 38, ymin = -20, ymax = 55),   # Chase zone
                fill = "transparent", color = "grey", linewidth = 1, linetype = "dashed") +
      theme_void()
  }
  
  
  # Getting data for heat map
  generate_strikezone_heatmap <- function(player_id, pitch_data) {
    message("Generating heatmap for player_id: ", player_id)
    message("Total pitch_data rows: ", nrow(pitch_data))
    
    heatmap_data <- pitch_data %>%
      filter(
        hitter_id == player_id,
        !is.na(pitch_location_x),
        !is.na(pitch_location_y),
        !is.na(contact_type),
        !ball_in_play_outcome %in% c("None", "SO Looking", "SO Swinging"),
        pitch_location_x >= -28,
        pitch_location_x <= 28,
        pitch_location_y >= -10,
        pitch_location_y <= 45
      ) %>%
      mutate(
        # map quality levels to numeric weights
        quality_score = case_when(
          contact_type == "Weak"    ~ 0,
          contact_type == "Average" ~ 2,
          contact_type == "Hard"    ~ 4,
          TRUE                        ~ NA_real_
        )
      )
    
    p <- draw_detailed_strike_zone()
    
    if (nrow(heatmap_data) > 0) {
      # Clamp coordinates to strike zone
      heatmap_data <- heatmap_data %>%
        mutate(
          pitch_location_x = pmin(pmax(pitch_location_x, -38), 38),
          pitch_location_y = pmin(pmax(pitch_location_y, -20), 55)
        )
      
      # Calculate dynamic bandwidth based on data range
      x_range   <- diff(range(heatmap_data$pitch_location_x))
      y_range   <- diff(range(heatmap_data$pitch_location_y))
      bandwidth <- c(max(x_range/3, 2), max(y_range/3, 2))
      
      # force them equal
      b <- mean(bandwidth)
      
      # Plot density heatmap
      p <- p +
        stat_density_2d(
          data    = heatmap_data,
          aes(x = pitch_location_x, y = pitch_location_y, weight = quality_score, fill = ..density..),
          geom    = "raster",
          contour = FALSE,
          n       = 100,
          alpha   = 0.7,
          h       = c(b, b) * c(8.5, 8.5)
        ) +
        scale_fill_gradientn(
          colors = c("green", "yellow", "red"),
          guide = "none"
        )
      
      # Add 3x3 grid lines
      # Create 3 equal columns and rows inside the heart zone
      # Split full strike zone into 3 vertical and 3 horizontal regions
      x_splits <- seq(-28, 28, length.out = 4)[2:3]  # vertical dividers
      y_splits <- seq(-10, 45, length.out = 4)[2:3]  # horizontal dividers
      
      p <- p +
        # Vertical lines
        geom_segment(aes(x = x_splits[1], xend = x_splits[1], y = -10, yend = 45), color = "black", linewidth = 0.5) +
        geom_segment(aes(x = x_splits[2], xend = x_splits[2], y = -10, yend = 45), color = "black", linewidth = 0.5) +
        
        # Horizontal lines
        geom_segment(aes(x = -28, xend = 28, y = y_splits[1], yend = y_splits[1]), color = "black", linewidth = 0.5) +
        geom_segment(aes(x = -28, xend = 28, y = y_splits[2], yend = y_splits[2]), color = "black", linewidth = 0.5)
      
    } else {
      p <- p + geom_text(aes(x = 0, y = 30, label = "No Pitch Data"), size = 6)
      message("No pitch data available for heatmap.")
    }
    
    p <- p +
      coord_cartesian(xlim = c(-46, 46), ylim = c(-40, 60), clip = "on") +
      coord_fixed(ratio = 1) +
      theme(legend.position = "none")
    
    return(p)
  }
  
  ## ---- Spray Chart Functioning ----
  # Spray Chart Plotting Function
  generate_spray_chart <- function(
    player_id,
    role         = c("hitter","pitcher"),
    pitch_data,
    player_side  = NULL    # "LHH", "RHH", "RHP", "LHP", or NULL for all
  ) {
    role <- match.arg(role)
    
    # Base filter for batted‐ball events
    if (role == "hitter") {
      spray_data <- pitch_data %>%
        filter(
          hitter_id         == player_id,
          ball_in_play_outcome %in% c("Single","Double","Triple","Homerun")
        ) %>%
        # join batter handedness from stored players
        left_join(
          stored_players$data,
          by = c("pitcher_id" = "player_id")
        ) %>%
        mutate(
          PlayerSide = case_when(
            throwing_handedness %in% c("Right","R") ~ "RHP",
            throwing_handedness %in% c("Left","L")  ~ "LHP",
            TRUE                                     ~ NA_character_
          ) %>% factor(levels = c("LHP","RHP"))
        ) %>%
        filter(!is.na(PlayerSide))
      
      # If user requested a specific side, filter to that
      if (!is.null(player_side)) {
        spray_data <- spray_data %>%
          filter(PlayerSide == player_side)
      }
    } else {
      spray_data <- pitch_data %>%
        filter(
          pitcher_id        == player_id,
          ball_in_play_outcome %in% c("Single","Double","Triple","Homerun")
        ) %>%
        # join batter handedness from stored players
        left_join(
          stored_players$data,
          by = c("hitter_id" = "player_id")
        ) %>%
        mutate(
          PlayerSide = case_when(
            batter_handedness %in% c("Right","R") ~ "RHH",
            batter_handedness %in% c("Left","L")  ~ "LHH",
            TRUE                                     ~ NA_character_
          ) %>% factor(levels = c("LHH","RHH"))
        ) %>%
        filter(!is.na(PlayerSide))
      
      # If user requested a specific side, filter to that
      if (!is.null(player_side)) {
        spray_data <- spray_data %>%
          filter(PlayerSide == player_side)
      }
    }
    
    # drop any missing coordinates
    spray_data <- spray_data %>%
      filter(!is.na(spray_chart_x), !is.na(spray_chart_y))
    
    # add contact‐quality factor for coloring
    spray_data <- spray_data %>%
      mutate(
        ContactQuality = factor(
          case_when(
            contact_type == "Weak"    ~ "Weak",
            contact_type == "Average" ~ "Average",
            contact_type == "Hard"    ~ "Hard",
            TRUE                       ~ "Average"
          ),
          levels = c("Weak","Average","Hard")
        )
      )
    
    # start with the field background
    p <- draw_spraychart_field()
    
    if (nrow(spray_data) > 0) {
      p <- p +
        geom_point(
          data = spray_data,
          aes(x = spray_chart_x, y = spray_chart_y, color = ContactQuality),
          size = 3, alpha = 0.7, shape = 16
        ) +
        scale_color_manual(
          values = c("Weak" = "#888888", "Average" = "#882255", "Hard" = "#117733"),
          breaks = c("Weak","Average","Hard")
        ) +
        theme(legend.position = "bottom")
    } else {
      p <- p +
        geom_text(aes(x = 0, y = 0, label = "No Spray Data"), size = 6, color = "red")
    }
    
    return(p)
  }
  
  ## ---- Leaderboard Functioning ----
  output$leaderboard_calculations <- renderUI({
    if (input$leaderboard_choice == "Pitchers") {
      req(global_pitch_data())
      
      pa_summary <- global_pitch_data() %>%
        filter(is_pitch_event == TRUE) %>%
        left_join(stored_players$data, by = c("pitcher_id" = "player_id")) %>%
        filter(college_name == "Hobart College") %>%
        arrange(event_id, game_pitch_number) %>%
        mutate(pa_id = cumsum(at_bat_pitch_number == 1)) %>%
        ungroup() %>%
        group_by(event_id, pa_id) %>%
        summarise(
          pitcher_id = last(pitcher_id),
          pitches_in_at_bat = max(at_bat_pitch_number, na.rm = TRUE),
          pitch_outcome = last(pitch_outcome),
          outcome = last(ball_in_play_outcome),
          contact_type = last(contact_type),
          balls = last(balls), 
          strikes = last(strikes),
          batter_base = last(batter_outcome),
          first_base = last(coalesce(first_base_outcome, "None")),
          second_base = last(coalesce(second_base_outcome, "None")),
          third_base = last(coalesce(third_base_outcome, "None")),
          first_pitch_outcome = first(pitch_outcome)
        )
      
      pitcher_per_ab <- pa_summary %>%
        group_by(pitcher_id) %>%
        summarise(
          total_bf = n(),
          hard_hit = sum(contact_type == "Hard", na.rm = TRUE),
          weak_hit = sum(contact_type == "Weak", na.rm = TRUE),
          ball_in_play = sum(pitch_outcome == "Ball In Play", na.rm = TRUE),
          first_strike = sum(first_pitch_outcome %in% c("Ball In Play", "Foul Ball", "Called Strike", "Swinging Strike", "Cage Ball"), na.rm = TRUE),
          hits = sum(outcome %in% c("Single", "Double", "Triple", "Homerun"), na.rm = TRUE),
          walks = sum(outcome == "Walk", na.rm = TRUE),
          hbps = sum(outcome == "Hit By Pitch", na.rm = TRUE),
          strikeouts = sum(outcome %in% c("SO Looking", "SO Swinging"), na.rm = TRUE),
          # QAB stuff below
          sac_flys = sum(outcome == "Sacrifice Fly", na.rm = TRUE),
          sac_bunts = sum(outcome == "Sacrifice Bunt", na.rm = TRUE),
          so_swinging = sum(outcome == "SO Swinging", na.rm = TRUE),
          so_looking = sum(outcome == "SO Looking", na.rm = TRUE),
          rbi = sum(batter_base == "Scored", na.rm = TRUE) +
            sum(first_base == "Scored", na.rm = TRUE) +
            sum(second_base == "Scored", na.rm = TRUE) +
            sum(third_base == "Scored", na.rm = TRUE),
          contact_total = sum(contact_type == "Weak", na.rm = TRUE) +
            2 * sum(contact_type == "Average", na.rm = TRUE) +
            3 * sum(contact_type == "Hard", na.rm = TRUE),
          six_plus_pitches = sum(pitches_in_at_bat >= 6 & !batter_base %in% c("None", "Cage Ball"), na.rm = TRUE),
          .groups = "drop"
        )
      
      
      pitcher_every_pitch <- global_pitch_data() %>%
        group_by(pitcher_id) %>%
        summarise(
          total_pitches = sum(is_pitch_event == TRUE),
          total_strikes = sum(pitch_outcome %in% c("Called Strike", "Swinging Strike", "Ball In Play", "Foul Ball", "Cage Ball"), na.rm = TRUE),
          total_swing_miss = sum(pitch_outcome == "Swinging Strike", na.rm = TRUE),
          outs =  sum(ball_in_play_outcome %in% c("Out", "SO Looking", "SO Swinging"), na.rm = TRUE) +
            sum(first_base_outcome == "Out", na.rm = TRUE) +
            sum(second_base_outcome == "Out", na.rm = TRUE) +
            sum(third_base_outcome == "Out", na.rm = TRUE) +
            sum(ball_in_play_outcome == "Double Play" & ((first_base_outcome == "Out") + (second_base_outcome == "Out") + (third_base_outcome == "Out") < 2), na.rm = TRUE) +
            sum(ball_in_play_outcome == "Triple Play" & ((first_base_outcome == "Out") + (second_base_outcome == "Out") + (third_base_outcome == "Out") < 3), na.rm = TRUE),
          .groups = "drop"
        ) 
      
      pitchers <- pitcher_per_ab %>%
        left_join(pitcher_every_pitch, by = "pitcher_id") %>%
        mutate(
          strike_pct = round(ifelse(total_pitches > 0, total_strikes / total_pitches * 100, NA_real_), 1),
          swing_miss_pct = round(ifelse(total_pitches > 0, total_swing_miss / total_pitches * 100, NA_real_), 1),
          pitches_per_batter = round(ifelse(total_bf > 0, total_pitches / total_bf, NA_real_), 1),
          hard_hit_pct = round(ifelse(ball_in_play > 0, hard_hit / ball_in_play * 100, NA_real_), 1),
          weak_hit_pct = round(ifelse(ball_in_play > 0, weak_hit / ball_in_play * 100, NA_real_), 1),
          first_strike_pct = round(ifelse(total_bf > 0, first_strike / total_bf * 100, NA_real_), 1),
          hits_per_nine = round(ifelse(outs > 0, hits / outs * 9, NA_real_), 1),
          walks_per_nine = round(ifelse(outs > 0, walks / outs * 9, NA_real_), 1),
          hbp_per_nine = round(ifelse(outs > 0, hbps / outs * 9, NA_real_), 1),
          stikeout_per_nine = round(ifelse(outs > 0, strikeouts / outs * 9, NA_real_), 1),
          qab_score = round(
            ifelse(total_bf > 0,
                   ((hits + walks + rbi + hbps + sac_flys + sac_bunts + six_plus_pitches) * 3 +
                      contact_total -
                      so_swinging * 3 -
                      so_looking * 2) / total_bf,
                   NA_real_),
            1
          )
        )
      
      pitcher_leaders <- pitchers %>%
        mutate(
          # Higher is better (scale normally)
          strike_pct_scaled = ifelse(is.na(strike_pct) | max(strike_pct, na.rm = TRUE) == min(strike_pct, na.rm = TRUE),
                                     NA_real_,
                                     round((strike_pct - min(strike_pct, na.rm = TRUE)) /
                                             (max(strike_pct, na.rm = TRUE) - min(strike_pct, na.rm = TRUE)) * 100, 1)),
          
          swing_miss_pct_scaled = ifelse(is.na(swing_miss_pct) | max(swing_miss_pct, na.rm = TRUE) == min(swing_miss_pct, na.rm = TRUE),
                                         NA_real_,
                                         round((swing_miss_pct - min(swing_miss_pct, na.rm = TRUE)) /
                                                 (max(swing_miss_pct, na.rm = TRUE) - min(swing_miss_pct, na.rm = TRUE)) * 100, 1)),
          
          first_strike_pct_scaled = ifelse(is.na(first_strike_pct) | max(first_strike_pct, na.rm = TRUE) == min(first_strike_pct, na.rm = TRUE),
                                           NA_real_,
                                           round((first_strike_pct - min(first_strike_pct, na.rm = TRUE)) /
                                                   (max(first_strike_pct, na.rm = TRUE) - min(first_strike_pct, na.rm = TRUE)) * 100, 1)),
          
          weak_hit_pct_scaled = ifelse(is.na(weak_hit_pct) | max(weak_hit_pct, na.rm = TRUE) == min(weak_hit_pct, na.rm = TRUE),
                                       NA_real_,
                                       round((weak_hit_pct - min(weak_hit_pct, na.rm = TRUE)) /
                                               (max(weak_hit_pct, na.rm = TRUE) - min(weak_hit_pct, na.rm = TRUE)) * 100, 1)),
          
          stikeout_per_nine_scaled = ifelse(is.na(stikeout_per_nine) | max(stikeout_per_nine, na.rm = TRUE) == min(stikeout_per_nine, na.rm = TRUE),
                                            NA_real_,
                                            round((stikeout_per_nine - min(stikeout_per_nine, na.rm = TRUE)) /
                                                    (max(stikeout_per_nine, na.rm = TRUE) - min(stikeout_per_nine, na.rm = TRUE)) * 100, 1)),
          
          # Lower is better (reverse the scale)
          pitches_per_batter_scaled = ifelse(is.na(pitches_per_batter) | max(pitches_per_batter, na.rm = TRUE) == min(pitches_per_batter, na.rm = TRUE),
                                             NA_real_,
                                             round(100 - (pitches_per_batter - min(pitches_per_batter, na.rm = TRUE)) /
                                                     (max(pitches_per_batter, na.rm = TRUE) - min(pitches_per_batter, na.rm = TRUE)) * 100, 1)),
          
          hard_hit_pct_scaled = ifelse(is.na(hard_hit_pct) | max(hard_hit_pct, na.rm = TRUE) == min(hard_hit_pct, na.rm = TRUE),
                                       NA_real_,
                                       round(100 - (hard_hit_pct - min(hard_hit_pct, na.rm = TRUE)) /
                                               (max(hard_hit_pct, na.rm = TRUE) - min(hard_hit_pct, na.rm = TRUE)) * 100, 1)),
          
          hits_per_nine_scaled = ifelse(is.na(hits_per_nine) | max(hits_per_nine, na.rm = TRUE) == min(hits_per_nine, na.rm = TRUE),
                                        NA_real_,
                                        round(100 - (hits_per_nine - min(hits_per_nine, na.rm = TRUE)) /
                                                (max(hits_per_nine, na.rm = TRUE) - min(hits_per_nine, na.rm = TRUE)) * 100, 1)),
          
          walks_per_nine_scaled = ifelse(is.na(walks_per_nine) | max(walks_per_nine, na.rm = TRUE) == min(walks_per_nine, na.rm = TRUE),
                                         NA_real_,
                                         round(100 - (walks_per_nine - min(walks_per_nine, na.rm = TRUE)) /
                                                 (max(walks_per_nine, na.rm = TRUE) - min(walks_per_nine, na.rm = TRUE)) * 100, 1)),
          
          hbp_per_nine_scaled = ifelse(is.na(hbp_per_nine) | max(hbp_per_nine, na.rm = TRUE) == min(hbp_per_nine, na.rm = TRUE),
                                       NA_real_,
                                       round(100 - (hbp_per_nine - min(hbp_per_nine, na.rm = TRUE)) /
                                               (max(hbp_per_nine, na.rm = TRUE) - min(hbp_per_nine, na.rm = TRUE)) * 100, 1)),
          
          qab_score_scaled = ifelse(is.na(qab_score) | max(qab_score, na.rm = TRUE) == min(qab_score, na.rm = TRUE),
                                    NA_real_,
                                    round(100 - (qab_score - min(qab_score, na.rm = TRUE)) /
                                            (max(qab_score, na.rm = TRUE) - min(qab_score, na.rm = TRUE)) * 100, 1)),
          composite_avg_score = round(rowMeans(cbind(
            pmax(coalesce(strike_pct_scaled, 50), 1),
            pmax(coalesce(swing_miss_pct_scaled, 50), 1),
            pmax(coalesce(first_strike_pct_scaled, 50), 1),
            pmax(coalesce(weak_hit_pct_scaled, 50), 1),
            pmax(coalesce(stikeout_per_nine_scaled, 50), 1),
            pmax(coalesce(pitches_per_batter_scaled, 50), 1),
            pmax(coalesce(hard_hit_pct_scaled, 50), 1),
            pmax(coalesce(hits_per_nine_scaled, 50), 1),
            pmax(coalesce(walks_per_nine_scaled, 50), 1),
            pmax(coalesce(hbp_per_nine_scaled, 50), 1),
            pmax(coalesce(qab_score_scaled, 50), 1)
          ), na.rm = TRUE), 1)
        ) %>%
        left_join(stored_players$data, by = c("pitcher_id" = "player_id")) %>%
        select(
          `Name` = full_name,
          `Composite Ranking` = composite_avg_score,
          `Batters Faced` = total_bf,
          `Total Pitches` = total_pitches,
          `Strike %` = strike_pct,
          `Swing & Miss %` = swing_miss_pct,
          `First-Pitch Strike %` = first_strike_pct,
          `Weak Contact %` = weak_hit_pct,
          `Strikeouts per 9` = stikeout_per_nine,
          `Pitches per Batter` = pitches_per_batter,
          `Hard Contact %` = hard_hit_pct,
          `Hits per 9` = hits_per_nine,
          `Walks per 9` = walks_per_nine,
          `HBP per 9` = hbp_per_nine,
          `QAB Score` = qab_score
        ) %>%
        arrange(desc(`Composite Ranking`))
      
      output$hobart_pitcher_data_table <- DT::renderDataTable({
        DT::datatable(pitcher_leaders)#will be combination of two above
      })
      
      DT::dataTableOutput("hobart_pitcher_data_table")
      
    } else if (input$leaderboard_choice == "Hitters") {
      req(global_pitch_data())
      
      hitter_leaders <- global_pitch_data() %>%
        filter(is_pitch_event == TRUE) %>%
        left_join(stored_players$data, by = c("hitter_id" = "player_id")) %>%
        filter(college_name == "Hobart College") %>%
        arrange(event_id, game_pitch_number) %>%
        mutate(pa_id = cumsum(at_bat_pitch_number == 1)) %>%
        ungroup() %>%
        group_by(event_id, pa_id) %>%
        slice_max(at_bat_pitch_number, n = 1, with_ties = FALSE) %>%
        transmute(
          hitter_id = hitter_id,
          pitches_in_at_bat = at_bat_pitch_number,
          pitch_outcome = pitch_outcome,
          outcome = ball_in_play_outcome,
          ball_flight = ball_flight,
          contact_type = contact_type,
          balls = balls, 
          strikes = strikes,
          batter_base = batter_outcome,
          first_base = coalesce(first_base_outcome, "None"),
          second_base = coalesce(second_base_outcome, "None"),
          third_base = coalesce(third_base_outcome, "None")
        ) %>%
        ungroup() %>%
        group_by(hitter_id) %>%
        summarise(
          total_pas = n(),
          hits = sum(outcome %in% c("Single", "Double", "Triple", "Homerun"), na.rm = TRUE),
          walks = sum(outcome == "Walk", na.rm = TRUE),
          hbp = sum(outcome == "Hit By Pitch", na.rm = TRUE),
          sac_flys = sum(outcome == "Sacrifice Fly", na.rm = TRUE),
          sac_bunts = sum(outcome == "Sacrifice Bunt", na.rm = TRUE),
          so_swinging = sum(outcome == "SO Swinging", na.rm = TRUE),
          so_looking = sum(outcome == "SO Looking", na.rm = TRUE),
          rbi = sum(batter_base == "Scored", na.rm = TRUE) +
            sum(first_base == "Scored", na.rm = TRUE) +
            sum(second_base == "Scored", na.rm = TRUE) +
            sum(third_base == "Scored", na.rm = TRUE),
          contact_total = sum(contact_type == "Weak", na.rm = TRUE) +
            2 * sum(contact_type == "Average", na.rm = TRUE) +
            3 * sum(contact_type == "Hard", na.rm = TRUE),
          six_plus_pitches = sum(pitches_in_at_bat >= 6 & !batter_base %in% c("None", "Cage Ball"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          qab_score = round(
            ((hits + walks + rbi + hbp + sac_flys + sac_bunts + six_plus_pitches) * 3 +
               contact_total -
               so_swinging * 3 -
               so_looking * 2) / ifelse(total_pas > 0, total_pas, 1), 1
          )
        ) %>%
        left_join(stored_players$data, by = c("hitter_id" = "player_id")) %>%
        select(full_name, qab_score, total_pas, hits, rbi, walks, hbp, sac_flys, sac_bunts,
               so_swinging, so_looking, contact_total, six_plus_pitches) %>%
        rename(
          "Player Name" = full_name,
          "QAB Score" = qab_score,
          "Total PAs" = total_pas,
          "Hits" = hits,
          "RBIs" = rbi,
          "Walks" = walks,
          "HBP" = hbp,
          "Sacrifice Fly" = sac_flys,
          "Sacrifice Bunt" = sac_bunts,
          "SO Swinging" = so_swinging,
          "SO Looking" = so_looking,
          "Contact Score" = contact_total,
          "6+ Pitch ABs" = six_plus_pitches
        ) %>%
        arrange(desc(`QAB Score`))
      
      output$hobart_hitter_data_table <- DT::renderDataTable({
        DT::datatable(hitter_leaders)
      })
      
      DT::dataTableOutput("hobart_hitter_data_table")
    } else {
      h4("You need to choose a player type")
    }
  })
  
  
  
  
  # ---- New Event Page Functioning ----
  
  # Choosing the event type "Game"
  observeEvent(input$type_game, {
    message("Game button pressed")
    # Set game info metadata
    game_info$date <- current_date
    game_info$event_type <- "Game"
  
    id <- latest_event_id()
    # Generate unique event_id by querying the database
    if (is.null(id) || is.na(id) || id == 0) {
      game_info$event_id <- 1
    } else {
      game_info$event_id <- id + 1
    }
  
    # Reset ball in play
    ball_in_play_data_reset()
  
    # Navigate to lineup page
    app_state$page <- "lineup"
  })
  
  # Choosing the event type "Practice"
  observeEvent(input$type_practice, {
    message("Practice button pressed")
    # Set game info metadata
    game_info$date <- current_date
    game_info$event_type <- "Practice"
  
    id <- latest_event_id()
    # Generate unique event_id by querying the database
    if (is.null(id) || is.na(id) || id == 0) {
      game_info$event_id <- 1
    } else {
      game_info$event_id <- id + 1
    }
  
    # Reset ball in play and game state
    ball_in_play_data_reset()
  
    # Navigate to lineup page
    app_state$page <- "before_practice"
  })
  
  
  # ---- Line Up Page Functioning ----
  # Populate away and home team dropdowns
  observe({
    req(app_state$page == "lineup", stored_players$data)
    players_wiht_team <- stored_players$data %>%
      pull(college_name) %>%
      unique()
    team_choices <- c("Choose Team", players_wiht_team)
    updateSelectInput(session, "home_team", choices = team_choices, selected = "Choose Team")
    updateSelectInput(session, "away_team", choices = team_choices, selected = "Choose Team")
  })
  
  # Populate home team player dropdowns with auto-selection
  observeEvent(input$home_team, {
    req(input$home_team, input$home_team != "Choose Team")
    
    # Get active players with display_name, sorted by jersey_number
    home_players_df <- stored_players$data %>%
      filter(college_name == input$home_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name)) %>%
      arrange(jersey_number)
    
    if (nrow(home_players_df) == 0) {
      for (i in 1:9) {
        updateSelectInput(session, paste0("ht_", i, "spot"), choices = c("No players available"))
      }
      updateSelectInput(session, "ht_starting_pitcher", choices = c("No players available"))
    } else {
      # Select 9 distinct hitters or two-way players for 1-9
      lineup_players <- home_players_df %>%
        head(9) %>%  # First 9 after sorting by jersey_number
        pull(display_name)
      all_players <- home_players_df$display_name
      
      # Update positions 1-9
      for (i in 1:9) {
        selected_player <- if (i <= length(lineup_players)) lineup_players[i] else ""
        updateSelectInput(session, paste0("ht_", i, "spot"), choices = all_players, selected = selected_player)
      }
      
      # Starting pitcher (any player, prefer non-batter)
      pitcher_df <- home_players_df %>%
        filter(!display_name %in% lineup_players)  # Exclude batting lineup
      pitcher <- if (nrow(pitcher_df) > 0) pitcher_df$display_name[1] else home_players_df$display_name[1]
      updateSelectInput(session, "ht_starting_pitcher", choices = all_players, selected = pitcher)
    }
  })
  
  
  # Populate away team player dropdowns with auto-selection
  observeEvent(input$away_team, {
    req(input$away_team, input$away_team != "Choose Team")
    
    # Get active players with display_name, sorted by jersey_number
    away_players_df <- stored_players$data %>%
      filter(college_name == input$away_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name)) %>%
      arrange(jersey_number)
    
    if (nrow(away_players_df) == 0) {
      for (i in 1:9) {
        updateSelectInput(session, paste0("at_", i, "spot"), choices = c("No players available"))
      }
      updateSelectInput(session, "at_starting_pitcher", choices = c("No players available"))
    } else {
      # Select 9 distinct hitters or two-way players for 1-9
      lineup_players <- away_players_df %>%
        head(9) %>%
        pull(display_name)
      all_players <- away_players_df$display_name
      
      for (i in 1:9) {
        selected_player <- if (i <= length(lineup_players)) lineup_players[i] else ""
        updateSelectInput(session, paste0("at_", i, "spot"), choices = all_players, selected = selected_player)
      }
      
      # Starting pitcher (any player, prefer non-batter)
      pitcher_df <- away_players_df %>%
        filter(!display_name %in% lineup_players)
      pitcher <- if (nrow(pitcher_df) > 0) pitcher_df$display_name[1] else away_players_df$display_name[1]
      updateSelectInput(session, "at_starting_pitcher", choices = all_players, selected = pitcher)
    }
  })
  
  
  # Confirm button to move to Game Page
  observeEvent(input$to_game, {
    req(input$home_team, input$away_team, input$ht_starting_pitcher, input$at_starting_pitcher)
    
    if (input$inning_specifier == "Choose") {
      showNotification("Make sure to specify the number of innings", type = "error")
      return()
    }
    
    # Set team IDs
    game_info$home_team <- input$home_team
    game_info$away_team <- input$away_team
    
    # Add into events table
    dbWriteTable(connect_to_db, "events", as.data.frame(reactiveValuesToList(game_info)), append = TRUE, row.names = FALSE)
    
    # Home team lineup and pitcher
    home_players <- stored_players$data %>%
      filter(college_name == game_info$home_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name))
    game_state$home_lineup <- sapply(1:9, function(i) {
      display_name <- input[[paste0("ht_", i, "spot")]]
      if (is.null(display_name) || display_name == "" || is.na(display_name)) {
        NA
      } else {
        player_id <- home_players$player_id[home_players$display_name == display_name][1]
        if (is.na(player_id)) {
          NA
        } else {
          player_id
        }
      }
    })
    ht_pitcher_name <- input$ht_starting_pitcher
    game_state$home_pitcher <- home_players$player_id[home_players$display_name == ht_pitcher_name][1]
    
    # Away team lineup and pitcher
    away_players <- stored_players$data %>%
      filter(college_name == game_info$away_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name))
    game_state$away_lineup <- sapply(1:9, function(i) {
      display_name <- input[[paste0("at_", i, "spot")]]
      if (is.null(display_name) || display_name == "" || is.na(display_name)) {
        NA
      } else {
        player_id <- away_players$player_id[away_players$display_name == display_name][1]
        if (is.na(player_id)) {
          NA
        } else {
          player_id
        }
      }
    })
    at_pitcher_name <- input$at_starting_pitcher
    game_state$away_pitcher <- away_players$player_id[away_players$display_name == at_pitcher_name][1]
    
    # Validate lineups
    if (any(is.na(game_state$home_lineup[1:9])) || any(is.na(game_state$away_lineup[1:9]))) {
      showNotification("Please fill all batting positions (1-9) for both teams.", type = "error")
      return()
    }
    
    # Initial game state
    game_state$half <- "Top"
    game_state$current_pitcher <- game_state$home_pitcher
    game_state$inning_specifier <- as.numeric(input$inning_specifier)
    game_state$pitch_log <- list()
    if (is.null(game_state$away_next_batter_idx)) game_state$away_next_batter_idx <- 1
    if (is.null(game_state$home_next_batter_idx)) game_state$home_next_batter_idx <- 1
    game_state$current_batter_idx <- game_state$away_next_batter_idx
    
    app_state$page <- "game_event"
  })
  
  
  # ---- Practice Events - All Functioning ----
  ## ---- Before Practice Page Functioning ----
  # Populate team dropdowns for before practice page
  observe({
    req(app_state$page == "before_practice", stored_players$data)
    players_wiht_team <- stored_players$data %>%
      pull(college_name) %>%
      unique()
    team_choices <- c("Choose Team", players_wiht_team)
    updateSelectInput(session, "practice_team", choices = team_choices, selected = "Choose Team")
  })
  
  # Confirm button
  observeEvent(input$go_practice, {
    if(any(c(input$option_innings, input$option_score, input$option_runners, input$option_bip_outcome) == "Choose") || input$practice_team == "Choose Team") {
      showNotification("Make sure to choose an option for all of the drop downs!", type = "warning")
      return()
    }
    
    if (input$option_runners == "Yes") {
      if (input$option_bip_outcome == "No") {
        showNotification("If you are keeping track of runners, then you must have ball in play outcome!", type = "warning")
        return()
      }
    }
    
    if (input$option_bip_outcome == "No") {
      if (input$option_runners == "Yes") {
        showNotification("If you don't keep track of ball in play, then you cannot have runners!", type = "warning")
        return()
      }
    }
    
    if (input$option_score == "Yes") {
      if (any(c(input$option_runners, input$option_bip_outcome) == "No")) {
        showNotification("If you want to keep track of score, then you need runners and ball in play outcome!", type = "warning")
        return()
      }
      if (input$option_innings == "No") {
        showNotification("If you want to keep track of score, then you need to keep track of innings!", type = "warning")
        return()
      }
    }
    
    # Set Practice State
    practice_state$have_innings <- input$option_innings
    practice_state$have_score <- input$option_score
    practice_state$have_runners <- input$option_runners
    practice_state$have_bip_outcome <- input$option_bip_outcome
    
    # Team drop down changing functioning of practice page
    if (input$practice_team != "Hobart College") {
      # Pop up to determine what team hits first
      showModal(modalDialog(
        title = "Is Hobart hitting first?",
        easyClose = FALSE,
        footer = tagList(
          actionButton("hitting_yes", "Yes"),
          actionButton("hitting_no", "No")
        )
      ))
    } else {
      # Set team IDs
      game_info$away_team <- "Hobart College"
      game_info$home_team <- "Hobart College"
      game_state$hitting_team <- game_info$away_team
      game_state$pitching_team <- game_info$home_team
      app_state$page <- "practice_event"
      pitch_update_trigger(pitch_update_trigger() + 1)
      print(game_state$current_pitcher)
      
      # Add into events table
      dbWriteTable(connect_to_db, "events", as.data.frame(reactiveValuesToList(game_info)), append = TRUE, row.names = FALSE)
    }
  })
  
  # When Hoabrt not hitting first
  observeEvent(input$hitting_no, {
    removeModal()
    # Set team IDs
    game_info$away_team <- input$practice_team
    game_info$home_team <- "Hobart College"
    game_state$hitting_team <- game_info$away_team
    game_state$pitching_team <- game_info$home_team
    
    # Add into events table
    dbWriteTable(connect_to_db, "events", as.data.frame(reactiveValuesToList(game_info)), append = TRUE, row.names = FALSE)
    
    app_state$page <- "practice_event"
    pitch_update_trigger(pitch_update_trigger() + 1)
  })
  
  # When Hobart hitting first
  observeEvent(input$hitting_yes, {
    removeModal()
    # Set team IDs
    game_info$away_team <- "Hobart College"
    game_info$home_team <- input$practice_team
    game_state$hitting_team <- game_info$away_team
    game_state$pitching_team <- game_info$home_team
    
    # Add into events table
    dbWriteTable(connect_to_db, "events", as.data.frame(reactiveValuesToList(game_info)), append = TRUE, row.names = FALSE)
    
    app_state$page <- "practice_event"
    pitch_update_trigger(pitch_update_trigger() + 1)
  })
  
  ## ---- Sidebar Functioning ----
  # Full inning info
  output$full_inning <- renderUI({
    req(game_state$half)
    req(game_state$inning)
    if (practice_state$have_innings == "Yes") {
      print(paste(as.character(game_state$half), as.numeric(game_state$inning)))
    }
  })  
  
  # Show runners on base graphic and place runners button
  output$having_runners <- renderUI({
    if (practice_state$have_runners == "Yes") {
      tagList(
        plotOutput("bases_graphic", height = "95px"),
        actionButton("place_runners", "Place Runners", class = "thin-wide-btn")
      )
    } else {
      NULL
    }
  })
  
  # Current Pitchers Pitch Count and Batters Faced
  output$practice_pitch_count_and_bf <- renderUI({
    if (is.na(game_state$current_pitcher) || 
        game_state$current_pitcher == "Choose Player" || 
        game_state$current_pitcher < 1000) {
      return(tags$p("Pitcher Not Selected"))
    } else {
      # Get pitcher ID from display name
      id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$player_id == game_state$current_pitcher]
      
      # Current Pitcher Pitch Count
      count <- if (length(id_pitcher) == 0 || is.null(id_pitcher)) {
        0
      } else {
        game_state$pitcher_pitch_count[[as.character(id_pitcher)]] %||% 0
      }
      #Current Pitcher Plate Appearance Count
      this_game <- global_pitch_data() %>%
        filter(
          event_id == game_info$event_id,
          pitcher_id == id_pitcher,
          is_pitch_event == TRUE,
          at_bat_pitch_number == 1
        ) %>%
        nrow()
      
      batter_count <- if (length(id_pitcher) == 0 || is.null(id_pitcher) || length(this_game) == 0 || is.null(this_game)) {
        0
      } else {
        this_game
      }
      return(tagList(
        tags$p(paste(count, "Pitches")),
        tags$p(paste("Batters Faced:", batter_count))
      ))
      
    }
  })
  
  # Showing the score for practice
  output$practice_score <- renderUI({
    if (practice_state$have_score == "Yes") {
      tagList(
        tags$p(paste(game_info$away_team, ": ", game_state$away_score)),
        tags$p(paste(game_info$home_team, ": ", game_state$home_score))
      )
    } else {
      NULL
    }
  })
  
  # Going to next inning or go to next pitcher - depending on practice input
  output$next_inning_button <- renderUI({
    if (practice_state$have_innings == "Yes") {
      tagList(
        actionButton("next_inning", "Next Half Inning", class = "thin-wide-btn")
      )
    } else {
      tagList(
        actionButton("next_pitcher", "Next Pitcher / Clear Situation", class = "thin-wide-btn")
      )
    }
  })
  
  # Manually Set The Count pop up
  observeEvent(input$set_the_count_outs, {
    showModal(
      modalDialog(
        title = "Set the Count and Outs",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_count_out_set", "Set Count")
        ),
        numericInput("manual_balls", "Balls", value = game_state$balls, min = 0, max = 3, step = 1),
        numericInput("manual_strikes", "Strikes", value = game_state$strikes, min = 0, max = 2, step = 1),
        numericInput("manual_outs", "Outs", value = game_state$outs, min = 0, max = 7, step = 1)
      )
    )
  })
  
  # Confirm Count button
  observeEvent(input$confirm_count_out_set, {
    game_state$balls <- input$manual_balls
    game_state$strikes <- input$manual_strikes
    game_state$outs <- input$manual_outs
    removeModal()
  })
  
  # Ability to set the score
  output$changing_score_ui <- renderUI({
    if (practice_state$have_score == "Yes") {
      actionButton("set_the_score", "Change Score", class = "thin-wide-btn")
    } else {
      NULL
    }
  })
  
  # Change score pop up
  observeEvent(input$set_the_score, {
    showModal(modalDialog(
      title = "Update Inning Score",
      size = "m",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_score_update", "Confirm")
      ),
      tags$div(
        style = "text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 10px;",
        paste0(game_info$away_team, ": ", game_state$away_score, " | ", 
               game_info$home_team, ": ", game_state$home_score)
      ),
      numericInput("update_away_score", 
                   label = paste0(game_info$away_team, " Score(Away):"), 
                   value = game_state$away_score, 
                   min = 0, step = 1),
      numericInput("update_home_score", 
                   label = paste0(game_info$home_team, " Score(Home):"), 
                   value = game_state$home_score, 
                   min = 0, step = 1)
    ))
  })
  
  # Confirm button to change the score
  observeEvent(input$confirm_score_update, {
    game_state$away_score <- input$update_away_score
    game_state$home_score <- input$update_home_score
    removeModal()
  })
  
  # Place Ghost Runners or Remove Runners
  observeEvent(input$place_runners, {
    
    # Helper to build selectInput for each base
    make_base_selector <- function(base_label, base_var, input_id) {
      current_val <- game_state$runners[[base_var]]
      
      # Base occupied by a ghost runner (Chose 50 because has to be higher than 0)
      if (!is.na(current_val) && current_val > 50) {
        selectInput(input_id, paste("Runner on", base_label),
                    choices = c("Keep Runner on Base", "Change to Ghost Runner", "Remove Runner"),
                    selected = "Keep Runner on Base")
      } else {
        selectInput(input_id, paste("Runner on", base_label),
                    choices = c("Empty", "Add Ghost Runner"),
                    selected = "Empty")
      }
    }
    # Decide which bases
    showModal(
      modalDialog(
        title = "Manage Runners",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_place_runners", "Update Runners")
        ),
        make_base_selector("First", "first", "runner_first_base"),
        make_base_selector("Second", "second", "runner_second_base"),
        make_base_selector("Third", "third", "runner_third_base")
      )
    )
  })
  
  # Add or remove the runners functioning
  observeEvent(input$confirm_place_runners, {
    if (
      input$runner_first_base %in% c("Empty", "Keep Runner on Base") &
      input$runner_second_base %in% c("Empty", "Keep Runner on Base") &
      input$runner_third_base %in% c("Empty", "Keep Runner on Base")
    ) {
      showNotification("You did not make any changes to the runners", type = "error")
      return()
    } else {
      if (input$runner_first_base %in% c("Add Ghost Runner", "Change to Ghost Runner")) {
        game_state$runners$first <- 100
      } else if (input$runner_first_base == "Remove Runner") {
        game_state$runners$first <- NA_real_
      }
      if (input$runner_second_base %in% c("Add Ghost Runner", "Change to Ghost Runner")) {
        game_state$runners$second <- 100
      } else if (input$runner_second_base == "Remove Runner") {
        game_state$runners$second <- NA_real_
      }
      if (input$runner_third_base %in% c("Add Ghost Runner", "Change to Ghost Runner")) {
        game_state$runners$third <- 100
      } else if (input$runner_third_base == "Remove Runner") {
        game_state$runners$third <- NA_real_
      }
      removeModal()
    }
  })
  
  # Going to the next inning
  observeEvent(input$next_inning, {
    showModal(
      modalDialog(
        title = "Confirm Half Inning Change",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("going_next_inning", "Yes")
        ),
        "Are you sure you want to go to the next inning? This will clear the bases, count, outs and pitcher."
      )
    )
  })
  
  # Confirmation for going to next inning, clear everything necessary
  observeEvent(input$going_next_inning, {
    update_practice_count_and_inning(reset = TRUE)
    resetPitchInputs()
    prev_runners$outcome_first <- NA_character_
    prev_runners$outcome_second <- NA_character_
    prev_runners$outcome_third <- NA_character_
    game_state$outs <- 0
    if (game_state$outs >= 3 & practice_state$have_runners == "Yes") {
      # Change runner outcome to LOB (Left On Base) - need for stats
      prev_runners$outcome_first <- ifelse(
        !is.na(game_state$runners$first),
        "LOB",
        prev_runners$outcome_first
      )
      prev_runners$outcome_second <- ifelse(
        !is.na(game_state$runners$second),
        "LOB",
        prev_runners$outcome_second
      )
      prev_runners$outcome_third <- ifelse(
        !is.na(game_state$runners$third),
        "LOB",
        prev_runners$outcome_third
      )
      
      dbExecute(connect_to_db, "
      UPDATE global_pitch_data
      SET first_base_outcome = $1, second_base_outcome = $2, third_base_outcome = $3
      WHERE game_pitch_number = $4 AND event_id = $5",
                params = list(prev_runners$outcome_first, prev_runners$outcome_second, prev_runners$outcome_third, 
                              game_state$pitch_count, game_info$event_id))
    }
    
    # Clear bases
    game_state$runners$first <- NA_real_
    game_state$runners$second <- NA_real_
    game_state$runners$third <- NA_real_
    
    # Switch Teams
    if (game_state$half == "Top") {
      game_state$hitting_team <- game_info$home_team
      game_state$pitching_team <- game_info$away_team
      game_state$half <- "Bottom"
    } else {
      game_state$hitting_team <- game_info$away_team
      game_state$pitching_team <- game_info$home_team
      game_state$half <- "Top"
      game_state$inning <- game_state$inning + 1
    }
    game_state$current_hitter <- NA_real_
    game_state$current_pitcher <- NA_real_
    removeModal()
  })
  
  # Going to the next pitcher
  observeEvent(input$next_pitcher, {
    showModal(
      modalDialog(
        title = "Confirm Pitcher Change",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("getting_next_pitcher", "Yes")
        ),
        "Warning! If you click confirm, it will clear the bases, count, outs and pitcher.
        If you are looking to just change the pitcher without changing the 
        game situation. Press cancel and then select a new pitcher from 
        the drop down."
      )
    )
  })
  
  # Go to the next pitcher, reset everything necessary
  observeEvent(input$getting_next_pitcher, {
    game_state$current_pitcher <- NA_real_
    game_state$runners$first <- NA_real_
    game_state$runners$second <- NA_real_
    game_state$runners$third <- NA_real_
    update_practice_count_and_inning(reset = TRUE)
    game_state$outs <- 0
    removeModal()
  })
  
  # End the practice session
  observeEvent(input$end_practice, {
    showModal(
      modalDialog(
        title = "End the Practice",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_practice_end", "End Session")
        ),
        "Are you sure you want to end the practice, the data has 
        been saved but the game situation will not!"
      )
    )
  })
  
  # Confirmed End of practice
  observeEvent(input$confirm_practice_end, {
    reset_the_game_state()
    app_state$page <- "start"
    removeModal()
  })
  
  ## ---- Player Drop Downs ----
  # Populate Pitcher Drop down
  available_pitcher_players <- reactive({
    req(stored_players$data)
    req(game_state$pitching_team)
    stored_players$data %>%
      filter(status == "Active", college_name == game_state$pitching_team) %>% # updates based on current pitching team
      mutate(display_name = paste0(jersey_number, " — ", full_name))
  })
  
  # Populate Hitter Drop down
  available_hitter_players <- reactive({
    req(stored_players$data)
    req(game_state$hitting_team)
    stored_players$data %>%
      filter(status == "Active", college_name == game_state$hitting_team) %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name))
  })
  
  # Update drop downs based on inputs from before practice page
  observe({
    req(app_state$page == "practice_event")
    req(pitch_update_trigger())
    
    game_state$available_pitchers <- available_pitcher_players()
    game_state$available_hitters <- available_hitter_players()
    
    # If current_pitcher is NULL, use "Choose Player"
    current_pitcher <- if (!is.na(game_state$current_pitcher)) {
      game_state$available_pitchers %>%
        filter(player_id == game_state$current_pitcher) %>%
        pull(display_name) %>%
        { if (length(.) == 0) "Choose Player" else . }
    } else {
      "Choose Player"
    }
    
    # If current_hitter is NULL, use "Choose Player"
    current_hitter <- if (!is.na(game_state$current_hitter)) {
      game_state$available_hitters %>%
        filter(player_id == game_state$current_hitter) %>%
        pull(display_name) %>%
        { if (length(.) == 0) "Choose Player" else . }
    } else {
      "Choose Player"
    }
    
    updateSelectInput(session, "practice_pitcher", 
                      choices = c("Choose Player", game_state$available_pitchers$display_name),
                      selected = current_pitcher)
    
    updateSelectInput(session, "practice_batter", 
                      choices = c("Choose Player", game_state$available_hitters$display_name),
                      selected = current_hitter)
  })
  
  # Updating game_state$current_pitcher whenever selectInput changes
  observeEvent(input$practice_pitcher, {
    req(input$practice_pitcher != "Choose Player")
    
    id_pitcher <- game_state$available_pitchers$player_id[
      game_state$available_pitchers$display_name == input$practice_pitcher
    ]
    
    if (length(id_pitcher) == 1) {
      game_state$current_pitcher <- id_pitcher
    }
  })
  
  # Updating game_state$current_hitter whenever selectInput changes
  observeEvent(input$practice_hitter, {
    req(input$practice_hitter != "Choose Player")
    
    id_hitter <- game_state$available_hitters$player_id[
      game_state$available_hitters$display_name == input$practice_hitter
    ]
    
    if (length(id_hitter) == 1) {
      game_state$current_hitter <- id_hitter
    }
  })
  
  ## ---- Observe Pitch Table ----
  # Pitches from current practice event
  output$pitch_table_current_game <- DT::renderDataTable({
    req(global_pitch_data(), game_info$event_id)
    
    # turn player_id into names for all columns
    data <- global_pitch_data() %>%
      filter(event_id == game_info$event_id) %>%
      arrange(desc(game_pitch_number)) %>%
      mutate(
        count = paste(balls, " - ", strikes),
        the_inning = paste(half_inning, inning)
      ) %>%
      left_join(
        stored_players$data %>% select(player_id, pitcher_name = full_name),
        by = c("pitcher_id" = "player_id")
      ) %>%
      left_join(
        stored_players$data %>% select(player_id, hitter_name = full_name),
        by = c("hitter_id" = "player_id")
      ) %>%
      left_join(
        stored_players$data %>% select(player_id, first_base_name = full_name),
        by = c("first_base_runner" = "player_id")
      ) %>%
      left_join(
        stored_players$data %>% select(player_id, second_base_name = full_name),
        by = c("second_base_runner" = "player_id")
      ) %>%
      left_join(
        stored_players$data %>% select(player_id, third_base_name = full_name),
        by = c("third_base_runner" = "player_id")
      ) %>%
      select(
        the_inning, game_pitch_number, count, outs, is_pitch_event, pitcher_name, pitchers_pitch_count, pitch_outcome, pitch_velo, 
        pitch_type, hitter_name, at_bat_pitch_number, ball_in_play_outcome, batter_outcome, ball_flight, contact_type, first_base_name, first_base_outcome, 
        first_base_reason, second_base_name, second_base_outcome, second_base_reason, third_base_name, third_base_outcome, third_base_reason
      )
    
    # Drop any columns that will not show up depending on the inputs at the beginning
    if (practice_state$have_runners == "No") {
      data <- data %>%
        select(-any_of(c("first_base_name", "first_base_outcome", "first_base_reason", "second_base_name", 
                         "second_base_outcome", "second_base_reason", "third_base_name", "third_base_outcome", "third_base_reason")))
    }
    if (practice_state$have_innings == "No") {
      data <- data %>%
        select(-any_of(c("the_inning")))
    }
    if (practice_state$have_bip_outcome == "No") {
      data <- data %>%
        select(-any_of(c("ball_in_play_outcome", "batter_outcome")))
    }
    
    DT::datatable(
      data,
      options = list(
        pageLength = 20,
        orderClasses = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  ## ---- Pitch Outcome Buttons ----
  # Drop Third Strike (Shows Pop Up)
  observeEvent(input$practice_drop_third_strike_pop_up, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "How was the drop third strike initiated?",
      "You will be able to advance runners on the next page.",
      easyClose = FALSE,
      footer = tagList(
        actionButton("practice_drop_third_swinging", "Swinging Strike"),
        actionButton("practice_drop_third_called", "Called Strike"),
        modalButton("Cancel")
      )
    ))
  })
  # Drop Third Swinging
  observeEvent(input$practice_drop_third_swinging, {
    removeModal()
    ball_in_play_data_reset()
    record_practice_pitch("Drop Third Swinging")
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Update Runner Outcome Selections based on situation
    the_hitter_choices <- c("Drop Third Swinging")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    
    # Pop up to decide where runners go
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("practice_reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("practice_outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("practice_reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("practice_outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("practice_reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("practice_outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("practice_reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("practice_outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("practice_confirm_advance", "Confirm Change", class = "btn btn-default")
      ),
      size = "l"
    ))
    
    update_practice_count_and_inning(reset = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  #Drop Third Called
  observeEvent(input$practice_drop_third_called, {
    removeModal()
    ball_in_play_data_reset()
    record_practice_pitch("Drop Third Called")
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Update Runner Outcome Selections based on situation
    the_hitter_choices <- c("Drop Third Called")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    
    # Pop Up to decide where runners go
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("practice_reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("practice_outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("practice_reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("practice_outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("practice_reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("practice_outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("practice_reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("practice_outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("practice_confirm_advance", "Confirm Change", class = "btn btn-default")
      ),
      size = "l"
    ))
    
    resetPitchInputs()
    update_practice_count_and_inning(reset = TRUE)
  }, ignoreInit = TRUE)
  
  # Called Strike
  observeEvent(input$practice_called_strike, {
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (is.na(id_pitcher) || is.null(id_pitcher) || input$practice_pitcher == "" || input$practice_pitcher == "Choose Player") {
      showNotification("Please select a pitcher for this at bat", type = "error")
      return()
    }
    if (is.na(id_hitter) || is.null(id_hitter) || input$practice_batter == "" || input$practice_batter == "Choose Player") {
      showNotification("Please select a hitter for this at bat", type = "error")
      return()
    }
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_practice_pitch("Called Strike")
    update_practice_count_and_inning(is_strike = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Swinging Strike
  observeEvent(input$practice_swing_strike, {
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (is.na(id_pitcher) || is.null(id_pitcher) || input$practice_pitcher == "" || input$practice_pitcher == "Choose Player") {
      showNotification("Please select a pitcher for this at bat", type = "error")
      return()
    }
    if (is.na(id_hitter) || is.null(id_hitter) || input$practice_batter == "" || input$practice_batter == "Choose Player") {
      showNotification("Please select a hitter for this at bat", type = "error")
      return()
    }
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_practice_pitch("Swinging Strike")
    update_practice_count_and_inning(is_strike = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Foul Ball
  observeEvent(input$practice_foul_ball, {
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (is.na(id_pitcher) || is.null(id_pitcher) || input$practice_pitcher == "" || input$practice_pitcher == "Choose Player") {
      showNotification("Please select a pitcher for this at bat", type = "error")
      return()
    }
    if (is.na(id_hitter) || is.null(id_hitter) || input$practice_batter == "" || input$practice_batter == "Choose Player") {
      showNotification("Please select a hitter for this at bat", type = "error")
      return()
    }
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_practice_pitch("Foul Ball")
    update_practice_count_and_inning(is_foul = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Ball
  observeEvent(input$practice_ball, {
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (is.na(id_pitcher) || is.null(id_pitcher) || input$practice_pitcher == "" || input$practice_pitcher == "Choose Player") {
      showNotification("Please select a pitcher for this at bat", type = "error")
      return()
    }
    if (is.na(id_hitter) || is.null(id_hitter) || input$practice_batter == "" || input$practice_batter == "Choose Player") {
      showNotification("Please select a hitter for this at bat", type = "error")
      return()
    }
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_practice_pitch("Ball")
    update_practice_count_and_inning(is_ball = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Hit By Pitch
  observeEvent(input$practice_hit_by_pitch, {
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (is.na(id_pitcher) || is.null(id_pitcher) || input$practice_pitcher == "" || input$practice_pitcher == "Choose Player") {
      showNotification("Please select a pitcher for this at bat", type = "error")
      return()
    }
    if (is.na(id_hitter) || is.null(id_hitter) || input$practice_batter == "" || input$practice_batter == "Choose Player") {
      showNotification("Please select a hitter for this at bat", type = "error")
      return()
    }
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_practice_pitch("Hit By Pitch")
    
    # If keeping track of runners - Then runners will need to advance, will move them automatically
    if (practice_state$have_runners == "Yes") {
      batter_team <- ifelse(game_state$half == "Top", "away_lineup", "home_lineup")
      if (!is.na(game_state$runners$third & game_state$runners$second & game_state$runners$first)) {
        game_state$runners$third <- game_state$runners$second
        game_state$runners$second <- game_state$runners$first
        game_state$runners$first <- id_hitter
        game_state[[batter_team]] <- game_state[[batter_team]] + 1
      } else if (!is.na(game_state$runners$second & game_state$runners$first)) {
        game_state$runners$third <- game_state$runners$second
        game_state$runners$second <- game_state$runners$first
        game_state$runners$first <- id_hitter
      } else if (!is.na(game_state$runners$first)) {
        game_state$runners$second <- game_state$runners$first
        game_state$runners$first <- id_hitter
      } else {
        game_state$runners$first <- id_hitter
      } 
    }
    game_state$current_hitter <- NA_real_
    update_practice_count_and_inning(reset = TRUE)
    pitch_update_trigger(pitch_update_trigger() + 1)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  
  ## ---- Ball In Play Functioning ----
  # Practice ball in play functioning
  observeEvent(input$practice_ball_in_play, {
    # Make sure there is a current batter and current hitter
    if (any(c(input$practice_pitcher, input$practice_batter) == "Choose Player")) {
      showNotification("Make sure that a pitcher and hitter are choosen!", type = "error")
      return()
    }
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    if (id_pitcher == id_hitter) {
      showNotification("The pitcher cannot hit and pitch at the same time, make sure to change one.", type = "error")
      return()
    }
    if (id_hitter %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third)) {
      showNotification("The batter is currently on base. Replace player on base with a ghost runner if you want this player to bat.", type = "error")
      return()
    }
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    
    if (practice_state$have_runners == "Yes") {
      # Count runners on base
      runners_on <- sum(!is.na(game_state$runners$first), 
                        !is.na(game_state$runners$second), 
                        !is.na(game_state$runners$third))
      # Add more outcomes based on situation
      if (runners_on >= 1 && game_state$outs < 2) {
        available_outcomes <- c(available_outcomes, "Double Play", "Sacrifice Fly")
      }
      if (runners_on >= 1) {
        available_outcomes <- c(available_outcomes, "Fielder's Choice")
      }
      if (runners_on >= 2 && game_state$outs < 1) {
        available_outcomes <- c(available_outcomes, "Triple Play")
      }
    }
    
    if (practice_state$have_bip_outcome == "No") {
      # Show Ui with no ball in play options, or hit coordinates
      showModal(modalDialog(
        title = "What happened on the swing",
        
        h4("Ball Flight"),
        radioButtons(
          inputId = "practice_selected_ball_flight",
          label = NULL,
          choices = ball_flight_types,
          selected = ball_flight_types[1],
          inline = TRUE
        ),
        
        h4("Contact Quality"),
        radioButtons(
          inputId = "practice_selected_contact_quality",
          label = NULL,
          choices = contact_quality_types,
          selected = contact_quality_types[1],
          inline = TRUE
        ),
        
        footer = tagList(
          actionButton("practice_confirm_hit", "Confirm"),
          actionButton("cancel_modal", "Cancel", class = "btn btn-default")
        ),
        
        easyClose = FALSE,
        size = "m"
      ))
    } else {
      # Show Ball In Play Ui with runner outcomes
      showModal(modalDialog(
        title = "Ball In Play Details",
        tags$style(HTML("
      /* Target radio button inputs */
      .shiny-options-group input[type='radio'] {
        width: 15px;    /* Increase circle width */
        height: 15px;   /* Increase circle height */
        margin-right: 8px; /* Adjust spacing between circle and label */
      }
      /* Optional: Increase label font size for balance */
      .shiny-options-group label {
        font-size: 16px;
        vertical-align: middle;
      }
      /* Ensure inline layout stays clean */
      .shiny-options-group {
        margin-bottom: 10px;
      }
    ")),
        plotOutput("practice_hit_field", click = "practice_hit_field_click", height = "600px", width = "600px"),
        
        h4("Ball Flight"),
        radioButtons(
          inputId = "practice_selected_ball_flight",
          label = NULL,
          choices = ball_flight_types,
          selected = ball_flight_types[1],
          inline = TRUE
        ),
        
        h4("Contact Quality"),
        radioButtons(
          inputId = "practice_selected_contact_quality",
          label = NULL,
          choices = contact_quality_types,
          selected = contact_quality_types[1],
          inline = TRUE
        ),
        
        conditionalPanel(
          condition = "practice_state.have_bip_outcome == 'Yes'",
          tagList(
            h4("Outcome"),
            radioButtons(
              inputId = "practice_selected_hit_outcome",
              label = NULL,
              choices = available_outcomes,
              selected = available_outcomes[1],
              inline = TRUE
            )
          )
        ),
        uiOutput("practice_BIP_runner_ui_output"),
        actionButton("practice_confirm_hit", "Confirm"),
        actionButton("cancel_modal", "Cancel", class = "btn btn-default"),
        footer = NULL,
        easyClose = TRUE,
        size = "l"
      ))
      # reset hit_location$coords
      hit_location$coords <- data.frame(x = numeric(), y = numeric())
    }
  }, ignoreInit = TRUE)
  
  # When runners are on base, this is what shows the dropdrop downs, only for bases that have runners occupy them
  output$practice_BIP_runner_ui_output <- renderUI({
    req(input$practice_selected_hit_outcome)
    req(practice_state$have_runners)
    
    if (practice_state$have_runners == "Yes") {
      outcome <- input$practice_selected_hit_outcome
      
      # Define possible runner destinations based on hit outcome
      runner_options <- list(
        first = if (!is.na(game_state$runners$first)) {
          num_runners <- sum(!sapply(game_state$runners, is.na))
          switch(outcome,
                 "Double Play" = {
                   if (num_runners == 1) {
                     c("Out")
                   } else if (game_state$outs == 1) {
                     c("Stay on First", "Second", "Home", "Out")
                   } else {
                     c("Out", "Stay on First", "Second", "Third", "Home")
                   }
                 },
                 "Triple Play" = {
                   if (num_runners == 2) {
                     c("Out")
                   } else {
                     c("Out", "Stay on First", "Second", "Third", "Home")
                   }
                 },
                 "Fielder's Choice" = c("Second", "Third", "Home", "Out"),
                 "Single" = c("Second", "Third", "Home", "Out"),
                 "Double" = c("Third", "Home", "Out"),
                 "Triple" = c("Home", "Out"),
                 "Homerun" = c("Home"), 
                 "Error" = c("Second", "Third", "Home", "Out"),
                 "Out" = c("Stay on First", "Second", "Third", "Home", "Out"),
                 "Sacrifice Fly" = c("Stay on First", "Second", "Third", "Home", "Out"),
                 c("Stay on First", "Second", "Third", "Home", "Out")
          )
        },
        second = if (!is.na(game_state$runners$second)) {
          num_runners <- sum(!sapply(game_state$runners, is.na))
          switch(outcome,
                 "Double Play" = {
                   if (num_runners == 1) {
                     c("Out")
                   } else if (game_state$outs == 1) {
                     c("Stay on Second", "Third", "Home", "Out")
                   } else {
                     c("Stay on Second", "Third", "Home", "Out")
                   }
                 },
                 "Triple Play" = {
                   if (num_runners == 2) {
                     c("Out")
                   } else {
                     c("Stay on Second", "Home", "Out")
                   }
                 },
                 "Fielder's Choice" = c("Stay on Second", "Third", "Home", "Out"),
                 "Single" = c("Stay on Second", "Third", "Home", "Out"),
                 "Double" = c("Third", "Home", "Out"),
                 "Triple" = c("Home", "Out"),
                 "Homerun" = c("Home"),
                 "Error" = c("Stay on Second", "Third", "Home", "Out"),
                 "Out" = c("Stay on Second", "Third", "Home", "Out"),
                 "Sacrifice Fly" = c("Stay on Second", "Third", "Home", "Out"),
                 c("Stay on Second", "Third", "Home", "Out")
          )
        },
        third = if (!is.na(game_state$runners$third)) {
          num_runners <- sum(!sapply(game_state$runners, is.na))
          switch(outcome,
                 "Double Play" = {
                   if (num_runners == 1) {
                     c("Out")
                   } else {
                     c("Stay on Third", "Home", "Out")
                   }
                 },
                 "Triple Play" = {
                   if (num_runners == 2) {
                     c("Out")
                   } else {
                     c("Stay on Third", "Out")
                   }
                 },
                 "Fielder's Choice" = c("Stay on Third", "Home", "Out"),
                 "Single" = c("Stay on Third", "Home", "Out"),
                 "Double" = c("Stay on Third", "Home", "Out"),
                 "Triple" = c("Home", "Out"),
                 "Homerun" = c("Home"),
                 "Error" = c("Stay on Third", "Home", "Out"),
                 "Out" = c("Stay on Third", "Home", "Out"),
                 "Sacrifice Fly" = c("Home"),
                 c("Stay on Third", "Home", "Out")
          )
        }
      )
      
      # Generate UI elements, only when runner occupies base during that pitch
      ui_elements <- list()
      if (!is.na(game_state$runners$first)) {
        ui_elements[[1]] <- selectInput("practice_first_advance", "Runner on First Outcome:",
                                        choices = runner_options$first,
                                        selected = runner_options$first[1])
      }
      if (!is.na(game_state$runners$second)) {
        ui_elements[[2]] <- selectInput("practice_second_advance", "Runner on Second Outcome:",
                                        choices = runner_options$second,
                                        selected = runner_options$second[1])
      }
      if (!is.na(game_state$runners$third)) {
        ui_elements[[3]] <- selectInput("practice_third_advance", "Runner on Third Outcome:",
                                        choices = runner_options$third,
                                        selected = runner_options$third[1])
      }
      
      # Return the UI elements
      do.call(tagList, ui_elements)
    } else {
      NULL
    }
  })
  
  # Observe hit on the field
  observeEvent(input$practice_hit_field_click, {
    hit_location$coords <- data.frame(x = input$practice_hit_field_click$x, 
                                      y = input$practice_hit_field_click$y)
  })
  
  # Confirm Hit
  observeEvent(input$practice_confirm_hit, {
    if (practice_state$have_bip_outcome == "Yes") {
      if (nrow(hit_location$coords) == 0) {
        showNotification("Make sure to click a location on the spray chart!", type = "error")
        return()
      }
    }
    req(input$practice_selected_ball_flight, 
        input$practice_selected_contact_quality)
    if (practice_state$have_bip_outcome == "Yes") {
      req(input$practice_selected_hit_outcome)
    }
    
    # Validation 1: If one is "Bunt", both must be "Bunt"
    if ((input$practice_selected_contact_quality == "Bunt" && input$practice_selected_ball_flight != "Bunt") ||
        (input$practice_selected_contact_quality != "Bunt" && input$practice_selected_ball_flight == "Bunt")) {
      showNotification("If the player bunted, both hit outcome and ball flight must be 'Bunt'", type = "error")
      return()
    }
    
    # Validations that require runners to be tracked
    if (practice_state$have_runners == "Yes") {
      # Store previous runners
      prev_runners <- list(
        first = if (is.na(game_state$runners$first)) NA_real_ else game_state$runners$first,
        second = if (is.na(game_state$runners$second)) NA_real_ else game_state$runners$second,
        third = if (is.na(game_state$runners$third)) NA_real_ else game_state$runners$third,
        outcome_first = NA_character_,
        outcome_second = NA_character_,
        outcome_third = NA_character_,
        reason_first = NA_character_,
        reason_second = NA_character_,
        reason_third = NA_character_
      )
      
      # Validation 2: Sac fly required if someone scored on a fly ball, but also allows for sac bunt to happen
      is_fly_out <- isTRUE(input$practice_selected_hit_outcome == "Out") &&
        isTRUE(input$practice_selected_contact_quality != "Bunt") &&
        !isTRUE(input$practice_selected_ball_flight %in% c("Bunt", "Ground Ball"))
      
      runner_scored <- (!is.null(input$practice_first_advance) && input$practice_first_advance == "Home") ||
        (!is.null(input$practice_second_advance) && input$practice_second_advance == "Home") ||
        (!is.null(input$practice_third_advance) && input$practice_third_advance == "Home")
      
      if (is_fly_out && runner_scored) {
        showNotification("Use 'Sac Fly' instead of 'Out' when a runner scores on a fly ball.", type = "error")
        return()
      }
      
      # Set up for val 3 & 4
      runner_values <- setNames(
        c(
          if (is.null(input$practice_selected_hit_outcome)) 5 else position_map[input$practice_selected_hit_outcome],
          if (is.na(game_state$runners$first)) 5 else position_map[input$practice_first_advance],
          if (is.na(game_state$runners$second)) 5 else position_map[input$practice_second_advance],
          if (is.na(game_state$runners$third)) 5 else position_map[input$practice_third_advance],
          if (is.na(prev_runners$first)) 5 else 1,
          if (is.na(prev_runners$second)) 5 else 2,
          if (is.na(prev_runners$third)) 5 else 3
        ),
        c("batter", "first", "second", "third", "prev_first", "prev_second", "prev_third")
      )
      # Checking for runner outcomes - debugging
      runner_order <- c(runner_values["batter"], runner_values["first"], runner_values["second"], runner_values["third"])
      print(runner_order)
      
      # Valuation 3: Illegal passing of runners
      if (any(runner_values["batter"] > runner_values[c("first", "second", "third")] & runner_values["batter"] != 5) ||
          any(runner_values["first"] > runner_values[c("second", "third")] & runner_values["first"] != 5) ||
          runner_values["second"] > runner_values["third"] && runner_values["second"] != 5) {
        showNotification("You cannot have runners pass each other!", type = "error")
        return()
      }
      
      # Valuation 4: Runners going to same base
      if (
        any(c(
          (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["first"]) && runner_values["first"] < 4),
          (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["second"]) && runner_values["second"] < 4),
          (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["third"]) && runner_values["third"] < 4),
          (runner_values["first"] < 4 && (runner_values["first"] == runner_values["second"]) && runner_values["second"] < 4),
          (runner_values["first"] < 4 && (runner_values["first"] == runner_values["third"]) && runner_values["third"] < 4),
          (runner_values["second"] < 4 && (runner_values["second"] == runner_values["third"]) && runner_values["third"] < 4)
        ))) {
        showNotification("Runners cannot end up at the same base, make sure to adjust that!", type = "error")
        return()
      }
      
      #Number of outs on Bases
      sum_of_outs <- sum(
        if (!is.na(prev_runners$first)) ifelse(input$practice_first_advance == "Out", 1, 0) else 0,
        if (!is.na(prev_runners$second)) ifelse(input$practice_second_advance == "Out", 1, 0) else 0,
        if (!is.na(prev_runners$third)) ifelse(input$practice_third_advance == "Out", 1, 0) else 0
      )
      
      # Validation 5: Too many people are being called out for the play (NEEDS WORK)
      if ((sum_of_outs > 2 && input$practice_selected_hit_outcome == "Double Play") ||
          (sum_of_outs > 3 && input$practice_selected_hit_outcome == "Triple Play")) {
        showNotification("Too many players are getting out for this game situation", type = "error")
        return()
      }
      
      # Validation 6: Not enough people are being called out for this play
      if ((sum_of_outs < 2 && input$practice_selected_hit_outcome == "Triple Play") ||
          sum_of_outs == 0 && input$practice_selected_hit_outcome == "Double Play" ||
          sum_of_outs > 2 && input$practice_selected_hit_outcome == "Double Play") {
        showNotification("Not enough players are getting out for this hit outcome", type = "error")
        return()
      }
    }
    
    # Remove pop-up
    removeModal()
    
    # These two are always an input
    ball_in_play_data$ball_flight <- input$practice_selected_ball_flight
    ball_in_play_data$contact_type <- input$practice_selected_contact_quality
    # The reset are not always an input
    if (practice_state$have_bip_outcome == "Yes") {
      # Update ball_in_play_data directly
      ball_in_play_data$ball_in_play_outcome <- input$practice_selected_hit_outcome
      ball_in_play_data$spray_chart_x <- hit_location$coords$x
      ball_in_play_data$spray_chart_y <- hit_location$coords$y
    } else {
      ball_in_play_data$ball_in_play_outcome <- "None"
      ball_in_play_data$spray_chart_x <- NA_real_
      ball_in_play_data$spray_chart_y <- NA_real_
    }
    
    # Record Pitch
    pitch_record <- record_practice_pitch("Ball In Play") 
    
    # Clear game state runners
    game_state$runners$first <- NA_real_
    game_state$runners$second <- NA_real_
    game_state$runners$third <- NA_real_
    
    # Updating runner locations and outcomes - Need have_runners == "Yes"
    if (practice_state$have_runners == "Yes") {
      # Determine which team's score to update
      batting_team_score <- if (game_state$half == "None") {
        "home_score"
      } else if (game_state$half == "Top") {
        "away_score"
      } else {
        "home_score"
      }
      
      # Needs Cage Ball when Ball In Play outcome == NO, because it will mess up stats otherwise
      outcome <- if (practice_state$have_bip_outcome == "No") {
        "Cage Ball"
      } else {
        input$practice_selected_hit_outcome
      }
      
      #Third Base Runner outcome and game_state changes
      if (!is.na(prev_runners$third) && !is.null(input$practice_third_advance)) {
        if (input$practice_third_advance == "Home") {
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          prev_runners$outcome_third <- "Scored"
        } else if (input$practice_third_advance == "Out") {
          game_state$outs <- game_state$outs + 1
          prev_runners$outcome_third <- "Out"
        } else {
          game_state$runners$third <- prev_runners$third
          prev_runners$outcome_third <- "Stayed"
        }
      }
      
      #Second Base Runner outcome and game_state changes
      if (!is.na(prev_runners$second) && !is.null(input$practice_second_advance)) {
        if (input$practice_second_advance == "Home") {
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          prev_runners$outcome_second <- "Scored"
        } else if (input$practice_second_advance == "Third") {
          game_state$runners$third <- prev_runners$second
          prev_runners$outcome_second <- "Third"
        } else if (input$practice_second_advance == "Out") {
          game_state$outs <- game_state$outs + 1
          prev_runners$outcome_second <- "Out"
        } else {
          game_state$runners$second <- prev_runners$second
          prev_runners$outcome_second <- "Stayed"
        }
      }
      
      #First Base Runner outcome and game_state changes
      if (!is.na(prev_runners$first) && !is.null(input$practice_first_advance)) {
        if (input$practice_first_advance == "Home") {
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          prev_runners$outcome_first <- "Scored"
        } else if (input$practice_first_advance == "Third") {
          game_state$runners$third <- prev_runners$first
          prev_runners$outcome_first <- "Third"
        } else if (input$practice_first_advance == "Second") {
          game_state$runners$second <- prev_runners$first
          prev_runners$outcome_first <- "Second"  
        } else if (input$practice_first_advance == "Out") {
          game_state$outs <- game_state$outs + 1
          prev_runners$outcome_first <- "Out"
        } else {
          game_state$runners$first <- prev_runners$first
          prev_runners$outcome_first <- "Stayed"
        }
      }
      # Outcome for hitter and reasons for runners movement
      if (outcome %in% c("Error", "Fielder's Choice")) {
        write_outcome <- ifelse(outcome == "Error", "E", "FC")
        prev_runners$reason_first <- ifelse(!is.na(prev_runners$first), write_outcome, NA_character_)
        prev_runners$reason_second <- ifelse(!is.na(prev_runners$second), write_outcome, NA_character_)
        prev_runners$reason_third <- ifelse(!is.na(prev_runners$third), write_outcome, NA_character_)
        game_state$runners$first <- game_state$current_hitter
        
      } else {
        prev_runners$reason_first <- ifelse(!is.na(prev_runners$first), "BIP", NA_character_)
        prev_runners$reason_second <- ifelse(!is.na(prev_runners$second), "BIP", NA_character_)
        prev_runners$reason_third <- ifelse(!is.na(prev_runners$third), "BIP", NA_character_)
        
        if (outcome == "Single") {
          game_state$runners$first <- game_state$current_hitter
        } else if (outcome == "Double") {
          game_state$runners$second <- game_state$current_hitter
        } else if (outcome == "Triple") {
          game_state$runners$third <- game_state$current_hitter
        } else if (outcome == "Homerun") {
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        } else if (outcome == "Double Play") {
          if (sum_of_outs == 1) {
            game_state$outs <- game_state$outs + 1
          } else {
            game_state$runners$first <- game_state$current_hitter
          }
        } else if (outcome == "Triple Play") {
          game_state$outs <- 3
        } else if (outcome == "Cage Ball") {
          # Do nothing
        } else {
          game_state$outs <- game_state$outs + 1
        }
      }
      
      if (practice_state$have_runners == "Yes") {
        # Check if there was a bunt, then check if it qualifies as a sac bunt
        if (input$practice_selected_contact_quality == "Bunt" && input$practice_selected_ball_flight == "Bunt") {
          if (
            (runner_values["first"] != 5 && runner_values["prev_first"] != 5 &&
             runner_values["first"] > runner_values["prev_first"]) ||
            
            (runner_values["second"] != 5 && runner_values["prev_second"] != 5 &&
             runner_values["second"] > runner_values["prev_second"]) ||
            
            (runner_values["third"] != 5 && runner_values["prev_third"] != 5 &&
             runner_values["third"] > runner_values["prev_third"])
          ) {
            # If it qualifies, change the outcome to Sacrifice Bunt
            ball_in_play_data$ball_in_play_outcome <- "Sacrifice Bunt"
          }
        }
        
        # Update the runner outcomes and if it was Sacrifice Bunt, otherwise data will stay the same
        dbExecute(connect_to_db, "
    UPDATE global_pitch_data
    SET ball_in_play_outcome = $1, first_base_outcome = $2, second_base_outcome = $3, third_base_outcome = $4, first_base_reason = $5, second_base_reason = $6, third_base_reason = $7
    WHERE game_pitch_number = $8 AND event_id = $9",
                  params = list(ball_in_play_data$ball_in_play_outcome, prev_runners$outcome_first, prev_runners$outcome_second, 
                                prev_runners$outcome_third, prev_runners$reason_first, prev_runners$reason_second, 
                                prev_runners$reason_third, game_state$pitch_count, game_info$event_id))
      }
    } else {
      # If runners are no being tracked, it clears the bases again
      game_state$runners$first <- NA_real_
      game_state$runners$second <- NA_real_
      game_state$runners$third <- NA_real_
    }
    
    # Reset batter and reset pitch state
    game_state$current_hitter <- NA_real_
    update_practice_count_and_inning(reset = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Practice Ball in Play plot
  output$practice_hit_field <- renderPlot({
    p2 <- draw_baseball_field()
    
    # Give point, shape and color
    if (nrow(hit_location$coords) > 0) {
      flight_color <- switch(input$practice_selected_ball_flight %||% "default",
                             "Bunt" = "black",
                             "Fly Ball" = "#FFD700",
                             "Ground Ball" = "#FF4500",
                             "Line Drive" = "#00CED1",
                             "Pop-Up" = "#FF00FF",
                             "default" = "#FFFFFF")
      contact_shape <- switch(input$practice_selected_contact_quality %||% "default",
                              "Bunt" = 16,
                              "Weak" = 16,
                              "Average" = 17,
                              "Hard" = 15,
                              "default" = 16)
      
      p2 <- p2 + geom_point(data = hit_location$coords, 
                            aes(x = x, y = y), 
                            color = flight_color, 
                            shape = contact_shape, 
                            size = 5)
    }
    
    p2
  })
  
  ## ---- Runners Advanced Functioning ----
  # Runner Advance shows up only when have_runners == "Yes
  output$the_practice_runners <- renderUI({
    req(app_state$page == "practice_event")
    if (practice_state$have_runners == "Yes") {
      tagList(
        actionButton("practice_runners_advance", "Runners Advance", class = "wide-btn")
      )
    } else {
      NULL
    }
  })
  
  # Updating the select drop downs on advance runner pop up
  observeEvent(input$practice_reason_hitter, { # hitter
    req(input$practice_reason_hitter)
    
    new_choices <- switch(input$practice_reason_hitter,
                          "None" = c("None"),
                          "Other (Interference)" = c("First", "Out"),
                          "Drop Third Called" = c("First", "Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("First", "Second", "Third", "Home", "Out"))
    updateSelectInput(session, "practice_outcome_hitter", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$practice_reason_first, { # first base
    req(input$practice_reason_first)
    
    new_choices <- switch(input$practice_reason_first,
                          "Choose Option" = c("None"),
                          "Drop Third Called" = c("Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Second", "Third", "Home", "Out"),
                          "Steal" = c("Second", "Third", "Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Second", "Third", "Home", "Out"),
                          "Wild Pitch" = c("Second", "Third", "Home", "Out"), 
                          "Pass Ball" = c("Second", "Third", "Home", "Out"),
                          "Balk" = c("Second"),
                          "Other (Interference)" = c("Second", "Out"),
                          "Out of Bounds" = c("Second")
    )
    updateSelectInput(session, "practice_outcome_first", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$practice_reason_second, { # second base
    req(input$practice_reason_second)
    
    new_choices <- switch(input$practice_reason_second,
                          "Choose Option" = "None", 
                          "Drop Third Called" = c("Stay on Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Stay on Second", "Third", "Home", "Out"),
                          "Steal" = c("Third", "Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Third", "Home", "Out"), 
                          "Wild Pitch" = c("Third", "Home", "Out"), 
                          "Pass Ball" = c("Third", "Home", "Out"),
                          "Balk" = c("Third"),
                          "Other (Interference)" = c("Third", "Out"),
                          "Out of Bounds" = c("Third")
    )
    updateSelectInput(session, "practice_outcome_second", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$practice_reason_third, { # third base
    req(input$practice_reason_third)
    
    new_choices <- switch(input$practice_reason_third,
                          "Choose Option" = "None", 
                          "Drop Third Called" = c("Stay on Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Stay on Third", "Home", "Out"),
                          "Steal" = c("Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Home", "Out"), 
                          "Wild Pitch" = c("Home", "Out"), 
                          "Pass Ball" = c("Home", "Out"),
                          "Balk" = c("Home"),
                          "Other (Interference)" = c("Home", "Out"),
                          "Out of Bounds" = c("Home")
    )
    updateSelectInput(session, "practice_outcome_third", choices = new_choices, selected = new_choices[1])
  })
  
  
  # Runners Advance Modal (one-time show)
  observeEvent(input$practice_runners_advance, {
    
    # Make sure there is a current pitcher
    if (practice_state$have_runners == "No" || input$practice_pitcher == "Choose Player" ||
        is.na(game_state$current_pitcher) || game_state$current_pitcher < 10
    ) {
      showNotification("Make sure that a pitcher and hitter are choosen!", type = "warning")
      return()
    }
    
    # Make sure there has been a pitch
    if (is.na(pitch_values$last_pitch_num) || is.null(pitch_values$last_pitch_num)) {
      showNotification("There are no pitches for this game yet", type = "warning")
      return()
    }
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Update drop down choices
    the_hitter_choices <- c("None", "Other (Interference)")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Balk", "Other (Interference)", "Out of Bounds")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Balk", "Other (Interference)", "Out of Bounds")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Balk", "Other (Interference)", "Out of Bounds")
    }
    
    # pop up for determining runner outcomes
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("practice_reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("practice_outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("practice_reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("practice_outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("practice_reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("practice_outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("practice_reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("practice_outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("practice_confirm_advance", "Confirm Change", class = "btn btn-default"),
        actionButton("cancel_modal", "Cancel", class = "btn btn-default")
      ),
      size = "l"
    ))
  })
  
  # Render Field (minimize redraws)
  output$runner_field <- renderPlot({
    p <- draw_baseball_field() +
      xlim(40, 250) +
      ylim(40, 250)
    
    # Add occupied bases
    if (nrow(runner_plot_data$occupied) > 0) {
      p <- p + geom_point(data = runner_plot_data$occupied, aes(x = x, y = y), color = "yellow", size = 8, shape = 15)
    }
    p
  }, height = 400, width = 400)
  
  # Confirm Advance
  observeEvent(input$practice_confirm_advance, {
    id_hitter_num <- last_pitch_reactive()$hitter_id
    outcome_batter <- if (input$practice_outcome_hitter == "Home") { # weird bug but need Home changed to Score in this situation
      "Scored"
    } else {
      input$practice_outcome_hitter
    }
    if (input$practice_reason_hitter %in% c("None","Choose Option") &&
        input$practice_reason_first %in% c("Empty","Choose Option") &&
        input$practice_reason_second %in% c("Empty","Choose Option") &&
        input$practice_reason_third %in% c("Empty","Choose Option")) {
      # Makes sure that something is choosen before running
      showNotification("Please update at least one runner or the hitter.", type = "error")
      return()
    } else if (id_hitter_num %in% c(game_state$runners$first, game_state$runners$second, game_state$runners$third) && input$practice_reason_hitter != "None") {
      # Batter cannot go on base if he is already on base somewhere
      showNotification("You cannot move this batter to first, the batter is on base right now. Switch batter or change the runner to a ghost runner", type = "error")
      return()
    } else {
      # Only require batter if the batter is involved in the advance
      if (!input$practice_reason_hitter %in% c("None","Choose Option") &&
          input$practice_reason_first %in% c("Empty","Choose Option") &&
          input$practice_reason_second %in% c("Empty","Choose Option") &&
          input$practice_reason_third %in% c("Empty","Choose Option")) {
        if (is.na(game_state$current_hitter) || game_state$current_hitter < 0 || input$practice_batter == "Choose Player") {
          # If batter gets on base, then we need to make sure a batter is choosen
          showNotification("Make sure that you have choosen a batter on the input page!", type = "error")
          return()
        }
      }
      # Store previous runners
      prev_runners <- list(
        first = if (is.na(game_state$runners$first)) NA_real_ else game_state$runners$first,
        second = if (is.na(game_state$runners$second)) NA_real_ else game_state$runners$second,
        third = if (is.na(game_state$runners$third)) NA_real_ else game_state$runners$third,
        outcome_first = NA_character_,
        outcome_second = NA_character_,
        outcome_third = NA_character_,
        reason_first = NA_character_,
        reason_second = NA_character_,
        reason_third = NA_character_
      )
      
      # Reasons why runners are advancing
      practice_selected_reasons <- c(input$practice_reason_first, input$practice_reason_second, input$practice_reason_third)
      
      # Valuation 1: Wild Pitch and Pass Ball need to have the last event be a pitch
      if (any(practice_selected_reasons %in% c("Wild Pitch", "Pass Ball")) &&
          !(last_pitch_reactive()$pitch_outcome %in% c("Ball", "Called Strike", "Swinging Strike"))) {
        showNotification("For Wild Pitch and Pass Ball, the last event has to be a pitch.", type = "error")
        return()
      }
      
      # Set up for val 2 & 3
      runner_values <- setNames(
        c(
          if (input$practice_reason_hitter == "None") 5 else position_map[input$practice_outcome_hitter],
          if (input$practice_outcome_first %in% c("Second", "Third", "Home", "Out")) position_map[input$practice_outcome_first] else 5,
          if (input$practice_outcome_second %in% c("Third", "Home", "Out")) position_map[input$practice_outcome_second] else 5,
          if (input$practice_outcome_third %in% c("Home", "Out")) position_map[input$practice_outcome_third] else 5
        ),
        c("batter", "first", "second", "third")
      )
      
      # Used for debugging, to see where batter and runners are going (not needed)
      runner_order <- c(runner_values["batter"], runner_values["first"], runner_values["second"], runner_values["third"])
      print(runner_order)
      
      # Valuation 2: Illegal passing of runners
      if (any(runner_values["batter"] > runner_values[c("first", "second", "third")] & runner_values["batter"] != 5) ||
          any(runner_values["first"] > runner_values[c("second", "third")] & runner_values["first"] != 5) ||
          runner_values["second"] > runner_values["third"] && runner_values["second"] != 5) {
        showNotification("You cannot have runners pass each other!", type = "error")
        return()
      }
      
      # Valuation 3: Runners going to same base
      if (
        any(c(
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["first"] && runner_values["first"] < 4,
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["second"] && runner_values["second"] < 4,
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["third"] && runner_values["third"] < 4,
          runner_values["first"] < 4 && runner_values["first"] == runner_values["second"] && runner_values["second"] < 4,
          runner_values["first"] < 4 && runner_values["first"] == runner_values["third"] && runner_values["third"] < 4,
          runner_values["second"] < 4 && runner_values["second"] == runner_values["third"] && runner_values["third"] < 4
        ))) {
        showNotification("Runners cannot end up at the same base, make sure to adjust that!", type = "error")
        return()
      }
      
      #Number of outs on Bases
      sum_of_outs <- sum(
        if (input$practice_reason_hitter == "None") 0 else ifelse(input$practice_outcome_hitter == "Out", 1, 0),
        if (input$practice_reason_first %in% c("Empty","Choose Option")) 0 else ifelse(input$practice_outcome_first == "Out", 1, 0),
        if (input$practice_reason_second %in% c("Empty","Choose Option")) 0 else ifelse(input$practice_outcome_second == "Out", 1, 0),
        if (input$practice_reason_third %in% c("Empty","Choose Option")) 0 else ifelse(input$practice_outcome_third == "Out", 1, 0)
      )
      
      # Validation 5: Not enough people are being called out for this play
      if (any(practice_selected_reasons %in% c("Caught Stealing", "Picked Off")) &&
          sum_of_outs == 0) {
        showNotification("Not enough players are getting out for this situation", type = "error")
        return()
      }
      
      # Reset runners - this has to happen and (prev_runners is need because of this)
      game_state$runners$third <- NA_real_
      game_state$runners$second <- NA_real_
      game_state$runners$first <- NA_real_
      
      # Finding who is hitting
      batting_team_score <- if (game_state$half == "None") {
        "home_score" #if innings are not being tracked, you cannot see the score but will always go to home_score just to make functioning easier
      } else if (game_state$half == "Top") {
        "away_score"
      } else {
        "home_score"
      }
      
      # Messes up when players gets out, this is the bug fix
      current_outs <- game_state$outs
      
      #Third outcome and game state changes
      if (!is.na(prev_runners$third)) {
        if (input$practice_reason_third %in% c("Steal", "Caught Stealing", "Picked Off", 
                                               "Error", "Wild Pitch", "Pass Ball",  "Balk",
                                               "Drop Third Called", "Drop Third Swinging",
                                               "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_third <- input$practice_reason_third
          if (input$practice_outcome_third == "Home") {
            prev_runners$outcome_third <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else {
            prev_runners$outcome_third <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_third <- "Stayed"
          prev_runners$reason_third <- "None"
          game_state$runners$third <- prev_runners$third
        }
      }
      
      #Second outcome and game state changes
      if (!is.na(prev_runners$second)) {
        if (input$practice_reason_second %in% c("Steal", "Caught Stealing", "Picked Off", 
                                                "Error", "Wild Pitch", "Pass Ball", "Balk",
                                                "Drop Third Called", "Drop Third Swinging",
                                                "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_second <- input$practice_reason_second
          if (input$practice_outcome_second == "Home") {
            prev_runners$outcome_second <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else if (input$practice_outcome_second == "Third"){
            game_state$runners$third <- prev_runners$second
            prev_runners$outcome_second <- "Third"
          } else {
            prev_runners$outcome_second <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_second <- "Stayed"
          prev_runners$reason_second <- "None"
          game_state$runners$second <- prev_runners$second
        }
      }
      
      #First outcome and game state changes
      if (!is.na(prev_runners$first)) {
        if (input$practice_reason_first %in% c("Steal", "Caught Stealing", "Picked Off", 
                                               "Error", "Wild Pitch", "Pass Ball", "Balk",
                                               "Drop Third Called", "Drop Third Swinging",
                                               "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_first <- input$practice_reason_first
          if (input$practice_outcome_first == "Home") {
            prev_runners$outcome_first <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else if (input$practice_outcome_first == "Third"){
            game_state$runners$third <- prev_runners$first
            prev_runners$outcome_first <- "Third"
          } else if (input$practice_outcome_first == "Second"){
            game_state$runners$second <- prev_runners$first
            prev_runners$outcome_first <- "Second"
          } else {
            prev_runners$outcome_first <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_first <- "Stayed"
          prev_runners$reason_first <- "None"
          game_state$runners$first <- prev_runners$first
        }
      }
      
      # Home outcome and game state changes
      if (input$practice_reason_hitter %in% c("Other (Interference)", "Drop Third Swinging", "Drop Third Called")) {
        if (input$practice_outcome_hitter == "Home") {
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        } else if (input$practice_outcome_hitter == "Third"){
          game_state$runners$third <- id_hitter_num
        } else if (input$practice_outcome_hitter == "Second"){
          game_state$runners$second <- id_hitter_num
        } else if (input$practice_outcome_hitter == "First") {
          game_state$runners$first <- id_hitter_num
        } else {
          game_state$outs <- game_state$outs + 1
        }
      }
      
      #Adding events that happened during the pitch into the database
      if (any(c(input$practice_reason_hitter, input$practice_reason_first, input$practice_reason_second, 
                input$practice_reason_third) %in% c("Wild Pitch", "Pass Ball", "Drop Third Swinging", "Drop Third Called"))) {
        
        # For Drop Third Strikes, Wild Pitch and Pass Ball update the last pitch throw because it was during the last pitch
        dbExecute(connect_to_db, "
          UPDATE global_pitch_data
          SET batter_outcome = $1, first_base_outcome = $2, second_base_outcome = $3, third_base_outcome = $4, first_base_reason = $5, second_base_reason = $6, third_base_reason = $7
          WHERE game_pitch_number = $8 AND event_id = $9",
                  params = list(outcome_batter, prev_runners$outcome_first, prev_runners$outcome_second, prev_runners$outcome_third,
                                prev_runners$reason_first, prev_runners$reason_second, prev_runners$reason_third,
                                game_state$pitch_count, pitch_values$current_event_id))
        
      } else {
        # Everything else was or could be between pitches so we are creating a new row of data
        game_state$pitch_count <- game_state$pitch_count + 1
        if (input$practice_reason_hitter == "Other (Interference)"){
          # This is if there was an interference and batter gets out or goes to first base
          middle_pitch_record_one <- data.frame(
            event_id = pitch_values$current_event_id,
            game_pitch_number = game_state$pitch_count,
            is_pitch_event = FALSE,
            hitter_id = game_state$current_hitter,
            pitcher_id = game_state$current_pitcher,
            pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(game_state$current_pitcher)]],
            at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
            half_inning = if (practice_state$have_innings == "Yes") {
              game_state$half %||% "Top"
            } else {
              "None"
            },
            inning = if (practice_state$have_innings == "Yes") {
              game_state$inning %||% 1
            } else {
              0
            },
            outs = current_outs %||% 0,
            strikes = game_state$strikes %||% 0,
            balls = game_state$balls %||% 0,
            pitch_outcome = input$practice_reason_hitter,
            pitch_velo = NA_real_,
            pitch_type = NA_character_,
            pitch_location_x = NA_real_,
            pitch_location_y = NA_real_,
            ball_in_play_outcome = NA_character_,
            batter_outcome = if (input$practice_outcome_hitter == "Out") {
              "None"
            } else if (input$practice_outcome_hitter == "Home"){
              "Scored"
            } else {
              input$practice_outcome_hitter
            },
            ball_flight = "None",
            contact_type = "None",
            spray_chart_x = NA_real_,
            spray_chart_y = NA_real_,
            first_base_runner = prev_runners$first,
            first_base_outcome = prev_runners$outcome_first,
            first_base_reason = prev_runners$reason_first,
            second_base_runner = prev_runners$second,
            second_base_outcome = prev_runners$outcome_second,
            second_base_reason = prev_runners$reason_second,
            third_base_runner = prev_runners$third,
            third_base_outcome = prev_runners$outcome_third,
            third_base_reason = prev_runners$reason_third,
            stringsAsFactors = FALSE
          )
          
          # Write to database
          dbWriteTable(connect_to_db, "global_pitch_data", middle_pitch_record_one, append = TRUE)
          
        } else {
          # Create New Line of Data for Runner Advancing in other ways outside of the previous ones above
          middle_pitch_record_two <- data.frame(
            event_id = pitch_values$current_event_id,
            game_pitch_number = game_state$pitch_count,
            is_pitch_event = FALSE,
            hitter_id = NA_real_,
            pitcher_id = game_state$current_pitcher,
            pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(game_state$current_pitcher)]],
            at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
            half_inning = if (practice_state$have_innings == "Yes") {
              game_state$half %||% "Top"
            } else {
              "None"
            },
            inning = if (practice_state$have_innings == "Yes") {
              game_state$inning %||% 1
            } else {
              0
            },
            outs = current_outs %||% 0,
            strikes = game_state$strikes %||% 0,
            balls = game_state$balls %||% 0,
            pitch_outcome = "None",
            pitch_velo = NA_real_,
            pitch_type = NA_character_,
            pitch_location_x = NA_real_,
            pitch_location_y = NA_real_,
            ball_in_play_outcome = "None",
            batter_outcome = "None",
            ball_flight = "None",
            contact_type = "None",
            spray_chart_x = NA_real_,
            spray_chart_y = NA_real_,
            first_base_runner = prev_runners$first,
            first_base_outcome = prev_runners$outcome_first,
            first_base_reason = prev_runners$reason_first,
            second_base_runner = prev_runners$second,
            second_base_outcome = prev_runners$outcome_second,
            second_base_reason = prev_runners$reason_second,
            third_base_runner = prev_runners$third,
            third_base_outcome = prev_runners$outcome_third,
            third_base_reason = prev_runners$reason_third,
            stringsAsFactors = FALSE
          )
          
          # Write to database
          dbWriteTable(connect_to_db, "global_pitch_data", middle_pitch_record_two, append = TRUE)
        }  
      }  
      
      # Update the inning, batter, and counts if necessary
      if (input$practice_outcome_hitter == "First"){
        update_practice_count_and_inning(reset = TRUE)
        game_state$current_hitter <- NA_real_
      }
      
      # Trigger global_pitch_data refresh
      pitch_update_trigger(pitch_update_trigger() + 1)
      
      removeModal()
    }
  })
  ## ---- Undo Functioning ----
  # ——— Helpers ————————————————————————————————————————————————
  
  # Safely look up a display name by player_id
  lookup_name <- function(id, df, default = "Choose Player") {
    if (is.na(id)) return(default)
    nm <- df %>% filter(player_id == id) %>% pull(display_name)
    if (length(nm) == 0) default else nm
  }
  
  # Sum how many runners scored in a given record
  compute_scored <- function(rec) {
    sum(
      rec$batter_outcome == "Scored",
      rec$first_base_outcome == "Scored",
      rec$second_base_outcome == "Scored",
      rec$third_base_outcome == "Scored",
      na.rm = TRUE
    ) %||% 0
  }
  
  # Restore all the fields of game_state from the given 'prev' record
  restore_game_state <- function(prev, practice_state, game_state, game_info) {
    with(prev, {
      game_state$inning   <- inning
      game_state$half     <- half_inning
      game_state$outs     <- outs
      game_state$balls    <- balls
      game_state$strikes  <- strikes
      
      # pitcher counts
      if (is_pitch_event) {
        pid <- pitcher_id %||% NA
        if (!is.na(pid)) {
          game_state$pitcher_pitch_count[[as.character(pid)]] <- 
            game_state$pitcher_pitch_count[[as.character(pid)]] - 1
        }
        game_state$at_bat_pitch_number <- max(at_bat_pitch_number - 1, 0)
      }
      
      # runners
      if (practice_state$have_runners == "Yes") {
        game_state$runners$first  <- as.numeric(first_base_runner)
        game_state$runners$second <- as.numeric(second_base_runner)
        game_state$runners$third  <- as.numeric(third_base_runner)
      }
      
      # scores & teams
      if (practice_state$have_innings == "Yes") {
        scored <- compute_scored(prev)
        if (half_inning == "Top") {
          game_state$away_score    <- game_state$away_score - scored
          game_state$pitching_team <- game_info$home_team
          game_state$hitting_team  <- game_info$away_team
        } else {
          game_state$home_score    <- game_state$home_score - scored
          game_state$pitching_team <- game_info$away_team
          game_state$hitting_team  <- game_info$home_team
        }
      }
      
      # current players
      game_state$current_pitcher <- pitcher_id
      game_state$current_hitter  <- hitter_id
      
      # update UI selectors
      updateSelectInput(
        session, "practice_pitcher",
        selected = lookup_name(pitcher_id, game_state$available_pitchers)
      )
      updateSelectInput(
        session, "practice_hitter",
        selected = lookup_name(hitter_id, game_state$available_hitters)
      )
    })
  }
  
  # Delete the last event (pitch or non-pitch) by event_id + pitch_number
  delete_last_event <- function(con, event_id, pitch_num, is_pitch) {
    sql <- "
    DELETE FROM global_pitch_data
     WHERE event_id          = $1
       AND game_pitch_number = $2
       AND is_pitch_event    = $3
  "
    DBI::dbExecute(con, sql, params = list(event_id, pitch_num, is_pitch))
  }
  
  
  # ——— Main observeEvent ——————————————————————————————————————————————
  
  observeEvent(input$practice_undo_pitch, {
    
    # 1) Guard: no pitches yet?
    if (is.na(pitch_values$last_pitch_num) || pitch_values$last_pitch_num <= 0) {
      showNotification("There are no pitches for this game yet", type = "warning")
      return()
    }
    
    # 2) Prevent double-undo
    if (game_state$pitch_count <= 0) {
      showNotification("Nothing left to undo", type = "warning")
      return()
    }
    
    # 3) Determine which record to use
    is_pitch <- nrow(last_record_reactive()) == 0
    prev     <- if (is_pitch) last_pitch_reactive() else last_record_reactive()
    event_id <- pitch_values$current_event_id
    pitch_num <- if (is_pitch) pitch_values$last_pitch_num else pitch_values$last_non_pitch_num
    
    # 4) DB transaction + state rollback
    tryCatch({
      DBI::dbBegin(connect_to_db)
      
      restore_game_state(prev, practice_state, game_state, game_info)
      delete_last_event(connect_to_db, event_id, pitch_num, is_pitch)
      
      DBI::dbCommit(connect_to_db)
      
      # 5) Refresh reactives & counters
      pitch_update_trigger(pitch_update_trigger() + 1)
      game_state$pitch_count <- game_state$pitch_count - 1
      
      showNotification("Last pitch successfully undone!", type = "message")
      
    }, error = function(e) {
      DBI::dbRollback(connect_to_db)
      showNotification("Undo failed! See console for details.", type = "error")
      message("UNDO ERROR: ", e$message)
    })
    
  })
  
  ## ---- Practice Functions ----
  # Record a practice pitch
  record_practice_pitch <- function(outcome) {
    # Update the current pitcher and hitter
    id_pitcher <- game_state$available_pitchers$player_id[game_state$available_pitchers$display_name == input$practice_pitcher]
    id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
    game_state$current_pitcher <- id_pitcher
    game_state$current_hitter <- id_hitter
    # Update the counts of pitcher, at bat, and game
    game_state$pitch_count <- game_state$pitch_count + 1
    game_state$at_bat_pitch_number <- game_state$at_bat_pitch_number + 1
    game_state$pitcher_pitch_count[[as.character(id_pitcher)]] <- (game_state$pitcher_pitch_count[[as.character(id_pitcher)]] %||% 0) + 1
    
    # reading where the pitch is located
    if (!is.data.frame(pitches$location) || nrow(pitches$location) == 0 || !all(c("xlocation", "ylocation") %in% names(pitches$location))) {
      pitches$location <<- data.frame(xlocation = NA_real_, ylocation = NA_real_)
    }
    loc_row <- nrow(pitches$location)
    pitch_x <- if (loc_row > 0 && !is.na(pitches$location$xlocation[loc_row])) pitches$location$xlocation[loc_row] else NA_real_
    pitch_y <- if (loc_row > 0 && !is.na(pitches$location$ylocation[loc_row])) pitches$location$ylocation[loc_row] else NA_real_
    
    # New row of data
    practice_pitch <- data.frame(
      event_id = game_info$event_id,
      game_pitch_number = game_state$pitch_count %||% 1,
      is_pitch_event = TRUE,
      hitter_id = id_hitter,
      pitcher_id = id_pitcher,
      pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(id_pitcher)]],
      at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
      half_inning = if (practice_state$have_innings == "Yes") {
        game_state$half %||% "Top"
      } else {
        "None"
      },
      inning = if (practice_state$have_innings == "Yes") {
        game_state$inning %||% 1
      } else {
        0
      },
      outs = game_state$outs %||% 0,
      strikes = game_state$strikes %||% 0,
      balls = game_state$balls %||% 0,
      pitch_outcome = if (outcome == "Drop Third Swinging") {
        "Swinging Strike"
      } else if (outcome == "Drop Third Called") {
        "Called Strike"
      } else if (practice_state$have_bip_outcome == "No") {
        if (outcome == "Ball In Play") {
          "Swinging Strike"
        } else {
          outcome
        }
      } else {
        outcome
      },
      pitch_velo = as.numeric(input$pitch_speed),
      pitch_type = input$pitch_type,
      pitch_location_x = as.numeric(pitch_x),
      pitch_location_y = as.numeric(pitch_y),
      ball_in_play_outcome = if (practice_state$have_bip_outcome == "No") {
        "Cage Ball"
      } else if (outcome == "Ball In Play") {
        ball_in_play_data$ball_in_play_outcome
      } else if (outcome == "Hit By Pitch") {
        "HBP"
      } else if (outcome == "Ball" & game_state$balls == 3) {
        "Walk"
      } else if (outcome == "Called Strike" & game_state$strikes == 2 || outcome == "Drop Third Called") {
        "SO Looking"
      } else if (outcome == "Swinging Strike" & game_state$strikes == 2 || outcome == "Drop Third Called") {
        "SO Swinging"
      } else {
        "None"
      },
      batter_outcome = if (practice_state$have_bip_outcome == "No") {
        if (outcome == "Ball In Play") {
          "Cage Ball"
        } else if (outcome == "Drop Third Swinging") {
          "SO Swinging"
        } else if (outcome == "Drop Third Called") {
          "SO Looking"
        } else {
          "None"
        }
      } else if (outcome == "Ball" & game_state$balls == 3) {
        "First"
      } else if (outcome == "Hit By Pitch") {
        "First"
      } else if (ball_in_play_data$ball_in_play_outcome == "Homerun") {
        "Scored"
      } else if (ball_in_play_data$ball_in_play_outcome == "Triple") {
        "Third"
      } else if (ball_in_play_data$ball_in_play_outcome == "Double") {
        "Second"
      } else if (ball_in_play_data$ball_in_play_outcome %in% c("Single", "Error", "Fielder's Choice")) {
        "First"
      } else {
        "None"
      },
      ball_flight = ball_in_play_data$ball_flight %||% NA_character_,
      contact_type = ball_in_play_data$contact_type %||% NA_character_,
      spray_chart_x = ball_in_play_data$spray_chart_x %||% NA_real_,
      spray_chart_y = ball_in_play_data$spray_chart_y %||% NA_real_,
      first_base_runner = if (is.na(game_state$runners$first) || length(game_state$runners$first) == 0 || practice_state$have_runners == "No") NA_real_ else as.numeric(game_state$runners$first),
      first_base_outcome = if (is.na(game_state$runners$first) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch") {
        "Second"
      } else if (outcome == "Ball" & game_state$balls == 3) {
        "Second"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "Stayed"
      } else {
        NA_character_
      },
      first_base_reason = if (is.na(game_state$runners$first) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch") {
        "HBP"
      } else if (outcome == "Ball" & game_state$balls == 3) {
        "Walk"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "None"
      } else {
        NA_character_
      },
      second_base_runner = if (is.na(game_state$runners$second) || length(game_state$runners$second) == 0 || practice_state$have_runners == "No") NA_real_ else as.numeric(game_state$runners$second),
      second_base_outcome = if (is.na(game_state$runners$second) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first)) {
        "Third"
      } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first)) {
        "Third"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "Stayed"
      } else {
        NA_character_
      },
      second_base_reason = if (is.na(game_state$runners$second) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first)) {
        "HBP"
      } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first)) {
        "Walk"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "None"
      } else {
        NA_character_
      },
      third_base_runner = if (is.na(game_state$runners$third) || length(game_state$runners$third) == 0 || practice_state$have_runners == "No") NA_real_ else as.numeric(game_state$runners$third),
      third_base_outcome = if (is.na(game_state$runners$third) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first & game_state$runners$second)) {
        "Scored"
      } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first & game_state$runners$second)) {
        "Scored"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "Stayed"
      } else {
        NA_character_
      },
      third_base_reason = if (is.na(game_state$runners$third) || practice_state$have_runners == "No") {
        NA_character_
      } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first & game_state$runners$second)) {
        "HBP"
      } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first & game_state$runners$second)) {
        "Walk"
      } else if (ball_in_play_data$ball_in_play_outcome == "None") {
        "None"
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE)
    
    # Write to database
    dbWriteTable(connect_to_db, "global_pitch_data", practice_pitch, append = TRUE)
    
    # Trigger global_pitch_data refresh
    pitch_update_trigger(pitch_update_trigger() + 1)
    
    # Reset pitch location and pending outcomes
    pitches$location <<- data.frame(xlocation = NA_real_, ylocation = NA_real_)
  }
  
  # Update the practice inning, count, or batter
  update_practice_count_and_inning <- function(is_strike = FALSE, is_foul = FALSE, is_ball = FALSE, reset = FALSE) {
    if (reset) { # clear count and at bat number
      game_state$balls <- 0
      game_state$strikes <- 0
      game_state$at_bat_pitch_number <- 0
      return()
    }
    
    if (is_strike) { # record a strike, if three strikes add out and reset count, next batter
      game_state$strikes <- game_state$strikes + 1
      if (game_state$strikes == 3) {
        game_state$outs <- game_state$outs + 1
        game_state$balls <- 0
        game_state$strikes <- 0
        game_state$at_bat_pitch_number <- 0
        game_state$current_hitter <- NA_real_
      }
    } else if (is_foul && game_state$strikes < 2) { # foul ball cannot be strike three
      game_state$strikes <- game_state$strikes + 1
    } else if (is_ball) {
      game_state$balls <- game_state$balls + 1
      if (game_state$balls == 4) { # four balls == walk, move runner, add score if necessary, next batter
        if (practice_state$have_runners == "Yes") {
          id_hitter <- game_state$available_hitters$player_id[game_state$available_hitters$display_name == input$practice_batter]
          batter_team <- ifelse(game_state$half == "Top", "away_score", "home_score")
          if (!is.na(game_state$runners$third & game_state$runners$second & game_state$runners$first)) {
            game_state$runners$third <- game_state$runners$second
            game_state$runners$second <- game_state$runners$first
            game_state$runners$first <- id_hitter
            game_state[[batter_team]] <- game_state[[batter_team]] + 1
          } else if (!is.na(game_state$runners$second & game_state$runners$first)) {
            game_state$runners$third <- game_state$runners$second
            game_state$runners$second <- game_state$runners$first
            game_state$runners$first <- id_hitter
          } else if (!is.na(game_state$runners$first)) {
            game_state$runners$second <- game_state$runners$first
            game_state$runners$first <- id_hitter
          } else {
            game_state$runners$first <- id_hitter
          } 
        }
        game_state$current_hitter <- NA_real_
        game_state$balls <- 0
        game_state$strikes <- 0
        game_state$at_bat_pitch_number <- 0
      }
    }
  }
  
  # ---- Game Page Input Functioning ----
  ## ---- Table with All the Pitches ----
  # 1) BASE INNINGS: read from your existing inning_specifier input
  base_innings <- reactive({
    # if the user hasn’t picked 7 or 9 yet, default back to 9
    if (is.null(input$inning_specifier) || input$inning_specifier == "Choose") {
      return(9L)
    }
    # force it to integer
    as.integer(input$inning_specifier)
  })
  
  # 2) EXTRA INNINGS counter
  extra_innings <- reactiveVal(0)
  
  # 3) TOTAL INNINGS = base + extras
  nInnings <- reactive({
    base_innings() + extra_innings()
  })
  
  # 4) bump extras when Add Inning clicked
  observeEvent(input$add_inning, {
    extra_innings(extra_innings() + 1)
  })
  
  
  # Half inning Table for Tab on Game Input Page
  observeEvent(pitch_update_trigger(), {
    output$all_half_inning_table <- renderUI({
      req(global_pitch_data())
      
      innings <- nInnings()
      all_tabs <- unlist(
        lapply(seq_len(innings), function(i) c(paste("Top", i), paste("Bottom", i)))
      )
      
      # figure default_tab as before…
      current_half   <- case_when(
        tolower(game_state$half) == "top"    ~ "Top",
        tolower(game_state$half) == "bottom" ~ "Bottom",
        TRUE                                ~ "Top"
      )
      current_inning <- as.numeric(game_state$inning)
      if (is.na(current_inning) || current_inning < 1 || current_inning > innings) {
        current_inning <- 1
      }
      default_tab <- paste(current_half, current_inning)
      if (!default_tab %in% all_tabs) default_tab <- sprintf("Top %d", base_innings())
      
      tabs <- lapply(all_tabs, function(label) {
        id <- gsub(" ", "_", label)
        tabPanel(label, DTOutput(paste0("half_inning_table_", id)))
      })
      
      do.call(tabsetPanel,
              c(list(id = "half_inning_tabs", selected = default_tab), tabs)
      )
    })
  })
  
  observe({
    req(global_pitch_data())
    for (i in seq_len(nInnings())) {
      local({
        ii <- i
        output[[paste0("half_inning_table_Top_", ii)]] <- renderDT({
          global_pitch_data() %>% filter(inning == ii, half == "Top")
        })
        output[[paste0("half_inning_table_Bottom_", ii)]] <- renderDT({
          global_pitch_data() %>% filter(inning == ii, half == "Bottom")
        })
      })
    }
  })  
  # Render the DataTable for each predefined tab dynamically with pitcher and hitter names
  observeEvent(pitch_update_trigger(), {
    req(global_pitch_data(), game_info$event_id)
    pitch_data_event <- global_pitch_data() %>%
      filter(event_id == game_info$event_id) %>%
      # Clean and validate numeric columns to prevent coercion warnings
      mutate(
        inning = as.numeric(as.character(inning)),
        game_pitch_number = as.numeric(as.character(game_pitch_number)),
        balls = as.numeric(as.character(balls)),
        strikes = as.numeric(as.character(strikes)),
        # Replace NA with defaults
        inning = ifelse(is.na(inning), 1, inning),
        game_pitch_number = ifelse(is.na(game_pitch_number), 0, game_pitch_number),
        balls = ifelse(is.na(balls), 0, balls),
        strikes = ifelse(is.na(strikes), 0, strikes),
        # Normalize half_inning for correct tab assignment
        half_inning = case_when(
          tolower(half_inning) == "top" ~ "Top",
          tolower(half_inning) == "bottom" ~ "Bottom",
          TRUE ~ half_inning  # Keep original for debugging
        )
      ) %>%
      left_join(stored_players$data, by = c("pitcher_id" = "player_id")) %>%
      rename(PitcherName = full_name) %>%
      left_join(stored_players$data, by = c("hitter_id" = "player_id")) %>%
      rename(HitterName = full_name) %>%
      left_join(stored_players$data, by = c("first_base_runner" = "player_id")) %>%
      rename(First_Runner_Name = full_name) %>%
      left_join(stored_players$data, by = c("second_base_runner" = "player_id")) %>%
      rename(Second_Runner_Name = full_name) %>%
      left_join(stored_players$data, by = c("third_base_runner" = "player_id")) %>%
      rename(Third_Runner_Name = full_name)
    
    # Handle empty data to prevent downstream errors
    if (nrow(pitch_data_event) == 0) {
      innings <- nInnings()
      lapply(seq_len(innings), function(i) c(paste("Top", i), paste("Bottom", i)))
      for (label in all_tabs) {
        local({
          tab_id <- gsub(" ", "_", label)
          output[[paste0("half_inning_table_", tab_id)]] <- renderDT({
            datatable(data.frame(), options = list(pageLength = 20))
          })
        })
      }
      return()
    }
    
    # Use predefined labels: Top 1, Bottom 1, ..., Top 9, Bottom 9 with functionality to add innings
    innings <- nInnings()
    all_tabs <- unlist(
      lapply(seq_len(innings), function(i) c(paste("Top", i), paste("Bottom", i)))
    )    
    for (label in all_tabs) {
      local({
        current_label <- label
        tab_id <- gsub(" ", "_", current_label)
        data_tab <- pitch_data_event %>%
          # Use normalized half_inning and inning to match current_label
          filter(paste(half_inning, inning) == current_label) %>%
          mutate(
            Count = paste(balls, "-", strikes)
          ) %>%
          select(
            game_pitch_number, 
            PitcherName, 
            pitchers_pitch_count,
            pitch_type,
            pitch_velo,
            pitch_outcome,
            outs,
            Count,
            HitterName, 
            at_bat_pitch_number,
            ball_in_play_outcome,
            ball_flight,
            contact_type,
            batter_outcome,
            First_Runner_Name,
            first_base_outcome,
            first_base_reason,
            Second_Runner_Name,
            second_base_outcome,
            second_base_reason,
            Third_Runner_Name,
            third_base_outcome,
            third_base_reason
          ) %>%
          arrange(desc(game_pitch_number))
        
        output[[paste0("half_inning_table_", tab_id)]] <- renderDT({
          datatable(data_tab, options = list(pageLength = 20))
        })
      })
    }
  })
  
  ## ---- Line Up Page ----
  # Home team lineup dropdowns
  output$home_team_table <- renderUI({
    req(app_state$page == "game_event", game_info$home_team)
    
    home_players <- stored_players$data %>%
      filter(college_name == game_info$home_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name))
    
    home_names <- home_players$display_name[match(game_state$home_lineup, home_players$player_id)]
    home_names[is.na(home_names)] <- "Not Set"
    pitcher_name <- home_players$display_name[match(game_state$home_pitcher, home_players$player_id)]
    pitcher_name <- if (is.na(pitcher_name)) "Not Set" else pitcher_name
    
    batter_choices <- home_players$display_name
    pitcher_choices <- home_players$display_name
    
    f7List(
      lapply(1:10, function(i) {
        position_label <- if (i <= 9) paste("Position", i) else "Pitcher"
        choices <- if (i <= 9) batter_choices else pitcher_choices
        current_selection <- if (i <= 9) home_names[i] else pitcher_name
        
        f7ListItem(
          f7Select(
            inputId = paste0("home_lineup_", i),
            label = position_label,
            choices = c("Not Set", choices),
            selected = current_selection
          )
        )
      })
    )
  })
  
  
  # Away team lineup dropdowns
  output$away_team_table <- renderUI({
    req(app_state$page == "game_event", game_info$away_team)
    
    away_players <- stored_players$data %>%
      filter(college_name == game_info$away_team, status == "Active") %>%
      mutate(display_name = paste0(jersey_number, " — ", full_name))
    
    away_names <- away_players$display_name[match(game_state$away_lineup, away_players$player_id)]
    away_names[is.na(away_names)] <- "Not Set"
    pitcher_name <- away_players$display_name[match(game_state$away_pitcher, away_players$player_id)]
    pitcher_name <- if (is.na(pitcher_name)) "Not Set" else pitcher_name
    
    batter_choices <- away_players$display_name
    pitcher_choices <- away_players$display_name
    
    f7List(
      lapply(1:10, function(i) {
        position_label <- if (i <= 9) paste("Position", i) else "Pitcher"
        choices <- if (i <= 9) batter_choices else pitcher_choices
        current_selection <- if (i <= 9) away_names[i] else pitcher_name
        
        f7ListItem(
          f7Select(
            inputId = paste0("away_lineup_", i),
            label = position_label,
            choices = c("Not Set", choices),
            selected = current_selection
          )
        )
      })
    )
  })
  
  
  # Handle home team lineup changes
  lapply(1:10, function(i) {
    observeEvent(input[[paste0("home_lineup_", i)]], {
      req(app_state$page == "game_event", game_info$home_team)
      new_player <- input[[paste0("home_lineup_", i)]]
      
      home_players <- stored_players$data %>%
        filter(college_name == game_info$home_team, status == "Active") %>%
        mutate(display_name = paste0(jersey_number, " — ", full_name))
      
      new_id <- if (new_player == "Not Set") NA else home_players$player_id[home_players$display_name == new_player][1]
      
      if (i <= 9) {
        game_state$home_lineup[i] <- new_id
        cat("Updated home lineup position", i, "to", new_player, "ID:", new_id, "\n")
      } else {
        game_state$home_pitcher <- new_id
        # Home team pitches in top half
        if (game_state$half == "Top") {
          game_state$current_pitcher <- new_id
        }
        cat("Updated home pitcher to", new_player, "ID:", new_id, "\n")
      }
    }, ignoreInit = TRUE)
  })
  
  # Handle away team lineup changes
  lapply(1:10, function(i) {
    observeEvent(input[[paste0("away_lineup_", i)]], {
      req(app_state$page == "game_event", game_info$away_team)
      new_player <- input[[paste0("away_lineup_", i)]]
      
      away_players <- stored_players$data %>%
        filter(college_name == game_info$away_team, status == "Active") %>%
        mutate(display_name = paste0(jersey_number, " — ", full_name))
      
      new_id <- if (new_player == "Not Set") NA else away_players$player_id[away_players$display_name == new_player][1]
      
      if (i <= 9) {
        game_state$away_lineup[i] <- new_id
        cat("Updated away lineup position", i, "to", new_player, "ID:", new_id, "\n")
      } else {
        game_state$away_pitcher <- new_id
        # Away team pitches in bottom half
        if (game_state$half == "Bottom") {
          game_state$current_pitcher <- new_id
        }
        cat("Updated away pitcher to", new_player, "ID:", new_id, "\n")
      }
    }, ignoreInit = TRUE)
  })
  
  ## ---- Strike Zone Functioning ----
  # strike zone clicking
  observeEvent(input$strike_zone_click, {
    if (input$strike_zone_click$x < -50 || input$strike_zone_click$x > 50 ||
        input$strike_zone_click$y < -26 || input$strike_zone_click$y > 66){
      showNotification("That pitch is too far from the graph", type = "error")
      return()
    }
    print(paste("Strike zone clicked at:", input$strike_zone_click$x, input$strike_zone_click$y))
    # Overwrite with a single point
    pitches$location <<- data.frame(xlocation = input$strike_zone_click$x, ylocation = input$strike_zone_click$y)
  })
  
  #Buttons on the graph
  output$floating_buttons <- renderUI({
    if (game_info$event_type == "Game") {
      absolutePanel(
        top = 10, left = 10, draggable = FALSE, fixed = FALSE,
        actionButton("clock_hitter_pop_up", "Clock On Hitter", class = "small-btn"),
        actionButton("clock_pitcher_pop_up", "Clock On Pitcher", class = "small-btn"),
        actionButton("balk_button_pop_up", "Balk", class = "small-btn"),
        
        # Conditionally show Drop Third Strike button
        if (game_state$strikes == 2 && (is.na(game_state$runners$first) || game_state$outs == 2)) {
          actionButton("drop_third_strike_pop_up", "Drop Third Strike", class = "small-btn")
        },
        
        style = "display: flex; flex-direction: column; gap: 5px;"
      )
    } else if (game_info$event_type == "Practice") {
      if (practice_state$have_bip_outcome == "Yes") {
        absolutePanel(
          top = 10, left = 10, draggable = FALSE, fixed = FALSE,
          if (game_state$strikes == 2 && (is.na(game_state$runners$first) || game_state$outs == 2)) {
            actionButton("practice_drop_third_strike_pop_up", "Drop Third Strike", class = "small-btn")
          },
          
          style = "display: flex; flex-direction: column; gap: 5px;"
        )
      }
    } else {
      NULL
    }
  })
  
  # For the clickable pitch location
  pitches <- reactiveValues(
    location = data.frame(
      xlocation = numeric(), 
      ylocation = numeric()
    )
  )
  
  # Reactive value for ball-in-play hit location
  hit_location <- reactiveValues(
    coords = data.frame(x = numeric(), y = numeric())
  )
  
  # Render Strike Zone Plot
  output$strike_zone <- renderPlot({
    ggplot() +
      # Strike zone rectangle
      geom_rect(aes(xmin = -28, xmax = 28, ymin = -10, ymax = 45),
                fill = "transparent", color = "black", linewidth = 1) +
      # Heart rectangle
      geom_rect(aes(xmin = -18, xmax = 18, ymin = 0, ymax = 35),
                fill = "transparent", color = "grey", linewidth = 1, linetype = "dashed") +
      # Chase rectangle
      geom_rect(aes(xmin = -38, xmax = 38, ymin = -20, ymax = 55),
                fill = "transparent", color = "grey", linewidth = 1, linetype = "dashed") +
      # Home plate shape (5-sided polygon)
      geom_polygon(aes(x = c(-28, -26, 0, 26, 28), 
                       y = c(-16, -22, -30, -22, -16)),
                   fill = "white", color = "black", linewidth = 1) +
      
      # Pitch locations
      geom_point(data = pitches$location, aes(x = xlocation, y = ylocation),
                 color = "red", size = 6) +
      coord_fixed() +
      xlim(-46, 46) +
      ylim(-40, 60) +   # expanded ylim for home plate visibility
      theme_void()
  })
  
  ## ---- Score Board Functioning ----
  # inning number
  output$inning_number <- renderText({
    as.character(game_state$inning)
  })  
  
  # inning arrow (up for top, down for bottom)
  output$inning_arrow <- renderUI({
    if (game_state$half == "Top") {
      HTML("&#9650;")  # Unicode for upward triangle (▲)
    } else {
      HTML("&#9660;")  # Unicode for downward triangle (▼)
    }
  })
  
  # Showing current hitter and pitcher
  output$current_matchup_label <- renderUI({
    current_idx <- game_state$current_batter_idx
    
    if (game_state$half == "Top") {
      batter_id <- game_state$away_lineup[current_idx]
      pitcher_id <- game_state$current_pitcher
    } else {
      batter_id <- game_state$home_lineup[current_idx]
      pitcher_id <- game_state$current_pitcher
    }
    
    batter_name <- stored_players$data$full_name[stored_players$data$player_id == batter_id]
    pitcher_name <- stored_players$data$full_name[stored_players$data$player_id == pitcher_id]
    
    if (length(batter_name) == 0) batter_name <- "Unknown"
    if (length(pitcher_name) == 0) pitcher_name <- "Unknown"
    
    if (game_state$half == "Top") {
      HTML(paste0(
        "<span style='font-size: 18px;'><strong>Hitter:</strong> ", batter_name, "</span><br>",
        "<span style='font-size: 18px;'><strong>Pitcher:</strong> ", pitcher_name, "</span>"
      ))
    } else {
      HTML(paste0(
        "<span style='font-size: 18px;'><strong>Pitcher:</strong> ", pitcher_name, "</span><br>",
        "<span style='font-size: 18px;'><strong>Hitter:</strong> ", batter_name, "</span>"
      ))
    }
    
  })
  
  # Render Away Team Name
  output$away_name <- renderText({
    away_team <- as.character(game_info$away_team)
    abbreviation <- teams$data %>%
      filter(college_name == away_team) %>%
      pull(college_abbreviation)
    
    if (length(abbreviation) == 0) {
      return(away_team)  # fallback to full name if abbreviation not found
    } else {
      return(abbreviation)
    }
  })
  
  # Render Home Team Name
  output$home_name <- renderText({
    home_team <- as.character(game_info$home_team)
    abbreviation <- teams$data %>%
      filter(college_name == home_team) %>%
      pull(college_abbreviation)
    
    if (length(abbreviation) == 0) {
      return(home_team)
    } else {
      return(abbreviation)
    }
  })
  
  # Render Away Score
  output$away_score <- renderText({
    as.character(game_state$away_score)
  })
  
  # Render Home Score
  output$home_score <- renderText({
    as.character(game_state$home_score)
  })
  
  # Render scoreboard bases 
  output$bases_graphic <- renderPlot({
    message("game_state$runners: ", paste(names(game_state$runners), collapse = ", "))
    message("first: ", game_state$runners$first, ", second: ", game_state$runners$second, ", third: ", game_state$runners$third)
    occupied_vector <- c(!is.na(game_state$runners$first),
                         !is.na(game_state$runners$second),
                         !is.na(game_state$runners$third))
    message("occupied length: ", length(occupied_vector), ", values: ", paste(occupied_vector, collapse = ", "))
    
    # Define base positions
    bases <- data.frame(
      base = c("First", "Second", "Third"),
      x = c(0.2, 0, -0.2),  
      y = c(0, 0.2, 0),    
      occupied = occupied_vector
    )
    
    ggplot(bases, aes(x = x, y = y)) +
      # Squares with black outline, filled based on occupancy
      geom_point(aes(fill = occupied), size = 11, shape = 22, color = "black") +
      scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black")) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none", 
            plot.margin = margin(0, 0, 0, 0),
            plot.background = element_rect(fill = "transparent", color = NA)) +
      xlim(-0.4, 0.4) + ylim(-0.2, 0.4)
  }, height = 80, bg = "transparent")
  
  # Render strikes/balls graphic
  output$count_and_outs <- renderUI({
    balls_circles <- paste(rep("<div class='circle ball-circle'></div>", game_state$balls), collapse = "")
    strikes_circles <- paste(rep("<div class='circle strike-circle'></div>", game_state$strikes), collapse = "")
    outs_circles <- paste(rep("<div class='circle out-circle'></div>", game_state$outs), collapse = "")
    HTML(paste(
      "<div>Balls: ", balls_circles, "</div>",
      "<div>Strikes: ", strikes_circles, "</div>",
      "<div>Outs: ", outs_circles, "</div>"
    ))
  })
  
  ## ---- Different Pitch Outcome Buttons ----
  # Clock on Hitter (all)
  observeEvent(input$clock_hitter_pop_up, {
    showModal(modalDialog(
      title = "Confirm Pitch Clock Violation",
      "Are you sure that this was a pitch clock violation called on the hitter?",
      easyClose = FALSE,  # Forces user to interact with buttons
      footer = tagList(
        actionButton("confirm_clock_hitter", "Yes"),
        modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$confirm_clock_hitter, {
    removeModal()
    ball_in_play_data_reset()
    resetPitchInputs()
    record_pitch("Clock Hitter")
    update_count_and_inning(is_strike = TRUE)
  }, ignoreInit = TRUE)
  
  # Clock on Pitcher (all)
  observeEvent(input$clock_pitcher_pop_up, {
    showModal(modalDialog(
      title = "Confirm Pitch Clock Violation",
      "Are you sure that this was a pitch clock violation called on the pitcher?",
      easyClose = FALSE,  # Forces user to interact with buttons
      footer = tagList(
        actionButton("confirm_clock_pitcher", "Yes"),
        modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$confirm_clock_pitcher, {
    removeModal()
    ball_in_play_data_reset()
    resetPitchInputs()
    record_pitch("Clock Pitcher")
    update_count_and_inning(is_ball = TRUE)
  }, ignoreInit = TRUE)
  
  # Balk button
  observeEvent(input$balk_button_pop_up, {
    ball_in_play_data_reset()
    record_pitch("Balk")
    resetPitchInputs()
  })
  
  # Drop Third Strike (all)
  observeEvent(input$drop_third_strike_pop_up, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "How was the drop third strike initiated?",
      "You will be able to advance runners on the next page.",
      easyClose = FALSE,
      footer = tagList(
        actionButton("drop_third_swinging", "Swinging Strike"),
        actionButton("drop_third_called", "Called Strike"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Drop Third Swinging
  observeEvent(input$drop_third_swinging, {
    removeModal()
    ball_in_play_data_reset()
    record_pitch("Drop Third Swinging")
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Update runner outcomes
    the_hitter_choices <- c("Drop Third Swinging")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Drop Third Swinging", "Error")
    }
    
    # Pop up for runner options
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("confirm_advance", "Confirm Change", class = "btn btn-default")
      ),
      size = "l"
    ))
    
    update_count_and_inning(reset = TRUE)
    game_state$current_batter_idx <- game_state$current_batter_idx + 1
    if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  #Drop Third Called
  observeEvent(input$drop_third_called, {
    removeModal()
    ball_in_play_data_reset()
    record_pitch("Drop Third Called")
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Update runners outcomes
    the_hitter_choices <- c("Drop Third Called")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Drop Third Called", "Error")
    }
    
    # pop up for runner options
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("confirm_advance", "Confirm Change", class = "btn btn-default")
      ),
      size = "l"
    ))
    
    resetPitchInputs()
    update_count_and_inning(reset = TRUE)
    game_state$current_batter_idx <- game_state$current_batter_idx + 1
    if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
  }, ignoreInit = TRUE)
  
  # Called Strike
  observeEvent(input$called_strike, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_pitch("Called Strike")
    update_count_and_inning(is_strike = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Swinging Strike
  observeEvent(input$swing_strike, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_pitch("Swinging Strike")
    update_count_and_inning(is_strike = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Foul Ball
  observeEvent(input$foul_ball, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_pitch("Foul Ball")
    update_count_and_inning(is_foul = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Ball
  observeEvent(input$ball, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_pitch("Ball")
    update_count_and_inning(is_ball = TRUE)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Hit By Pitch
  observeEvent(input$hit_by_pitch, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    ball_in_play_data_reset()
    record_pitch("Hit By Pitch")
    batter_team <- ifelse(game_state$half == "Top", "away_lineup", "home_lineup")
    current_hitter_id <- game_state[[batter_team]][game_state$current_batter_idx]
    
    # Update runners on base if needed, automatically
    if (!is.na(game_state$runners$third & game_state$runners$second & game_state$runners$first)) {
      game_state$runners$third <- game_state$runners$second
      game_state$runners$second <- game_state$runners$first
      game_state$runners$first <- current_hitter_id
      game_state[[ifelse(game_state$half == "Top", "away_score", "home_score")]] <- game_state[[ifelse(game_state$half == "Top", "away_score", "home_score")]] + 1
    } else if (!is.na(game_state$runners$second & game_state$runners$first)) {
      game_state$runners$third <- game_state$runners$second
      game_state$runners$second <- game_state$runners$first
      game_state$runners$first <- current_hitter_id
    } else if (!is.na(game_state$runners$first)) {
      game_state$runners$second <- game_state$runners$first
      game_state$runners$first <- current_hitter_id
    } else {
      game_state$runners$first <- current_hitter_id
    } 
    
    # next batter
    game_state$current_batter_idx <- game_state$current_batter_idx + 1
    if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
    update_count_and_inning(reset = TRUE)
    pitch_update_trigger(pitch_update_trigger() + 1)
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  ## ---- Ball In Play ----
  # Ball In Play button
  observeEvent(input$ball_in_play, {
    if (is.null(input$pitch_speed) || is.null(input$pitch_type) || is.na(input$pitch_type)) {
      showNotification("Please select a pitch speed and pitch type.", type = "error")
      return()
    }
    if (nrow(pitches$location) == 0) {
      showNotification("Please click on the strike zone first.", type = "error")
      return()
    }
    
    # Count runners on base
    runners_on <- sum(!is.na(game_state$runners$first), 
                      !is.na(game_state$runners$second), 
                      !is.na(game_state$runners$third))
    
    # Base outcomes
    available_outcomes <- c("Out", "Single", "Double", "Triple", "Homerun", "Error")
    if (runners_on >= 1 && game_state$outs < 2) {
      available_outcomes <- c(available_outcomes, "Double Play", "Sacrifice Fly")
    }
    if (runners_on >= 1) {
      available_outcomes <- c(available_outcomes, "Fielder's Choice")
    }
    if (runners_on >= 2 && game_state$outs < 1) {
      available_outcomes <- c(available_outcomes, "Triple Play")
    }
    
    # Show modal dialog
    showModal(modalDialog(
      title = "Ball In Play Details",
      tags$style(HTML("
      /* Target radio button inputs */
      .shiny-options-group input[type='radio'] {
        width: 15px;    /* Increase circle width */
        height: 15px;   /* Increase circle height */
        margin-right: 8px; /* Adjust spacing between circle and label */
      }
      /* Optional: Increase label font size for balance */
      .shiny-options-group label {
        font-size: 16px;
        vertical-align: middle;
      }
      /* Ensure inline layout stays clean */
      .shiny-options-group {
        margin-bottom: 10px;
      }
    ")),
      plotOutput("hit_field", click = "hit_field_click", height = "600px", width = "600px"),
      
      h4("Ball Flight"),
      radioButtons(
        inputId = "selected_ball_flight",
        label = NULL,
        choices = ball_flight_types,
        selected = ball_flight_types[1],
        inline = TRUE
      ),
      
      h4("Contact Quality"),
      radioButtons(
        inputId = "selected_contact_quality",
        label = NULL,
        choices = contact_quality_types,
        selected = contact_quality_types[1],
        inline = TRUE
      ),
      
      h4("Outcome"),
      radioButtons(
        inputId = "selected_hit_outcome",
        label = NULL,
        choices = available_outcomes,
        selected = available_outcomes[1],
        inline = TRUE
      ),
      uiOutput("BIP_runner_ui_output"), # Dynamic runner UI
      actionButton("confirm_hit", "Confirm"),
      actionButton("cancel_modal", "Cancel", class = "btn btn-default"),
      footer = NULL,
      easyClose = TRUE,
      size = "l"
    ))
    
    hit_location$coords <- data.frame(x = numeric(), y = numeric())
  }, ignoreInit = TRUE)
  
  # Select inputs for runners on base
  output$BIP_runner_ui_output <- renderUI({
    req(input$selected_hit_outcome)
    outcome <- input$selected_hit_outcome
    
    # Define possible runner destinations based on hit outcome
    runner_options <- list(
      first = if (!is.na(game_state$runners$first)) {
        num_runners <- sum(!sapply(game_state$runners, is.na))
        switch(outcome,
               "Double Play" = {
                 if (num_runners == 1) {
                   c("Out")
                 } else if (game_state$outs == 1) {
                   c("Stay on First", "Second", "Home", "Out")
                 } else {
                   c("Out", "Stay on First", "Second", "Third", "Home")
                 }
               },
               "Triple Play" = {
                 if (num_runners == 2) {
                   c("Out")
                 } else {
                   c("Out", "Stay on First", "Second", "Third", "Home")
                 }
               },
               "Fielder's Choice" = c("Second", "Third", "Home", "Out"),
               "Single" = c("Second", "Third", "Home", "Out"),
               "Double" = c("Third", "Home", "Out"),
               "Triple" = c("Home", "Out"),
               "Homerun" = c("Home"), 
               "Error" = c("Second", "Third", "Home", "Out"),
               "Out" = c("Stay on First", "Second", "Third", "Home", "Out"),
               "Sacrifice Fly" = c("Stay on First", "Second", "Third", "Home", "Out"),
               c("Stay on First", "Second", "Third", "Home", "Out")
        )
      },
      second = if (!is.na(game_state$runners$second)) {
        num_runners <- sum(!sapply(game_state$runners, is.na))
        switch(outcome,
               "Double Play" = {
                 if (num_runners == 1) {
                   c("Out")
                 } else if (game_state$outs == 1) {
                   c("Stay on Second", "Third", "Home", "Out")
                 } else {
                   c("Stay on Second", "Third", "Home", "Out")
                 }
               },
               "Triple Play" = {
                 if (num_runners == 2) {
                   c("Out")
                 } else {
                   c("Stay on Second", "Home", "Out")
                 }
               },
               "Fielder's Choice" = c("Stay on Second", "Third", "Home", "Out"),
               "Single" = c("Stay on Second", "Third", "Home", "Out"),
               "Double" = c("Third", "Home", "Out"),
               "Triple" = c("Home", "Out"),
               "Homerun" = c("Home"),
               "Error" = c("Stay on Second", "Third", "Home", "Out"),
               "Out" = c("Stay on Second", "Third", "Home", "Out"),
               "Sacrifice Fly" = c("Stay on Second", "Third", "Home", "Out"),
               c("Stay on Second", "Third", "Home", "Out")
        )
      },
      third = if (!is.na(game_state$runners$third)) {
        num_runners <- sum(!sapply(game_state$runners, is.na))
        switch(outcome,
               "Double Play" = {
                 if (num_runners == 1) {
                   c("Out")
                 } else {
                   c("Stay on Third", "Home", "Out")
                 }
               },
               "Triple Play" = {
                 if (num_runners == 2) {
                   c("Out")
                 } else {
                   c("Stay on Third", "Out")
                 }
               },
               "Fielder's Choice" = c("Stay on Third", "Home", "Out"),
               "Single" = c("Stay on Third", "Home", "Out"),
               "Double" = c("Stay on Third", "Home", "Out"),
               "Triple" = c("Home", "Out"),
               "Homerun" = c("Home"),
               "Error" = c("Stay on Third", "Home", "Out"),
               "Out" = c("Stay on Third", "Home", "Out"),
               "Sacrifice Fly" = c("Home"),
               c("Stay on Third", "Home", "Out")
        )
      }
    )
    
    # Generate Inputs if runner occupies the base
    ui_elements <- list()
    if (!is.na(game_state$runners$first)) {
      ui_elements[[1]] <- selectInput("first_advance", "Runner on First Outcome:",
                                      choices = runner_options$first,
                                      selected = runner_options$first[1])
    }
    if (!is.na(game_state$runners$second)) {
      ui_elements[[2]] <- selectInput("second_advance", "Runner on Second Outcome:",
                                      choices = runner_options$second,
                                      selected = runner_options$second[1])
    }
    if (!is.na(game_state$runners$third)) {
      ui_elements[[3]] <- selectInput("third_advance", "Runner on Third Outcome:",
                                      choices = runner_options$third,
                                      selected = runner_options$third[1])
    }
    
    # Return the UI elements
    do.call(tagList, ui_elements)
  })
  
  # Observe click on baseball field
  observeEvent(input$hit_field_click, {
    hit_location$coords <- data.frame(x = input$hit_field_click$x, 
                                      y = input$hit_field_click$y)
  })
  
  # Confirm Observe Hit
  observeEvent(input$confirm_hit, {
    if (nrow(hit_location$coords) == 0) {
      showNotification("Make sure to click a location on the spray chart!", type = "error")
      return()
    }
    req(nrow(hit_location$coords) > 0, 
        input$selected_ball_flight, 
        input$selected_contact_quality, 
        input$selected_hit_outcome)
    
    # Store previous runners
    prev_runners <- list(
      first = if (is.na(game_state$runners$first)) NA_real_ else game_state$runners$first,
      second = if (is.na(game_state$runners$second)) NA_real_ else game_state$runners$second,
      third = if (is.na(game_state$runners$third)) NA_real_ else game_state$runners$third,
      outcome_first = NA_character_,
      outcome_second = NA_character_,
      outcome_third = NA_character_,
      reason_first = NA_character_,
      reason_second = NA_character_,
      reason_third = NA_character_
    )
    
    # Validation 1: If one is "Bunt", both must be "Bunt"
    if ((input$selected_contact_quality == "Bunt" && input$selected_ball_flight != "Bunt") ||
        (input$selected_contact_quality != "Bunt" && input$selected_ball_flight == "Bunt")) {
      showNotification("If the player bunted, both hit outcome and ball flight must be 'Bunt'", type = "error")
      return()
    }
    
    # Validation 2: Sac fly required if someone scored on a fly ball, but also allows for sac bunt to happen
    is_fly_out <- isTRUE(input$selected_hit_outcome == "Out") &&
      isTRUE(input$selected_contact_quality != "Bunt") &&
      !isTRUE(input$selected_ball_flight %in% c("Bunt", "Ground Ball"))
    
    runner_scored <- (!is.null(input$first_advance) && input$first_advance == "Home") ||
      (!is.null(input$second_advance) && input$second_advance == "Home") ||
      (!is.null(input$third_advance) && input$third_advance == "Home")
    
    if (is_fly_out && runner_scored) {
      showNotification("Use 'Sac Fly' instead of 'Out' when a runner scores on a fly ball.", type = "error")
      return()
    }
    
    # Set up for val 3 & 4
    runner_values <- setNames(
      c(
        if (is.null(input$selected_hit_outcome)) 5 else position_map[input$selected_hit_outcome],
        if (is.na(game_state$runners$first)) 5 else position_map[input$first_advance],
        if (is.na(game_state$runners$second)) 5 else position_map[input$second_advance],
        if (is.na(game_state$runners$third)) 5 else position_map[input$third_advance],
        if (is.null(prev_runners$first)) 5 else 1,
        if (is.null(prev_runners$second)) 5 else 2,
        if (is.null(prev_runners$third)) 5 else 3
      ),
      c("batter", "first", "second", "third", "prev_first", "prev_second", "prev_third")
    )
    
    # Not needed - this was used for debugging
    runner_order <- c(runner_values["batter"], runner_values["first"], runner_values["second"], runner_values["third"])
    print(runner_order)
    
    # Valuation 3: Illegal passing of runners
    if (any(runner_values["batter"] > runner_values[c("first", "second", "third")] & runner_values["batter"] != 5) ||
        any(runner_values["first"] > runner_values[c("second", "third")] & runner_values["first"] != 5) ||
        runner_values["second"] > runner_values["third"] && runner_values["second"] != 5) {
      showNotification("You cannot have runners pass each other!", type = "error")
      return()
    }
    
    # Valuation 4: Runners going to same base
    if (
      any(c(
        (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["first"]) && runner_values["first"] < 4),
        (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["second"]) && runner_values["second"] < 4),
        (runner_values["batter"] < 4 && (runner_values["batter"] == runner_values["third"]) && runner_values["third"] < 4),
        (runner_values["first"] < 4 && (runner_values["first"] == runner_values["second"]) && runner_values["second"] < 4),
        (runner_values["first"] < 4 && (runner_values["first"] == runner_values["third"]) && runner_values["third"] < 4),
        (runner_values["second"] < 4 && (runner_values["second"] == runner_values["third"]) && runner_values["third"] < 4)
      ))) {
      showNotification("Runners cannot end up at the same base, make sure to adjust that!", type = "error")
      return()
    }
    
    #Number of outs on Bases - used in validations
    sum_of_outs <- sum(
      if (!is.na(prev_runners$first)) ifelse(input$first_advance == "Out", 1, 0) else 0,
      if (!is.na(prev_runners$second)) ifelse(input$second_advance == "Out", 1, 0) else 0,
      if (!is.na(prev_runners$third)) ifelse(input$third_advance == "Out", 1, 0) else 0
    )
    
    # Validation 5: Too many people are being called out for the play (NEEDS WORK)
    if ((sum_of_outs > 2 && input$selected_hit_outcome == "Double Play") ||
        (game_state$outs + sum_of_outs > 2 && input$selected_hit_outcome == "Out") ||
        (game_state$outs + sum_of_outs > 3)) {
      showNotification("Too many players are getting out for this game situation", type = "error")
      return()
    }
    
    # Validation 6: Not enough people are being called out for this play
    if ((sum_of_outs < 2 && input$selected_hit_outcome == "Triple Play") ||
        sum_of_outs == 0 && input$selected_hit_outcome == "Double Play") {
      showNotification("Not enough players are getting out for this hit outcome", type = "error")
      return()
    }
    
    # Remove pop-up
    removeModal()
    
    # Update ball_in_play_data directly
    ball_in_play_data$ball_in_play_outcome <- input$selected_hit_outcome
    ball_in_play_data$ball_flight <- input$selected_ball_flight
    ball_in_play_data$contact_type <- input$selected_contact_quality
    ball_in_play_data$spray_chart_x <- hit_location$coords$x
    ball_in_play_data$spray_chart_y <- hit_location$coords$y
    
    # Record pitch and get batter ID
    pitch_record <- record_pitch("Ball In Play")
    batter_team <- ifelse(game_state$half == "Top", "away_lineup", "home_lineup")
    current_batter <- game_state[[batter_team]][game_state$current_batter_idx]
    
    # Determine which team's score to update
    batting_team_score <- ifelse(game_state$half == "Top", "away_score", "home_score")
    outcome <- input$selected_hit_outcome
    
    # Initialize pitch record outcomes & reasons & clear game_state$runners
    game_state$runners$first <- NA_real_
    game_state$runners$second <- NA_real_
    game_state$runners$third <- NA_real_
    
    #Third Base Runner
    if (!is.na(prev_runners$third) && !is.null(input$third_advance)) {
      if (input$third_advance == "Home") {
        game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        prev_runners$outcome_third <- "Scored"
      } else if (input$third_advance == "Out") {
        game_state$outs <- game_state$outs + 1
        prev_runners$outcome_third <- "Out"
      } else {
        game_state$runners$third <- prev_runners$third
        prev_runners$outcome_third <- "Stayed"
      }
    }
    
    #Second Base Runner
    if (!is.na(prev_runners$second) && !is.null(input$second_advance)) {
      if (input$second_advance == "Home") {
        game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        prev_runners$outcome_second <- "Scored"
      } else if (input$second_advance == "Third") {
        game_state$runners$third <- prev_runners$second
        prev_runners$outcome_second <- "Third"
      } else if (input$second_advance == "Out") {
        game_state$outs <- game_state$outs + 1
        prev_runners$outcome_second <- "Out"
      } else {
        game_state$runners$second <- prev_runners$second
        prev_runners$outcome_second <- "Stayed"
      }
    }
    
    #First Base Runner
    if (!is.na(prev_runners$first) && !is.null(input$first_advance)) {
      if (input$first_advance == "Home") {
        game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        prev_runners$outcome_first <- "Scored"
      } else if (input$first_advance == "Third") {
        game_state$runners$third <- prev_runners$first
        prev_runners$outcome_first <- "Third"
      } else if (input$first_advance == "Second") {
        game_state$runners$second <- prev_runners$first
        prev_runners$outcome_first <- "Second"  
      } else if (input$first_advance == "Out") {
        game_state$outs <- game_state$outs + 1
        prev_runners$outcome_first <- "Out"
      } else {
        game_state$runners$first <- prev_runners$first
        prev_runners$outcome_first <- "Stayed"
      }
    }
    
    # Outcome for bases
    if (outcome %in% c("Error", "Fielder's Choice")) {
      write_outcome <- ifelse(outcome == "Error", "E", "FC")
      prev_runners$reason_first <- ifelse(!is.na(prev_runners$first), write_outcome, NA_character_)
      prev_runners$reason_second <- ifelse(!is.na(prev_runners$second), write_outcome, NA_character_)
      prev_runners$reason_third <- ifelse(!is.na(prev_runners$third), write_outcome, NA_character_)
      game_state$runners$first <- current_batter
      
    } else {
      prev_runners$reason_first <- ifelse(!is.na(prev_runners$first), "BIP", NA_character_)
      prev_runners$reason_second <- ifelse(!is.na(prev_runners$second), "BIP", NA_character_)
      prev_runners$reason_third <- ifelse(!is.na(prev_runners$third), "BIP", NA_character_)
      
      if (outcome == "Single") {
        game_state$runners$first <- current_batter
      } else if (outcome == "Double") {
        game_state$runners$second <- current_batter
      } else if (outcome == "Triple") {
        game_state$runners$third <- current_batter
      } else if (outcome == "Homerun") {
        game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
      } else if (outcome == "Double Play") {
        if (sum_of_outs == 1) {
          game_state$outs <- game_state$outs + 1
        } else {
          game_state$runners$first <- current_batter
        }
      } else if (outcome == "Triple Play") {
        game_state$outs <- 3
      } else { # This is for sacrifice fly
        game_state$outs <- game_state$outs + 1
      }
    }
    
    # Label the people left on base (LOB)
    if (game_state$outs >= 3) {
      prev_runners$outcome_first <- ifelse(
        !is.na(prev_runners$first) & !(prev_runners$outcome_first %in% c("Out", "Home")),
        "LOB",
        prev_runners$outcome_first
      )
      prev_runners$outcome_second <- ifelse(
        !is.na(prev_runners$second) & !(prev_runners$outcome_second %in% c("Out", "Home")),
        "LOB",
        prev_runners$outcome_second
      )
      prev_runners$outcome_third <- ifelse(
        !is.na(prev_runners$third) & !(prev_runners$outcome_third %in% c("Out", "Home")),
        "LOB",
        prev_runners$outcome_third
      )
    }
    
    runner_order <- c(runner_values["batter"], runner_values["first"], runner_values["second"], runner_values["third"])
    
    # Check if runner bunted and if situation was a sacrifice bunt
    if (input$selected_hit_outcome == "Out" && input$selected_ball_flight == "Bunt") {
      if (
        (runner_values["first"] != 5 && runner_values["prev_first"] != 5 &&
         runner_values["first"] > runner_values["prev_first"]) ||
        
        (runner_values["second"] != 5 && runner_values["prev_second"] != 5 &&
         runner_values["second"] > runner_values["prev_second"]) ||
        
        (runner_values["third"] != 5 && runner_values["prev_third"] != 5 &&
         runner_values["third"] > runner_values["prev_third"])
      ) {
        dbExecute(connect_to_db, "
    UPDATE global_pitch_data
    SET ball_in_play_outcome = $1, first_base_outcome = $2, second_base_outcome = $3, third_base_outcome = $4, first_base_reason = $5, second_base_reason = $6, third_base_reason = $7
    WHERE game_pitch_number = $8 AND event_id = $9",
                  params = list("Sacrifice Bunt", prev_runners$outcome_first, prev_runners$outcome_second, 
                                prev_runners$outcome_third, prev_runners$reason_first, prev_runners$reason_second, 
                                prev_runners$reason_third, game_state$pitch_count, game_info$event_id))
      }
    } else {
      # Update database with runner outcomes
      dbExecute(connect_to_db, "
    UPDATE global_pitch_data
    SET first_base_outcome = $1, second_base_outcome = $2, third_base_outcome = $3, first_base_reason = $4, second_base_reason = $5, third_base_reason = $6
    WHERE game_pitch_number = $7 AND event_id = $8",
                params = list(prev_runners$outcome_first, prev_runners$outcome_second, prev_runners$outcome_third,
                              prev_runners$reason_first, prev_runners$reason_second, prev_runners$reason_third,
                              game_state$pitch_count, game_info$event_id))
    }
    
    
    # Advance batter and reset pitch state
    game_state$current_batter_idx <- game_state$current_batter_idx + 1
    if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
    update_count_and_inning(reset = TRUE)
    update_count_and_inning()
    resetPitchInputs()
  }, ignoreInit = TRUE)
  
  # Ball in Play plot
  output$hit_field <- renderPlot({
    p2 <- draw_baseball_field()
    
    if (nrow(hit_location$coords) > 0) {
      flight_color <- switch(input$selected_ball_flight %||% "default",
                             "Bunt" = "black",
                             "Fly Ball" = "#FFD700",
                             "Ground Ball" = "#FF4500",
                             "Line Drive" = "#00CED1",
                             "Pop-Up" = "#FF00FF",
                             "default" = "#FFFFFF")
      contact_shape <- switch(input$selected_contact_quality %||% "default",
                              "Bunt" = 16,
                              "Weak" = 16,
                              "Average" = 17,
                              "Hard" = 15,
                              "default" = 16)
      
      p2 <- p2 + geom_point(data = hit_location$coords, 
                            aes(x = x, y = y), 
                            color = flight_color, 
                            shape = contact_shape, 
                            size = 5)
    }
    
    p2
  })
  
  ## ---- Runners Advanced Functioning ----
  # Reactive data for plot to avoid full redraw
  runner_plot_data <- reactiveValues(
    occupied = data.frame(base = character(), x = numeric(), y = numeric()))
  
  # Updating the select drop downs
  observeEvent(input$reason_hitter, { # hitter
    req(input$reason_hitter)
    
    new_choices <- switch(input$reason_hitter,
                          "None" = c("None"),
                          "Other (Interference)" = c("First", "Out"),
                          "Drop Third Called" = c("First", "Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("First", "Second", "Third", "Home", "Out"))
    updateSelectInput(session, "outcome_hitter", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$reason_first, { # first base
    req(input$reason_first)
    
    new_choices <- switch(input$reason_first,
                          "Choose Option" = c("None"), 
                          "Steal" = c("Second", "Third", "Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Second", "Third", "Home", "Out"),
                          "Wild Pitch" = c("Second", "Third", "Home", "Out"), 
                          "Pass Ball" = c("Second", "Third", "Home", "Out"),
                          "Drop Third Called" = c("Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Second", "Third", "Home", "Out"),
                          "Other (Interference)" = c("Second", "Out"),
                          "Out of Bounds" = c("Second")
    )
    updateSelectInput(session, "outcome_first", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$reason_second, { # second base
    req(input$reason_second)
    
    new_choices <- switch(input$reason_second,
                          "Choose Option" = "None", 
                          "Steal" = c("Third", "Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Third", "Home", "Out"), 
                          "Wild Pitch" = c("Third", "Home", "Out"), 
                          "Pass Ball" = c("Third", "Home", "Out"),
                          "Drop Third Called" = c("Stay on Second", "Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Stay on Second", "Third", "Home", "Out"),
                          "Other (Interference)" = c("Third", "Out"),
                          "Out of Bounds" = c("Third")
    )
    updateSelectInput(session, "outcome_second", choices = new_choices, selected = new_choices[1])
  })
  
  observeEvent(input$reason_third, { # third base
    req(input$reason_third)
    
    new_choices <- switch(input$reason_third,
                          "Choose Option" = "None", 
                          "Steal" = c("Home"), 
                          "Caught Stealing" = c("Out"), 
                          "Picked Off" = c("Out"), 
                          "Error" = c("Home", "Out"), 
                          "Wild Pitch" = c("Home", "Out"), 
                          "Pass Ball" = c("Home", "Out"),
                          "Drop Third Called" = c("Stay on Third", "Home", "Out"),
                          "Drop Third Swinging" = c("Stay on Third", "Home", "Out"),
                          "Other (Interference)" = c("Home", "Out"),
                          "Out of Bounds" = c("Home")
    )
    updateSelectInput(session, "outcome_third", choices = new_choices, selected = new_choices[1])
  })
  
  # Runners Advance Modal (one-time show)
  observeEvent(input$runners_advance, {
    
    # Make sure there has been a pitch
    if (is.na(pitch_values$last_pitch_num) || is.null(pitch_values$last_pitch_num)) {
      showNotification("There are no pitches for this game yet", type = "warning")
      return()
    }
    
    # Initialize plot data
    bases <- data.frame(
      base = c("First", "Second", "Third", "Home"),
      x = c(192, 192, 102, 102),
      y = c(102, 192, 192, 102),
      occupied = I(list(game_state$runners$first, game_state$runners$second, game_state$runners$third, NA))
    )
    runner_plot_data$occupied <- bases[!sapply(bases$occupied, is.na), c("base", "x", "y")]
    
    # Options for reasoning
    the_hitter_choices <- c("None", "Other (Interference)")
    the_first_choices <- if(is.na(game_state$runners$first)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Other (Interference)", "Out of Bounds")
    }
    the_second_choices <- if(is.na(game_state$runners$second)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Other (Interference)", "Out of Bounds")
    }
    the_third_choices <- if(is.na(game_state$runners$third)) {
      "Empty"
    } else {
      c("Choose Option", "Steal", "Caught Stealing", "Picked Off", "Error", 
        "Wild Pitch", "Pass Ball", "Other (Interference)", "Out of Bounds")
    }
    
    # pop up to decide on reasoning
    showModal(modalDialog(
      title = "Runners Advance",
      easyClose = FALSE,
      tagList(
        plotOutput("runner_field", height = "400px", width = "400px"),
        
        fluidRow(
          column(3,
                 tags$h4("The Hitter"),
                 selectInput("reason_hitter", "Reason For Change:", choices = the_hitter_choices),
                 selectInput("outcome_hitter", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("First Base Runner"),
                 selectInput("reason_first", "Reason For Change:", choices = the_first_choices),
                 selectInput("outcome_first", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Second Base Runner"),
                 selectInput("reason_second", "Reason For Change:", choices = the_second_choices),
                 selectInput("outcome_second", "Outcome of Play:", choices = NULL)
          ),
          column(3,
                 tags$h4("Third Base Runner"),
                 selectInput("reason_third", "Reason For Change:", choices = the_third_choices),
                 selectInput("outcome_third", "Outcome of Play:", choices = NULL)
          )
        ),
        uiOutput("runner_options")
      ),
      footer = tagList(
        actionButton("confirm_advance", "Confirm Change", class = "btn btn-default"),
        actionButton("cancel_modal", "Cancel", class = "btn btn-default")
      ),
      size = "l"
    ))
  })
  
  # Render Field (allows for only one point to be on the graph)
  output$runner_field <- renderPlot({
    p <- draw_baseball_field() +
      xlim(40, 250) +
      ylim(40, 250)
    
    # Add occupied bases
    if (nrow(runner_plot_data$occupied) > 0) {
      p <- p + geom_point(data = runner_plot_data$occupied, aes(x = x, y = y), color = "yellow", size = 8, shape = 15)
    }
    p
  }, height = 400, width = 400)
  
  # Confirm Advance for runner advance button
  observeEvent(input$confirm_advance, {
    if (input$reason_hitter %in% c("None","Choose Option") &&
        input$reason_first %in% c("Empty","Choose Option") &&
        input$reason_second %in% c("Empty","Choose Option") &&
        input$reason_third %in% c("Empty","Choose Option")) {
      showNotification("Please update at least one runner or the hitter.", type = "error")
      return()
    } else {
      print(last_pitch_reactive()$pitch_outcome) # not needed
      # Store previous runners
      prev_runners <- list(
        first = if (is.na(game_state$runners$first)) NA_real_ else game_state$runners$first,
        second = if (is.na(game_state$runners$second)) NA_real_ else game_state$runners$second,
        third = if (is.na(game_state$runners$third)) NA_real_ else game_state$runners$third,
        outcome_first = NA_character_,
        outcome_second = NA_character_,
        outcome_third = NA_character_,
        reason_first = NA_character_,
        reason_second = NA_character_,
        reason_third = NA_character_
      )
      
      # Needed for validations
      selected_reasons <- c(input$reason_first, input$reason_second, input$reason_third)
      
      # Valuation 1: Wild Pitch and Pass Ball need to have the last event be a pitch
      if (any(selected_reasons %in% c("Wild Pitch", "Pass Ball")) &&
          !(last_pitch_reactive()$pitch_outcome %in% c("Ball", "Called Strike", "Swinging Strike"))) {
        showNotification("For Wild Pitch and Pass Ball, the last event has to be a pitch.", type = "error")
        return()
      }
      
      # Set up for val 2 & 3
      runner_values <- setNames(
        c(
          if (input$reason_hitter == "None") 5 else position_map[input$outcome_hitter],
          if (input$outcome_first %in% c("Second", "Third", "Home", "Out")) position_map[input$outcome_first] else 5,
          if (input$outcome_second %in% c("Third", "Home", "Out")) position_map[input$outcome_second] else 5,
          if (input$outcome_third %in% c("Home", "Out")) position_map[input$outcome_third] else 5
        ),
        c("batter", "first", "second", "third")
      )
      
      # not needed, this was used for debugging - optional
      runner_order <- c(runner_values["batter"], runner_values["first"], runner_values["second"], runner_values["third"])
      print(runner_order)
      
      # Valuation 2: Illegal passing of runners
      if (any(runner_values["batter"] > runner_values[c("first", "second", "third")] & runner_values["batter"] != 5) ||
          any(runner_values["first"] > runner_values[c("second", "third")] & runner_values["first"] != 5) ||
          runner_values["second"] > runner_values["third"] && runner_values["second"] != 5) {
        showNotification("You cannot have runners pass each other!", type = "error")
        return()
      }
      
      # Valuation 3: Runners going to same base
      if (
        any(c(
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["first"] && runner_values["first"] < 4,
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["second"] && runner_values["second"] < 4,
          runner_values["batter"] < 4 && runner_values["batter"] == runner_values["third"] && runner_values["third"] < 4,
          runner_values["first"] < 4 && runner_values["first"] == runner_values["second"] && runner_values["second"] < 4,
          runner_values["first"] < 4 && runner_values["first"] == runner_values["third"] && runner_values["third"] < 4,
          runner_values["second"] < 4 && runner_values["second"] == runner_values["third"] && runner_values["third"] < 4
        ))) {
        showNotification("Runners cannot end up at the same base, make sure to adjust that!", type = "error")
        return()
      }
      
      #Number of outs on Bases
      sum_of_outs <- sum(
        if (input$reason_hitter == "None") 0 else ifelse(input$outcome_hitter == "Out", 1, 0),
        if (input$reason_first %in% c("Empty","Choose Option")) 0 else ifelse(input$outcome_first == "Out", 1, 0),
        if (input$reason_second %in% c("Empty","Choose Option")) 0 else ifelse(input$outcome_second == "Out", 1, 0),
        if (input$reason_third %in% c("Empty","Choose Option")) 0 else ifelse(input$outcome_third == "Out", 1, 0)
      )
      
      # Validation 4: Too many people are being called out for the play (NEEDS WORK)
      if (game_state$outs + sum_of_outs > 3) {
        showNotification("Too many players are getting out for this game situation", type = "error")
        return()
      }
      
      # Validation 5: Not enough people are being called out for this play
      if (any(selected_reasons %in% c("Caught Stealing", "Picked Off")) &&
          sum_of_outs == 0) {
        showNotification("Not enough players are getting out for this situation", type = "error")
        return()
      }
      
      # Reset runners
      game_state$runners$third <- NA_real_
      game_state$runners$second <- NA_real_
      game_state$runners$first <- NA_real_
      
      batting_team_score <- ifelse(game_state$half == "Top", "away_score", "home_score")
      current_outs <- game_state$outs # needed for previous bugs
      
      #Third
      if (!is.na(prev_runners$third)) {
        if (input$reason_third %in% c("Steal", "Caught Stealing", "Picked Off", 
                                      "Error", "Wild Pitch", "Pass Ball", 
                                      "Drop Third Called", "Drop Third Swinging", 
                                      "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_third <- input$reason_third
          if (input$outcome_third == "Home") {
            prev_runners$outcome_third <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else {
            prev_runners$outcome_third <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_third <- "Stayed"
          prev_runners$reason_third <- "None"
          game_state$runners$third <- prev_runners$third
        }
      }
      
      #Second
      if (!is.na(prev_runners$second)) {
        if (input$reason_second %in% c("Steal", "Caught Stealing", "Picked Off", 
                                       "Error", "Wild Pitch", "Pass Ball", 
                                       "Drop Third Called", "Drop Third Swinging", 
                                       "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_second <- input$reason_second
          if (input$outcome_second == "Home") {
            prev_runners$outcome_second <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else if (input$outcome_second == "Third"){
            game_state$runners$third <- prev_runners$second
            prev_runners$outcome_second <- "Third"
          } else {
            prev_runners$outcome_second <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_second <- "Stayed"
          prev_runners$reason_second <- "None"
          game_state$runners$second <- prev_runners$second
        }
      }
      
      #First
      if (!is.na(prev_runners$first)) {
        if (input$reason_first %in% c("Steal", "Caught Stealing", "Picked Off", 
                                      "Error", "Wild Pitch", "Pass Ball", 
                                      "Drop Third Called", "Drop Third Swinging", 
                                      "Other (Interference)", "Out of Bounds")) {
          prev_runners$reason_first <- input$reason_first
          if (input$outcome_first == "Home") {
            prev_runners$outcome_first <- "Scored"
            game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
          } else if (input$outcome_first == "Third"){
            game_state$runners$third <- prev_runners$first
            prev_runners$outcome_first <- "Third"
          } else if (input$outcome_first == "Second"){
            game_state$runners$second <- prev_runners$first
            prev_runners$outcome_first <- "Second"
          } else {
            prev_runners$outcome_first <- "Out"
            game_state$outs <- game_state$outs + 1
          }
        } else {
          prev_runners$outcome_first <- "Stayed"
          prev_runners$reason_first <- "None"
          game_state$runners$first <- prev_runners$first
        }
      }
      
      current_hitter_id <- if (game_state$half == "Top") {
        game_state$away_lineup[game_state$current_batter_idx]
      } else {
        game_state$home_lineup[game_state$current_batter_idx]
      } 
      
      # Home
      if (input$reason_hitter %in% c("Other (Interference)", "Drop Third Swinging", "Drop Third Called")) {
        if (input$outcome_hitter == "First") {
          game_state$runners$first <- current_hitter_id
        } else {
          game_state$outs <- game_state$outs + 1
        }
      }
      
      
      #Adding into database for things that happened during the pitch
      if (any(c(input$reason_hitter, input$reason_first, input$reason_second, input$reason_third) %in% c("Wild Pitch", "Pass Ball", "Drop Third Swinging", "Drop Third Called"))) {
        
        # Update database with runner outcomes
        dbExecute(connect_to_db, "
          UPDATE global_pitch_data
          SET batter_outcome = $1, first_base_outcome = $2, second_base_outcome = $3, third_base_outcome = $4, first_base_reason = $5, second_base_reason = $6, third_base_reason = $7
          WHERE game_pitch_number = $8 AND event_id = $9",
                  params = list(input$outcome_hitter, prev_runners$outcome_first, prev_runners$outcome_second, prev_runners$outcome_third,
                                prev_runners$reason_first, prev_runners$reason_second, prev_runners$reason_third,
                                game_state$pitch_count, pitch_values$current_event_id))
        
      } else { # other reasons did not happen during the pitch \/
        game_state$pitch_count <- game_state$pitch_count + 1
        if (input$reason_hitter == "Other (Interference)"){
          # This is if the batter has an interference
          middle_pitch_record_one <- data.frame(
            event_id = pitch_values$current_event_id,
            game_pitch_number = game_state$pitch_count,
            is_pitch_event = FALSE,
            hitter_id = current_hitter_id,
            pitcher_id = game_state$current_pitcher,
            pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(game_state$current_pitcher)]],
            at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
            game_state$at_bat_pitch_number,
            half_inning = game_state$half %||% "Top",
            inning = game_state$inning %||% 1,
            outs = current_outs %||% 0,
            strikes = game_state$strikes %||% 0,
            balls = game_state$balls %||% 0,
            pitch_outcome = "Other (Interference)",
            pitch_velo = NA_real_,
            pitch_type = NA_character_,
            pitch_location_x = NA_real_,
            pitch_location_y = NA_real_,
            ball_in_play_outcome = "Other (Interference)",
            batter_outcome = if (input$outcome_hitter %in% c("Out", "None")) {
              "None"
            } else if (input$outcome_hitter == "Home"){
              "Scored"
            } else {
              input$outcome_hitter
            },
            ball_flight = "None",
            contact_type = "None",
            spray_chart_x = NA_real_,
            spray_chart_y = NA_real_,
            first_base_runner = prev_runners$first,
            first_base_outcome = prev_runners$outcome_first,
            first_base_reason = prev_runners$reason_first,
            second_base_runner = prev_runners$second,
            second_base_outcome = prev_runners$outcome_second,
            second_base_reason = prev_runners$reason_second,
            third_base_runner = prev_runners$third,
            third_base_outcome = prev_runners$outcome_third,
            third_base_reason = prev_runners$reason_third,
            stringsAsFactors = FALSE
          )
          
          # Write to database
          dbWriteTable(connect_to_db, "global_pitch_data", middle_pitch_record_one, append = TRUE)
          
        } else {
          # Create New Line of Data for Runner Advancing outside of the reasons above
          middle_pitch_record_two <- data.frame(
            event_id = pitch_values$current_event_id,
            game_pitch_number = game_state$pitch_count,
            is_pitch_event = FALSE,
            hitter_id = NA_real_,
            pitcher_id = game_state$current_pitcher,
            pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(game_state$current_pitcher)]],
            at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
            half_inning = game_state$half %||% "Top",
            inning = game_state$inning %||% 1,
            outs = current_outs %||% 0,
            strikes = game_state$strikes %||% 0,
            balls = game_state$balls %||% 0,
            pitch_outcome = "None",
            pitch_velo = NA_real_,
            pitch_type = NA_character_,
            pitch_location_x = NA_real_,
            pitch_location_y = NA_real_,
            ball_in_play_outcome = "None",
            batter_outcome = "None",
            ball_flight = "None",
            contact_type = "None",
            spray_chart_x = NA_real_,
            spray_chart_y = NA_real_,
            first_base_runner = prev_runners$first,
            first_base_outcome = prev_runners$outcome_first,
            first_base_reason = prev_runners$reason_first,
            second_base_runner = prev_runners$second,
            second_base_outcome = prev_runners$outcome_second,
            second_base_reason = prev_runners$reason_second,
            third_base_runner = prev_runners$third,
            third_base_outcome = prev_runners$outcome_third,
            third_base_reason = prev_runners$reason_third,
            stringsAsFactors = FALSE
          )
          
          # Write to database
          dbWriteTable(connect_to_db, "global_pitch_data", middle_pitch_record_two, append = TRUE)
        }  
      }  
      
      # Update the inning, batter, and counts if necessary
      if (game_state$outs >=3) {
        update_count_and_inning()
      } else if (input$reason_hitter == "Other (Interference)"){
        update_count_and_inning(reset = TRUE)
        game_state$current_batter_idx <- game_state$current_batter_idx + 1
        if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
      }
      
      # Trigger global_pitch_data refresh
      pitch_update_trigger(pitch_update_trigger() + 1)
      
      removeModal()
    }
  })
  
  ## ---- Undo Pitch Button ----
  observeEvent(input$undo_pitch, {
    removeModal()
    print(last_pitch_reactive())
    # Check if there are any pitches
    if (is.na(pitch_values$last_pitch_num) || is.null(pitch_values$last_pitch_num)) {
      showNotification("There are no pitches for this game yet", type = "warning")
      return()
    }
    
    # Update the game state
    if (nrow(last_record_reactive()) == 0) {
      # If last data input was a pitch event, is_pitch_event == True
      game_state$inning <- last_pitch_reactive()$inning
      game_state$half <- last_pitch_reactive()$half_inning
      game_state$outs <- last_pitch_reactive()$outs
      game_state$balls <- last_pitch_reactive()$balls
      game_state$strikes <- last_pitch_reactive()$strikes
      game_state$current_pitcher <- last_pitch_reactive()$pitcher_id
      game_state$at_bat_pitch_number <- last_pitch_reactive()$at_bat_pitch_number - 1
      
      # Delete the last pitch
      dbExecute(connect_to_db, paste0("DELETE FROM global_pitch_data 
                        WHERE event_id = ", pitch_values$current_event_id, 
                                      " AND game_pitch_number = ", pitch_values$last_pitch_num))
      
      #Update Pitchers Pitch Count
      game_state$pitcher_pitch_count[[as.character(last_pitch_reactive()$pitcher_id)]] <- game_state$pitcher_pitch_count[[as.character(last_pitch_reactive()$pitcher_id)]] - 1
      
      #Use last_pitch_data
      game_state$runners$first <- if (is.na(last_pitch_reactive()$first_base_runner)) NA_real_ else as.numeric(last_pitch_reactive()$first_base_runner)
      game_state$runners$second <- if (is.na(last_pitch_reactive()$second_base_runner)) NA_real_ else as.numeric(last_pitch_reactive()$second_base_runner)
      game_state$runners$third <- if (is.na(last_pitch_reactive()$third_base_runner)) NA_real_ else as.numeric(last_pitch_reactive()$third_base_runner)
      
      #number of runners that scored from last pitch event
      runners_that_scored_is_pitch <- as.numeric(
        sum(
          last_pitch_reactive()$batter_outcome == "Scored",
          last_pitch_reactive()$first_base_outcome == "Scored", 
          last_pitch_reactive()$second_base_outcome == "Scored", 
          last_pitch_reactive()$third_base_outcome == "Scored",
          na.rm = TRUE
        )
      )
      if (is.na(runners_that_scored_is_pitch) || length(runners_that_scored_is_pitch) == 0) {
        runners_that_scored_is_pitch <- 0
      }
      
      if (last_pitch_reactive()$half_inning == "Top") {
        game_state$away_score <- game_state$away_score - runners_that_scored_is_pitch
        game_state$home_pitcher <- last_pitch_reactive()$pitcher_id
      } else {
        game_state$home_score <- game_state$home_score - runners_that_scored_is_pitch
        game_state$away_pitcher <- last_pitch_reactive()$pitcher_id
      }
      
      
    } else {
      # If last data input was not a pitch event, is_pitch_event == False
      game_state$inning <- last_record_reactive()$inning
      game_state$half <- last_record_reactive()$half_inning
      game_state$outs <- last_record_reactive()$outs
      game_state$balls <- last_record_reactive()$balls
      game_state$strikes <- last_record_reactive()$strikes
      game_state$current_pitcher <- last_record_reactive()$pitcher_id
      game_state$at_bat_pitch_number <- last_record_reactive()$at_bat_pitch_number
      
      game_state$runners$first <- if (is.na(last_record_reactive()$first_base_runner)) NA_real_ else as.numeric(last_record_reactive()$first_base_runner)
      game_state$runners$second <- if (is.na(last_record_reactive()$second_base_runner)) NA_real_ else as.numeric(last_record_reactive()$second_base_runner)
      game_state$runners$third <- if (is.na(last_record_reactive()$third_base_runner)) NA_real_ else as.numeric(last_record_reactive()$third_base_runner)
      
      # runners that scored from last non pitch event
      runners_that_scored_not_pitch <- sum(
        last_record_reactive()$first_base_outcome == "Scored", 
        na.rm = TRUE
      ) +
        sum(
          last_record_reactive()$second_base_outcome == "Scored", 
          na.rm = TRUE
        ) +
        sum(
          last_record_reactive()$third_base_outcome == "Scored", 
          na.rm = TRUE
        )
      
      # Delete the last record
      dbExecute(connect_to_db, paste0("DELETE FROM global_pitch_data 
                        WHERE event_id = ", pitch_values$current_event_id, 
                                      " AND game_pitch_number = ", pitch_values$last_non_pitch_num,
                                      " AND is_pitch_event = ", FALSE))
      
      
      if (!is.na(runners_that_scored_not_pitch) || length(runners_that_scored_not_pitch) != 0 || runners_that_scored_not_pitch != 0) {
        if (last_record_reactive()$half_inning[1] == "Top") {
          game_state$away_score <- game_state$away_score - runners_that_scored_not_pitch
          game_state$home_pitcher <- last_pitch_reactive()$pitcher_id
        } else {
          game_state$home_score <- game_state$home_score - runners_that_scored_not_pitch
          game_state$away_pitcher <- last_pitch_reactive()$pitcher_id
        }
      }
    }
    
    # Update Game Pitch Counts
    game_state$pitch_count <- game_state$pitch_count - 1
    
    # Trigger global_pitch_data refresh
    pitch_update_trigger(pitch_update_trigger() + 1)
  })
  
  ## ---- Functions and Extra ----
  # Reset ball in play
  ball_in_play_data_reset <- function() {
    ball_in_play_data$ball_in_play_outcome <- "None"
    ball_in_play_data$ball_flight <- "None"
    ball_in_play_data$contact_type <- "None"
    ball_in_play_data$spray_chart_x <- NA_real_
    ball_in_play_data$spray_chart_y <- NA_real_
  }
  
  # Reset pitch input fields
  resetPitchInputs <- function() {
    updateSliderInput(session, "pitch_speed", value = 0)
    updateRadioButtons(session, "pitch_type", selected = character(0))
    pitches$location <- data.frame(xlocation = numeric(), ylocation = numeric())
  }
  
  # Function to record new pitch
  record_pitch <- function(outcome) {
    game_state$pitch_count <- game_state$pitch_count + 1
    
    # current hitter
    current_hitter_id <- if (game_state$half == "Top") {
      game_state$away_lineup[game_state$current_batter_idx]
    } else {
      game_state$home_lineup[game_state$current_batter_idx]
    } 
    
    # num of runners
    num_runners <- sum(!sapply(game_state$runners, is.na))
    
    # current pitcher
    current_pitcher_id <- game_state$current_pitcher %||% NA_character_
    if (outcome %in% c("Clock Hitter", "Clock Pitcher", "Balk")) {
      game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]] <- (game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]] %||% 0)
    } else {
      game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]] <- (game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]] %||% 0) + 1
    }
    
    # current pitch location
    if (!is.data.frame(pitches$location) || nrow(pitches$location) == 0 || !all(c("xlocation", "ylocation") %in% names(pitches$location))) {
      pitches$location <<- data.frame(xlocation = NA_real_, ylocation = NA_real_)
    }
    loc_row <- nrow(pitches$location)
    pitch_x <- if (loc_row > 0 && !is.na(pitches$location$xlocation[loc_row])) pitches$location$xlocation[loc_row] else NA_real_
    pitch_y <- if (loc_row > 0 && !is.na(pitches$location$ylocation[loc_row])) pitches$location$ylocation[loc_row] else NA_real_
    
    # add at bat pitch number
    game_state$at_bat_pitch_number <- game_state$at_bat_pitch_number + 1
    
    if (outcome == "Balk") {
      # new row of data if balk
      pitch_record <- data.frame(
        event_id = game_info$event_id,
        game_pitch_number = game_state$pitch_count %||% 1,
        is_pitch_event = FALSE,
        hitter_id = current_hitter_id,
        pitcher_id = current_pitcher_id,
        pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]],
        at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
        half_inning = game_state$half %||% "Top",
        inning = game_state$inning %||% 1,
        outs = game_state$outs %||% 0,
        strikes = game_state$strikes %||% 0,
        balls = game_state$balls %||% 0,
        pitch_outcome = if (num_runners > 0) {
          "None" 
        } else {
          "Ball"
        },
        pitch_velo = NA_real_,
        pitch_type = NA_character_,
        pitch_location_x = NA_real_,
        pitch_location_y = NA_real_,
        ball_in_play_outcome = "Balk",
        batter_outcome = if (game_state$balls == 3) {
          "First"
        } else {
          "None"
        },
        ball_flight = "None",
        contact_type = "None",
        spray_chart_x = NA_real_,
        spray_chart_y = NA_real_,
        first_base_runner = if (is.na(game_state$runners$first) || length(game_state$runners$first) == 0) NA_real_ else as.numeric(game_state$runners$first),
        first_base_outcome = if (is.na(game_state$runners$first)) {
          NA_character_
        } else {
          "Second"
        },
        first_base_reason = if (is.na(game_state$runners$first)) {
          NA_character_
        } else {
          "Balk"
        },
        second_base_runner = if (is.na(game_state$runners$second) || length(game_state$runners$second) == 0) NA_real_ else as.numeric(game_state$runners$second),
        second_base_outcome = if (is.na(game_state$runners$second)) {
          NA_character_
        } else {
          "Third"
        },
        second_base_reason = if (is.na(game_state$runners$second)) {
          NA_character_
        } else {
          "Balk"
        },
        third_base_runner = if (is.na(game_state$runners$third) || length(game_state$runners$third) == 0) NA_real_ else as.numeric(game_state$runners$third),
        third_base_outcome = if (is.na(game_state$runners$third)) {
          NA_character_
        } else {
          "Scored"
        },
        third_base_reason = if (is.na(game_state$runners$third)) {
          NA_character_
        } else {
          "Balk"
        },
        stringsAsFactors = FALSE
      )
      
    } else {
      # new row of data for everything else
      pitch_record <- data.frame(
        event_id = game_info$event_id,
        game_pitch_number = game_state$pitch_count %||% 1,
        is_pitch_event = if (outcome %in% c("Clock Hitter", "Clock Pitcher")) {
          FALSE
        } else {
          TRUE
        },
        hitter_id = current_hitter_id,
        pitcher_id = current_pitcher_id,
        pitchers_pitch_count = game_state$pitcher_pitch_count[[as.character(current_pitcher_id)]],
        at_bat_pitch_number = ifelse(game_state$at_bat_pitch_number == 0, 1, game_state$at_bat_pitch_number),
        half_inning = game_state$half %||% "Top",
        inning = game_state$inning %||% 1,
        outs = game_state$outs %||% 0,
        strikes = game_state$strikes %||% 0,
        balls = game_state$balls %||% 0,
        pitch_outcome = if (outcome %in% c("Clock Hitter", "Drop Third Called")) {
          "Called Strike" 
        } else if (outcome == "Drop Third Swinging") {
          "Swinging Strike"
        } else if (outcome == "Clock Pitcher") {
          "Ball"
        } else {
          outcome
        },
        pitch_velo = if (outcome %in% c("Clock Hitter", "Clock Pitcher")) {
          NA_real_
        } else {
          as.numeric(input$pitch_speed)
        },
        pitch_type = if (outcome %in% c("Clock Hitter", "Clock Pitcher")) {
          NA_real_
        } else {
          input$pitch_type
        },
        pitch_location_x = if (outcome %in% c("Clock Hitter", "Clock Pitcher")) {
          NA_real_
        } else {
          as.numeric(pitch_x)
        },
        pitch_location_y = if (outcome %in% c("Clock Hitter", "Clock Pitcher")) {
          NA_real_
        } else {
          as.numeric(pitch_y)
        },
        ball_in_play_outcome = if (outcome == "Ball In Play") {
          ball_in_play_data$ball_in_play_outcome
        } else if (outcome == "Hit By Pitch") {
          "HBP"
        } else if (outcome == "Ball" & game_state$balls == 3) {
          "Walk"
        } else if (outcome %in% c("Called Strike", "Drop Third Called") & game_state$strikes == 2) {
          "SO Looking"
        } else if (outcome %in% c("Swinging Strike", "Drop Third Swinging") & game_state$strikes == 2) {
          "SO Swinging"
        } else {
          "None"
        },
        batter_outcome = if (outcome == "Ball" & game_state$balls == 3) {
          "First"
        } else if (outcome == "Hit By Pitch") {
          "First"
        } else if (ball_in_play_data$ball_in_play_outcome == "Homerun") {
          "Scored"
        } else if (ball_in_play_data$ball_in_play_outcome == "Triple") {
          "Third"
        } else if (ball_in_play_data$ball_in_play_outcome == "Double") {
          "Second"
        } else if (ball_in_play_data$ball_in_play_outcome %in% c("Single", "Error", "Fielder's Choice")) {
          "First"
        } else {
          "None"
        },
        ball_flight = ball_in_play_data$ball_flight %||% NA_character_,
        contact_type = ball_in_play_data$contact_type %||% NA_character_,
        spray_chart_x = ball_in_play_data$spray_chart_x %||% NA_real_,
        spray_chart_y = ball_in_play_data$spray_chart_y %||% NA_real_,
        first_base_runner = if (is.na(game_state$runners$first) || length(game_state$runners$first) == 0) NA_real_ else as.numeric(game_state$runners$first),
        first_base_outcome = if (is.na(game_state$runners$first)) {
          NA_character_
        } else if (outcome == "Hit By Pitch") {
          "Second"
        } else if (outcome == "Ball" & game_state$balls == 3) {
          "Second"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "Stayed"
        } else {
          NA_character_
        },
        first_base_reason = if (is.na(game_state$runners$first)) {
          NA_character_
        } else if (outcome == "Hit By Pitch") {
          "HBP"
        } else if (outcome == "Ball" & game_state$balls == 3) {
          "Walk"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "None"
        } else {
          NA_character_
        },
        second_base_runner = if (is.na(game_state$runners$second) || length(game_state$runners$second) == 0) NA_real_ else as.numeric(game_state$runners$second),
        second_base_outcome = if (is.na(game_state$runners$second)) {
          NA_character_
        } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first)) {
          "Third"
        } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first)) {
          "Third"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "Stayed"
        } else {
          NA_character_
        },
        second_base_reason = if (is.na(game_state$runners$second)) {
          NA_character_
        } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first)) {
          "HBP"
        } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first)) {
          "Walk"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "None"
        } else {
          NA_character_
        },
        third_base_runner = if (is.na(game_state$runners$third) || length(game_state$runners$third) == 0) NA_real_ else as.numeric(game_state$runners$third),
        third_base_outcome = if (is.na(game_state$runners$third)) {
          NA_character_
        } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first & game_state$runners$second)) {
          "Scored"
        } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first & game_state$runners$second)) {
          "Scored"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "Stayed"
        } else {
          NA_character_
        },
        third_base_reason = if (is.na(game_state$runners$third)) {
          NA_character_
        } else if (outcome == "Hit By Pitch" & !is.na(game_state$runners$first & game_state$runners$second)) {
          "HBP"
        } else if (outcome == "Ball" & game_state$balls == 3 & !is.na(game_state$runners$first & game_state$runners$second)) {
          "Walk"
        } else if (ball_in_play_data$ball_in_play_outcome == "None") {
          "None"
        } else {
          NA_character_
        },
        stringsAsFactors = FALSE
      )
    }
    
    print(pitch_record) # not needed
    
    # Write to database
    dbWriteTable(connect_to_db, "global_pitch_data", pitch_record, append = TRUE)
    
    # Balk Functioning
    batting_team_score <- ifelse(game_state$half == "Top", "away_score", "home_score")
    if (outcome == "Balk") {
      if (any(!is.na(game_state$runners[c("first", "second", "third")]))) {
        if (!is.na(game_state$runners$third)) {
          game_state$runners$third <- NA_real_
          game_state[[batting_team_score]] <- game_state[[batting_team_score]] + 1
        }
        if (!is.na(game_state$runners$second)) {
          game_state$runners$third <- game_state$runners$second
          game_state$runners$second <- NA_real_
        }
        if (!is.na(game_state$runners$first)) {
          game_state$runners$second <- game_state$runners$first
          game_state$runners$first <- NA_real_
        }
      } else {
        update_count_and_inning(is_ball = TRUE)
      }
    }
    
    # Trigger global_pitch_data refresh
    pitch_update_trigger(pitch_update_trigger() + 1)
    
    # Reset pitch location and pending outcomes
    pitches$location <<- data.frame(xlocation = NA_real_, ylocation = NA_real_)
    
  }
  
  # Update count & game state helper function
  update_count_and_inning <- function(is_strike = FALSE, is_foul = FALSE, is_ball = FALSE, reset = FALSE) {
    if (reset) { # reset the count
      game_state$balls <- 0
      game_state$strikes <- 0
      game_state$at_bat_pitch_number <- 0
      return()
    }
    
    # get current hitter id
    current_hitter_id <- if (game_state$half == "Top") {
      game_state$away_lineup[game_state$current_batter_idx]
    } else {
      game_state$home_lineup[game_state$current_batter_idx]
    }
    
    if (is_strike) { # recorded strike
      game_state$strikes <- game_state$strikes + 1
      if (game_state$strikes == 3) { # if three strikes, the guy is out and you go to the next batter
        game_state$outs <- game_state$outs + 1
        game_state$current_batter_idx <- game_state$current_batter_idx + 1
        game_state$at_bat_pitch_number <- 0
        if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
        game_state$balls <- 0
        game_state$strikes <- 0
      }
    } else if (is_foul && game_state$strikes < 2) { # foul ball cannot be strike three
      game_state$strikes <- game_state$strikes + 1
    } else if (is_ball) { # recorded ball
      game_state$balls <- game_state$balls + 1
      if (game_state$balls == 4) { # if four balls == walk, then it will move runners that is needed, goes to next batter
        if (!is.na(game_state$runners$third & game_state$runners$second & game_state$runners$first)) {
          game_state$runners$third <- game_state$runners$second
          game_state$runners$second <- game_state$runners$first
          game_state$runners$first <- current_hitter_id
          game_state[[ifelse(game_state$half == "Top", "away_score", "home_score")]] <- game_state[[ifelse(game_state$half == "Top", "away_score", "home_score")]] + 1
        } else if (!is.na(game_state$runners$second & game_state$runners$first)) {
          game_state$runners$third <- game_state$runners$second
          game_state$runners$second <- game_state$runners$first
          game_state$runners$first <- current_hitter_id
        } else if (!is.na(game_state$runners$first)) {
          game_state$runners$second <- game_state$runners$first
          game_state$runners$first <- current_hitter_id
        } else {
          game_state$runners$first <- current_hitter_id
        } 
        game_state$current_batter_idx <- game_state$current_batter_idx + 1
        if (game_state$current_batter_idx > 9) game_state$current_batter_idx <- 1
        game_state$balls <- 0
        game_state$strikes <- 0
        game_state$at_bat_pitch_number <- 0
      }
    }
    
    if (game_state$outs >= 3) { # next inning if there are three outs
      # Store the next batter index for the current team
      if (game_state$half == "Top") {
        game_state$away_next_batter_idx <- game_state$current_batter_idx
      } else {
        game_state$home_next_batter_idx <- game_state$current_batter_idx
      }
      
      # Proceed to next half-inning, reset anything that is needed
      game_state$outs <- 0
      game_state$balls <- 0
      game_state$strikes <- 0
      game_state$at_bat_pitch_number <- 0
      game_state$runners$first <- NA_real_
      game_state$runners$second <- NA_real_
      game_state$runners$third <- NA_real_
      
      # switch the current batter idx to the team that is hitting
      if (game_state$half == "Top") {
        game_state$half <- "Bottom"
        game_state$current_batter_idx <- game_state$home_next_batter_idx
        game_state$current_pitcher <- game_state$away_pitcher # Away pitches in bottom
      } else {
        game_state$half <- "Top"
        game_state$inning <- game_state$inning + 1
        game_state$current_batter_idx <- game_state$away_next_batter_idx
        game_state$current_pitcher <- game_state$home_pitcher # Home pitches in top
      }
    }
    
    # This checks if the game is over, it will check if home team is winning after top 9, if away team wins after bottom 9, 
    #   if home team is winning by 10 after top 7, or if away team is winning by 10 after bottom 7
    #   -  Also will change depending on the number of innings specified, will work whether game is 7 or 9 innings
    if (game_state$inning >= game_state$inning_specifier &&
        game_state$half == "Bottom" &&
        game_state$home_score > game_state$away_score) {
      
      showModal(modalDialog(
        title = "End Game",
        paste(game_info$home_team, "Wins:", game_state$home_score, "-", game_state$away_score),
        footer = tagList(
          actionButton("end_game_confirm", "End Game"),
          actionButton("undo_pitch", "Undo Last Pitch")  # Custom cancel button
        ),
        easyClose = FALSE
      ))
    } else if (game_state$inning >= game_state$inning_specifier + 1 &&
               game_state$half == "Top" &&
               game_state$home_score < game_state$away_score) {
      
      showModal(modalDialog(
        title = "End Game",
        paste(game_info$away_team, "Wins:", game_state$home_score, "-", game_state$away_score),
        footer = tagList(
          actionButton("end_game_confirm", "End Game"),
          actionButton("undo_pitch", "Undo Last Pitch")  # Custom cancel button
        ),
        easyClose = FALSE
      ))
    } else if (game_state$inning >= game_state$inning_specifier - 2 &&
               game_state$half == "Bottom" &&
               game_state$home_score - 10 >= game_state$away_score) {
      
      showModal(modalDialog(
        title = "End Game",
        paste(game_info$home_team, "Wins:", game_state$home_score, "-", game_state$away_score),
        footer = tagList(
          actionButton("end_game_confirm", "End Game"),
          actionButton("undo_pitch", "Undo Last Pitch")  # Custom cancel button
        ),
        easyClose = FALSE
      ))
    } else if (game_state$inning >= game_state$inning_specifier - 1 &&
               game_state$half == "Top" &&
               game_state$home_score <= game_state$away_score - 10) {
      
      showModal(modalDialog(
        title = "End Game",
        paste(game_info$away_team, "Wins:", game_state$home_score, "-", game_state$away_score),
        footer = tagList(
          actionButton("end_game_confirm", "End Game"),
          actionButton("undo_pitch", "Undo Last Pitch")  # Custom cancel button
        ),
        easyClose = FALSE
      ))
    }
  }
  
  # Cancel Modal Button (Used a lot)
  observeEvent(input$cancel_modal, {
    removeModal()
  })
  
  # End game confirm button
  observeEvent(input$end_game_confirm, {
    removeModal()
    app_state$page <- "start"
  })
  
  # ---- Players Page ----
  ## ---- Data Table of All Players ----
  # Render the players table for listPage ("All players")
  output$player_list <- DT::renderDT({
    message("Rendering player_list. Rows: ", nrow(stored_players$data))
    # Force dependency on stored_players$data
    stored_players$data
    if (nrow(stored_players$data) == 0) {
      datatable(data.frame(Message = "No players in database yet"),
                options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      players_data <- stored_players$data %>%
        select(college_name, full_name, jersey_number, player_type, grad_year, batter_handedness, throwing_handedness, status)
      datatable(players_data, options = list(pageLength = 10, autoWidth = TRUE))
    }
  }, server = FALSE)
  
  ## ---- Add Players Individually ----
  # Render team selection dropdown
  output$team_selection_ui <- renderUI({
    valid_teams <- unique(teams$data$college_name[!teams$data$college_name %in% c("", "Choose Team") & !is.na(teams$data$college_name)])
    team_choices <- c("Choose Team", valid_teams)
    selectInput("team_selection", "Team", choices = team_choices, selected = "Choose Team")
  })
  
  ## Individually Adding players on the Input Page, this is the Confirm Button
  observeEvent(input$confirm_player, {
    req(input$team_selection != "Choose Team", input$first_name, input$last_name,
        input$jersey_number, input$player_type, input$grad_year, input$batter_handedness, 
        input$throwing_handedness)
    
    # get existing players
    existing_player <- dbGetQuery(connect_to_db, "
      SELECT * FROM players
      WHERE first_name = $1 AND last_name = $2 AND college_name = $3 AND jersey_number = $4",
                                  params = list(input$first_name, input$last_name, input$team_selection, input$jersey_number))
    if (nrow(existing_player) > 0) {
      showNotification("Player already exists!", type = "error")
      return()
    }
    
    # Generate unique player_id
    repeat {
      new_player_id <- sample(100000:999999, 1)
      id_check <- dbGetQuery(connect_to_db, "SELECT COUNT(*) as count FROM players WHERE player_id = $1",
                             params = list(new_player_id))
      if (id_check$count[1] == 0) break
    }
    
    new_player <- data.frame(
      player_id = new_player_id,
      college_name = input$team_selection,
      first_name = input$first_name,
      last_name = input$last_name,
      full_name = paste(input$first_name, input$last_name),
      jersey_number = input$jersey_number,
      player_type = input$player_type,
      grad_year = input$grad_year,
      batter_handedness = input$batter_handedness,
      throwing_handedness = input$throwing_handedness,
      status = ifelse(input$grad_year < current_year, "Graduated", "Active")
    )
    
    dbWriteTable(connect_to_db, "players", new_player, append = TRUE)
    stored_players$data <- dbReadTable(connect_to_db, "players")
    message("Player added. connect_to_db rows: ", nrow(stored_players$data))
    
    showNotification("Player added!", type = "message")
    # reset the inputs
    updateSelectInput(session, "team_selection", selected = "Choose Team")
    updateTextInput(session, "first_name", value = "")
    updateTextInput(session, "last_name", value = "")
    updateNumericInput(session, "jersey_number", value = NA)
    updateSelectInput(session, "player_type", selected = "")
    updateNumericInput(session, "grad_year", value = NA)
    updateSelectInput(session, "batter_handedness", selected = "")
    updateSelectInput(session, "throwing_handedness", selected = "")
  })
  
  ## ---- Bulk Adding of Players ----
  # Bulk Import of players
  observeEvent(input$import_players, {
    req(input$excel_file)
    
    uploaded_data <- read_excel(input$excel_file$datapath)
    required_columns <- c("college_name", "first_name", "last_name", "jersey_number", "player_type", "grad_year", "batter_handedness", "throwing_handedness")
    
    if (!all(required_columns %in% colnames(uploaded_data))) {
      showNotification("Invalid file format! Please check required column names.", type = "error")
      return()
    }
    
    # Load existing teams from database
    existing_teams <- dbReadTable(connect_to_db, "teams")
    if (!"college_name" %in% colnames(existing_teams)) {
      showNotification("Database error: teams table is missing 'college_name'. Please check the database.", type = "error")
      return()
    }
    
    # Find if any Excel College Names do not match SQL database
    unmatched <- setdiff(uploaded_data$college_name, existing_teams$college_name)
    if (length(unmatched) > 0) {
      showNotification(paste("Unmatched colleges:", paste(unmatched, collapse = ", ")), type = "error")
      return()
    }
    
    # Making sure format is the same
    uploaded_data <- uploaded_data %>%
      mutate(
        college_name = as.character(college_name),
        first_name = as.character(first_name),
        last_name = as.character(last_name),
        full_name = as.character(paste(first_name, last_name)),
        jersey_number = as.numeric(jersey_number),
        player_type = as.character(player_type),
        grad_year = as.numeric(grad_year),
        batter_handedness = as.character(batter_handedness),
        throwing_handedness = as.character(throwing_handedness)
      ) %>%
      mutate(across(where(is.character), ~ ifelse(is.na(.), "", .))
      )
    
    # Check for new players
    existing_players <- dbReadTable(connect_to_db, "players") %>%
      select(college_name, first_name, last_name, full_name, jersey_number, player_type, grad_year, batter_handedness, throwing_handedness)
    
    new_players <- uploaded_data %>%
      anti_join(existing_players, by = c("full_name", "batter_handedness", "throwing_handedness"))
    
    if (nrow(new_players) == 0) {
      showNotification("No new players to import. All already exist!", type = "warning")
      return()
    }
    
    # Assign unique PlayerIDs
    existing_ids <- dbGetQuery(connect_to_db, "SELECT player_id FROM players")$player_id
    all_possible_ids <- 100000:999999
    available_ids <- setdiff(all_possible_ids, existing_ids)
    new_players$player_id <- sample(available_ids, nrow(new_players))
    
    new_players <- new_players %>%
      mutate(status = ifelse(grad_year < current_year, "Graduated", "Active"))
    
    # Insert new players into database
    dbWriteTable(connect_to_db, "players", new_players, append = TRUE)
    
    # Update stored_players reactive value
    stored_players$data <- dbReadTable(connect_to_db, "players")
    
    showNotification("players imported successfully!", type = "message")
  })
  
  ## ---- Updating of players ----
  # Server-side filtering for edit player search
  observe({
    req(stored_players$data, app_state$page == "update_player") # Only update on updatePlayerPage
    choices <- c("Choose Player", stored_players$data$full_name)
    message("Updating edit_player_name. Choices length: ", length(choices))
    message("First 5 choices: ", paste(head(choices, 5), collapse = ", "))
    updateSelectizeInput(session, "edit_player_name", 
                         choices = choices,
                         server = TRUE)
  })
  
  # Prefill when player is selected
  observeEvent(input$edit_player_name, {
    req(input$edit_player_name)
    player_idx_edit <- which(stored_players$data$full_name == input$edit_player_name)
    if (length(player_idx_edit) == 0 & input$edit_player_name != "Choose Player") {
      output$player_info_display_edit <- renderText("Player not found!")
      return()
    }
    selected_player_edit <- stored_players$data[player_idx_edit, ]
    
    updateTextInput(session, "edit_first_name", value = selected_player_edit$first_name)
    updateTextInput(session, "edit_last_name", value = selected_player_edit$last_name)
    updateNumericInput(session, "edit_jersey_number", value = selected_player_edit$jersey_number)
    updateSelectInput(session, "edit_player_type", selected = selected_player_edit$player_type)
    updateNumericInput(session, "edit_grad_year", value = selected_player_edit$grad_year)
    updateSelectInput(session, "edit_batter_handedness", selected = selected_player_edit$batter_handedness)
    updateSelectInput(session, "edit_throwing_handedness", selected = selected_player_edit$throwing_handedness)
    updateSelectInput(session, "edit_status", selected = selected_player_edit$status)
  })
  
  # confirm the update of the player
  observeEvent(input$update_player, {
    req(input$edit_player_name)
    player_idx_update <- which(stored_players$data$full_name == input$edit_player_name)
    if (length(player_idx_update) == 0 & input$edit_player_name != "Choose Player") {
      showNotification("Player not found!", type = "error")
      return()
    }
    player_id <- stored_players$data$player_id[player_idx_update]
    dbExecute(connect_to_db, "
      UPDATE players
      SET first_name = $1, last_name = $2, full_name = $3, jersey_number = $4, player_type = $5, grad_year = $6, batter_handedness = $7, throwing_handedness = $8, status = $9
      WHERE player_id = $10",
              params = list(input$edit_first_name, input$edit_last_name, paste(input$edit_first_name, input$edit_last_name),
                            input$edit_jersey_number, input$edit_player_type, input$edit_grad_year,
                            input$edit_batter_handedness, input$edit_throwing_handedness, input$edit_status, player_id))
    stored_players$data <- dbReadTable(connect_to_db, "players")
    showNotification("Player updated!", type = "message")
    updateSelectizeInput(session, "edit_player_name", selected = "Choose Player")
  })
  
  ## ---- Deleting of Players ----
  # Server-side filtering for delete player search
  observe({
    req(stored_players$data, app_state$page == "update_player") # Only update on updatePlayerPage
    choices <- c("Choose Player", stored_players$data$full_name)
    message("Updating delete_player_id. Choices length: ", length(choices))
    message("First 5 choices: ", paste(head(choices, 5), collapse = ", "))
    updateSelectizeInput(session, "delete_player_id", 
                         choices = choices,
                         server = TRUE)
  })
  
  # Make sure they actually want to delete the player
  observeEvent(input$remove_player, {
    req(input$delete_player_id)
    player_idx_gone <- which(stored_players$data$full_name == input$delete_player_id)
    if (length(player_idx_gone) == 0 & input$remove_player != "Choose Player") {
      showNotification("Player ID not found!", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete", input$delete_player_id, "?"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_player", "Confirm", class = "btn-danger")
      )
    ))
  })
  
  # Deletion logic
  observeEvent(input$confirm_remove_player, {
    player_idx_gone <- which(stored_players$data$full_name == input$delete_player_id)
    if (length(player_idx_gone) == 0 & input$remove_player != "Choose Player") {
      showNotification("Player not found!", type = "error")
      return()
    }
    player_id <- stored_players$data$player_id[player_idx_gone]
    dbExecute(connect_to_db, "DELETE FROM players WHERE player_id = $1", params = list(player_id))
    showNotification("Player deleted!", type = "message")
    stored_players$data <- dbReadTable(connect_to_db, "players")
    removeModal()
    updateSelectizeInput(session, "delete_player_id", selected = "")
  })
  
  ## ---- Navigating the Players Page ----
  # List page navigation
  observeEvent(input$go_home, { 
    app_state$page <- "start" 
  })
  observeEvent(input$add_player, { 
    app_state$page <- "input" 
  })
  observeEvent(input$player_info, { 
    app_state$page <- "update_player" 
  })
  observeEvent(input$import_excel_page, { 
    app_state$page <- "import_page" 
  })
  
  # ---- Render UI Dynamically ----
  output$dynamicUI <- renderUI({
    switch(app_state$page,
           "start" = startPage(),
           "input" = inputPage(current_year),
           "update_player" = updatePlayerPage(),
           "list" = listPage(),
           "stat" = statPage(),
           "import_page" = importPage(),
           "new_event" = newEventPage(),
           "before_practice" = beforePracticePage(),
           "practice_event" = practicePage(),
           "lineup" = lineupPage(),
           "game_event" = gamePage(),
           "team_stats" = teamStatsPage(),
           "see_pitches" = pitchesPage(),
           "settings" = settingsPage(),
           div("Unknown page: ", app_state$page) 
    )
  })
  
  # ---- Extra Navigation Button ----
  # Remove all references to 'start' page
  # Removed duplicate and conflicting navigation for go_home and go_back
  
  
}

shinyApp(ui, server)
