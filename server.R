server <- function(input, output, session) {
  shiny::addResourcePath("photos", "photos")
  
  # --- Reactive triggers ---
  journal_trigger        <- reactiveVal(0)
  flight_data_trigger    <- reactiveVal(0)
  budget_trigger         <- reactiveVal(0)
  photo_trigger          <- reactiveVal(0)
  current_folder         <- reactiveVal(NULL)
  
  # --- Dynamic flight management ---
  flight_ids <- reactiveVal(c())
  flight_inputs <- reactiveValues()
  
  # Safe null operator
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Helper function to save current flight data
  save_current_flights <- function() {
    req(current_folder())
    ids <- flight_ids()
    if (length(ids) > 0) {
      flight_data <- lapply(ids, function(i) {
        # Only use flight_inputs if input values don't exist yet (UI not rendered)
        # This prevents using stale data from previously deleted flights
        data.frame(
          Type = if (!is.null(input[[paste0("type_", i)]])) input[[paste0("type_", i)]] else flight_inputs[[paste0("type_", i)]] %||% "Departure",
          FlightNumber = if (!is.null(input[[paste0("flight_num_", i)]])) input[[paste0("flight_num_", i)]] else flight_inputs[[paste0("flight_num_", i)]] %||% "",
          From = if (!is.null(input[[paste0("from_", i)]])) input[[paste0("from_", i)]] else flight_inputs[[paste0("from_", i)]] %||% "",
          To = if (!is.null(input[[paste0("to_", i)]])) input[[paste0("to_", i)]] else flight_inputs[[paste0("to_", i)]] %||% "",
          Seats = if (!is.null(input[[paste0("seats_", i)]])) input[[paste0("seats_", i)]] else flight_inputs[[paste0("seats_", i)]] %||% "",
          FlightCost = if (!is.null(input[[paste0("cost_", i)]])) input[[paste0("cost_", i)]] else flight_inputs[[paste0("cost_", i)]] %||% 0,
          stringsAsFactors = FALSE
        )
      })
      flights <- do.call(rbind, flight_data)
      write.csv(flights, file.path(current_folder(), "flights.csv"), row.names = FALSE)
    } else {
      # If no flights, remove existing file
      flight_file <- file.path(current_folder(), "flights.csv")
      if (file.exists(flight_file)) {
        file.remove(flight_file)
      }
    }
    # Trigger table updates
    flight_data_trigger(flight_data_trigger() + 1)
    budget_trigger(budget_trigger() + 1)
  }
  
  # --- Lodging choices + persistence ---
  lodging_list_path <- "utils/lodging_choices.csv"
  if (file.exists(lodging_list_path)) {
    lodging_choices <- read.csv(lodging_list_path, stringsAsFactors = FALSE)$Lodging
  } else {
    lodging_choices <- character(0)
  }
  
  # Initialize selectizeInput on app load after UI is ready
  session$onFlushed(function() {
    updateSelectizeInput(session, "lodging",
                         choices = lodging_choices,
                         selected = NULL,
                         server = TRUE)
  })
  
  # observeEvent({
  #   input$total_ticket_cost
  #   input$total_lodging_cost
  # }, {
  #   budget_trigger(budget_trigger() + 1)
  # })
  
  # --- When the date changes (load existing entry or clear fields) ---
  observeEvent(input$entry_date, {
    req(current_folder(), input$entry_date)
    file_today <- file.path(current_folder(), paste0(format(input$entry_date, "%Y-%m-%d"), ".csv"))
    
    if (file.exists(file_today)) {
      saved <- read.csv(file_today, stringsAsFactors = FALSE)
      
      lodging_val <- saved$Lodging[1]
      if (is.character(lodging_val) && nzchar(lodging_val)) {
        selected_lodging <- unlist(strsplit(lodging_val, ",\\s*"))
      } else {
        selected_lodging <- character(0)
      }
      
      updateSelectizeInput(session, "lodging",
                           choices = lodging_choices,
                           selected = selected_lodging,
                           server = TRUE)
      updateTextInput(session, "plans", value = saved$Plans)
      updateTextInput(session, "reservations", value = saved$Reservations)
      updateTextAreaInput(session, "journal", value = saved$Journal)
      updateNumericInput(session, "total_spent", value = saved$TotalSpent)
    } else {
      # Clear fields if no record exists for selected date
      updateSelectizeInput(session, "lodging",
                           choices = lodging_choices,
                           selected = character(0),
                           server = TRUE
      )
      updateTextInput(session, "plans", value = "")
      updateTextInput(session, "reservations", value = "")
      updateTextAreaInput(session, "journal", value = "")
      updateNumericInput(session, "total_spent", value = NA)
    }
  })
  
  # --- Vacation folder selector logic ---
  get_vacation_folders <- function() {
    fs <- list.dirs("data", full.names = FALSE, recursive = FALSE)
    fs[fs != ""]
  }
  observe({
    updateSelectInput(session, "vacation_folder",
                      choices = get_vacation_folders())
  })
  
  observeEvent(input$create_vacation, {
    req(input$new_vacation_name)
    fname <- gsub("[^A-Za-z0-9_]", "_", input$new_vacation_name)
    fpath <- file.path("data", fname)
    if (!dir.exists(fpath)) dir.create(fpath, recursive = TRUE)
    
    updateSelectInput(session, "vacation_folder",
                      choices = get_vacation_folders(), selected = fname)
    current_folder(fpath)
  })
  
  observeEvent(input$vacation_folder, {
    req(input$vacation_folder)
    current_folder(file.path("data", input$vacation_folder))
    journal_trigger(journal_trigger() + 1)
    flight_data_trigger(flight_data_trigger() + 1)
    budget_trigger(budget_trigger() + 1)
    
    # Load additional costs if available
    cost_file <- file.path(current_folder(), "costs.csv")
    if (file.exists(cost_file)) {
      costs <- read.csv(cost_file)
      updateNumericInput(session, "total_ticket_cost", value = costs$TicketCost[1])
      updateNumericInput(session, "total_lodging_cost", value = costs$LodgingCost[1])
    } else {
      updateNumericInput(session, "total_ticket_cost", value = NULL)
      updateNumericInput(session, "total_lodging_cost", value = NULL)
    }
    
    # Load flight data and populate dynamic inputs
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      flights <- read.csv(flight_file, stringsAsFactors = FALSE)
      if (nrow(flights) > 0) {
        # Set flight IDs based on existing data
        flight_ids(seq_len(nrow(flights)))
        
        # Populate flight inputs with existing data
        for (i in seq_len(nrow(flights))) {
          flight_inputs[[paste0("flight_num_", i)]] <- flights$FlightNumber[i]
          flight_inputs[[paste0("from_", i)]] <- flights$From[i]
          flight_inputs[[paste0("to_", i)]] <- flights$To[i]
          flight_inputs[[paste0("seats_", i)]] <- flights$Seats[i] %||% ""
          flight_inputs[[paste0("cost_", i)]] <- flights$FlightCost[i] %||% 0
          flight_inputs[[paste0("type_", i)]] <- flights$Type[i] %||% ""
        }
      } else {
        # Initialize with empty flight if no data exists
        flight_ids(c(1))
      }
    } else {
      # Initialize with one empty flight if no file exists
      flight_ids(c(1))
    }
    
    # Find and load the first available journal entry
    entry_files <- list.files(current_folder(), pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    
    if (length(entry_files) > 0) {
      first_file <- sort(entry_files)[1]
      saved <- read.csv(first_file, stringsAsFactors = FALSE)
      
      first_date <- as.Date(basename(first_file), format = "%Y-%m-%d.csv")
      
      lodging_val <- saved$Lodging[1]
      if (is.character(lodging_val) && nzchar(lodging_val)) {
        selected_lodging <- unlist(strsplit(lodging_val, ",\\s*"))
      } else {
        selected_lodging <- character(0)
      }
      
      updateDateInput(session, "entry_date", value = first_date)
      updateSelectizeInput(session, "lodging",
                           choices = lodging_choices,
                           selected = selected_lodging,
                           server = TRUE)
      updateTextInput(session, "plans", value = saved$Plans)
      updateTextInput(session, "reservations", value = saved$Reservations)
      updateTextAreaInput(session, "journal", value = saved$Journal)
      updateNumericInput(session, "total_spent", value = saved$TotalSpent)
    }
    
  })
  
  # --- Dynamic Flight UI Rendering ---
  output$flight_inputs <- renderUI({
    ids <- flight_ids()
    if (length(ids) == 0) return(NULL)
    
    lapply(ids, function(i) {
      card(
        class = "highlight-card",
        #style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; margin-bottom: 10px;",
        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            h5(paste("Flight"), style = "margin: 0; color: #495057;"),
            actionButton(paste0("delete_", i), "🗑️ Delete", class = "btn-warning btn-sm", 
                        style = "padding: 2px 8px; font-size: 12px; background-color: #ff4d4d; color: white; border-color: #ff4d4d;")
        ),
        
        layout_column_wrap(
          width = 1/2,
          textInput(paste0("flight_num_", i), "Flight Number",
                    value = flight_inputs[[paste0("flight_num_", i)]] %||% "",
                    placeholder = "e.g. UA 12345"),
          selectInput(paste0("type_", i), "Flight Type",
                     choices = c("Departure" = "Departure", "Return" = "Return", "Connecting" = "Connecting"),
                     selected = flight_inputs[[paste0("type_", i)]] %||% "Departure")
        ),
        
        layout_column_wrap(
          width = 1/2,
          textInput(paste0("from_", i), "From",
                    value = flight_inputs[[paste0("from_", i)]] %||% "",
                    placeholder = "e.g. IAD 3:35 PM"),
          textInput(paste0("to_", i), "To",
                    value = flight_inputs[[paste0("to_", i)]] %||% "",
                    placeholder = "e.g. MCO 6:00 PM")
        ),
        
        layout_column_wrap(
          width = 1/2,
          textInput(paste0("seats_", i), "Seats",
                    value = flight_inputs[[paste0("seats_", i)]] %||% "",
                    placeholder = "e.g. 1A & 1B"),
          numericInputIcon(paste0("cost_", i), "Cost",
                          value = flight_inputs[[paste0("cost_", i)]] %||% 0,
                          min = 0, icon = list(prefix = "$"))
        )
      )
    })
  })
  
  # --- Flight Management Event Handlers ---
  
  # Removed real-time input observers to prevent typing interruption
  # Flight data is now only saved when user clicks "Save Entry"
  

  
  # Add Flight Button
  observeEvent(input$add_flight, {
    ids <- flight_ids()
    
    # FIRST: Preserve current input values before UI re-renders
    for (i in ids) {
      if (!is.null(input[[paste0("type_", i)]])) {
        flight_inputs[[paste0("type_", i)]] <- input[[paste0("type_", i)]]
      }
      if (!is.null(input[[paste0("flight_num_", i)]])) {
        flight_inputs[[paste0("flight_num_", i)]] <- input[[paste0("flight_num_", i)]]
      }
      if (!is.null(input[[paste0("from_", i)]])) {
        flight_inputs[[paste0("from_", i)]] <- input[[paste0("from_", i)]]
      }
      if (!is.null(input[[paste0("to_", i)]])) {
        flight_inputs[[paste0("to_", i)]] <- input[[paste0("to_", i)]]
      }
      if (!is.null(input[[paste0("seats_", i)]])) {
        flight_inputs[[paste0("seats_", i)]] <- input[[paste0("seats_", i)]]
      }
      if (!is.null(input[[paste0("cost_", i)]])) {
        flight_inputs[[paste0("cost_", i)]] <- input[[paste0("cost_", i)]]
      }
    }
    
    # THEN: Add the new flight
    new_id <- ifelse(length(ids) == 0, 1, max(ids) + 1)
    flight_ids(c(ids, new_id))
    
    # Initialize default values for the new flight
    flight_inputs[[paste0("type_", new_id)]] <- "Departure"
    flight_inputs[[paste0("flight_num_", new_id)]] <- ""
    flight_inputs[[paste0("from_", new_id)]] <- ""
    flight_inputs[[paste0("to_", new_id)]] <- ""
    flight_inputs[[paste0("seats_", new_id)]] <- ""
    flight_inputs[[paste0("cost_", new_id)]] <- 0
  })
  
  # Clear All Flights Button
  observeEvent(input$clear_flights, {
    flight_ids(c())
    # Clear all flight inputs
    flight_keys <- names(reactiveValuesToList(flight_inputs))
    flight_keys <- flight_keys[grepl("^(flight_num_|from_|to_|seats_|cost_|type_)", flight_keys)]
    for (key in flight_keys) {
      flight_inputs[[key]] <- NULL
    }
    # Immediately save empty flight data
    req(current_folder())
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      file.remove(flight_file)
    }
    flight_data_trigger(flight_data_trigger() + 1)
    budget_trigger(budget_trigger() + 1)
  })
  
  # Delete specific flight inputs
  observe({
    ids <- flight_ids()
    lapply(ids, function(i) {
      observeEvent(input[[paste0("delete_", i)]], {
        # FIRST: Preserve current input values from all existing flights
        current_ids <- flight_ids()
        for (id in current_ids) {
          if (!is.null(input[[paste0("type_", id)]])) {
            flight_inputs[[paste0("type_", id)]] <- input[[paste0("type_", id)]]
          }
          if (!is.null(input[[paste0("flight_num_", id)]])) {
            flight_inputs[[paste0("flight_num_", id)]] <- input[[paste0("flight_num_", id)]]
          }
          if (!is.null(input[[paste0("from_", id)]])) {
            flight_inputs[[paste0("from_", id)]] <- input[[paste0("from_", id)]]
          }
          if (!is.null(input[[paste0("to_", id)]])) {
            flight_inputs[[paste0("to_", id)]] <- input[[paste0("to_", id)]]
          }
          if (!is.null(input[[paste0("seats_", id)]])) {
            flight_inputs[[paste0("seats_", id)]] <- input[[paste0("seats_", id)]]
          }
          if (!is.null(input[[paste0("cost_", id)]])) {
            flight_inputs[[paste0("cost_", id)]] <- input[[paste0("cost_", id)]]
          }
        }
        
        # SECOND: Simply remove the specific flight ID from the list (no renumbering)
        remaining_ids <- setdiff(current_ids, i)
        flight_ids(remaining_ids)
        
        # THIRD: Only clean up reactive values for the deleted flight
        flight_inputs[[paste0("flight_num_", i)]] <- NULL
        flight_inputs[[paste0("from_", i)]] <- NULL
        flight_inputs[[paste0("to_", i)]] <- NULL
        flight_inputs[[paste0("seats_", i)]] <- NULL
        flight_inputs[[paste0("cost_", i)]] <- NULL
        flight_inputs[[paste0("type_", i)]] <- NULL
      }, ignoreInit = TRUE)
    })
  })
  
  # Don't automatically update tables when flight_ids change
  # Tables will update when user clicks "Save Entry" or when flights are deleted
  
  # --- Save entry block ---
  observeEvent(input$save_entry, {
    req(current_folder(), input$entry_date)
    
    df <- data.frame(
      Date         = as.character(input$entry_date),
      Weekday      = weekdays(input$entry_date),
      Lodging      = paste(input$lodging, collapse = ", "),
      Plans        = input$plans,
      Reservations = input$reservations,
      Journal      = input$journal,
      TotalSpent   = input$total_spent,
      stringsAsFactors = FALSE
    )
    write.csv(df, file.path(current_folder(), paste0(format(input$entry_date, "%Y-%m-%d"), ".csv")), row.names = FALSE)
    
    # Persist new lodging value
    new_lodging <- trimws(input$lodging)  # trim each element
    
    # Find lodging entries that are not already in lodging_choices
    lodging_to_add <- setdiff(new_lodging, lodging_choices)
    
    if (length(lodging_to_add) > 0) {
      lodging_choices <<- sort(c(lodging_choices, lodging_to_add))
      write.csv(data.frame(Lodging = lodging_choices), lodging_list_path, row.names = FALSE)
    }
    
    # Save dynamic flight data
    ids <- flight_ids()
    if (length(ids) > 0) {
      flight_data <- lapply(ids, function(i) {
        data.frame(
          Type = input[[paste0("type_", i)]] %||% "",
          FlightNumber = input[[paste0("flight_num_", i)]] %||% "",
          From = input[[paste0("from_", i)]] %||% "",
          To = input[[paste0("to_", i)]] %||% "",
          Seats = input[[paste0("seats_", i)]] %||% "",
          FlightCost = input[[paste0("cost_", i)]] %||% 0,
          stringsAsFactors = FALSE
        )
      })
      flights <- do.call(rbind, flight_data)
      write.csv(flights, file.path(current_folder(), "flights.csv"), row.names = FALSE)
    }
    
    # Save additional costs to a file in the current vacation folder
    additional_costs <- data.frame(
      TicketCost = ifelse(is.null(input$total_ticket_cost), 0, input$total_ticket_cost),
      LodgingCost = ifelse(is.null(input$total_lodging_cost), 0, input$total_lodging_cost)
    )
    write.csv(additional_costs, file.path(current_folder(), "costs.csv"), row.names = FALSE)
    
    # Create corresponding photos folder
    photo_folder <- file.path("photos", input$vacation_folder, format(input$entry_date, "%Y-%m-%d"))
    if (!dir.exists(photo_folder)) dir.create(photo_folder, recursive = TRUE)
    
    journal_trigger(journal_trigger() + 1)
    flight_data_trigger(flight_data_trigger() + 1)
    budget_trigger(budget_trigger() + 1)
    photo_trigger(photo_trigger() + 1)
    
    tryCatch({
      source("utils/generate_vacation_summary.R")
      folder_name <- basename(current_folder())
      generate_vacation_report(folder_name)
    }, error = function(e) {
      message("⚠️ Could not regenerate markdown summary: ", e$message)
    })
    
    showNotification("✅ Entry saved!", type = "message")
  })
  
  # --- Table renderers ---
  output$journal_table <- renderDT({
    req(current_folder()); journal_trigger()
    files <- list.files(current_folder(), "\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    if (length(files) == 0) return(NULL)
    df <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
    datatable(df[order(df$Date), ], options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$flight_table <- renderDT({
    flight_data_trigger()
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      DT::datatable(read.csv(flight_file, stringsAsFactors = FALSE), options = list(scrollX = TRUE))
    } else {
      DT::datatable(data.frame(Message = "No flight info available"))
    }
  })
  
  output$budget_table <- renderDT({
    budget_trigger(); req(current_folder())
    # Daily total
    files <- list.files(current_folder(), "\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    total_spent <- if (length(files)) sum(sapply(files, function(f) sum(read.csv(f)$TotalSpent, na.rm = TRUE))) else 0
    
    flight_cost <- 0
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file))
      flight_cost <- sum(read.csv(flight_file)$FlightCost, na.rm = TRUE)
    
    # Read saved additional costs
    cost_file <- file.path(current_folder(), "costs.csv")
    if (file.exists(cost_file)) {
      costs <- read.csv(cost_file)
      ticket_cost <- costs$TicketCost[1]
      lodging_cost <- costs$LodgingCost[1]
    } else {
      ticket_cost <- 0
      lodging_cost <- 0
    }
    
    overall_total <- total_spent + flight_cost + ticket_cost + lodging_cost
    
    summary_df <- data.frame(
      Category = c(
        "Total Daily Spending",
        "Total Flight Cost",
        "Theme Park Tickets",
        "Lodging",
        "Overall Total"
      ),
      Amount = c(
        total_spent,
        flight_cost,
        ticket_cost,
        lodging_cost,
        overall_total
      )
    )
    datatable(summary_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  output$photo_gallery <- renderUI({
    photo_trigger(); req(input$vacation_folder)
    photo_base <- file.path("photos", input$vacation_folder)
    
    if (!dir.exists(photo_base)) return(h4("No photos yet…"))
    day_folders <- list.dirs(photo_base, recursive = FALSE, full.names = TRUE)
    if (!length(day_folders)) return(h4("No daily photo folders found."))
    
    # Add custom CSS for hover effect
    custom_css <- tags$style(HTML("
    .photo-thumbnail img:hover {
      transform: scale(1.05);
      transition: transform 0.2s ease-in-out;
      z-index: 10;
    }
  "))
    
    sections <- lapply(day_folders, function(dfolder) {
      date_str <- basename(dfolder)
      
      # Parse weekday
      weekday <- tryCatch({
        format(as.Date(date_str), "%A")
      }, error = function(e) {
        "Invalid date"
      })
      
      header_label <- paste0(date_str, " (", weekday, ")")
      
      imgs <- list.files(dfolder, pattern = "\\.(jpg|jpeg|png|gif|bmp)$", full.names = TRUE, ignore.case = TRUE)
      
      if (!length(imgs)) {
        return(div(
          h4(header_label),
          p(class = "text-muted", "No photos for this day."),
          hr()
        ))
      }
      
      photo_items <- lapply(imgs, function(img_path) {
        rel_path <- file.path("photos", input$vacation_folder, date_str, basename(img_path))
        
        div(class = "col-md-2 col-sm-3 col-4 mb-2",
            div(class = "photo-thumbnail",
                img(src = rel_path,
                    class = "img-fluid thumbnail-img",
                    style = "width: 100%; height: 120px; object-fit: cover; cursor: pointer; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                    onclick = paste0("window.open('", rel_path, "', '_blank')"),
                    title = basename(img_path))
            )
        )
      })
      
      tagList(
        h3(header_label),
        div(class = "row", photo_items),
        hr()
      )
    })
    
    tagList(
      custom_css,
      div(class = "container-fluid", sections)
    )
  })
  
  
  
}
