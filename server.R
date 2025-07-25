server <- function(input, output, session) {
  shiny::addResourcePath("photos", "photos")
  
  # --- Reactive triggers ---
  journal_trigger        <- reactiveVal(0)
  flight_data_trigger    <- reactiveVal(0)
  budget_trigger         <- reactiveVal(0)
  photo_trigger          <- reactiveVal(0)
  current_folder         <- reactiveVal(NULL)
  
  # --- Lodging choices + persistence ---
  lodging_list_path <- "utils/lodging_choices.csv"
  if (file.exists(lodging_list_path)) {
    lodging_choices <- read.csv(lodging_list_path, stringsAsFactors = FALSE)$Lodging
  } else {
    lodging_choices <- character(0)
  }
  
  session$onFlushed(function() {
    updateSelectizeInput(session, "lodging",
                         choices = lodging_choices,
                         selected = NULL,
                         server = TRUE)
  })
  
  # --- Flight inputs/reactiveValues setup ---
  flight_ids <- reactiveVal(c(1))  
  flight_inputs <- reactiveValues()
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # --- Load vacation folder & associated data ---
  observeEvent(input$vacation_folder, {
    req(input$vacation_folder)
    current_folder(file.path("data", input$vacation_folder))
    
    # Increment triggers
    journal_trigger(journal_trigger() + 1)
    flight_data_trigger(flight_data_trigger() + 1)
    budget_trigger(budget_trigger() + 1)
    
    # Load additional costs
    cost_file <- file.path(current_folder(), "costs.csv")
    if (file.exists(cost_file)) {
      costs <- read.csv(cost_file)
      updateNumericInput(session, "total_ticket_cost", value = costs$TicketCost[1])
      updateNumericInput(session, "total_lodging_cost", value = costs$LodgingCost[1])
    } else {
      updateNumericInput(session, "total_ticket_cost", value = NULL)
      updateNumericInput(session, "total_lodging_cost", value = NULL)
    }
    
    # Load flights CSV and populate reactive values
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      flights <- read.csv(flight_file, stringsAsFactors = FALSE)
      ids <- seq_len(nrow(flights))
      flight_ids(ids)
      for (i in ids) {
        flight_inputs[[paste0("flight_num_", i)]] <- flights$FlightNumber[i]
        flight_inputs[[paste0("from_", i)]]       <- flights$From[i]
        flight_inputs[[paste0("to_", i)]]         <- flights$To[i]
      }
    } else {
      flight_ids(c(1))
    }
    
    # Load first journal entry if exists
    entry_files <- list.files(current_folder(), pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
    if (length(entry_files) > 0) {
      first_file <- sort(entry_files)[1]
      saved <- read.csv(first_file, stringsAsFactors = FALSE)
      first_date <- as.Date(basename(first_file), format = "%Y-%m-%d.csv")
      
      lodging_val <- saved$Lodging[1]
      selected_lodging <- if (is.character(lodging_val) && nzchar(lodging_val)) {
        unlist(strsplit(lodging_val, ",\\s*"))
      } else character(0)
      
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
  
  # --- Observe flight inputs changes and store to reactiveValues ---
  observe({
    lapply(flight_ids(), function(i) {
      observeEvent(input[[paste0("flight_num_", i)]], {
        flight_inputs[[paste0("flight_num_", i)]] <- input[[paste0("flight_num_", i)]]
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("from_", i)]], {
        flight_inputs[[paste0("from_", i)]] <- input[[paste0("from_", i)]]
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("to_", i)]], {
        flight_inputs[[paste0("to_", i)]] <- input[[paste0("to_", i)]]
      }, ignoreInit = TRUE)
    })
  })
  
  # --- Render dynamic flight inputs UI ---
  output$flight_inputs <- renderUI({
    ids <- flight_ids()
    if (length(ids) == 0) return(NULL)
    tagList(lapply(ids, function(i) {
      wellPanel(
        h4(paste("Flight", i)),
        textInput(paste0("flight_num_", i), "Flight Number",
                  value = flight_inputs[[paste0("flight_num_", i)]] %||% ""),
        textInput(paste0("from_", i), "From",
                  value = flight_inputs[[paste0("from_", i)]] %||% ""),
        textInput(paste0("to_", i), "To",
                  value = flight_inputs[[paste0("to_", i)]] %||% ""),
        actionButton(paste0("delete_", i), "Delete", class = "btn-danger btn-sm")
      )
    }))
  })
  
  # --- Add flight button ---
  observeEvent(input$add_flight, {
    ids <- flight_ids()
    new_id <- ifelse(length(ids) == 0, 1, max(ids) + 1)
    flight_ids(c(ids, new_id))
  })
  
  # --- Delete flights ---
  observe({
    lapply(flight_ids(), function(i) {
      observeEvent(input[[paste0("delete_", i)]], {
        flight_ids(setdiff(flight_ids(), i))
        flight_inputs[[paste0("flight_num_", i)]] <- NULL
        flight_inputs[[paste0("from_", i)]] <- NULL
        flight_inputs[[paste0("to_", i)]] <- NULL
      }, ignoreInit = TRUE)
    })
  })
  
  # --- Save entry button: save flights and other info ---
  observeEvent(input$save_entry, {
    ids <- flight_ids()
    if (length(ids) > 0) {
      flights <- lapply(ids, function(i) {
        data.frame(
          FlightNumber = input[[paste0("flight_num_", i)]] %||% "",
          From         = input[[paste0("from_", i)]] %||% "",
          To           = input[[paste0("to_", i)]] %||% "",
          stringsAsFactors = FALSE
        )
      })
      flights_df <- do.call(rbind, flights)
      if (!is.null(current_folder())) {
        write.csv(flights_df, file.path(current_folder(), "flights.csv"), row.names = FALSE)
      }
    }
    flight_data_trigger(flight_data_trigger() + 1)
    
    # TODO: add saving for journal, costs, daily entry, etc. here or separate observers
  })
  
  # --- Render flights table ---
  output$flight_table <- DT::renderDT({
    flight_data_trigger()
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      DT::datatable(read.csv(flight_file, stringsAsFactors = FALSE), options = list(scrollX = TRUE))
    } else {
      DT::datatable(data.frame(Message = "No flight info available"))
    }
  })
  
  # --- Other server logic (journal, photos, budget, etc.) ---
  # Add as needed here, following similar reactive patterns
  
}
