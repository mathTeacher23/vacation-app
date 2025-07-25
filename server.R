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
    
    # Load flight data into input fields if available
    flight_file <- file.path(current_folder(), "flights.csv")
    if (file.exists(flight_file)) {
      flights <- read.csv(flight_file, stringsAsFactors = FALSE)
      if (nrow(flights) >= 2) {
        updateTextInput(session, "dep_flight_number", value = flights$FlightNumber[1])
        updateTextInput(session, "dep_from",          value = flights$From[1])
        updateTextInput(session, "dep_to",            value = flights$To[1])
        updateTextInput(session, "dep_seats",         value = flights$Seats[1])
        
        updateTextInput(session, "ret_flight_number", value = flights$FlightNumber[2])
        updateTextInput(session, "ret_from",          value = flights$From[2])
        updateTextInput(session, "ret_to",            value = flights$To[2])
        updateTextInput(session, "ret_seats",         value = flights$Seats[2])
        updateNumericInput(session, "flight_cost",    value = flights$FlightCost[2])
      }
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
    
    flights <- data.frame(
      Type         = c("Departure", "Return"),
      FlightNumber = c(input$dep_flight_number, input$ret_flight_number),
      From         = c(input$dep_from, input$ret_from),
      To           = c(input$dep_to, input$ret_to),
      Seats        = c(input$dep_seats, input$ret_seats),
      FlightCost   = c(0, input$flight_cost),
      stringsAsFactors = FALSE
    )
    write.csv(flights, file.path(current_folder(), "flights.csv"), row.names = FALSE)
    
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
