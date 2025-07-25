ui <- page_sidebar(
  title = "🎢 Vacation Planner",
  theme = bs_theme(bootswatch = "sandstone"),
  useShinyjs(),
  
  sidebar = sidebar(
    position = "left",
    width = 400,
    open = TRUE,
    tags$style(HTML("
  .highlight-card {
    background-color: #f5f5f5 !important;
    border: none !important;
    border-radius: 8px !important;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2) !important;
    overflow: hidden !important;
  }

  .highlight-card .card-header {
    background-color: #f5f5f5 !important;
    border-bottom: none !important;
    font-weight: bold;
    font-size: 1.1rem;
    padding: 15px 20px;
    color: #333;
  }

  .highlight-card .card-body {
    background-color: #f5f5f5 !important;
    border: none !important;
    padding: 20px !important;
    margin: 0 !important;
    box-shadow: none !important;
  }

  .highlight-card:hover {
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.3) !important;
  }
")),
    
    
    
    # Accordion starts here
    accordion(
      open = FALSE,
      accordion_panel("📁 Vacation Selection",
                      div(class = "highlight-card",
                        card(
                          selectizeInput(
                            "vacation_folder",
                            "Select Vacation:",
                            choices = NULL,
                            options = list(placeholder = "Select existing or create new")
                          ),
                          textInput("new_vacation_name", "New Vacation", placeholder = "e.g. 2025_08_Disney"),
                          actionBttn("create_vacation", "📁 Create Vacation Folder", style = "fill", color = "primary")
                        )
                      )
      ),
      accordion_panel("✈️ Flight Info",
                      div(style = "padding: 0.25rem 0; margin-bottom: 10px;",
                          numericInputIcon(
                            inputId = "flight_cost",
                            label = "Flight Cost",
                            value = NULL,
                            min = 0,
                            icon = list(prefix = "$")
                          )
                      ),
                      
                      layout_column_wrap(
                        width = 1,
                        card(
                          class = "highlight-card",
                          title = "🛫 Departure Flight",
                          textInput("dep_flight_number", "Flight Number", placeholder = "e.g. UA 12345"),
                          textInput("dep_from", "From", placeholder = "e.g. IAD 3:35 PM"),
                          textInput("dep_to", "To", placeholder = "e.g. MCO 6:00 PM"),
                          textInput("dep_seats", "Seats", placeholder = "e.g. 1A & 1B")
                        ),
                        card(
                          class = "highlight-card",
                          title = "🛬 Return Flight",
                          textInput("ret_flight_number", "Flight Number", placeholder = "e.g. MCO 45678"),
                          textInput("ret_from", "From", placeholder = "e.g. MCO 5:00 PM"),
                          textInput("ret_to", "To", placeholder = "e.g. IAD 8:00 PM"),
                          textInput("ret_seats", "Seats", placeholder = "e.g. 2B & 2C")
                        )
                      )
      ),
      accordion_panel("️💲Additional Costs",
                      card(
                        class = "highlight-card",
                        numericInputIcon(
                          inputId = "total_ticket_cost",
                          label = "Total Theme Park Tickets Cost",
                          value = NULL,
                          min = 0,
                          icon = list(prefix = "$")
                        ),
                        numericInputIcon(
                          inputId = "total_lodging_cost",
                          label = "Total Lodging Cost",
                          value = NULL,
                          min = 0,
                          icon = list(prefix = "$")
                        )
                      )
      
    ),
      accordion_panel("🗓️ Daily Entry",
                      card(
                        class = "highlight-card",
                        dateInput("entry_date", "Date", value = Sys.Date()),
                        selectizeInput("lodging", "Lodging", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
                        textInput("plans", "Plans"),
                        textInput("reservations", "Reservations"),
                        textAreaInput("journal", "Journal", height = "80px"),
                        numericInputIcon(
                          inputId = "total_spent",
                          label = "Total Spent",
                          value = NULL,
                          min = 0,
                          icon = list(prefix = "$")
                        )
                      )
      )
    ),
    actionBttn("save_entry", "💾 Save Entry", style = "fill", color = "success")
  ),
  
  layout_column_wrap(
    width = 1,
    card(
      full_screen = TRUE,
      navset_tab(
        nav_panel("📋 Vacation Journal", DTOutput("journal_table")),
        nav_panel("🛩️ Flight Details", DTOutput("flight_table")),
        nav_panel("💰 Budget Summary", DTOutput("budget_table")),
        nav_panel("📸 Photos", uiOutput("photo_gallery"))
      )
    )
  )
)