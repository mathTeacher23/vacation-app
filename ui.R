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
                      div(style = "margin-bottom: 15px;",
                          actionBttn("add_flight", "➕ Add Flight", style = "fill", color = "primary", size = "sm"),
                          actionBttn("clear_flights", "🗑️ Clear All", style = "fill", color = "danger", size = "sm")
                      ),
                      uiOutput("flight_inputs")
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