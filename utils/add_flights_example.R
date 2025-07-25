library(shiny)

ui <- fluidPage(
  titlePanel("Flight Entry App"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("add_flight", "Add Flight"),
      actionButton("save_flights", "Save All Flights"),
      br(), br(),
      uiOutput("flight_inputs")
    ),
    
    mainPanel(
      h3("Saved Flights"),
      tableOutput("flights_table")
    )
  )
)

server <- function(input, output, session) {
  # Store flight block IDs
  flight_ids <- reactiveVal(c(1))
  # Store saved flights
  saved_flights <- reactiveVal(data.frame())
  # Temporary storage for unsaved inputs
  flight_inputs <- reactiveValues()
  
  # Helper to update reactiveValues on input change
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
  
  # UI Rendering
  output$flight_inputs <- renderUI({
    ids <- flight_ids()
    lapply(ids, function(i) {
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
    })
  })
  
  # Add Flight Button
  observeEvent(input$add_flight, {
    ids <- flight_ids()
    new_id <- ifelse(length(ids) == 0, 1, max(ids) + 1)
    flight_ids(c(ids, new_id))
  })
  
  # Delete specific flight inputs
  observe({
    ids <- flight_ids()
    lapply(ids, function(i) {
      observeEvent(input[[paste0("delete_", i)]], {
        flight_ids(setdiff(flight_ids(), i))
        
        # Clean up reactive values
        flight_inputs[[paste0("flight_num_", i)]] <- NULL
        flight_inputs[[paste0("from_", i)]] <- NULL
        flight_inputs[[paste0("to_", i)]] <- NULL
      }, ignoreInit = TRUE)
    })
  })
  
  # Save all flight inputs into a data frame
  observeEvent(input$save_flights, {
    ids <- flight_ids()
    flight_data <- lapply(ids, function(i) {
      data.frame(
        FlightNumber = input[[paste0("flight_num_", i)]] %||% "",
        From = input[[paste0("from_", i)]] %||% "",
        To = input[[paste0("to_", i)]] %||% "",
        stringsAsFactors = FALSE
      )
    })
    saved_flights(do.call(rbind, flight_data))
  })
  
  # Display saved flights
  output$flights_table <- renderTable({
    saved_flights()
  })
}

# Safe null operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui = ui, server = server)
