# Simple test to verify dynamic flight functionality
cat("🧪 Testing Dynamic Flight Integration\n")

# Test 1: Check if flight data structure is correct
test_flight_file <- "data/YYYY_MM_Vacation_Test/flights.csv"
if (file.exists(test_flight_file)) {
  flights <- read.csv(test_flight_file, stringsAsFactors = FALSE)
  cat("✅ Flight file exists with", nrow(flights), "flights\n")
  cat("📋 Columns:", paste(names(flights), collapse = ", "), "\n")
  
  # Test budget calculation
  total_cost <- sum(flights$FlightCost, na.rm = TRUE)
  cat("💰 Total flight cost:", total_cost, "\n")
} else {
  cat("❌ Flight file not found\n")
}

# Test 2: Verify UI components exist
ui_content <- readLines("ui.R")
has_add_button <- any(grepl("add_flight", ui_content))
has_clear_button <- any(grepl("clear_flights", ui_content))
has_dynamic_ui <- any(grepl("flight_inputs", ui_content))

cat("🎛️  UI Components:\n")
cat("  - Add Flight Button:", ifelse(has_add_button, "✅", "❌"), "\n")
cat("  - Clear Flights Button:", ifelse(has_clear_button, "✅", "❌"), "\n")
cat("  - Dynamic Flight UI:", ifelse(has_dynamic_ui, "✅", "❌"), "\n")

# Test 3: Verify server logic
server_content <- readLines("server.R")
has_flight_ids <- any(grepl("flight_ids", server_content))
has_dynamic_save <- any(grepl("flight_data.*lapply", server_content))

cat("⚙️  Server Logic:\n")
cat("  - Flight ID Management:", ifelse(has_flight_ids, "✅", "❌"), "\n")
cat("  - Dynamic Save Logic:", ifelse(has_dynamic_save, "✅", "❌"), "\n")

cat("\n🎉 Dynamic Flight Integration Test Complete!\n")