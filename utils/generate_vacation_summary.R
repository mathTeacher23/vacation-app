library(readr)
library(dplyr)
library(stringr)

generate_vacation_report <- function(vacation_folder_name) {
  base_path <- file.path("data", vacation_folder_name)
  photo_path <- file.path("photos", vacation_folder_name)
  summary_dir <- file.path(base_path, "summary")
  
  if (!dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE)
  
  entry_files <- list.files(base_path, pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)
  entry_files <- sort(entry_files)
  
  if (length(entry_files) == 0) stop("No entries found.")
  
  # Load flight data
  flight_file <- file.path(base_path, "flights.csv")
  flights <- if (file.exists(flight_file)) read_csv(flight_file, show_col_types = FALSE) else NULL
  
  # Load additional cost data
  cost_file <- file.path(base_path, "costs.csv")
  costs <- if (file.exists(cost_file)) read_csv(cost_file, show_col_types = FALSE) else NULL
  ticket_cost <- if (!is.null(costs) && "TicketCost" %in% names(costs)) costs$TicketCost[1] else 0
  lodging_cost <- if (!is.null(costs) && "LodgingCost" %in% names(costs)) costs$LodgingCost[1] else 0
  
  # Compile entry data
  entries <- bind_rows(lapply(entry_files, read_csv, show_col_types = FALSE)) %>%
    arrange(Date)
  
  total_spent <- sum(entries$TotalSpent, na.rm = TRUE)
  flight_cost <- if (!is.null(flights)) sum(flights$FlightCost, na.rm = TRUE) else 0
  total_budget <- total_spent + flight_cost + ticket_cost + lodging_cost
  
  # Markdown output
  md_path <- file.path(summary_dir, "vacation_summary.md")
  con <- file(md_path, "w")
  
  writeLines(paste0("# 🏖️ Vacation Summary: ", gsub("_", " ", vacation_folder_name)), con)
  writeLines("", con)
  writeLines(paste0("**Trip Dates**: ", entries$Date[1], " to ", entries$Date[nrow(entries)]), con)
  writeLines(paste0("**Total Days**: ", nrow(entries)), con)
  writeLines(paste0("**Total Spent**: $", round(total_budget, 2)), con)
  writeLines("", con)
  
  writeLines("### 💲 Budget Breakdown", con)
  writeLines(paste0("- Daily Spending Total: $", round(total_spent, 2)), con)
  writeLines(paste0("- Flight Cost: $", round(flight_cost, 2)), con)
  writeLines(paste0("- Theme Park Tickets: $", round(ticket_cost, 2)), con)
  writeLines(paste0("- Lodging Cost: $", round(lodging_cost, 2)), con)
  writeLines(paste0("- **Overall Total**: $", round(total_budget, 2)), con)
  writeLines("", con)
  
  if (!is.null(flights)) {
    writeLines("## ✈️ Flights", con)
    for (i in seq_len(nrow(flights))) {
      f <- flights[i, ]
      writeLines(paste0("- **", f$Type, "**: ", f$FlightNumber, " from ", f$From, " to ", f$To,
                        " | Seats: ", f$Seats, " | Cost: $", f$FlightCost), con)
    }
    writeLines("", con)
  }
  
  writeLines("## 🗓️ Daily Highlights", con)
  for (i in seq_len(nrow(entries))) {
    row <- entries[i, ]
    writeLines(paste0("### ", row$Date, " (", row$Weekday, ")"), con)
    writeLines(paste0("- **Lodging**: ", row$Lodging), con)
    writeLines(paste0("- **Plans**: ", row$Plans), con)
    writeLines(paste0("- **Reservations**: ", row$Reservations), con)
    writeLines(paste0("- **Spending**: $", row$TotalSpent), con)
    writeLines("- **Journal Entry**:", con)
    writeLines(paste0("  > ", str_replace_all(row$Journal, "\n", "\n  > ")), con)
    writeLines("", con)
  }
  
  writeLines("## 📸 Photo Folders", con)
  if (dir.exists(photo_path)) {
    days <- list.dirs(photo_path, full.names = FALSE, recursive = FALSE)
    if (length(days)) {
      for (day in days) {
        img_count <- length(list.files(file.path(photo_path, day),
                                       pattern = "\\.(jpg|jpeg|png|gif)$", ignore.case = TRUE))
        writeLines(paste0("- **", day, "**: ", img_count, " photo(s)"), con)
      }
    } else {
      writeLines("- No photos found.", con)
    }
  } else {
    writeLines("- No photo folder found.", con)
  }
  
  close(con)
  
  cat("✅ Summary saved to:\n")
  cat("  - Markdown: ", md_path, "\n")
}
