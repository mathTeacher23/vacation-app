# DISNEY 3.0
# global.R
# Load packages here too if not handled by app.R
library(bslib)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet)
library(stringr)
library(readr)

#run_test <- FALSE  # Toggle this to TRUE to generate dummy data, FALSE to clean it up
run_test <- FALSE
dummy_folder_name <- "YYYY_MM_Vacation_Test"
dummy_data_path <- file.path("data", dummy_folder_name)
dummy_photos_path <- file.path("photos", dummy_folder_name)

if (run_test) {
  message("⚙️ Generating dummy data...")
  source("utils/generate_dummy_data.R")
} else {
  message("🧹 Cleaning up previous dummy data...")
  
  if (dir.exists(dummy_data_path)) {
    unlink(dummy_data_path, recursive = TRUE, force = TRUE)
    message("🗑️ Removed dummy data folder: ", dummy_data_path)
  } else {
    message("✅ No dummy data folder to remove.")
  }
  
  if (dir.exists(dummy_photos_path)) {
    unlink(dummy_photos_path, recursive = TRUE, force = TRUE)
    message("🗑️ Removed dummy photos folder: ", dummy_photos_path)
  } else {
    message("✅ No dummy photo folder to remove.")
  }
}

# Ensure data directory exists
if (!dir.exists("data")) dir.create("data")

# Helper to list vacation folders
list_vacation_folders <- function() {
  folders <- list.dirs("data", full.names = FALSE, recursive = FALSE)
  folders[folders != ""]
}

# Helper to load all CSVs from a folder
load_vacation_data <- function(folder) {
  folder_path <- file.path("data", folder)
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(data.frame())
  do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
}

lodging_list_path <- "utils/lodging_choices.csv"

if (file.exists(lodging_list_path)) {
  lodging_choices <- read.csv(lodging_list_path, stringsAsFactors = FALSE)$Lodging
  lodging_choices <- sort(unique(lodging_choices))
} else {
  lodging_choices <- lodging_choices <- c("Hilton", "Marriott", "Hyatt", "Hampton Inn", "Double Tree", "Airbnb",
                                          "All Star Movies Resort", "All Star Music Resort", "All Star Sports Resort", "Animal Kingdom Lodge Resort",
                                          "Art of Animation Resort", "Beach Club Resort", "Boardwalk Inn Resort", "Caribbean Beach Resort",
                                          "Contemporary Resort", "Coronado Springs Resort", "Dolphin Resort", "Grand Floridian Resort",
                                          "Old Key West Resort", "Polynesian Resort", "Pop Century Resort", "Port Orleans French Quarter Resort",
                                          "Port Orleans Riverside Resort", "Riviera Resort", "Saratoga Springs Resort", "Swan Resort",
                                          "Wilderness Lodge Resort", "Yacht Club Resort"
                                        )
  
  dir.create("utils", showWarnings = FALSE)
  write.csv(data.frame(Lodging = lodging_choices), lodging_list_path, row.names = FALSE)
}
