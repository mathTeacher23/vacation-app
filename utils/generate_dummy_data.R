
# Load Disney dining locations
disney_path <- "utils/disney.csv"
if (!file.exists(disney_path)) stop("disney.csv not found!")

d <- read_csv(disney_path, show_col_types = FALSE)
dining_list <- sort(d[d$Class == "Dining", ]$Id)
if (length(dining_list) == 0) stop("No dining entries found in disney.csv")

generate_reservation <- function() {
  n <- sample(1:3, 1)  # number of reservations for the day (1 to 3)
  restaurants <- sample(dining_list, n)
  times <- sample(17:21, n, replace = TRUE)  # hours between 5 PM and 9 PM
  minutes <- sample(c("00", "15", "30", "45"), n, replace = TRUE)
  
  reservations <- mapply(function(r, h, m) {
    time <- sprintf("%d:%s PM", ifelse(h > 12, h - 12, h), m)
    paste0(r, " @ ", time)
  }, restaurants, times, minutes, SIMPLIFY = TRUE)
  
  paste(reservations, collapse = ", ")
}

# Load lodging choices or create default list
lodging_list_path <- "utils/lodging_choices.csv"

# Load lodging choices from CSV
if (file.exists(lodging_list_path)) {
  lodging_choices <- read.csv(lodging_list_path, stringsAsFactors = FALSE)$Lodging
} else {
  stop("lodging_choices.csv not found.")
}


# Paths
vacation_name <- "YYYY_MM_Vacation_Test"
data_dir <- file.path("data", vacation_name)
photos_dir <- file.path("photos", vacation_name)

# Create main folders if not exist
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(photos_dir, recursive = TRUE, showWarnings = FALSE)

# Dates for dummy daily data (7 days)
start_date <- Sys.Date()
days <- 0:6  # Now 7 days

# Sample plan and journal entries (cycled over the 7 days)
sample_plans <- c(
  "Spend the morning at Magic Kingdom and watch the afternoon parade.",
  "Relax by the pool and try out a new resort restaurant for dinner.",
  "Full day at EPCOT, focusing on World Showcase.",
  "Animal Kingdom safari and a late lunch at Yak & Yeti.",
  "Shopping at Disney Springs and a fireworks cruise in the evening.",
  "Hollywood Studios morning with Rise of the Resistance and lunch at Sci-Fi Dine-In.",
  "Character breakfast and mini-golf before heading back to the resort."
)

sample_journals <- c(
  "An amazing start to the trip! We hit all our favorite rides and had churros by the castle.",
  "The pool was relaxing, and dinner at the resort was surprisingly delicious. Very chill day.",
  "EPCOT was hot but incredible – the Flower & Garden Festival displays were stunning!",
  "Saw so many animals on the safari. Everest was wild, and lunch was spicy and great.",
  "Bittersweet end to the trip. Picked up some souvenirs and enjoyed the view from the boat ride.",
  "The Star Wars rides blew our minds! So glad we got Genie+ today.",
  "Met Mickey at Chef Mickey's and had some downtime playing games at the arcade."
)
# Pick one lodging for the whole vacation
lodging_for_vacation <- sample(lodging_choices, 1)
for (d in days) {
  day_date <- start_date + d
  date_str <- format(day_date, "%Y-%m-%d")
  
  # Pick lodging option from lodging_choices, cycling if needed
  lodging_for_day <- lodging_choices[(d %% length(lodging_choices)) + 1]
  
  # Create daily CSV
  daily_df <- data.frame(
    Date = date_str,
    Weekday = weekdays(day_date),
    Lodging = lodging_for_vacation,
    Plans = sample_plans[(d %% length(sample_plans)) + 1],
    Reservations = generate_reservation(),
    Journal = sample_journals[(d %% length(sample_journals)) + 1],
    TotalSpent = round(runif(1, 50, 300), 2),
    stringsAsFactors = FALSE
  )
  
  write.csv(daily_df, file.path(data_dir, paste0(date_str, ".csv")), row.names = FALSE)
  
  # Create matching photo folder for this date
  photo_day_dir <- file.path(photos_dir, date_str)
  dir.create(photo_day_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Path to the sample image
  sample_img <- file.path("www", "sample_image.JPG")  # Adjust this path if needed
  
  # Add dummy photo files by copying the sample image
  for (i in 1:3) {
    target_path <- file.path(photo_day_dir, paste0("IMG_", 1000 * i + d, ".jpeg"))
    file.copy(sample_img, target_path, overwrite = TRUE)
  }
}

# Create flights.csv
flights_df <- data.frame(
  Type = c("Departure", "Return"),
  FlightNumber = c("UA12345", "UA54321"),
  From = c("IAD 3:35 PM", "MCO 5:00 PM"),
  To = c("MCO 6:00 PM", "IAD 8:00 PM"),
  Seats = c("1A & 1B", "2B & 2C"),
  FlightCost = c(0, 1320.00),
  stringsAsFactors = FALSE
)

write.csv(flights_df, file.path(data_dir, "flights.csv"), row.names = FALSE)

# Create costs.csv with additional budget values
additional_costs <- data.frame(
  TicketCost = round(runif(1, 400, 1200), 2),   # total for all theme park tickets
  LodgingCost = round(runif(1, 900, 2400), 2)   # total for all lodging
)
write.csv(additional_costs, file.path(data_dir, "costs.csv"), row.names = FALSE)

cat("✅ Dummy vacation folder and photos created at:\n")
cat(data_dir, "\n")
cat(photos_dir, "\n")
