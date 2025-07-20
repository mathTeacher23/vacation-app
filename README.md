# 🌴 Vacation Planner App

The **Vacation Planner App** is a dynamic R Shiny application for organizing and documenting your vacation—day by day. It allows you to track your itinerary, journal your experiences, record daily expenses, and manage photos, all with a clean, interactive UI.

Whether you're prepping for Disney or relaxing on the beach, this app gives structure to the fun and creates a detailed memory log of your adventures.

---

## ✈️ Key Features

### ✅ Daily Planning

- Track your **lodging**, **daily plans**, and **dinner reservations**.
- Auto-saves individual CSV files per date for modular editing.
- Customize with your own **resorts or hotels**.

### 📔 Journaling & Budgeting

- Add daily journal entries and reflections.
- Track **total daily spending**.
- Summarize memories and costs in one view.

### 🏨 Lodging & Dining Options

- Preloaded with dozens of popular hotels including Disney Resorts.
- Add your own custom lodging in real time.
- Smart reservation generation using Disney dining locations.

### 📸 Photo Management

- Drag-and-drop daily photo folders.
- Auto-renders photo gallery by date.
- Clean **hover zoom effect** and weekday labeling for browsing.

### 📦 Data Management

- Each day gets its own CSV file under `/data/`.
- Photo folders match each day's date under `/photos/`.
- A flight CSV stores your travel details.

---

## 📂 Project Structure

```
.
├── data
│   └── YYYY_MM_Vacation_Test
│       ├── 2025-07-20.csv
│       ├── 2025-07-21.csv
│       ├── 2025-07-22.csv
│       ├── 2025-07-23.csv
│       ├── 2025-07-24.csv
│       ├── 2025-07-25.csv
│       ├── 2025-07-26.csv
│       └── flights.csv
├── global.R
├── models
├── photos
│   └── YYYY_MM_Vacation_Test
│       ├── 2025-07-20
│       │   ├── IMG_1000.jpeg
│       │   ├── IMG_2000.jpeg
│       │   └── IMG_3000.jpeg
│       ├── 2025-07-21
│       │   ├── IMG_1001.jpeg
│       │   ├── IMG_2001.jpeg
│       │   └── IMG_3001.jpeg
│       ├── 2025-07-22
│       │   ├── IMG_1002.jpeg
│       │   ├── IMG_2002.jpeg
│       │   └── IMG_3002.jpeg
│       ├── 2025-07-23
│       │   ├── IMG_1003.jpeg
│       │   ├── IMG_2003.jpeg
│       │   └── IMG_3003.jpeg
│       ├── 2025-07-24
│       │   ├── IMG_1004.jpeg
│       │   ├── IMG_2004.jpeg
│       │   └── IMG_3004.jpeg
│       ├── 2025-07-25
│       │   ├── IMG_1005.jpeg
│       │   ├── IMG_2005.jpeg
│       │   └── IMG_3005.jpeg
│       └── 2025-07-26
│           ├── IMG_1006.jpeg
│           ├── IMG_2006.jpeg
│           └── IMG_3006.jpeg
├── server.R
├── ui.R
├── utils
│   ├── disney.csv
│   ├── generate_dummy_data.R
│   └── lodging_choices.csv
└── www
    └── sample_image.JPG
```

---

## 🛠 Setup & Run

### 1. Load Dependencies

```r
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
```

### 2. Run the app

```r
shiny::runApp()
```
## 🧪 Generate Dummy Data

The app includes a utility script to auto-generate a full sample vacation for testing and demo purposes.

To run it, simply source:

```r
source("utils/generate_dummy_data.R")
```

### ✨ What it Generates:

- **7-day sample vacation** starting from today.
- A **randomly selected lodging** for the entire trip (chosen from `lodging_choices.csv`).
- Day-by-day `.csv` files with:
  - **Date** and **weekday**
  - Selected **lodging**
  - Rotating **plans** (e.g., "Morning at Magic Kingdom")
  - Multiple **dinner reservations** (e.g., `"Boma's @ 6:30 PM, Ohana @ 9:00 PM"`)
  - Realistic **journal entries** sampled from a curated list
  - Randomized **total spent** values (between $50 and $300)

- A `flights.csv` file with:
  - Departure and return flight info
  - Times, seats, costs, and flight numbers

- **Photos** folder with:
  - 3 photos per day, named `IMG_XXXX.jpeg`
  - Copied from a sample placeholder image in `/www/sample_image.JPG`
  - Organized in `/photos/YYYY_MM_Vacation_Test/<date>/`

### 📌 Dependencies

Make sure the following files exist before running:

- `utils/lodging_choices.csv`: List of hotel and resort options
- `utils/disney.csv`: Must contain a column `Class == "Dining"` for restaurant sampling
- `www/sample_image.JPG`: Used to generate placeholder photos
