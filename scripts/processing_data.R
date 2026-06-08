################################################################################
# PURPOSE:
#
# Change the data-files for the different datasets to fit the required format 
# for the estimation to be performed. Included datasets are currently:
#   - VANHASBROECK_2021
#   - VANHASBROECK_2022
#   - VANHASBROECK_2024
#
################################################################################

devtools::load_all()



################################################################################
# VANHASBROECK_2021
#
# Variable roles (based on Rutledge et al., 2014 / Vanhasbroeck et al., 2021):
#   Y (dependent variable)   : happiness
#   X (independent variables): cr, ev, rpe
#   Ignored                  : trial, id, outcome
#
# NAs are kept as-is (as requested).
# Data are saved per participant, sorted by trial number.
# Output files are named: VANHASBROECK_2021_<id>.rds

# Settings
input_file  <- file.path("scripts/data", "VANHASBROECK_2021.csv")
output_dir  <- file.path("scripts/data", "VANHASBROECK_2021_per_participant")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

y_cols          <- "happiness"
x_cols          <- c("cr", "ev", "rpe")
sorting_variable <- "trial"

# Load data
raw <- read.csv(input_file)

# Loop over participants and create dataset objects
participant_ids <- sort(unique(raw$id))

for (pid in participant_ids) {
  
  participant_data <- raw[raw$id == pid, ]
  participant_data <- participant_data[order(participant_data[[sorting_variable]]), ]
  
  ds <- dataset(
    data             = participant_data,
    y_cols           = y_cols,
    x_cols           = x_cols,
    sorting_variable = sorting_variable
  )
  
  out_file <- file.path(output_dir, paste0("VANHASBROECK_2021_", pid, ".rds"))
  saveRDS(ds, file = out_file)
  
}



################################################################################
# VANHASBROECK_2022
#
# Variable roles (based on Vanhasbroeck et al., 2022):
#   Y (dependent variable)   : possitive_affect, negative_affect, valence
#   X (independent variables): outcome, total
#   Ignored                  : trial, id, door_clicked, time
#
# NAs are kept as-is (as requested).
# Data are saved per participant, sorted by trial number.
# Output files are named: VANHASBROECK_2022_<id>.rds

# Settings
input_file  <- file.path("scripts/data", "VANHASBROECK_2022.csv")
output_dir  <- file.path("scripts/data", "VANHASBROECK_2022_per_participant")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

y_cols          <- c("positive_affect","negative_affect","valence")
x_cols          <- c("outcome", "total")
sorting_variable <- "trial"

# Load data
raw <- read.csv(input_file)

# Loop over participants and create dataset objects
participant_ids <- sort(unique(raw$id))

for (pid in participant_ids) {
  
  participant_data <- raw[raw$id == pid, ]
  participant_data <- participant_data[order(participant_data[[sorting_variable]]), ]
  
  ds <- dataset(
    data             = participant_data,
    y_cols           = y_cols,
    x_cols           = x_cols,
    sorting_variable = sorting_variable
  )
  
  out_file <- file.path(output_dir, paste0("VANHASBROECK_2022_", pid, ".rds"))
  saveRDS(ds, file = out_file)
  
}



################################################################################
# VANHASBROECK_2024
#
# Variable roles (based on Vanhasbroeck et al., 2024):
#   Y (dependent variable)   : possitive_affect, negative_affect, valence
#   X (independent variables): outcome
#   Ignored                  : trial, id, door_clicked, time
#
# NAs are kept as-is (as requested).
# Data are saved per participant, sorted by trial number.
# Output files are named: VANHASBROECK_2024_<id>.rds
################################################################################

# Settings
input_file  <- file.path("scripts/data", "VANHASBROECK_2024.csv")
output_dir  <- file.path("scripts/data", "VANHASBROECK_2024_per_participant")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

y_cols          <- c("positive_affect","negative_affect","valence")
x_cols          <- "outcome"
sorting_variable <- "trial"

# Load data
raw <- read.csv(input_file)

# Loop over participants and create dataset objects
participant_ids <- sort(unique(raw$id))

for (pid in participant_ids) {
  
  participant_data <- raw[raw$id == pid, ]
  participant_data <- participant_data[order(participant_data[[sorting_variable]]), ]
  
  ds <- dataset(
    data             = participant_data,
    y_cols           = y_cols,
    x_cols           = x_cols,
    sorting_variable = sorting_variable
  )
  
  out_file <- file.path(output_dir, paste0("VANHASBROECK_2024_", pid, ".rds"))
  saveRDS(ds, file = out_file)
  
}
