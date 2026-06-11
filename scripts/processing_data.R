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

y_cols          <- c("positive_affect","negative_affect")
x_cols          <- c("outcome")
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
#   X (independent variables): outcome                              (k = 1)
#   Ignored                  : trial, id, door_clicked, time
#
# This dataset contains two participant types:
#   - 939 full participants: positive_affect and negative_affect observed
#       Y = positive_affect, negative_affect   (d = 2, valence dropped)
#       Saved to: VANHASBROECK_2024_full_per_participant/
#
#   - 459 valence-only participants: only valence observed
#       Y = valence                             (d = 1)
#       Saved to: VANHASBROECK_2024_valence_per_participant/
#
# It was indeed much easier to change the processing of the data into two separate
# folders rather then separating them in the estimation.R script
#
# How we detect the participant type:
#   We check whether positive_affect has any non-NA values. If yes -> full.
#   If all positive_affect values are NA -> valence-only.
#
# NAs are kept as-is.
# Data are saved per participant, sorted by trial number.
 
# Settings
input_file         <- file.path("scripts/data", "VANHASBROECK_2024.csv")
output_dir_full    <- file.path("scripts/data", "VANHASBROECK_2024_full_per_participant")
output_dir_valence <- file.path("scripts/data", "VANHASBROECK_2024_valence_per_participant")
 
dir.create(output_dir_full,    recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_valence, recursive = TRUE, showWarnings = FALSE)
 
x_cols           <- "outcome"
sorting_variable <- "trial"
 
# Load data
raw <- read.csv(input_file)
 
# Loop over participants, detect type, and save to the correct folder
participant_ids <- sort(unique(raw$id))
 
n_full    <- 0
n_valence <- 0
 
for (pid in participant_ids) {
 
  participant_data <- raw[raw$id == pid, ]
  participant_data <- participant_data[order(participant_data[[sorting_variable]]), ]
 
  # Detect participant type by checking whether positive_affect has any
  # observed (non-NA) values
  is_full <- any(!is.na(participant_data$positive_affect))
 
  if (is_full) {
    # Full participant: save PA + NA only (d=2), drop valence
    ds <- dataset(
      data             = participant_data,
      y_cols           = c("positive_affect", "negative_affect"),
      x_cols           = x_cols,
      sorting_variable = sorting_variable
    )
    out_file <- file.path(output_dir_full,
                          paste0("VANHASBROECK_2024_", pid, ".rds"))
    n_full <- n_full + 1
 
  } else {
    # Valence-only participant: save valence only (d=1)
    ds <- dataset(
      data             = participant_data,
      y_cols           = "valence",
      x_cols           = x_cols,
      sorting_variable = sorting_variable
    )
    out_file <- file.path(output_dir_valence,
                          paste0("VANHASBROECK_2024_", pid, ".rds"))
    n_valence <- n_valence + 1
  }
 
  saveRDS(ds, file = out_file)

}
 
