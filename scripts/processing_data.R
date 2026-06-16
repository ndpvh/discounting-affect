################################################################################
# PURPOSE:
#
# Change the data-files for the different datasets to fit the required format 
# for the estimation to be performed. Included datasets are currently:
#   - VANHASBROECK_2021
#   - VANHASBROECK_2022
#   - VANHASBROECK_2024
#   - NIEMEIJER_2022
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

y_cols           <- "happiness"
x_cols           <- c("cr", "ev", "rpe")
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
#   Y (dependent variable)   : possitive_affect, negative_affect
#   X (independent variables): outcome
#   Ignored                  : trial, id, total, door_clicked, time
#
# NAs are kept as-is (as requested).
# Data are saved per participant, sorted by trial number.
# Output files are named: VANHASBROECK_2022_<id>.rds

# Settings
input_file  <- file.path("scripts/data", "VANHASBROECK_2022.csv")
output_dir  <- file.path("scripts/data", "VANHASBROECK_2022_per_participant")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

y_cols           <- c("positive_affect","negative_affect")
x_cols           <- c("outcome")
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
#
#   - 459 valence-only participants: only valence observed
#       Y = valence                             (d = 1)
#
# How we detect the participant type:
#   We check whether positive_affect has any non-NA values. If yes, then the 2D
#   variant is used. If all positive_affect values are NA, then the 1D variant 
#   is used. 
#
# NAs are kept as-is.
# Data are saved per participant, sorted by trial number.
 
# Settings
input_file <- file.path("scripts/data", "VANHASBROECK_2024.csv")
output_dir <- file.path("scripts/data", "VANHASBROECK_2024_per_participant")
 
dir.create(output_dir,    recursive = TRUE, showWarnings = FALSE)
 
x_cols           <- "outcome"
sorting_variable <- "trial"
 
# Load data
raw <- read.csv(input_file)
 
# Loop over participants, detect type, and save to the correct folder
participant_ids <- sort(unique(raw$id))
 
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
 
  } else {
    # Valence-only participant: save valence only (d=1)
    ds <- dataset(
      data             = participant_data,
      y_cols           = "valence",
      x_cols           = x_cols,
      sorting_variable = sorting_variable
    )
  }

  out_file <- file.path(
    output_dir,
    paste0("VANHASBROECK_2024_", pid, ".rds")
  ) 
  saveRDS(ds, file = out_file)
}
 


################################################################################
# NIEMEIJER_2022
#
# Variable roles:
#   Y (dependent variables):   positive_affect, negative_affect     (d = 2)
#   X (independent variables): context_pos, context_neg             (k = 2)
#   Ignored                  : study_day, participant, beep_nr, positive, negative
#
# We perform a special trick here: We keep NAs in both the dependent and 
# independent variables (10 beeps a day) and add an additional NA at the end of 
# the day (beep 11). This will trigger a restart of the discounting under the 
# hood, conforming to how autoregressive models are usually estimated within 
# these data and furthermore conforming to efforts taken in a related package 
# (impulseR)
#
# NAs are kept as-is.
# Data are saved per participant, sorted by study_day and beep_nr.

# Settings
input_file <- file.path("scripts/data", "NIEMEIJER_2022.csv")
output_dir <- file.path("scripts/data", "NIEMEIJER_2022_per_participant")
 
dir.create(output_dir,    recursive = TRUE, showWarnings = FALSE)
 
y_cols           <- c("positive_affect", "negative_affect")
x_cols           <- c("context_pos", "context_neg")
sorting_variable <- "sorting_variable"
 
# Load data
raw <- read.csv(input_file)
 
# Loop over participants, detect type, and save to the correct folder
participant_ids <- sort(unique(raw$participant))
 
for (pid in participant_ids) {
 
  participant_data <- raw[raw$participant == pid, ]
  participant_data <- participant_data[order(participant_data[[sorting_variable]]), ]
 
  # Full participant: save PA + NA only (d=2), drop valence
  ds <- dataset(
    data             = participant_data,
    y_cols           = y_cols,
    x_cols           = x_cols,
    sorting_variable = sorting_variable
  )

  out_file <- file.path(
    output_dir,
    paste0("NIEMEIJER_2022_", pid, ".rds")
  ) 
  saveRDS(ds, file = out_file)
}


