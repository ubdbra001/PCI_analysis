
# Analysis to include:
event_summary <- TRUE    # Provide summary stats for all event
process_looks <- TRUE    # Process mutual looks inc summaries
process_naming <- TRUE   # Process naming overlaps inc summaries

# Ignore events marked as ambiguous:
remove_ambiguous <- FALSE

# Size of gap for event merging
frame_gap <- 2

# Data input and output
raw_data_path <- "data/raw_data"
proc_data_path <- "." # Not sure what this does?

input_pattern <- ".*.csv"
output_file_name <- "PCIsummary"

data_col_def <- "ddddcdddcdddcdddccdddccdddcccccdddcdddcdddcccccdddcdddcdddcdddcdddcdddcdddcccccdddccccdddccccdddc"

# Variables for data extraction
behav_names <- c(
  "PCIduration",
  "bookreading",
  "offCameras",
  "babyATparentface",
  "parentATbabyface",
  "babyobj",
  "parentobj",
  "parentnoun",
  "babyATobj",
  "parentATobj"
  )

partial_matching <- c(T, T, T, T, T, F, F, T, T, T)