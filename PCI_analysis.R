# Load custom functions & libraries ####
source("R/PCI_functions.r")

# Set up variables ####

# Should we ignore events marked as ambiguous
remove_ambiguous <- TRUE

# Size of gap for merging data
frame_gap <- 2

# Data input and output
raw_data_path <- "data"
proc_data_path <- "."

input_pattern <- ".*.csv"
output_file_name <- "PCIsummary"

# Generate output filename
if (remove_ambiguous) {
  output_file <- sprintf("%s_noAmbg.csv", output_file_name)
} else {
  output_file <- sprintf("%s_inAmbg.csv", output_file_name)
}

data_col_def <- "ddddcdddcdddcdddccdddccdddcccccdddcdddcdddcccccdddcdddcdddcdddcdddcdddcdddcccccdddccccdddccccdddc"

# Variables for data extraction
behav_names <- c("PCIduration", "bookreading","offCameras",
                 "babyATparentface", "parentATbabyface",
                 "babyobj", "parentobj", "parentnoun",
                 "babyATobj", "parentATobj")

partial_matching <- c(T, T, T, T, T, F, F, T, T, T)

# Body of Script ----

files <- dir(path = raw_data_path,
             pattern = input_pattern,
             full.names = TRUE)

for (file_name in files) {

  # Extract partipcipant ID
  PartID <- str_extract(file_name, "(?<=/)[:alnum:]+_[:alnum:]+")

  # Load PCI data
  PCIData <- read_csv(file_name, col_types = data_col_def)

  # Parse behavioural events from raw data
  behav_events <- map2_df(.x = behav_names,
                          .y = partial_matching,
                          raw_data = PCIData,
                          frame_gap = frame_gap,
                          remove_ambig = remove_ambiguous,
                          .f = parse_behav_events) %>%
    mutate(event_ID = row_number(), .before = 1)

}

