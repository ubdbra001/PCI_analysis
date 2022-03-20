library(tidyverse)

source("PCI_uservars.R")
source("R/IRR_functions.R")


# Load file specifies each participant ID and the columns to compare
IRR_master <- read_csv(file.path(data_dir, "IRR_template.csv"))

# Get list of behavs from header row of file
IRR_behav_vec <- colnames(IRR_master)[-1]

# Create empty output dataframe
IRR_summary <- IRR_master
IRR_summary[, IRR_behav_vec] <- NA_real_
IRR_summary$diff_file_len <- F


# For each ID
for (row_n in seq_len( nrow(IRR_master) ) ){

  # Pull out info from row
  part_info <- slice(IRR_master, row_n)

  # Participant ID
  part_ID <- part_info$ID

  # Columns to assess
  col_include <- as.logical(part_info[-1])
  behav_interest <- IRR_behav_vec[col_include]

  message(sprintf("\nStarting %s \n", part_ID))

  # Generate patten to search dirs for
  file_pattern <- sprintf(pattern_template, part_ID)

  # See if there are corresponding files in coder1 and coder2 dirs
  c1_files <- file_search(coder1_dir, file_pattern)
  c2_files <- file_search(coder2_dir, file_pattern)

  # Skip if either file is not present or there are multiples
  if (c1_files$single_file & c1_files$single_file){
    c1_data <- load_file(c1_files$path, data_col_def, delim = file_delim, filter_col = data_filt)
    c2_data <- load_file(c2_files$path, data_col_def, delim = file_delim, filter_col = data_filt)
  } else {
    message(sprintf(skip_mess, part_ID))
    next
  }

  # Only need for unequal dataframe lengths
  row_diff <- nrow(c1_data) - nrow(c2_data)

  # If there is a difference make a not and then shorten the longer data frame
  if (row_diff != 0) {
    IRR_summary <- rows_update(IRR_summary, tibble(ID = part_ID, diff_file_len = T), by = "ID")

    if (row_diff > 0) {
      c1_data <- c1_data[1:nrow(c2_data), ]
    } else {
      c2_data <- c2_data[1:nrow(c1_data), ]
    }
  }

  # Compare listed variables on a frame by frame basis and return vectors of overlap
  overlaps <- map_dfc(behav_interest, column_compare, c1_data, c2_data) %>%
    rename_all(~ behav_interest)

  # Compute proportion of overlap and add to summary
  IRR_summary <- summarise_all(overlaps, mean) %>%
    add_column(ID = part_ID, .before = 1) %>%
    rows_update(IRR_summary, ., by = 'ID')

  # Use the rle and find_differences to get a dataframe with all overlaps
  diff_locations_raw <- map(overlaps, rle) %>%
    map(find_differences, min_diff_length = min_diff_length) %>%
    stack()

  # Tidy up dataframe by renaming auto generated colnames, adding time in S
  # and adding participant ID
  diff_locations_tidy <- rename(diff_locations_raw,
                                col_name = ind,
                                row_number = values) %>%
    mutate(time_s = c1_data$time[row_number]/1000) %>%
    relocate(row_number, .after = time_s) %>%
    add_column(ID = part_ID, .before = 1)

  # Save diff locations into a file
  write_csv(diff_locations_tidy, file.path(diff_dir, paste0(part_ID, ".csv")))

}

# Save IRR summary
write_csv(IRR_summary, file.path(data_dir, "IRR_summary.csv"))

