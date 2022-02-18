## Import packages ----

library(tidyverse)

## Set up constants ----

remove_ambiguous = F

firstCoderID <- "firstcoding"
secondCoderID <- "secondcoding"

raw_data_path = 'ADDS_Wave2'
proc_data_path = '.'

file_extension <- ".csv"
output_file_name = "PCI_IRR_summary"

behav_names <- c('PCIduration', 'bookreading', 'babyATparentface', 'parentATbabyface', 'offCameras')
out_behav_names <- paste(behav_names, "overlapProp", sep = ".")

frame_len <- 33


# Start by removing functions and placing them in seperate file
# Two options IRR and Training
# IRR: Frame by frame (all frames and just event frames) comparison for two
# files, returns percentage overlap for requested behaviours
# Training: Files compared to exemplar and parts with significant deviations
# highlighted in report (RMarkdown?)


## Define functions ----

# Checks the data to make sure the number of columns in the header and body are consistent, corrects if not
import_data <- function(file_name){

  # Import header
  header_row <- read.csv(file.path(raw_data_path, file_name), header = F, stringsAsFactors = F, nrows = 1)
  # Import sample of data body
  data_body <- read.csv(file.path(raw_data_path, file_name), header = F, stringsAsFactors = F, skip = 1, nrows = 5)

  # Compare the number of columns for differences
  if (ncol(header_row) < ncol(data_body)){
    # If there are import the whole lot but not treating the first row as a header
    full_data <- read.csv(file.path(raw_data_path, file_name), header = F, stringsAsFactors = F)
    # Find the missing header value
    missing_header <- is.na(full_data[1,])
    # Remove the column associated with the missing header
    full_data <- full_data[,!missing_header]
    # Save the corrected data
    write_csv(full_data, path = file.path(raw_data_path, file_name), col_names = FALSE)
  }

  # Load the data in prep for analysis
  correct_data <- read.csv(file.path(raw_data_path, file_name), stringsAsFactors = F)

  return(correct_data)
}

column_compare <- function(behav_name, df1, df2){

  df1.extracted <- extract_vector(df1, behav_name)
  df2.extracted <- extract_vector(df2, behav_name)

  overlap_prop <- vector_overlap(df1.extracted, df2.extracted)

  return(overlap_prop)
}

extract_vector <- function(df.in, behav_name){

  if (remove_ambiguous){
    df.in <- remove_ambi(df.in, behav_name)
  }

  df.in %>%
    select(starts_with(behav_name)) %>%
    select(ends_with("ordinal")) %>%
    pull() ->
    df.extracted

  return(df.extracted)
}

vector_overlap <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  overlap <- mean(same)
  return(overlap)
}

remove_ambi <- function(df.in, behav_name){

  amb.ID <- paste(behav_name, "a", sep = '.')
  ord.ID <- paste(behav_name, "ordinal", sep = '.')

  amb.events <- str_detect(df.in[[amb.ID]], "a")

  df.in[[ord.ID]][amb.events] <- NA

  return(df.in)
}

## Body of script ----

output_df <- NULL

FC_input_pattern = sprintf(".*%s.*%s", firstCoderID, file_extension)
firstCoder.FileList <- dir(path = raw_data_path, pattern = FC_input_pattern, recursive = T)

if (remove_ambiguous){
  output_file = sprintf('%s_noAmbg.csv', output_file_name)
} else {
  output_file = sprintf('%s_inAmbg.csv', output_file_name)
}

for (FC.file in firstCoder.FileList){

  # find paired file
  PartID = str_extract(basename(FC.file), '^[:alnum:]+_[:alnum:]+') # Extract PartID using Regex
  SC_input_pattern <- sprintf(".*%s.*%s.*%s", PartID,  secondCoderID, file_extension)
  SC.file <- dir(path = raw_data_path, pattern = SC_input_pattern, recursive = T)

  # Check to make sure only one file IDed in SC.file (none is bad, more than one is bad)
  if (length(SC.file) != 1){
    warning(sprintf('Found more than one second coded file for participant %s. Skipping.', PartID))
    next
  }

  # Load both files
  FC.df <- import_data(FC.file)
  SC.df <- import_data(SC.file)

  # Times from start and end frames in prep for aligning the files
  FC.start <- FC.df$time[1]
  SC.start <- SC.df$time[1]

  FC.end <- FC.df$time[nrow(FC.df)]
  SC.end <- SC.df$time[nrow(SC.df)]

  # check if the difference in start times is greater than one frame
  if (abs(FC.start - SC.start) > frame_len){
    # If so see which came first
    if (FC.start > SC.start) {
      # Work out how many additional frames to add
      additional_time <- seq(from = FC.start-frame_len, to = SC.start, by = -frame_len)
      # Insert the frames at the beginning
      FC.df <- add_row(FC.df, time = rev(additional_time), .before = 0)
    } else if (SC.start > FC.start){ # Same as above but for when the opposite coder is first
      additional_time <- seq(from = SC.start-frame_len, to = FC.start, by = -frame_len)
      SC.df <- add_row(SC.df, time = rev(additional_time), .before = 0)
    }
  }

  # check if the difference in end times is greater than one frame
  if (abs(FC.end - SC.end) > frame_len){
    # If so see which came last
    if (FC.end < SC.end) {
      # Work out how many additional frames to add
      additional_time <- seq(from = FC.end+frame_len, to = SC.end, by = frame_len)
      # Append the frames at the end
      FC.df <- add_row(FC.df, time = additional_time)
    } else if (SC.end < FC.end){
      additional_time <- seq(from = SC.end+frame_len, to = FC.end, by = frame_len)
      SC.df <- add_row(SC.df, time = additional_time)
    }
  }

  # This might need a warning just in case a column in one df doesn't exist in the other
  behav_names %>%
    map_dfc(column_compare, FC.df, SC.df) %>% # Compare presepcified columns for the two data frames and return proportions in a data frame
    rename_all(~ out_behav_names) %>% # Re-add column names derived from original vector
    add_column(ID = PartID, .before = 0) %>% # Add participant ID
    bind_rows(output_df, .) -> # Bind to output_df
    output_df
}

write_csv(output_df, path = file.path(proc_data_path, output_file))