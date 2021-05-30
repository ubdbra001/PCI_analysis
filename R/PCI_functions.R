
# Load libraries ----
library(tidyverse)

between <- data.table::between

# Event proccessing functions ----

merge_events <- function(events_df, frame_gap, behav_name) {
  # Merges events if gap betwen last and first frames is less than or equal to
  # specified value.

  # Loops backwards through data frame. Checks gap, if less than frame.gap then
  # update preceding row with current row end points and then delete current row

  # Early return if less than 2 events or frame_gap is 0
  if (nrow(events_df) < 2 | frame_gap == 0) return(events_df)

  # Make sure df is not grouped before processing further
  if (is_grouped_df(events_df)) events_df <- ungroup(events_df)

  label_col_exists <- "label" %in% names(events_df)

  # Run through events backwards
  for (row_n in seq(from = nrow(events_df), to = 2)) {

    # Get current row and one before
    current_row <- events_df[row_n, ]
    preceding_row <- events_df[row_n - 1, ]

    vals_to_update <- tibble()

    # If the label column exists in the data frame
    if (label_col_exists) {
      # See if the labels for the two events match
      same_label <- current_row$label == preceding_row$label
    } else {
      # If it doesn't then just set as true
      same_label <- TRUE
    }

    # See if the two events are within range of one another
    if ((current_row$first_frame - preceding_row$last_frame) <= frame_gap &
        same_label) {

      # If the above conditions are met then merge the events by extending the
      # preceding event
      vals_to_update <- tibble(select(preceding_row, ordinal),
                               select(current_row, offset, last_frame))

      events_df <- rows_update(events_df, vals_to_update, by = "ordinal") %>%
        rows_delete(current_row["ordinal"], by = "ordinal")
    }
  }
  return(events_df)
}

format_events <- function(input_df){

  # May remove the ungroup function if it is done earlier in the process
  input_df <- ungroup(input_df) %>%
    # Convert onset and offset times to ms and calculate duration
    mutate(onset = round(onset / 1000, 3),
           offset = round(offset / 1000, 3),
           duration = offset - onset)

  # Remove time column, any columns containing "frame" and the "y" column if
  # it exists
  output_df <- select(input_df, -time, -contains("frame"), -matches("^y$"))

  return(output_df)
}

extract_behavs <- function(input_data, behav_name) {
  # Extract columns containing behaviour of interest and prepare them
  # for further analysis

  pattern = paste0("^", behav_name)

  # Drop any column that doesn't contain behav_name
  output_data <- select(input_data, time, matches(pattern)) %>%
    # Remove behav_name and eveything up to period from column names
    # leaving just suffixes
    rename_all(str_remove,  paste0(pattern, ".*\\."))

  return(output_data)
}

