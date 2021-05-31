
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

extend_event_by_frame <- function(input_data, time_in, frame_shift = 1){

  # Extends timings of an event by a specified number of frames
  #
  # This is for events like offCamera where other behavioral events may lead
  # into them but will not overlap
  #
  # Currently only works symmetrically
  # May also be worht making a temporary shift to avoid inflating length of
  # events in question

  # Ealry return if frame_shift is less than 1 (no negative numbers)
  if (frame_shift < 1) return(input_data)

  # Make sure frame_shift number is an integer
  frame_shift <- as.integer(frame_shift)

  data_len <- length(time_in)

  adjusted_data <- mutate(
    input_data,
    # If frame is not first or last then adjust, otherwise leave as is
    first_frame = if_else(first_frame != 1,
                          first_frame - frame_shift,
                          first_frame),
    last_frame = if_else(last_frame != data_len,
                         last_frame + frame_shift,
                         last_frame),
    # Now adjust onset and offset times accordingly
    onset = time_in[first_frame],
    offset = time_in[last_frame])

  return(adjusted_data)

}

extract_unique_objects <- function(input_df){

  # Extracts all the unique objects from all columns beginning with "obj"
  # Ignores anything not a word (spaces and punctuation)

  # Select only obj cols
  obj_cols <- select(input_df, matches("^obj"))
  # Collapse multiple cols into single col
  long_obj_df <- pivot_longer(obj_cols, cols = contains("obj")) %>%
    # Remove 'blank' cells
    filter(str_detect(value, "[:word:]"))

  # Pull out unique objects
  unique_objs <- unique(long_obj_df$value)

  return(unique_objs)
}

extract_obj_events <- function(object_label, input_df, frame_gap = 2){

  # Extracts onset and offset for manipulation of specific objects from PCI
  # events

  # Filter df so only events with that object are inlcuded
  object_df <- filter(input_df, if_any(starts_with("obj"), ~ . == object_label))

  # Merge proximal events (1-2 frames, ignore label differences)
  object_df <- merge_events(events = object_df, frame_gap = frame_gap)

  # Add object as single variable
  object_df <- select(object_df, -starts_with("obj")) %>%
    mutate(obj = object_label)

  return(object_df)

}

convert_events_to_objs <- function(input_df){

  # Takes a data frame of events tagged with objects and convertis it to a data
  # frame of object events, ie events for each opject manipulated

  # Get a list of unique objects for that participant
  unique_objects <- extract_unique_objects(input_df)

  # Run through list extracting event details for each object
  output_df <- map_df(unique_objects,
                      input_df,
                      .f = extract_obj_events)

  return(output_df)

}

parse_behav_events <- function(){

}