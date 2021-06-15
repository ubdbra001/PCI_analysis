
# Load libraries ----
library(dplyr)
library(tidyverse)

between <- data.table::between

# Event proccessing functions ----

merge_events <- function(events_df, frame_gap, behav_name = NULL) {
  # Merges events if gap betwen last and first frames is less than or equal to
  # specified value.

  # Loops backwards through data frame. Checks gap, if less than frame.gap then
  # update preceding row with current row end points and then delete current row

  # Early return if less than 2 events or frame_gap is 0
  if (nrow(events_df) < 2 | frame_gap == 0) return(events_df)

  # Make sure df is not grouped before processing further
  if (is_grouped_df(events_df)) events_df <- ungroup(events_df)

  if (is.null(behav_name)) {
    col_select <- NULL
  } else if (str_detect(behav_name, "AT(parent|baby)")) {
    col_select <- NULL
  } else if (str_detect(behav_name, "(parent|baby)obj")) {
    col_select <- "obj"
  } else if (str_detect(behav_name, "ATobj")) {
    col_names <- names(events_df)
    cols_inc_na <- str_extract(col_names, "referent.*")
    col_select <- discard(cols_inc_na, is.na)
  }

  # Run through events backwards
  for (row_n in seq(from = nrow(events_df), to = 2)) {

    # Get current row and one before
    current_row <- events_df[row_n, ]
    preceding_row <- events_df[row_n - 1, ]

    vals_to_update <- tibble()

    same_label <- TRUE

    # If the label column exists in the data frame
    if (!is.null(col_select)) {
      # See if the labels for the two events match
      for (col_name in col_select) {
        same_val <- pull(current_row, col_name) == pull(preceding_row, col_name)
        same_label <- same_label & same_val
      }
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

format_events <- function(input_df) {

  # May remove the ungroup function if it is done earlier in the process
  input_df <- ungroup(input_df) %>%
    # Convert onset and offset times to ms and calculate duration
    mutate(onset = round(onset / 1000, 3),
           offset = round(offset / 1000, 3),
           duration = offset - onset)

  # Remove time column, any columns containing "frame" and the "y" column if
  # it exists
  output_df <- select(input_df, -time,
                      -contains("frame"),
                      -matches("^(y|a)$")) %>%
    relocate(behav_name, .before = onset)

  return(output_df)
}

extract_behavs <- function(input_data, behav_name, partial = T) {
  # Extract columns containing behaviour of interest and prepare them
  # for further analysis

  if (partial) {
    pattern <- paste0("^", behav_name, ".*\\.")
  } else {
    pattern <- paste0("^", behav_name, "\\.")
  }
  # Drop any column that doesn't contain behav_name
  output_data <- select(input_data, time, matches(pattern)) %>%
    # Remove behav_name and eveything up to period from column names
    # leaving just suffixes
    rename_all(str_remove,  pattern)

  return(output_data)
}

extend_event_by_frame <- function(input_data, time_in, frame_shift = 1) {

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

extract_unique_objects <- function(input_df) {

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

extract_obj_events <- function(object_label, input_df) {

  # Extracts onset and offset for manipulation of specific objects from PCI
  # events

  # Filter df so only events with that object are inlcuded
  object_df <- filter(input_df, if_any(starts_with("obj"), ~ . == object_label))

  # Add object as single variable
  object_df <- select(object_df, -starts_with("obj")) %>%
    mutate(obj = object_label)

  return(object_df)

}

convert_events_to_objs <- function(input_df) {

  # Takes a data frame of events tagged with objects and convertis it to a data
  # frame of object events, ie events for each opject manipulated

  # Get a list of unique objects for that participant
  unique_objects <- extract_unique_objects(input_df)

  # Run through list extracting event details for each object
  output_df <- map_df(unique_objects,
                      input_df,
                      .f = extract_obj_events)

  output_df <- mutate(output_df,
                      event_ordinal = ordinal,
                      ordinal = row_number())

  return(output_df)

}

parse_behav_events <- function(behav_name, partial_matching = T, raw_data,
                               remove_ambig = F, frame_gap = 2) {

  # Takes raw data and parses selected behavioral events from it
  # Includes 'Remove Ambiguous' flag:
  #   Whether to keep or remove looks coded as ambiguous

  # Extract behav_name columns from data
  raw_behav <- extract_behavs(raw_data, behav_name, partial_matching)

  behav_events <- mutate(raw_behav, # Add frame numbers & behav_name
                         frame_n = row_number(),
                         behav_name = behav_name) %>%
    na.omit() # Remove NAs = include all events, certain and ambiguous

  if (nrow(behav_events) == 0) {
    # If there are no events: set all values as NAs, format as usual and
    # return early.
    behav_events <- add_row(behav_events, behav_name = behav_name) %>%
      format_events()

    return(behav_events)

  } else {
    # Group and summarise data into individual events
    # summarise not used as non-named columns are dropped

    grouped_events <- group_by(behav_events, ordinal)

    grouped_events <- mutate(grouped_events,
                             first_frame = min(frame_n),
                             last_frame = max(frame_n)) %>%
      filter(frame_n == first_frame)

    behav_events <- ungroup(grouped_events)
  }

  # Optional event proccessing
  # Remove abiguous events
  if (remove_ambig & "a" %in% colnames(behav_events)) {
    behav_events <- filter(behav_events, a != "a")
  }

  # Extend offcameras and book reading (and possibly other) events to ensure
  # overlap with events that end/start adjacent
  if (str_detect(behav_name, "offCameras|bookreading")) {
    behav_events <- extend_event_by_frame(behav_events,
                                          time_in = raw_data$time)
  }

  # Extract object events from *obj variables
  if (str_detect(behav_name, "(parent|baby)obj")) {
    behav_events <- convert_events_to_objs(behav_events)
  }

  # Merge proximal events
  if (str_detect(behav_name, "(parent|baby)(AT|obj)")) {
    # Should only be for looking events, not actions, with consistent labels
    #
    # Could modify convert_events_to_objs so merging done here, but would need
    # to account for different variable names (label/obj) in merge_events
    behav_events <- merge_events(behav_events,
                                 frame_gap = frame_gap,
                                 behav_name)
  }

  # Format events (convert ms to s, add duration, drop unneeded columns)
  behav_events <- format_events(behav_events)

  return(behav_events)

}

# Finding and processing overlap functions ----

select_behav <- function(input_df, behav_selected, add_col_suffix = NULL) {

  df_out <- filter(input_df, str_detect(behav_name, behav_selected)) %>%
    rename_all(add_col_suffix, .funs = str_c)

  return(df_out)
}


find_overlaps <- function(behav_name1, behav_name2, df_in, incbounds = FALSE) {

  df1 <- select_behav(df_in, behav_name1, add_col_suffix = ".1")
  df2 <- select_behav(df_in,behav_name2, add_col_suffix = ".2")

  expanded_dfs <- crossing(df1, df2)

  overlap_df <- mutate(
    expanded_dfs,
    onset1_in_ev2 = between(onset.1, onset.2, offset.2,
                            incbounds, NAbounds = NA),
    offset1_in_ev2 = between(offset.1, onset.2, offset.2,
                             incbounds, NAbounds = NA),
    onset2_in_ev1 = between(onset.2, onset.1, offset.1,
                            incbounds, NAbounds = NA),
    offset2_in_ev1 = between(offset.2, onset.1, offset.1,
                             incbounds, NAbounds = NA)) %>%
    mutate(across(contains("_in_"), replace_na, FALSE))

  return(overlap_df)

}

extend_event_overlaps <- function(behav_df_in, overlapping_events) {

  # Set to proccess data rowwise
  adjusted_overlapping_events <- rowwise(overlapping_events) %>%
    # Find which onset was earlier and which offset was later across each
    # overlapping event pairing
    mutate(event_ID = event_ID.2,
           behav_name = behav_name.2,
           onset = min(onset.1, onset.2),
           offset = max(offset.1, offset.2))

  # Group the event_IDs together and find the overall earliest/latest
  # onset/offset for that event
  adjusted_events <- group_by(adjusted_overlapping_events, event_ID) %>%
    summarise(behav_name = first(behav_name),
              onset = min(onset),
              offset = max(offset),
              .groups = "drop")

  # Update main behav df with new timings
  behav_df_out <- rows_upsert(behav_df_in, adjusted_events, by = "event_ID") %>%
    mutate(duration = offset - onset)

  return(behav_df_out)
}


remove_overlapping_events <- function(target_event, comparator_event,
                                      behav_df_in, extend = FALSE){


}

find_overlapping_events <- function(df_in, behav_name1, behav_name2,
                                    out_behav_name = "overlapping_events",
                                    which_first_names = NULL){

  # If first event names not given then set them to be the behav_name with the
  # suffix "_first"
  if (is.null(which_first_names) | length(which_first_names) != 2) {
    ev1_first = str_c(behav_name1, "first", sep = "_")
    ev2_first = str_c(behav_name2, "first", sep = "_")
  } else {
    ev1_first = str_c(which_first_names[1], "first", sep = "_")
    ev2_first = str_c(which_first_names[2], "first", sep = "_")
  }

  # Find overlapping events
  crossed_events <- find_overlaps(behav_name1, behav_name2, df_in)

  # Only select events with overlap
  overlapping_events <- mutate(crossed_events, event_overlap =
             onset1_in_ev2 | offset1_in_ev2 |
             onset2_in_ev1 | offset2_in_ev1) %>%
    filter(event_overlap)

  if (nrow(overlapping_events)>0){
    # Find Which event came first in the overlap
    overlapping_df <- rowwise(overlapping_events) %>%
      transmute(behav_name = out_behav_name,
                onset = max(onset.1, onset.2),
                offset = min(offset.1, offset.2),
                which_first = case_when(onset2_in_ev1 ~ ev1_first,
                                        onset1_in_ev2 ~ ev2_first))
  } else {
    overlapping_df <- tibble(
      behav_name = character(),
      onset = numeric(),
      offset = numeric(),
      which_first = character())
  }

  return(overlapping_df)
}
