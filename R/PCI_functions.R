
# Load libraries ----
library(tidyverse)

between <- data.table::between

# Event proccessing functions ----

merge_events <- function(events_df, frame_gap, behav_name) {
  # Merges events if gap betwen last and first frames is less than or equal to
  # specified value.

  # Loops backwards through data frame. Checks gap, if less than frame.gap then
  # update preceding row with current row end points and then delete current row

  if (nrow(events_df) < 2) return(events_df)

  # Make sure df is not grouped before processing further
  if (is_grouped_df(events_df)) events_df <- ungroup(events_df)

  for (row_n in seq(from = nrow(events_df), to = 2)) {

    current_row <- events_df[row_n, ]
    preceding_row <- events_df[row_n - 1, ]

    vals_to_update <- tibble()

    if ((current_row$first_frame - preceding_row$last_frame) <= frame_gap) {

      vals_to_update <- tibble(select(preceding_row, ordinal),
                               select(current_row, offset, last_frame))

      events_df <- rows_update(events_df, vals_to_update, by = ordinal) %>%
        rows_delete(current_row["ordinal"])
    }
  }
  return(events_df)
}
