# Load custom functions & libraries ####
source("R/PCI_functions.R")

# Load user defined variables ####
source("PCI_uservars.R")

# Generate output filename
if (remove_ambiguous) {
  output_file <- sprintf("%s_noAmbg.csv", output_file_name)
} else {
  output_file <- sprintf("%s_inAmbg.csv", output_file_name)
}

# Prepare empty dataframes for data output
summary_stats_output <- tibble()
all_looks_output <- tibble()
mutual_looks_output <- tibble()
overlapping_events_output <- tibble()
count_naming_overlaps_output <- tibble()


# Body of Script ----

files <- dir(path = raw_data_path,
             pattern = input_pattern,
             full.names = TRUE)

for (file_name in files) {

  # Extract participant ID
  PartID <- str_extract(file_name, partID_regex)

  # Load PCI data
  PCIData <- read_delim(file_name, delim = "|", col_types = data_col_def)

  # Parse behavioural events from raw data
  behav_events <- map2_df(.x = behav_names,
                          .y = partial_matching,
                          raw_data = PCIData,
                          frame_gap = frame_gap,
                          remove_ambig = remove_ambiguous,
                          .f = parse_behav_events) %>%
    mutate(event_ID = row_number(), .before = 1)

  # Save raw events

  # Remove events that overlap with offCameras and extend offCameras
  behav_events <- remove_overlapping_events(
    target_ev_name = "offCamera",
    comparator_ev_name = "baby|parent",
    behav_df_in = behav_events,
    extend = TRUE)

  if (remove_bookreading) {
    behav_events <- remove_overlapping_events(
      target_ev_name = "bookreading",
      comparator_ev_name = "baby|parent",
      behav_df_in = behav_events,
      extend = TRUE)
  }


  all_looks_output <- filter(behav_events, str_detect(behav_name, "AT(parent|baby)") ) %>%
    select(ordinal, behav_name, onset, offset, duration) %>%
    mutate(PartID = PartID, .before = 1) %>%
    bind_rows(all_looks_output, .)

  # Generate stats for all events
  event_summary_stats <- summarise_events(behav_events)

  summary_stats_output <- add_summary_stats(event_summary_stats,
                                            summary_stats_output,
                                            PartID)

  # Get individual mutual look events
  mutual_look_events <- process_mutual_looks(behav_events,
                                               "parentATbaby",
                                               "babyATparent") %>%
    add_column(Part_ID = PartID, .before = 1)

  # Bind events for output
  mutual_looks_output <- bind_rows(mutual_looks_output, mutual_look_events)

  # Still need to decide what to do with summarising events, might want to set up
  # a separate function that calls the summary_function on the mutual looks and
  # the different first look types
  # summarise_events(mutual_looks_summary)

  overlapping_events <- map_df(comp_events, find_naming_overlap,
                               target_event = target_event, data_in = behav_events)

  # Save overlapping events
  overlapping_events_output <- add_column(overlapping_events, Part_ID = PartID, .before = 1) %>%
    bind_rows(overlapping_events_output, .)


  # Get all of the naming events
  naming_events <- select_behav(behav_events, target_event) %>%
    select(event_ID, ordinal, label, referent1, referent2)

  overlapping_ev_counts <- count_naming_overlaps(data_in = overlapping_events,
                                                 naming_events = naming_events) %>%
    add_column(Part_ID = PartID, .before = 1)

  count_naming_overlaps_output <- bind_rows(count_naming_overlaps_output, overlapping_ev_counts)



  # Generate stats for other crossover events
  #   parentnoun and parent/babyobj - Does the noun refer to something that the parent or baby is holding?
  #   parentnoun and baby/parentATobj - Does the noun refer to something that the parent or baby is looking at?
  #
  #   Will need a function to extend the parentnoun behav and add the noun referent

  # repeat these actions after removing bookreading crossover events

}



save(mutual_looks_output, file = "data/looks_data/mutual_looks.RData")
save(all_looks_output, file = "data/looks_data/all_looks.RData")
save(overlapping_events_output, file = "data/naming_data/overlaping_naming_events.RData")

write_csv(summary_stats_output, "data/event_data/events_summary.csv")
write_csv(count_naming_overlaps_output, "data/naming_data/overlaps_count.csv")

