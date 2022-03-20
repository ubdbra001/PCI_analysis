file_search <- function(path, pattern){

  # Function that takes a path and pattern as input, finds all matching
  # occurrences and checks if there is just a single occurrence
  # Returns a list with an indicator if it was a single occurrence, and path to all
  # ocurrences
  matching_files <- dir(path, pattern, full.names = TRUE)
  n_files <- length(matching_files)
  single_file <- n_files > 0 & n_files < 2

  output <- list(single_file = single_file, path = matching_files)
  return(output)
}


load_file <- function(path, col_def, delim, filter_col) {
  data <- read_delim(path, delim = delim, col_types = col_def) %>%
    filter(!is.na(!!sym(filter_col)))
}


column_compare <- function(behav_name, df1, df2){

  df1.extracted <- select(df1, behav_name) %>% pull()
  df2.extracted <- select(df2, behav_name) %>% pull()

  overlap_prop <- vector_overlap(df1.extracted, df2.extracted)

  return(overlap_prop)
}


vector_overlap <- function(v1, v2) {

  # Function that takes two vectors, compares them, and return a vector of overlaps
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}


extract_vector <- function(df.in, behav_name){

  df.extracted <- select(df.in, starts_with(behav_name) & ends_with("ordinal")) %>%
    pull()

  return(df.extracted)
}

find_differences <- function(rle_list, min_diff_length = 0){

  if (length(rle_list$values) == 1){return()}

  # Get the starting location of each block
  rle_list$location <- c(1, cumsum(rle_list$lengths)[-length(rle_list$lengths)] + 1)

  # Get the start location only for blocks where value is F (mismatch) and
  # overlap is greater than specified above
  diff_locations <- rle_list$location[!rle_list$values & rle_list$lengths >= min_diff_length]

}

