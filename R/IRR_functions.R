file_search <- function(path, pattern){

  # Function that takes a path and pattern as input, finds all matching
  # occurrences and checks if there is just a single occurrence
  # Returns a list with an indicator if it was a single occurrence, and path to all
  # ocurrences
  matching_files <- dir(path, pattern, full.names = TRUE)
  single_file <- length(c1_files) > 0 & length(c1_files) < 2

  output <- list(single_file, path = matching_files)
  return(output)
}