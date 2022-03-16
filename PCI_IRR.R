library(tidyverse)

source("PCI_uservars.R")
source("R/IRR_functions.R")

# set directories
data_dir <- "data/IRR"
coder1_dir <- file.path(data_dir, "coder1")
coder2_dir <- file.path(data_dir, "coder2")


# Load file that specifies the IRR available for each participant
IRR_master <- read_csv(file.path(data_dir, "IRR_master.csv"))

# For each ID
for (part_ID in IRR_master$ID){

  # Generate patten to search dirs for
  file_pattern <- 10

  # See if there are corresponding files in coder1 and coder2 dirs
  c1_files <- file_search(coder1_dir, file_pattern)
  c2_files <- file_search(coder2_dir, file_pattern)

  # Skip if either file is not present
  if (c1_files$single_file & c1_files$single_file){
    c1_data <- read_csv(c1_files$path)
    c2_data <- read_csv(c2_files$path)
  } else {
    next
  }

  # Need to check file consistency?


  # Select variables to compare
  #   Will be participant dependent
  #   See associated csv file


}





# Compare variables
#   Frame by frame basis
#   Compute percentage of overlap
#   Bonus: Return places where differences occur
#     Customise length of difference

# Output results
#   Overall IRR
#   Locations of deviations for training


