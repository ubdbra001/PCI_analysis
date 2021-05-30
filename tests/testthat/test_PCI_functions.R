#Imports and sourcing functions to test ----

library(testthat)
library(tidyverse)

source("../../R/PCI_functions.R")


# Test event merging function ----

# Mock function to generate data to test:

merging_data <- function() {
  test_data <- tibble(
    ordinal = seq_len(4),
    first_frame = c(1, 12, 34, 55),
    last_frame = c(5, 32, 50, 75),
    onset = c(450, 813, 1539, 3000),
    offset = c(582, 1308, 2067, 4000)
  )

  return(test_data)
}

# No merging (frame_gap = 0) ----

test_that("No merging when frame_gap equals 0", {

  test_data <- gen_merging_data()

  frame_gap <- 0
  expect_equal(merge_events(test_data, frame_gap),
               test_data)

})
