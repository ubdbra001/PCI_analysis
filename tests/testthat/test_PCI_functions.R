#Imports and sourcing functions to test ----

library(testthat)
library(tidyverse)

source("../../R/PCI_functions.R")


# 1 Test event merging function ----

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

# 1.1 No merging (frame_gap = 0) ----

test_that("No merging when frame_gap equals 0", {

  test_data <- merging_data()
  expected_output <- test_data
  frame_gap <- 0

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)

})

# 1.2 Default merging (frame_gap = 3) ----

test_that("Merging when frame_gap equals 3", {

  test_data <- merging_data()
  expected_output <- tibble(
    ordinal = c(1, 2, 4),
    first_frame = c(1, 12, 55),
    last_frame = c(5, 50, 75),
    onset = c(450, 813, 3000),
    offset = c(582, 2067, 4000)
  )
  frame_gap <- 3

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)
})

# 1.3 Generous merging (frame_gap = 5) ----

test_that("Merging when frame_gap equals 5", {

  test_data <- merging_data()
  expected_output <- tibble(
    ordinal = c(1, 2),
    first_frame = c(1, 12),
    last_frame = c(5, 75),
    onset = c(450, 813),
    offset = c(582, 4000)
  )
  frame_gap <- 5

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)
})

# 1.4 Do not merge when labels are different ----

test_that("Events not merged when lebels are different", {
  test_data <- merging_data()
  test_data <- add_column(test_data, label = c("a", "a", "b", "b"))
  expected_output <- test_data
  frame_gap <- 3

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)
})
