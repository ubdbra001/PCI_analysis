#Imports and sourcing functions to test ----

library(testthat)
library(tidyverse)

source("../../R/PCI_functions.R")


# 1 Test merge_events function ----

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

# 1.1
test_that("No merging when frame_gap equals 0", {

  test_data <- merging_data()
  expected_output <- test_data
  frame_gap <- 0

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)

})

# 1.2
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

# 1.3
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

# 1.4
test_that("Events not merged when labels are different", {
  test_data <- merging_data()
  test_data <- add_column(test_data, label = c("a", "a", "b", "b"))
  expected_output <- test_data
  frame_gap <- 3

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)
})

# 1.5
test_that("Events merged when labels are the same", {
  test_data <- merging_data()
  test_data <- add_column(test_data, label = c("a", "a", "a", "a"))
  expected_output <- tibble(
    ordinal = c(1, 2, 4),
    first_frame = c(1, 12, 55),
    last_frame = c(5, 50, 75),
    onset = c(450, 813, 3000),
    offset = c(582, 2067, 4000),
    label = "a")
  frame_gap <- 3

  expect_equal(
    merge_events(test_data, frame_gap),
    expected_output)
})

# 2 Test format_events function ----

# 2.1
test_that("Times converted from ms to seconds and duration added", {

  test_data <- tibble(
    time = 73,
    behav_name = "behav",
    onset = 2718,
    offset = 3141)

  expected_output <- tibble(
    behav_name = "behav",
    onset = 2.718,
    offset = 3.141,
    duration = 0.423)

  expect_equal(
    format_events(test_data),
    expected_output)

})

# 2.2
test_that("Columns with 'frame' in name removed", {

  test_data <- tibble(
    time = 73,
    behav_name = "behav",
    onset = 2718,
    offset = 3141,
    frame_n = 23,
    a_column_with_frame_in_name = 35)

  expected_output <- tibble(
    behav_name = "behav",
    onset = 2.718,
    offset = 3.141,
    duration = 0.423)

  expect_equal(
    format_events(test_data),
    expected_output)

})

# 2.3
test_that("Column y removed", {

  test_data <- tibble(
    time = c(73, 85),
    behav_name = "behav",
    onset = c(2718, 5000),
    offset = c(3141, 7000),
    frame_n = c(23, 632),
    y = "y")

  expected_output <- tibble(
    behav_name = "behav",
    onset = c(2.718, 5.000),
    offset = c(3.141, 7.000),
    duration = c(0.423, 2.000))

  expect_equal(
    format_events(test_data),
    expected_output)
})

# 3 Test extract_behavs function ----

# 3.1
test_that("Specific behaviour is extracted correctly from data", {

  behav_name <- 'behav'
  test_data <- tibble(
    time = NA_integer_,
    behav.var1 = 1,
    behav.var2 = 2,
    bar.var1 = 3,
    bar.var2 = 4)
  expected_output <- tibble(
    time = NA_integer_,
    var1 = 1,
    var2 = 2)

  expect_equal(
    extract_behavs(test_data, behav_name),
    expected_output)

})

# 3.1
test_that("Partial matching for behaviour", {

  behav_name <- 'behav'
  test_data <- tibble(
    time = NA_integer_,
    behaviour.var1 = 1,
    behaviour.var2 = 2,
    bar.var1 = 3,
    bar.var2 = 4)
  expected_output <- tibble(
    time = NA_integer_,
    var1 = 1,
    var2 = 2)

  expect_equal(
    extract_behavs(test_data, behav_name),
    expected_output)

})

# 4 Test extend_event_by_frame functions ----

# 4.1

test_that("Times adjusted by default: one frame", {

  times <- seq(from = 450, by = 25, length.out = 50)
  test_data <- tibble(
    first_frame = c(1, 12, 34),
    last_frame = c(6, 27, 50),
    onset = c(450, 725, 1275),
    offset = c(575, 1100, 1675))

  expected_output <- tibble(
    first_frame = c(1, 11, 33),
    last_frame = c(7, 28, 50),
    onset = c(450, 700, 1250),
    offset = c(600, 1125, 1675))

  expect_equal(
    extend_event_by_frame(test_data, time_in = times),
    expected_output)

})


# 4.2
test_that("Times adjusted to three frames preceding/following event", {

  times <- seq(from = 450, by = 25, length.out = 50)
  test_data <- tibble(
    first_frame = c(1, 12, 34),
    last_frame = c(6, 27, 50),
    onset = c(450, 725, 1275),
    offset = c(575, 1100, 1675))

  expected_output <- tibble(
    first_frame = c(1, 9, 31),
    last_frame = c(9, 30, 50),
    onset = c(450, 650, 1200),
    offset = c(650, 1175, 1675))

  expect_equal(
    extend_event_by_frame(test_data, time_in = times, frame_shift = 3),
    expected_output)

})

# 4.3
test_that("Times not adjusted", {

  times <- seq(from = 450, by = 25, length.out = 50)
  test_data <- tibble(
    first_frame = c(1, 12, 34),
    last_frame = c(6, 27, 50),
    onset = c(450, 725, 1275),
    offset = c(575, 1100, 1675))

  expected_output <- test_data

  expect_equal(
    extend_event_by_frame(test_data, time_in = times, frame_shift = 0),
    expected_output)

})