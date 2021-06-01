# Imports and sourcing functions to test ----

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
test_that("Events not merged when single referent is different", {
  test_data <- merging_data()
  test_data <- add_column(test_data, referent1 = c("a", "a", "b", "b"))
  expected_output <- test_data

  frame_gap <- 3
  behav_name <- "parentATobj"

  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)
})


# 1.5
test_that("Events not merged when one of two referents are different",{
  test_data <- merging_data()
  test_data <- mutate(test_data,
                      referent1 = c("a", "a", "a", "a"),
                      referent2 = c("a", "a", "b", "a"))
  expected_output <- test_data

  frame_gap <- 3
  behav_name <- "babyATobj"

  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)

})

# 1.6
test_that("Single event merged when frame gap = 8, as referents are different",{
  test_data <- merging_data()
  test_data <- mutate(test_data,
                      referent1 = "a",
                      referent10 = c(".", ".", "d", "."),
                      referent42 = "e")
  expected_output <- tibble(
    ordinal = c(1,3,4),
    first_frame = c(1, 34, 55),
    last_frame = c(32, 50, 75),
    onset = c(450, 1539, 3000),
    offset = c(1308, 2067, 4000),
    referent1 = c("a", "a", "a"),
    referent10 = c(".", "d", "."),
    referent42 = c("e", "e", "e"))

  frame_gap <- 8
  behav_name <- "parentATobj"

  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)

})

# 1.7
test_that("Events merged when referents are the same", {
  test_data <- merging_data()
  test_data <- mutate(test_data,
                      referent1 = c("a", "a", "a", "a"),
                      referent23 = c("b", "b", "b", "b"))
  expected_output <- tibble(
    ordinal = c(1, 2, 4),
    first_frame = c(1, 12, 55),
    last_frame = c(5, 50, 75),
    onset = c(450, 813, 3000),
    offset = c(582, 2067, 4000),
    referent1 = "a",
    referent23 = "b")

  frame_gap <- 3
  behav_name <- "babyATobj"

  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)
})

# 1.8
test_that("Events not merged when objs are different",{
  test_data <- merging_data()
  test_data <- add_column(test_data, obj = c("a", "a", "b", "b"))
  expected_output <- test_data

  frame_gap <- 3
  behav_name <- "babyobj"

  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)
})


# 1.9
test_that("Events merged when objs are the same",{
  test_data <- merging_data()
  test_data <- add_column(test_data, obj = c("a", "a", "b", "b"))
  expected_output <- tibble(
    ordinal = c(1, 2, 3),
    first_frame = c(1, 12, 34),
    last_frame = c(5, 32, 75),
    onset = c(450, 813, 1539),
    offset = c(582, 1308, 4000),
    obj = c("a", "a", "b"))

  frame_gap <- 5
  behav_name <- "parentobj"


  expect_equal(
    merge_events(test_data, frame_gap, behav_name),
    expected_output)
})


# Need to add tests for referents, not labels.

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

# 4.4
test_that("Negative values treated as 0", {

  times <- seq(from = 450, by = 25, length.out = 50)
  test_data <- tibble(
    first_frame = c(1, 12, 34),
    last_frame = c(6, 27, 50),
    onset = c(450, 725, 1275),
    offset = c(575, 1100, 1675))

  expected_output <- test_data

  expect_equal(
    extend_event_by_frame(test_data, time_in = times, frame_shift = -5),
    expected_output)

})


# 5 Test extract_unique_objects function ----

# 5.1
test_that("Unique objects extracted", {

  test_data <- tibble(
    obj1 = c("teapot", ".", "."),
    obj2 = c("book", "book", "glass"),
    obj3 = c("shirt", "!", "glasses"),
    obj34 = c(" ", "pillow", "?")
  )

  expected_output <- c(
    "teapot", "book","shirt", "glasses", "pillow","glass")

  expect_equal(
    sort( extract_unique_objects(test_data) ),
    sort( expected_output) )

})

# 6 Test extract_objs_from_events function ----

# 6.1
test_that("Object correctly extracted from single event", {

  test_data <- tibble(
    behav_name = "behav",
    onset = 2.000,
    offset = 3.500,
    obj1 = "spoon",
    obj2 = ".")

  expected_output <- tibble(
    behav_name = "behav",
    onset = 2.000,
    offset = 3.500,
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )

})

# 6.2
test_that("ignores other objects", {
  test_data <- tibble(
    behav_name = "behav",
    onset = 2.000,
    offset = 3.500,
    obj1 = "spoon",
    obj2 = ".")

  expected_output <- tibble(
    behav_name = character(),
    onset = double(),
    offset = double(),
    obj = character()
  )

  target_obj <- "teacup"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )
})

# 6.3
test_that("Proximal obj events with same object merged", {

  test_data <- tibble(
    ordinal = c(1, 2),
    behav_name = "behav",
    onset = c(2.000, 4.200),
    offset = c(3.500, 7.500),
    first_frame = c(25, 35),
    last_frame = c(33, 56),
    obj1 = "spoon",
    obj2 = ".")

  expected_output <- tibble(
    ordinal = 1,
    behav_name = "behav",
    onset = 2.000,
    offset = 7.500,
    first_frame = 25,
    last_frame = 56,
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )

})

# 6.4
test_that("Proximal events with different objects unaffected", {
  test_data <- tibble(
    ordinal = c(1, 2),
    behav_name = "behav",
    onset = c(2.000, 4.200),
    offset = c(3.500, 7.500),
    first_frame = c(25, 35),
    last_frame = c(33, 56),
    obj1 = c("spoon", "teacup"),
    obj2 = ".")

  expected_output <- tibble(
    ordinal = 1,
    behav_name = "behav",
    onset = 2.000,
    offset = 3.500,
    first_frame = 25,
    last_frame = 33,
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )

})

# 6.5
test_that("Proximal obj events with objs across diff columns merged", {
  test_data <- tibble(
    ordinal = c(1, 2),
    behav_name = "behav",
    onset = c(2.000, 4.200),
    offset = c(3.500, 7.500),
    first_frame = c(25, 35),
    last_frame = c(33, 56),
    obj1 = c("spoon", "."),
    obj2 = c("teacup", "spoon"))

  expected_output <- tibble(
    ordinal = 1,
    behav_name = "behav",
    onset = 2.000,
    offset = 7.500,
    first_frame = 25,
    last_frame = 56,
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )
})

# 6.6
test_that("More that 2 proximal events across different columns merged", {

  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", ".", "spoon"),
    obj2 = c("teacup", "spoon", "."))

  expected_output <- tibble(
    ordinal = 1,
    behav_name = "behav",
    onset = 2.000,
    offset = 9.365,
    first_frame = 25,
    last_frame = 75,
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )

})

# 6.7
test_that("Non-proximal events extracted but remain unmerged", {

  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", ".", "spoon"),
    obj2 = c("teacup", "teapot", "."))

  expected_output <- tibble(
    ordinal = c(1, 3),
    behav_name = "behav",
    onset = c(2.000, 8.000),
    offset = c(3.500, 9.365),
    first_frame = c(25, 57),
    last_frame = c(33, 75),
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )

})

# 7 Test convert_events_to_objs function ----

# 7.1
test_that("Single obj event successfully extracted", {
  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", ".", "spoon"),
    obj2 = c(".", "spoon", "."))

  expected_output <- tibble(
    ordinal = 1,
    behav_name = "behav",
    onset = 2.000,
    offset = 9.365,
    first_frame = 25,
    last_frame = 75,
    obj = "spoon")

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 7.2
test_that("Two object events successfully extracted", {
  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", ".", "spoon"),
    obj2 = c("teacup", "spoon", "."))

  expected_output <- tibble(
    ordinal = c(1, 1),
    behav_name = "behav",
    onset = c(2.000, 2.000),
    offset = c(9.365, 3.500),
    first_frame = c(25, 25),
    last_frame = c(75, 33),
    obj = c("spoon", "teacup")
  )

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 7.3
test_that("Three object events successfully extracted across two cols", {
  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", "teapot", "spoon"),
    obj2 = c("teacup", "spoon", "."))

  expected_output <- tibble(
    ordinal = c(1, 1, 2),
    behav_name = "behav",
    onset = c(2.000, 2.000, 4.200),
    offset = c(9.365, 3.500, 7.500),
    first_frame = c(25, 25, 35),
    last_frame = c(75, 33, 56),
    obj = c("spoon", "teacup", "teapot")
  )

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 7.4
test_that("Six object events successfully extracted across three cols ", {
  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", "teapot", "spoon"),
    obj2 = c("teacup", "hat", "hat"),
    obj3 = c(".", "teacup", "glasses"))

  expected_output <- tibble(
    ordinal = c(1, 3, 1, 2, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 8.000, 2.000, 4.200, 4.200, 8.000),
    offset = c(3.500, 9.365, 7.500, 7.500, 9.365, 9.365),
    first_frame = c(25, 57, 25, 35, 35, 57),
    last_frame = c(33, 75, 56, 56, 75, 75),
    obj = c("spoon", "spoon", "teacup", "teapot", "hat", "glasses")
  )

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 8 Test parse_behav_events function ----

load_mock_PCIdata <- function(){

  data_col_def <- "ddddcdddcdddcdddccdddccdddcccccdddcdddcdddcccccdddcdddcdddcdddcdddcdddcdddcccccdddccccdddccccdddc"

  file_path <- test_path("mock_input", "mock_data.csv")
  test_data <- read_csv(file_path, col_types = data_col_def)

  return(test_data)
}

# 8.1
test_that("Single event successfully extracted", {

  behav_name <- 'PCIduration'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = 1,
    behav_name = behav_name,
    onset = 0.020,
    offset = 4.000,
    duration = 3.980)

  expect_equal(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = T),
    expected_output)

})

# 8.2
test_that("Two events successfully extracted", {

  behav_name <- 'offCamera'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 2),
    behav_name = behav_name,
    onset = c(0.180, 2.760),
    offset = c(0.400, 2.900),
    duration = c(0.220, 0.140))

  expect_equal(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = T),
    expected_output)

})

# 8.3
test_that("behav with no events returns data frame with just name and NAs", {

  behav_name <- 'notes'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = as.double(NA),
    behav_name = behav_name,
    onset = as.double(NA),
    offset = as.double(NA),
    general = as.character(NA),
    duration = as.double(NA))

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = T),
    expected_output)

})

# 8.4
test_that("Ambiguous events removed when flag is set to true", {

  behav_name <- 'parentATbaby'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = 2,
    behav_name = behav_name,
    onset = 2.140,
    offset = 2.300,
    duration = 0.160)

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = T),
    expected_output)
})

# 8.5
test_that("Parse behavioural events - don't remove ambiguous", {

  behav_name <- 'parentATbaby'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 2),
    behav_name = behav_name,
    onset = c(1.380, 2.140),
    offset = c(1.600, 2.300),
    duration = c(0.220, 0.160))

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F),
    expected_output)

})

# 8.6
test_that("Proximal ambiguous and non-ambiguous events merged", {

  behav_name <- 'babyATparent'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = 1,
    behav_name = behav_name,
    onset = 0.080,
    offset = 0.620,
    duration = 0.540)

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F),
    expected_output)

})

# 8.6
test_that("Proximal events not merged when frame_gap is set to 0", {

})

# 8.7
test_that("Proximal events merged when frame_gap is set to 10", {

})

# 8.8
test_that("Proximal events with different labels not merged", {})

# 8.9
test_that("Object events extracted from data correctly", {})



# 9 Test select_behavs function ----
