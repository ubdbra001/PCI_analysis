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

# 3.2
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

# 3.3
test_that("Partial matching doesn't catch similarly named behavs",{

  behav_name <- 'behav'
  test_data <- tibble(
    time = NA_integer_,
    behav.var1 = 42,
    behav.var2 = 66,
    behav.var3 = 67,
    behaviour.var1 = 1,
    behaviour.var2 = 2,
    bar.var1 = 3,
    bar.var2 = 4)
  expected_output <- tibble(
    time = NA_integer_,
    var1 = 42,
    var2 = 66,
    var3 = 67)

  expect_equal(
    extract_behavs(test_data, behav_name, partial = F),
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
test_that("Obj events across diff columns extracted", {
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
    ordinal = c(1, 2),
    behav_name = "behav",
    onset = c(2.000, 4.200),
    offset = c(3.500, 7.500),
    first_frame = c(25, 35),
    last_frame = c(33, 56),
    obj = "spoon")

  target_obj <- "spoon"

  expect_equal(
    extract_obj_events(target_obj, test_data),
    expected_output
  )
})

# 6.7
test_that("Non-proximal events extracted", {

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
test_that("Single obj type successfully extracted", {
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
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj = "spoon",
    event_ordinal = c(1, 2, 3))

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 7.2
test_that("Two object types successfully extracted", {
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
    ordinal = c(1, 2, 3, 4),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000, 2.000),
    offset = c(3.500, 7.500, 9.365, 3.500),
    first_frame = c(25, 35, 57, 25),
    last_frame = c(33, 56, 75, 33),
    obj = c("spoon", "spoon", "spoon", "teacup"),
    event_ordinal = c(1, 2, 3, 1)
  )

  expect_equal(
    convert_events_to_objs(test_data),
    expected_output
  )

})

# 7.3
test_that("Four object types successfully extracted across three cols", {
  test_data <- tibble(
    ordinal = c(1, 2, 3),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57),
    last_frame = c(33, 56, 75),
    obj1 = c("spoon", "teapot", "spoon"),
    obj2 = c("teacup", "spoon", "."),
    obj3 = c(".", ".", "hat"))

  expected_output <- tibble(
    ordinal = c(1, 2, 3, 4, 5, 6),
    behav_name = "behav",
    onset = c(2.000, 4.200, 8.000, 2.000, 4.200, 8.000),
    offset = c(3.500, 7.500, 9.365, 3.500, 7.500, 9.365),
    first_frame = c(25, 35, 57, 25, 35, 57),
    last_frame = c(33, 56, 75, 33, 56, 75),
    obj = c("spoon", "spoon", "spoon", "teacup", "teapot", "hat"),
    event_ordinal = c(1, 2, 3, 1, 2, 3))

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
test_that("Two events successfully extracted - include ambiguous", {

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
test_that("Exculsion events extended by one frame at either end", {

  behav_name <- 'offCameras'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 2),
    behav_name = behav_name,
    onset = c(0.160, 2.740),
    offset = c(0.420, 2.920),
    duration = c(0.260, 0.180))

  expect_equal(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = T),
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

  behav_name <- 'babyATobj'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 2),
    behav_name = behav_name,
    onset = c(2.520, 2.860),
    offset = c(2.720, 3.060),
    referent1 = "teacup",
    referent2 = ".",
    duration = c(0.200, 0.200))

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F,
                       frame_gap = 0),
    expected_output)

})

# 8.7
test_that("Proximal events merged when frame_gap is set to 10", {

  behav_name <- 'babyATobj'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = 1,
    behav_name = behav_name,
    onset = 2.520,
    offset = 3.060,
    referent1 = "teacup",
    referent2 = ".",
    duration = 0.540)

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F,
                       frame_gap = 10),
    expected_output)

})

# 8.8
test_that("Proximal events with different referents not merged", {

  behav_name <- 'parentATobj'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 2),
    behav_name = behav_name,
    onset = c(3.620, 3.780),
    offset = c(3.760, 3.880),
    referent1 = "teapot",
    referent2 = c(".", "teacup"),
    duration = c(0.140, 0.100))

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F,
                       frame_gap = 2),
    expected_output)


})

# 8.9
test_that("Object events extracted from data correctly", {

  behav_name <- 'babyobj'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 3),
    behav_name = behav_name,
    onset = c(0.740, 0.740),
    offset = c(1.100, 0.920),
    obj = c("spoon", "fork"),
    event_ordinal = c(1, 1),
    duration = c(0.360, 0.180))

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F,
                       partial_matching = F),
    expected_output)
})

# 8.10
test_that("Object events extracted and merged across cols correctly", {

  behav_name <- 'parentobj'

  test_data <- load_mock_PCIdata()

  expected_output <- tibble(
    ordinal = c(1, 4, 5, 6),
    behav_name = behav_name,
    onset = c(1.240, 1.240, 1.560, 1.400),
    offset = c(1.700, 1.380, 1.700, 1.700),
    obj = c("spoon", "knife", "knife", "fork"),
    event_ordinal = c(1, 1, 3, 2),
    duration = offset - onset)

  expect_equivalent(
    parse_behav_events(behav_name,
                       raw_data = test_data,
                       remove_ambig = F,
                       partial_matching = F),
    expected_output)
})

# 8.11
test_that("All events successfully extracted - keep ambiguous", {

  behav_names <- c("PCIduration", "bookreading", "offCameras",
                   "babyATparentface", "parentATbabyface",
                   "babyobj", "parentobj", "parentnoun",
                   "babyATobj", "parentATobj")

  partial_matching <- c(T, T, T, T, T, F, F, T, T, T)

  test_data <- load_mock_PCIdata()

  file_path <- test_path("mock_input", "mock_behavs.csv")
  expected_output <- read_csv(file_path, col_types = "dcdddcdccc")

  expect_equivalent(
    map2_df(.x = behav_names,
            .y = partial_matching,
            raw_data = test_data,
            remove_ambig = FALSE,
            .f = parse_behav_events),
    expected_output)

})

# 9 Test select_behavs function ----

load_mock_behavData <- function(){

  data_col_def <- "dcdddcdccc"

  file_path <- test_path("mock_input", "mock_behavs.csv")
  test_data <- read_csv(file_path, col_types = data_col_def)

  return(test_data)

}

# 9.1
test_that("Single behav selected",{

  test_data <- load_mock_behavData()

  behav_name <- "parentobj"

  expected_output <- tibble(
    ordinal = c(1, 4, 5, 6),
    behav_name = behav_name,
    onset = c(1.24, 1.24, 1.56, 1.4),
    offset = c(1.7, 1.38, 1.7, 1.7),
    duration = offset - onset,
    obj = c("spoon", "knife", "knife", "fork"),
    event_ordinal = c(1, 1, 3, 2),
    label = NA_character_,
    referent1 = NA_character_,
    referent2 = NA_character_)

  expect_equivalent(
    select_behav(test_data, behav_name),
    expected_output)

})

# 9.2
test_that("Nothing returned when selected behav not present",{

  test_data <- load_mock_behavData()

  behav_name <- "behav"

  expected_output <- tibble(
    ordinal = numeric(),
    behav_name = character(),
    onset = numeric(),
    offset = numeric(),
    duration = numeric(),
    obj = character(),
    event_ordinal = numeric(),
    label = character(),
    referent1 = character(),
    referent2 = character())

  expect_equivalent(
    select_behav(test_data, behav_name),
    expected_output)

})

# 9.3
test_that("Suffix added to variable names",{

  test_data <- load_mock_behavData()

  behav_name <- "parentobj"

  expected_output <- tibble(
    ordinal.test = c(1, 4, 5, 6),
    behav_name.test = behav_name,
    onset.test = c(1.24, 1.24, 1.56, 1.4),
    offset.test = c(1.7, 1.38, 1.7, 1.7),
    duration.test = offset.test - onset.test,
    obj.test = c("spoon", "knife", "knife", "fork"),
    event_ordinal.test = c(1, 1, 3, 2),
    label.test = NA_character_,
    referent1.test = NA_character_,
    referent2.test = NA_character_)

  expect_equivalent(
    select_behav(test_data, behav_name, add_col_suffix = ".test"),
    expected_output)
})

# 10 Test find_overlaps function ----

# 10.1
test_that("Overlap between two events found: event 1 before event 2", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(2.5, 5.4),
    offset = c(5.6, 7.2)
  )

  expected_output <- tibble(
    behav_name.1 = behav_name1,
    onset.1 = 2.5,
    offset.1 = 5.6,
    behav_name.2 = behav_name2,
    onset.2 = 5.4,
    offset.2 = 7.2,
    onset1_in_ev2 = FALSE,
    offset1_in_ev2 = TRUE,
    onset2_in_ev1 = TRUE,
    offset2_in_ev1 = FALSE)

  expect_equivalent(
    find_overlaps(behav_name1, behav_name2, test_data),
    expected_output)

})

# 10.2
test_that("Overlap between two events found: event 1 during event 2", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(5.4, 2.5),
    offset = c(5.6, 7.2)
  )

  expected_output <- tibble(
    behav_name.1 = behav_name1,
    onset.1 = 5.4,
    offset.1 = 5.6,
    behav_name.2 = behav_name2,
    onset.2 = 2.5,
    offset.2 = 7.2,
    onset1_in_ev2 = TRUE,
    offset1_in_ev2 = TRUE,
    onset2_in_ev1 = FALSE,
    offset2_in_ev1 = FALSE)

  expect_equivalent(
    find_overlaps(behav_name1, behav_name2, test_data),
    expected_output)

})

# 10.3
test_that("Overlap between two events found: event 2 before event 1", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(5.4, 2.5),
    offset = c(7.2, 5.6)
  )

  expected_output <- tibble(
    behav_name.1 = behav_name1,
    onset.1 = 5.4,
    offset.1 = 7.2,
    behav_name.2 = behav_name2,
    onset.2 = 2.5,
    offset.2 = 5.6,
    onset1_in_ev2 = TRUE,
    offset1_in_ev2 = FALSE,
    onset2_in_ev1 = FALSE,
    offset2_in_ev1 = TRUE)

  expect_equivalent(
    find_overlaps(behav_name1, behav_name2, test_data),
    expected_output)

})

# 10.4
test_that("Overlap between two events found: event 2 during event 1", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(2.5, 5.4),
    offset = c(7.2, 5.6)
  )

  expected_output <- tibble(
    behav_name.1 = behav_name1,
    onset.1 = 2.5,
    offset.1 = 7.2,
    behav_name.2 = behav_name2,
    onset.2 = 5.4,
    offset.2 = 5.6,
    onset1_in_ev2 = FALSE,
    offset1_in_ev2 = FALSE,
    onset2_in_ev1 = TRUE,
    offset2_in_ev1 = TRUE)

  expect_equivalent(
    find_overlaps(behav_name1, behav_name2, test_data),
    expected_output)

})

# 10.5
test_that("Behavs with NAs do not return overlaps", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(2.5, NA),
    offset = c(7.2, NA)
  )

  actual_output <- find_overlaps(behav_name1, behav_name2, test_data)
  actual_output <- select(actual_output,
                          onset1_in_ev2, offset1_in_ev2,
                          onset2_in_ev1, offset2_in_ev1)

  expected_output <- tibble(
    onset1_in_ev2 = FALSE,
    offset1_in_ev2 = FALSE,
    onset2_in_ev1 = FALSE,
    offset2_in_ev1 = FALSE)

  expect_equivalent(
    actual_output,
    expected_output)
})

# 10.6
test_that("Boundary overlaps not found by default", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(2.5, 7.2),
    offset = c(7.2, 10.3)
  )

  actual_output <- find_overlaps(behav_name1, behav_name2, test_data)
  actual_output <- select(actual_output,
                          onset1_in_ev2, offset1_in_ev2,
                          onset2_in_ev1, offset2_in_ev1)

  expected_output <- tibble(
    onset1_in_ev2 = FALSE,
    offset1_in_ev2 = FALSE,
    onset2_in_ev1 = FALSE,
    offset2_in_ev1 = FALSE)

  expect_equivalent(
    actual_output,
    expected_output)

})

# 10.7
test_that("Boundary overlaps found when incbounds is True", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(behav_name1, behav_name2),
    onset = c(2.5, 7.2),
    offset = c(7.2, 10.3)
  )

  actual_output <- find_overlaps(behav_name1, behav_name2, test_data,
                                 incbounds = TRUE)
  actual_output <- select(actual_output,
                          onset1_in_ev2, offset1_in_ev2,
                          onset2_in_ev1, offset2_in_ev1)

  expected_output <- tibble(
    onset1_in_ev2 = FALSE,
    offset1_in_ev2 = TRUE,
    onset2_in_ev1 = TRUE,
    offset2_in_ev1 = FALSE)

  expect_equivalent(
    actual_output,
    expected_output)

})

# 10.8
test_that("Function can handle multiple events at once", {

  behav_name1 <- "behav1"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(rep(behav_name1, 3), rep(behav_name2, 2)),
    onset = c(2.5, 6.2, 11.0, 4.2, 11.5),
    offset = c(5.7, 9.8, 12.0, 7.8, 11.7)
  )

  actual_output <- find_overlaps(behav_name1, behav_name2, test_data)
  actual_output <- select(actual_output,
                          onset1_in_ev2, offset1_in_ev2,
                          onset2_in_ev1, offset2_in_ev1)

  expected_output <- tibble(
    onset1_in_ev2 = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    offset1_in_ev2 = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    onset2_in_ev1 = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
    offset2_in_ev1 = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE))

  expect_equivalent(
    actual_output,
    expected_output)

})

# 10.9
test_that("Function can handle matching partial behav_names", {

  behav_name1 <- "behav1"
  behav_name1a <- "behav1a"
  behav_name1b <- "behav1b"
  behav_name2 <- "behav2"

  test_data <- tibble(
    behav_name = c(rep(behav_name1a, 2), behav_name1b, behav_name2),
    onset = c(2.5, 11.0, NA, 4.2),
    offset = c(5.7, 12.0, NA, 7.8)
  )

  expected_output <- tibble(
    behav_name.1 = c(behav_name1a, behav_name1a, behav_name1b),
    onset.1 = c(2.5, 11.0, NA),
    offset.1 = c(5.7, 12.0, NA),
    behav_name.2 = rep(behav_name2, 3),
    onset.2 = rep(4.2, 3),
    offset.2 = rep(7.8, 3),
    onset1_in_ev2 = c(FALSE, FALSE, FALSE),
    offset1_in_ev2 = c(TRUE, FALSE, FALSE),
    onset2_in_ev1 = c(TRUE, FALSE, FALSE),
    offset2_in_ev1 = c(FALSE, FALSE, FALSE))

  expect_equivalent(
    find_overlaps(behav_name1, behav_name2, test_data),
    expected_output)

})

# 11 Test extend_overlap_events function ----

# 11.1
test_that("Single target event extended to encompass single overlapped event", {

  test_behav_data <- tibble(
    event_ID = c(1, 2),
    behav_name = c("behav1", "behav2"),
    onset = c(2.5, 4.2),
    offset = c(5.7, 7.8)
  )

  test_crossed_data <- tibble(
    event_ID.1 = 1,
    behav_name.1 = "behav1",
    onset.1 = 2.5,
    offset.1 = 5.7,
    event_ID.2 = 2,
    behav_name.2 = "behav2",
    onset.2 = 4.2,
    offset.2 = 7.8)

  expected_output <- tibble(
    event_ID = c(1, 2),
    behav_name = c("behav1", "behav2"),
    onset = c(2.5, 2.5),
    offset = c(5.7, 7.8),
    duration = offset- onset
  )

  expect_equivalent(
    extend_overlaps(test_behav_data, test_crossed_data),
    expected_output
  )

})

# 11.2
test_that("Single target event extended to encompass two overlapped events", {

  test_behav_data <- tibble(
    event_ID = c(1, 2, 3),
    behav_name = c("behav1", "behav1", "behav2"),
    onset = c(2.5, 6.5, 4.2),
    offset = c(5.7, 9.1, 7.8)
  )

  test_crossed_data <- tibble(
    event_ID.1 = c(1,2),
    behav_name.1 = "behav1",
    onset.1 = c(2.5, 6.5),
    offset.1 = c(5.7, 9.1),
    event_ID.2 = 3,
    behav_name.2 = "behav2",
    onset.2 = 4.2,
    offset.2 = 7.8)

  expected_output <- tibble(
    event_ID = c(1, 2, 3),
    behav_name = c("behav1", "behav1", "behav2"),
    onset = c(2.5, 6.5, 2.5),
    offset = c(5.7, 9.1, 9.1),
    duration = offset- onset
  )

  expect_equivalent(
    extend_overlaps(test_behav_data, test_crossed_data),
    expected_output
  )

})

# 11.3
test_that("Single target event that completely overlaps event not changed", {

  test_behav_data <- tibble(
    event_ID = c(1, 2),
    behav_name = c("behav1", "behav2"),
    onset = c(4.5, 2.5),
    offset = c(5.7, 9.1)
  )

  test_crossed_data <- tibble(
    event_ID.1 = 1,
    behav_name.1 = "behav1",
    onset.1 = 4.5,
    offset.1 = 5.7,
    event_ID.2 = 2,
    behav_name.2 = "behav2",
    onset.2 = 2.4,
    offset.2 = 9.1)

  expected_output <- test_behav_data %>%
    mutate(duration = offset - onset)

  expect_equivalent(
    extend_overlaps(test_behav_data, test_crossed_data),
    expected_output
  )

})

# 11.4
test_that("Multiple target events extended to encompass multiple overlapped events", {


  test_behav_data <- tibble(
    event_ID = c(1, 2, 3, 4),
    behav_name = c("behav1", "behav1", "behav2", "behav2"),
    onset = c(2.5, 16.5, 4.2, 12.7),
    offset = c(5.7, 19.1, 7.8, 16.6)
  )

  test_crossed_data <- tibble(
    event_ID.1 = c(1, 1, 2, 2),
    behav_name.1 = "behav1",
    onset.1 = c(2.5, 2.5, 16.5, 16.5),
    offset.1 = c(5.7, 5.7, 19.1, 19.1),
    event_ID.2 = c(3, 4, 3, 4),
    behav_name.2 = "behav2",
    onset.2 = c(4.2, 12.7, 4.2, 12.7),
    offset.2 = c(7.8, 16.6, 7.8, 16.6))


  expected_output <- tibble(
    event_ID = c(1, 2, 3, 4),
    behav_name = c("behav1", "behav1", "behav2", "behav2"),
    onset = c(2.5, 16.5, 2.5, 12.7),
    offset = c(5.7, 19.1, 7.8, 19.1),
    duration = offset- onset
  )

  expect_equivalent(
    extend_overlaps(test_behav_data, test_crossed_data),
    expected_output
  )

})

