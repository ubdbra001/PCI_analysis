library(readr)

## PCI only

# Analysis to include:
event_summary <- TRUE    # Provide summary stats for all event
process_looks <- TRUE    # Process mutual looks inc summaries
process_naming <- FALSE   # Process naming overlaps inc summaries

# Ignore events marked as ambiguous:
remove_ambiguous <- FALSE

remove_bookreading <- TRUE

# Size of gap for event merging
frame_gap <- 2

# Data input and output paths
raw_data_path <- "data/raw_data"
looks_data_path <- "data/looks_data"
naming_data_path <- "data/naming_data"
event_data_path <- "data/event_data"

input_pattern <- ".*.txt"

partID_regex <- "(?<=/)[:alnum:]+_[:upper:]*[:digit:]+"

# Variables for data extraction
behav_names <- c(
  "PCIduration",
  "bookreading",
  "offCameras",
  "babyATparentface",
  "parentATbabyface",
  "babyobj",
  "parentobj",
  "parentnoun",
  "babyATobj",
  "parentATobj")

partial_matching <- c(T, T, T, T, T, F, F, T, T, T)

target_event <- "parentnoun"
comp_events <- c("babyobj", "parentobj", "babyATobj", "parentATobj")

### IRR only

# Minimum length of difference (anything lower will be ignored)
min_diff_length <- 0


# Column indicating extent of data to include
data_filt <- "PCIduration.ordinal"

# set directories
data_dir <- "data/IRR"
coder1_dir <- file.path(data_dir, "coder1")
coder2_dir <- file.path(data_dir, "coder2")
diff_dir <- file.path(data_dir, "differences")


skip_mess <- c("\n%s skipped, please check that there is one, ",
               "and only one, file per coder for this participant\n")
pattern_template <- ".*%s.*.txt"



data_col_def <- cols(
  time = col_double(),
  PCIduration.ordinal = col_double(),
  PCIduration.onset = col_double(),
  PCIduration.offset = col_double(),
  PCIduration.y = col_character(),
  offCameras.ordinal = col_double(),
  offCameras.onset = col_double(),
  offCameras.offset = col_double(),
  offCameras.y = col_character(),
  bookreading.ordinal = col_double(),
  bookreading.onset = col_double(),
  bookreading.offset = col_double(),
  bookreading.y = col_character(),
  babyATparentface.ordinal = col_double(),
  babyATparentface.onset = col_double(),
  babyATparentface.offset = col_double(),
  babyATparentface.y = col_character(),
  babyATparentface.a = col_character(),
  parentATbabyface.ordinal = col_double(),
  parentATbabyface.onset = col_double(),
  parentATbabyface.offset = col_double(),
  parentATbabyface.y = col_character(),
  parentATbabyface.a = col_character(),
  babyobj.ordinal = col_double(),
  babyobj.onset = col_double(),
  babyobj.offset = col_double(),
  babyobj.obj1 = col_character(),
  babyobj.obj2 = col_character(),
  babyobj.obj3 = col_character(),
  babyobj.obj4 = col_character(),
  babyobj.obj5 = col_character(),
  babyobjuncodable.ordinal = col_double(),
  babyobjuncodable.onset = col_double(),
  babyobjuncodable.offset = col_double(),
  babyobjuncodable.y = col_character(),
  babyactions.ordinal = col_double(),
  babyactions.onset = col_double(),
  babyactions.offset = col_double(),
  babyactions.action = col_character(),
  parentobj.ordinal = col_double(),
  parentobj.onset = col_double(),
  parentobj.offset = col_double(),
  parentobj.obj1 = col_character(),
  parentobj.obj2 = col_character(),
  parentobj.obj3 = col_character(),
  parentobj.obj4 = col_character(),
  parentobj.obj5 = col_character(),
  parentobjuncodable.ordinal = col_double(),
  parentobjuncodable.onset = col_double(),
  parentobjuncodable.offset = col_double(),
  parentobjuncodable.y = col_character(),
  parentactions.ordinal = col_double(),
  parentactions.onset = col_double(),
  parentactions.offset = col_double(),
  parentactions.action = col_character(),
  parentutter.ordinal = col_double(),
  parentutter.onset = col_double(),
  parentutter.offset = col_double(),
  parentutter.utterance = col_character(),
  parentnoun.ordinal = col_double(),
  parentnoun.onset = col_double(),
  parentnoun.offset = col_double(),
  parentnoun.label = col_character(),
  lookingATobj_parentnoun.ordinal = col_double(),
  lookingATobj_parentnoun.onset = col_double(),
  lookingATobj_parentnoun.offset = col_double(),
  lookingATobj_parentnoun.label = col_character(),
  lookingATobj_parentnoun.referent1 = col_character(),
  lookingATobj_parentnoun.referent2 = col_character(),
  lookingATobj_parentnoun.babylooking = col_character(),
  lookingATobj_parentnoun.parentlooking = col_character(),
  babyATobj_parentnoun.ordinal = col_double(),
  babyATobj_parentnoun.onset = col_double(),
  babyATobj_parentnoun.offset = col_double(),
  babyATobj_parentnoun.referent1 = col_character(),
  babyATobj_parentnoun.referent2 = col_character(),
  babyATobj_parentnoun.babylooking = col_character(),
  babyATobj_parentnoun.y = col_character(),
  babyATobj_parentnoun.a = col_character(),
  parentATobj_parentnoun.ordinal = col_double(),
  parentATobj_parentnoun.onset = col_double(),
  parentATobj_parentnoun.offset = col_double(),
  parentATobj_parentnoun.referent1 = col_character(),
  parentATobj_parentnoun.referent2 = col_character(),
  parentATobj_parentnoun.parentlooking = col_character(),
  parentATobj_parentnoun.y = col_character(),
  parentATobj_parentnoun.a = col_character(),
  .default = col_skip()
)
