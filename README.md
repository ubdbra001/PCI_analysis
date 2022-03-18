# PCI analysis scripts

## Prerequisites

### MacOS

1. Install xcode: `xcode-select --install`
2. gfortran compiler may also be needed, install from [here](https://github.com/fxcoudert/gfortran-for-macOS/releases)

## Installation

1. Start by downloading the scripts. The easiest option is to select: *Code* > *Download Zip*
2. Once downloaded extract the PCI_Analysis folder to wherever you want the code to be.
3. Open the folder containing the code and select the PCI_Analysis.RProj file.
   - This should open up the PCI_Analysis project within RStudio
   - You should receive a message about `renv` not being installed, and then RStudio will install it for you and restart the session
4. Once renv is installed you need to get the dependencies for the project, you can do this by executing `renv::restore()` in the RStudio console
5. Once the dependencies are installed then you are ready to run the script

---

## Usage

Currently there is a single script that runs all the processing steps, this is *PCI_analysis.R*, this will:
1. Extract events from the raw data and produce a summary of these events
2. Save all the individual looks in an RData file
3. Extract the mutual looks from the data
4. Extract all the handling and looking events that overlap with a naming event, and return a detailed list and a count of each type of overlap for each naming event

To run this script, ensure that all the raw data (.txt files) is placed in the *data/raw_data* directory. Then open and run the PCI_analysis.R script.


### User specified variables

Specific analysis variables can be altered by the user in the `PCI_uservars.R` file.  

#### Analyses to inlcude:
__Not currently implemented__ These allow specific analyses to be toggled on and off depending on user preference.
- `events_summary`: Provide summary statistics for events specified in `behav_names` variable
- `process_looks`: Process and provide summaries for mutual look events
- `process_naming`: Process and provide summaries for naming events

#### `remove_ambiguous`:
Remove any events coded as ambiguous before any further processing

#### `remove_bookreading`:
Treat bookreading events as offCamera and remove them

#### `frame_gap`:
Specifies the maximum size of frame gap between two proximal events, a gap equal to or less than between two events will result in them being merged

#### Paths:
Specify location of input and output directories
- `raw_data_path`: Location of raw data (in)
- `looks_data_path`: Location of looks data (out) 
- `naming_data_path`: Location of naming data (out)
- `event_data_path`: Location of event data (out)

#### `input_pattern`:
Specify the search pattern for the raw data

#### `partID_regex`:
Regex string for extracting the participant ID


#### `behav_names`:
Defines the behaviours that should be summarised

#### `partial_matching`:
Not 100% sure why I included this one, will double check and update

#### `target_event` & `comp_events`:
Variables that define the target and comparison events for overlapping event analysis

#### `data_col_def`:
Variable that defines the column names and types in the data

---

# IRR Scripts

Use the `PCI_IRR.R` script to run.  
User variables can be found in the `PCI_uservars.R`.  
See `data/IRR` for readme.  