# PCI analysis scripts

## Prerequisites

Install Docker, see [here](https://docs.docker.com/get-docker/) for instructions for different platforms. 
For the initial installation you will need an internet connection.

## Installation

1. Start by downloading the scripts. Either by using:
  - `git clone https://github.com/ubdbra001/PCI_analysis.git` at the terminal/command line (requires `git` installation)
  - Alternatively (if you don't have `git` installed): Go to *Code* > *Download Zip*
2. Once downloaded, extract the PCI_Analysis folder to wherever you want the code to be (if required) and navigate there in the terminal.
3. Run the command `docker compose up -d`, this will start Rstudio server running in the docker container.
  - You may need to include the `sudo` command before the above command
  - If the command runs successfully you will see docker taking several actions to, and the final line when done should read: _Container pci_analysis-rstudio-1  Started_.
4. Navigate to `localhost:8787` in a web browser. It show a circular spinning dots for a few seconds, then you should see the RStudio interface.
  - The project `PCI_analysis` should be open, check the top right hand corner of the window.
  - There should be some text about "Bootstrapping `renv`", the final line should be: _Use `renv::restore()` to install packages recorded in the lockfile._
5. Executing `renv::restore()` in the RStudio console to install the dependencies for the project.
6. Once the dependencies are installed then you are ready to run the script

---

## Usage

Currently there is a single script that runs all the processing steps, this is *PCI_analysis.R*, this will:
1. Extract events from the raw data and produce a summary of these events
2. Save all the individual looks in an RData file
3. Extract the mutual looks from the data
4. Extract all the handling and looking events that overlap with a naming event, and return a detailed list and a count of each type of overlap for each naming event

To run this script, ensure that all the raw data (.txt files) is placed in the *data/raw_data* directory on your host machine (this will automatically be shared with the docker container). Then open and run the PCI_analysis.R script.

### Starting and stopping the container  
- If you have finished analysis for now, but would like to use the container again later without re-running the installation steps above then use the command `docker compose stop`. This will stop the container but preserve the installation state (but possibly not workspace variables).  
- You can go back to the container by running the command `docker compose start`.
- If you are finished with the container and want to delete it, you can use the command `docker compose down`

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
