# PCI analysis scripts

## Installation

1. Start by downloading the scripts. The easiest option is to select: *Code* > *Download Zip*
2. Once downloaded extract the PCI_Analysis folder to wherever you want the code to be.
3. Open the folde containing the code and select the PCI_Analysis.RProj file
   - This should open up the PCI_Analysis project within RStudio
   - You should receive a message about packrat not being installed, and then RStudio will install it for you and restart the session
4. Once Packrat is installed you need to get the dependencies for the project, you can do this by executing `packrat::restore()` in the RStudio console
5. Once the dependencies are installed then you are ready to run the script

---

## Usage

Currently there is a single script that runs all the processing steps, this is *PCI_analysis.R*, this will:
1. Extract events from the raw data and produce a summary of these events
2. Extract the mutual looks from the data
3. Extract all the handling and looking events that overlap with a naming event, and return a detailed list and a count of each type of overalp for each naming event

To run this script, ensure that all the raw data is placed in the *data/raw_data* directory. Then open and run the PCI_analysis.R script.
