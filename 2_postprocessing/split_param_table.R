###########################
# Script to split a parameter table by setting.
# A setting in this case is defined by EIR, seasonality and biting pattern.
# Each split is written to a separate .txt file.
#
# INPUTS: 
#       param_table: full path of the .txt file containing the parameters for each scenario
#       destination_folder: folder where all the split files will be saved
#
# OUTPUTS:
#   For each setting, a .txt file containing the parameters for the scenarios corresponding to that setting
# 
# created 14.05.2019
# monica.golumbeanu@unibas.ch
########################## 

args = commandArgs(TRUE)
param_table = args[1]
destination_folder = args[2]

# Read the parameter table
param_tab = read.table(param_table, sep= "\t", header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)

# Label to be added in front of the names of splits
experiment_name = substr(param_tab$Scenario_Name[1], 1, nchar(as.character(param_tab$Scenario_Name[1])) - 2)

# Create destination folder if it doesn't exist
if(!dir.exists(destination_folder)) {
    dir.create(destination_folder)
}

# Split the parameter table by seasonality and biting pattern
list_tab_splits = split(param_tab, paste(param_tab$Seasonality, param_tab$Biting_pattern, sep="_"))

# Write each split to a file named according to the corresponding EIR and seasonality
for (i in 1:length(list_tab_splits)) {
    final_table_dest = paste(destination_folder, experiment_name, "_",
                             names(list_tab_splits)[i], ".txt", sep="")
    write.table(list_tab_splits[[i]], final_table_dest, sep="\t",
                col.names = TRUE, row.names = FALSE, quote=FALSE)
}
