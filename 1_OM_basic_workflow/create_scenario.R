########################################
# script generate_RCD_files_scenario.R - given parameter names and values for a scenario, 
#                               generates a base .xml file and a file containing 
#                               replacement patterns for parameters using the sed command
#
# COMMAND:
#   Rscript create_scenario.R --column_names $column_names --params $param_line 
#           --base_folder $BASE_FOLDER --scenario_folder $SCENARIO_FOLDER 
#           --scaffold_file $SCAFFOLD_FILE
# INPUTS:
#   column_names = space-separated column names (parameter names)
#   param_line = space-separated parameter values
#   BASE_FOLDER = destination folder where replacement specifications are saved
#   SCENARIO_FOLDER = destination folder where scenario xml file is saved
#   SCAFFOLD_FILE = path of the file containing special formatting for varied parameters
#
# OUTPUTS: two files created in the SCENARIO_FOLDER
#	- file containing parameter replacement specifications to be provided to the command sed
#   - scenario xml file
#
# created 04.04.2019
# monica.golumbeanu@unibas.ch
########################################
library(rapportools)

# # If reading the inputs from command line:
args = commandArgs(trailingOnly = TRUE)
hh = paste(unlist(args), collapse = ' ')
listoptions = unlist(strsplit(hh,'--'))[-1]
options.args = sapply(listoptions,function(x){ unlist(strsplit(x, ' '))[-1] }, simplify = FALSE)
options.names = sapply(listoptions,function(x){ option =  unlist(strsplit(x, ' '))[1] })
names(options.args) = unlist(options.names)

# Process the inputs
base_folder = options.args$base_folder
scenario_folder = options.args$scenario_folder
scaffold_file = options.args$scaffold_file
param_table = as.data.frame(t(options.args$params), stringsAsFactors=FALSE)
colnames(param_table) = options.args$column_names
param_names = options.args$column_names

# Check if all inputs are ok
if(is.empty(base_folder) || is.empty(scaffold_file) ||
   is.empty(options.args$params) || is.empty(options.args$column_names)){
    print("ERROR: Some of the inputs to script create_scenario.R are not provided.")
    quit()
}

# Create the destination folders if they do not already exist
if(!dir.exists(base_folder)) {
    print("Destination folder for base xml created")
    dir.create(base_folder)
}
if(!dir.exists(scenario_folder)) {
    print("Destination folder for scenario xml created")
    dir.create(scenario_folder)
}

# Build the file with replacement patterns for parameters
scenario_sed_file = paste(base_folder, param_table$Scenario_Name, "_",
                          param_table$SEED, "_patterns.txt", sep="")
sed_patterns_vec = paste("s/@", param_names[2:length(param_names)] ,"@/",
                         param_table[1,2:length(param_names)],"/g;", sep="")
write.table(sed_patterns_vec, file = scenario_sed_file, quote = FALSE,
            col.names = FALSE, row.names = FALSE)

# Build the scenario xml file
scenario_file = paste(scenario_folder, param_table$Scenario_Name, "_",
                    param_table$SEED, ".xml", sep="")
print(paste("Creating", scenario_file, "..."))
system(paste("sed -f", scenario_sed_file, scaffold_file, ">", scenario_file))
