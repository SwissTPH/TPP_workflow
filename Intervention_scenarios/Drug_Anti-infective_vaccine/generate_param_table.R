########################################
# script generate_experiment_param_table.R
#
# construct a table with parameter values for each scenario of an OM experiment
#
# INPUTS:
#   table_file - name of the file where the table should be saved
#   experiment - name of the experiment
#
# OUTPUTS:
#	- a csv table with all the parameters for OM experiment

# created 21.01.2018
# monica.golumbeanu@unibas.ch
########################################
library(tgp)

# Training and test folders
training_folder = "~/MMC/TPP/simulations/cohort_MDA_short_AIV/"
test_folder = "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_short_AIV/"

if(!dir.exists(test_folder)) {
    dir.create(test_folder)
}

# Path of the file where the parameter table will be saved
table_file_training = paste0(training_folder, "param_tab.txt")
ranges_file_training = paste0(training_folder, "param_ranges.RData")
table_file_test = paste0(test_folder, "param_tab.txt")
ranges_file_test = paste0(test_folder, "param_ranges.RData")

# Seasonality and biting patterns values
tanzania_s = read.table("~/MMC/resource_files/Tanzania_seasons.txt", sep="\t", header = TRUE)
biting_patterns = read.table("~/MMC/resource_files/biting.txt", sep="\t", header = TRUE)

# Name of the experiment and parameters
experiment = "cohort_MDA_short_AIV"
EIR = c(1, 25)
Drug_halflife = c(0.014, 0.027)
Drug_efficacy = c(0.8, 1)
Halflife = c(0.5, 5)
Efficacy = c(0.3, 0.95)
Coverage = c(0, 1)
Access = c(0, 0.5)

# Parameter ranges
param_ranges = rbind(EIR, Drug_halflife, Drug_efficacy, Halflife, Efficacy, Coverage, Access)

# Uniform samples from the parameter space
set_length = 750
train_size = 500
Xcand = as.data.frame(lhs(set_length, param_ranges))
colnames(Xcand) = rownames(param_ranges)
Xcand_training = Xcand[1:train_size, ]
Xcand_test = Xcand[(train_size + 1):(set_length), ]

SEED = c(1:10)

# Table with the parameter values for training and testing
param_tab_training = Reduce(merge, list(tanzania_s, biting_patterns,
                               as.data.frame(Xcand_training)))
scenarios_names_training = paste(experiment, 1:nrow(param_tab_training), sep="_")
param_tab_training = cbind(scenarios_names_training, param_tab_training)
colnames(param_tab_training)[1] = "Scenario_Name"

param_tab_test = Reduce(merge, list(tanzania_s, biting_patterns,
                                        as.data.frame(Xcand_test)))
scenarios_names_test = paste(experiment, 1:nrow(param_tab_test), sep="_")
param_tab_test = cbind(scenarios_names_test, param_tab_test)
colnames(param_tab_test)[1] = "Scenario_Name"

# Add seed column at the beginning
param_tab_training = merge(param_tab_training, as.data.frame(SEED))
param_tab_test = merge(param_tab_test, as.data.frame(SEED))

# Save parameter ranges and table to training and test locations
save(param_ranges, file = ranges_file_training)
save(param_ranges, file = ranges_file_test)
write.table(param_tab_training, table_file_training, sep = "\t", quote = FALSE, col.names = TRUE,
            row.names = FALSE)
write.table(param_tab_test, table_file_test, sep = "\t", quote = FALSE, col.names = TRUE,
            row.names = FALSE)

