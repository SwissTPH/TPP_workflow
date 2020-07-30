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

# Path of the file where the parameter table will be saved
table_file = "~/MMC/TPP/simulations/examples/ATSB/param_tab.txt"
ranges_file = "~/MMC/TPP/simulations/examples/ATSB/param_ranges.RData"

# Seasonality and biting patterns values
tanzania_s = read.table("~/MMC/resource_files/Tanzania_seasons.txt", sep="\t", header = TRUE)
biting_patterns = read.table("~/MMC/resource_files/biting.txt", sep="\t", header = TRUE)

# Name of the experiment and parameters
experiment = "ATSB_once_3_years"
EIR = c(1, 25)
Halflife = c(0.167, 0.667)
Efficacy = c(0.7, 0.99)
Coverage = c(0, 1)
Access = c(0, 0.5)
param_ranges = rbind(EIR, Halflife, Efficacy, Coverage, Access)
save(param_ranges, file = ranges_file)

Xcand = as.data.frame(lhs(5, param_ranges))
colnames(Xcand) = rownames(param_ranges)
SEED = c(1:2)

# Table with the parameter values
param_tab = Reduce(merge, list(tanzania_s, biting_patterns,
                               as.data.frame(Xcand)))


scenarios_names = paste(experiment, 1:nrow(param_tab), sep="_")
param_tab = cbind(scenarios_names, param_tab)
colnames(param_tab)[1] = "Scenario_Name"

# Add seed column at the beginning
param_tab = merge(param_tab, as.data.frame(SEED))

# Write table to specified destination file
write.table(param_tab, table_file, sep = "\t", quote = FALSE, col.names = TRUE,
            row.names = FALSE)


