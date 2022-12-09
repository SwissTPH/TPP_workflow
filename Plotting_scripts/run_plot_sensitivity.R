##########################
# call the plotting functions for sensitivity analysis results
#
# created 06.12.2019
# monica.golumbeanu@swisstph.ch
##########################

# Load the necessary plotting functions
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_sensitivity.R")
library(stringr)

# Retrieve the command argments and define function inputs
# args = commandArgs(TRUE)
# sim_dir = args[1]
# follow_up = args[2]
# plot_title = args[3]

# For testing
sim_dir = "~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/"
follow_up = 4

sens_dir = paste0(sim_dir, "gp_", follow_up, "/sensitivity/")
param_ranges_file = paste0(sim_dir, "param_ranges.RData")
param_table = read.table(paste0(sim_dir, "param_tab.txt"), header = TRUE)
model_pattern = str_remove(param_table$Scenario_Name[1], "_1")
plot_dir = paste0("~/MMC/TPP/figures/sensitivity_analysis/", model_pattern, "2/")

dir.create(plot_dir, showWarnings = FALSE)

# Plot the sensitivity analysis for all the 6 models
plot_sens_GP(sens_dir, plot_dir, param_ranges_file, paste0(model_pattern, "_", follow_up))

# Plot only one figure
# plot_single_sens_GP(sens_dir, "Seasonal", "Low_indoor", plot_dir, "main_effects", "area",
#                                param_ranges_file, paste0(model_pattern, "_", follow_up), "Impact determinants") 



