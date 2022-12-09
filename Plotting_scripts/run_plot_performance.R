##################################
# Run the plotting of GP performance for various models/settings
#
# created 02.12.2019
# monica.golumbeanu@swisstph.ch
##################################

# Load the necessary plotting functions and packages
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")
library(stringr)
library(ggplot2)

# Retrieve the command argments and define function inputs
# args = commandArgs(TRUE)
# sim_dir = args[1]
# test_dir = args[2]
# follow_up = args[3]
# plot_title = args[4]

# For testing:
sim_dir = "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/"
test_dir = "~/MMC/TPP/simulations/test_sets/test_TBV_once_3years/"
follow_up = 4

training_dir = paste0(sim_dir, "gp_", follow_up, "/trained/")
as_dir = paste0(sim_dir, "gp_", follow_up, "/as/")
train_processing_dir = paste0(sim_dir, "postprocessing_", follow_up, "/")
test_processing_dir = paste0(test_dir, "postprocessing_", follow_up, "/")
param_ranges_file = paste0(sim_dir, "param_ranges.RData")
param_table = read.table(paste0(sim_dir, "param_tab.txt"), header = TRUE)
model_pattern = str_remove(param_table$Scenario_Name[1], "_1")
model_name = paste0("seeds_", model_pattern)
plot_dir = paste0("~/MMC/TPP/figures/gp_performance/", model_pattern, "/")

## PLOTS GENERATION
dir.create(plot_dir, showWarnings = FALSE)

test_plot_df = prepare_test_plot_df(as_dir, test_processing_dir, param_ranges_file)
# cv_test_plot_df = prepare_plot_df_cv_test(training_dir, as_dir, test_processing_dir, param_ranges_file, model_name)
# plot_single_corr_box(cv_test_plot_df, "Seasonal", "High_indoor")
#plot_scatter(test_plot_df, plot_dir, paste0("test_", follow_up))
p = plot_single_scatter(test_plot_df, "Seasonal", "High_indoor", "GP emulator performance")
# plot_file = paste(plot_dir, "CV_", follow_up, ".pdf", sep="")
# plot_CV_panel(training_dir, param_ranges_file, plot_file)
