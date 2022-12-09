##################################
# Run the plotting for GP performance on subsamples
#
# created 30.01.2020
# monica.golumbeanu@swisstph.ch
##################################

# Load the necessary plotting functions and packages
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")
library(stringr)
library(ggplot2)

sim_dir = "~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/"
test_dir = "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_MAB_once_3years/"
plot_dir = "~/MMC/TPP/figures/simulation_desc/subsample_performance/"
follow_up = 6

training_dir = paste0(sim_dir, "gp_", follow_up, "/trained_subsamples/")
test_processing_dir = paste0(test_dir, "postprocessing_", follow_up, "/")
param_ranges_file = paste0(sim_dir, "param_ranges.RData")
param_table = read.table(paste0(sim_dir, "param_tab.txt"), header = TRUE)
model_pattern = str_remove(param_table$Scenario_Name[1], "_1")
model_name = paste0("seeds_", model_pattern)

test_plot_df = prepare_test_plot_df_subsample(training_dir, test_processing_dir, param_ranges_file)
p1 = plot_subsample_performance(test_plot_df, plot_dir, paste0(model_pattern, "_", follow_up))
# plot_file = paste0(plot_dir, "scatter_", exp_name, ".pdf")
# ggsave(plot_file, plot = p, width = 8, height = 5)
# plot_single_scatter(test_plot_df, "Seasonal", "Mid_indoor", plot_dir, exp_name, plot_title)

