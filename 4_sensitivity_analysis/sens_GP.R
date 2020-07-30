#############################
# Sensitivity analysis for GP prevalence reduction 
#
# created 28.11.2018
# monica.golumbeanu@unibas.ch
#############################

source("~/MMC/TPP/scripts_v38/3_GP_train/GP_toolbox.R")

args = commandArgs(TRUE)
gp_file = args[1]
ranges_file = args[2]
EIR_fixed_lvl = args[3]
results_folder = args[4]
# print(cv_file)
# print(ranges_file)
# print(EIR_fixed_lvl)
# print(results_folder)

# For testing:
# gp_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
# ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_cont_EIR/param_ranges.RData"
# results_folder = "~/MMC/TPP/simulations/MAB_once_3years_cont_EIR/gp_6/sensitivity/"
# EIR_fixed_lvl = 0.5

exp_name = tools::file_path_sans_ext(basename(gp_file))
sidx_file = paste(results_folder, exp_name, "_EIR_", EIR_fixed_lvl, "_sidx.RData", sep="")

# Load the GP and parameter ranges
gp_result_name = load(gp_file)
gp_result = get(gp_result_name)
load(ranges_file)
param_ranges["EIR",] = c(EIR_fixed_lvl, EIR_fixed_lvl)

# Calculate the Sobol indices
sobol_idx_list = calc_sobol_idx(gp_result$GP_model, param_ranges, num_points = 200000)
sobol_idx_list$EIR = EIR_fixed_lvl
sobol_idx_list$seasonality = gp_result$seasonality
sobol_idx_list$biting_pattern = gp_result$biting_pattern
save(sobol_idx_list, file = sidx_file)

