#################################
# Plot performance at boundaries
#
# monica.golumbeanu@unibas.ch
#################################
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")

# immediate follow-up
gp_cv_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_boundaries_MAB_once/postprocessing_4/seeds_MAB_once_3_years_boundaries_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_4 = prepare_test_plot_df_single_extended(gp_cv_file, test_file_name, ranges_file)
p_scatter_4 = plot_single_scatter(df_4, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 


# long follow-up
gp_cv_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_6/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_boundaries_MAB_once/postprocessing_6/seeds_MAB_once_3_years_boundaries_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_6 = prepare_test_plot_df_single_extended(gp_cv_file, test_file_name, ranges_file)
p_scatter_4 = plot_single_scatter(df_4, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 




gp_cv_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_semi_boundaries_MAB_once/postprocessing_4/seeds_MAB_once_3_years_boundaries_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_4 = prepare_test_plot_df_single_extended(gp_cv_file, test_file_name, ranges_file)
p_scatter_4 = plot_single_scatter(df_4, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 



gp_cv_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_6/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_semi_boundaries_MAB_once/postprocessing_6/seeds_MAB_once_3_years_boundaries_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_6 = prepare_test_plot_df_single_extended(gp_cv_file, test_file_name, ranges_file)
p_scatter_6 = plot_single_scatter(df_6, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 

pts = df_4[which(df_4$abs_error>=10),]

gp_as_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/as/seeds_MAB_once_3_years_seasonal_High_indoor_cv_as.RData"
df_4 = pprepare_test_plot_df_single_extended(gp_as_file, test_file_name, ranges_file)
p_scatter_4b = plot_single_scatter(df_4, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 
