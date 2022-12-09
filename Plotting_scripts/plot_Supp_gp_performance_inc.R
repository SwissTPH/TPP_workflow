#################################################
# plot the performance for incidence reduction
#################################################
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")

plot_cv_corr_box = function(points_df, seas, biting) {
    plot_df = points_df[which(points_df$Seasonality == seas & points_df$Biting_pattern == biting),]
    p = ggplot(plot_df, aes(x = "", y = corr_cv)) + 
        geom_boxplot() +
        theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs( x = "", y = expression("r"^{2}))
    return(p)
}

# Plot for immediate followup
gp_file_name = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained_others/inc_red_seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_MAB_once_3years_avg_prev/postprocessing_4/other_outputs/seeds_MAB_once_3_years_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_4 = prepare_test_plot_df_single(gp_file_name, test_file_name, ranges_file)
cv_test_plot_df_4 = prepare_plot_df_cv_single(gp_file_name, test_file_name, ranges_file)
p_scatter_4 = plot_single_scatter(df_4, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, immediate follow-up") 
p_r_distr_4 = plot_cv_corr_box(cv_test_plot_df_4, "Seasonal", "High_indoor") 
p_perf_4 = p_scatter_4 + annotation_custom(ggplotGrob(p_r_distr_4), xmin = 55, xmax = 100, ymin = -17, ymax = 46)

# Plot for late followup
gp_file_name = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_6/trained_others/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
test_file_name = "~/MMC/TPP/simulations/test_sets/test_MAB_once_3years_avg_prev/postprocessing_6/other_outputs/seeds_MAB_once_3_years_seasonal_High_indoor.txt"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
df_6 = prepare_test_plot_df_single(gp_file_name, test_file_name, ranges_file)
cv_test_plot_df_6 = prepare_plot_df_cv_single(gp_file_name, test_file_name, ranges_file)
p_scatter_6 = plot_single_scatter(df_6, "Seasonal", "High_indoor", "Anti-infective monoclonal\nonce per year, late follow-up") 
p_r_distr_6 = plot_cv_corr_box(cv_test_plot_df_6, "Seasonal", "High_indoor") 
p_perf_6 = p_scatter_6 + annotation_custom(ggplotGrob(p_r_distr_6), xmin = 55, xmax = 100, ymin = -17, ymax = 46) 

perf_incidence = ggarrange(p_perf_4, p_perf_6, ncol = 2, nrow = 1, labels = c("A", "B"))
ggsave("~/MMC/TPP/figures/gp_performance/Supp_perf_incidence.pdf",
       plot = perf_incidence,  width = 10, height = 4)



