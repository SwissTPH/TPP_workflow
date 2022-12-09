#################################
# Plot the performance of trained GPs and 
# prepare data table for supplement
# 
# created 18.07.2020
# monica.golumbeanu@unibas.ch
#################################
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")

############ Create figure and supplementary data file for GP performance - immediate follow-up ################
gp_perf = vector('list', 11)
gp_perf[[1]] = create_perf_report("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_MAB_once_3years_avg_prev/",
                                  "Anti-infective monoclonal \n(once per year)")
gp_perf[[2]] = create_perf_report("~/MMC/TPP/simulations/MAB_twice_sync_3years/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_MAB_twice_sync_3years/",
                                  "Anti-infective monoclonal \n(twice per year)")
gp_perf[[3]] = create_perf_report("~/MMC/TPP/simulations/cohort_MDA_short_MAB_once/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_short_MAB_once/",
                                  "Anti-infective monoclonal + \nblood stage drug (once per year)")
gp_perf[[4]] = create_perf_report("~/MMC/TPP/simulations/cohort_MDA_short_MAB_twice/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_short_MAB_twice/",
                                  "Anti-infective monoclonal + \nblood stage drug (twice per year)")
gp_perf[[5]] = create_perf_report("~/MMC/TPP/simulations/PEV_once_3years/", 4, 
                              "~/MMC/TPP/simulations/test_sets/test_PEV_once_3years/",
                              "Anti-infective vaccine \n(once per year)")
gp_perf[[6]] = create_perf_report("~/MMC/TPP/simulations/cohort_MDA_short_AIV/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_short_AIV/",
                                  "Anti-infective vaccine + \nblood stage drug (once per year)")
gp_perf[[7]] = create_perf_report("~/MMC/TPP/simulations/TBV_once_3years/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_TBV_once_3years/",
                                  "Transmission-blocking vaccine \n(once per year)")
gp_perf[[8]] = create_perf_report("~/MMC/TPP/simulations/cohort_MDA_short_TBV/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_cohort_MDA_short_TBV/",
                                  "Transmission-blocking vaccine + \nblood stage drug (once per year)")
gp_perf[[9]] = create_perf_report("~/MMC/TPP/simulations/ATSB_once_3years/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_ATSB_once_3years/",
                                  "Attractive targeted sugar baits \n(once per year)")
gp_perf[[10]] = create_perf_report("~/MMC/TPP/simulations/ATSB_twice_sync_3years/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_ATSB_twice_sync_3years/",
                                  "Attractive targeted sugar baits \n(twice per year)")
gp_perf[[11]] = create_perf_report("~/MMC/TPP/simulations/preprandial_once_3years/", 4, 
                                  "~/MMC/TPP/simulations/test_sets/test_preprandial_once_3years/",
                                  "Eave tubes \n(once per year)")

plot_perf = vector('list', 12)
df_perf = NULL
for (i in 1:11) {
    plot_perf[[i]] = gp_perf[[i]]$corr_plot
    df_perf = rbind.data.frame(df_perf, gp_perf[[i]]$perf_df)
}

error_plot = ggplot(df_perf, aes( x = Out_of_sample_ME)) +
    geom_histogram(bins = 25, color = "black", fill = "lightgrey") + 
    theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Mean absolute error (%)", y = "Count") + xlim(0,3)

corr_plot = ggplot(df_perf, aes(x = "", y = Out_of_sample_corr)) +
    geom_boxplot() + 
    theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )+
    labs( x = "", y = expression("r"^{2}))

plot_perf[[12]] = error_plot + annotation_custom(ggplotGrob(corr_plot), xmin = 1.8, xmax = 3.1, ymin = 6, ymax = 45) +
    theme(plot.title = element_text(size=10, face = "bold")) + labs(title = "Performance of trained emulators \nfor all simulated settings")

perf_fig_4 = ggarrange(plotlist = plot_perf, ncol = 3, nrow = 4, labels = LETTERS[seq( from = 1, to = 12 )])
ggsave("~/MMC/TPP/figures/gp_performance/Supp_perf_4_updated.pdf",
       plot = perf_fig_4,  width = 10, height = 10)

a = df_perf %>% group_by(Intervention) %>% summarize(mean_CV_train_corr = mean(CV_train_corr),
                                                     mean_CV_test_err = mean(CV_test_ME),
                                                     mean_corr = mean(Out_of_sample_corr),
                                                     mean_ME = mean(Out_of_sample_ME) )

write.table(df_perf, "~/MMC/TPP/figures/gp_performance/gp_perf_4_updated.txt", quote = FALSE, sep= "\t", col.names = TRUE, row.names = FALSE)


