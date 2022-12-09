#######################
# plot the percent of simulations reaching stochastic elimination
#
# monica.golumbeanu@unibas.ch
# created 17.03.2020
#######################
source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")

list_folders = c("~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_PEV_TBV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_TBV_PEV_MDA_once_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_PEV_TBV_once_3years/",
                 "~/MMC/TPP/simulations/cohort_TBV_PEV_MDA_once_3years/",
                 "~/MMC/TPP/simulations/MAB_twice_sync_3years/",
                 "~/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/",
                 "~/MMC/TPP/simulations/ATSB_twice_sync_3years/")

list_names = c("MDA + Passive anti-infective", "MDA + Active anti-infective", 
               "MDA + Active transmission-blocking", 
               "Active anti-infective + Active transmission-blocking",
               "MDA + Active transmission-blocking + Active anti-infective",
               "MDA + Passive anti-infective", "MDA + Active anti-infective", 
               "MDA + Active transmission-blocking", 
               "Active anti-infective + Active transmission-blocking",
               "MDA + Active transmission-blocking + Active anti-infective",
               "Passive anti-infective twice/year", 
               "MDA + Passive anti-infective twice/year", 
               "Pre- and postprandial killing twice/year")

final_plot_df = NULL
for (i in 1:length(list_folders)) {
    sim_folder = list_folders[i]
    processed_folder = paste0(sim_folder, paste0("postprocessing_", 4,"/"))
    plot_df = NULL
    # Plot the distribution of prevalence across transmission settings
    processing_files = list.files(processed_folder, pattern = "seeds_", full.names = TRUE)
    for(p_file in processing_files) {
        OM_result = read.table(p_file, sep="\t", header = TRUE, as.is = TRUE)
        plot_df = rbind.data.frame(plot_df, OM_result[, c("Seasonality", "Biting_pattern", "EIR", "initial_prev")])
    }
    plot_df$EIR = floor(plot_df$EIR) - (plot_df$EIR == 25)
    plot_df$Seasonality = factor(str_to_title(plot_df$Seasonality), levels = c("Perennial", "Seasonal"))
    plot_df$Biting_pattern = paste(str_replace(plot_df$Biting_pattern, "_", " "), "biting")
    plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    plot_df$Experiment = list_names[i]
    final_plot_df = rbind.data.frame(final_plot_df, plot_df)
}


stats_table = final_plot_df %>% group_by(Experiment, Seasonality, Biting_pattern, EIR) %>% 
    dplyr::summarise(Sample_size_EIR = length(initial_prev), 
                     Proportion_extinct = length(which(initial_prev == 0))/length(initial_prev)) %>%
    dplyr::mutate(Total_sample_size = sum(Sample_size_EIR))

write.table(stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/stochastic_extinction.txt", quote = FALSE, 
            sep="\t", col.names = TRUE, row.names = FALSE)

stats_table = stats_table[which(stats_table$Proportion_extinct>0), ]
p = ggplot(stats_table, aes(x=as.factor(EIR), y=Proportion_extinct*100)) + 
    geom_violin(color = "grey") +
    geom_boxplot(position=position_dodge(width = 0.9), color = "#E69F00", width=.2) +
    #scale_x_discrete(breaks = c(2, 8, 14, 22), labels = c("1-2", "6-8", "12-14", "20-22")) +
    theme_bw(base_size=13) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~Seasonality + Biting_pattern) + labs( x = expression(paste("Input EIR (Corresponding Median ", italic("Pf"), "PR"["2-10"], " %)")), 
                                                      y = "Simulations reaching elimination \n before tool deployment (%)") +
    theme(panel.background = element_rect(fill = 'white')) + 
    theme(strip.background = element_rect(colour="white", fill="white")) +
    theme(legend.position="top") 
ggsave("~/MMC/TPP/figures/simulation_desc/prev_red/stochastic_extinction.pdf",
       plot = p, width = 7, height = 3.5)



