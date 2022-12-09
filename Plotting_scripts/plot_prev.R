##################################
# Data descriptive plots
#
# created 19.02.2020
# monica.golumbeanu@unibas.ch
#################################

sim_folder = "/scicore/home/smith/golmon00/MMC/TPP/simulations/MAB_twice_3years/"
processed_folder = paste0(sim_folder, "postprocessing_4/")

# Plot the distribution of prevalence across transmission settings
processing_files = list.files(processed_folder, pattern = "seeds_", full.names = TRUE)

plot_df = NULL
for(p_file in processing_files) {
    OM_result = read.table(p_file, sep="\t", header = TRUE, as.is = TRUE)
    plot_df = rbind.data.frame(plot_df, OM_result[, c("Seasonality", "Biting_pattern", "EIR", "initial_prev", "initial_prev_2_10")])
}
# plot_df$EIR = 2*(floor(plot_df$EIR) %/% 2) + 1 # for 1, 3, 5, 7, ...
plot_df$EIR = floor(plot_df$EIR) - (plot_df$EIR == 25) # for 1, 2, 3, 4, ...

# Remove the simulations where the system reaches elimination
if(length(which(plot_df$initial_prev == 0))>0) {
    plot_df = plot_df[-which(plot_df$initial_prev == 0),]
}

plot_df$Seasonality = factor(str_to_title(plot_df$Seasonality), levels = c("Perennial", "Seasonal"))
plot_df$Biting_pattern = paste(str_replace(plot_df$Biting_pattern, "_", " "), "biting")
plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))

# Extract the statistics on prevalence reduction
stats_table = plot_df %>% group_by(Seasonality, Biting_pattern, EIR) %>% 
    dplyr::summarise(Median_0_99 = median(initial_prev), 
                     Conf_95_low_0_99 = boxplot.stats(initial_prev)$conf[1], 
                     Conf_95_high_0_99 = boxplot.stats(initial_prev)$conf[2],
                     Median_2_10 = median(initial_prev_2_10), 
                     Conf_95_low_2_10 = boxplot.stats(initial_prev_2_10)$conf[1],
                     Conf_95_high_2_10 = boxplot.stats(initial_prev_2_10)$conf[2])

ex_table = plot_df[,c("Seasonality", "Biting_pattern", "EIR", "initial_prev", "initial_prev_2_10")]
ex_table_m = melt(as.data.frame(ex_table), id=c("Seasonality", "Biting_pattern", "EIR"))

ggplot(ex_table_m, aes(x=as.factor(EIR), y=value*100, color = variable)) + 
    geom_boxplot(outlier.size = 0.5) + #scale_x_discrete(breaks = c(2, 10, 18), labels = c("2:4", "10:12", "18:20")) +
    theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    facet_wrap(~Seasonality + Biting_pattern) + labs( x = "Input EIR level", y = expression(italic("Pf")~"PR"~"(%)")) +
    scale_x_discrete(breaks=c(1, seq(5,25,5)), labels=c(1, seq(5,25,5))) +
    theme(panel.background = element_rect(fill = 'white')) + 
    theme(strip.background = element_rect(colour="white", fill="white")) +
    theme(legend.position="top") +
    scale_color_manual(values=c("#56B4E9", "#E69F00"), 
                      name="",
                      breaks=c("initial_prev", "initial_prev_2_10"),
                      labels=c("Age 0-99", "Age 2-10")) + ggtitle(expression(paste("True simulated ", italic("Pf"), "PR")))
    #theme(axis.text.x = element_text(size=8, angle=45))
ggsave(filename = "~/MMC/TPP/figures/simulation_desc/EIR_prevalence.pdf",
       width = 8, height = 4.5)
write.table(stats_table, "~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", quote = FALSE,
            sep="\t", col.names = TRUE, row.names = FALSE)


    