###########################
# Test implementation of MDA with other interventions
# monica.golumbeanu@unibas.ch
# created 29.06.2020
###########################
library(dplyr)
library(ggplot2)
sim_folder = "~/MMC/TPP/simulations/vaccine_test/simulations/"
files_list = dir(sim_folder, full.names = TRUE, pattern = "_out.txt")
int_plot_df = NULL

for (i in 1:length(files_list)) {
    sim_file = files_list[i]
    om_result = read.table(sim_file, sep="\t")
    colnames(om_result) = c("time", "age_group", "measure", "value")
    n_cases = as.data.frame(om_result[om_result$measure == 1,])
    
    # Summarize results by summing up over age groups
    om_result$age_group=NULL
    agg_n_cases = n_cases %>% group_by(time, measure) %>% summarise(val = sum(value)/10000)
    summary_plot_df = cbind.data.frame(agg_n_cases$time, agg_n_cases$val, basename(sim_file))
    colnames(summary_plot_df) = c("time", "prev", "experiment")
    int_plot_df = rbind(int_plot_df, summary_plot_df)
}

# int_plot_df = NULL
# sim_file = "/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/om/cohort_MDA_PEV_once_3_years_999_8_out.txt"
# om_result = read.table(sim_file, sep="\t")
# colnames(om_result) = c("time", "age_group", "measure", "value")
# n_cases = as.data.frame(om_result[om_result$measure == 1,])
# om_result$age_group=NULL
# agg_n_cases = n_cases %>% group_by(time, measure) %>% summarise(val = sum(value)/10000)
# summary_plot_df = cbind.data.frame(agg_n_cases$time, agg_n_cases$val, basename(sim_file))
# colnames(summary_plot_df) = c("time", "prev", "experiment")
# int_plot_df = rbind(int_plot_df, summary_plot_df)

# # Summarize results by summing up over age groups
# om_result$age_group=NULL
# agg_n_cases = n_cases %>% group_by(time, measure) %>% summarise(val = sum(value)/10000)
# summary_plot_df = cbind.data.frame(agg_n_cases$time, agg_n_cases$val, basename(sim_file))
# colnames(summary_plot_df) = c("time", "prev", "experiment")
# int_plot_df = rbind(int_plot_df, summary_plot_df)

p = ggplot(int_plot_df) + 
    theme_bw(base_size=15) +
    geom_rect(mapping=aes(xmin = 4, xmax = 5, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_rect(mapping=aes(xmin = 8, xmax = 9, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_rect(mapping=aes(xmin = 10, xmax = 11, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_line(aes(x = time/73, y=prev, color = experiment), lwd = 1) + theme_bw(base_size=13) + 
    geom_vline(xintercept = 5.4167, linetype="dashed", size = 0.3) +
    geom_vline(xintercept = 6.4167, linetype="dashed", size = 0.3) +
    geom_vline(xintercept = 7.4167, linetype="dashed", size = 0.3) +
    scale_x_continuous(breaks=c(0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15), expand = c(0.001, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab("Time (years)") + ylab("Prevalence") + 
    # scale_fill_manual(name = "Applied\ninterventions", values = intervention_colors) +
    # scale_color_manual(name = "Applied\ninterventions", values = intervention_colors) +
    #scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2") + 
    labs(color = "Intervention target", fill = "Intervention target") +
    annotate(geom="text", x=4.5, y=0.85, label="reference\nyear", color="black") + 
    annotate(geom="text", x=8.5, y=0.85, label="immediate\nfollow-up", color="black") +
    annotate(geom="text", x=10.5, y=0.85, label="late\nfollow-up", color="black") 
