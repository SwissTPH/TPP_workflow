#################################
# Plot the seasonality pattern
#
#
# created 03.10.2019
# monica.golumbeanu@unibas.ch
################################

library(data.table)
library(stringr)

tanzania_s = read.table("~/MMC/resource_files/Tanzania_seasons.txt", sep="\t", header = TRUE)
plot_df = as.data.frame(t(tanzania_s[which(tanzania_s$Seasonality == "seasonal"), 2:ncol(tanzania_s)]))
colnames(plot_df) = "Seasonality"
plot_df$Month = c(1:12)
plot_df$Seasonality = plot_df$Seasonality/sum(plot_df$Seasonality)
plot_file = "~/MMC/TPP/figures/simulation_desc/seasonality2.pdf"

sim_folder = "~/MMC/TPP/simulations/TBV_once_3years/om/"
param_tab = read.table("~/MMC/TPP/simulations/TBV_once_3years/param_tab.txt", header = TRUE, stringsAsFactors = FALSE, as.is = TRUE)
file_pattern = "TBV_once_3_years_1000_"
exp_list = param_tab[which(param_tab$Scenario_Name == "TBV_once_3_years_1000"),]
# 7.777597
files_list = dir(sim_folder, pattern = file_pattern, full.names = TRUE)
out_files = files_list[which(str_detect(files_list, "_out.txt"))]
cts_files = files_list[which(str_detect(files_list, "_cts.txt"))]
# code for the number of cases
measure_code = 1

final_plot_df = NULL
for (file_n in c(1:length(out_files))) {
    # Read the simulation output and extract the time series with the number of cases
    om_out_result = read.table(out_files[file_n], sep="\t")
    colnames(om_out_result) = c("time", "age_group", "measure", "value")
    n_cases = as.data.frame(om_out_result[om_out_result$measure==measure_code,])
    # Summarize results by summing up over age groups
    om_out_result$age_group=NULL
    agg_n_cases = n_cases %>% group_by(time, measure) %>% dplyr::summarise(prevalence = sum(value)/10000)
    tab_df = cbind.data.frame(file_n, agg_n_cases$time[1:73], "prevalence", agg_n_cases$prevalence[1:73])#/sum(agg_n_cases$prevalence[1:73]))
    colnames(tab_df) = c("replicate", "time", "variable", "value")
    final_plot_df = rbind.data.frame(final_plot_df, tab_df, stringsAsFactors = FALSE)
    
    # Extract the input and simulated EIR
    om_cts_result = read.table(cts_files[file_n], header = TRUE, sep="\t")
    colnames(om_cts_result) = c("time", "input_EIR", "simulated_EIR")
    om_cts_result = om_cts_result[2:74, ]
    om_cts_result$input_EIR = om_cts_result$input_EIR#/sum(om_cts_result$input_EIR)
    om_cts_result$simulated_EIR = om_cts_result$simulated_EIR#/sum(om_cts_result$simulated_EIR)
    om_cts_result_m = reshape2::melt(om_cts_result, id = c("time"))
    om_cts_result_m = cbind.data.frame(file_n, om_cts_result_m)
    colnames(om_cts_result_m) = c("replicate", "time", "variable", "value")
    final_plot_df = rbind.data.frame(final_plot_df, om_cts_result_m, stringsAsFactors = FALSE)
}

# Plot the input EIR, simulated EIR and prevalence
final_plot_df$time = final_plot_df$time*5
final_plot_df$variable = plyr::mapvalues(final_plot_df$variable, from = c("prevalence", "input_EIR", "simulated_EIR"), 
                                         to = c("Prevalence", "Input EIR", "Simulated EIR"))
final_plot_df$variable = factor(final_plot_df$variable, levels = c("Input EIR", "Simulated EIR", "Prevalence"))

# Plot the seasonality #title = "Modelled monthly transmission pattern"
p1 = ggplot(plot_df, aes(x=Month, y=Seasonality)) + geom_line() + geom_point() +
    theme_bw(base_size=13) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ylim(0, 0.3) + labs( x = "Month", y = "Proportion of transmission") + 
    scale_x_continuous(breaks = c(1:12), labels = c(rownames(plot_df))) +
    geom_segment(mapping = aes(x=9, y=0.3, xend=9, yend=0.25), 
                 arrow=arrow(type = "closed", length = unit(2,"mm")), size=0.2, color="black") +
    ggtitle("Input transmission profile") +
theme(strip.background = element_rect(colour="white", fill="white")) 

# Plot input EIR
df_sim_eir = final_plot_df[which(final_plot_df$variable %in% c("Input EIR")), ] #, "Simulated EIR"
p2 = ggplot(df_sim_eir, aes(x=time, y=value, color = variable, fill = variable)) + 
    scale_color_manual(values = c("#2ca25f")) +
    scale_fill_manual(values = c("#2ca25f")) +
    stat_summary(geom = "line", fun = mean, size = 1) +
    stat_summary(geom = "ribbon", fun.min = min, fun.max = max, color = NA, alpha = 0.3) +
    geom_vline(xintercept = 152, linetype="dashed", size = 0.5) + 
    geom_vline(xintercept = 335, linetype="dashed", size = 0.5) +
    theme_bw(base_size=13) + 
    geom_segment(mapping = aes(x=273, y=0.4, xend=273, yend=0.35), 
                 arrow=arrow(type = "closed", length = unit(2,"mm")), 
                 size=0.2, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title=element_blank()) +
    ggtitle("Modeled transmission profile") +
    xlab("Time (days)") + ylab("Proportion of transmission") 

# Plot prevalence
df_sim_eir = final_plot_df[which(final_plot_df$variable == "Prevalence"), ]
p3 = ggplot(df_sim_eir, aes(x=time, y=value, color = variable, fill = variable)) + 
    scale_color_manual(values = c("#d94801")) +
    scale_fill_manual(values = c("#d94801")) +
    stat_summary(geom = "line", fun = mean, size = 1) +
    stat_summary(geom = "ribbon", fun.min = min, fun.max = max, color = NA, alpha = 0.3) +
    geom_vline(xintercept = 152, linetype="dashed", size = 0.5) + 
    geom_vline(xintercept = 335, linetype="dashed", size = 0.5) +
    theme_bw(base_size=13) + 
    geom_segment(mapping = aes(x=273, y=0.7, xend=273, yend=0.65), 
                 arrow=arrow(type = "closed", length = unit(2,"mm")), 
                 size=0.2, color="black") + ylim(0, 0.7) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title=element_blank()) + ggtitle("Simulated prevalence profile") +
    xlab("Time (days)") + ylab("Proportion of population") 


figure = ggarrange(p1, p2, p3, labels = c("A", "B", "C"), ncol = 3, nrow = 1, legend = "none", widths = c(1.2,1,1))
ggsave("~/MMC/TPP/figures/simulation_desc/seasonality2.pdf",
       plot = figure, width = 11, height = 4)



