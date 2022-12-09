#################################
# Plot the seasonality pattern
#
#
# created 03.10.2019
# monica.golumbeanu@unibas.ch
################################

library(data.table)
library(stringr)
library(ggplot2)
library(ggnewscale)
library(ggpubr)

######## Plot the age distribution #############
bins = seq(5, 90, by=5)
pop_p = c(16.23476, 14.52151394, 12.75565434, 10.836323739999999, 8.393312454, 7.001421452, 5.800587654,
          5.102136612, 4.182561874, 3.339409351, 2.986112356, 2.555766582, 2.332763433, 1.77400255, 1.008525491, 0.74167341, 0.271863401, 0.161614642)
pop_df = cbind.data.frame(bins, pop_p)
p1 = ggplot(pop_df, aes(x = bins, y = pop_p)) + 
    geom_col() + theme_bw(base_size=20) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(5, 90, by=10)) +
    labs( x = "Age group (years)", y = "Percentage of population (%)") +
    ggtitle("Modelled age distribution")


####### Plot the simulated settings #############
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

# Construct the table with seasonality values per month
dates = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
long_tab_s = NULL
long_tab_s$time = unique(final_plot_df$time)
long_tab_s$value = rep(plot_df$Seasonality, dates)[long_tab_s$time]
long_tab_s$variable = "Seasonality"
long_tab_s = as.data.frame(long_tab_s)

### Plot setting characteristics (seasonality, input EIR, simulated EIR)
df_sim_eir = final_plot_df[which(final_plot_df$variable %in% c("Prevalence", "Input EIR")), ] 
df_sim_eir$variable = factor(df_sim_eir$variable, levels = c("Prevalence", "Input EIR"))
p2 = ggplot() + 
    geom_col(data = long_tab_s, aes(x = time, y = value, fill = variable), color = "#E8E8E8") +
    scale_fill_manual(values = c("#E8E8E8"))+
    theme(legend.title=element_blank(), legend.text.align = 0) +
    ggnewscale::new_scale_fill() + 
    stat_summary(data=df_sim_eir, aes(x=time, y=value, fill = variable), geom = "ribbon", alpha = 0.2, fun.min = min, fun.max = max) + 
    stat_summary(data=df_sim_eir, aes(x=time, y=value, color = variable), geom = "line", fun = mean, size = 1) +
    scale_color_manual(values = c("#d94801","#2ca25f"), labels = c(expression(italic("Pf")~"PR"["0-99"]), "Input EIR")) +
    scale_fill_manual(values = c("#d94801","#2ca25f"), labels = c(expression(italic("Pf")~"PR"["0-99"]), "Input EIR")) +
    theme_bw(base_size=20) + 
    # geom_segment(mapping = aes(x=273, y=0.4, xend=273, yend=0.35), 
    #              arrow=arrow(type = "closed", length = unit(2,"mm")), 
    #              size=0.2, color="black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.title=element_blank()) +
    ggtitle("Modelled transmission profile") +
    xlab("Time (days)") + ylab("Proportion") 

######## Plot the prevalence to EIR relationship ##########
sim_folder = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/"
processed_folder = paste0(sim_folder, "postprocessing_4/")

# Plot the distribution of prevalence across transmission settings
processing_files = list.files(processed_folder, pattern = "seeds_", full.names = TRUE)

plot_df = NULL
for(p_file in processing_files) {
    OM_result = read.table(p_file, sep="\t", header = TRUE, as.is = TRUE)
    plot_df = rbind.data.frame(plot_df, OM_result[, c("Seasonality", "Biting_pattern", "EIR", "initial_prev")])
}
plot_df = as.data.frame(plot_df)
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
                     Conf_95_high_0_99 = boxplot.stats(initial_prev)$conf[2])

ex_table = plot_df[,c("Seasonality", "Biting_pattern", "EIR", "initial_prev")]
ex_table_m = reshape2::melt(as.data.frame(ex_table), id=c("Seasonality", "Biting_pattern", "EIR"))
ex_table_m = ex_table_m[which(ex_table_m$Seasonality == "Seasonal" & ex_table_m$Biting_pattern == "High indoor biting"),]
p3 = ggplot(ex_table_m, aes(x=as.factor(EIR), y=value*100, color = variable)) + 
    geom_boxplot(outlier.size = 0.5) + #scale_x_discrete(breaks = c(2, 10, 18), labels = c("2:4", "10:12", "18:20")) +
    theme_bw(base_size=20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Input EIR level", y = expression(italic("Pf")~"PR"["0-99"]~"(%)")) +
    scale_x_discrete(breaks=c(1, seq(5,25,5)), labels=c(1, seq(5,25,5))) +
    scale_color_manual(values = c("#d94801")) +
    theme(panel.background = element_rect(fill = 'white')) + 
    theme(strip.background = element_rect(colour="white", fill="white")) + guides(color = FALSE) +
    theme(legend.position="none") + theme(legend.title = element_blank()) +
    ggtitle(expression(paste("True simulated ", italic("Pf"), "PR"["0-99"])))

# Combine the figures
figure = ggarrange(p2, p1, p3, labels = c("B", "C", "D"), font.label = list(size = 23),
            ncol = 3, nrow = 1, legend = "right", widths = c(1.2,1,1))
ggsave("~/MMC/TPP/figures/main_figures/Fig1BCD.pdf",
       plot = figure, width = 15, height = 5)



