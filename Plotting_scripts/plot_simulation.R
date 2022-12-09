#######################################
# Plot a sample prevalence time series of an
# OpenMalaria simulation
#
# created 10.02.2020
# monica.golumbeanu@unibas.ch
######################################

library(ggplot2)

# Plots the time series of a given output
plot_measure = function(om_result, measure_code, plot_title) {
    colnames(om_result) = c("time", "age_group", "measure", "value")
    data_to_plot = as.data.frame(om_result[om_result$measure==measure_code,])
    
    ggplot(data_to_plot) + geom_line(aes(x=time, y=value, colour=as.factor(age_group)), size=1) + 
        ggtitle(plot_title) + ylim(0,max(data_to_plot$value)) + labs(color="type") 
}

# Comparison of two outputs
plot_two_measures = function(om_result, measure_code1, measure_code2) {
    colnames(om_result) = c("time", "age_group", "measure", "value")
    data_to_plot = as.data.frame(om_result[om_result$measure==measure_code1 | om_result$measure==measure_code2,])
    
    ggplot(data_to_plot) + geom_line(aes(x=time, y=value, colour=as.factor(measure)), size=1) + 
        ylim(0,max(data_to_plot$value)) + labs(color="Output codes") 
}

# Plots the time series of a given output between two time points
zoom_measure = function(om_result, ind, measure_code, plot_title) {
    colnames(om_result) = c("time", "age_group", "measure", "value")
    data_to_plot = as.data.frame(om_result[om_result$measure==measure_code,])
    data_to_plot = data_to_plot[ind, ]
    ggplot(data_to_plot) + geom_line(aes(x=time, y=value, colour=as.factor(age_group)), size=1) + 
        ggtitle(plot_title) + ylim(0,max(data_to_plot$value)) + labs(color="type") 
    return(data_to_plot[,4])
}

sim_folder = "/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/om/"
file_pattern = "TBV_once_3_years_1000_"
files_list = dir(sim_folder, pattern = file_pattern, full.names = TRUE)
out_files = files_list[which(str_detect(files_list, "_out.txt"))]
# code for the number of cases
measure_code = 1

plot_df = NULL
for (sim_file in out_files) {
    # Read the simulation output and extract the number of cases time series
    om_result = read.table(sim_file, sep="\t")
    colnames(om_result) = c("time", "age_group", "measure", "value")
    n_cases = as.data.frame(om_result[om_result$measure==measure_code,])
    
    # Summarize results by summing up over age groups
    om_result$age_group=NULL
    agg_n_cases = n_cases %>% group_by(time, measure) %>% summarise(val = sum(value)/10000)
    if (is.null(plot_df)) {
        plot_df = agg_n_cases$val
    } else {
        plot_df = cbind.data.frame(plot_df, agg_n_cases$val)
    }
}
summary_plot_df = cbind.data.frame(agg_n_cases$time, rowMeans(plot_df), do.call(pmin, plot_df), do.call(pmax, plot_df))
colnames(summary_plot_df) = c("time", "mean", "min_cases", "max_cases")

arrow.length <- 0.05
touchoff.distance <- 0.03 # distance between data and start of arrow
arrowhead.size <- 1.5 # in millimeters

ggplot(summary_plot_df) + 
    geom_rect(mapping=aes(xmin = 4, xmax = 5, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_rect(mapping=aes(xmin = 8, xmax = 9, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_rect(mapping=aes(xmin = 10, xmax = 11, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
    geom_ribbon(aes(x = time/73, ymin = min_cases, ymax = max_cases), fill = "#fc9272", alpha = 0.6) +
    geom_line(aes(x = time/73, y=mean), size=0.3, color = "#de2d26") + theme_bw(base_size=13) + 
    geom_vline(xintercept = 5.4167, linetype="dashed", size = 0.5) + geom_vline(xintercept = 5.9167, linetype="dashed", size = 0.3) + 
    geom_vline(xintercept = 6.4167, linetype="dashed", size = 0.5) + geom_vline(xintercept = 6.9167, linetype="dashed", size = 0.3) +
    geom_vline(xintercept = 7.4167, linetype="dashed", size = 0.5) + geom_vline(xintercept = 7.9167, linetype="dashed", size = 0.3) +
    scale_x_continuous(breaks=c(0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15), expand = c(0.001, 0)) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab("Time (years)") + ylab(expression(paste(italic("Pf"), "PR"["0-99"])))

ggsave("~/MMC/TPP/figures/simulation_desc/simulation_description2.pdf", width = 4, height = 2)

