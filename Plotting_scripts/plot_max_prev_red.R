##################################
# Run the plotting for various models/settings
#
# created 02.12.2019
# monica.golumbeanu@swisstph.ch
##################################

# Load the necessary plotting functions
library(stringr)
library(ggplot2)
library(hetGP)

plot_figure = function(plot_df, plot_file) {
    plot_df[which(plot_df$mean > 100), "mean"] = 100
    plot_df[which(plot_df$mean < 0), "mean"] = 0
    max_r = max(plot_df$mean + plot_df$sd)
    ggplot(plot_df, aes(x = EIR, y = mean)) +
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        geom_line() + theme(strip.background = element_rect(colour="white", fill="white")) +
        facet_wrap(~Seasonality + Biting_pattern) + ylim(0, max_r) +
        geom_ribbon(aes(ymin = mean-sd, ymax=mean+sd, x=EIR, fill = "band"), alpha = 0.3)+
        labs( x = "EIR", y = "Maximum prevalence reduction") + theme(legend.position = "none") 
    ggsave(plot_file, width = 7, height = 5)
}

plot_max_prev_red = function(GP_dir, plot_dir, param_ranges_file, exp_name) {
    load(param_ranges_file)
    file.names = dir(GP_dir, pattern =".RData", full.names = TRUE)
    final_plot_df = NULL
    points_df = NULL
    
    # Construct the points where the GP models are to be evaluated
    EIR = seq(from=2, to=25, by=0.5)
    points_table = cbind.data.frame(EIR, t(param_ranges[-which(row.names(param_ranges) == "EIR"),2]))
    points_table$Access = 0.2                 
    for(i in 1:length(file.names)){
        gp_result_name = load(file.names[i])
        gp_result = get(gp_result_name)
        rm(gp_result_name)
        prediction = predict(x = as.matrix(points_table), object = gp_result$GP_model)
        prediction_df = cbind.data.frame(prediction$mean, prediction$sd2, gp_result$seasonality, 
                                         gp_result$biting_pattern)
        colnames(prediction_df) = c("mean", "sd", "Seasonality", "Biting_pattern")
        points_df = rbind.data.frame(points_df, cbind.data.frame(points_table, prediction_df))
    }
    plot_file = paste(plot_dir, exp_name, "_max_prev_red", ".pdf", sep="")
    plot_figure(points_df, plot_file) 
}

# Retrieve the command argments and define function inputs
# args = commandArgs(TRUE)
# sim_dir = args[1]
# follow_up = args[2]
# plot_title = args[3]

# For testing:
sim_dir = "~/MMC/TPP/simulations/MDA_MAB_twice_3years/"
follow_up = 4

GP_dir = paste0(sim_dir, "gp_", follow_up, "/as/")
param_ranges_file = paste0(sim_dir, "param_ranges.RData")
param_table = read.table(paste0(sim_dir, "param_tab.txt"), header = TRUE)
model_pattern = str_remove(param_table$Scenario_Name[1], "_1")
plot_dir = "~/MMC/TPP/figures/simulation_desc/max_prev_red/"

plot_max_prev_red(GP_dir, plot_dir, param_ranges_file, paste(model_pattern, "_", follow_up, sep=""))

