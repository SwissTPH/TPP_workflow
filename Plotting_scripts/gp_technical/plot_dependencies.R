#####################################
# Plot dependencies between prevalence reduction and input parameters
#
# monica.golumbeanu@unibas.ch
#####################################
library(tgp)
library(hetGP)
library(ggplot2)
library(lhs)
library(stringr)

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")

rep.row = function(x,n){
    return(matrix(rep(x,each=n),nrow=n))
}

rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# Experiment files and follow-up
sim_files = c("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/",
              "~/MMC/TPP/simulations/PEV_once_3years/",
              "~/MMC/TPP/simulations/TBV_once_3years/",
              "~/MMC/TPP/simulations/ATSB_once_3years/",
              "~/MMC/TPP/simulations/preprandial_once_3years/")
sim_titles = c("Anti-infective monoclonal",
               "Anti-infective vaccine",
               "Transmission-blocking vaccine",
               "Attractive targeted sugar baits",
               "Eave tubes")
follow_up = 4

# Setting levels and initialization
EIR_levels = c(2, 5, 10, 20)
num_points = 10000
plot_list_param = list()
plot_list_gp = list()
t_param = t_gp = 0

# Construct the plots
for (sim_file in 1:length(sim_files)) {
    # Extract the correct file paths for gp and parameter ranges
    exp_name = str_replace(basename(sim_files[sim_file]), "_avg_prev", "")
    exp_name = str_replace(exp_name, "_3years", "")
    exp_name = str_replace(exp_name, "preprandial", "Preprandial")
    gp_file = paste0(sim_files[sim_file], "gp_", follow_up, "/as/seeds_", exp_name, "_3_years_seasonal_High_indoor_cv_as.RData")
    load(paste0(sim_files[sim_file], "param_ranges.RData"))
    rownames(param_ranges)[which(rownames(param_ranges) == "Halflife")] = "Half-life"
    rownames(param_ranges)[which(rownames(param_ranges) == "Access")] = "Access to treatment"
    
    # Load the sampled points for OpenMalaria simulations and build the plot data frame
    output_table = read.table(paste0(sim_files[sim_file], "postprocessing_", follow_up, "/seeds_", exp_name, "_3_years_seasonal_High_indoor.txt"), 
                              header = TRUE, stringsAsFactors = FALSE, as.is = TRUE)
    output_table = output_table[which(output_table$Seasonality == "seasonal" & output_table$Biting_pattern == "High_indoor"),]
    colnames(output_table)[which(colnames(output_table) == "Preprandial_efficacy")] = "Efficacy"
    colnames(output_table)[which(colnames(output_table) == "Halflife")] = "Half-life"
    colnames(output_table)[which(colnames(output_table) == "Access")] = "Access to treatment"
    
    Y = output_table[,c("EIR", "Half-life", "Efficacy","Coverage", "Access to treatment", "prev_red")]
    Y_param = Y[, -ncol(Y)]
    Y_mean = colMeans(Y_param)
    Y_sd = apply(Y_param, 2, sd)
    dep1 = NULL
    for (j in 1:ncol(Y_param)) {
        Z = Y_param[,-j]
        sel_idx = apply(Z, 1, function(x) sum((Y_mean[-j] - Y_sd[-j] <= x) & (Y_mean[-j] + Y_sd[-j] >= x)) == 4 )
        # normalise Y
        norm_Y = (Y[sel_idx, j] - min(Y[sel_idx, j])) / (max(Y[sel_idx, j]) - min(Y[sel_idx, j]))
        dep_rows = data.frame(x = norm_Y, y = Y[sel_idx, "prev_red"], 
                              Strategy = rownames(param_ranges)[j])
        dep1 = rbind(dep1, dep_rows)
    }
    
    # Load the GP emulator and build the plot data frame
    gp_result_name = load(gp_file)
    trained_model = get(gp_result_name)
    X = lhs(num_points, as.matrix(param_ranges))
    X_mean = colMeans(X)
    dep2 = NULL
    for (j in 1:ncol(X)) {
        X_new = rep.row(X_mean, num_points)
        X_new[,j] = X[,j]
        pred_gp = predict(x = as.matrix(X_new), object = trained_model$GP_model)
        out_pred = pred_gp$mean
        out_sd = sqrt(pred_gp$sd2 + pred_gp$nugs)
        # normalise X
        norm_X = (X[,j] - min(X[,j])) / (max(X[,j]) - min(X[,j]))
        dep_rows = data.frame(x = norm_X, y = out_pred, sd_y = out_sd,
                              Strategy = rownames(param_ranges)[j])
        dep2 = rbind(dep2, dep_rows)
    }
    # Build the complete plotting data frame
    dep1$exp = "OpenMalaria"
    dep2$exp = "Gaussian process emulator"
    dep1$sd_y = 0
    dep_all = rbind(dep1, dep2)
    dep_all$exp = factor(dep_all$exp)
    dep_all[which(dep_all$Strategy == "Preprandial_efficacy"), "Strategy"] = "Efficacy"
    dep1[which(dep1$Strategy == "Preprandial_efficacy"), "Strategy"] = "Efficacy"
    dep2[which(dep2$Strategy == "Preprandial_efficacy"), "Strategy"] = "Efficacy"
    
    t_param = t_param + 1
    plot_list_param[[t_param]] = ggplot() + 
                                    stat_summary(data = dep1, aes(x = x, y = y), size = 0.2, alpha = 0.4, fun.data = "mean_cl_boot") +
                                    geom_line(data = dep2, aes(x = x, y = y), color = "#e6550d", alpha = 1, lwd = 1.5) + 
                                    facet_wrap("Strategy", ncol = 5) + theme_bw(base_size=12) + ylim(0, 100) +
                                    theme(strip.background = element_rect(colour="white", fill="white")) +
                                    labs( x = "Normalized input value", y = expression(italic("Pf")~"PR"["0-99"]~" reduction (%)")) +
                                    theme(panel.background = element_rect(fill = 'white')) +
                                    ggtitle(sim_titles[sim_file])
    
    # For each EIR level, vary each column in turn, while the others are set to the average value
    for (i in 1:length(EIR_levels)) {
        X = lhs(num_points, as.matrix(param_ranges))
        X[,1] = EIR_levels[i]
        X_mean = colMeans(X)
        dep = NULL
        for (j in 2:ncol(X)) {
            X_new = rep.row(X_mean, num_points)
            X_new[,j] = X[,j]
            out_pred = predict(x = as.matrix(X_new), object = trained_model$GP_model)$mean
            out_pred[which(out_pred > 100)] = 100
            
            # normalise X
            norm_X = (X[,j] - min(X[,j])) / (max(X[,j]) - min(X[,j]))
            dep_rows = data.frame(x = norm_X, y = out_pred, 
                                  Parameter = rownames(param_ranges)[j])
            dep = rbind(dep, dep_rows)
        }
        t_gp = t_gp + 1
        parameter_colors = create_color_palette_sens()
        
        dep[which(dep$Parameter == "Preprandial_efficacy"), "Parameter"] = "Efficacy"
        plot_list_gp[[t_gp]] = ggplot(dep) + geom_line(aes(x = x, y = y, colour = Parameter), size=2) +
                                theme_bw(base_size=11) + 
                                theme(strip.background = element_rect(colour="white", fill="white")) +
                                scale_color_manual(name = "Parameter", values = parameter_colors) +
                                labs( x = "Normalized input value", y = expression(italic("Pf")~"PR"["0-99"]~" reduction (%)")) +
                                theme(panel.background = element_rect(fill = 'white')) +
                                ggtitle(paste0(sim_titles[sim_file], "\n", "EIR = ", EIR_levels[i]))
                                # labs(title = paste(exp_name, "\n", "EIR =", EIR_levels[i]), fill="", 
                                #         x="standardized input", y="prevalence reduction")
    }
    
}


# figure1 = ggarrange(plotlist = plot_list_param, ncol = 1, nrow = 5, labels = c("A", "B", "C", "D", "E"), common.legend = TRUE)
figure2 = ggarrange(plotlist = plot_list_gp, ncol = 4, nrow = 5, labels = LETTERS[seq( from = 1, to = 20 )], common.legend = TRUE)
# ggsave("~/MMC/TPP/figures/gp_performance/dependencies_plots_gp_om_6.pdf",
#        plot = figure1, width = 12, height = 13)
ggsave("~/MMC/TPP/figures/gp_performance/dependencies_plots_EIR_4.pdf",
       plot = figure2, width = 12, height = 13)




