##########################
# Plot sensitivity analysis plots
##########################

source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/plot_GP_sensitivity.R")
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/plot_prev_red_distr.R")
library(grid)
library(gridExtra) 

plot_prev_red_distr_new = function(sim_folders, sim_names, follow_up, legend_rows, 
                               seas = NULL, b_p = NULL, plot_title = "", EIR_prev_tab) {
    final_plot_df = NULL
    for (i in 1:length(sim_folders)) {
        sim_folder = sim_folders[i]
        processed_folder = paste0(sim_folder, paste0("postprocessing_", follow_up,"/"))
        plot_df = NULL
        # Plot the distribution of prevalence across transmission settings
        processing_files = list.files(processed_folder, pattern = "seeds_", full.names = TRUE)
        for(p_file in processing_files) {
            OM_result = read.table(p_file, sep="\t", header = TRUE, as.is = TRUE)
            plot_df = rbind.data.frame(plot_df, OM_result[, c("Seasonality", "Biting_pattern", "EIR", "prev_red")])
        }
        # Define discrete EIR levels 
        plot_df$EIR = floor(plot_df$EIR) - (plot_df$EIR == 25)
        
        # Remove stochastic elimination
        plot_df = plot_df[which(plot_df$prev_red>0),]
        
        plot_df$Seasonality = factor(str_to_title(plot_df$Seasonality), levels = c("Perennial", "Seasonal"))
        plot_df$Biting_pattern = paste(str_replace(plot_df$Biting_pattern, "_", " "), "biting")
        plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
        plot_df$Experiment = sim_names[i]
        final_plot_df = rbind.data.frame(final_plot_df, plot_df)
    }
    
    # Extract the statistics on prevalence reduction
    stats_table = final_plot_df %>% group_by(Experiment, Seasonality, Biting_pattern, EIR) %>% 
        dplyr::summarise(Median = median(prev_red), Conf_95_low = boxplot.stats(prev_red)$conf[1], 
                         Conf_95_high = boxplot.stats(prev_red)$conf[2])
    
    final_plot_df$Experiment = factor(final_plot_df$Experiment, levels = sim_names)
    
    # Transform EIR to median prevalence 
    final_plot_df = merge(final_plot_df, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR"))
    final_plot_df = final_plot_df[which(final_plot_df$EIR %in% c(1, 7, 16, 24)),]
    
    # Plot the prevalence reduction distributions
    intervention_colors = create_color_palette_int()
    # intervention_colors = intervention_colors[which(names(intervention_colors) %in% c("Coverage", "Efficacy", "Half-life", "Access to treatment"))]
    # intervention_colors = intervention_colors[order(factor(names(intervention_colors), levels = c("Coverage", "Efficacy", "Half-life", "Access to treatment")))]
    
    
    if (!is.null(seas) & !is.null(b_p)) {
        final_plot_df = final_plot_df[which(final_plot_df$Seasonality == seas & 
                                                final_plot_df$Biting_pattern == b_p),]
        intervention_colors_plot = intervention_colors[which(names(intervention_colors) %in% final_plot_df$Experiment)]
        p = ggplot(final_plot_df, aes(x=as.factor(round(Median_2_10*100)), y=prev_red, color = Experiment)) + 
            #geom_violin() + 
            geom_boxplot(outlier.size = 0.1, position=position_dodge(width = 0.9)) +
            scale_color_manual(name = "Experiment", values = intervention_colors_plot)+
            # scale_x_discrete(breaks = c(1, 7, 16, 24), labels = c("1", "7", "16", "24")) +
            theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), 
                  y = expression(paste(italic("Pf"), "PR"["0\u201399"], " reduction (%)")), title = plot_title) +
            theme(plot.title = element_text(size=10)) +
            theme(panel.background = element_rect(fill = 'white')) + 
            theme(strip.background = element_rect(colour="white", fill="white")) +
            theme(legend.position="top") + 
            guides(color=guide_legend(nrow = legend_rows, byrow = FALSE, title = "Applied\ninterventions")) +
            theme(text = element_text(family = "Arial")) 
        #theme(legend.title = element_blank())
    } else {
        # labels_plot = paste0(unique(final_plot_df$EIR), " (", unique(round(final_plot_df$Median_2_10*100)), ")")
        intervention_colors_plot = intervention_colors[which(names(intervention_colors) %in% final_plot_df$Experiment)]
        p = ggplot(final_plot_df, aes(x=as.factor(EIR), y=prev_red, color = Experiment)) + 
            #geom_violin() + 
            geom_boxplot(outlier.size = 0.1, position=position_dodge(width = 0.9)) +
            scale_color_manual(name = "Experiment", values = intervention_colors_plot) +
            theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            facet_wrap(~Seasonality + Biting_pattern, scales='free_x') + 
            labs( x = expression(paste("Input EIR (Corresponding Median ", italic("Pf"), "PR"["2\u201310"], " %)")), y = "Prevalence reduction (%)") +
            # scale_x_discrete(breaks = rep(unique(final_plot_df$EIR), 6), 
            #                  labels = labels_plot) +
            theme(panel.background = element_rect(fill = 'white')) + 
            theme(strip.background = element_rect(colour="white", fill="white")) +
            theme(legend.position="top") + 
            guides(color=guide_legend(nrow = legend_rows, byrow = FALSE, title = "Applied\ninterventions")) +
            theme(text = element_text(family = "Arial")) 
        #theme(legend.title = element_blank())
    }
    
    return(list(p = p, stats_table = stats_table))
}


get_plot_group = function(query_list, all_exp, all_names, EIR_prev_tab) {
    p_array_list = vector('list', length(query_list))
    for(i in 1:length(query_list)) {
        exp_folder = query_list[i]
        param_ranges_file = paste0(exp_folder, "param_ranges.RData")
        plot_title = all_names[which(all_exp == exp_folder)]
        
        gp_folder = paste0(exp_folder, "gp_4/")
        sens_folder = paste0(gp_folder, "sensitivity/")
        p_4 = plot_single_sens_GP(sens_folder, "Seasonal", "High_indoor", "total_effects", 
                                  "area", param_ranges_file, "Immediate follow-up", EIR_prev_tab, c(1, 5, 10, 15, 20, 24))
        
        gp_folder = paste0(exp_folder, "gp_6/")
        sens_folder = paste0(gp_folder, "sensitivity/")
        p_6 = plot_single_sens_GP(sens_folder, "Seasonal", "High_indoor", "total_effects", 
                                  "area", param_ranges_file, "Late follow-up", EIR_prev_tab, c(1, 5, 10, 15, 20, 24))
        
        p_4$selected_points = cbind.data.frame(p_4$selected_points, "Immediate follow-up")
        colnames(p_4$selected_points) = c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects", "follow_up")
        p_6$selected_points = cbind.data.frame(p_6$selected_points, "Late follow-up")
        colnames(p_6$selected_points) = c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects", "follow_up")
        final_tab = rbind.data.frame(p_4$selected_points, p_6$selected_points)
        final_tab$Biting_pattern = paste(str_replace(final_tab$Biting_pattern, "_", " "))
        final_tab$Biting_pattern = factor(final_tab$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
        
        # Transform the EIR axis
        final_tab$EIR_trans = floor(final_tab$EIR) - (final_tab$EIR == 25)
        EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
        EIR_prev_tab$EIR = NULL
        final_tab = merge(final_tab, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
        intervention_colors = create_color_palette_sens()
        intervention_colors = intervention_colors[which(names(intervention_colors) %in% c("Coverage", "Efficacy", "Half-life", "Access to treatment"))]
        intervention_colors = intervention_colors[order(factor(names(intervention_colors), levels = c("Coverage", "Efficacy", "Half-life", "Access to treatment")))]
        
        # final_tab = final_tab[which(final_tab$EIR <= 20), ]
        x_ticks = c(1, 7, 16, 24)
        inds = which(final_tab$EIR_trans %in% x_ticks)
        labs_x = unique(final_tab[inds, c("EIR_trans", "Median_2_10")])
        
        final_tab$parameter = str_replace(final_tab$parameter, "Access", "Access to treatment")
        final_tab$parameter = str_replace(final_tab$parameter, "Preprandial efficacy", "Efficacy")
        final_tab$parameter = factor(final_tab$parameter, levels = c("Coverage", "Efficacy", "Half-life", "Access to treatment"))
        
        p_array_list[[i]] = ggplot(final_tab, aes(x = EIR, y = total_effects, group = parameter, fill = parameter)) +
            theme_bw(base_size=13) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            geom_area(alpha = 0.9) + theme(strip.background = element_rect(colour="white", fill="white")) +
            scale_fill_manual(name = "parameter", values = intervention_colors) +
            labs(x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), 
                 y = "Relative importance", title = plot_title) +
            guides(fill=guide_legend(title="")) + facet_wrap(~ follow_up) +
            scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100)) +
            theme(text = element_text(family = "Arial")) 
        
        # p_array_list[[i]] = ggarrange(p_4, p_6, ncol = 2, nrow = 1,
        #                               common.legend = TRUE, legend = "top")
        # figure_title = all_names[which(all_exp == exp_folder)]
        # p_array_list[[i]] = annotate_figure(p_array_list[[i]], top = text_grob(figure_title, size = 12))
    }
    
    return(p_array_list)
}

# Load the EIR_to_prevalence transformations
EIR_prev_tab = read.table("/scicore/home/penny/golmon00/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)

# Plot prevalence reduction
exp_names = define_experiment_names()
plot_prev_red_4 = plot_prev_red_distr_new(exp_names$list_folders_single, 
                                      exp_names$list_names_single,
                                      4, 6, "Seasonal", "High indoor biting", "Immediate follow-up", EIR_prev_tab)
plot_prev_red_6 = plot_prev_red_distr_new(exp_names$list_folders_single, 
                                      exp_names$list_names_single,
                                      6, 6, "Seasonal", "High indoor biting", "Late follow-up", EIR_prev_tab)
plot_prev_red = ggarrange(plot_prev_red_4$p, plot_prev_red_6$p, ncol = 2, nrow = 1,
                          common.legend = TRUE, legend = "right")
# ggsave("~/MMC/TPP/figures/main_figures/Fig2A.pdf",
#        plot = plot_prev_red, width = 9.5, height = 2.5)

# Define the interventions to be displayed
all_exp = exp_names$all_folders
all_names = exp_names$all_names
plotted_folders = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",
                    "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/", 
                    "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/",               
                    "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/") 
plotted_folders2 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")

single_plots = get_plot_group(plotted_folders, all_exp, all_names, EIR_prev_tab)
single_plots2 = get_plot_group(plotted_folders2, all_exp, all_names, EIR_prev_tab)
figure_single = ggarrange(plotlist = single_plots, ncol = 2, nrow = 2,
                          legend = "none", labels = c("B", "C", "D", "E"))
figure_dummy = ggarrange(plotlist = single_plots2, legend = "right")
plot_legend = cowplot::get_legend(single_plots2[[1]])
figure_double = ggarrange(single_plots2[[1]], plot_grid(plot_legend), ncol = 2, nrow = 1, 
                          labels = c("F", ""), legend = "none")

fig_2 = ggarrange(plot_prev_red, figure_single, figure_double, ncol = 1, 
                  nrow = 3, labels = c("A", ""), heights = c(1,2,1))

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig4.pdf",
       plot = fig_2, width = 10, height = 10, device = cairo_pdf)

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig4.ps",
       plot = fig_2, width = 10, height = 10, device = cairo_ps)
