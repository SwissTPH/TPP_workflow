#######################
# plot the distributions of prevalence reduction for all interventions and combinations
#
# monica.golumbeanu@unibas.ch
# created 16.03.2020
#######################
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/color_palettes.R")
library(stringr)
library(dplyr)
library(lemon)

plot_prev_red_distr = function(sim_folders, sim_names, follow_up, legend_rows, 
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
    
    if (!is.null(seas) & !is.null(b_p)) {
        final_plot_df = final_plot_df[which(final_plot_df$Seasonality == seas & 
                                                final_plot_df$Biting_pattern == b_p),]
        p = ggplot(final_plot_df, aes(x=as.factor(round(Median_2_10*100)), y=prev_red, color = Experiment)) + 
            #geom_violin() + 
            geom_boxplot(outlier.size = 0.1, position=position_dodge(width = 0.9)) +
            scale_color_manual(name = "Experiment", values = intervention_colors)+
            # scale_x_discrete(breaks = c(1, 7, 16, 24), labels = c("1", "7", "16", "24")) +
            theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            labs( x = expression(paste("Median ", italic("Pf"), "PR"["2-10"], " (%)")), 
                  y = expression(paste(italic("Pf"), "PR"["0-99"], " reduction (%)")), title = plot_title) +
            theme(plot.title = element_text(size=10)) +
            theme(panel.background = element_rect(fill = 'white')) + 
            theme(strip.background = element_rect(colour="white", fill="white")) +
            theme(legend.position="top") + guides(color=guide_legend(nrow = legend_rows, byrow = FALSE, title = "Applied\ninterventions")) 
        #theme(legend.title = element_blank())
    } else {
        # labels_plot = paste0(unique(final_plot_df$EIR), " (", unique(round(final_plot_df$Median_2_10*100)), ")")
        p = ggplot(final_plot_df, aes(x=as.factor(EIR), y=prev_red, color = Experiment)) + 
            #geom_violin() + 
            geom_boxplot(outlier.size = 0.1, position=position_dodge(width = 0.9)) +
            scale_color_manual(name = "Experiment", values = intervention_colors) +
            theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            facet_wrap(~Seasonality + Biting_pattern, scales='free_x') + 
            labs( x = expression(paste("Input EIR (Corresponding Median ", italic("Pf"), "PR"["2-10"], " %)")), y = "Prevalence reduction (%)") +
            # scale_x_discrete(breaks = rep(unique(final_plot_df$EIR), 6), 
            #                  labels = labels_plot) +
            theme(panel.background = element_rect(fill = 'white')) + 
            theme(strip.background = element_rect(colour="white", fill="white")) +
            theme(legend.position="top") + guides(color=guide_legend(nrow = legend_rows, byrow = FALSE, title = "Applied\ninterventions")) 
        #theme(legend.title = element_blank())
    }
        
    return(list(p = p, stats_table = stats_table))
}

