#########################################
# plot GP sensitivity analysis results
#########################################
library(ggplot2)
library(dplyr)

PARAMETER_NAMES = c("Coverage", "Efficacy", "Preprandial efficacy", "Postprandial efficacy", "PI Efficacy", 
                    "TB Efficacy",  "Halflife", "Half-life", "PI Halflife", "TB Halflife",
                    "Access", "Drug halflife", "Drug efficacy")

PARAMETER_NAMES2 = c("Coverage", "Efficacy", "Half-life", "Access")

create_color_palette_sens = function() {
    p = c("Halflife", "Half-life", "PI Halflife", "TB Halflife", "Efficacy", "PI Efficacy", 
          "TB Efficacy", "Coverage", "Access", "Drug halflife", "Drug efficacy", 
          "Preprandial efficacy", "Postprandial efficacy")
    categories = factor(p, levels = p)
    library(RColorBrewer)
    # myColors = brewer.pal(length(categories),"Set1")
    myColors = c("#6CC3B9", "#6CC3B9", "#6CC3B9", "#c7e9b4", "#3792BF", "#3792BF", 
                 "#a6bddb", "#0D539C", "#fec44f", "#efedf5", "#bcbddc", 
                 "#3792BF", "#3792BF") #"#20546F"
    names(myColors) = levels(categories)
    return(myColors)
    # colScale = scale_colour_manual(name = "grp",values = myColors)
}

plot_sens = function(plot_df, plot_val, plot_type, plot_file) {
    if(plot_type == "barplot") {
        plotType = geom_bar(stat = "identity", alpha = 0.9) 
    } else {
        plotType = geom_area(alpha = 0.9)
    }
    
    if(plot_val == "main_effects") {
        y_val = "Main effects"
    } else {
        y_val = "Total effects"
    }
    intervention_colors = create_color_palette_sens()
    plot_df$parameter = factor(plot_df$parameter, levels = PARAMETER_NAMES)
    ggplot(plot_df, aes_string(x = factor(plot_df$EIR), y = plot_val, group = "parameter", fill = "parameter")) +
        theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        plotType + theme(strip.background = element_rect(colour="white", fill="white")) +
        facet_wrap(~Seasonality + Biting_pattern) + scale_fill_manual(name = "parameter", values = intervention_colors) +
        labs( x = "Transmission level", y = y_val) + guides(fill=guide_legend(title="")) + scale_x_discrete(breaks=c(1, seq(5,25,5)), labels=c("1", "", "10", "", "20", ""))
    ggsave(plot_file, width = 5, height = 3)
}

get_sens_plots = function(plot_df, plot_val, plot_type, EIR_prev_tab, plot_t) {
    if(plot_type == "barplot") {
        plotType = geom_bar(stat = "identity", alpha = 0.9) 
    } else {
        plotType = geom_area(alpha = 0.9)
    }
    
    if(plot_val == "main_effects") {
        y_val = "Main effects"
    } else {
        y_val = "Total effects"
    }
    
    plot_df$EIR_trans = floor(plot_df$EIR) - (plot_df$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    plot_df = merge(plot_df, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    
    plot_df = plot_df[which(plot_df$EIR <= 24), ]
    x_ticks = c(2, 6, 10, 15, 20)
    inds = which(plot_df$EIR_trans %in% x_ticks)
    labs_x = unique(plot_df[inds, c("EIR_trans", "Median_2_10")])
    
    intervention_colors = create_color_palette_sens()
    plot_df$parameter = factor(plot_df$parameter, levels = PARAMETER_NAMES)
    p = ggplot(plot_df, aes_string(x = plot_df$EIR, y = plot_val, group = "parameter", fill = "parameter")) +
        theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        plotType + theme(strip.background = element_rect(colour="white", fill="white")) +
        guides(fill=guide_legend(title="")) +
        facet_wrap(~Seasonality + Biting_pattern) + scale_fill_manual(name = "parameter", values = intervention_colors) +
        labs(x = "EIR", y = "Relative importance", title = plot_t) + theme(plot.title = element_text(size=10, hjust = 0.5))
        # labs(x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), y = y_val) +
        # scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100, digits = 1))
    return(p)
}

plot_sens_GP = function(train_dir, plot_dir, param_ranges_file, exp_name) {
    load(param_ranges_file)
    file.names = dir(train_dir, pattern ="_sidx.RData", full.names = TRUE)
    final_plot_df = NULL
    points_df = NULL
    for(i in 1:length(file.names)){
        load(file.names[i])
        params = factor(rownames(param_ranges), levels = rownames(param_ranges))
        sens_df = cbind.data.frame(as.double(sobol_idx_list$EIR), str_to_title(sobol_idx_list$seasonality), sobol_idx_list$biting_pattern, params, sobol_idx_list$S_eff, sobol_idx_list$T_eff)
        final_plot_df = rbind.data.frame(final_plot_df, sens_df)
    }
    final_plot_df = final_plot_df[-which(final_plot_df$params == "EIR"),]
    colnames(final_plot_df) = c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects")
    final_plot_df$Seasonality = factor(final_plot_df$Seasonality, levels = c("Perennial", "Seasonal"))
    final_plot_df$Biting_pattern = paste(str_replace(final_plot_df$Biting_pattern, "_", " "), "biting")
    final_plot_df$Biting_pattern = factor(final_plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    final_plot_df$parameter = factor(str_replace(final_plot_df$parameter, "_", " "), levels = PARAMETER_NAMES)
    # Normalize effects for area plot
    final_plot_df_avg = (final_plot_df %>% group_by(EIR, Seasonality, Biting_pattern) %>% 
                             dplyr::mutate(main_effects = main_effects/sum(main_effects), 
                                    total_effects = total_effects/sum(total_effects))) 
    
    # Plotting Main effects 
    plot_sens(final_plot_df, "main_effects", "barplot", 
              paste(plot_dir, "main_effects_barplot_", exp_name, ".pdf", sep=""))
    plot_sens(final_plot_df_avg, "main_effects", "area",
              paste(plot_dir, "main_effects_area_", exp_name, ".pdf", sep="")) 
    # Plotting Total effects
    plot_sens(final_plot_df, "total_effects", "barplot", 
              paste(plot_dir, "total_effects_barplot_", exp_name, ".pdf", sep=""))
    plot_sens(final_plot_df_avg, "total_effects", "area", 
              paste(plot_dir, "total_effects_area_", exp_name, ".pdf", sep="")) 
}

plot_all_sens_GP = function(train_dir, param_ranges_file, plot_t, EIR_prev_tab) {
    load(param_ranges_file)
    file.names = dir(train_dir, pattern ="_sidx.RData", full.names = TRUE)
    final_plot_df = NULL
    points_df = NULL
    for(i in 1:length(file.names)){
        load(file.names[i])
        params = factor(rownames(param_ranges), levels = rownames(param_ranges))
        sens_df = cbind.data.frame(as.double(sobol_idx_list$EIR), str_to_title(sobol_idx_list$seasonality), sobol_idx_list$biting_pattern, params, sobol_idx_list$S_eff, sobol_idx_list$T_eff)
        final_plot_df = rbind.data.frame(final_plot_df, sens_df)
    }
    final_plot_df = final_plot_df[-which(final_plot_df$params == "EIR"),]
    colnames(final_plot_df) = c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects")
    final_plot_df$Seasonality = factor(final_plot_df$Seasonality, levels = c("Perennial", "Seasonal"))
    final_plot_df$Biting_pattern = paste(str_replace(final_plot_df$Biting_pattern, "_", " "), "biting")
    final_plot_df$Biting_pattern = factor(final_plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    
    final_plot_df$parameter = factor(str_replace(final_plot_df$parameter, "_", " "), levels = PARAMETER_NAMES)
    final_plot_df[which(final_plot_df$parameter == "Preprandial efficacy"), "parameter"] = "Efficacy"
    # Normalize effects for area plot
    final_plot_df_avg = (final_plot_df %>% group_by(EIR, Seasonality, Biting_pattern) %>% 
                             dplyr::mutate(main_effects = main_effects/sum(main_effects), 
                                           total_effects = total_effects/sum(total_effects))) 
    
    p = get_sens_plots(final_plot_df_avg, "total_effects", "area", EIR_prev_tab, plot_t) 
    return(p)
}

plot_single_sens_GP = function(train_dir, seasonality, biting, plot_val, plot_type,
                               param_ranges_file, plot_title, EIR_prev_tab, x_ticks) {
    load(param_ranges_file)
    file.names = dir(train_dir, pattern ="_sidx.RData", full.names = TRUE)
    final_plot_df = NULL
    points_df = NULL
    for(i in 1:length(file.names)){
        load(file.names[i])
        params = factor(rownames(param_ranges), levels = rownames(param_ranges))
        sens_df = cbind.data.frame(as.double(sobol_idx_list$EIR), str_to_title(sobol_idx_list$seasonality), sobol_idx_list$biting_pattern, params, sobol_idx_list$S_eff, sobol_idx_list$T_eff)
        final_plot_df = rbind.data.frame(final_plot_df, sens_df)
    }
    final_plot_df = final_plot_df[-which(final_plot_df$params == "EIR"),] 
    colnames(final_plot_df) = c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects")
    final_plot_df$Seasonality = factor(final_plot_df$Seasonality, levels = c("Perennial", "Seasonal"))
    final_plot_df$parameter = factor(str_replace(final_plot_df$parameter, "_", " "), levels = PARAMETER_NAMES)
    
    # Normalize effects for area plot
    final_plot_df_avg = (final_plot_df %>% group_by(EIR, Seasonality, Biting_pattern) %>% 
                             mutate(main_effects = main_effects/sum(main_effects), 
                                    total_effects = total_effects/sum(total_effects)))
    selected_points = final_plot_df_avg[which(final_plot_df_avg$Seasonality == seasonality & final_plot_df_avg$Biting_pattern == biting), ]
    selected_points$Biting_pattern = paste(str_replace(selected_points$Biting_pattern, "_", " "), "biting")
    selected_points$Biting_pattern = factor(selected_points$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    
    selected_points$EIR_trans = floor(selected_points$EIR) - (selected_points$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    selected_points = merge(selected_points, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    
    #### if plot Fig 2 then change to 24
    selected_points = selected_points[which(selected_points$EIR <= 26), ]
    inds = which(selected_points$EIR_trans %in% x_ticks)
    labs_x = unique(selected_points[inds, c("EIR_trans", "Median_2_10")])
    
    # Plotting
    if(plot_type == "barplot") {
        plotType = geom_bar(stat = "identity", alpha = 0.9) 
    } else {
        plotType = geom_area(alpha = 0.9)
    }
    
    if(plot_val == "main_effects") {
        y_val = "Main effects"
        y_val = "Relative importance"
    } else {
        y_val = "Total effects"
        y_val = "Relative importance"
    }
    intervention_colors = create_color_palette_sens()
    
    # Correct half-life
    selected_points[which(selected_points$parameter == "Halflife"), "parameter"] = "Half-life"
    
    p = ggplot(selected_points, aes(x = EIR, y = total_effects, group = parameter, fill = parameter)) +
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        plotType + theme(strip.background = element_rect(colour="white", fill="white")) +
        scale_fill_manual(name = "parameter", values = intervention_colors) +
        labs(x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), y = y_val, title = plot_title) + 
        guides(fill=guide_legend(title="")) + theme(legend.position="top") +
        scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100))
        
    #plot_file = paste(plot_dir, "single_sens_", exp_name, "_", seasonality, "_", biting, ".pdf")
    #ggsave(plot_file, width = 4.5, height = 3)

    return(list(p = p, selected_points = selected_points[,c("EIR", "Seasonality", "Biting_pattern", "parameter", "main_effects", "total_effects")]))
}

# # Only for testing:
# plot_dir = "~/MMC/TPP/figures/sensitivity_analysis/MAB_once_3_years/"
# 
# plot_sens_GP("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_6/sensitivity/", 
#                 plot_dir, "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData",
#                 "", "MAB_once_3years_avg_prev_6")

