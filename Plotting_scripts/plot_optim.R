#############################
# plot optimization results
#############################

source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/color_palettes.R")
library(stringr)

# Construct a data frame with all the optimisation results for
# each model and optimisation setup (CM level, EIR level, variable to be optimised, etc.)
build_opt_df = function(opt_dir) {
    file.names = dir(opt_dir, pattern =".RData", full.names = TRUE)
    final_plot_df = NULL
    points_df = NULL
    for(i in 1:length(file.names)){
        load(file.names[i])
        opt_df = cbind.data.frame(str_to_title(opt_obj$seasonality), opt_obj$biting_pattern, 
                                  opt_obj$opt_param, opt_obj$CM_level, opt_obj$table)
        final_plot_df = rbind.data.frame(final_plot_df, opt_df)
    }
    colnames(final_plot_df) = c("Seasonality", "Biting_pattern", "Param_opt", "CM_level", colnames(opt_obj$table))
    final_plot_df$Seasonality = factor(final_plot_df$Seasonality, levels = c("Perennial", "Seasonal"))
    final_plot_df$Biting_pattern = paste(str_replace(final_plot_df$Biting_pattern, "_", " "), "biting")
    final_plot_df$Biting_pattern = factor(final_plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    return(final_plot_df)
}

# Plot the landscape of optimal profiles 
plot_profiles_landscape = function(final_plot_df, opt_param_ranges, 
                                   exp_name, param_to_opt, x_ticks) {
    
    if(param_to_opt == "halflife") {
        param_to_opt = "halflife or duration"
        final_plot_df$opt_param = round(final_plot_df$opt_param*12)
        opt_param_ranges = round(opt_param_ranges * 12)
        opt_param_ranges[1] = 0
        n = paste("Minimum half-life value (months)", sep="\n")
        low_color = "#c7e9b4"
        high_color = "#6CC3B9"
    } else if(param_to_opt == "efficacy") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum", param_to_opt, "value (%)", sep="\n")
        low_color = "#d0d1e6"
        high_color = "#3792BF"
    } else if(param_to_opt == "coverage") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum", param_to_opt, "value (%)", sep="\n")
        low_color = "#9ecae1"
        high_color = "#0D539C"
    }
    
    # select_ind = which(!is.na(final_plot_df$opt_param) & !is.na(final_plot_df$opt_param_sd_plus) & 
    #                        !is.na(final_plot_df$opt_param_sd_minus)) 
    #& !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    plot_df = final_plot_df#[select_ind,]
    
    p = ggplot(plot_df, aes(EIR, prev_red, fill= opt_param)) + geom_tile() + 
        theme_bw(base_size=11.5) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_fill_gradient(low = low_color, high = high_color, na.value = "white", 
                            name = n, limits = opt_param_ranges ) +
        facet_wrap(~Seasonality + Biting_pattern, scales='free_x') + labs( x = "Transmission level", y = "Minimum reduction (%)") +
        theme(panel.background = element_rect(fill = 'white')) + 
        theme(strip.background = element_rect(colour="white", fill="white")) 
        # scale_x_continuous(breaks = x_ticks, labels = x_ticks) + xlim(2, 25)
    # plot_file = paste(plot_dir, "heatmap_", exp_name, ".pdf", sep= "")
    # ggsave(filename = plot_file, plot = p, width = 5, height = 3)
    return(p)
}

# Extract optimal profiles for a given prevalence reduction objective and a list of experiments
build_profiles_df = function(list_sim_dir, follow_up, prev_threshold) {
    final_plot_df = NULL
    if (length(list_sim_dir) < 1) {
        print("No simulation experiments provided.")
        return(0)
    }
    for(sim_dir in list_sim_dir) {
        opt_dir = paste0(sim_dir, "gp_", follow_up, "/optimisation/")
        file.names = dir(opt_dir, pattern =".RData", full.names = TRUE)
        exp_name = basename(sim_dir)
        points_df = NULL
        print(length(file.names))
        if(length(file.names) > 0) {
            for(i in 1:length(file.names)){
                load(file.names[i])
                opt_df = cbind.data.frame(str_to_title(opt_obj$seasonality), opt_obj$biting_pattern, 
                                          opt_obj$opt_param, opt_obj$CM_level, opt_obj$table)
                select_ind = which(opt_df$prev_red == prev_threshold)
                if (length(select_ind) > 0) {
                    final_plot_df = rbind.data.frame(final_plot_df, cbind.data.frame(opt_df[select_ind, ], sim_dir))
                }
            }
        }
    }
    colnames(final_plot_df) = c("Seasonality", "Biting_pattern", "Param_opt", "CM_level", colnames(opt_obj$table), "experiment")
    final_plot_df$Seasonality = factor(final_plot_df$Seasonality, levels = c("Perennial", "Seasonal"))
    final_plot_df$Biting_pattern = paste(str_replace(final_plot_df$Biting_pattern, "_", " "), "biting")
    final_plot_df$Biting_pattern = factor(final_plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    # Remove empty entries
    final_plot_df = final_plot_df[-which(is.na(final_plot_df$opt_param)),]
    return(final_plot_df)
}

plot_profiles_df = function(profiles_df) {
    
    # Update the y axis scale depending on the displayed parameter
    ind_hl = which(profiles_df$Param_opt == "Halflife")
    ind_eff_cov = which(profiles_df$Param_opt == "Efficacy" | profiles_df$Param_opt == "Coverage")
    profiles_df[ind_hl, "opt_param"] = round(profiles_df[ind_hl, "opt_param"]*12)
    profiles_df[ind_hl, "opt_param_sd_plus"] = round(profiles_df[ind_hl, "opt_param_sd_plus"]*12)
    profiles_df[ind_hl, "opt_param_sd2_plus"] = round(profiles_df[ind_hl, "opt_param_sd2_plus"]*12)
    profiles_df[ind_hl, "opt_param_sd_minus"] = round(profiles_df[ind_hl, "opt_param_sd_minus"]*12)
    profiles_df[ind_hl, "opt_param_sd2_minus"] = round(profiles_df[ind_hl, "opt_param_sd2_minus"]*12)
    profiles_df[ind_eff_cov, "opt_param"] = round(profiles_df[ind_eff_cov, "opt_param"]*100)
    profiles_df[ind_eff_cov, "opt_param_sd_plus"] = round(profiles_df[ind_eff_cov, "opt_param_sd_plus"]*100)
    profiles_df[ind_eff_cov, "opt_param_sd2_plus"] = round(profiles_df[ind_eff_cov, "opt_param_sd2_plus"]*100)
    profiles_df[ind_eff_cov, "opt_param_sd_minus"] = round(profiles_df[ind_eff_cov, "opt_param_sd_minus"]*100)
    profiles_df[ind_eff_cov, "opt_param_sd2_minus"] = round(profiles_df[ind_eff_cov, "opt_param_sd2_minus"]*100)
    
    select_ind = which(!is.na(profiles_df$opt_param) & !is.na(profiles_df$opt_param_sd_plus) & 
                           !is.na(profiles_df$opt_param_sd_minus)) 
                       #& !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    plot_df = profiles_df[select_ind,]
    plot_df = plot_df[which(plot_df$Biting_pattern == "High indoor biting"),]
    
    # Calculate variance of optimum solutions
    plot_df$sd = apply(cbind(plot_df$opt_param, plot_df$opt_param_sd2_minus, 
                             plot_df$opt_param_sd2_plus, 
                             plot_df$opt_param_sd_minus, 
                             plot_df$opt_param_sd_plus), 1, function(x) sd(x, na.rm = TRUE))
    plot_df$sd[which(is.na(plot_df$sd))] = 0
    plot_df$val_min = plot_df$opt_param - plot_df$sd
    plot_df$val_max = plot_df$opt_param + plot_df$sd

    plot_colors = create_color_palette_deployment()

    p = ggplot(plot_df, aes(x = EIR, y = opt_param, color = factor(experiment), fill = factor(experiment))) + 
        geom_line() +
        labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)"))) +
              # y = paste0("Minimum ", str_to_lower(Param_opt))) +
        # geom_point(size = 1, aes(group = experiment), position=position_dodge(0.2)) + 
        geom_pointrange(aes(ymin = val_min, ymax = val_max), size = 0.1) +  
        geom_ribbon(aes(ymin = val_min, ymax = val_max), alpha=.1, linetype = 0) +
        theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality) + labs( x = "Transmission level", y = "Minimum profile") + 
        theme(panel.background = element_rect(fill = 'white')) + 
        theme(strip.background = element_rect(colour="white", fill="white")) 
        # scale_fill_manual(name = "", values = plot_colors) + 
        # scale_color_manual(name = "", values = plot_colors)
        # geom_errorbar(aes(ymin=opt_param_sd_plus, ymax=opt_param_sd_minus), width=.2,
        #               position=position_dodge(0.2))
    return(p)
}

plot_profile_df = function(profiles_df, opt_param, CM_level, seas, biting, 
                           plot_title, EIR_prev_tab, plot_colors, x_ticks, y_lims) {
    # select_ind = which(!is.na(profiles_df$opt_param) & !is.na(profiles_df$opt_param_sd_plus) &
    #                        !is.na(profiles_df$opt_param_sd_minus))
    #& !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    # Remove entries for EIR = 1
    if (length(which(profiles_df$EIR == 1) > 0)) {
        profiles_df = profiles_df[-which(profiles_df$EIR == 1), ]
    }
    
    # Update the y axis scale depending on the displayed parameter
    if(opt_param == "Halflife") {
        # param_to_opt = "Half-life or duration"
        profiles_df$opt_param = round(profiles_df$opt_param*12)
        extra = "\n(months)"
        opt_param_name = "half-life"
        mult = 1
    } else if(opt_param == "Efficacy") {
        profiles_df$opt_param = profiles_df$opt_param*100
        extra = "(%)"
        opt_param_name = "efficacy"
        mult = 100
    } else if(opt_param == "Preprandial_efficacy") {
        profiles_df$opt_param = profiles_df$opt_param*100
        opt_param_name = "efficacy"
        extra = "(%)"
        mult = 100
    } else if(opt_param == "Coverage") {
        profiles_df$opt_param = profiles_df$opt_param*100
        opt_param_name = "coverage"
        extra = "(%)"
        mult = 100
    }
    
    # Extract the relevant values
    plot_df = profiles_df[which(profiles_df$Param_opt == opt_param &
                                    profiles_df$CM_level == CM_level &
                                    profiles_df$Seasonality == seas &
                                    profiles_df$Biting_pattern == biting),]
    
    # Calculate variance of optimum solutions
    plot_df$sd = apply(cbind(plot_df$opt_param, plot_df$opt_param_sd2_minus*mult, 
                      plot_df$opt_param_sd2_plus*mult, 
                      plot_df$opt_param_sd_minus*mult, 
                      plot_df$opt_param_sd_plus*mult), 1, function(x) sd(x, na.rm = TRUE))
    plot_df$sd[which(is.na(plot_df$sd))] = 0
    plot_df$val_min = plot_df$opt_param - plot_df$sd
    plot_df$val_max = plot_df$opt_param + plot_df$sd
    
    
    # Modify x axis labels to median prevalence
    plot_df$EIR_trans = floor(plot_df$EIR) - (plot_df$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    plot_df = merge(plot_df, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    plot_df = plot_df[which(plot_df$EIR <= 16), ]
    inds = which(plot_df$EIR_trans %in% x_ticks)
    labs_x = unique(plot_df[inds, c("EIR_trans", "Median_2_10")])
    
    # Plotting
    dep_names = define_deployment_names()
    plot_df$experiment = apply(as.data.frame(plot_df$experiment), 1, function(x) 
        dep_names$dep_exp_names[which(dep_names$dep_exp_folders == x)])
    # names(plot_colors) = levels(factor(unique(plot_df$experiment), levels = unique(plot_df$experiment)))
    plot_colors = create_color_palette_deployment()
    plot_colors = plot_colors[which(names(plot_colors) %in% plot_df$experiment)]
    p = ggplot(plot_df, aes(x = EIR, y = opt_param, color = factor(experiment), fill = factor(experiment))) + 
        #geom_point(size = 1, aes(group = factor(experiment))) +
        geom_line() +
        theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")),
                y = paste0("Minimum ", str_to_lower(opt_param_name)," ", extra), title = plot_title) +
        theme(panel.background = element_rect(fill = 'white')) +
        theme(strip.background = element_rect(colour="white", fill="white")) +
        geom_pointrange(aes(ymin = val_min, ymax = val_max), size = 0.1) + #shape = factor(experiment)
        geom_ribbon(aes(ymin = val_min, ymax = val_max), alpha=.1, linetype = 0) +
        scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100)) +
        scale_fill_manual(name = "", values = plot_colors) + 
        theme(plot.title = element_text(size=11)) +
        # scale_shape_manual(name = "", values = c(0, 3, 2, 4)) + 
        scale_color_manual(name = "", values = plot_colors) + ylim(y_lims) + 
        theme(text = element_text(family = "Arial")) 
    return(p)
}

plot_single_landscape = function(final_plot_df, CM_level, param_to_opt, opt_param_ranges, 
                                 seas, biting, plot_title, EIR_prev_tab) {
    if(param_to_opt == "Halflife") {
        # param_to_opt = "Half-life or duration"
        final_plot_df$opt_param = round(final_plot_df$opt_param*12)
        opt_param_ranges = round(opt_param_ranges * 12)
        opt_param_ranges[1] = 0
        n = paste("Minimum", "half-life", "(months) ", sep=" ")
        low_color = "#c7e9b4"
        high_color = "#6CC3B9"
    } else if(param_to_opt == "Efficacy") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum", str_to_lower(param_to_opt), "(%) ", sep=" ")
        low_color = "#d0d1e6"
        high_color = "#3792BF"
    } else if(param_to_opt == "Coverage") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum", str_to_lower(param_to_opt), "(%) ", sep=" ")
        low_color = "#9ecae1"
        high_color = "#0D539C"
    }
    
    select_ind = which(!is.na(final_plot_df$opt_param) & !is.na(final_plot_df$opt_param_sd_plus) &
                           !is.na(final_plot_df$opt_param_sd_minus))
                        # & !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    plot_df = final_plot_df[select_ind,]
    optim_df_part = plot_df[which(plot_df$CM_level == CM_level & plot_df$Param_opt == param_to_opt &
                                       plot_df$Seasonality == seas & plot_df$Biting_pattern == biting), ]
    # Remove values for EIR = 1
    optim_df_part = optim_df_part[-which(optim_df_part$EIR == 1), ]
    
    # Modify x axis labels to median prevalence
    optim_df_part$EIR_trans = floor(optim_df_part$EIR) - (optim_df_part$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    optim_df_part = merge(optim_df_part, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    optim_df_part = optim_df_part[which(optim_df_part$EIR <= 20), ]
    # x_ticks = c(2, 4, 6, 8, 10, 13, 15, 20)
    x_ticks = c(2, 6, 10, 14)
    inds = which(optim_df_part$EIR_trans %in% x_ticks)
    labs_x = unique(optim_df_part[inds, c("EIR_trans", "Median_2_10")])
    
    p = ggplot(optim_df_part, aes(EIR, prev_red, fill= opt_param)) + geom_tile() + 
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_fill_gradient(low = low_color, high = high_color, na.value = "white", 
                            name = n, limits = opt_param_ranges) +
        labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), 
              y = expression(paste("Minimum ", italic("Pf"), "PR"["0\u201399"], " reduction (%)")),
              title = plot_title) +
        theme(panel.background = element_rect(fill = 'white')) + 
        theme(strip.background = element_rect(colour="white", fill="white")) +
        scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100))
    return(p)
}

plot_single_landscape_row = function(final_plot_df, CM_level, param_to_opt, opt_param_ranges, 
                                 seas, biting, plot_title, EIR_prev_tab, prev_goal) {
    if(param_to_opt == "Halflife") {
        # param_to_opt = "Half-life or duration"
        final_plot_df$opt_param = round(final_plot_df$opt_param*12)
        opt_param_ranges = round(opt_param_ranges * 12)
        opt_param_ranges[1] = 0
        n = paste("Minimum\n", "half-life", " (months) ", sep="")
        low_color = "#c7e9b4"
        high_color = "#6CC3B9"
    } else if(param_to_opt == "Efficacy") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum\n", str_to_lower(param_to_opt), " (%) ", sep="")
        low_color = "#d0d1e6"
        high_color = "#3792BF"
    } else if(param_to_opt == "Preprandial_efficacy") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        param_to_opt = "Efficacy"
        n = paste("Minimum\n", str_to_lower(param_to_opt), " (%) ", sep="")
        low_color = "#d0d1e6"
        high_color = "#3792BF"
    } else if(param_to_opt == "Coverage") {
        final_plot_df$opt_param = final_plot_df$opt_param*100
        opt_param_ranges = opt_param_ranges * 100
        n = paste("Minimum\n", str_to_lower(param_to_opt), " (%) ", sep="")
        low_color = "#9ecae1"
        high_color = "#0D539C"
    }
    
    # Correction for eave tubes:
    final_plot_df[which(final_plot_df$Param_opt == "Preprandial_efficacy"), "Param_opt"] = "Efficacy"
    
    
    # select_ind = which(!is.na(final_plot_df$opt_param) & !is.na(final_plot_df$opt_param_sd_plus) &
    #                        !is.na(final_plot_df$opt_param_sd_minus))
    # & !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    plot_df = final_plot_df[,]
    optim_df_part = plot_df[which(plot_df$CM_level == CM_level & plot_df$Param_opt == param_to_opt &
                                      plot_df$Seasonality == seas & plot_df$Biting_pattern == biting), ]
    # Remove values for EIR = 1
    if (length(which(optim_df_part$EIR == 1)) > 0) {
        optim_df_part = optim_df_part[-which(optim_df_part$EIR == 1), ]
    }
    
    # Modify x axis labels to median prevalence
    optim_df_part$EIR_trans = floor(optim_df_part$EIR) - (optim_df_part$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    optim_df_part = merge(optim_df_part, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    optim_df_part = optim_df_part[which(optim_df_part$EIR <= 16), ] 
    x_ticks = c(2, 6, 10, 13, 15)
    inds = which(optim_df_part$EIR_trans %in% x_ticks)
    labs_x = unique(optim_df_part[inds, c("EIR_trans", "Median_2_10")])
    
    p = ggplot(optim_df_part, aes(EIR, prev_red, fill= opt_param)) + geom_tile() + 
        theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        scale_fill_gradient(low = low_color, high = high_color, na.value = "white", 
                            name = n, limits = opt_param_ranges ) +
        labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), y = "Minimum reduction (%)",
              title = plot_title) + ylim(30, 100) +
        theme(plot.title = element_text(size=11)) +
        theme(panel.background = element_rect(fill = 'white')) + 
        theme(strip.background = element_rect(colour="white", fill="white")) +
        scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100)) +
        theme(text = element_text(family = "Arial")) 
    if(!is.null(prev_goal)) {
        p = p + geom_hline(yintercept=prev_goal, linetype="dashed")
    }
    return(p)
}

plot_single_profile = function(profiles_df, prev_red, CM_level, param_to_opt, seas, biting, EIR_prev_tab) {
    select_ind = which(!is.na(profiles_df$opt_param) & !is.na(profiles_df$opt_param_sd_plus) & 
                           !is.na(profiles_df$opt_param_sd_minus)) 
    #& !is.na(profiles_df$opt_param_sd2_minus) & !is.na(profiles_df$opt_param_sd2_plus))
    plot_df = profiles_df[select_ind,]
    optim_df_part = plot_df[which(plot_df$CM_level == CM_level & plot_df$Param_opt == param_to_opt &
                                      plot_df$Seasonality == seas & plot_df$Biting_pattern == biting &
                                      plot_df$prev_red == prev_red), ]
    
    # Modify x axis labels to median prevalence
    optim_df_part$EIR_trans = floor(optim_df_part$EIR) - (optim_df_part$EIR == 25)
    EIR_prev_tab$EIR_trans = EIR_prev_tab$EIR
    EIR_prev_tab$EIR = NULL
    optim_df_part = merge(optim_df_part, EIR_prev_tab, by = c("Seasonality", "Biting_pattern", "EIR_trans"))
    optim_df_part = optim_df_part[which(optim_df_part$EIR <= 20), ]
    # x_ticks = c(2, 4, 6, 8, 10, 12, 15, 20)
    # inds = which(optim_df_part$EIR_trans %in% x_ticks)
    labs_x = unique(optim_df_part[, c("EIR_trans", "Median_2_10")])
    
    p = ggplot(optim_df_part, aes(x = EIR, y = opt_param*100)) + 
        # geom_point(size = 1, position=position_dodge(0.2)) + 
        geom_pointrange(aes(ymin = opt_param_sd_plus*100, ymax = opt_param_sd_minus*100), size = 0.1) + #shape = factor(experiment)
        geom_ribbon(aes(ymin = opt_param_sd_plus*100, ymax = opt_param_sd_minus*100), alpha=.2, linetype = 0) +
        geom_line() +
        #geom_line(aes(group = experiment)) + 
        theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs( x = expression(paste("Median ", italic("Pf"), "PR"["2\u201310"], " (%)")), y = "Minimum\nprofile (%)") + 
        theme(panel.background = element_rect(fill = 'white')) + 
        theme(strip.background = element_rect(colour="white", fill="white")) +
        # geom_errorbar(aes(ymin=opt_param_sd_plus, ymax=opt_param_sd_minus), width=.2,
        #               position=position_dodge(0.2)) + 
        scale_x_continuous(breaks=labs_x[,1], labels=round(labs_x[,2]*100))
    return(p)
}

