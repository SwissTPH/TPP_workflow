############################
# Plot profiles of interventions across settings
###########################

########################
# Plot supplementary figures with optimal landscapes
########################

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")

construct_profile_fig_experiment = function(exp_list, exp_title, prev_threshold, EIR_prev_tab, lims_hl) {
    # exp_name = "Anti-infective monoclonal + blood stage drug (once per year)"
    vec_follow_up = c(4, 6)
    vec_follow_up_names = c("immediate follow-up", "late follow-up")
    figure_opt = vector('list', 2)
    
    for (follow_up in 1:length(vec_follow_up)) {
        profiles_df = build_profiles_df(exp_list, vec_follow_up[follow_up], prev_threshold)
        opt_params = unique(profiles_df$Param_opt)
        if ("Preprandial_efficacy" %in% opt_params) {
            opt_params = c("Coverage", "Preprandial_efficacy", "Halflife")
        } else {
            opt_params = c("Coverage", "Efficacy", "Halflife")
        }
        figure_param = vector('list', length(opt_params))
        i = 0
        for (param_opt in opt_params) {
            i = i + 1
            if (param_opt == "Halflife") {
                y_lim = lims_hl
            } else {
                y_lim = c(0, 111)
            }
            
            if (follow_up == 1) {
                ticks_x = c(2, 6, 10, 15, 20)
            } else {
                if (param_opt == "Halflife") {
                    ticks_x = c(2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
                } else {
                    ticks_x = c(2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
                }
            }
            optim_df_part = profiles_df[which(profiles_df$CM_level == "MidCM" & profiles_df$Param_opt == param_opt), ]
            profile_plot_seas = plot_profile_df(profiles_df, param_opt, "MidCM", 
                                                 "Seasonal", "High indoor biting", 
                                                 "Seasonal, \nhigh indoor biting", EIR_prev_tab,
                                                 c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                                 x_ticks = ticks_x, y_lims = y_lim) + 
                theme(axis.title = element_text(size=10), plot.title = element_text(size=11))
                
            profile_plot_per = plot_profile_df(profiles_df, param_opt, "MidCM", 
                                                "Perennial", "High indoor biting", 
                                                "Perennial, \nhigh indoor biting", EIR_prev_tab,
                                                c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                                x_ticks = ticks_x, y_lims = y_lim) + 
                theme(axis.title = element_text(size=10), plot.title = element_text(size=11))
            
            if (i == 3) {
                figure_param[[i]] = ggarrange(profile_plot_seas, profile_plot_per, ncol = 2, 
                                              nrow = 1, common.legend = TRUE, legend = "right")
            } else {
                figure_param[[i]] = ggarrange(profile_plot_seas, profile_plot_per, ncol = 2, 
                                              nrow = 1, legend = "none")
            }
            
            
            
        }
        figure_opt[[follow_up]] = ggarrange(plotlist = figure_param, ncol = 3, nrow = 1, 
                                            widths = c (1, 1, 1.55), labels = LETTERS[(follow_up^2):(follow_up*3)])
        figure_opt[[follow_up]] = annotate_figure(figure_opt[[follow_up]],
                                                  top = text_grob(paste0(exp_title, ", ", vec_follow_up_names[follow_up]), face = "bold"))
    }
    figure = ggarrange(plotlist = figure_opt, 
                       ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
    return(figure)
}

# Load the EIR_to_prevalence transformations
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)

# Monoclonal antibodies
exp_list = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",
             "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_twice_sync_3years/",
             "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_once/",
             "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_twice/")
prev_threshold = 70
fig_MAB = construct_profile_fig_experiment(exp_list, "Monoclonal antibodies", prev_threshold, EIR_prev_tab, c(0, 14))
ggsave("~/MMC/TPP/figures/supp_optimization/profiles_MAB.pdf",  plot = fig_MAB,  width = 12, height = 6)

# Anti-infective vaccines
exp_list = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/",
             "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/")
prev_threshold = 70
fig_AIV = construct_profile_fig_experiment(exp_list, "Anti-infective vaccine", prev_threshold, EIR_prev_tab, c(0, 90))
ggsave("~/MMC/TPP/figures/supp_optimization/profiles_AIV.pdf",  plot = fig_AIV,  width = 12, height = 6)

# Transmission-blocking vaccines
exp_list = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/",
             "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/")
prev_threshold = 70
fig_TBV = construct_profile_fig_experiment(exp_list, "Transmission-blocking vaccine", prev_threshold, EIR_prev_tab, c(0, 90))
ggsave("~/MMC/TPP/figures/supp_optimization/profiles_TBV.pdf",  plot = fig_TBV,  width = 12, height = 6)

# ATSB
exp_list = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/ATSB_once_3years/",
             "/scicore/home/smith/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/")
prev_threshold = 70
fig_ATSB = construct_profile_fig_experiment(exp_list, "Attractive targeted sugar baits", prev_threshold, EIR_prev_tab, c(0, 14))
ggsave("~/MMC/TPP/figures/supp_optimization/profiles_ATSB.pdf",  plot = fig_ATSB,  width = 12, height = 6)

# Eave tubes
exp_list = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")
prev_threshold = 70
fig_eave = construct_profile_fig_experiment(exp_list, "Eave tubes", prev_threshold, EIR_prev_tab, c(0, 90))
ggsave("~/MMC/TPP/figures/supp_optimization/profiles_eave.pdf",  plot = fig_eave,  width = 12, height = 6)
