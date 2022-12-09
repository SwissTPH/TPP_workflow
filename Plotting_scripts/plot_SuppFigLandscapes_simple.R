########################
# Plot supplementary figures with optimal landscapes
########################

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")

construct_landscape_fig_experiment = function(exp_dir, exp_name, EIR_prev_tab, labs) {
    param_ranges_file = paste0(exp_dir, "param_ranges.RData")
    load(param_ranges_file)
    vec_follow_up = c(4, 6)
    vec_follow_up_names = c("immediate follow-up", "late follow-up")
    figure_opt = vector('list', 2)
    
    for (follow_up in 1:length(vec_follow_up)) {
        optim_dir = paste0(exp_dir,"gp_", vec_follow_up[follow_up], "/optimisation/")
        optim_df = build_opt_df(optim_dir)
        opt_params = unique(optim_df$Param_opt)
        if ("Preprandial_efficacy" %in% opt_params) {
            opt_params = c("Coverage", "Preprandial_efficacy", "Halflife")
        }
        figure_param = vector('list', length(opt_params))
        i = 0
        for (param_opt in opt_params) {
            i = i + 1
            opt_plot_seas = plot_single_landscape_row(optim_df, "MidCM", param_opt, 
                                                      param_ranges[param_opt, ], "Seasonal", 
                                                      "High indoor biting", 
                                                      "Seasonal, \nhigh indoor biting", EIR_prev_tab)
            opt_plot_per = plot_single_landscape_row(optim_df, "MidCM", param_opt, 
                                                     param_ranges[param_opt, ], "Perennial", 
                                                      "High indoor biting", 
                                                     "Perennial, \nhigh indoor biting", EIR_prev_tab)
            figure_param[[i]] = ggarrange(opt_plot_seas, opt_plot_per, ncol = 2, 
                                          nrow = 1, common.legend = TRUE, legend = "top")
        }
        figure_opt[[follow_up]] = ggarrange(plotlist = figure_param, ncol = 3, nrow = 1)
        figure_opt[[follow_up]] = annotate_figure(figure_opt[[follow_up]], 
                                                   top = text_grob(paste0(exp_name, ", ", vec_follow_up_names[follow_up]), face = "bold"))
    }
    figure = ggarrange(plotlist = figure_opt, 
                       ncol = 1, nrow = 2, labels = labs)
    return(figure)
}

# Load the EIR_to_prevalence transformations
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)


# # Human-side interventions
# # MABs once
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/"
exp_name = "Anti-infective monoclonal (once per year)"
f_MAB_once = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A", "B")) 

# # MABs once + MDA
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/"
exp_name = "Anti-infective monoclonal + blood stage drug (once per year)"
f_MAB_MDA_once = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("C", "D")) 

figure_MAB = ggarrange(f_MAB_once, f_MAB_MDA_once, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_MAB_once.pdf", 
       figure_MAB, width = 12, height = 12)

##########################
# # MABs twice
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_twice_sync_3years/"
exp_name = "Anti-infective monoclonal (twice per year)"
f_MAB_twice = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A","B")) 

# # MABs twice + MDA
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/"
exp_name = "Anti-infective monoclonal + blood stage drug (twice per year)"
f_MAB_MDA_twice = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("C", "D")) 

figure_MAB_MDA = ggarrange(f_MAB_twice, f_MAB_MDA_twice, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_MAB_twice.pdf", 
       figure_MAB_MDA, width = 12, height = 12)

##########################
# # AIV 
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/"
exp_name = "Anti-infective vaccine (once per year)"
f_AIV = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A", "B")) 

# # AIV + MDA
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/"
exp_name = "Anti-infective vaccine + blood stage drug (once per year)"
f_AIV_MDA = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("C", "D")) 

figure_vacc = ggarrange(f_AIV, f_AIV_MDA, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_AIV.pdf", 
       figure_vacc, width = 12, height = 12)


##########################
# # TBV 
exp_dir ="/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/"
exp_name = "Transmisison-blocking vaccine (once per year)"
f_TBV = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A", "B")) 

# # TBV + MDA
exp_dir ="/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/"
exp_name = "Transmisison-blocking vaccine + blood stage drug (once per year)"
f_TBV_MDA = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("C", "D")) 

figure_vacc_MDA = ggarrange(f_TBV, f_TBV_MDA, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_TBV.pdf", 
       figure_vacc_MDA, width = 12, height = 12)

##########################
# # ATSB once
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/"
exp_name = "Attractive targeted sugar baits (once per year)"
f_ATSB_once = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A", "B")) 

# # ATSB twice
exp_dir ="/scicore/home/smith/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/"
exp_name = "Attractive targeted sugar baits (twice per year)"
f_ATSB_twice = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("C", "D")) 

figure_ATSB = ggarrange(f_ATSB_once, f_ATSB_twice, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_ATSB.pdf", 
       figure_ATSB, width = 12, height = 12)

##########################
# # Eave tubes
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/"
exp_name = "Eave tubes (once per year)"
f_eave = construct_landscape_fig_experiment(exp_dir, exp_name, EIR_prev_tab, c("A", "B")) 

ggsave("~/MMC/TPP/figures/supp_optimization/landscape_simple_eave.pdf", 
       f_eave, width = 12, height = 6)





