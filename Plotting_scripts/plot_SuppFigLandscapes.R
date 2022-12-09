########################
# Plot supplementary figures with optimal landscapes
########################

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")

create_landscape_fig = function(sim_dir, plot_title) {
    param_ranges_file = paste0(sim_dir, "param_ranges.RData")
    load(param_ranges_file)
    
    follow_up_vec = c(4,6)
    follow_up_str = c("immediate follow-up", "late follow-up")
    CM_level = "MidCM"
    figs = vector('list', 2)
    
    for (ind in 1:2) {
        follow_up = follow_up_vec[ind]
        optim_dir = paste0(sim_dir, "gp_", follow_up, "/optimisation/")
        optim_df = build_opt_df(optim_dir)
        opt_params = unique(optim_df$Param_opt)
        p_array = vector('list', 3)
        i = 0
        for (param_opt in opt_params) {
            optim_df_part = optim_df[which(optim_df$CM_level == CM_level & optim_df$Param_opt == param_opt), ]
            # Remove values for EIR = 1
            if (length(which(optim_df_part$EIR == 1)) > 0) {
                optim_df_part = optim_df_part[-which(optim_df_part$EIR == 1), ]
            }
            if(nrow(optim_df_part) > 0)
            {
                optim_df_part = optim_df_part[-which(is.na(optim_df_part$opt_param)),]
                i = i + 1
                p_array[[i]] = plot_profiles_landscape(optim_df_part, param_ranges[param_opt, ], 
                                                       plot_dir, str_to_lower(param_opt), NULL)
            }
        }
        figs[[ind]] = ggarrange(plotlist = p_array, ncol = 3, nrow = 1, legend = "top")
        figs[[ind]] = annotate_figure(figs[[ind]], 
                                       top = text_grob(paste0(plot_title, ", ", follow_up_str[ind]),face = "bold"))
    }
    
    figure = ggarrange(plotlist = figs, ncol = 1, nrow = 2, labels = c("A", "B"))
    return(figure)
}

#### Create all supplementary figures with optimal landscapes
fig_l = create_landscape_fig("/scicore/home/smith/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/", 
                             "Anti-infective monoclonal (once per year)")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MAB_once.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("~/MMC/TPP/simulations/MAB_twice_sync_3years/", 
                             "Anti-infective monoclonal (twice per year)")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MAB_twice.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("~/MMC/TPP/simulations/ATSB_once_3years/", 
                             "Attractive targeted sugar baits (once per year)")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_ATSB_once.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("/scicore/home/smith/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/", 
                             "Attractive targeted sugar baits (twice per year)")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_ATSB_twice.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/", 
                             "Anti-infective vaccine")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_AIV.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/", 
                             "Transmission-blocking vaccine")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_TBV.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/", 
                             "Anti-infective monoclonal (once per year) + blood stage drug")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MDA_MAB_once.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("/scicore/home/smith/golmon00/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/", 
                             "Anti-infective monoclonal (twice per year) + blood stage drug")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MDA_MAB_twice.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("~/MMC/TPP/simulations/cohort_MDA_AIV_once_3years/", 
                             "Anti-infective vaccine + blood stage drug")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MDA_AIV.pdf", fig_l, width = 12, height = 9)

fig_l = create_landscape_fig("~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/", 
                             "Transmission-blocking vaccine + blood stage drug")
ggsave("~/MMC/TPP/figures/supp_optimization/landscape_MDA_TBV.pdf", fig_l, width = 12, height = 9)


