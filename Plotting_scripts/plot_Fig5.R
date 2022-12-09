##################################
# Plot the lower panel of Figure 1 - results for PEV
#
# 01.04.2020
# monica.golumbeanu@unibas.ch
#################################
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/plot_optim.R")

plot_landscape_row = function(optim_dir, param_ranges_file, EIR_prev_tab, title_vec, prev_goal) {
    optim_df = build_opt_df(optim_dir)
    opt_params = unique(optim_df$Param_opt)
    load(param_ranges_file)
    p_array = vector('list', length(opt_params))
    i = 0
    for (param_opt in opt_params) {
        i = i + 1
        p_array[[i]] = plot_single_landscape_row(optim_df, "MidCM", param_opt, param_ranges[param_opt, ], 
                                                 "Seasonal", "High indoor biting", title_vec[i], EIR_prev_tab,prev_goal)
    }
    figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 1, legend = "right")
    return(figure)
}

# Common files:
EIR_prev_tab = read.table("/scicore/home/penny/golmon00/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)

prev_threshold = 70
#### Landscape plots
# MAB
optim_dir1 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/optimisation/"
param_ranges_file1 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
figure_MAB_once = plot_landscape_row(optim_dir1, param_ranges_file1, EIR_prev_tab, 
                                     c("e = 85%, h = 4mos", "c = 60%, h = 4mos", "e = 85%, c = 60%"), prev_threshold) 
figure_MAB_once = annotate_figure(figure_MAB_once, 
                                  top = text_grob("Anti-infective monoclonal: landscapes of minimum properties", face = "bold"))

# PEV
optim_dir2 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/optimisation/"
param_ranges_file2 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/param_ranges.RData"
figure_PEV_once = plot_landscape_row(optim_dir2, param_ranges_file2, EIR_prev_tab, 
                                     c("e = 85%, h = 7mos","c = 60%, h = 7mos","e = 85%, c = 60%"), prev_threshold)
figure_PEV_once = annotate_figure(figure_PEV_once,
                                  top = text_grob("Anti-infective vaccine: landscapes of minimum properties", face = "bold"))

# TBV
optim_dir3 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/gp_4/optimisation/"
param_ranges_file3 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/param_ranges.RData"
figure_TBV_once = plot_landscape_row(optim_dir3, param_ranges_file3, EIR_prev_tab, 
                                     c("e = 85%, h = 7mos","c = 60%, h = 7mos","e = 85%, c = 60%"), prev_threshold)
figure_TBV_once = annotate_figure(figure_TBV_once, 
                                  top = text_grob("Transmission-blocking vaccine: landscapes of minimum properties", face = "bold"))

#### Profile plots
exp_list1 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/PEV_once_3years/", 
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/")
exp_list2 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/TBV_once_3years/",
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/")
exp_list3 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/",
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_twice_sync_3years/",
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_once/",
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_MAB_twice/")
# PEV
follow_up = 4
profiles_df1 = build_profiles_df(exp_list1, follow_up, prev_threshold)
profile_PEV_cov = plot_profile_df(profiles_df1, "Coverage", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "e = 85%, h = 7mos", EIR_prev_tab,
                                  c("#276419", "#1B9E77"), 
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_PEV_eff = plot_profile_df(profiles_df1, "Efficacy", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "c = 60%, h = 7mos", EIR_prev_tab,
                                  c("#276419", "#1B9E77"), 
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_PEV_hl = plot_profile_df(profiles_df1, "Halflife", "MidCM", 
                                 "Seasonal", "High indoor biting", 
                                 "e = 85%, c = 60%", EIR_prev_tab,
                                 c("#1B9E77", "#276419"),
                                 x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0, 85))
# TBV
profiles_df2 = build_profiles_df(exp_list2, follow_up, prev_threshold)
profile_TBV_eff = plot_profile_df(profiles_df2, "Efficacy", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "c = 60%, h = 7mos", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0, 111))

profile_TBV_cov = plot_profile_df(profiles_df2, "Coverage", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "e = 85%, h = 7mos", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_TBV_hl = plot_profile_df(profiles_df2, "Halflife", "MidCM", 
                                 "Seasonal", "High indoor biting", 
                                 "e = 85%, c = 60%", EIR_prev_tab,
                                 c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                 x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,85))

# MAB
profiles_df3 = build_profiles_df(exp_list3, follow_up, prev_threshold)
profile_MAB_eff = plot_profile_df(profiles_df3, "Efficacy", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "c = 60%, h = 4mos", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 3, 4, 5, 6, 8, 10, 12, 15, 18, 20), y_lims = c(0, 111))

profile_MAB_cov = plot_profile_df(profiles_df3, "Coverage", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "e = 85%, h = 4mos", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_MAB_hl = plot_profile_df(profiles_df3, "Halflife", "MidCM", 
                                 "Seasonal", "High indoor biting", 
                                 "e = 85%, c = 60%", EIR_prev_tab,
                                 c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                 x_ticks = c(2, 4, 6, 8, 10, 12, 15, 18, 20), y_lims = c(0,12.5))


### Construct the final figure
profile_MAB_once = ggarrange(profile_MAB_cov, profile_MAB_eff, profile_MAB_hl, ncol = 3, nrow = 1, 
                             legend = "right")
profile_MAB_once = annotate_figure(profile_MAB_once, 
                                   top = text_grob("Anti-infective monoclonal: minimum profiles of properties", face = "bold"))

profile_PEV_once = ggarrange(profile_PEV_cov, profile_PEV_eff, profile_PEV_hl, ncol = 3, nrow = 1, 
                             legend = "right")
profile_PEV_once = annotate_figure(profile_PEV_once, 
                                   top = text_grob("Anti-infective vaccine: minimum profiles of properties", face = "bold"))

profile_TBV_once = ggarrange(profile_TBV_cov, profile_TBV_eff, profile_TBV_hl, ncol = 3, nrow = 1, 
                             legend = "right")
profile_TBV_once = annotate_figure(profile_TBV_once, 
                                   top = text_grob("Transmission-blocking vaccine: minimum profiles of properties", face = "bold"))

figure = ggarrange(figure_MAB_once, profile_MAB_once, figure_PEV_once, profile_PEV_once, figure_TBV_once, profile_TBV_once,
                   ncol = 1, nrow = 6, labels = c("A", "B", "C", "D", "E", "F"), legend = "none")

# Lower panels: profile plots
# figure_profiles = ggarrange(profile_MAB_cov, profile_MAB_eff, profile_MAB_hl, 
#                             profile_ATSB_cov, profile_ATSB_eff, profile_ATSB_hl, 
#                             ncol = 3, nrow = 2, labels = c("C", "", "", "D", "", ""))
# 
# figure = ggarrange(figure_landscapes, figure_profiles, ncol = 1, nrow = 2, heights = c(1, 1.1))

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig5.pdf",
       plot = figure,  width = 12, height = 13, device = cairo_pdf)

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig5.ps",
       plot = figure,  width = 12, height = 13, device = cairo_ps)
