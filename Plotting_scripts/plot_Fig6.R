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
    if (sum(opt_params == c("Coverage", "Halflife", "Preprandial_efficacy")) == 3) {
        opt_params = c("Coverage", "Preprandial_efficacy", "Halflife")
    }
    for (param_opt in opt_params) {
        i = i + 1
        p_array[[i]] = plot_single_landscape_row(optim_df, "MidCM", param_opt, param_ranges[param_opt, ], 
                                                 "Seasonal", "High indoor biting", title_vec[i], EIR_prev_tab, prev_goal)
    }
    figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 1, legend = "right")
    return(figure)
}

# Common files:
EIR_prev_tab = read.table("/scicore/home/penny/golmon00/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)
prev_threshold = 60

#### Landscape plots
optim_dir1 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/optimisation/"
param_ranges_file1 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/param_ranges.RData"
figure_ATSB_once = plot_landscape_row(optim_dir1, param_ranges_file1, EIR_prev_tab, 
                                      c("e = 85%, h = 4mos", "c = 60%, h = 4mos", "e = 85%, c = 60%"), prev_threshold)
figure_ATSB_once = annotate_figure(figure_ATSB_once, 
                                   top = text_grob("Attractive targeted sugar baits: landscapes of minimum properties", face = "bold"))

optim_dir2 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/gp_4/optimisation/"
param_ranges_file2 = "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/param_ranges.RData"
figure_eaves_once = plot_landscape_row(optim_dir2, param_ranges_file2, EIR_prev_tab, 
                                       c("e = 85%, h = 3y", "c = 60%, h = 3y", "e = 60%, c = 85%"), prev_threshold)
figure_eaves_once = annotate_figure(figure_eaves_once, 
                                   top = text_grob("Eave tubes: landscapes of minimum properties", face = "bold"))

#### Profile plots
exp_list1 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/", 
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_twice_sync_3years/")
exp_list2 = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/")
follow_up = 4

profiles_df1 = build_profiles_df(exp_list1, follow_up, prev_threshold)
profile_ATSB_cov = plot_profile_df(profiles_df1, "Coverage", "MidCM", 
                                   "Seasonal", "High indoor biting", 
                                   "e = 85%, h = 4mos", EIR_prev_tab,
                                   c("#276419", "#1B9E77"), 
                                   x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_ATSB_eff = plot_profile_df(profiles_df1, "Efficacy", "MidCM", 
                                   "Seasonal", "High indoor biting", 
                                   "c = 60%, h = 4mos", EIR_prev_tab,
                                   c("#276419", "#1B9E77"), 
                                   x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_ATSB_hl = plot_profile_df(profiles_df1, "Halflife", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "e = 85%, c = 60%", EIR_prev_tab,
                                  c("#1B9E77", "#276419"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0, 10))

profiles_df2 = build_profiles_df(exp_list2, follow_up, prev_threshold)
profiles_df2$Param_opt = str_replace(profiles_df2$Param_opt, "Preprandial_efficacy", "Efficacy")
profile_eaves_eff = plot_profile_df(profiles_df2, "Efficacy", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "c = 60%, h = 3y", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 4, 6, 8, 10, 12, 15, 18, 20), y_lims = c(0, 111))

profile_eaves_cov = plot_profile_df(profiles_df2, "Coverage", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "e = 85%, h = 3y", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_eaves_hl = plot_profile_df(profiles_df2, "Halflife", "MidCM", 
                                 "Seasonal", "High indoor biting", 
                                 "e = 85%, c = 60%", EIR_prev_tab,
                                 c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                 x_ticks = c(2, 4, 6, 8, 10, 12, 15, 18, 20), y_lims = c(0,25))

### Construct the final figure
profile_eaves_once = ggarrange(profile_eaves_cov, profile_eaves_eff, profile_eaves_hl, ncol = 3, nrow = 1, 
                             legend = "right")
profile_eaves_once = annotate_figure(profile_eaves_once, 
                                   top = text_grob("Eave tubes: minimum profiles of properties", face = "bold"))

profile_ATSB_once = ggarrange(profile_ATSB_cov, profile_ATSB_eff, profile_ATSB_hl, ncol = 3, nrow = 1, 
                              legend = "right")
profile_ATSB_once = annotate_figure(profile_ATSB_once, 
                                    top = text_grob("Attractive targeted sugar baits: minimum profiles of properties", face = "bold"))

figure = ggarrange(figure_ATSB_once, profile_ATSB_once, figure_eaves_once, profile_eaves_once, 
                   ncol = 1, nrow = 4, labels = c("A", "B", "C", "D"), legend = "none")

# Lower panels: profile plots
# figure_profiles = ggarrange(profile_MAB_cov, profile_MAB_eff, profile_MAB_hl, 
#                             profile_ATSB_cov, profile_ATSB_eff, profile_ATSB_hl, 
#                             ncol = 3, nrow = 2, labels = c("C", "", "", "D", "", ""))
# 
# figure = ggarrange(figure_landscapes, figure_profiles, ncol = 1, nrow = 2, heights = c(1, 1.1))

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig6.pdf",
       plot = figure,  width = 12, height = 9, device = cairo_pdf)

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig6.ps",
       plot = figure,  width = 12, height = 9, device = cairo_ps)
