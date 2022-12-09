##################################
# Plot the lower panel of Figure 1 - results for PEV
#
# 01.04.2020
# monica.golumbeanu@unibas.ch
#################################
source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")
library(ggpubr)


plot_landscape_row = function(optim_dir, param_ranges_file, EIR_prev_tab) {
    optim_df = build_opt_df(optim_dir)
    opt_params = unique(optim_df$Param_opt)
    load(param_ranges_file)
    p_array = vector('list', length(opt_params))
    i = 0
    for (param_opt in opt_params) {
        i = i + 1
        p_array[[i]] = plot_single_landscape_row(optim_df, "MidCM", param_opt, param_ranges[param_opt, ], 
                                                 "Seasonal", "High indoor biting", "", EIR_prev_tab)
    }
    figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 1, legend = "right")
    return(figure)
}

# Common files:
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)


#### Landscape plots
optim_dir1 = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/optimisation/"
param_ranges_file1 = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/param_ranges.RData"
figure_PEV_once = plot_landscape_row(optim_dir1, param_ranges_file1, EIR_prev_tab)
figure_PEV_once = annotate_figure(figure_PEV_once,
                                  top = text_grob("Anti-infective vaccine: landscapes of minimum properties"))


# optim_dir1 = "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/gp_4/optimisation/"
# param_ranges_file1 = "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/param_ranges.RData"
# figure_PEV_once = plot_landscape_row(optim_dir1, param_ranges_file1, EIR_prev_tab) 
# 

optim_dir2 = "/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/gp_4/optimisation/"
param_ranges_file2 = "/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/param_ranges.RData"
figure_TBV_once = plot_landscape_row(optim_dir2, param_ranges_file2, EIR_prev_tab)
figure_TBV_once = annotate_figure(figure_TBV_once, 
                                   top = text_grob("Transmission-blocking vaccine: landscapes of minimum properties"))

#### Profile plots
exp_list1 = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/", 
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_AIV/")
exp_list2 = c("/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/",
              "/scicore/home/penny/golmon00/MMC/TPP/simulations/cohort_MDA_short_TBV/")
follow_up = 4
prev_threshold = 70
profiles_df1 = build_profiles_df(exp_list1, follow_up, prev_threshold)
profile_PEV_cov = plot_profile_df(profiles_df1, "Coverage", "MidCM", 
                                   "Seasonal", "High indoor biting", 
                                   "", EIR_prev_tab,
                                   c("#276419", "#1B9E77"), 
                                   x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_PEV_eff = plot_profile_df(profiles_df1, "Efficacy", "MidCM", 
                                   "Seasonal", "High indoor biting", 
                                   "", EIR_prev_tab,
                                   c("#276419", "#1B9E77"), 
                                   x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_PEV_hl = plot_profile_df(profiles_df1, "Halflife", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "", EIR_prev_tab,
                                  c("#1B9E77", "#276419"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0, 85))

prev_threshold = 70
profiles_df2 = build_profiles_df(exp_list2, follow_up, prev_threshold)
profile_TBV_eff = plot_profile_df(profiles_df2, "Efficacy", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0, 111))

profile_TBV_cov = plot_profile_df(profiles_df2, "Coverage", "MidCM", 
                                  "Seasonal", "High indoor biting", 
                                  "", EIR_prev_tab,
                                  c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                  x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,111))

profile_TBV_hl = plot_profile_df(profiles_df2, "Halflife", "MidCM", 
                                 "Seasonal", "High indoor biting", 
                                 "", EIR_prev_tab,
                                 c("#E78AC3", "#E7298A", "#984EA3", "red"),
                                 x_ticks = c(2, 6, 10, 15, 20), y_lims = c(0,85))

### Construct the final figure
profile_PEV_once = ggarrange(profile_PEV_cov, profile_PEV_eff, profile_PEV_hl, ncol = 3, nrow = 1, 
                             legend = "right")
profile_PEV_once = annotate_figure(profile_PEV_once, 
                                   top = text_grob("Anti-infective vaccine: minimum profiles of properties"))

profile_TBV_once = ggarrange(profile_TBV_cov, profile_TBV_eff, profile_TBV_hl, ncol = 3, nrow = 1, 
                              legend = "right")
profile_TBV_once = annotate_figure(profile_TBV_once, 
                                    top = text_grob("Transmission-blocking vaccine: minimum profiles of properties"))

figure = ggarrange(figure_PEV_once, profile_PEV_once, figure_TBV_once, profile_TBV_once,
                   ncol = 1, nrow = 4, labels = c("A", "B", "C", "D"), legend = "none")

ggsave("~/MMC/TPP/figures/simulation_desc/SuppFigVacc.pdf",
       plot = figure,  width = 12, height = 9)
