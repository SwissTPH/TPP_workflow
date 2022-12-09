##################################
# Plot the lower panel of Figure 1 - results for PEV
#
# 01.04.2020
# monica.golumbeanu@unibas.ch
#################################
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_sensitivity.R")
source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")

# Folders for PEV
training_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/trained/"
as_dir = '/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/as/'
sens_dir = '/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/sensitivity/'
test_processing_dir = "/scicore/home/smith/golmon00//MMC/TPP/simulations/test_sets/test_PEV_once_3years/postprocessing_4/"
param_ranges_file = "/scicore/home/smith/golmon00//MMC/TPP/simulations/PEV_once_3years/param_ranges.RData"

# Plot GP emulator performance
test_plot_df = prepare_test_plot_df(as_dir, test_processing_dir, param_ranges_file)
cv_test_plot_df = prepare_plot_df_cv_test(training_dir, as_dir, test_processing_dir, param_ranges_file)

p_scatter = plot_single_scatter(test_plot_df, "Seasonal", "High_indoor", "Emulator performance") 
p_r_distr = plot_single_corr_box(cv_test_plot_df, "Seasonal", "High_indoor") 

p_perf = p_scatter + annotation_custom(ggplotGrob(p_r_distr), xmin = 55, xmax = 100, ymin = -17, ymax = 46)

# Plot the sensitivity analysis result
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)
p_sens = plot_single_sens_GP(sens_dir, "Seasonal", "High_indoor", "total_effects", 
                    "area", param_ranges_file, "Key impact determinants", EIR_prev_tab, c(1, 5, 10, 15, 20, 24)) 

# Plot the optimisation result plot
optim_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/gp_4/optimisation/"
optim_df = build_opt_df(optim_dir)
param_opt = "Efficacy"
load(param_ranges_file)
p_landscape = plot_single_landscape(optim_df, "MidCM", param_opt, param_ranges[param_opt, ], 
                                    "Seasonal", "High indoor biting", "Optimal tool profiles", EIR_prev_tab) 
p_profile = plot_single_profile(optim_df, 60, "MidCM", param_opt,  "Seasonal", "High indoor biting", EIR_prev_tab)
p_optim = p_landscape + geom_segment(mapping = aes(x=6, y=60, xend=8, yend=60), 
                                     arrow=arrow(type = "closed", length = unit(2,"mm")), 
                                     size=0.2, color="black") +
            annotation_custom(ggplotGrob(p_profile), xmin = 8, xmax = 14, ymin = 45, ymax = 75)

# Build the final figure
fig_1 = ggarrange(p_perf, p_sens$p, p_optim, ncol = 3, 
                  nrow = 1, labels = c("B", "C", "D"), legend = "top", widths = c(1, 1.2, 1.2))
ggsave("~/MMC/TPP/figures/main_figures/Fig1.pdf",
       plot = fig_1, width = 12, height = 4.5)
