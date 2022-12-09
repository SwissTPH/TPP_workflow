##########################
# plot optimal profiles of interventions or comparisons
#
# created 17.02.2020
# monica.golumbeanu@swisstph.ch
##########################

# Load the necessary plotting functions and packages
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")
library(stringr)

# Compare optimisation profiles
exp_list = c("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/", "~/MMC/TPP/simulations/MAB_twice_sync_3years/",
                "~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/", 
                "~/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/")

exp_list = c("~/MMC/TPP/simulations/ATSB_once_3years/", "~/MMC/TPP/simulations/ATSB_twice_sync_3years/")

# exp_list = c("~/MMC/TPP/simulations/PEV_once_3years/", "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
#              "~/MMC/TPP/simulations/TBV_once_3years/", 
#              "~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/")
follow_up = 6
prev_threshold = 90
exp_name = "ATSB"
plot_dir = "~/MMC/TPP/figures/optimisation/profiles/"
profiles_df = build_profiles_df(exp_list, follow_up, prev_threshold)
CM_levels = unique(profiles_df$CM_level)
opt_params = unique(profiles_df$Param_opt)

p_array = vector('list', length(CM_levels)*length(opt_params))
i = 0
for (cm_l in CM_levels) {
    for (param_opt in opt_params) {
        optim_df_part = profiles_df[which(profiles_df$CM_level == cm_l & profiles_df$Param_opt == param_opt), ]
        if(nrow(optim_df_part) > 0)
        {
            i = i + 1
            p_array[[i]] = plot_profiles_df(optim_df_part, plot_dir, 
                             paste0(exp_name, "_", follow_up, "_", prev_threshold, "_", cm_l, "_", param_opt))
        }
    }
}


figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 3)
figure_file = paste0(plot_dir, "all_", exp_name, "_", follow_up, "_", prev_threshold, ".pdf")
ggsave(filename = figure_file, plot = figure, width = 16, height = 10)

