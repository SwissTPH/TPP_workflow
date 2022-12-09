##########################
# call the plotting functions for optimisation results
#
# created 15.01.2020
# monica.golumbeanu@swisstph.ch
##########################

# Load the necessary plotting functions and packages
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")
library(stringr)
library(ggpubr)
theme_set(theme_pubr())

# Retrieve the command argments and define function inputs
# args = commandArgs(TRUE)
# sim_dir = args[1]
# follow_up = args[2]
# plot_title = args[3]

# For testing
sim_dir = "~/MMC/TPP/simulations/ATSB_twice_sync_3years/"
follow_up = 4

optim_dir = paste0(sim_dir, "gp_", follow_up, "/optimisation/")
param_ranges_file = paste0(sim_dir, "param_ranges.RData")
load(param_ranges_file)
param_table = read.table(paste0(sim_dir, "param_tab.txt"), header = TRUE)
model_pattern = basename(sim_dir)
plot_dir = paste0("~/MMC/TPP/figures/optimisation/", model_pattern, "/")
dir.create(plot_dir, showWarnings = FALSE)

optim_df = build_opt_df(optim_dir)
CM_levels = unique(optim_df$CM_level)
opt_params = unique(optim_df$Param_opt)
p_array = vector('list', length(CM_levels)*length(opt_params))
i = 0
for (cm_l in CM_levels) {
    for (param_opt in opt_params) {
        optim_df_part = optim_df[which(optim_df$CM_level == cm_l & optim_df$Param_opt == param_opt), ]
        if(nrow(optim_df_part) > 0)
        {
            optim_df_part = optim_df_part[-which(is.na(optim_df_part$opt_param)),]
            i = i + 1
            p_array[[i]] = plot_profiles_landscape(optim_df_part, param_ranges[param_opt, ], 
                                    plot_dir, paste0(model_pattern, "_", follow_up, "_", cm_l, "_", param_opt),
                                    str_to_lower(param_opt))
        }    
    }
}

figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 3)
figure_file = paste0(plot_dir, "figure_", follow_up, ".pdf")
ggsave(filename = figure_file, plot = figure, width = 16, height = 10)


# plot_profiles(optim_dir, plot_dir, paste0(model_pattern, "_", follow_up), 45)
