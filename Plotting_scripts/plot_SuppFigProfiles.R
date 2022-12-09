#############################
# Plot supplementary figures with optimal profiles
# created 15.07.2020
# monica.golumbeanu@unibas.ch
#############################

# Load the necessary plotting functions and packages
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")
library(stringr)

# create_profile_fig = function(exp_list) {
    exp_list = c("~/MMC/TPP/simulations/ATSB_once_3years/", 
      "~/MMC/TPP/simulations/MAB_twice_sync_3years/")
    follow_up_vec = c(4,6)
    CM_level = "MidCM"
    figs = vector('list', 2)
    prev_threshold = 60
    
    for (ind in 1:2) {
        follow_up = follow_up_vec[ind]
        profiles_df = build_profiles_df(exp_list, follow_up, prev_threshold)
        opt_params = unique(profiles_df$Param_opt)
        p_array = vector('list', 3)
        i = 0
        for (param_opt in opt_params) {
            optim_df_part = profiles_df[which(profiles_df$CM_level == CM_level & profiles_df$Param_opt == param_opt), ]
            # Remove values for EIR = 1
            if (length(which(optim_df_part$EIR == 1)) > 0) {
                optim_df_part = optim_df_part[-which(optim_df_part$EIR == 1), ]
            }
            if(nrow(optim_df_part) > 0)
            {
                if (length(which(is.na(optim_df_part$opt_param))) > 0) {
                    optim_df_part = optim_df_part[-which(is.na(optim_df_part$opt_param)),]
                }
                i = i + 1
                p_array[[i]] = plot_profiles_df(optim_df_part)
            }
        }
        figs[[ind]] = ggarrange(plotlist = p_array, ncol = 3, nrow = 1, legend = "top")
        
    }
    
    figure = ggarrange(plotlist = figs, ncol = 1, nrow = 2)
    return(figure)
# }

# # Compare optimisation profiles
# f1 = create_profile_fig(c("~/MMC/TPP/simulations/ATSB_once_3years/", 
#              "~/MMC/TPP/simulations/MAB_twice_sync_3years/"))
