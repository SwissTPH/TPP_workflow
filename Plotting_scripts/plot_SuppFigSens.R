##########################
# Plot sensitivity analysis plots for all interventions
##########################

source("~/MMC/TPP/scripts_v38/plotting/plot_GP_sensitivity.R")
source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")

construct_sens_fig = function(plotted_folders, exp_names) {
    p_array = vector('list', length(plotted_folders))
    for(i in 1:length(plotted_folders)) {
        exp_folder = plotted_folders[i]
        param_ranges_file = paste0(exp_folder, "param_ranges.RData")
        # plot_title = all_names[which(all_exp == exp_folder)]
        
        gp_folder = paste0(exp_folder, "gp_4/")
        sens_folder = paste0(gp_folder, "sensitivity/")
        p_4 = plot_all_sens_GP(sens_folder, param_ranges_file, "Immediate follow-up", EIR_prev_tab)
        
        gp_folder = paste0(exp_folder, "gp_6/")
        sens_folder = paste0(gp_folder, "sensitivity/")
        p_6 = plot_all_sens_GP(sens_folder, param_ranges_file, "Late follow-up", EIR_prev_tab)
        p_array[[i]] = ggarrange(p_4, p_6, ncol = 2, nrow = 1, legend = "right", 
                                 common.legend = TRUE)
        p_array[[i]] = annotate_figure(p_array[[i]], 
                                       top = text_grob(exp_names[i], face = "bold", size = 11))
    }
    return(p_array)
}

construct_sens_fig_experiment = function(exp_dir, exp_name, seas_list, 
                                         biting_list, fig_labs, ncol_n, nrow_n) {
    param_ranges_file = paste0(exp_dir, "param_ranges.RData")
    vec_follow_up = c(4, 6)
    vec_follow_up_names = c("immediate follow-up", "late follow-up")
    
    figure_sens = vector('list', 2)
    for (follow_up in 1:length(vec_follow_up)) {
        sens_dir = paste0(exp_dir,"gp_", vec_follow_up[follow_up], "/sensitivity/")
        p_sens = vector('list', length(seas_list)*length(biting_list))
        i = 0
        for (seas in seas_list) {
            for (biting in biting_list) {
                i = i + 1
                plot_name = paste0(seas, "\n", str_replace(biting, "_", " "), " biting")
                sens_plot = plot_single_sens_GP(sens_dir, seas, biting, "total_effects", "area",
                                                param_ranges_file, plot_name, 
                                                EIR_prev_tab, c(1, 5, 10, 15, 20, 24))
                p_sens[[i]] = sens_plot$p + theme_bw(base_size=13) +
                    theme(plot.title = element_text(size=11, face = "bold")) 
            }
        }
        figure_sens[[follow_up]] = ggarrange(plotlist = p_sens, 
                                             ncol = ncol_n, nrow = nrow_n, 
                                             common.legend = TRUE, legend = "top")
        figure_sens[[follow_up]] = annotate_figure(figure_sens[[follow_up]], 
                                                   top = text_grob(paste0(exp_name, ", ", vec_follow_up_names[follow_up]), face = "bold"))
        
    }
    figure = ggarrange(plotlist = figure_sens, 
                       ncol = 2, nrow = 1, labels = fig_labs,
                       common.legend = TRUE, legend = "right")
    return(figure)
}

# Load the EIR_to_prevalence transformations
EIR_prev_tab = read.table("~/MMC/TPP/figures/simulation_desc/prev_red/prev_distr.txt", sep = "\t", header = TRUE, 
                          stringsAsFactors = FALSE)


# # Human-side interventions
# # MABs
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/"
exp_name = "Anti-infective monoclonal"
seas_list = c("Seasonal", "Perennial")
biting_list = c("Low_indoor", "Mid_indoor", "High_indoor")
f1 = construct_sens_fig_experiment(exp_dir, exp_name, seas_list, biting_list, c("A", "B"), 3, 2)

# Anti-infective vaccines
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/"
exp_name = "Anti-infective vaccine"
seas_list = c("Seasonal", "Perennial")
biting_list = c("High_indoor")
f2 = construct_sens_fig_experiment(exp_dir, exp_name, seas_list, biting_list, c("C", "D"), 2, 1) 

# Transmission-blocking vaccines
exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/TBV_once_3years/"
exp_name = "Transmission-blocking vaccine"
seas_list = c("Seasonal", "Perennial")
biting_list = c("High_indoor")
f3 = construct_sens_fig_experiment(exp_dir, exp_name, seas_list, biting_list, c("E", "F"), 2, 1) 

fig_vaccines = ggarrange(f2, f3, ncol = 1, nrow = 2)
# ggsave("~/MMC/TPP/figures/sensitivity_analysis/test.pdf", fig_vaccines, width = 12.5, height = 8)

sens_human_fig = ggarrange(f1, fig_vaccines, ncol = 1, nrow = 2, heights = c(1,1.5))
ggsave("~/MMC/TPP/figures/sensitivity_analysis/sens_human_interventions.pdf", sens_human_fig, width = 13.5, height = 13)


# # Vector control interventions
# # ATSBs
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/"
exp_name = "Attractive targeted sugar baits"
seas_list = c("Seasonal", "Perennial")
biting_list = c("Low_indoor", "Mid_indoor", "High_indoor")
f4 = construct_sens_fig_experiment(exp_dir, exp_name, seas_list, biting_list, c("A", "B"), 3, 2)


# # Eave tubes
exp_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/preprandial_once_3years/"
exp_name = "Eave tubes"
seas_list = c("Seasonal", "Perennial")
biting_list = c("Low_indoor", "Mid_indoor", "High_indoor")
f5 = construct_sens_fig_experiment(exp_dir, exp_name, seas_list, biting_list, c("C", "D"), 3, 2)

sens_vc_fig = ggarrange(f4, f5, ncol = 1, nrow = 2)
ggsave("~/MMC/TPP/figures/sensitivity_analysis/sens_vector_interventions.pdf", sens_vc_fig, width = 13.5, height = 11)


# #### Plot sensitivity result human-side interventions
# exp_names_human = define_experiment_names()$all_names[1:3]
# plotted_folders_human = c("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/", 
#                     "~/MMC/TPP/simulations/PEV_once_3years/",
#                     "~/MMC/TPP/simulations/TBV_once_3years/")
# p_array_human = construct_sens_fig(plotted_folders_human, exp_names_human)
# figure = ggarrange(plotlist = p_array_human, ncol = 1, nrow = 3, 
#                    legend = "right", labels = c("A", "B", "C"))
# ggsave("~/MMC/TPP/figures/simulation_desc/SuppFigSens_human.pdf",
#        plot = figure,  width = 8, height = 9)
# 
# #### Plot sensitivity result human-side interventions
# exp_names_vc = define_experiment_names()$all_names[4:5]
# plotted_folders_vc = c("~/MMC/TPP/simulations/ATSB_once_3years/",              
#                           "~/MMC/TPP/simulations/preprandial_once_3years/")
# p_array_vc = construct_sens_fig(plotted_folders_vc, exp_names_vc)
# figure = ggarrange(plotlist = p_array_vc, ncol = 1, nrow = 2, legend = "right", labels = c("A", "B"))
# ggsave("~/MMC/TPP/figures/simulation_desc/SuppFigSens_vc.pdf",
#        plot = figure,  width = 8, height = 7)
# 
