#############################
# Plot the specifications for a given intervention
#
# 23.01.2020
############################
source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_prev_red_distr.R")

construct_decay_df = function(sim_names) {
    df_names = c("decay_type", "points", "decay_values", "sim_name", "hl", "unit", 
                 "min_eff", "max_eff", "min_hl", "max_hl")
    points_MAB = seq(from = 0 , to = 8, length.out = 100)
    decay_MAB = cbind.data.frame("Sigmoidal", points_MAB, 
                                 0.8*Weibull_function(points_MAB, 4, 3), sim_names[1], 4, "months",
                                 30, 95, 2, 8)
    
    points_AIV = seq(from = 0 , to = 50, length.out = 100)
    decay_AIV = cbind.data.frame("Biphasic", points_AIV, 
                                 0.8*Weibull_function(points_AIV, 10, 0.8), sim_names[2], 10, "months",
                                 30, 95, 6, 60)
    
    points_TBV = seq(from = 0 , to = 50, length.out = 100)
    decay_TBV = cbind.data.frame("Biphasic", points_TBV, 
                                 0.8*Weibull_function(points_TBV, 10, 0.8), sim_names[3], 10, "months",
                                 30, 95, 6, 60)
    
    points_MDA = seq(from = 0 , to = 50, length.out = 100)
    decay_MDA = cbind.data.frame("Exponential", points_MDA, 
                                 0.8*exponential_function(points_MDA, 10), sim_names[4], 10, "days",
                                 80, 100, 5, 10)
    
    points_ATSB = seq(from = 0 , to = 4, length.out = 100)
    decay_ATSB = cbind.data.frame("Step", points_ATSB, 
                                  0.8*step_function(points_ATSB, 2), sim_names[5], 2, "months",
                                  70, 100, 2, 8)
    
    points_eave = seq(from = 0 , to = 50, length.out = 100)
    decay_eave = cbind.data.frame("Sigmoidal", points_eave, 
                                  0.8*Weibull_function(points_eave, 20, 3), sim_names[6], 20, "months",
                                  30, 100, 6, 60)
    
    colnames(decay_MAB) = colnames(decay_AIV) = colnames(decay_TBV) = 
        colnames(decay_MDA) = colnames(decay_ATSB) = colnames(decay_eave) = df_names
    plot_df = rbind(decay_MAB, decay_AIV, decay_TBV, decay_MDA, decay_ATSB, decay_eave)
    return(plot_df)
}

plot_int_specs = function(sim_folders, sim_names, p_array_list, df_decays) {
    for (i in 1:length(sim_folders)) {
        # Load the parameter ranges for each intervention
        sim_folder = sim_folders[i]
        load(paste0(sim_folder, "param_ranges.RData"))
        plot_df = as.data.frame(t(c(param_ranges[2,], param_ranges[3,])))
        colnames(plot_df) = c("x1", "x2", "y1", "y2")
        
        decay_rows = df_decays[which(df_decays$sim_name == sim_names[i]),]
        hl_lab = paste0("Half-life: ", unique(decay_rows$hl), " ", unique(decay_rows$unit))
        efficacy_range = paste0("Efficacy: ", unique(decay_rows$min_eff), "% - ", unique(decay_rows$max_eff), "%")
        hl_range = paste0("Half-life: ", unique(decay_rows$min_hl), " - ", unique(decay_rows$max_hl), " ", unique(decay_rows$unit))
        decay_type = paste0("Decay: ", unique(decay_rows$decay_type))
        
        intervention_colors = create_color_palette_int()
        
        p_array_list[[i]] = ggplot() + theme_bw(base_size=13) + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            theme(strip.background = element_rect(colour="white", fill="white"), title =element_text(size=12)) + 
            ylim(0, 100) + xlim(0, 5) +
            geom_rect(data = plot_df, mapping=aes(xmin=x1, xmax=x2, ymin=y1*100, ymax=y2*100), 
                      fill = intervention_colors[sim_names[i]], alpha = 0.5, color=NA, lwd=1) +
            annotate("text", label = efficacy_range, x = 0, y = 17, size = 3, hjust = 0) +
            annotate("text", label = hl_range, x = 0, y = 9, size = 3, hjust = 0) +
            annotate("text", label = decay_type, x = 0, y = 1, size = 3, hjust = 0) +
            labs(x = "Half-life (years)", y = "Efficacy (%)", title = sim_names[i]) +
            theme(plot.title = element_text(size=11), axis.title=element_text(size=10))
        
        p_decay = ggplot() + geom_line(data = decay_rows, aes(x = points, y=decay_values*100)) +
                    theme_bw(base_size=9) +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                    theme(strip.background = element_rect(colour="white", fill="white")) + 
                    geom_vline(xintercept = unique(decay_rows$hl), lty = 2, lwd = 0.2) +
                    geom_hline(yintercept = 40, lty = 2, lwd = 0.2) +
                    theme(plot.title = element_text(size=9), axis.title=element_text(size=9)) +
                    labs(x = paste0("Time (", decay_rows$unit, ")"), y = "Efficacy (%)") +
                    theme(rect = element_rect(fill = "transparent"))
        p_array_list[[i]] = p_array_list[[i]] + 
            annotation_custom(ggplotGrob(p_decay), xmin = 1.5, xmax = 4.9, ymin = 35, ymax = 90)
        
    }
    return(p_array_list)
}

# Functions for calculating effect decays
Weibull_function = function(t, L, k) {
    return(exp(-(t/L)^k * log(2)))
}

step_function = function(t, L){
    output = ifelse(t >= L, 0, 1)
    return(output)
}

exponential_function = function(t, L){
    return(exp(-t/L * log(2)))
}

plot_decay = function() {
    ###### Plot the decay functions #####
    points = seq(from = 0 , to = 5, by = 0.01)
    df_names = c("decay_type", "points", "decay_values")
    decay1 = cbind.data.frame("Sigmoidal", points, Weibull_function(points, 1, 3))
    decay2 = cbind.data.frame("Biphasic", points, Weibull_function(points, 1, 0.8))
    decay3 = cbind.data.frame("Step", points, step_function(points, 1))
    decay4 = cbind.data.frame("Exponential", points, exponential_function(points, 1))
    colnames(decay1) = colnames(decay2) = colnames(decay3) = colnames(decay4) = df_names
    plot_df2 = rbind(decay3, decay1, decay4, decay2)
    decay_colors = c("#fa9fb5", "#c994c7", "#dd3497", "#7a0177")
    decay_palette = create_color_palette_decay()
    p = ggplot(plot_df2, aes(x = points, y = decay_values, color = as.factor(decay_type))) + geom_line()+
        theme_bw(base_size=13) + scale_color_manual(name = "decay_type", values = decay_palette) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(strip.background = element_rect(colour="white", fill="white"), title =element_text(size=12)) +
        ylim(0, 1) + xlim(0, 5) + geom_line(lwd = 1.5) + guides(color=guide_legend(title="Decay type")) +
        labs(x = "Time (years)", y = "Efficacy", title = "Intervention decay") +
        theme(plot.title = element_text(size=10), axis.title=element_text(size=10)) +
        theme(legend.position="top") + guides(color=guide_legend(nrow=2,byrow=TRUE)) +
        theme(legend.title = element_blank())
    return(p)
}



###### Plot the efficacy and halflife profiles ######
sim_folders = c("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/",      # passive anti-infective 
                "~/MMC/TPP/simulations/PEV_once_3years/",               # active anti-infective (vaccine)
                "~/MMC/TPP/simulations/TBV_once_3years/",               # active transmission-blocking (vaccine)
                "~/MMC/TPP/simulations/MDA_MAB_once_3years/",           # active blood-stage clearance    
                "~/MMC/TPP/simulations/ATSB_once_3years/",              # pre- and postprandial killing
                "~/MMC/TPP/simulations/preprandial_once_3years/")       # preprandial killing

sim_names = c("Anti-infective monoclonal", "Anti-infective vaccine", 
              "Transmission-blocking vaccine", "Blood stage clearance", 
              "Attractive targeted sugar baits", "Eave tubes")
decay_df = construct_decay_df(sim_names)

p_array = vector('list', length(sim_folders))
p_array = plot_int_specs(sim_folders, sim_names, p_array, decay_df)
figure = ggarrange(plotlist = p_array, ncol = 3, nrow = 2,
                   labels = c("A", "B", "C", "D", "E", "F", "G"))

ggsave("~/MMC/TPP/figures/simulation_desc/intervention_specs_decay.pdf",
       plot = figure, width = 8, height = 6)
