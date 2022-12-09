########################
# Plot the decay profiles for the interventions
#
# created 14.03.2020
# monica.golumbeanu@unibas.ch
########################

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


###### Plot the decay functions #####
points = seq(from = 0 , to = 5, by = 0.01)
df_names = c("decay_type", "points", "decay_values")
decay1 = cbind.data.frame("sigmoidal", points, Weibull_function(points, 1, 3))
decay2 = cbind.data.frame("biphasic", points, Weibull_function(points, 1, 0.8))
decay3 = cbind.data.frame("step", points, step_function(points, 1))
decay4 = cbind.data.frame("exponential", points, exponential_function(points, 1))
colnames(decay1) = colnames(decay2) = colnames(decay3) = colnames(decay4) = df_names
plot_df2 = rbind(decay3, decay1, decay2, decay4)
decay_colors = c("#fa9fb5", "#c994c7", "#dd3497", "#7a0177")
ggplot(plot_df2, aes(x = points, y = decay_values, color = as.factor(decay_type))) + geom_line()+
    theme_bw(base_size=14) + scale_color_manual(values = decay_colors) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(strip.background = element_rect(colour="white", fill="white"), title =element_text(size=12)) +
    ylim(0, 1) + xlim(0, 5) + geom_line(lwd = 1.5) + guides(color=guide_legend(title="Decay type")) +
    labs(x = "Time (years)", y = "Efficacy", title = "Intervention decay")
plot_file = paste0(out_folder, "decay.pdf")
ggsave(plot_file, width = 5, height = 4)

