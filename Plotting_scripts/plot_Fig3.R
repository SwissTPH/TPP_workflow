##################################
# Plot time series of simulation outcomes for varying properties 
# of an intervention
#
# monica.golumbeanu@unibas.ch
# 10.03.2020
##################################
library(RColorBrewer)
require(ggplot2)
require(scales) 
library(tgp)
library(ggpubr)

rep.row = function(x,n){
    return(matrix(rep(x,each=n),nrow=n))
}

rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("/scicore/home/penny/golmon00/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")

compute_ts = function(exp_file, exp_n, param_tab, req_list) {
    # Select the relevant scenarios
    scenarios_list = param_tab[which(between(param_tab$EIR, req_list$EIR[1], req_list$EIR[2]) &
                                         between(param_tab$Access, req_list$CM[1], req_list$CM[2]) &
                                         between(param_tab$Efficacy, req_list$Eff[1], req_list$Eff[2]) &
                                         between(param_tab$Halflife, req_list$Hl[1], req_list$Hl[2]) &
                                         param_tab$Seasonality == req_list$seas &
                                         param_tab$Biting_pattern == req_list$biting), ]
    
    # Summarize the selected simulations
    plot_df = NULL
    for (i in 1:nrow(scenarios_list)) {
        sim_file = paste0(exp_file, "om/", scenarios_list$Scenario_Name[i], "_", scenarios_list$SEED[i], "_out.txt")
        if(file.exists(sim_file)) {
            om_result = read.table(sim_file, sep="\t")
            colnames(om_result) = c("time", "age_group", "measure", "value")
            n_cases = as.data.frame(om_result[om_result$measure == 1,])
            
            # Summarize results by summing up over age groups
            om_result$age_group=NULL
            agg_n_cases = n_cases %>% group_by(time, measure) %>% summarise(val = sum(value)/10000)
            if (is.null(plot_df)) {
                plot_df = agg_n_cases$val
            } else {
                plot_df = cbind.data.frame(plot_df, agg_n_cases$val)
            }
        }
    }
    sd_val = apply(plot_df, 1, sd)
    mean_val = rowMeans(plot_df)
    summary_plot_df = cbind.data.frame(agg_n_cases$time, mean_val, 
                                       mean_val - sd_val, mean_val + sd_val, exp_n)
    colnames(summary_plot_df) = c("time", "mean", "min_cases", "max_cases", "experiment")
    
    return(summary_plot_df)
}

plot_ts2 = function(exp_list, exp_name, seas, biting) {
    # Required EIR and case management levels
    req_list = NULL
    req_list$EIR = c(10, 10)
    req_list$CM = c(0.4, 0.4)
    req_list$Eff = c(0.7, 0.7)
    req_list$seas = seas
    req_list$biting = biting
    
    int_plot_df = NULL
    for (exp_n in 1:length(exp_list)) {
        exp = exp_list[exp_n] 
        param_tab = read.table(paste0(exp, "param_tab.txt"), sep="\t", header = TRUE, stringsAsFactors = FALSE, as.is = TRUE)
        req_list$Hl = c(0.167, 0.2)
        s_plot_df_1 = compute_ts(exp, exp_name, param_tab, req_list)
        s_plot_df_1$exp = "2 months"
        req_list$Hl = c(0.3, 0.35)
        s_plot_df_2 = compute_ts(exp, exp_name, param_tab, req_list)
        s_plot_df_2$exp = "4 months"
        req_list$Hl = c(0.65, 0.667)
        s_plot_df_3 = compute_ts(exp, exp_name, param_tab, req_list)
        s_plot_df_3$exp = "8 months"
        
        # scenario_name = scenarios_list$Scenario_Name[1]
        # scenarios_list = scenarios_list[which(scenarios_list$Scenario_Name == scenario_name),]
        
        int_plot_df = rbind(s_plot_df_1, s_plot_df_2, s_plot_df_3)
    }
    
    intervention_colors = create_color_palette_int()
    int_plot_df$experiment = factor(int_plot_df$experiment , levels = exp_names)
    # int_plot_df$exp = factor(int_plot_df$exp , levels = exp_names)
    p = ggplot(int_plot_df) + 
        theme_bw(base_size=11) +
        geom_rect(mapping=aes(xmin = 4, xmax = 5, ymin = 0, ymax = 100), fill = "#d9d9d9", alpha = 0.5) +
        geom_rect(mapping=aes(xmin = 8, xmax = 9, ymin = 0, ymax = 100), fill = "#d9d9d9", alpha = 0.5) +
        geom_rect(mapping=aes(xmin = 10, xmax = 11, ymin = 0, ymax = 100), fill = "#d9d9d9", alpha = 0.5) +
        geom_ribbon(aes(x = time/73, ymin = min_cases*100, ymax = max_cases*100, fill = factor(exp)), alpha = 0.15) +
        geom_line(aes(x = time/73, y=mean*100, color = factor(exp)), lwd = 1) + 
        geom_vline(xintercept = 5.4167, linetype="dashed", size = 0.3) +
        geom_vline(xintercept = 6.4167, linetype="dashed", size = 0.3) +
        geom_vline(xintercept = 7.4167, linetype="dashed", size = 0.3) +
        scale_x_continuous(breaks=c(0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15), expand = c(0.001, 0)) +
        scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        xlab("Time (years)") + ylab(expression(paste(italic("Pf"), "PR"["0\u201399"]~" (%)"))) + 
        scale_fill_manual(values = c("#66c2a4", "#238b45" , "#006d2c")) +
        scale_color_manual(values = c("#66c2a4", "#238b45", "#006d2c")) +
        #scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2") + 
        labs(color = "Half-life levels", fill = "Half-life levels") +
        #annotate(geom="text", x=4.5, y=85, label="reference\nyear", color="black", size = 3) +
        annotate(geom="text", x=4.5, y=85, label="Reference", color="black", size = 3) +
        annotate(geom="text", x=8.5, y=85, label="Immediate", color="black", size = 3) +
        annotate(geom="text", x=10.5, y=85, label="Late", color="black", size = 3) +
        theme(text = element_text(family = "Arial")) 
        # annotate(geom="text", x=8.5, y=85, label="immediate\nfollow-up", color="black", size = 3) +
        # annotate(geom="text", x=10.5, y=85, label="late\nfollow-up", color="black", size = 3) 
    return(p)
}

### Calculate time series for different ATSB half-lives
exp_list = c("/scicore/home/penny/golmon00/MMC/TPP/simulations/test_sets/test_time_series_ATSB/")          
exp_names = factor(c("Attractive targeted sugar baits"))
p_series = plot_ts2(exp_list, exp_names[1], "seasonal", "High_indoor")


#### Plot the emulator performance
# Folders for ATSB
training_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/trained/"
as_dir = '/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/as/'
sens_dir = '/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/sensitivity/'
test_processing_dir = "/scicore/home/penny/golmon00/MMC/TPP/simulations/test_sets/test_ATSB_once_3years/postprocessing_4/"
param_ranges_file = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/param_ranges.RData"

### Plot GP emulator performance
test_plot_df = prepare_test_plot_df(as_dir, test_processing_dir, param_ranges_file)
cv_test_plot_df = prepare_plot_df_cv_test(training_dir, as_dir, test_processing_dir, param_ranges_file)
p_scatter = plot_single_scatter(test_plot_df, "Seasonal", "High_indoor", "") 
p_r_distr = plot_single_corr_box(cv_test_plot_df, "Seasonal", "High_indoor") 
p_perf = p_scatter + annotation_custom(ggplotGrob(p_r_distr), xmin = 60, xmax = 105, ymin = -17, ymax = 55) + theme(text = element_text(family = "Arial")) 


### Plot GP emulator parameter dependencies
# Extract the correct file paths for gp and parameter ranges
gp_file = ("/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/as/seeds_ATSB_once_3_years_seasonal_High_indoor_cv_as.RData")
load("/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/param_ranges.RData")
rownames(param_ranges)[which(rownames(param_ranges) == "Halflife")] = "Half-life"
rownames(param_ranges)[which(rownames(param_ranges) == "Access")] = "Access to treatment"

EIR_level = 10
num_points = 10000
gp_result_name = load(gp_file)
trained_model = get(gp_result_name)
dep = NULL
X = lhs(num_points, as.matrix(param_ranges))
X[,1] = EIR_level
X_mean = colMeans(X)
for (j in 2:ncol(X)) {
    X_new = rep.row(X_mean, num_points)
    X_new[,j] = X[,j]
    out_pred = predict(x = as.matrix(X_new), object = trained_model$GP_model)$mean
    out_pred[which(out_pred > 100)] = 100
    
    # normalise X
    norm_X = (X[,j] - min(X[,j])) / (max(X[,j]) - min(X[,j]))
    dep_rows = data.frame(x = norm_X, y = out_pred, 
                          Parameter = rownames(param_ranges)[j])
    dep = rbind(dep, dep_rows)
}
parameter_colors = create_color_palette_sens()
parameter_colors = parameter_colors[which(names(parameter_colors) %in% c("Half-life", "Coverage", "Efficacy", "Access to treatment"))]
dep[which(dep$Parameter == "Preprandial_efficacy"), "Parameter"] = "Efficacy"
parameter_colors = parameter_colors[order(factor(names(parameter_colors), levels = c("Access to treatment", "Coverage", "Efficacy", "Half-life")))]
plot_dep = ggplot(dep, aes(x = x, y = y, colour = Parameter)) + geom_line(size=1.5) +
    theme_bw(base_size=11) + scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    theme(strip.background = element_rect(colour="white", fill="white")) +
    scale_color_manual(name = "Parameter", values = parameter_colors) +
    labs( x = "Normalized input value", y = expression(paste(italic("Pf"), "PR"["0\u201399"], " reduction (%)"))) +
    theme(panel.background = element_rect(fill = 'white')) +
    theme(text = element_text(family = "Arial")) 
# labs(title = paste(exp_name, "\n", "EIR =", EIR_levels[i]), fill="", 
#         x="standardized input", y="prevalence reduction")

### Calculate GP prediction time
param_table = read.table("/scicore/home/penny/golmon00/MMC/TPP/simulations/other_tests/om_pop_size_time/param_tab.txt", 
                         header = TRUE, stringsAsFactors = FALSE)
om_folder = "/scicore/home/penny/golmon00/MMC/TPP/simulations/other_tests/om_pop_size_time/om/"

time_df = NULL
for( i in 1:nrow(param_table)) {
    # Read the OM simulation result
    OM_result_file = paste0(om_folder, param_table[i,]$Scenario_Name, "_1_time.txt")
    if(file.exists(OM_result_file) & file.info(OM_result_file)$size > 0) {
        run_time = read.table(OM_result_file)
        time_df = rbind.data.frame(time_df, cbind(param_table[i,], run_time))
    }
}
time_df = time_df[-which(time_df$popSize > 50000),]

# Plot of the time as function of the number of simulations
time_df_sub = time_df[which(time_df$popSize==10000), ]
mean_time = mean(time_df_sub$V1)
sd_time = sd(time_df_sub$V1)
df_sim = NULL
df_sim$sim_size = c(10000, 20000, 30000, 40000, 50000, 75000, 100000, 150000, 200000) #1000, 5000, 
df_sim$sim_mean = mean_time*df_sim$sim_size 
df_sim$sim_sd_plus = (mean_time+sd_time)*df_sim$sim_size
df_sim$sim_sd_minus = (mean_time-sd_time)*df_sim$sim_size
df_sim = as.data.frame(df_sim)

gp_file = "/scicore/home/penny/golmon00/MMC/TPP/simulations/ATSB_once_3years/gp_4/as/seeds_ATSB_once_3_years_seasonal_High_indoor_cv_as.RData"
ranges_file = "/scicore/home/penny/golmon00/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
EIR_fixed_lvl = 10
load(ranges_file)
param_ranges["EIR",] = c(EIR_fixed_lvl, EIR_fixed_lvl)
gp_result_name = load(gp_file)
gp_result = get(gp_result_name)

times_vec = vector(length = length(df_sim$sim_size))
for (i in 1:length(df_sim$sim_size)) {
    set_size = df_sim$sim_size[i]
    X = lhs(set_size, as.matrix(param_ranges))
    start_time = Sys.time()
    out = predict(x = as.matrix(X), object = gp_result$GP_model)
    end_time = Sys.time()
    times_vec[i] = end_time - start_time
}

# Create data frame with both results
simulation_times_df = NULL
simulation_times_df$sim_size = df_sim$sim_size
simulation_times_df$OpenMalaria = df_sim$sim_mean
simulation_times_df$GP_emulator = times_vec
simulation_times_df = as.data.frame(simulation_times_df)
simulation_times_df_m = reshape2::melt(simulation_times_df, id=c("sim_size"))
simulation_times_df_m$variable = str_replace(simulation_times_df_m$variable, "_", " ")
simulation_times_df_m$variable = factor(simulation_times_df_m$variable, levels=c("OpenMalaria", "GP emulator"))

p_time = ggplot(simulation_times_df_m, aes(x = sim_size, y = value, color = variable)) + 
    geom_point(size=3) + geom_line() + theme_bw(base_size=11) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top") +
    scale_y_continuous(
        trans = log10_trans(),
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_color_manual(values = c("#252525", "#969696")) + 
    labs(color = "", fill = "") +
    scale_x_continuous(labels=comma) +
    labs( x = "Number of points", y = "Average execution time (s)") +
    theme(text = element_text(family = "Arial")) 

figure_upper = ggarrange(p_series, p_perf, labels = c("A", "B"), legend = "top", widths = c(1.6, 1)) 
figure_lower = ggarrange( plot_dep, p_time, nrow =1, labels = c("C", "D"), widths = c(1.6, 1))

figure = ggarrange(figure_upper, figure_lower, nrow = 2) + theme(plot.margin = unit(c(0, 0.05, 0, 0), "cm"))

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig3.pdf",
       plot = figure, width = 22, height = 14, units = c("cm"), device = cairo_pdf)

ggsave("/scicore/home/penny/golmon00/MMC/TPP/figures/revision_figures/Fig3.eps",
       plot = figure, width = 22, height = 14, units = c("cm"), device = cairo_ps)

