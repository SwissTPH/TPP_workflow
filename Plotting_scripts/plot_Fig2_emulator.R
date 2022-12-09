##################################
# Plot time series of simulation outcomes for a given
# list of interventions
#
# monica.golumbeanu@unibas.ch
# 10.03.2020
##################################
library(RColorBrewer)
library(scales)

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_performance.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_GP_sensitivity.R")
source("~/MMC/TPP/scripts_v38/plotting/plot_optim.R")


###### Plot the simulation time series #######
plot_ts = function(exp_list, exp_names, seas) {
    # Required EIR and case management levels
    EIR_req = c(6, 7)
    CM_req = c(0.2, 0.3)

    int_plot_df = NULL
    for (exp_n in 1:length(exp_list)) {
        exp = exp_list[exp_n]
        param_tab = read.table(paste0(exp, "param_tab.txt"), sep="\t", header = TRUE, stringsAsFactors = FALSE, as.is = TRUE)
        scenarios_list = param_tab[which(param_tab$EIR > EIR_req[1] & 
                                             param_tab$EIR < EIR_req[2] &
                                             param_tab$Access > CM_req[1] &
                                             param_tab$Access < CM_req[2] &
                                             param_tab$Seasonality == seas &
                                             param_tab$Biting_pattern == "Mid_indoor"),]
        # scenario_name = scenarios_list$Scenario_Name[1]
        # scenarios_list = scenarios_list[which(scenarios_list$Scenario_Name == scenario_name),]
        # Read all the time series
        plot_df = NULL
        print(nrow(scenarios_list))
        for (i in 1:nrow(scenarios_list)) {
            sim_file = paste0(exp, "om/", scenarios_list$Scenario_Name[i], "_", scenarios_list$SEED[i], "_out.txt")
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
        sd_val = apply(plot_df, 1, sd)
        mean_val = rowMeans(plot_df)
        summary_plot_df = cbind.data.frame(agg_n_cases$time, mean_val, 
                                           mean_val - sd_val, mean_val + sd_val, exp_names[exp_n])
        colnames(summary_plot_df) = c("time", "mean", "min_cases", "max_cases", "experiment")
        int_plot_df = rbind(int_plot_df, summary_plot_df)
    }
    
    intervention_colors = create_color_palette_int()
    int_plot_df$experiment = factor(int_plot_df$experiment , levels = exp_names)
    p = ggplot(int_plot_df) + 
        theme_bw(base_size=15) +
        geom_rect(mapping=aes(xmin = 4, xmax = 5, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
        geom_rect(mapping=aes(xmin = 8, xmax = 9, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
        geom_rect(mapping=aes(xmin = 10, xmax = 11, ymin = 0, ymax = 1), fill = "#d9d9d9", alpha = 0.5) +
        geom_ribbon(aes(x = time/73, ymin = min_cases, ymax = max_cases, fill = experiment), alpha = 0.15) +
        geom_line(aes(x = time/73, y=mean, color = experiment), lwd = 1) + 
        geom_vline(xintercept = 5.4167, linetype="dashed", size = 0.3) +
        geom_vline(xintercept = 6.4167, linetype="dashed", size = 0.3) +
        geom_vline(xintercept = 7.4167, linetype="dashed", size = 0.3) +
        scale_x_continuous(breaks=c(0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15), expand = c(0.001, 0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        xlab("Time (years)") + ylab(expression(paste(italic("Pf"), "PR"["0-99"]))) + 
        scale_fill_manual(name = "Applied\ninterventions", values = intervention_colors) +
        scale_color_manual(name = "Applied\ninterventions", values = intervention_colors) +
        #scale_color_brewer(palette = "Set2") + scale_fill_brewer(palette = "Set2") + 
        # labs(color = "Intervention target", fill = "Intervention target") +
        annotate(geom="text", x=4.5, y=0.85, label="reference\nyear", color="black") + 
        annotate(geom="text", x=8.5, y=0.85, label="immediate\nfollow-up", color="black") +
        annotate(geom="text", x=10.5, y=0.85, label="late\nfollow-up", color="black") 
    
    return(p)
}

exp_list = c("~/MMC/TPP/simulations/MAB_once_3years_avg_prev/",           # anti-infective injectable
                  "~/MMC/TPP/simulations/PEV_once_3years/",               # anti-infective vaccine
                  "~/MMC/TPP/simulations/TBV_once_3years/",               # transmission-blocking vaccine
                  "~/MMC/TPP/simulations/ATSB_once_3years/",              # pre- and postprandial killing
                  "~/MMC/TPP/simulations/preprandial_once_3years/")       # preprandial killing
#"~/MMC/TPP/simulations/postprandial_once_3years/")
exp_names = factor(c("Anti-infective monoclonal", "Anti-infective vaccine", 
              "Transmission-blocking vaccine", "Attractive targeted sugar baits", "Eave tubes"))
p1 = plot_ts(exp_list, exp_names, "seasonal")


#### Plot the emulator performance
# Folders for PEV
training_dir = "~/MMC/TPP/simulations/PEV_once_3years/gp_4/trained/"
as_dir = '~/MMC/TPP/simulations/PEV_once_3years/gp_4/as/'
sens_dir = '~/MMC/TPP/simulations/PEV_once_3years/gp_4/sensitivity/'
test_processing_dir = "~/MMC/TPP/simulations/test_sets/test_PEV_once_3years/postprocessing_4/"
param_ranges_file = "~/MMC/TPP/simulations/PEV_once_3years/param_ranges.RData"

# Plot GP emulator performance
test_plot_df = prepare_test_plot_df(as_dir, test_processing_dir, param_ranges_file)
cv_test_plot_df = prepare_plot_df_cv_test(training_dir, as_dir, test_processing_dir, param_ranges_file)
p_scatter = plot_single_scatter(test_plot_df, "Seasonal", "High_indoor", "") 
p_r_distr = plot_single_corr_box(cv_test_plot_df, "Seasonal", "High_indoor") 
p_perf = p_scatter + annotation_custom(ggplotGrob(p_r_distr), xmin = 55, xmax = 100, ymin = -17, ymax = 46)


####### Plot the execution time for the emulator and OpenMalaria #######
param_table = read.table("~/MMC/TPP/simulations/other_tests/om_pop_size_time/param_tab.txt", 
                         header = TRUE, stringsAsFactors = FALSE)
om_folder = "~/MMC/TPP/simulations/other_tests/om_pop_size_time/om/"

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

# Calculate GP prediction time
gp_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/seeds_MAB_once_3_years_seasonal_High_indoor_cv.RData"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
EIR_fixed_lvl = 5
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
    geom_point(size=3) + geom_line() + theme_bw(base_size=16) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_y_continuous(
        trans = log10_trans(),
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_color_manual(values = c("#252525", "#969696")) + 
    labs(color = "", fill = "") +
    labs( x = "Number of points", y = "Average execution time (s)")


# Assemble all figures
p1 = p1 + guides(fill=guide_legend(nrow=3,byrow=TRUE))
figure = ggarrange(p1, p_perf, p_time, ncol = 3, nrow = 1,
                   legend = "top", labels = c("B", "C", "D"), widths = c(1.9, 1, 1)) 
ggsave("~/MMC/TPP/figures/main_figures/Fig2BCD.pdf",
       plot = figure, width = 12, height = 4)

