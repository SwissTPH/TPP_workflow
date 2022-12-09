##################################
# Plot time series of simulation outcomes for a given
# list of interventions
#
# monica.golumbeanu@unibas.ch
# 10.03.2020
##################################
library(RColorBrewer)

source("~/MMC/TPP/scripts_v38/plotting/color_palettes.R")

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
        geom_line(aes(x = time/73, y=mean, color = experiment), lwd = 1) + theme_bw(base_size=13) + 
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
        labs(color = "Intervention target", fill = "Intervention target") +
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


# exp_list[[2]] = c("~/MMC/TPP/simulations/cohort_MDA_MAB_once_3years/",
#              "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
#              "~/MMC/TPP/simulations/cohort_MDA_TBV_once_3years/",
#              "~/MMC/TPP/simulations/cohort_MDA_PEV_once_3years/",
#              "~/MMC/TPP/simulations/cohort_PEV_TBV_once_3years/")
# 
# exp_list[[3]] = c("~/MMC/TPP/simulations/MAB_twice_sync_3years/",
#              "~/MMC/TPP/simulations/cohort_MDA_MAB_twice_sync_3years/",
#              "~/MMC/TPP/simulations/ATSB_twice_sync_3years/")


p1 = plot_ts(exp_list, exp_names, "seasonal")
p2 = plot_ts(exp_list, exp_names, "perennial")

figure = ggarrange(p2, p1,  ncol = 1, nrow = 2, 
                   common.legend = TRUE, legend = "right", labels = c("A", "B")) 
# ggsave("~/MMC/TPP/figures/simulation_desc/simulation_ts2.pdf",
#        plot = figure, width = 9.5, height = 5)
# 

#+ rremove("x.text") + rremove("x.axis") + rremove("xlab"),

