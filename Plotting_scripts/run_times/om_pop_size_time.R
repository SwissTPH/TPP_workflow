####################################
# Generate plot on execution time
#
# created 07.04.2021
# monica.golumbeanu@unibas.ch
###################################

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

# Plot of the population size time
p = ggplot(time_df, aes(x = factor(popSize), y = V1)) + 
    geom_boxplot(color = "#E78AC3") + theme_bw(base_size=14) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Simulated human population size", y = "OpenMalaria simulation speed (sec)") +
    geom_segment(mapping = aes(x="10000", y=750, xend="10000", yend=500), 
                 arrow=arrow(type = "closed", length = unit(2,"mm")), 
                 size=0.2, color="black") 
# ggsave("~/MMC/TPP/figures/runtimes/om_pop_size_time.pdf",
#        plot = p, width = 6, height = 5)

# Plot of the time as function of the number of simulations
time_df_sub = time_df[which(time_df$popSize==10000), ]
mean_time = mean(time_df_sub$V1)
sd_time = sd(time_df_sub$V1)
df_sim = NULL
df_sim$sim_size = c(1000, 5000, 10000, 20000, 30000, 40000, 50000, 75000, 100000, 200000)
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

# Plot of the simulation time
p = ggplot(df_sim, aes(x = sim_size, y = sim_mean/3600/24)) + 
    geom_point() + theme_bw(base_size=12) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Number of OpenMalaria simulations", y = "Average execution time (days)") 

# Plot of the simulation time
times_df = NULL
times_df$times = times_vec
times_df$sample_size = df_sim$sim_size
times_df = as.data.frame(times_df)
p = ggplot(times_df, aes(x = sample_size, y = times)) + 
    geom_point() + theme_bw(base_size=12) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Number of estimated points", y = "Average emulator execution time (sec)") 

