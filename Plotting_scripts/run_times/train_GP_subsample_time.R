#############################
# Training GP on a database of simulations
#
# INPUT: 
#       split_file: file with the data points for a setting
#       results_folder: folder where the trained object will be saved
#       predicted: name of the quantity to be predicted (e.g. prev_red for prevalence reduction)
#       ranges_file: file with parameter names and ranges of their values
#
# OUTPUT: an Rdata file comtaining a list with the following attributes:
#           model: the trained GP model
#           EIR: the setting EIR
#           seasonality: the setting seasonality
#           biting_pattern: the mosquito biting pattern
#
# created 28.11.2018
# monica.golumbeanu@unibas.ch
#############################

source("~/MMC/TPP/scripts_v38/3_GP_train/GP_toolbox.R")
library(dplyr)
set.seed(5)

# args = commandArgs(TRUE)
# split_file = args[1]
# results_folder = args[2]
# predicted = args[3]
# ranges_file = args[4]
# sample_run = args[5]

# Only for testing:
split_file = "MMC/TPP/simulations/MAB_once_3years_avg_prev/postprocessing_4/seeds_MAB_once_3_years_seasonal_Mid_indoor.txt"
# results_folder = "MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained_subsamples/"
predicted = "prev_red"
ranges_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
sample_run = 1

OM_result = read.table(split_file, sep="\t", header = TRUE, as.is = TRUE)
exp_name = tools::file_path_sans_ext(basename(split_file))
ranges = load(ranges_file)

if((predicted %in%colnames(OM_result)) == FALSE) {
    stop(paste("Column ", predicted, "not found.", sep=""))
}

# Select only the entries where the prevalence pre-deployment was not 0
input_data = OM_result[which(OM_result$initial_prev > 0), ]

# Select relevant columns from dataframe such that the last column is the predicted one:
input_data = input_data[, c(rownames(param_ranges), predicted)]

# Identify the unique data points
unique_input_data = unique(input_data[, rownames(param_ranges)])

# Subsample from the simulations database for different sample sizes
subsample_sizes = c(seq(from = 10, to = 100, by = 20), seq(from = 100, to = 1000, by = 100))
times_vec = vector(length = length(subsample_sizes))
run_results = NULL
for (runs in 1:5) {
    run_df = NULL
    run_df$run = rep(runs, length(subsample_sizes))
    for (i in 1:length(subsample_sizes)) {
        sample_size = subsample_sizes[i]
        print(paste("Base training set size:", sample_size))
        sampled_indices = sample(1:nrow(unique_input_data), size = min(c(sample_size, nrow(unique_input_data))), replace = FALSE)
        sampled_unique_data = unique_input_data[sampled_indices, ]
        sampled_data = inner_join(sampled_unique_data, input_data)
        
        print(paste("Training GP with:", nrow(sampled_data), "points"))
        start_time = Sys.time()
        trained_model = train_GP(sampled_data) 
        end_time = Sys.time()
        times_vec[i] = end_time - start_time
    
        # training_result$model = trained_model
        # training_result$seasonality = unique(OM_result$Seasonality)
        # training_result$biting_pattern = unique(OM_result$Biting_pattern)
        # training_result$sample_size = sample_size
        # training_result$sample_run = sample_run
        # training_result$times_vec = times_vec
        # train_file = paste(results_folder, exp_name, "_", sample_size, "_", sample_run, ".RData", sep="")
        # save(training_result, file = train_file)
    }
    run_df$sample_size = subsample_sizes
    run_df$total_size = rep(nrow(sampled_data), length(subsample_sizes))
    run_df$time = times_vec
    run_results = rbind.data.frame(run_results, run_df)
}

# ggplot(run_results, aes(sample_size, , fill= opt_param))
p1 = ggplot(run_results, aes(x = factor(10*sample_size), y = time)) + 
    geom_boxplot(color = "#d95f0e") + theme_bw(base_size=14) + #geom_hline(yintercept = 10, linetype = "dashed", color = "grey") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Sample size", y = "Emulator training speed (sec)") +
    geom_segment(mapping = aes(x="5000", y=18, xend="5000", yend=12),
                 arrow=arrow(type = "closed", length = unit(2,"mm")),
                 size=0.2, color="black")

###### Population size
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
p2 = ggplot(time_df, aes(x = factor(popSize), y = V1)) + 
    geom_boxplot(color = "#E78AC3") + theme_bw(base_size=14) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs( x = "Simulated human population size", y = "OpenMalaria simulation speed (sec)") +
    geom_segment(mapping = aes(x="10000", y=750, xend="10000", yend=500), 
                 arrow=arrow(type = "closed", length = unit(2,"mm")), 
                 size=0.2, color="black") 

figure1 = ggarrange(p2, p1, ncol = 2, nrow = 1, labels = c("A", "B"), widths = c(1, 1.6))

ggsave("~/MMC/TPP/figures/runtimes/run_times.pdf",
       plot = figure1, width = 11, height = 4.5)
