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

args = commandArgs(TRUE)
split_file = args[1]
results_folder = args[2]
predicted = args[3]
ranges_file = args[4]

# Only for testing:
# split_file = "MMC/TPP/simulations/MAB_twice_3years/postprocessing_4/seeds_MAB_twice_3_years_seasonal_High_indoor.txt"
# results_folder = "MMC/TPP/simulations/MAB_twice_3years/gp_4/trained/"
# predicted = "prev_red"
# ranges_file = "~/MMC/TPP/simulations/MAB_twice_3years/param_ranges.RData"

OM_result = read.table(split_file, sep="\t", header = TRUE, as.is = TRUE)
exp_name = tools::file_path_sans_ext(basename(split_file))
cv_file = paste(results_folder, exp_name, "_cv.RData", sep="")
ranges = load(ranges_file)

if((predicted %in%colnames(OM_result)) == FALSE) {
    stop(paste("Column ", predicted, "not found.", sep=""))
}

# Select only the entries where the prevalence pre-deployment was not 0
input_data = OM_result[which(OM_result$initial_prev > 0), ]

# Select relevant columns from dataframe such that the last column is the predicted one:
input_data = input_data[, c(rownames(param_ranges), predicted)]

# 5-fold cross-validation
cv_result = cv_train(input_data, 5)
cv_result$seasonality = unique(OM_result$Seasonality)
cv_result$biting_pattern = unique(OM_result$Biting_pattern)
save(cv_result, file = cv_file)


