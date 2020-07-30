####################################################
# Adaptive sampling scheme for a multi-dimensional space
#
# Points are sampled uniformly then distributed into bins. 
# A selection criteria based on point density and variance is employed.
#
# created 03.12.2019
# monica.golumbeanu@unibas.ch
###################################################

library(hetGP)
library(tgp)
library(dplyr)
library(magrittr)
library(rapportools)
source("~/MMC/TPP/scripts_v38/2_postprocessing/postprocessing_resources.R")

# Function that returns a set of points to be explored with OpenMalaria and added to the training set
# Method: uniform sampling across the parameter space
select_points_uniform = function(GP_model, param_ranges, num_points) {
    # select a random set of points uniformly distributed across the parameter space
    X_random_samples = lhs(100000, param_ranges) 
    colnames(X_random_samples) = rownames(param_ranges)
    
    # Predict the output with the GP model on the new points and evaluate the variance   
    pred_obj = predict(x = X_random_samples, object = GP_model)
    new_data = cbind.data.frame(X_random_samples, pred_obj$sd2, pred_obj$mean, row.names = NULL)
    colnames(new_data) = c(colnames(X_random_samples), "variance", "prediction")
    XX = new_data[order(new_data$variance, decreasing = TRUE),]
    remove(new_data)
    
    # Calculate output point density in space and distribute points in bins
    predicted_values = XX[ ,"prediction"]
    predicted_values[which(predicted_values < 0)] = 0
    predicted_values[which(predicted_values > 100)] = 100
    br = seq(0, 102, by=34)
    freqs = hist(predicted_values, br, plot = FALSE)
    points_distr = round(num_points*(1-(freqs$counts/100000))/2)
    points_cat = findInterval(predicted_values, br)
    
    # Select points in each bin with the largest posterior predictive variance
    selected_points = NULL
    for (i in 1:length(points_distr)) {
        indices = which(points_cat == i)
        if (points_distr[i] > 0) {
            bin_points = XX[indices[1:min(c(length(indices), points_distr[i]))], ]
            selected_points = rbind.data.frame(selected_points, bin_points)
        }
    }
    
    return(list(param_tab = selected_points[, colnames(X_random_samples)], 
                variances_vec = c(min(selected_points$variance), 
                                  mean(selected_points$variance), 
                                  max(selected_points$variance))))
}

# Create parameter table from newly sampled points to be used for building corresponding scenarios 
create_param_table_from_samples = function(sampled_points, seasonality, biting, n_seeds) {
    # remove variance column
    # sampled_points$variance = NULL
    # load seasonality monthly values and mosquito biting patterns
    tanzania_s = read.table("~/MMC/resource_files/Tanzania_seasons.txt", sep="\t", header = TRUE)
    biting_patterns = read.table("~/MMC/resource_files/biting.txt", sep="\t", header = TRUE)

    # add setting parameters
    as_param_tab = cbind.data.frame(tanzania_s[which(tanzania_s$Seasonality == seasonality),], 
                                    biting_patterns[which(biting_patterns$Biting_pattern == biting),],
                                    sampled_points, row.names = NULL)
    # add seeds and define scenarios names
    SEED = c(1:n_seeds)
    scenarios_names = paste("Scenario", 1:nrow(as_param_tab), sep="_")
    # construct parameter table
    as_param_tab = cbind.data.frame(scenarios_names, as_param_tab, row.names = NULL)
    colnames(as_param_tab)[1] = "Scenario_Name"
    as_param_tab = merge(as_param_tab, as.data.frame(SEED))
    
    return(as_param_tab)
}

args = commandArgs(TRUE)
as_folder = args[1]
gp_file = args[2]
parameter_ranges = args[3]
follow_up = as.double(args[4])
predicted = args[5]

# For testing:
# as_folder = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/as/as_model_1/"
# gp_file = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/gp_4/trained/seeds_MAB_once_3_years_seasonal_Low_indoor_cv.RData"
# parameter_ranges = "~/MMC/TPP/simulations/MAB_once_3years_avg_prev/param_ranges.RData"
# follow_up = 4
# predicted = "prev_red"

print(as_folder)
print(gp_file)
print(parameter_ranges)
print(follow_up)
print(predicted)

## Load the GP model and parameter ranges
load(gp_file)
GP_updated = cv_result$GP_model
load(parameter_ranges)

## Initialization
# Set number of seeds for OM runs
n_seeds = 10
# Set number of new sampled points for updating the GP model
n_samples = 30
# Initalize variances vector
variances = NULL
om_results_dir = paste0(as_folder, "om/")
om_scenarios_dir = paste0(as_folder, "scenarios/")

## Adaptive sampling
for(as_runs in 1:5) {
    cat("Adaptive sampling run ", as_runs)
    # Remove log file before resampling
    #system(paste("rm", log_file))
    
    # Obtain new samples and create the parameter table with new data points
    new_points_obj = select_points_uniform(GP_updated, param_ranges, n_samples)
    param_tab = create_param_table_from_samples(new_points_obj$param_tab, cv_result$seasonality, 
                                                cv_result$biting_pattern, n_seeds) 
    if (nrow(param_tab > 0)) {
        print(nrow(param_tab))
        table_file = paste(as_folder, "param_tab.txt", sep="")
        write.table(param_tab, table_file, sep = "\t", quote = FALSE, col.names = TRUE,
                    row.names = FALSE)
        variances = rbind(variances, new_points_obj$variances_vec)
        
        # Create the new scenarios and run OpenMalaria simulations
        # To make the workflow more generic, the OM workflow folder could be passed as an argument to this script
        system(paste("cd ~/MMC/TPP/scripts_v38/1_OM_basic_workflow/; bash OM_base_workflow.sh", as_folder))
        
        # Run postprocessing, get the new training data and update GP
        processing_results = postprocess_OM_as(om_results_dir, param_tab, follow_up)
        processing_results = processing_results[which(processing_results$initial_prev > 0), ]
        
        if (nrow(processing_results) > 0) {
            new_train_data = processing_results[, c(rownames(param_ranges), predicted)]
            n = ncol(new_train_data)
            prdata = find_reps(X = as.matrix(new_train_data[, 1:(n-1)]), Z = as.matrix(new_train_data[, n]),
                               rescale = FALSE, normalize = FALSE)
            new_GP_updated = update(GP_updated, prdata$X0, prdata$Z0, maxiter = 0, maxit=0)
            GP_updated = new_GP_updated 
            rm(new_GP_updated)
            # Cleanup of simulations necessary before the next run of OpenMalaria
            system(paste("rm -r", om_results_dir))
            system(paste("rm -r", om_scenarios_dir))
        } else {
            print("No training points due to sampling of stochastic elimination.")
        }
        
    }
}

# Save the new GP model object to a file
as_result = list(GP_model = GP_updated, seasonality = cv_result$seasonality, 
             biting_pattern = cv_result$biting_pattern)
exp_name = tools::file_path_sans_ext(basename(gp_file))
as_file = paste(dirname(as_folder), "/", exp_name, "_as.RData", sep="")
save(as_result, file = as_file)

# Save the vector of variances
var_file = paste0(as_folder, "variances.RData")
save(variances, file = var_file)

