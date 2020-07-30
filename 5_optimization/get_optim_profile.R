###########################
# Solve the following optimization problem: given fixed (default = average) efficacy, 
# halflife and access to treatment, at what minimum coverage should the intervention
# be implemented to achieve a given prevalence reduction level at a gven transmission level
# 
# created 08.01.2019
# monica.golumbeanu@unibas.ch
###########################
library(nloptr)
library(hetGP)
library(Rsolnp)
library(metaheuristicOpt)
library(tools)
library(rapportools)
library(stringr)

# Function returning the parameter to be optimized, 
get_param = function(x, GP_model, param_vec, param_name) {
    return(x)
}

# Function returning the predicted mean prevalence reduction for a given input
get_p_red = function(x, GP_model, param_vec, param_name) {
    param_vec[param_name,] = x
    prev_red = predict(x = t(param_vec), GP_model)$mean
    if(prev_red>100) {
        prev_red = 100
    }
    # print(prev_red)
    return(prev_red)
}

# Function returning the predicted mean prevalence reduction - sd for a given input
get_p_red_sd_minus = function(x, GP_model, param_vec, param_name) {
    param_vec[param_name,] = x
    prediction_res = predict(x = t(param_vec), GP_model)
    prev_red = prediction_res$mean - sqrt(prediction_res$sd2 + prediction_res$nugs)
    if(prev_red>100) {
        prev_red = 100
    }
    return(prev_red)
}

# Function returning the predicted mean prevalence reduction + sd for a given input
get_p_red_sd_plus = function(x, GP_model, param_vec, param_name) {
    param_vec[param_name,] = x
    prediction_res = predict(x = t(param_vec), GP_model)
    prev_red = prediction_res$mean + sqrt(prediction_res$sd2 + prediction_res$nugs)
    if(prev_red>100) {
        prev_red = 100
    }
    return(prev_red)
}

# Function returning the predicted mean prevalence reduction - 2sd for a given input
get_p_red_sd2_minus = function(x, GP_model, param_vec, param_name) {
    param_vec[param_name,] = x
    prediction_res = predict(x = t(param_vec), GP_model)
    prev_red = prediction_res$mean - sqrt(2*prediction_res$sd2 + prediction_res$nugs)
    if(prev_red>100) {
        prev_red = 100
    }
    return(prev_red)
}

# Function returning the predicted mean prevalence reduction + 2sd for a given input
get_p_red_sd2_plus = function(x, GP_model, param_vec, param_name) {
    param_vec[param_name,] = x
    prediction_res = predict(x = t(param_vec), GP_model)
    prev_red = prediction_res$mean + sqrt(2*prediction_res$sd2 + prediction_res$nugs)
    if(prev_red>100) {
        prev_red = 100
    }
    return(prev_red)
}



args = commandArgs(TRUE)
gp_file = args[1]
ranges_file = args[2]
results_folder = args[3]
opt_setup_file = args[4]
opt_row = args[5]


# For testing
# gp_file = "~/MMC/TPP/simulations/ATSB_once_3years/gp_4/as/seeds_ATSB_once_3_years_seasonal_High_indoor_cv_as.RData"
# ranges_file = "~/MMC/TPP/simulations/ATSB_once_3years//param_ranges.RData"
# results_folder = "~/MMC/TPP/simulations/ATSB_once_3years//gp_4/optimisation/"
# opt_setup_file = "~/MMC/TPP/simulations/ATSB_once_3years//opt_setup.txt"
# opt_row = 8

# Retrieve the optimization specifications
opt_setup = read.table(opt_setup_file, header=TRUE, sep="\t", stringsAsFactors = FALSE)[opt_row,]
# opt_setup$Halflife = 0.334

# Load GP model and parameter ranges
gp_result_name = load(gp_file)
gp_result = get(gp_result_name)
rm(gp_result_name)
load(ranges_file)

# Define prevalence and EIR ranges
EIRValuesRange = seq(1, 25, by = 1)
prev_ranges = seq(30, 100, by = 10)

# For testing only:
# EIRValuesRange = seq(1, 3, by = 1)
# prev_ranges = c(10, 20)

# Initialize optimizer settings
opt_var_name = opt_setup$Param_opt
param_vec = opt_setup
param_vec$CM_name = NULL
param_vec$Param_opt = NULL
LB = param_ranges[opt_var_name, 1]
UB = param_ranges[opt_var_name, 2]

opt_df = point_df = NULL

for (EIR_lvl in 1:length(EIRValuesRange)) {
    param_vec["EIR"] = EIRValuesRange[EIR_lvl]
    for (prev_red_lvl in 1:(length(prev_ranges)-1)) {
        print(paste(EIRValuesRange[EIR_lvl], prev_ranges[prev_red_lvl]))
        # Find minimum parameter value for given EIR and prevalence reduction range
        
        # Calculate maximum attainable prevalence reduction
        max_param_vals = t(param_ranges[,2])
        max_param_vals[,"EIR"] = EIRValuesRange[EIR_lvl]
        max_prev_red = predict(x = max_param_vals, gp_result$GP_model)$mean
        
        # Run optimization algorithm only of prev_red_lvl is below max_prev_red
        ans_mean = ans_sd_plus = ans_sd_minus = ans_sd2_plus = ans_sd2_minus = NULL
        ans_mean$pars = ans_sd_plus$pars = ans_sd_minus$pars = ans_sd2_plus$pars = ans_sd2_minus$pars = NA
        
        if(max_prev_red >= prev_ranges[prev_red_lvl] | EIR_lvl < 10) {
            result = tryCatch({
                ans_mean = gosolnp(pars  = NULL, fixed = NULL, fun = get_param, ineqfun = get_p_red, 
                              ineqLB = c(prev_ranges[prev_red_lvl]), ineqUB = c(100), 
                              LB = LB, UB = UB, distr = rep(1, length(LB)), distr.opt = list(), 
                              n.restarts = 10, control = list(maxit = 100), n.sim = 1000,  
                              GP_model = gp_result$GP_model, param_vec = t(param_vec), param_name = opt_var_name)
                
                ans_sd_plus = gosolnp(pars  = NULL, fixed = NULL, fun = get_param, ineqfun = get_p_red_sd_plus, 
                                   ineqLB = c(prev_ranges[prev_red_lvl]), ineqUB = c(100), #c(prev_ranges[prev_red_lvl+1]), 
                                   LB = LB, UB = UB, distr = rep(1, length(LB)), distr.opt = list(), 
                                   n.restarts = 2, control = list(maxit = 100), n.sim = 500,  
                                   GP_model = gp_result$GP_model, param_vec = t(param_vec), param_name = opt_var_name)
                
                ans_sd_minus = gosolnp(pars  = NULL, fixed = NULL, fun = get_param, ineqfun = get_p_red_sd_minus, 
                                   ineqLB = c(prev_ranges[prev_red_lvl]), ineqUB = c(100),
                                   LB = LB, UB = UB, distr = rep(1, length(LB)), distr.opt = list(), 
                                   n.restarts = 2, control = list(maxit = 100), n.sim = 500,  
                                   GP_model = gp_result$GP_model, param_vec = t(param_vec), param_name = opt_var_name)
                ans_sd2_plus = gosolnp(pars  = NULL, fixed = NULL, fun = get_param, ineqfun = get_p_red_sd2_plus, 
                                      ineqLB = c(prev_ranges[prev_red_lvl]), ineqUB = c(100), #c(prev_ranges[prev_red_lvl+1]), 
                                      LB = LB, UB = UB, distr = rep(1, length(LB)), distr.opt = list(), 
                                      n.restarts = 2, control = list(maxit = 100), n.sim = 500,  
                                      GP_model = gp_result$GP_model, param_vec = t(param_vec), param_name = opt_var_name)
                
                ans_sd2_minus = gosolnp(pars  = NULL, fixed = NULL, fun = get_param, ineqfun = get_p_red_sd2_minus, 
                                       ineqLB = c(prev_ranges[prev_red_lvl]), ineqUB = c(100),
                                       LB = LB, UB = UB, distr = rep(1, length(LB)), distr.opt = list(), 
                                       n.restarts = 2, control = list(maxit = 100), n.sim = 500,  
                                       GP_model = gp_result$GP_model, param_vec = t(param_vec), param_name = opt_var_name)
            }, warning = function(w) {
                
            }, error = function(e) {
                print(paste("An error was handled:", e))
            }, finally = {
                
            })
        } else {
            print("Maximum attainable prevalence reduction is lower than desired level.")
        }
        # Update the results data frame
        point_df$EIR = EIRValuesRange[EIR_lvl]
        point_df$prev_red = prev_ranges[prev_red_lvl]
        
        
        point_df$opt_param = ans_mean$pars
        point_df$opt_param_sd_plus = ans_sd_plus$pars
        point_df$opt_param_sd_minus = ans_sd_minus$pars
        point_df$opt_param_sd2_plus = ans_sd2_plus$pars
        point_df$opt_param_sd2_minus = ans_sd2_minus$pars
        
        opt_df = rbind.data.frame(opt_df, point_df)
    }
}
# Save the optimisation results
opt_obj = NULL
opt_obj$table = opt_df
opt_obj$seasonality = gp_result$seasonality
opt_obj$biting_pattern = gp_result$biting_pattern
opt_obj$opt_param = opt_var_name
opt_obj$CM_level = opt_setup$CM_name
model_name = file_path_sans_ext(basename(gp_file))
model_name = str_remove(model_name, "cv_as")
opt_file = paste(results_folder, model_name, opt_setup$CM_name, "_", opt_setup$Param_opt, ".RData", sep="")
save(opt_obj, file = opt_file)

# Testing plot:
# ggplot(opt_df, aes(EIR, prev_red, fill= opt_param)) + geom_tile() + scale_fill_gradient(low = "#c994c7", high = "#756bb1")

