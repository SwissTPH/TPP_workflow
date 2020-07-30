##########################
# Auxiliary functions for postprocessing OpenMalaria results
#
# created 02.10.2019
# monica.golumbeanu@unibas.ch
##########################
library(rapportools)

# Calculate initial prevalence and prevalence reduction
calculate_prev = function(om_result_df, year_to_5day, years_before_interv, follow_up) {
    # Summarize results by summing up over age groups
    om_result_df$age_group=NULL
    agg_result = om_result_df %>% group_by(time, measure) %>% summarise(val = sum(value))
    total_pop = as.data.frame(agg_result[agg_result$time == 2 & agg_result$measure == 0, "val"])
    
    # Calculate the prevalence for all the monitored years
    # Prevalence = total number of infected people / total population
    n_infected = as.data.frame(agg_result[agg_result$measure == 1,])
    prev = n_infected$val/total_pop$val
    
    # Calculate the yearly average prevalence
    yearly_avg_prev = colMeans(matrix(prev, year_to_5day))
    
    initial_prev = yearly_avg_prev[years_before_interv]
    # Select the last time point according to the type of follow-up
    final_prev = yearly_avg_prev[years_before_interv + follow_up]
    
    # Calculate the prevalence reduction
    prev_red = (initial_prev - final_prev)/initial_prev * 100
    prev_red = prev_red*(prev_red >= 0) 
    if(is.empty(prev_red)) {
        prev_red = 0
    }
    return(list(initial_prev = initial_prev, prev_red = prev_red))
}

# Function which calculates prevalence reduction given an OpenMalaria
# simulation result.
calculate_outputs = function(om_result, scenario_params, follow_up) {
    paste("Checking follow-up:", follow_up)
    colnames(om_result) = c("time", "age_group", "measure", "value")
    year_to_5day = 73
    month_to_5day = 6
    years_before_interv = 5
    
    # Remove first year
    # to_remove = which(om_result$time == 1)
    # om_result = om_result[-to_remove,]
    
    # Calculate initial prevalence and reduction for all ages
    prev_vec_all = calculate_prev(om_result, year_to_5day, years_before_interv, follow_up)
    
    # Calculate initial prevalence and reduction in 2-10 year olds
    om_result_2_10 = om_result[which(om_result$age_group %in% c(3, 4)), ]
    prev_vec_2_10 = calculate_prev(om_result_2_10, year_to_5day, years_before_interv, follow_up)
    
    # Final row with outputs to return
    return_row = cbind.data.frame(scenario_params$Scenario_Name, scenario_params$SEED, 
                                  prev_vec_all$initial_prev, prev_vec_all$prev_red,
                                  prev_vec_2_10$initial_prev, prev_vec_2_10$prev_red)
    colnames(return_row) = c("Scenario_Name", "seed", "initial_prev", "prev_red", "initial_prev_2_10", "prev_red_2_10")
    return(return_row)
}

# Wrapper for looping across all simulation results and gathering postprocessing results in a table
postprocess_OM = function(results_folder, param_table_file, final_table_dest, 
                          final_seed_table_dest, follow_up) {
    
    param_table = read.table(param_table_file, sep= "\t", as.is = TRUE, header = TRUE, stringsAsFactors = FALSE)
    processed_OM_sim = NULL
    for( i in 1:nrow(param_table)) {
        # Read the OM simulation result
        OM_result_file = paste(results_folder, param_table[i,]$Scenario_Name, "_", 
                               param_table[i,]$SEED, "_out.txt", sep="")
        # Calculate the necessary outputs
        if(file.exists(OM_result_file) & file.info(OM_result_file)$size > 0) {
            # if (str_detect(OM_result_file, "_1220_")) {
            #     write("detected")
            # }
            OM_result = read.table(OM_result_file, sep="\t")
            # om_result, scenario_params, total_pop, survey_start, survey_end, int_start, int_end, pulsed_int_start
            scenario_row = calculate_outputs(OM_result, param_table[i,], follow_up)
            processed_OM_sim = rbind(processed_OM_sim, scenario_row)
        }
    }
    
    # Summarize results over seeds and create final results tables
    aggregated_OM = processed_OM_sim %>% group_by(Scenario_Name) %>% summarise(initial_prev = median(initial_prev, na.rm=TRUE),
                                                                               prev_red = median(prev_red, na.rm=TRUE))
    no_seed_table = param_table[,-c(which(colnames(param_table)=="SEED"))]
    no_seed_table = unique(no_seed_table)
    final_seed_table = merge(no_seed_table, processed_OM_sim, by = c("Scenario_Name"))
    final_table = merge(no_seed_table, aggregated_OM, by = c("Scenario_Name"))
    
    # Write result tables (summarized and with seeds) to files
    write.table(final_table, final_table_dest, sep="\t",
                col.names = TRUE, row.names = FALSE, quote=FALSE)
    write.table(final_seed_table, final_seed_table_dest, sep="\t",
                col.names = TRUE, row.names = FALSE, quote=FALSE)
}

# Wrapper for looping across all simulation results and gathering postprocessing results in a table
# Version to use during adaptive sampling, returns the postprocessing results instead 
# of writing them to a file
postprocess_OM_as = function(results_folder, param_table, follow_up) {
    processed_OM_sim = NULL
    for( i in 1:nrow(param_table)) {
        # Read the OM simulation result
        OM_result_file = paste(results_folder, param_table[i,]$Scenario_Name, "_", 
                               param_table[i,]$SEED, "_out.txt", sep="")
        # Calculate the necessary outputs
        if(file.exists(OM_result_file) & file.info(OM_result_file)$size > 0) {
            OM_result = read.table(OM_result_file, sep="\t")
            # om_result, scenario_params, total_pop, survey_start, survey_end, int_start, int_end, pulsed_int_start
            scenario_row = calculate_outputs(OM_result, param_table[i,], follow_up)
            processed_OM_sim = rbind(processed_OM_sim, scenario_row)
        }
    }
    
    # # Summarize results over seeds and create final results tables
    # aggregated_OM = processed_OM_sim %>% group_by(Scenario_Name) %>% summarise(initial_prev = median(initial_prev, na.rm=TRUE),
    #                                                                            prev_red = median(prev_red, na.rm=TRUE))
    no_seed_table = param_table[,-c(which(colnames(param_table)=="SEED"))]
    no_seed_table = unique(no_seed_table)
    final_seed_table = merge(no_seed_table, processed_OM_sim, by = c("Scenario_Name"))
    # final_table = merge(no_seed_table, aggregated_OM, by = c("Scenario_Name"))
    
    return(final_seed_table)
}

# # # # Only for testing:
# om_results_folder = "~/MMC/TPP/simulations/MAB_twice_3years/om/"
# split_file = "~/MMC/TPP/simulations/MAB_twice_3years/postprocessing_4/split/MAB_twice_3_years_seasonal_High_indoor.txt"
# dest_dir = "~/MMC/TPP/simulations/MDA_MAB_twice_3years/postprocessing_4/"
# follow_up = 4
# 
# # Create output file names
# split_name = basename(split_file)
# dest_table_agg = paste(dest_dir, "agg_", split_name, sep="")
# dest_table_seeds = paste(dest_dir, "seeds_", split_name, sep="")
# 
# # Postprocess the OpenMalaria simulations
# postprocess_OM(results_folder = om_results_folder, param_table_file = split_file,
#                final_table_dest = dest_table_agg, final_seed_table_dest = dest_table_seeds,
#                follow_up)
