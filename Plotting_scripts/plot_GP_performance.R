#####################
# plot GP training results
####################

library(hetGP)
library(dplyr)
library(stringr)
library(gridExtra)
library(cowplot)
library(ggplot2)

predict_GP_test = function(gp_result, test_set, param_ranges){
    # remove the points where the initial prevalence was 0
    if (length(which(test_set$initial_prev == 0)) > 0){
        test_set = test_set[-which(test_set$initial_prev == 0), ]
    }
    test_data = test_set[, c(rownames(param_ranges), "prev_red")]
    
    D = ncol(test_data) - 1
    param_col = c(1:D)
    response_col = D + 1
    # apply the model on the test data and calculate the error
    prediction_train = predict(x = as.matrix(test_data[, param_col]), object = gp_result$GP_model)
    predicted_values = prediction_train$mean
    predicted_values[which(predicted_values < 0)] = 0
    predicted_values[which(predicted_values > 100)] = 100
    
    points_data_frame = cbind.data.frame(test_data[, param_col], 
                                         test_data[, response_col], predicted_values)
    colnames(points_data_frame) = c(rownames(param_ranges), "true_prev_red", "predicted_prev_red")
    agg_points_df = points_data_frame %>% dplyr::group_by_at(rownames(param_ranges)) %>% dplyr::summarise(true_prev_red = mean(true_prev_red, na.rm=TRUE),
                                                                                                          predicted_prev_red = mean(predicted_prev_red, na.rm=TRUE))
    return(agg_points_df)
}

plot_figures = function(points_df, plot_file_1, plot_file_2, plot_file_3) {
    ggplot(points_df, aes(x=as.factor(points_df$Biting_pattern), y=points_df$Error)) + geom_violin(color = "#d95f0e") +
        theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality) + labs( x = "Biting pattern", y = "Absolute error") +
        theme(strip.background = element_rect(colour="white", fill="white")) + ylim(0, 1)
    ggsave(plot_file_1, width = 10, height = 5)
    
    p = ggplot(points_df, aes(x=True, y=Predicted)) + geom_point(color = "#d95f0e", fill = "#d95f0e", shape = 21, alpha = 0.5, size = 3) + #stat_binhex() + 
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "True value", y = "Predicted value")+
        theme(strip.background = element_rect(colour="black", fill="white")) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(points_df$Corr, digits = 2))), x=20, y=90, check_overlap = TRUE, parse = TRUE)
    ggsave(plot_file_2, plot = p, width = 6, height = 5)
    
    p = ggplot(points_df, aes(x=True, y=Predicted)) + stat_binhex() + scale_fill_viridis_c() +
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "True value", y = "Predicted value")+
        theme(strip.background = element_rect(colour="black", fill="white")) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(points_df$Corr, digits = 2))), x=20, y=90, check_overlap = TRUE, parse = TRUE)
    ggsave(plot_file_3, plot = p, width = 6, height = 5)
}

plot_performance_GP = function(train_dir, plot_dir, plot_title, exp_name, model_pattern) {
    file.names = dir(train_dir, pattern = ".RData", full.names = TRUE)
    error_df = NULL
    points_df = NULL
    for(i in 1:length(file.names)){
        load(file.names[i])
        error_df = rbind.data.frame(error_df, cbind.data.frame(cv_result$seasonality, cv_result$biting_pattern, cv_result$test_errors))
        points_df = rbind.data.frame(points_df, cbind.data.frame(cv_result$seasonality, 
                                                                 cv_result$biting_pattern, 
                                                                 cv_result$test_points[,1], 
                                                                 cv_result$test_points[,2], 
                                                                 abs(cv_result$test_points[,1] - 
                                                                     cv_result$test_points[,2]),
                                                                 cor(cv_result$test_points[,1], 
                                                                     cv_result$test_points[,2])))
    }
    colnames(error_df) = c("Seasonality", "Biting_pattern", "Test_error")
    colnames(points_df) = c("Seasonality", "Biting_pattern", "True", "Predicted", "Error", "Corr")
    error_df$Seasonality = factor(error_df$Seasonality, levels = c("perennial", "seasonal"))
    error_df$Biting_pattern = factor(error_df$Biting_pattern, levels = c("Low_indoor", "Mid_indoor", "High_indoor"))
    
    # Plot the results
    plot1 = paste(plot_dir, model_pattern, "_cv_error_", exp_name, ".pdf", sep="")
    plot2 = paste(plot_dir, model_pattern, "_cv_corr_", exp_name, ".pdf", sep="")
    plot3 = paste(plot_dir, model_pattern, "_cv_corr_hex_", exp_name, ".pdf", sep="")
    plot_figures(points_df, plot1, plot2, plot3)
}

plot_performance_GP_test = function(gp_dir, test_dir, ranges_file, plot_dir, 
                                    plot_title, exp_name, model_pattern) {
    
    ggplot(points_df, aes(x=as.factor(points_df$Biting_pattern), y=points_df$Error)) + geom_violin(color = "#d95f0e") +
        theme_bw(base_size=16) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality) + labs( x = "Biting pattern", y = "Absolute error") +
        theme(strip.background = element_rect(colour="white", fill="white")) + ylim(0, 1)
    ggsave(plot_file_1, width = 10, height = 5)
    
    p = ggplot(points_df, aes(x=True, y=Predicted)) + geom_point(color = "#d95f0e", fill = "#d95f0e", shape = 21, alpha = 0.5, size = 3) + #stat_binhex() + 
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "True value", y = "Predicted value")+
        theme(strip.background = element_rect(colour="black", fill="white")) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(points_df$Corr, digits = 2))), x=20, y=90, check_overlap = TRUE, parse = TRUE)
    ggsave(plot_file_2, plot = p, width = 6, height = 5)
    
    p = ggplot(points_df, aes(x=True, y=Predicted)) + stat_binhex() + scale_fill_viridis_c() +
        theme_bw(base_size=14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "True value", y = "Predicted value")+
        theme(strip.background = element_rect(colour="black", fill="white")) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(points_df$Corr, digits = 2))), x=20, y=90, check_overlap = TRUE, parse = TRUE)
    ggsave(plot_file_3, plot = p, width = 6, height = 5)
    
    # Plot the results
    plot1 = paste(plot_dir, model_pattern, "_error_", exp_name, ".pdf", sep="")
    plot2 = paste(plot_dir, model_pattern, "_corr_", exp_name, ".pdf", sep="")
    plot3 = paste(plot_dir, model_pattern, "_corr_hex_", exp_name, ".pdf", sep="")
    plot_figures(points_df, plot1, plot2, plot3)
}

plot_scatter = function(plot_df, plot_dir, exp_name) {
    plot_df$Biting_pattern = paste(str_replace(plot_df$Biting_pattern, "_", " "), "biting")
    plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    p = ggplot(plot_df, aes(x=True, y=Predicted)) + geom_point(color = "#d95f0e", fill = "#d95f0e", shape = 21, alpha = 0.5, size = 2) + #stat_binhex() + 
        theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "True value", y = "Predicted value")+
        theme(strip.background = element_rect(colour=NA, fill=NA)) + xlim(0, 100) + ylim(0, 100) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        geom_text(aes(label = paste("r^2 ==", round(plot_df$Corr, digits = 3))), x=20, y=90, check_overlap = TRUE, parse = TRUE, size = 2)
    plot_file = paste0(plot_dir, "scatter_", exp_name, ".pdf")
    ggsave(plot_file, plot = p, width = 4, height = 3)
}

plot_single_scatter = function(plot_df, seasonality, biting, plot_title) {
    selected_points = as.data.frame(plot_df[which(plot_df$Seasonality == seasonality & plot_df$Biting_pattern == biting), ])
    p = ggplot(selected_points, aes(x=True, y=Predicted)) + geom_point(color = "#d95f0e", fill = "#d95f0e", shape = 21, alpha = 0.5, size = 3) + #stat_binhex() + 
        theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        #geom_ribbon(aes(x = True, ymin=Predicted-10, ymax=Predicted+10), fill = "grey")+
        labs( x = "True value", y = "Predicted value", title = plot_title) + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        theme(strip.background = element_rect(colour=NA, fill=NA)) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(selected_points$Corr, digits = 3))), x=15, y=90, check_overlap = TRUE, parse = TRUE, size = 4)
    return(p)
}


plot_single_scatter_small = function(plot_df, seasonality, biting, plot_title) {
    selected_points = as.data.frame(plot_df[which(plot_df$Seasonality == seasonality & plot_df$Biting_pattern == biting), ])
    p = ggplot(selected_points, aes(x=True, y=Predicted)) + geom_point(color = "#d95f0e", fill = "#d95f0e", shape = 21, alpha = 0.5, size = 3) + #stat_binhex() + 
        theme_bw(base_size=14) + theme(plot.title = element_text(size=11, face = "bold")) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        #geom_ribbon(aes(x = True, ymin=Predicted-10, ymax=Predicted+10), fill = "grey")+
        labs( x = "True value", y = "Predicted value", title = plot_title) + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        theme(strip.background = element_rect(colour=NA, fill=NA)) + xlim(0, 100) + ylim(0, 100) +
        geom_text(aes(label = paste("r^2 ==", round(selected_points$Corr, digits = 3))), x=10, y=90, check_overlap = TRUE, parse = TRUE, size = 4)
    return(p)
}

plot_error = function(plot_df, plot_dir, exp_name) {
    plot_df$Biting_pattern = str_replace(plot_df$Biting_pattern, "_indoor", "")
    #strsplit(x$Biting_pattern, "_")[[1]][1]
    plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low", "Mid", "High"))
    p = ggplot(plot_df, aes(x=as.factor(plot_df$Biting_pattern), y=plot_df$Error)) + geom_violin(color = "#d95f0e") +
        theme_bw(base_size=10) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality) + labs( x = "Indoor biting", y = "Absolute error") +
        theme(strip.background = element_rect(colour=NA, fill=NA)) + ylim(0, 1)
    plot_file = paste0(plot_dir, "error_", exp_name, ".pdf")
    ggsave(plot_file, plot = p, width = 4, height = 3)
}

# Plots the performance of a GP model when trained on different training set sizes
plot_subsample_performance = function(plot_df, plot_dir, exp_name) {
    plot_df$Biting_pattern = paste(str_replace(plot_df$Biting_pattern, "_", " "), "biting")
    plot_df$Biting_pattern = factor(plot_df$Biting_pattern, levels = c("Low indoor biting", "Mid indoor biting", "High indoor biting"))
    optimal_points = plot_df[which(plot_df$Sample_size == 600),]
    optimal_points2 = optimal_points %>% dplyr::group_by_at(c("Seasonality", "Biting_pattern", "Sample_size")) %>% 
        dplyr::summarise(Corr = mean(Corr, na.rm=TRUE))
    p = ggplot(plot_df, aes(x=Sample_size, y=Corr)) + 
        geom_smooth(se=FALSE, method="loess", span=0.2, lwd=0.5, color = "#0570b0") +
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="errorbar", color="#cb181d", width=0.2) +
        stat_summary(fun.y=mean, geom="point", color="#cb181d", size = 1.5) +
        geom_hline(data = optimal_points2, aes(yintercept=Corr), linetype="dashed", color = "#737373") +
        theme_bw(base_size=12) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        facet_wrap(~Seasonality + Biting_pattern, ncol = 3) + labs( x = "Training set size", y = expression("r"^{2})) +
        theme(strip.background = element_rect(colour=NA, fill=NA)) + ylim(-0.1, 1) + xlim(10,1000) +
        scale_x_continuous(breaks = c(10, 200, 400, 600, 800, 1000))
        
    return(p)
}


prepare_test_plot_df = function(gp_dir, test_dir, ranges_file) {
    file.names = dir(gp_dir, pattern = ".RData", full.names = TRUE)
    points_df = NULL
    ranges = load(ranges_file)
    for(i in 1:length(file.names)){
        gp_result_name = load(file.names[i])
        gp_result = get(gp_result_name)
        
        # test_set_name = paste0(model_pattern, "_", 
        #                        gp_result$seasonality, "_", gp_result$biting_pattern, ".txt")
        test_set_name = dir(test_dir, pattern = paste0("seeds_.*", gp_result$seasonality, "_", 
                                                       gp_result$biting_pattern, "\\.txt"), full.names = TRUE)
        test_set = read.table(test_set_name, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
        # remove the points where the initial prevalence was 0
        if (length(which(test_set$initial_prev == 0)) > 0){
            test_set = test_set[-which(test_set$initial_prev == 0), ]
        }
        test_data = test_set[, c(rownames(param_ranges), "prev_red")]
        
        D = ncol(test_data) - 1
        param_col = c(1:D)
        response_col = D + 1
        # apply the model on the test data and calculate the error
        prediction_train = predict(x = as.matrix(test_data[, param_col]), object = gp_result$GP_model)
        predicted_values = prediction_train$mean
        predicted_values[which(predicted_values < 0)] = 0
        predicted_values[which(predicted_values > 100)] = 100
        
        points_data_frame = cbind.data.frame(test_data[, param_col], 
                                             test_data[, response_col], predicted_values)
        colnames(points_data_frame) = c(rownames(param_ranges), "true_prev_red", "predicted_prev_red")
        agg_points_df = points_data_frame %>% dplyr::group_by_at(rownames(param_ranges)) %>% dplyr::summarise(true_prev_red = mean(true_prev_red, na.rm=TRUE),
                                                                                                predicted_prev_red = mean(predicted_prev_red, na.rm=TRUE))
        
        # calculate each absolute error between true and predicted value
        abs_error = abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)
        # calculate the correlation between true and predicted values
        corr = cor(agg_points_df$predicted_prev_red, agg_points_df$true_prev_red)
        # calculate root mean squared error between true and predicted values
        # RMSE = sqrt(sum((agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)^2)/length(agg_points_df$true_prev_red))
        RMSE = sum(abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red))/length(agg_points_df$true_prev_red)
        points_df = rbind.data.frame(points_df, cbind.data.frame(str_to_title(gp_result$seasonality), 
                                                                 gp_result$biting_pattern, 
                                                                 agg_points_df$true_prev_red, 
                                                                 agg_points_df$predicted_prev_red, 
                                                                 abs_error, corr, RMSE))
    }
    colnames(points_df) = c("Seasonality", "Biting_pattern", "True", "Predicted", "Absolute_Error", "Corr", "Root_Mean_Squared_Error")
    points_df$Seasonality = factor(points_df$Seasonality, levels = c("Perennial", "Seasonal"))
    return(points_df)
}

prepare_test_plot_df_single = function(gp_file_name, test_file_name, ranges_file) {
    points_df = NULL
    ranges = load(ranges_file)
    gp_result_name = load(gp_file_name)
    gp_result = get(gp_result_name)
        
   test_set = read.table(test_file_name, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
        # remove the points where the initial prevalence was 0
   if (length(which(test_set$initial_prev == 0)) > 0){
       test_set = test_set[-which(test_set$initial_prev == 0), ]
   }
   test_data = test_set[, c(rownames(param_ranges), "prev_red")]
   
   D = ncol(test_data) - 1
   param_col = c(1:D)
   response_col = D + 1
   # apply the model on the test data and calculate the error
   prediction_train = predict(x = as.matrix(test_data[, param_col]), object = gp_result$GP_model)
   predicted_values = prediction_train$mean
   predicted_values[which(predicted_values < 0)] = 0
   predicted_values[which(predicted_values > 100)] = 100
   
   points_data_frame = cbind.data.frame(test_data[, param_col], 
                                        test_data[, response_col], predicted_values)
   colnames(points_data_frame) = c(rownames(param_ranges), "true_prev_red", "predicted_prev_red")
   agg_points_df = points_data_frame %>% dplyr::group_by_at(rownames(param_ranges)) %>% dplyr::summarise(true_prev_red = mean(true_prev_red, na.rm=TRUE),
                                                                                                         predicted_prev_red = mean(predicted_prev_red, na.rm=TRUE))
   
   # calculate each absolute error between true and predicted value
   abs_error = abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)
   # calculate the correlation between true and predicted values
   corr = cor(agg_points_df$predicted_prev_red, agg_points_df$true_prev_red)
   # calculate root mean squared error between true and predicted values
   # RMSE = sqrt(sum((agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)^2)/length(agg_points_df$true_prev_red))
   RMSE = sum(abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red))/length(agg_points_df$true_prev_red)
   points_df = rbind.data.frame(points_df, cbind.data.frame(str_to_title(gp_result$seasonality), 
                                                            gp_result$biting_pattern, 
                                                            agg_points_df$true_prev_red, 
                                                            agg_points_df$predicted_prev_red, 
                                                            abs_error, corr, RMSE))
    colnames(points_df) = c("Seasonality", "Biting_pattern", "True", "Predicted", "Absolute_Error", "Corr", "Root_Mean_Squared_Error")
    points_df$Seasonality = factor(points_df$Seasonality, levels = c("Perennial", "Seasonal"))
    return(points_df)
}


prepare_test_plot_df_single_extended = function(gp_file_name, test_file_name, ranges_file) {
  points_df = NULL
  ranges = load(ranges_file)
  gp_result_name = load(gp_file_name)
  gp_result = get(gp_result_name)
  
  test_set = read.table(test_file_name, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
  # remove the points where the initial prevalence was 0
  if (length(which(test_set$initial_prev == 0)) > 0){
    test_set = test_set[-which(test_set$initial_prev == 0), ]
  }
  test_data = test_set[, c(rownames(param_ranges), "prev_red")]
  
  D = ncol(test_data) - 1
  param_col = c(1:D)
  response_col = D + 1
  # apply the model on the test data and calculate the error
  prediction_train = predict(x = as.matrix(test_data[, param_col]), object = gp_result$GP_model)
  predicted_values = prediction_train$mean
  predicted_values[which(predicted_values < 0)] = 0
  predicted_values[which(predicted_values > 100)] = 100
  
  points_data_frame = cbind.data.frame(test_data[, param_col], 
                                       test_data[, response_col], predicted_values)
  colnames(points_data_frame) = c(rownames(param_ranges), "true_prev_red", "predicted_prev_red")
  agg_points_df = points_data_frame %>% dplyr::group_by_at(rownames(param_ranges)) %>% dplyr::summarise(true_prev_red = mean(true_prev_red, na.rm=TRUE),
                                                                                                        predicted_prev_red = mean(predicted_prev_red, na.rm=TRUE))
  
  # calculate each absolute error between true and predicted value
  abs_error = abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)
  # calculate the correlation between true and predicted values
  corr = cor(agg_points_df$predicted_prev_red, agg_points_df$true_prev_red)
  # calculate root mean squared error between true and predicted values
  # RMSE = sqrt(sum((agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)^2)/length(agg_points_df$true_prev_red))
  RMSE = sum(abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red))/length(agg_points_df$true_prev_red)
  points_df = rbind.data.frame(points_df, cbind.data.frame(str_to_title(gp_result$seasonality), 
                                                           gp_result$biting_pattern, 
                                                           agg_points_df, 
                                                           abs_error, corr, RMSE))
  # colnames(points_df) = c("Seasonality", "Biting_pattern", "True", "Predicted", "Absolute_Error", "Corr", "Root_Mean_Squared_Error")
  # points_df$Seasonality = factor(points_df$Seasonality, levels = c("Perennial", "Seasonal"))
  return(points_df)
}

prepare_plot_df_cv_test = function(gp_dir, as_dir, test_dir, ranges_file){
    # Read the data
    cv_file.names = dir(gp_dir, pattern = ".RData", full.names = TRUE)
    as_file.names = dir(as_dir, pattern = ".RData", full.names = TRUE)
    load(ranges_file)
    
    # Build the cross-validation table
    cv_tab = NULL
    for(i in 1:length(cv_file.names)){
        # Read the CV result for model i
        gp_result_name = load(cv_file.names[i])
        gp_cv_result = get(gp_result_name)
        
        # Construct the data frame with the cross validation results on the K test sets
        cv_test_tab = gp_cv_result$test_data
        selected_cols = cbind.data.frame(cv_test_tab[,c("i", "prev_red", "predicted_prev_red")], str_to_title(gp_cv_result$seasonality),  gp_cv_result$biting_pattern)
        colnames(selected_cols) = c("i", "cv_prev_red", "cv_predicted_prev_red", "Seasonality", "Biting_pattern")
        
        selected_cols = selected_cols %>% dplyr::group_by_at(c("i", "Seasonality", "Biting_pattern")) %>% 
                                dplyr::summarise(corr_cv = cor(cv_prev_red, cv_predicted_prev_red),
                                                 err_cv = sum(abs(cv_prev_red - cv_predicted_prev_red))/length(cv_prev_red))
        
        cv_tab = rbind.data.frame(cv_tab, selected_cols, stringsAsFactors = FALSE)
    }
    
    # Build the test table
    test_tab = NULL
    for(i in 1:length(as_file.names)){
        # Read the AS model i
        gp_result_name = load(as_file.names[i])
        gp_as_result = get(gp_result_name)
        
        # Read the test data for model i
        test_file.name = dir(test_dir, pattern = paste0("seeds_.*", gp_as_result$seasonality, "_", 
                                                         gp_as_result$biting_pattern, "\\.txt"), full.names = TRUE)
        test_set = read.table(test_file.name, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
        
        # Predict with the as model for the test data points
        gp_test_pred = predict_GP_test(gp_as_result, test_set, param_ranges)
        
        # calculate the correlation between true and predicted values
        corr_test = cor(gp_test_pred$predicted_prev_red, gp_test_pred$true_prev_red)
        test_tab = rbind.data.frame(test_tab, cbind.data.frame(str_to_title(gp_as_result$seasonality), 
                                                                 gp_as_result$biting_pattern, 
                                                                 corr_test))
    }
    colnames(test_tab) = c("Seasonality", "Biting_pattern", "corr_test")
    
    # Merge cv and test tables
    all_tab = merge(cv_tab, test_tab, by = c("Seasonality", "Biting_pattern"))
    return(all_tab)
}

prepare_plot_df_cv_single = function(gp_file, test_file, ranges_file){
    load(ranges_file)
    # Build the cross-validation table
    cv_tab = NULL
    gp_result_name = load(gp_file)
    gp_cv_result = get(gp_result_name)
    
    # Construct the data frame with the cross validation results on the K test sets
    cv_test_tab = gp_cv_result$test_data
    selected_cols = cbind.data.frame(cv_test_tab[,c("i", "ind_red", "predicted_prev_red")], str_to_title(gp_cv_result$seasonality),  gp_cv_result$biting_pattern)
    colnames(selected_cols) = c("i", "cv_inc_red", "cv_predicted_inc_red", "Seasonality", "Biting_pattern")
    
    selected_cols = selected_cols %>% dplyr::group_by_at(c("i", "Seasonality", "Biting_pattern")) %>% 
        dplyr::summarise(corr_cv = cor(cv_inc_red, cv_predicted_inc_red),
                         err_cv = sum(abs(cv_inc_red - cv_predicted_inc_red))/length(cv_inc_red))
    
    cv_tab = rbind.data.frame(cv_tab, selected_cols, stringsAsFactors = FALSE)
  
    return(cv_tab)
}

prepare_test_plot_df_subsample = function(gp_dir, test_dir, ranges_file) {
    file.names = dir(gp_dir, pattern = ".RData", full.names = TRUE)
    points_df = NULL
    ranges = load(ranges_file)
    for(i in 1:length(file.names)){
        gp_result_name = load(file.names[i])
        gp_result = get(gp_result_name)
        
        test_set_name = dir(test_dir, pattern = paste0("agg_.*", gp_result$seasonality, "_", 
                                                        gp_result$biting_pattern, "\\.txt"), full.names = TRUE)
        # test_set_name = paste0(model_pattern, "_", 
        #                        gp_result$seasonality, "_", gp_result$biting_pattern, ".txt")
        test_set = read.table(test_set_name, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
        # remove the points where the initial prevalence was 0
        if (length(which(test_set$initial_prev == 0)) > 0){
            test_set = test_set[-which(test_set$initial_prev == 0), ]
        }
        test_data = test_set[, c(rownames(param_ranges), "prev_red")]
        
        D = ncol(test_data) - 1
        param_col = c(1:D)
        response_col = D + 1
        # apply the model on the test data and calculate the error
        prediction_train = predict(x = as.matrix(test_data[, param_col]), object = gp_result$model)
        predicted_values = prediction_train$mean
        predicted_values[which(predicted_values < 0)] = 0
        predicted_values[which(predicted_values > 100)] = 100
        
        points_data_frame = cbind.data.frame(test_data[, param_col], 
                                             test_data[, response_col], predicted_values)
        colnames(points_data_frame) = c(rownames(param_ranges), "true_prev_red", "predicted_prev_red")
        agg_points_df = points_data_frame %>% dplyr::group_by_at(rownames(param_ranges)) %>% dplyr::summarise(true_prev_red = mean(true_prev_red, na.rm=TRUE),
                                                                                                predicted_prev_red = mean(predicted_prev_red, na.rm=TRUE))
        
        abs_error = abs(agg_points_df$predicted_prev_red - agg_points_df$true_prev_red)/100
        # calculate the correlation between true and predicted values
        corr = cor(agg_points_df$predicted_prev_red, agg_points_df$true_prev_red)
        points_df = rbind.data.frame(points_df, cbind.data.frame(str_to_title(gp_result$seasonality), 
                                                                 gp_result$biting_pattern, 
                                                                 gp_result$sample_size,
                                                                 gp_result$sample_run,
                                                                 agg_points_df$true_prev_red, 
                                                                 agg_points_df$predicted_prev_red, 
                                                                 abs_error, corr))
    }
    colnames(points_df) = c("Seasonality", "Biting_pattern", "Sample_size", "Sample_run", "True", "Predicted", "Error", "Corr")
    points_df$Seasonality = factor(points_df$Seasonality, levels = c("Perennial", "Seasonal"))
    return(points_df)
}

plot_single_corr_box = function(points_df, seas, biting) {
    plot_df = points_df[which(points_df$Seasonality == seas & points_df$Biting_pattern == biting),]
    p = ggplot(plot_df, aes(x = "", y = corr_cv)) + 
        geom_boxplot() +
        geom_point(data = plot_df, aes(y = corr_test), shape = 23, fill = "lightgray", color = "black", size = 3) +
        # coord_flip() +
        theme_bw(base_size=11) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs( x = "", y = expression("r"^{2}))
    return(p)
}

create_perf_report = function(exp_dir, follow_up, test_dir, int_name){
    
    # For testing only:"
    # exp_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/PEV_once_3years/"
    # test_dir = "/scicore/home/smith/golmon00/MMC/TPP/simulations/test_sets/test_PEV_once_3years/"
    # follow_up = 4
    
    # Create the paths of the relevant folders 
    training_dir = paste0(exp_dir, "gp_", follow_up, "/trained/")
    as_dir = paste0(exp_dir, "gp_", follow_up, "/as/")
    param_ranges_file = paste0(exp_dir, "param_ranges.RData")
    test_processing_dir = paste0(test_dir, "postprocessing_", follow_up, "/")
    # Build the data frames
    test_plot_df = prepare_test_plot_df(as_dir, test_processing_dir, param_ranges_file)
    cv_test_plot_df = prepare_plot_df_cv_test(training_dir, as_dir, test_processing_dir, param_ranges_file)
    perf_df = merge(unique(test_plot_df[,c("Seasonality", "Biting_pattern", "Corr", "Root_Mean_Squared_Error")]), cv_test_plot_df, 
                    by = c("Seasonality", "Biting_pattern"))
    colnames(perf_df) = c("Seasonality", "Biting_pattern", "Out_of_sample_corr", "Out_of_sample_ME", 
                          "CV_iteration", "CV_train_corr", "CV_test_ME", "CV_test_corr")
    perf_df$Follow_up = follow_up
    perf_df$Intervention = int_name
    col_order = c("Intervention", "Seasonality", "Biting_pattern", "Follow_up", "CV_iteration", "CV_train_corr", "CV_test_ME", 
                  "CV_test_corr", "Out_of_sample_corr", "Out_of_sample_ME")
    perf_df = perf_df[, col_order]
    # Build the plot
    p_scatter = plot_single_scatter_small(test_plot_df, "Seasonal", "High_indoor", int_name) 
    return(list(corr_plot = p_scatter, perf_df = perf_df))
}



