#############################
# Toolbox with functions for training a GP regression model based on a 
# database of OpenMalaria realisations.
#
# created 17.06.2019
# monica.golumbeanu@unibas.ch
#############################

library(tgp)
library(hetGP)
library(ggplot2)
library(viridis)
library(sensitivity)
library(multisensi)
library(lhs)
library(dplyr)
library(reshape2)
library(gridExtra)

test_GP_plot = function(GP_model, test) {
    D = ncol(test) - 1
    param_col = c(1:D)
    response_col = D + 1
    # apply the model on the test data and calculate the error
    prediction = predict(x = as.matrix(test[, param_col]), object = GP_model)
    MSE.gp = sum((test[, response_col] - prediction$mean)^2)/nrow(test)
    print(MSE.gp)
    true_result = as.matrix(test[, response_col])
    plot(as.data.frame(cbind(prediction$mean, true_result)),
         col = densCols(as.data.frame(cbind(prediction$mean, true_result))),
         xlab = "prediction", ylab = "true",
         main = paste("r2 = ",round(cor(prediction$mean, true_result),2)), pch = 20)
}

test_GP = function(GP_model, train_data, test_data) {
    D = ncol(train_data) - 1
    param_col = c(1:D)
    response_col = D + 1
    # apply the model on the train data and calculate the error
    prediction_train = predict(x = as.matrix(train_data[, param_col]), object = GP_model)
    predicted_prev_red = prediction_train$mean
    train_data = cbind.data.frame(train_data, predicted_prev_red)
    
    # apply the model on the train data and calculate the error
    if(!is.null(test_data)) {
        prediction_test = predict(x = as.matrix(test_data[, param_col]), object = GP_model)
        predicted_prev_red = prediction_test$mean
        test_data = cbind.data.frame(test_data, predicted_prev_red)
    } else {
        test_data = NULL
    }
    
    return(list(train_data = train_data, test_data = test_data))
}

# Function that trains a GP regression given a training set
train_GP = function(train_data) {
    D = ncol(train_data) - 1
    param_col = c(1:D)
    response_col = D + 1
    
    print("Training GP regression model ...")
    prdata = find_reps(X = as.matrix(train_data[, param_col]), 
                       Z = as.matrix(train_data[, response_col]), 
                       rescale = FALSE, normalize = FALSE)
    GP_model = mleHetGP(X = list(X0 = as.matrix(prdata$X0), 
                                 Z0 = as.matrix(prdata$Z0), mult = prdata$mult), 
                        Z = prdata$Z, lower = rep(0.0001, D), upper = rep(10, D), 
                        covtype = "Gaussian")
    return(GP_model)
}


plot_gradient = function(data_tab, plot_file = NULL, labels_names) {
    a = interp.loess(data_tab[,1],data_tab[,2],data_tab[,3])
    #image(a, col=terrain.colors(10))
    d = as.data.frame(cbind(expand.grid(a$x, a$y), c(a$z)))
    colnames(d) = c("X", "Y", "Z")
    colfunc = colorRampPalette(c("#2c7fb8", "#7fcdbb", "#fec44f"))
    ggplot() + theme_bw() + geom_tile(data = d, aes(x = X, y = Y, fill = Z)) +
        xlab(labels_names[1]) + ylab(labels_names[2]) +
        #scale_y_continuous(limits=c(0,1)) + scale_x_continuous(limits=c(0,10)) +
        #scale_fill_gradientn(colours = viridis(50), name = labels_names[3]) + 
        scale_fill_gradient(low = "darkgrey", high = "white", name = labels_names[3])+
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(), panel.background = element_blank()) 
}

cv_train = function(input_data, K) {
    # retrieve all data points indices
    indices = c(1:nrow(input_data))
    
    # split the data in K groups
    cv_indices = sample(rep(1:K, length.out = nrow(input_data)))
    train_data = test_data = NULL
    for (i in 1:K) {
        print(paste("Performing CV run",i,"..."))
        test_points = which(cv_indices == i)
        train_points = setdiff(indices, test_points)
        trained_model = train_GP(input_data[train_points,]) 
        print(paste("Computing errors run",i,"..."))
        data_tabs = test_GP(trained_model, input_data[train_points,], 
                         input_data[test_points,])
        train_data = rbind.data.frame(train_data, cbind.data.frame(i, data_tabs$train_data))
        test_data = rbind.data.frame(test_data, cbind.data.frame(i, data_tabs$test_data))
    }
    # Train the final GP model on the entire training set
    trained_model = train_GP(input_data) 
    return(list(train_data = train_data, test_data = test_data, GP_model = trained_model))
}

rep.row = function(x,n){
    return(matrix(rep(x,each=n),nrow=n))
}
rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# Calculate the main and total effects for global sensitivity analysis
calc_sobol_idx = function(GP_model, param_spec, num_points = 200000){
    S_mat = T_mat = NULL
    
    # define wrapper for the GP_model prediction to be called by Sobol analysis
    GP_f = function(X){
        out = predict(x = as.matrix(X), object = GP_model)
        return(out$mean)
    }
    
    # construct the two random lhs samples
    X1 = lhs(num_points, as.matrix(param_spec))
    X2 = lhs(num_points, as.matrix(param_spec))
    
    # compute the Sobol indices
    SA = soboljansen(model = GP_f, as.data.frame(X1), as.data.frame(X2), nboot = 2000)
    S_eff = SA$S$original
    S_eff[S_eff < 0] = 0
    T_eff = SA$T$original
    return(list(S_eff = S_eff, T_eff = T_eff))
}
