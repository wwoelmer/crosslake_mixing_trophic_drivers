
# script to run an autoregressive linear model given a dataset, target variable, and target covariate
# function will determine the significant amount of lags to include for the target variable, based on pacf
# and export the model's r2 and covariate diagnostics (covariate values, std error, and pvalue)

run_ar <- function(data, id_var, id_covar, window_length = 99, lag_id = TRUE){
  library(tidyverse)
  library(MuMIn)
  library(tidymodels)
  
  if(lag_id==TRUE){
    #### look at acf and pacf to extract significant lags
    x <- as.data.frame(data[,id_var])
    par(mfrow = c(1, 1))
    pacf(x, plot = TRUE)
    acf(x, plot = TRUE)
    r <- pacf(x, plot = FALSE)$acf
    lag_save <- which(abs(r) >= qnorm(1 - 0.05 / 2) / sqrt(nrow(x))) 
    lag_save
    
  }else{
    lag_save <- 1
  }
  
  if(length(lag_save) >= 1){
    #print(lag_save)
    #### create df with the significant lags
    newcols <- paste0("tli_lag_", lag_save)
    sampdf <- data.frame(matrix(ncol = length(newcols), nrow = nrow(data)))
    colnames(sampdf) <- newcols
    newdf <- cbind(data, sampdf)
    
    for(i in 1:length(lag_save)){
      newdf[, ncol(data) + i] <- lag(data[,id_var], n = lag_save[i])
    }
    
    # remove NA's for AR modeling
    newdf <- na.omit(newdf)
    
    ##### create model with all lags and id_covar
    
    # setup some names
    `%notin%` <- Negate(`%in%`)
    extras <- c('date', id_var, 'lake')
    drivers_selected <- names(newdf)[names(newdf)%notin%extras]
    
    fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
    fml_selected
    
    # run the model and store outputs
    m1 <- lm(fml_selected, data = newdf)
    summary(m1)
    summ <- summary(m1)
    r2 <- summ$adj.r.squared
    covars <- summ$coefficients
    aic <- AICc(m1)
    
    # rearrange and write outputs to file
    covars <- as.data.frame(covars)
    covars <- rownames_to_column(covars, var = 'covar')
    colnames(covars) <- c('covar', 'value', 'std_error', 't_value', 'p_value')
    
    covars$id_covar <- id_covar
    covars$covar <- gsub("[()]", "", covars$covar)
    covars$aic <- aic
    covars$r2 <- r2
    covars$sig_lags <- 1 # record whether or not there were significant PACF lags (0 = no, 1 = yes)
    
  }else{
    print('no significant lags, assign 1 lag')
    lag_save <- 1
    
    newcols <- paste0("tli_lag_", lag_save)
    sampdf <- data.frame(matrix(ncol = length(newcols), nrow = nrow(data)))
    colnames(sampdf) <- newcols
    newdf <- cbind(data, sampdf)
    
    for(i in 1:length(lag_save)){
      newdf[, ncol(data) + i] <- lag(data[,id_var], n = lag_save[i])
    }
    
    # remove the first NA for AR modeling
    newdf <- newdf[-1,]
    
    ##### create model with all lags and id_covar
    
    # setup some names
    `%notin%` <- Negate(`%in%`)
    extras <- c('date', id_var, 'lake')
    drivers_selected <- names(newdf)[names(newdf)%notin%extras]
    
    fml_selected <- as.formula(paste0(id_var, " ~ ", paste(drivers_selected, collapse= "+")))
    fml_selected
    
    # run the model and store outputs
    m1 <- lm(fml_selected, data = newdf)
    summary(m1)
    summ <- summary(m1)
    r2 <- summ$adj.r.squared
    covars <- summ$coefficients
    aic <- AICc(m1)
    
    # rearrange and write outputs to file
    covars <- as.data.frame(covars)
    covars <- rownames_to_column(covars, var = 'covar')
    colnames(covars) <- c('covar', 'value', 'std_error', 't_value', 'p_value')
    
    covars$id_covar <- id_covar
    covars$covar <- gsub("[()]", "", covars$covar)
    covars$aic <- aic
    covars$r2 <- r2
    covars$sig_lags <- 0 # record whether or not there were significant PACF lags (0 = no, 1 = yes)
  }
  
  
  
  # some visual diagnostics
#  pred_train <- predict(m1, newdata = newdf)
#  pred <- predict(m1, newdata = test)
#  
#  a <- ggplot() +
#    geom_point(data = newdf, aes(x = as.Date(date), y = tli_monthly)) +
#    geom_line(data = newdf, aes(x = as.Date(date), y = pred_train, color = 'train')) +
#    geom_point(data = newdf, aes(x = as.Date(date), y = pred_train, color = 'train')) +
#    ggtitle(fml_selected) +
#    geom_hline(yintercept = 3.9) +
#    theme(plot.title = element_text(size = 8, face = "bold")) 
#  a
#  b <- ggplot(newdf, aes(x = tli_monthly, y = pred_train, color = 'train')) +
#    geom_point() +
#    geom_abline(intercept = 0, slope = 1) +
#    xlab('Monthly TLI') +
#    ylab('Prediction') +
#    theme_bw() +
#    theme(legend.position = "none") 
#  b
#  c <- ggplot(m1, aes(x = m1$residuals)) +
#    geom_histogram()
#  
#  c
  
  return(covars)
  
}



