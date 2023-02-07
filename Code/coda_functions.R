#Helper functions for the CODA model




#Prepares the data for the VAR model (pivoting to wide format, handling the zeros and transforming to ilr coordinates)
#For now written with dplyr, may be optimised later
#Currently aggregating on the main categories
#If one vs all is chosen, pivot groups have to be supplied as a character

coda.data.preparation <- function(data,
           zero_handling = c("all", "zeros_only", "none"),
           tspace = FALSE,
           transform = TRUE,
           log = FALSE,
           one_vs_all = FALSE,
           pivot_group = "1") {
  
    
    plus <- function(x,value=1){
         x[is.na(x)] <- 0
      x <- x + value
      return(x)
    }
      
    
    zero_handling <- match.arg(zero_handling)
    
    #If we compare one category to all the others we use all categories. Otherwise we only use the first 2
    
    if(one_vs_all) {
      categories <- c(1,2,3,4)
    }
    else {
      categories <- c(1,2)
    }

    data <- data.preparation(data_raw=data,
                             one_vs_all = one_vs_all,
                             pivot_group = pivot_group,
                             nas_to=0,
                             categories = categories)

    
    if (zero_handling == "none") {
      data$tsum <- rowSums(data[,-1])
      return(data)
    }
    
    if (zero_handling == "all") {
      data <- data %>% mutate(across(where(is.numeric),.fns = plus))
    }
    else if (zero_handling == "zeros_only") {
      data[data == 0] <- 0.5
    }
    else {
      stop("Enter valid zero handling option")
    }
    
    data_ilr <- cbind(data$week_date, pivotCoord(as.data.frame(data[,-1])))
    
    if (tspace) {
      if (log) {
        data_ilr$tsum <- log(rowSums(data[,-1]))
      }
      else {
        data_ilr$tsum <- rowSums(data[,-1])
      }
    }
    
    names(data_ilr)[1] <- "week_date"
    
    return(data_ilr)
  }



Eucledian <- function(x, y, standardise = 1) return(sum((x - y) / standardise) ^ 2)


## Fits the model and calculates the predictions based on the prediction windows for coda timeseries
#IMPORTANT NOTES:
#
#NOTE 1:
#
#the maximum lag must be smaller or equal to (n-1)/D 
#https://stats.stackexchange.com/questions/234975/how-many-endogenous-variables-in-a-var-model-with-120-observations
#This is necessary for computing the error covariance matrix sigma see Literature chapter 5
#Not sure why but the hard bound also does not work. Hence -1 is added
#
coda.prediction <- function(data_transformed_windows, data_notransformed_windows, data_notransformed, prediction_error_step,
                            one_vs_all,tspace, take_log, pivot_group) {
  
  prediction_results <- lapply(c(1:length(data_transformed_windows)), function(index) {
    
    
    # Selecting the fitting data and the data which should be predicted 
    fitting_data <- data_transformed_windows[[index]]$fitting[,-1]
    prediction_date <- data_transformed_windows[[index]]$prediction_value[1, 1]
    frame <- dim(fitting_data)[1]
    
    # Calculating the max possible lag. Note 1
    D <- dim(fitting_data)[2]
    lag.max <- (frame-1)/D-1
    lag.max <- max(min(lag.max,floor((frame-1)/(ncol(fitting_data[,-1])+1))-1),1) # Note 1
    
    if(lag.max==1) {
      lag.max <- NULL
    }
    
    
    #Depending on whether we have tspace or not we fit a VAR model or an AR model
    if (tspace) {
      model <- VAR(fitting_data, lag.max = lag.max, ic= "AIC")
      predicted_value <-  predict(model, fitting_data, n.ahead = prediction_error_step)
      size = 2
    }
    else{
      model <- ar(fitting_data, aic = F, order.max = 1)
      predicted_value <- predict(model, fitting_data, n.ahead = prediction_error_step)
      size = 1
    }
    
    #Initialising result vectors
    predicted_values_vector <-matrix(data = NA,nrow = 1,ncol = size)
    lower_bounds_vector<- upper_bounds_vector <- vector(mode="numeric",length=size)
    
    
    #Filling up result vectors
    for (i in 1:size) {
      predicted_values_vector[1, i] <-  predicted_value[[1]][[i]][prediction_error_step]
      lower_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+1]
      upper_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+2]
    }
    
    
    
    tsums <- as.numeric(tail(data_notransformed_windows[[index]]$fitting$tsum, 1))
    
    #Back transformations in case we use tspace
    if (tspace) {
      predicted_value <- predicted_values_vector[-size] %>%
        matrix(nrow = 1) %>%
        D2invPC()
      
      lower_bound <- lower_bounds_vector[-size] %>% 
        
        matrix(nrow = 1) %>%
        D2invPC()
      
      upper_bound <- upper_bounds_vector[-size] %>% 
        matrix(nrow = 1) %>%
        D2invPC()
      
    
      # Back transformations when we use the log of the total sum
      if (take_log) {
        predicted_value <- predicted_value * exp(predicted_values_vector[size])
        predicted_value <- (append(predicted_value, exp(predicted_values_vector[size])))
        
        lower_bound <- lower_bound * exp(predicted_values_vector[size])
        lower_bound <- (append(lower_bound, exp(predicted_values_vector[size])))
        
        upper_bound <- upper_bound * exp(predicted_values_vector[size])
        upper_bound <- (append(upper_bound, exp(predicted_values_vector[size])))
        
      }
      # Normal back transformations
      else{
        predicted_value <- predicted_value * predicted_values_vector[size]
        predicted_value <- (append(predicted_value, predicted_values_vector[size]))
        
        lower_bound <- lower_bound * predicted_values_vector[size]
        lower_bound <- (append(lower_bound, predicted_values_vector[size]))
        
        upper_bound <- upper_bound * predicted_values_vector[size]
        upper_bound <- (append(upper_bound, predicted_values_vector[size]))
      }
      
      
      #Naive prediction values. For the first time point this is the median, for the rest it is the last known value
      # if (index == 1) {
      #   naive_predicted_value <- apply(data_notransformed[,-1], 2, median, na.rm = TRUE)
      # } 
      # else{
      #   naive_predicted_value <- as.numeric(tail(data_notransformed_windows[[index]]$fitting[,-1], 1))
      # }
      
      naive_predicted_value <- as.numeric(tail(data_notransformed_windows[[index]]$fitting[,-1], 1))
  
      #Getting true value and last known values
      frame <- dim(data_notransformed_windows[[index]]$fitting)[1]
      true_value <- as.numeric(data_notransformed_windows[[index]]$prediction_value[,-1])
      last_known_value <- as.numeric(data_notransformed_windows[[index]]$fitting[frame,-1])
      
      
      if (one_vs_all) {
        category <- factor(c(pivot_group, "other", "tsum"))
      } 
      else{
        category <- factor(c("1", "2", "tsum"))
      }
      
    }
    #Back transformation if no tspace was used 
    else{
      predicted_value <- predicted_values_vector %>%
        matrix(nrow = 1) %>%
        D2invPC()
      predicted_value <- predicted_value * tsums
      
      
      lower_bound <- lower_bounds_vector %>% 
        matrix(nrow = 1) %>%
        D2invPC()
      lower_bound <- lower_bound * tsums
      
      
      upper_bound <- upper_bounds_vector %>% 
        matrix(nrow = 1) %>%
        D2invPC()
      upper_bound <- upper_bound * tsums
      
      
      true_value <- as.numeric(data_notransformed_windows[[index]]$prediction_value[,-c(1, 4)])
      
      if (one_vs_all) {
        category <- factor(c(pivot_group, "other"))
      } 
      else{
        category <- factor(c("1", "2"))
      }
      
  
      #Naive prediction values. For the first time point this is the median, for the rest it is the last known value
      # if (index == 1) {
      #   naive_predicted_value <- apply(data_notransformed[,-c(1, 4)], 2, median, na.rm = TRUE)
      #   } 
      # else{
      #     naive_predicted_value <- as.numeric(tail(data_notransformed_windows[[index]]$fitting[,-c(1, 4)], 1))
      # }
      
      naive_predicted_value <- as.numeric(tail(data_notransformed_windows[[index]]$fitting[,-c(1, 4)], 1))
    }
    
    
    predicted_value <- round(as.numeric(predicted_value))
    prediction_error <- as.numeric(true_value - predicted_value)
    prediction_error_naive <- as.numeric(true_value - naive_predicted_value)
    
    #Calculating the normed prediction error
    prediction_error_normed <- prediction_error
    
    
    if(one_vs_all){
      
      return(
        list(
          prediction = data.frame(
            prediction_error = prediction_error,
            prediction_error_naive = prediction_error_naive,
            prediction_error_normed = prediction_error_normed,
            predicted_value = predicted_value,
            lower_bound = lower_bound,
            upper_bound = upper_bound,
            true_value = true_value,
            last_known_value = last_known_value,
            naive_predicted_value = naive_predicted_value,
            category = category,
            prediction_date = prediction_date,
            pivot_group = pivot_group,
            window = index
          ),
          model = model
        )
      )
    } 
    else {
      return(
        list(
          prediction = data.frame(
            prediction_error = prediction_error,
            prediction_error_naive = prediction_error_naive,
            prediction_error_normed = prediction_error_normed,
            predicted_value = predicted_value,
            lower_bound = lower_bound,
            upper_bound = upper_bound,
            true_value = true_value,
            last_known_value = last_known_value,
            naive_predicted_value = naive_predicted_value,
            category = category,
            prediction_date = prediction_date,
            window = index
        ),
        model = model
        )
      )
    }
    })
  
  
  result_prediction <- bind_rows(unlist.element(prediction_results,"prediction"))
  result_model <- unlist.element(prediction_results,"model")
  names(result_model) <- sapply(c(1:length(result_model)),function(i){paste("window",i,sep="")})
  
  #Calculation the normed prediction error 
  div <- sapply(c(1:dim(result_prediction)[1]),function(i){
    
    return(normation(x = result_prediction$true_value[1:i],
                     y = result_prediction$last_known_value[1:i]))}
  )
  if(0 %in% div ) div[div==0] <- 0.5
  result_prediction$prediction_error_normed <- result_prediction$prediction_error_normed/div
  
  
  return(list(result = result_prediction,
              model= result_model))
}



## Wrapper function for the coda analysis. Still WIP
## Standard CI is 95%

coda.analysis<-function(weekly_category_data, ids, frame=10, zero_handling = "zeros_only", prediction_error_step = 1, take_log = T,
                        tspace = T, one_vs_all = F , pivot_groups = c("1"), model_type = "coda", window_method ="extending") {
  
  stopifnot(model_type %in% c("coda","coda_one_vs_all"))
  
  #one vs all for all pivot groups
  
  if(one_vs_all) {
  
  prediction_results_all_pivot_groups_all_ids<-lapply(ids,function(id){
    prediction_results_all_pivot_groups<-lapply(pivot_groups,function(pivot_group){
      
        #Preparing raw data
        data_raw <- weekly_category_data %>%
          filter(fridge_id == id &
                   main_category_id %in% c(1, 2, 3, 4)) %>%
          dplyr::select(week_date, main_category_id, sold) %>%
          arrange(week_date)
        
        
        #Preparing transformed data
        data_transformed <- coda.data.preparation(data_raw, 
                                               zero_handling = zero_handling,
                                               tspace = tspace, 
                                               log = take_log,
                                               one_vs_all = T,
                                               pivot_group = pivot_group) %>%
          arrange(week_date)
        #Splitting transformed data into windows
        data_transformed_windows <- windows(data_transformed,
                                         frame=frame,
                                         method = window_method,
                                         prediction_error_step = prediction_error_step)
        
        
        
        #Preparing non transformed data 
        data_notransformed <- coda.data.preparation(data_raw,
                                                        zero_handling="none",
                                                        tspace=tspace, log=F, 
                                                        one_vs_all = T,
                                                        pivot_group = pivot_group) %>%
          arrange(week_date)
        #Splitting non transformed data into windows
        data_notransformed_windows <- windows(data_notransformed,
                                            frame=frame,
                                            method = window_method,
                                            prediction_error_step = prediction_error_step)
        
        
        #Carrying out model fitting and prediction
        prediction_results <- coda.prediction(data_transformed_windows = data_transformed_windows, 
                                              data_notransformed_windows = data_notransformed_windows, 
                                              data_notransformed = data_notransformed, 
                                              prediction_error_step = prediction_error_step,
                                              one_vs_all = T,
                                              tspace = tspace,
                                              take_log =  take_log,
                                              pivot_group = pivot_group)
        prediction_results$result$id <- id
        prediction_results$result$window_method <- window_method
        prediction_results$result$model <- model_type 
      
          
        #Tidying up data
        result_prediction <- prediction_results$result
        result_model <- prediction_results$model
        
        
        return(list(results = result_prediction,
                    model = result_model))
    

     })
    
      #Tidying up data
      result_prediction <- bind_rows(unlist.element(prediction_results_all_pivot_groups,"results"))
      result_model <- unlist.element(prediction_results_all_pivot_groups,"model")
      names(result_model) <- pivot_groups
    
      return(list(results = result_prediction,
                  models = result_model))
    
    })
  
    #Tidying up data
    result_prediction <- bind_rows(unlist.element(prediction_results_all_pivot_groups_all_ids,"results"))
    result_model <- unlist.element(prediction_results_all_pivot_groups_all_ids,"models")
    names(result_model) <- ids
  
    return(list(results = result_prediction,
                models = result_model))
  }
  #Not one vs all
  else {
    
    prediction_results_all_ids <- lapply(ids,function(id){
        #Preparing raw data
        data_raw <- weekly_category_data %>%
          filter(fridge_id == id &
                   main_category_id %in% c(1, 2, 3, 4)) %>%
          dplyr::select(week_date, main_category_id, sold) %>%
          arrange(week_date)
        
        
        #Preparing transformed data
        data_transformed <- coda.data.preparation(data_raw, 
                                               zero_handling = zero_handling,
                                               tspace = tspace, 
                                               log = take_log,
                                               one_vs_all = F) %>% arrange(week_date)
        #Splitting transformed data into windows
        data_transformed_windows <- windows(data_transformed,
                                         frame= 20 ,
                                         method = window_method,
                                         prediction_error_step = prediction_error_step)
        
        
        
        #Preparing non transformed data 
        data_notransformed <- coda.data.preparation(data_raw,
                                                        zero_handling="none",
                                                        tspace=tspace,
                                                        log=take_log, 
                                                        one_vs_all = F) %>% arrange(week_date)
        #Splitting non transformed data into windows
        data_notransformed_windows <- windows(data_notransformed,
                                         frame = 20,
                                         method = window_method,
                                         prediction_error_step = prediction_error_step)
        
        
        prediction_results <- coda.prediction(data_transformed_windows = data_transformed_windows, 
                                              data_notransformed_windows = data_notransformed_windows, 
                                              data_notransformed = data_notransformed, 
                                              prediction_error_step = prediction_error_step,
                                              one_vs_all = F,
                                              tspace = tspace,
                                              take_log =  take_log)
        prediction_results$result$id <- id
        prediction_results$result$window_method <- window_method
        prediction_results$result$model <- model_type 
        
        
        #Tidying up data
        result_prediction <- prediction_results$result
        result_model <- prediction_results$model
        
        return(list(results = result_prediction,
                    models = result_model))
        
    })
    
    #Tidying up data
    result_prediction <- bind_rows(unlist.element(prediction_results_all_ids,"results"))
    result_model <- unlist.element(prediction_results_all_ids,"models")
    names(result_model) <- ids
    
    return(list(results = result_prediction,
                models = result_model))
    
  }
  
}


## Inverse Pivot coordinates in the case of D=2
## See Coda1 pdf p 4

D2invPC <- function(x, norm = "orthonormal") {
  if (!(norm %in% c("orthogonal", "orthonormal")))
    stop("only orthogonal and orthonormal is allowd for norm")
  x <- -x
  y <- matrix(0, nrow = nrow(x), ncol = ncol(x) + 1)
  D <- ncol(x) + 1
  if (norm == "orthonormal")
    y[, 1] <- -sqrt((D - 1) / D) * x[, 1]
  else
    y[, 1] <- x[, 1]
  for (i in 2:ncol(y)) {
    for (j in 1:(i - 1)) {
      y[, i] = y[, i] + x[, j] / if (norm == "orthonormal")
        sqrt((D - j + 1) * (D - j))
      else
        1
    }
  }
  
  if (ncol(y) > 2) {
    for (i in 2:(ncol(y) - 1)) {
      y[, i] = y[, i] - x[, i] * if (norm == "orthonormal")
        sqrt((D - i) / (D - i + 1))
      else
        1
    }
  }
  
  yexp = exp(y)
  x.back = yexp / apply(yexp, 1, sum)
  if (is.data.frame(x))
    x.back <- data.frame(x.back)
  return(x.back)
  
}

coda.plot.name <- function(id,zero_handling,tspace,take_log){
  if(tspace){
    tspace_char<-"_tspace"
    
    if(take_log){
      take_log_char<-"_log"
    } 
    else{
      take_log_char<-""
    }
    
  } 
  else{
    tspace_char<-""
    take_log_char<-""
  }
  
  name<-paste("fridge_","id",id,"_zero_handling_",zero_handling,take_log_char,tspace_char,sep="")
  return(name)
}



coda.get.lag <- function (coda_result) {
  
  coda_result_model <- coda_result$models
  
  ids <- names(coda_result_model)
  pivot_groups <- unique(coda_result$results$pivot_group)
  lags <- NULL

  
  if (is.null(pivot_groups)) {
    
    for (id in ids) {
      window_number <- length(coda_result_model[[id]])
      lags <-
        append(lags, unlist(unlist.element(coda_result_model[[id]], element = "p")))
    }
    id <- rep(ids, each = (window_number))
    return(data.frame(
      lag = lags,
      id = id,
      window = c(1:window_number)
    ))
  }
  else{
    
    for(id in ids){
      window_number <- length(coda_result_model[[id]][[1]])
      for(pivot_group in pivot_groups){
        lags <- append(lags,unlist(unlist.element(coda_result_model[[id]][[pivot_group]],element= "p")))
        
      }
    }
    category <- rep(pivot_groups,each=window_number)
    id <- rep(ids,each=(length(pivot_groups)*window_number))
    return(data.frame(lag=lags,
                      id=id,
                      category=category,
                      window=c(1:window_number)))
  }
}




coda.get.coefficients <- function(coda_result) {
  
  coda_result_model <- coda_result$models
  id <- names(coda_result_model)
  pivot_groups <- unique(coda_result$results$pivot_group)
  model_det <- NULL
  lags <- NULL
  window_all <- NULL
  
  if (is.null(pivot_groups)) {
  
      window_number <- length(coda_result_model[[id]])
      for(window in 1:window_number){
        lags[window] <- coda_result_model[[id]][[window]][["p"]]
        
        x <- sapply(Acoef(coda_result_model[[id]][[window]]),norm,type="F")

        window_all <- append(window_all,rep(window,length(x)))
        
        model_det <- append(model_det, x)
      }
       
    return(data.frame(
      determinant =  model_det,
      id = id,
      window = window_all))
  }
  else{
    category <- NULL
      for(pivot_group in pivot_groups){
        window_number <- length(coda_result_model[[id]][[pivot_group]])
        for(window in 1:window_number){
          lags[window] <- coda_result_model[[id]][[pivot_group]][[window]][["p"]]
          
          x<-sapply(Acoef(coda_result_model[[id]][[pivot_group]][[window]]),norm,type="F")
          
          category <- append(category,rep(pivot_group,length(x)))
          
          window_all <- append(window_all,rep(window,length(x)))
          
          model_det <- append(model_det, x)
        }

      }
    return(data.frame(model_det=model_det,
                      id=id,
                      category=category,
                      window=window_all))
  }
  
}
