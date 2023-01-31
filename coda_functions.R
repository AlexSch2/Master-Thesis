#Helper functions for the CODA model

#This function splits the time series into appropriate windows to determine the optimal length for the model

windows <- function(timeseries,frame,method = c("non-overlapping", "overlapping"),
           prediction_error_step = 1) {
  
    method <- match.arg(method)
    timeseries_length <- dim(timeseries)[1]
    
    stopifnot(timeseries_length >= frame)
    
    if (method == "non-overlapping") {
      window_number <-
        floor(timeseries_length / frame) - ifelse(timeseries_length %% frame < prediction_error_step, 1, 0) - 
        ifelse(frame < prediction_error_step, 1, 0) #div and prediction step
      
      start_index <-
        max(1,timeseries_length - window_number * frame - prediction_error_step)
      
      timeseries <- timeseries[start_index:timeseries_length, ]
      
      res <- lapply(c(0:(window_number - 1)), function(i) {
        return(list(fitting = timeseries[(i * frame + 1):((i + 1) * frame), ],
                    prediction_value = timeseries[((i + 1) * frame + prediction_error_step), ]))
      })
      
      names(res) <- c(1:window_number)
    }
    
    else if (method == "overlapping") {
      #Length of the series - length of the window - prediction step equals the number of windows
      window_number <- timeseries_length - frame - prediction_error_step +
        1
      
      res <- lapply(c(1:window_number), function(i) {
        return(list(fitting = timeseries[i:(frame + i - 1), ],
                    prediction_value = timeseries[(frame + i - 1 + prediction_error_step), ]))
      })
      names(res) <- c(1:window_number)
    }
    
    else {
      stop("Enter valid method")
    }
    
    return(res)
  }






#Prepares the data for the VAR model (pivoting to wide format, handling the zeros and transforming to ilr coordinates)
#For now written with dplyr, may be optimised later
#Currently aggregating on the main categories
#If one vs all is chosen, pivot groups have to be supplied as a character

coda.data.preperation <- function(data,
           zero_handling = c("all", "zeros_only", "none"),
           tspace = FALSE,
           transform = TRUE,
           log = FALSE,
           one_vs_all = FALSE,
           pivot_group = "1") {
  
#    stopifnot(is.character(pivot_group))
    
    plus <- function(x,value=0.5)
      return(x + value)
    
    zero_handling <- match.arg(zero_handling)
    
    if (one_vs_all) {
      data$main_category_id <- as.factor(data$main_category_id)
      
      levels(data$main_category_id) <-
        c(levels(data$main_category_id), "other")
      
      data <- data %>%
        group_by(week_date) %>%
        dplyr::filter(main_category_id != pivot_group) %>%
        dplyr::summarise(main_category_id = 'other',
                         sold = sum(sold) - sum()) %>%
        bind_rows(data) %>%
        arrange(week_date) %>%
        dplyr::filter(main_category_id %in% c(pivot_group, "other"))
      
      data <- data %>%
        group_by(main_category_id, week_date) %>%
        dplyr::mutate(sold = sum(sold)) %>%
        dplyr::distinct() %>%
        pivot_wider(names_from
                    = main_category_id, values_from = sold) %>%
        dplyr::select(all_of(pivot_group), "other")%>%
        setnafill(type = "const", fill = 0)
      
    }
    
    else {
      data <- data %>%
        group_by(main_category_id, week_date) %>%
        dplyr::mutate(sold = sum(sold)) %>%
        dplyr::distinct() %>%
        pivot_wider(names_from
                    = main_category_id, values_from = sold) %>%
        dplyr::select("1", "2")%>%
        setnafill(type = "const", fill = 0)
      
      
    }
    
    
    if (zero_handling == "none") {
      data$tsum <- rowSums(data[, -1])
      
      
      return(data)
    }
    
    if (zero_handling == "all") {
      data <- data %>% mutate_if(is.numeric, plus) %>%
        mutate_all( ~ replace(., is.na(.), 0.5))
    }
    
    else if (zero_handling == "zeros_only") {
      data <- data %>%
        mutate_all( ~ na_if(., 0)) %>%
        mutate_all( ~ replace(., is.na(.), 0.5))
    }
    else {
      stop("Enter valid zero handling option")
    }
    
    
    data_ilr <- cbind(data$week_date, pivotCoord(as.data.frame(data[, -1])))
    
    if (tspace) {
      if (log) {
        data_ilr$tsum <- log(rowSums(data[, -1]))
      }
      
      else {
        data_ilr$tsum <- rowSums(data[, -1])
      }
      
    }
    
    names(data_ilr)[1] <- "week_date"
    
    
    return(data_ilr)
  }



Eucledian <- function(x, y, standardise = 1) return(sum((x - y) / standardise) ^ 2)


## Fits the model and calculates the predictions based on the prediction windows for coda timeseries
coda.prediction <- function(data_prepared_windows, data_notransf_windows, data_prepared_notransf, prediction_error_step,
                            one_vs_all,tspace, take_log, pivot_group) {
  
  prediction_results <- bind_rows(lapply(c(1:length(data_prepared_windows)), function(index) {
    
    # Selecting the fitting data and the data which should be predicted 
    fitting_data <- data_prepared_windows[[index]]$fitting[,-1]
    prediction_date <- data_prepared_windows[[index]]$prediction_value[1, 1]
    
    #Depending on whether we have tspace or not we fit a VAR model or an AR model
    if (tspace) {
      model <- VAR(fitting_data, p = 1)
      
      predicted_value <-  predict(model, fitting_data, n.ahead = prediction_error_step)
      
      size = 2
    }
    
    else{
      model <- ar(fitting_data, aic = F, order.max = 1)
      
      predicted_value <- predict(model, fitting_data, n.ahead = prediction_error_step)
      
      size = 1
    }
    
    #Initialising the vectors for the prediction results and CI values
    
    # if (sum(is.na(predicted_value[[1]])) > 0) {
    #   predicted_value <- NA
    # } else {
    #   predicted_values_vector <-matrix(data = NA,nrow = 1,ncol = size)
    #   
    #   lower_bounds_vector<- upper_bounds_vector <- vector(mode="numeric",length=size)
    #   
    #   
    #   
    #   for (i in 1:size) {
    #     predicted_values_vector[1, i] <-  predicted_value[[1]][[i]][prediction_error_step]
    #     lower_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+1]
    #     upper_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+2]
    #   }
    # }
    
    predicted_values_vector <-matrix(data = NA,nrow = 1,ncol = size)
    
    lower_bounds_vector<- upper_bounds_vector <- vector(mode="numeric",length=size)
    
    
    
    for (i in 1:size) {
      predicted_values_vector[1, i] <-  predicted_value[[1]][[i]][prediction_error_step]
      lower_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+1]
      upper_bounds_vector[i] <-  predicted_value[[1]][[i]][prediction_error_step+2]
    }
    
    
    
    tsums <- as.numeric(tail(data_notransf_windows[[index]]$fitting$tsum, 1))
    
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
      if (index == 1) {
        
        naive_predicted_value <- apply(data_prepared_notransf[,-1], 2, median, na.rm = TRUE)
        
      } else{
        
        naive_predicted_value <- as.numeric(tail(data_notransf_windows[[index]]$fitting[,-1], 1))
        
      }
      
      true_value <- as.numeric(data_notransf_windows[[index]]$prediction_value[,-1])
      
      if (one_vs_all) {
        
        category <- factor(c(pivot_group, "other", "tsum"))
        
      } else{
        
        category <- factor(c("1", "2", "tsum"))
        
      }
      
      
      
     # Back transformation if no tspace was used 
    } else{
      
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
      
      
      true_value <- as.numeric(data_notransf_windows[[index]]$prediction_value[,-c(1, 4)])
      
      if (one_vs_all) {
        
        category <- factor(c(pivot_group, "other"))
        
      } else{
        
        category <- factor(c("1", "2"))
        
      }
  
      #Naive prediction values. For the first time point this is the median, for the rest it is the last known value

      if (index == 1) {
        
        naive_predicted_value <- apply(data_prepared_notransf[,-c(1, 4)], 2, median, na.rm = TRUE)
        
      } else{
        
        naive_predicted_value <- as.numeric(tail(data_notransf_windows[[index]]$fitting[,-c(1, 4)], 1))
        
      }
      
    }
    
    
    predicted_value <- round(as.numeric(predicted_value))
    
    prediction_error <- as.numeric(true_value - predicted_value)
    prediction_error_naive <- as.numeric(true_value - naive_predicted_value)
    
    if(one_vs_all){
      
      return(
        data.frame(
          prediction_error = prediction_error,
          prediction_error_naive = prediction_error_naive,
          predicted_value = predicted_value,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          true_value = true_value,
          naive_predicted_value = naive_predicted_value,
          category = category,
          prediction_date = prediction_date,
          pivot_group = pivot_group
        )
      )
      
    } else {
      
      return(
        data.frame(
          prediction_error = prediction_error,
          prediction_error_naive = prediction_error_naive,
          predicted_value = predicted_value,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          true_value = true_value,
          naive_predicted_value = naive_predicted_value,
          category = category,
          prediction_date = prediction_date
        )
      )
      
    }
    
   
    }))
  
  return(prediction_results)
}



## Wrapper function for the coda analysis. Still WIP
## Standard CI is 95%

coda.analysis<-function(weekly_category_data, ids, frame=10, zero_handling = "zeros_only", prediction_error_step = 1, take_log = T,
                        tspace = T, one_vs_all = F , pivot_groups = c("1"), model_type = "coda") {
  
  stopifnot(model_type %in% c("coda","coda_one_vs_all"))
  
  #one vs all for all pivot groups
  
  if(one_vs_all) {
  
  prediction_results_all_pivot_groups_all_ids<-bind_rows(lapply(ids,function(id){
    prediction_results_all_pivot_groups<-bind_rows(lapply(pivot_groups,function(pivot_group){
      
        #Preparing raw data
        data_raw <- weekly_category_data %>%
          filter(fridge_id == id &
                   main_category_id %in% c(1, 2, 3, 4)) %>%
          dplyr::select(week_date, main_category_id, sold) %>%
          arrange(week_date)
        
        
        #Preparing transformed data
        data_prepared <- coda.data.preperation(data_raw, zero_handling = zero_handling,tspace = tspace, log = take_log,
                                               one_vs_all = T,
                                               pivot_group = pivot_group) %>%
          arrange(week_date)
        
        data_prepared_windows <- windows(data_prepared,frame=frame,method = "overlapping",
                                            prediction_error_step = prediction_error_step)
        
        
        #Preparing non transformed data 
        
        data_prepared_notransf <- coda.data.preperation(data_raw,zero_handling="none",tspace=tspace, log=F, 
                                                        one_vs_all = T,
                                                        pivot_group = pivot_group) %>%
          arrange(week_date)
        
        data_notransf_windows <- windows(data_prepared_notransf,
                                            frame=frame,method = "overlapping",
                                            prediction_error_step = prediction_error_step)
        
        
        prediction_results<-coda.prediction(data_prepared_windows = data_prepared_windows, 
                                            data_notransf_windows = data_notransf_windows, 
                                            data_prepared_notransf = data_prepared_notransf, 
                                            prediction_error_step = prediction_error_step,
                                            one_vs_all = T,
                                            tspace = tspace,
                                            take_log =  take_log,
                                            pivot_group = pivot_group)
        
        prediction_results$id<-id
        
        return(prediction_results)
    

     }))
    
      return(prediction_results_all_pivot_groups)
    
    }))
  
    prediction_results_all_pivot_groups_all_ids$model<-model_type
  
    return(prediction_results_all_pivot_groups_all_ids)
  }
  
  #Not one vs all
  
  else {
    
    prediction_results_all_ids<-bind_rows(lapply(ids,function(id){
        
        #Preparing raw data
        data_raw <- weekly_category_data %>%
          filter(fridge_id == id &
                   main_category_id %in% c(1, 2, 3, 4)) %>%
          dplyr::select(week_date, main_category_id, sold) %>%
          arrange(week_date)
        
        
        #Preparing transformed data
        data_prepared <- coda.data.preperation(data_raw, zero_handling = zero_handling,tspace = tspace, log = take_log,
                                               one_vs_all = one_vs_all) %>%
          arrange(week_date)
        
        data_prepared_windows <- windows(data_prepared,20,method = "overlapping",
                                              prediction_error_step = prediction_error_step)
        
        
        #Preparing non transformed data 
        
        data_prepared_notransf <- coda.data.preperation(data_raw,zero_handling="none",tspace=tspace, log=take_log, 
                                                        one_vs_all = one_vs_all) %>%
          arrange(week_date)
        
        data_notransf_windows<- windows(data_prepared_notransf,
                                              20,method = "overlapping",
                                              prediction_error_step = prediction_error_step)
        
        
        prediction_results<-coda.prediction(data_prepared_windows = data_prepared_windows, 
                                            data_notransf_windows = data_notransf_windows, 
                                            data_prepared_notransf = data_prepared_notransf, 
                                            prediction_error_step = prediction_error_step,
                                            one_vs_all = F,
                                            tspace = tspace,
                                            take_log =  take_log)
        
        prediction_results$id<-id
        
        return(prediction_results)
        
    }))
    
    prediction_results_all_ids$model<-model_type
    
    return(prediction_results_all_ids)
    
  }
  
}

#Function for naming the plot of a coda model
coda.plot.name<-function(id,zero_handling,tspace,take_log){
  if(tspace){
    tspace_char<-"_tspace"
    
    if(take_log){
      take_log_char<-"_log"
    } else{
      take_log_char<-""
    }
    
  } else{
    tspace_char<-""
    take_log_char<-""
  }
  
  name<-paste("fridge_","id",id,"_zero_handling_",zero_handling,take_log_char,tspace_char,sep="")
  return(name)
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

coda.plot.name<-function(id,zero_handling,tspace,take_log){
  if(tspace){
    tspace_char<-"_tspace"
    
    if(take_log){
      take_log_char<-"_log"
    } else{
      take_log_char<-""
    }
    
  } else{
    tspace_char<-""
    take_log_char<-""
  }
  
  name<-paste("fridge_","id",id,"_zero_handling_",zero_handling,take_log_char,tspace_char,sep="")
  return(name)
}
