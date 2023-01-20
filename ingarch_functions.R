
#Data preperation for INGARCH models

ingarch.data.preperation <- function(weekly_category_data,one_vs_all=F,pivot_group="1") {
  
  data_raw <- weekly_category_data %>%
    filter(fridge_id == id) %>%
    dplyr::select(week_date, main_category_id, sold) %>%
    arrange(week_date) %>%
    group_by(main_category_id, week_date) %>%
    dplyr::mutate(sold = sum(sold)) %>%
    dplyr::distinct() %>%
    pivot_wider(names_from
                = main_category_id, values_from = sold) %>%
    ungroup()
  
  if (one_vs_all) {
    data_raw <-
      data_raw %>% dplyr::select("week_date", "1", "2", "3", "4") %>%
      setnafill(type = "const", fill = 0) %>%
      dplyr::mutate(other = dplyr::select(., -all_of(pivot_group)) %>% rowSums(na.rm =
                                                                                 TRUE)) %>%
      dplyr::select("week_date", all_of(pivot_group), "other")
    
  }
  
  return(data_raw)
}


#Prediction function for INGARCH models. Fits the model for the specified category and calculates the predicted value, 
#prediction errors and PREDICTIVE INTERVALS (NOT CIs)

ingarch.prediction <- function(data,category,prediction_error_step=1,frame=10,distribution="nbinom",plot=F){
  
  data <- data %>% 
          dplyr::select(c("week_date",all_of(category))) %>% 
          na.omit()
  
  data_window <- windows(data,frame = frame,"overlapping",prediction_error_step = prediction_error_step)
  
  number_of_windows <- length(data_window)
  
  predicted_values <- bind_rows(lapply(c(1:number_of_windows),function(window_index){
    
    fitting_values <- data_window[[window_index]]$fitting
    true_value <- data_window[[window_index]]$prediction_value[[2]]
    
    model <<- tsglm(fitting_values[[2]],model = list("past_obs"=1,"past_mean"=2),distr = distribution)
    
    prediction_result <- predict(model,n.ahead=prediction_error_step,type="shortest",level=0.90)
    
    predicted_value <- round(prediction_result$pred)
    
    prediction_interval_lower <- prediction_result$interval[1,"lower"]
    prediction_interval_upper <- prediction_result$interval[1,"upper"]
    
    prediction_error <- as.numeric(true_value-predicted_value)
    
    return(
      data.frame(
        prediction_error = prediction_error,
        predicted_value = predicted_value,
        lower_bound = prediction_interval_lower,
        upper_bound = prediction_interval_upper,
        true_value = true_value,
        category = category,
        prediction_date = data_window[[window_index]]$prediction_value[[1]],
        distribution = distribution
      )
    )
    
  }))
  
  if (plot) {
    plot(model, ask = F)
  }
  
  return(predicted_values)
  
}



ingarch.analysis<-function(weekly_category_data, ids, prediction_error_step = 1,distribution="nbinom",
                           model_type="ingarch",plot=F,categories=c("1","2","3","4"),frame=10){
  
  stopifnot(model_type %in% c("ingarch","ingarch_one_vs_all"))
  
  #Preparing data
  data_raw<-ingarch.data.preperation(weekly_category_data = weekly_category_data,
                                     pivot_group = pivot_group)
  
  
  #Calculating Prediction results for all ids and each category
  prediction_results_all_combinations<-bind_rows(lapply(ids,function(id){
    prediction_results_all_categories<-bind_rows(lapply(categories,function(category){
      
      prediction_result<-ingarch.prediction(data=data_raw,category=category,prediction_error_step = prediction_error_step,
                                            frame=frame,plot=F,distribution=distribution)
      
      return(prediction_result)
      
    }))
    
    prediction_results_all_categories$id<-id
    
    return(prediction_results_all_categories)
  }))
  
  prediction_results_all_combinations$model<-model_type
  
  return(prediction_results_all_combinations)
  
}





