
#Data preparation for INGARCH models
ingarch.data.preparation <- function(data_raw, 
                                     one_vs_all = F, 
                                     pivot_group = "1",
                                     zero_handling = c("none","zero_to_one")){
  
  zero_handling <- match.arg(zero_handling)
  
  data_raw <- data.preparation(data_raw=data_raw,
                               one_vs_all = one_vs_all,
                               pivot_group = pivot_group,
                               categories = c(1,2,3,4),
                               nas_to=0)
  
  if (zero_handling == "none") {
    return(data_raw)
  }
  else if (zero_handling == "zero_to_one") {
    data_raw[data_raw == 0] <- 1
    return(data_raw)
  }
  else{
    stop("Enter valid zero handling method")
    }
}


#Prediction function for INGARCH models. Fits the model for the specified category and calculates the predicted value, 
#prediction errors and PREDICTIVE INTERVALS (NOT CIs)
ingarch.prediction <- function(data,
                               category,
                               prediction_error_step=1,
                               frame=10,
                               distribution="nbinom",
                               plot=F,
                               window_method="extending"){
  
  #Selecting the group
  data <- data %>% 
          dplyr::select(c("week_date",all_of(category))) 
  
  
  #Creating fitting and prediction windows
  data_window <- windows(data,frame = frame,method=window_method,prediction_error_step = prediction_error_step)
  number_of_windows <- length(data_window)
  
  
  #Calculating the prediction for each window
  result <- lapply(c(1:number_of_windows),function(window_index){
    
    
    #Extracting fitting values, true value and last known value
    fitting_values <- data_window[[window_index]]$fitting
    true_value <- data_window[[window_index]]$prediction_value[[2]]
    last_known_value <- tail(fitting_values[[2]],n=1)
    
    
    #Fitting the model
    model <- tsglm(fitting_values[[2]],model = list("past_obs"=1,"past_mean"=2),distr = distribution)
    
    
    #Predicting the future value depending on prediction_error_step
    prediction_result <- predict(model,n.ahead=prediction_error_step,type="shortest",level=0.90)
    
    
    #Rounding it since we only have integers
    predicted_value <- round(prediction_result$pred)
    
    
    #Extracting the lower and upper prediction interval 
    prediction_interval_lower <- prediction_result$interval[1,"lower"]
    prediction_interval_upper <- prediction_result$interval[1,"upper"]
    
    
    #Calculating the prediction error
    prediction_error <- as.numeric(true_value-predicted_value)
    
  
    #Calculating the normed prediction error
    prediction_error_normed <- prediction_error
    
    return(list(
      prediction = data.frame(
        prediction_error = prediction_error,
        predicted_value = predicted_value,
        prediction_error_normed = prediction_error_normed,
        lower_bound = prediction_interval_lower,
        upper_bound = prediction_interval_upper,
        true_value = true_value,
        last_known_value = last_known_value,
        category = category,
        prediction_date = data_window[[window_index]]$prediction_value[[1]],
        distribution = distribution,
        window = window_index
      ),
      model = model
    ))
    
  })
  
  #Transforming result in a nicer format
  result_prediction <- bind_rows(unlist.element(result,"prediction"))
  result_model <- unlist.element(result,"model")
  names(result_model) <- sapply(c(1:number_of_windows),function(i){paste("window",i,sep="")})
  
  
  #Calculation the normed prediction error 
  div <- sapply(c(1:dim(result_prediction)[1]),function(i){
    
    return(normation(x = result_prediction$true_value[1:i],
                     y = result_prediction$last_known_value[1:i]))}
    )
  if(0 %in% div ) div[div==0] <- 0.5
  result_prediction$prediction_error_normed <- result_prediction$prediction_error_normed/div
  
  
  #Plotting diagnostic plots or not
  if (plot) {
    plot(model, ask = F)
  }
  
  return(list(results=result_prediction,
              models=result_model))
  
}



#Wrapper function for the analysis of the data with an Ingarch model
ingarch.analysis <- function(weekly_category_data,
                             ids,
                             prediction_error_step = 1,
                             distribution = "nbinom",
                             model_type = "ingarch",
                             plot = F,
                             categories = c("1", "2", "3", "4"),
                             frame = 10,
                             window_method = "extending",
                             zero_handling = "none") {
  
  
  stopifnot(model_type %in% c("ingarch","ingarch_one_vs_all"))
  
  
  
  #Calculating Prediction results for all ids and each category
  prediction_results_all_combinations <- lapply(ids,function(id){
    
    
    #Preparing data
    data_raw <- weekly_category_data %>%
      filter(fridge_id == id &
               main_category_id %in% as.integer(categories)) %>%
      dplyr::select(week_date, main_category_id, sold) %>%
      arrange(week_date) %>%
      ingarch.data.preparation(zero_handling=zero_handling)
    
    
    #Calculating Prediction results for each category   
    prediction_results_all_categories<-lapply(categories,function(category){
      
      prediction_result <- ingarch.prediction(data=data_raw,
                                              category=category,
                                              prediction_error_step = prediction_error_step,
                                              frame=frame,
                                              plot=F,
                                              distribution=distribution,
                                              window_method=window_method)
      
      return(list(results=bind_rows(prediction_result$results),
                  models=prediction_result$models))
      
    })
    
    
    
    #Transforming data in nicer format
    result_prediction <- bind_rows(unlist.element(prediction_results_all_categories,"results"))
    result_model <- unlist.element(prediction_results_all_categories,"models")
    names(result_model) <- categories
    result_prediction$id <- id
    result_prediction$window_method <- window_method
    
    return(list(results=result_prediction,
                models=result_model))
  })
  
  
  #Transforming data in nicer format
  result_prediction <- bind_rows(unlist.element(prediction_results_all_combinations,"results"))
  result_model <- unlist.element(prediction_results_all_combinations,"models")
  names(result_model) <- ids
  result_prediction$model <- model_type
  
  return(list(results=result_prediction,
              models=result_model))
  
 
  
}



## Plotting a specified model result
ingarch.parameter.plot <- function(result,category,element,func="idf",save=TRUE,plot.type=c("default","histogram")){
  
  
  plot.type <- match.arg(plot.type)
  ids <- unique(result$results$id)
  distributions <- unique(result$results$distribution)
  
  
  
  for (id in ids){
    for ( distribution in distributions){
      
      if(save){
        png(paste("Ingarch_plot_id",id,distribution,"category",category,plot.type,func,".png",sep=""),height = 15,width = 20,units = "cm",res=300)
      }
      
      plot_data <- result$models[[paste(id)]][[category]]
      plot_data <- unlist(lapply(c(1:length(plot_data)),function(i)return(do.call(func,list(plot_data[[i]][[element]])))))
      
      if(plot.type=="default"){
        
        plot(plot_data,type="l",ylab=paste("Distribution mean",func,sep=" "),main=paste("Ingarch_",distribution,"_id_",id,sep=""),
             sub=paste("Category: ",category,sep=""))
        
      }
      
      else if (plot.type =="histogram") {
        
        hist(plot_data,xlab=paste("Distribution mean",func,sep=" "),main=paste("Ingarch_",distribution,"_id_",id,sep=""),
             sub=paste("Category: ",category,sep=""))
        
      }else {
        
        stop("Enter valid plot type") 
        
      }

    }
    
  }
  if(save){
    dev.off()
  }
  
}






