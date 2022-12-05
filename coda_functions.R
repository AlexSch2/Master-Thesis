#Helper functions for the CODA model

#This function splits the time series into appropriate windows to determine the optimal length for the model 

windows<-function(timeseries,frame,method=c("non-overlapping","overlapping"),prediction_error_step=1){
  
  method<-match.arg(method)
  timeseries_length<-dim(timeseries)[1]
  
  stopifnot(timeseries_length>=frame)
  
  if(method=="non-overlapping") {
  
    window_number<-floor(timeseries_length/frame) - ifelse(timeseries_length%%frame<prediction_error_step,1,0) - ifelse(frame<prediction_error_step,1,0) #div and prediction step
    
    start_index<-max(1,timeseries_length-window_number*frame-prediction_error_step)
    
    timeseries<-timeseries[start_index:timeseries_length,]
    
    res<-lapply(c(0:(window_number-1)),function(i){
      return(list(fitting=timeseries[(i*frame+1):((i+1)*frame),],
                  prediction_value=timeseries[((i+1)*frame+prediction_error_step),]))
    })
    
    names(res)<-c(1:window_number)
  }
  
  else if(method=="overlapping") {
    
    #Length of the series - length of the window - prediction step equals the number of windows
    window_number<-timeseries_length-frame-prediction_error_step+1
    
    res<-lapply(c(1:window_number),function(i){
      return(list(fitting=timeseries[i:(frame+i-1),],
                  prediction_value=timeseries[(frame+i-1+prediction_error_step),]))
    })
    names(res)<-c(1:window_number)
  }
  
  else {stop("Enter valid method")}
  
  return(res)
}






#Prepares the data for the VAR model (pivoting to wide format, handling the zeros and transforming to ilr coordinates)
#For now written with dplyr, may be optimised later
#Currently aggregating on the main categories

coda.data.preperation<-function(data,zero_handling=c("all","zeros_only"),tspace=F){
  
  plusone<-function(x)return(x+1)
  zero_handling<-match.arg(zero_handling)
  
  data<-data%>%
    group_by(main_category_id,week_date)%>%
    dplyr::mutate(sold=sum(sold))%>%
    dplyr::distinct()%>%
    pivot_wider(names_from 
                = main_category_id,values_from = sold)
  
  if(zero_handling=="all"){
    
    data<-data%>%mutate_if(is.numeric,plusone)%>%
    mutate_all(~replace(., is.na(.), 1))
  }
  
  else if(zero_handling=="zeros_only"){
    
    data<-data%>%
      mutate_all(~na_if(., 0))%>%
      mutate_all(~replace(., is.na(.), 1))
  }
  else {stop("Enter valid zero handling option")}
  
  data_ilr<-cbind(data$week_date,pivotCoord(as.data.frame(data[,-1])))
  
  if(tspace){
    
    data_ilr$tsum<-rowSums(data[,-1])
    
  }
  
  
  return(data_ilr)
}

Eucledian<-function(x,y)return(sum((x-y)^2))


#Function for calculating the prediction error 

prediction.error<-function(model,fitted_data,true_values,prediction_error_step,
                           measure="Eucledian"){
  
  measure.fun<-match.fun(measure)
  
  predicted_values<-predict(model,fitted_data,n.ahead = prediction_error_step)
  size<-length(true_values)
  
  
  predicted_values_vector<-matrix(data=NA,nrow=1,ncol=size)
    
  for(i in 1: size){
    
    predicted_values_vector[1,i]<-predicted_values$fcst[[i]][prediction_error_step]
    
  }

  if(sum(is.na(predicted_values_vector))>0){return(NA)}
  
  else{
    
    if("tsum" %in% names(true_values)){
      
      tsum_index<-which(colnames(true_values)=="tsum")
      predicted_values_inverse<-pivotCoordInv(predicted_values_vector[,-tsum_index])
      
      true_values_inverse<-pivotCoordInv(as.matrix(true_values[,-tsum_index]))
      
      return(measure.fun(cbind(predicted_values_inverse,predicted_values_vector[,tsum_index]),
                         cbind(true_values_inverse,true_values[,tsum_index])))
      
    }
    
    else {
      
      predicted_values_inverse<-pivotCoordInv(predicted_values_vector)
      true_values_inverse<-pivotCoordInv(as.matrix(true_values))
      
      return(measure.fun(predicted_values_inverse,true_values_inverse))
      
   }
    
  }

}




#Returns the best time frame length according to the specifified information criteria (ic)

#IMPORTANT NOTES:
#
#NOTE 1:
#
#the maximum lag must be smaller or equal to (n-1)/D 
#https://stats.stackexchange.com/questions/234975/how-many-endogenous-variables-in-a-var-model-with-120-observations
#This is necessary for computing the error covariance matrix sigma see Literature chapter 5
#Not sure why but the hard bound also does not work. Hence -1 is added
#
#
#NOTE 2:
#
#Right now only AIC, BIC and prediction error are implemented as an information criteria for the frame. 
#For selecting the optimal lag only AIC,HQ,SC and FPE are implemented


coda.tuning<-function(data,timeframes,lag.max,information_criteria_lag="AIC",
                      information_criteria_frame="prediction.error",
                      prediction_error_step=1,multicore=TRUE,n_cores=1){
  
  
  ic<-match.fun(information_criteria_frame)
  
  if(multicore==TRUE){
    
    cluster1<-makeCluster(n_cores)
    print("Initiating cluster")
    
    invisible(clusterCall(cluster1,function(){
      source("dependencies.R")
    }))
    
    invisible(clusterExport(cluster1,list("windows","coda.data.preperation","data"),
                            envir = environment()))
    print("Starting Calculations")
    
    all_frames_mean<-bind_rows(parLapply(cluster1,timeframes,function(frame){
      
      data_time_windows<-windows(data,frame,method="overlapping",
                                 prediction_error_step = prediction_error_step)
      
      window_number<-length(data_time_windows)
      
      data_timeframe_mean<-apply(sapply(window_number,function(i){
        
        prediction_value<-data_time_windows[[i]]$prediction_value[,-1]
        fitting_data<-data_time_windows[[i]]$fitting[,-1]
        
        lag.max<-min(lag.max,floor((frame-1)/(ncol(fitting_data[,-1])+1))-1) # Note 1 
        coda_model<-VAR(fitting_data,lag.max=lag.max,ic=information_criteria_lag)
        
        p<-coda_model$p
        if(information_criteria_frame=="prediction.error"){
          
          information_criteria<-ic(coda_model,fitted_data=fitting_data,
                                   true_values=prediction_value,
                                   prediction_error_step=prediction_error_step,
                                   measure="Eucledian")
        }
        
        else {
          
          information_criteria<-ic(coda_model)
        }
        
        
        return(c(ic=information_criteria,p=p))
        
      }),1,mean)
      
      return(data.frame(ic=data_timeframe_mean["ic"],
                        frame=frame,
                        p=data_timeframe_mean[paste("p",".",information_criteria_lag,"(n)",sep="")])) 
    }))
    
    print("Stopping Calculations")
    print("Stopping Cluster")
    stopCluster(cluster1)
    
  }  
  
  else {
    
    print("Starting Calculations")
    all_frames_mean<-bind_rows(lapply(timeframes,function(frame){
      
      data_time_windows<-windows(data,frame,method="overlapping",
                                 prediction_error_step = prediction_error_step)
      
      window_number<-length(data_time_windows)
      
      data_timeframe_mean<-apply(sapply(window_number,function(i){
        
        prediction_value<-data_time_windows[[i]]$prediction_value[,-1]
        fitting_data<-data_time_windows[[i]]$fitting[,-1]
        
        lag.max<-min(lag.max,floor((frame-1)/(ncol(fitting_data[,-1])+1))-1) # Note 1
        coda_model<-VAR(fitting_data,lag.max=lag.max,ic=information_criteria_lag)
        
        p<-coda_model$p
        
        if(information_criteria_frame=="prediction.error"){
          
          information_criteria<-ic(coda_model,fitted_data=fitting_data,
                                   true_values=prediction_value,
                                   prediction_error_step=prediction_error_step,
                                   measure="Eucledian")
        }
        
        else {
          
          information_criteria<-ic(coda_model)
        }
        return(c(ic=information_criteria,p=p))
      }),1,mean)
      
     return(data.frame(ic=data_timeframe_mean["ic"],
                 frame=frame,
                 p=data_timeframe_mean[paste("p",".",information_criteria_lag,"(n)",sep="")])) 
    }))
    
    print("Stopping Calculations")
  }
  
  best_frame<-all_frames_mean[which.min(all_frames_mean$ic),]
  rownames(best_frame)<-""
  return(best_frame)
}




#Analysis function for coda data. The function tunes the model for the optimal frame and p on training data 
#and then calculates the prediction error on test data. With aggregation the results can be further analysed 
# for example with the mean, sd, median or no aggregation. 


#If no aggregation is wanted
none<-function(x)x

coda.analysis<-function(data_raw,ids,prediction_error_step=1,aggregation="mean"){
  
  aggregation<-match.fun(aggregation)
  
  model_results_id<-bind_rows(lapply(ids,function(id){
    
    data<-data_raw%>%
      filter(fridge_id==id)%>%
      dplyr::select(week_date,main_category_id,sold)
    
    
    mean_prediction_error_methods<-bind_rows(lapply(c("all","zeros_only"),function(type,tspace){
      
      data_prepared<-coda.data.preperation(data,zero_handling = type)
      data_length<-dim(data_prepared)[1]

      data_prepared_train<-data_prepared[c(1:floor(data_length/2)),]
      data_prepared_test<-data_prepared[c((floor(data_length/2)+1):data_length),]
      
      timeframes<-floor(c(0.1*data_length/2,0.5*data_length/2,0.9*data_length/2))
      
      optimal_parameters<-coda.tuning(data_prepared_train,timeframes = timeframes,
                                                lag.max = 6,information_criteria_lag = "AIC",
                                                information_criteria_frame = "prediction.error",
                                                prediction_error_step = 1,multicore = FALSE)
      
      
      indexes<-windows(data_prepared_test,optimal_parameters$frame,method = "overlapping",
                       prediction_error_step = prediction_error_step)
      
      
      prediction_errors<-aggregation(sapply(c(1:length(indexes)),function(index){

        fitting_data<-indexes[[index]]$fitting[,-1]
        prediction_value<-indexes[[index]]$prediction_value[,-1]
        
        model<-VAR(fitting_data,p=optimal_parameters$p)
        
        
        prediction_error<-prediction.error(model,fitted_data = fitted_data,
                                           true_values = prediction_value,
                                           prediction_error_step = prediction_error_step)
      
        return(prediction_error)
      }))
      
      return(data.frame(pred_error=prediction_errors,
                        method=rep(type,length(prediction_errors))))
      
    },tspace=FALSE))
    
   mean_prediction_error_methods$id<-as.factor(id)
   mean_prediction_error_methods$method<-as.factor(mean_prediction_error_methods$method)
  
   return(mean_prediction_error_methods)
  }))
  
  return(model_results_id)
}


#Plotting function for the result of coda.analysis. Right now only scatterplots and boxplots can be made. 


coda.plots<-function(analysis_results,plot.type="dots",save=TRUE,path=NULL){
  
  basis_plot<-ggplot(data=analysis_results,aes(x=id,y=pred_error,colour=method))
  
  if(plot.type=="dots"){
    
    final_plot<-basis_plot+
    geom_point(position=position_dodge(width = 0.1))
  }
  else if(plot.type=="boxplot"){
    final_plot<-basis_plot+
      geom_boxplot(position = "dodge")
  }
  
  else {
    stop("Enter valid plot type")
  }
  if(save){
    ggsave(filename = paste("coda",plot.type,"png",sep="."),
           path = path,plot=final_plot,
           width = 20,heigh=15,units="cm")
  }
  return(final_plot)
}

´
  
  

