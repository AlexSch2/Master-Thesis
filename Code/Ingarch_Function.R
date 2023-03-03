
#Data preparation for INGARCH models
Ingarch.DataPreparation <- function(Data_Raw, 
                                     OneVsAll = F, 
                                     PivotGroup = "1",
                                     ZeroHandling = c("none","zero_to_one")){
  
  ZeroHandling <- match.arg(ZeroHandling)
  
  Data_Raw <- Data.Preparation(Data_Raw = Data_Raw,
                               OneVsAll = OneVsAll,
                               PivotGroup = PivotGroup,
                               Category = c(1,2,3,4),
                               NA_to = 0)
  
  if (ZeroHandling == "none") {
    return(Data_Raw)
  }
  else if (ZeroHandling == "zero_to_one") {
    Data_Raw[Data_Raw == 0] <- 1
    return(Data_Raw)
  }
  else{
    stop("Enter valid zero handling method")
    }
}


#Prediction function for INGARCH models. Fits the model for the specified category and calculates the predicted value, 
#prediction errors and PREDICTIVE INTERVALS (NOT CIs)
Ingarch.Prediction <- function(Data_Window,
                               Category,
                               PredictionStep = 1,
                               Frame = 10,
                               Distribution = "poisson",
                               Plot = F,
                               WindowMethod = "extending",
                               External = FALSE,
                               PastOb = 1,
                               PastMean = 1){
  
  NumberOfWindows <- length(Data_Window)
  
  
  #Calculating the prediction for each window
  Result <- lapply(c(1:NumberOfWindows),function(WindowIndex){
    
    
    #Extracting fitting values, true value and last known value
    TimeSeriesValue_Window <- Data_Window[[WindowIndex]]$timeSeriesValue_window[c("week_date",Category)]
    TimeseriesValue_Future <- Data_Window[[WindowIndex]]$timeSeriesValue_future[[Category]]
    TimeSeriesValue_LastKnown <- tail(TimeSeriesValue_Window[[Category]],n=1)
    
    if(External){
      Xreg <- Data_Window[[WindowIndex]]$timeSeriesValue_window %>%
              dplyr::select(-c("week_date", all_of(Category))) %>%
              as.matrix()
    }
    else{
      Xreg <-NULL
    }
    
    #Fitting the model
   
    PastOb_Used <- c(1:PastOb)
    if (PastMean == 0) {
      PastMean_Used <- NULL
    } else{
      PastMean_Used <- c(1:PastMean)
    }
    
    Model <- tsglm(TimeSeriesValue_Window[[Category]],
                   model = list("past_obs" = PastOb_Used,
                                "past_mean" = PastMean_Used,
                                external = ncol(Xreg)),
                   xreg = Xreg,
                   distr = Distribution,
                   link = "identity")
    
    
    #Predicting the future value depending on PredictionStep
    PredictionResult <- predict(Model,n.ahead = PredictionStep,type = "shortest",level = 0.90)
    
    
    #Rounding it since we only have integers
    PredictedValue <- round(PredictionResult$pred)
    
    
    #Extracting the lower and upper prediction interval 
    PredictionInterval_Lower <- PredictionResult$interval[1,"lower"]
    PredictionInterval_Upper <- PredictionResult$interval[1,"upper"]
    
    
    #Calculating the prediction error
    PredictionError <- as.numeric(PredictedValue - TimeseriesValue_Future)
    
  
    #Calculating the normed prediction error
    PredictionError_Normed <- PredictionError
    
    return(list(
      prediction = data.frame(
        predictionError = PredictionError,
        predictedValue = PredictedValue,
        predictionError_normed = PredictionError_Normed,
        lower_bound = PredictionInterval_Lower,
        upper_bound = PredictionInterval_Upper,
        trueValue = TimeseriesValue_Future,
        lastKnownValue = TimeSeriesValue_LastKnown,
        category = Category,
        predictionDate = Data_Window[[WindowIndex]]$timeSeriesValue_future[[1]],
        distribution = Distribution,
        window = WindowIndex,
        window_length = dim(TimeSeriesValue_Window)[1],
        window_baseLength = Frame,
        pastOb = PastOb,
        pastMean = PastMean,
        external = External
      ),
      model = Model
    ))
    
  })
  
  #Transforming result in a nicer format
  ResultPrediction <- bind_rows(UnlistListElement(Result, "prediction"))
  ResultModel <- UnlistListElement(Result, "model")
  names(ResultModel) <- sapply(c(1:NumberOfWindows),function(i){paste("window",i,sep = "")})
  
  
  #Calculation the normed prediction error 
  div <- sapply(c(1:dim(ResultPrediction)[1]),function(i){
    
    return(Normation(x = ResultPrediction$trueValue[1:i],
                     y = ResultPrediction$lastKnownValue[1:i]))}
    )
  if(0 %in% div ) div[div == 0] <- 0.5
  ResultPrediction$predictionError_normed <- ResultPrediction$predictionError_normed/div
  
  
  #Plotting diagnostic plots or not
  #if (Plot) {
  #  plot(model, ask = F)
  #}
  
  return(list(result = ResultPrediction,
              model = ResultModel))
  
}



#Wrapper function for the analysis of the data with an Ingarch model
Ingarch.Analysis <- function(DataRaw,
                             Id,
                             PredictionStep = 1,
                             Distribution = "poisson",
                             ModelType = "ingarch",
                             Plot = F,
                             Category = c("1", "2", "3", "4"),
                             Frame = 10,
                             WindowMethod = "extending",
                             ZeroHandling = "none",
                             PastOb = 1,
                             PastMean = 1,
                             External = FALSE,
                             Multicore = TRUE,
                             NCores = 2
                             ) {
  
  
  stopifnot(ModelType %in% c("ingarch","ingarch_OneVsAll"))
  
  
  
  #Calculating Prediction results for all ids and each category
  PredictionResult_AllIDAllCategory <- lapply(Id,function(Id_RunVariable){
    
    #Preparing data
    DataPrepared <- DataRaw %>%
      filter(fridge_id == Id_RunVariable &
               main_category_id %in% as.integer(Category)) %>%
      dplyr::select(week_date, main_category_id, sold) %>%
      arrange(week_date) %>%
      Ingarch.DataPreparation(ZeroHandling = ZeroHandling)
    
    
    #Creating fitting and prediction windows
    Data_Window <- Data.Window(DataPrepared,Frame = Frame,Method=WindowMethod,PredictionStep = PredictionStep)
    
    
    #Calculating Prediction results for each category 
    if(Multicore == TRUE){
      
      Cluster1 <- makeCluster(NCores)
      print("Initiating cluster")
      
      invisible(clusterCall(Cluster1, function() {
        source("General_Dependency.R")
        source("General_Function.R")
      }))
      
      invisible(clusterExport(Cluster1,list("Data.Window","Ingarch.DataPreparation","Ingarch.Prediction","Data_Window",
                                            "External","PastOb","PastMean","Distribution","Plot"),
                              envir = environment()))
      print("Starting Calculations")
    PredictionResult_AllCategory <- parLapply(Cluster1, Category,function(Category_RunVariable){
      
      PredictionResult <- Ingarch.Prediction(Data_Window = Data_Window,
                                             Category = Category_RunVariable,
                                             PredictionStep = PredictionStep,
                                             Frame = Frame,
                                             Plot = F,
                                             Distribution = Distribution,
                                             WindowMethod = WindowMethod,
                                             External = External,
                                             PastOb = PastOb,
                                             PastMean = PastMean)
      
      return(list(result=bind_rows(PredictionResult$result),
                  model=PredictionResult$model))
      
    })
    print("Stopping Calculations")
    print("Stopping Cluster")
    stopCluster(Cluster1)
    } 
    else {
      PredictionResult_AllCategory <- lapply(Category,function(Category_RunVariable){
        
        PredictionResult <- Ingarch.Prediction(Data_Window = Data_Window,
                                               Category = Category_RunVariable,
                                               PredictionStep = PredictionStep,
                                               Frame = Frame,
                                               Plot = F,
                                               Distribution = Distribution,
                                               WindowMethod = WindowMethod,
                                               External = External,
                                               PastOb = PastOb,
                                               PastMean = PastMean)
        
        return(list(result = bind_rows(PredictionResult$result),
                    model = PredictionResult$model))
        
      })
    }
    
    #Transforming data in nicer format
    ResultPrediction <- bind_rows(UnlistListElement(PredictionResult_AllCategory,"result"))
    ResultModel <- UnlistListElement(PredictionResult_AllCategory,"model")
    names(ResultModel) <- Category
    ResultPrediction$id <- Id_RunVariable
    ResultPrediction$windowMethod <- WindowMethod
    ResultPrediction$zeroHandling <- ZeroHandling
    
    return(list(result = ResultPrediction,
                model = ResultModel))
  })

  
  
  #Transforming data in nicer format
  ResultPrediction <- bind_rows(UnlistListElement(PredictionResult_AllIDAllCategory,"result"))
  ResultModel <- UnlistListElement(PredictionResult_AllIDAllCategory,"model")
  names(ResultModel) <- Id
  ResultPrediction$model <- ModelType
  
  return(list(result = ResultPrediction,
              model = ResultModel))
  
 
  
}



## Plotting a specified model result
Ingarch.ParameterPlot <- function(Ingarch_Result, Category, Element, Fnct = "idf", Save = TRUE, Plot_Type =
                                    c("default", "histogram")){
  
  
  Plot_Type <- match.arg(Plot_Type)
  Id_all <- unique(Ingarch_Result$result$id)
  Distribution_all <- unique(Ingarch_Result$result$distribution)
  
  
  
  for (Id_RunVariable in Id_all){
    for (Distribution_RunVariable in Distribution_All){
      
      if(Save){
        png(paste("Ingarch_plot_id", Id_RunVariable, Distribution_RunVariable, "category", Category, Plot_Type, Fnct, ".png", sep = ""), 
            height = 15,width = 20,units = "cm",res = 300)
      }
      
      Data_Plot <- Ingarch_Result$models[[paste(Id_RunVariable)]][[Category]]
      Data_Plot <- unlist(lapply(c(1:length(Data_Plot)),function(i)return(do.call(Fnct,list(Data_Plot[[i]][[Element]])))))
      
      if(Plot_Type == "default"){
        
        plot(Data_Plot,type = "l",ylab = paste("Distribution mean",Fnct,sep = " "),
             main = paste("Ingarch_", Distribution_RunVariable, "_id_", Id_RunVariable, sep = ""),
             sub = paste("Category: ", Category, sep = ""))
        
      }
      
      else if (Plot_Type =="histogram") {
        
        hist(Data_Plot,
             xlab = paste("Distribution mean", Fnct, sep = " "), 
             main = paste("Ingarch_", Distribution_RunVariable, "_id_", Id_RunVariable, sep = ""),
             sub = paste("Category: ", Category, sep = ""))
        
      }else {
        
        stop("Enter valid plot type") 
        
      }

    }
    
  }
  if(Save){
    dev.off()
  }
  
}






