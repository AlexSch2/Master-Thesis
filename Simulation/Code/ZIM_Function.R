#Data Preparation for Zim models
Zim.DataPreparation <- function(Data_Raw,
                                HistoryLength = 1,
                                Category = c(1,2,3,4),
                                TakeSubCategory = F){
  
  if(TakeSubCategory){
    Category <- sort(unique(Data_Raw$sub_category_id))
  }
  else{
    Category <- sort(unique(Data_Raw$main_category_id))
  }
  
  DataPrepared <- Data.Preparation(Data_Raw = Data_Raw,
                                    OneVsAll = F,
                                    Category = Category,
                                    NA_to = 0,
                                    HistoryLength = HistoryLength,
                                    TakeSubCategory = TakeSubCategory)
  DataPrepared_Length <- dim(DataPrepared)[1]
  
  #Merging the data with itself and an offset of 1
  DataPrepared <- cbind(DataPrepared[-1,],DataPrepared[-DataPrepared_Length,-1])
  
  Data_Names <- c("week_date",paste("y",Category,sep=""),paste("x",Category,sep=""))
  names(DataPrepared) <- Data_Names
  
  return(DataPrepared)
}

#Prediction Function for ZIM models

Zim.Prediction <- function(Data_Window,
                           Category,
                           PredictionStep = 1,
                           Frame = 10,
                           Plot = F,
                           Distribution = Distribution,
                           WindowMethod = WindowMethod) {
  
  
  NumberOfWindows <- length(Data_Window)
  
  
  #Calculating the prediction for each window
  Result <- lapply(c(1:NumberOfWindows),function(WindowIndex){
    
    
    #Extracting fitting values, true value and last known value
    Category_Future <- c(paste("y",Category,sep=""))
    Category_Past <- c(paste("x",Category,sep=""))
    Category_Both <- c(Category_Future,Category_Past)
    TimeSeriesValue_Window <- Data_Window[[WindowIndex]]$timeSeriesValue_window[c("week_date",Category_Both)]
    TimeseriesValue_Future <- Data_Window[[WindowIndex]]$timeSeriesValue_future[[Category_Future]]
    TimeSeriesValue_LastKnown <- tail(TimeSeriesValue_Window[[Category_Future]],n=1)
    names(TimeSeriesValue_LastKnown) <- Category_Past
    
    #Safe Catching
    SkipWindow <- FALSE
    Formula <- formula(paste(Category_Future,"~",Category_Past))
    
      Model <- tryCatch(
        expr = {zeroinfl(Formula, data=TimeSeriesValue_Window[Category_Both],dist = Distribution)},
        error = function (e) e
      )
      
      if (inherits(Model, "error")) {
        SkipWindow <- TRUE
        print("Skipping Window.")
      }

    
    if(SkipWindow)return(list(prediction=NA,model=NA))
    
    #Predicting the future value depending on PredictionStep
    PredictionResult <- predict(Model,newdata = data.frame(as.list(TimeSeriesValue_LastKnown)))
    
    
    #Rounding it since we only have integers
    ValuePredict <- round(PredictionResult)
    
    
    #Extracting the lower and upper prediction interval 
#    PredictionInterval_Lower <- PredictionResult$interval[1,"lower"]
#    PredictionInterval_Upper <- PredictionResult$interval[1,"upper"]
    
    
    #Calculating the prediction error
    PredictionError <- as.numeric(ValuePredict - TimeseriesValue_Future)
    
    
    #Calculating the normed prediction error
    PredictionError_Normed <- PredictionError
    
    return(list(
      prediction = data.frame(
        predictionError = PredictionError,
        valuePredict = ValuePredict,
        predictionError_normed = PredictionError_Normed,
        valueTrue = TimeseriesValue_Future,
        valueLastKnown = TimeSeriesValue_LastKnown,
        category = Category,
        date = Data_Window[[WindowIndex]]$timeSeriesValue_future[[1]],
        distribution = Distribution,
        window = WindowIndex,
        window_length = dim(TimeSeriesValue_Window)[1],
        window_baseLength = Frame
      ),
      model = Model
    ))
    
  })
  
  #Transforming result in a nicer format
  Result <- discard(Result, ~all(is.na(.x)))
  Result_Prediction <- bind_rows(UnlistListElement(Result, "prediction"))
  Result_Model <- UnlistListElement(Result, "model")
  names(Result_Model) <- sapply(c(1:NumberOfWindows),function(i){paste("window",i,sep = "")})
  
  
  #Calculation the normed prediction error 
  div <- sapply(c(1:dim(Result_Prediction)[1]),function(i){
    
    return(Normation(x = Result_Prediction$valueTrue[1:i],
                     y = Result_Prediction$valueLastKnown[1:i]))}
  )
  if(0 %in% div ) div[div == 0] <- 0.5
  Result_Prediction$predictionError_normed <- Result_Prediction$predictionError_normed/div
  
  
  #Plotting diagnostic plots or not
  #if (Plot) {
  #  plot(model, ask = F)
  #}
  
  return(list(result = Result_Prediction,
              model = Result_Model))
  
}




#Wrapper Function for Zim Analysis
Zim.Analysis <- function(Data_Raw,
                         Id,
                         PredictionStep = 1,
                         Distribution = "poisson",
                         ModelType = "zim",
                         Category_Main = c("1", "2", "3", "4"),
                         TakeSubCategory = F,
                         Category_Sub = NULL,
                         Frame = 10,
                         WindowMethod = "extending",
                         HistoryLength = 1,
                         Multicore = TRUE,
                         NCores = 2){
  
  
  stopifnot(ModelType %in% c("zim"))
  
  #Only return IDs with results
  Id_Result <- Id
  
  #Operating on Sub category Level
  SubCategory_Column  <- NULL
  if(TakeSubCategory){
    SubCategory_Column <- "sub_category_id"
    stopifnot(length(Category_Main)==1)
  }
  if(is.null(Category_Sub)){
    Category_Sub <- unique(Data_Raw$sub_category_id)
  }
  
  #Calculating Prediction results for all ids and each category
  PredictionResult_AllIDAllCategory <- lapply(Id,function(Id_RunVariable){
    print(paste("Calculating for ID:",Id_RunVariable))
    #Preparing data
    Data_Processed <- Data_Raw %>%
      filter(fridge_id == Id_RunVariable &
               main_category_id %in% as.integer(Category_Main) &
               sub_category_id %in% as.integer(Category_Sub)) %>%
      dplyr::select(week_date, main_category_id, any_of(SubCategory_Column) ,sold) %>%
      arrange(week_date)
    
    Data_Prepared <- Zim.DataPreparation(Data_Raw = Data_Processed,
                                             HistoryLength = HistoryLength,
                                             TakeSubCategory = TakeSubCategory)
    
    Category <- unique(gsub("\\D", "", names(Data_Prepared)[-1]))
    
    #Calculating the length of the timeseries
    TimeSeries_Length <- length(unique(Data_Prepared$week_date))
    
    #If the Frame is given as a fraction, calculate the absolute length. We set 5 as the minimum length needed.
    Frame_Help <- "fixed"
    if(dim(Data_Prepared)[1]<5){
      print(paste("Insufficient data. Skipping ID: ",Id_RunVariable))
      Id_Result <<- Id_Result[Id_RunVariable!=Id_Result]
      return(NA)
    }
    if(Frame < 1){
      Frame_Help <- as.character(Frame)
      Frame = round(Frame*dim(Data_Prepared)[1])
      if(Frame < 4){
        Frame = 4
      }
    }
    
    
    #Creating fitting and prediction windows
    Data_Window <- Data.Window(Data_Prepared,Frame = Frame,Method=WindowMethod,PredictionStep = PredictionStep)
    
    #Calculating Prediction results for each category 
    if(Multicore == TRUE){
      
      Cluster1 <- makeCluster(NCores)
      print("Initiating Cluster")
      
      invisible(clusterCall(Cluster1, function() {
        source("General_Dependency.R")
        source("General_Function.R")
      }))
      
      invisible(clusterExport(Cluster1,list("Data.Window","Zim.DataPreparation","Zim.Prediction","Data_Window"),
                              envir = environment()))
      print("Starting Calculations")
      PredictionResult_AllCategory <- parLapply(Cluster1, Category,function(Category_RunVariable){
        
        PredictionResult <- tryCatch(
          expr = { Zim.Prediction(Data_Window = Data_Window,
                                      Category = Category_RunVariable,
                                      PredictionStep = PredictionStep,
                                      Frame = Frame,
                                      Plot = F,
                                      Distribution = Distribution,
                                      WindowMethod = WindowMethod)},
          error = function (e) e
        )
        if(inherits(PredictionResult,"error")){
          print(paste("Error occured in prediction: ID",Id_RunVariable,", Category",Category_RunVariable,PredictionResult))
          return(NA)
        }
        
        return(list(result=bind_rows(PredictionResult$result),
                    model=PredictionResult$model))
        
      })
      print("Stopping Calculations")
      print("Stopping Cluster")
      stopCluster(Cluster1)
    } 
    else {
      PredictionResult_AllCategory <- lapply(Category,function(Category_RunVariable){
        
        PredictionResult <- tryCatch(
          expr = { Zim.Prediction(Data_Window = Data_Window,
                                      Category = Category_RunVariable,
                                      PredictionStep = PredictionStep,
                                      Frame = Frame,
                                      Plot = F,
                                      Distribution = Distribution,
                                      WindowMethod = WindowMethod)},
          error = function (e) e
        )
        if(inherits(PredictionResult,"error")){
          print(paste("Error occured in prediction: ID",Id_RunVariable,", Category",Category_RunVariable,PredictionResult))
          return(NA)
        }
        
        return(list(result = bind_rows(PredictionResult$result),
                    model = PredictionResult$model))
        
      })
    }
    
    #Transforming data in nicer format and removing NA values
    NA_Index <- which(is.na(PredictionResult_AllCategory)==TRUE)
    PredictionResult_AllCategory <- PredictionResult_AllCategory[!is.na(PredictionResult_AllCategory)]
    Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllCategory,"result"))
    Result_Model <- UnlistListElement(PredictionResult_AllCategory,"model")
    names(Result_Model) <- Category[-NA_Index]
    Result_Prediction$id <- Id_RunVariable
    Result_Prediction$windowMethod <- WindowMethod
    Result_Prediction$zeroHandling <- "none"
    Result_Prediction$frame <- Frame_Help
    Result_Prediction$history <- as.character(HistoryLength)
    Result_Prediction$timeseriesLength <- as.character(TimeSeries_Length)
    
    if(TakeSubCategory){
      Result_Prediction$main_category <- Category_Main
    }
    
    return(list(result = Result_Prediction,
                model = Result_Model))
  })
  
  #Removing NA (aka Timeseries which are too short)
  PredictionResult_AllIDAllCategory <- PredictionResult_AllIDAllCategory[!is.na(PredictionResult_AllIDAllCategory)]
  
  #Transforming data in nicer format
  Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllIDAllCategory,"result"))
  Result_Prediction$id <- as.factor(Result_Prediction$id)
  Result_Model <- UnlistListElement(PredictionResult_AllIDAllCategory,"model")
  names(Result_Model) <- Id_Result
  Result_Prediction$model <- ModelType
  
  return(list(result = Result_Prediction,
              model = Result_Model))
  
}
