
#Data preparation for INGARCH models
Ingarch.DataPreparation <- function(Data_Raw,
                                     ZeroHandling = c("none","zero_to_one"),
                                    HistoryLength = 1,
                                    TakeSubCategory = F){
  
  ZeroHandling <- match.arg(ZeroHandling)
  
  if(TakeSubCategory){
    Category <- sort(unique(Data_Raw$sub_category_id))
  }
  else{
    Category <- sort(unique(Data_Raw$main_category_id))
  }
  
  Data_Prepared <- Data.Preparation(Data_Raw = Data_Raw,
                               OneVsAll = F,
                               Category = Category,
                               NA_to = 0,
                               HistoryLength = HistoryLength,
                               TakeSubCategory = TakeSubCategory)
  
  if (ZeroHandling == "none") {
    return(Data_Prepared)
  }
  else if (ZeroHandling == "zero_to_one") {
    Data_Prepared[Data_Prepared == 0] <- 1
    return(Data_Prepared)
  }
  else{
    stop("Enter valid zero handling method")
    }
}


#Prediction function for INGARCH models. Fits the model for the specified category and calculates the predicted value, 
#prediction errors and PREDICTIVE INTERVALS (NOT CIs)
Ingarch.Prediction <- function(Data_Window,
                               Data_WindowNoTransform,
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
    TimeseriesValue_Future <- Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_future[[Category]]
    TimeSeriesValue_LastKnown <- tail(TimeSeriesValue_Window[[Category]],n=1)
    
    Xreg <-NULL
    Ext <- NULL
    XregFuture <- NULL
    
    if(External){
      Xreg <- Data_Window[[WindowIndex]]$timeSeriesValue_window %>%
              dplyr::select(-c("week_date", all_of(Category))) %>%
              as.matrix()
      Ext <- rep(TRUE,ncol(Xreg))
      XregFuture <- matrix(round(apply(tail(Xreg,n=5),2,mean)),nrow=1)
      names(XregFuture) <- colnames(Xreg)
    }
    
    #Fitting the model
   
    PastOb_Used <- c(1:PastOb)
    if (PastMean == 0) {
      PastMean_Used <- NULL
    } else{
      PastMean_Used <- c(1:PastMean)
    }
    
    #Determining init.method. If none works, we skip this fridge 
    SkipWindow <- FALSE
    
    for (method in c("marginal", "iid", "firstobs", "zero")) {
      
      Model <- tryCatch(
        expr = {tsglm(TimeSeriesValue_Window[[Category]],
                      model = list("past_obs" = PastOb_Used,
                                   "past_mean" = PastMean_Used,
                                   external = Ext),
                      xreg = Xreg,
                      distr = Distribution,
                      link = "identity",
                      init.method = method)},
        error = function (e) e
      )
      
    if(inherits(Model,"error")){
      if(method == "zero") {
        SkipWindow <- TRUE
        print("No init.method works. Skipping Window.")
        break
      }
      next
    }else{
      initMethod <- method
      break 
    }
    }
    
    if(SkipWindow)return(list(prediction=NA,model=NA))
    
    #Predicting the future value depending on PredictionStep
    PredictionResult <- predict(Model,n.ahead = PredictionStep,type = "shortest",
                                level = 0.90,newxreg = XregFuture)
    
    
    #Rounding it since we only have integers
    ValuePredict <- round(PredictionResult$pred)
    
    
    #Extracting the lower and upper prediction interval 
    PredictionInterval_Lower <- PredictionResult$interval[1,"lower"]
    PredictionInterval_Upper <- PredictionResult$interval[1,"upper"]
    
    
    #Calculating the prediction error
    PredictionError <- as.numeric(ValuePredict - TimeseriesValue_Future)
    
  
    #Calculating the normed prediction error
    PredictionError_Normed <- PredictionError
    
    return(list(
      prediction = data.frame(
        predictionError = PredictionError,
        valuePredict = ValuePredict,
        predictionError_normed = PredictionError_Normed,
        lowerBound = PredictionInterval_Lower,
        upperBound = PredictionInterval_Upper,
        valueTrue = TimeseriesValue_Future,
        valueLastKnown = TimeSeriesValue_LastKnown,
        category = Category,
        date = Data_Window[[WindowIndex]]$timeSeriesValue_future[[1]],
        distribution = Distribution,
        window = WindowIndex,
        window_length = dim(TimeSeriesValue_Window)[1],
        window_baseLength = Frame,
        pastOb = PastOb,
        pastMean = PastMean,
        external = External,
        initMethod = initMethod
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



#Wrapper function for the analysis of the data with an Ingarch model
Ingarch.Analysis <- function(Data_Raw,
                             Id,
                             PredictionStep = 1,
                             Distribution = "poisson",
                             ModelType = "ingarch",
                             Plot = F,
                             Category_Main = c("1", "2", "3", "4"),
                             TakeSubCategory = F,
                             Category_Sub = NULL,
                             Frame = 10,
                             WindowMethod = "extending",
                             ZeroHandling = "none",
                             PastOb = 1,
                             PastMean = 1,
                             External = FALSE,
                             HistoryLength = 1,
                             Multicore = TRUE,
                             NCores = 2
                             ) {
  
  stopifnot(ModelType %in% c("ingarch","ingarch_OneVsAll"))
  
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
    
    Data_Prepared <- Ingarch.DataPreparation(Data_Raw = Data_Processed,ZeroHandling = ZeroHandling,
                              HistoryLength = HistoryLength,
                              TakeSubCategory = TakeSubCategory)
    
    Data_PreparedNoTransform <- Ingarch.DataPreparation(Data_Raw = Data_Processed,ZeroHandling = "none",
                                                        HistoryLength = HistoryLength,
                                                        TakeSubCategory = TakeSubCategory)
    Category <- names(Data_Prepared)[-1]
    
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
    Data_WindowNoTransform <- Data.Window( Data_PreparedNoTransform,Frame = Frame,Method=WindowMethod,PredictionStep = PredictionStep)
  
    #Calculating Prediction results for each category 
    if(Multicore == TRUE){
      
      Cluster1 <- makeCluster(NCores)
      print("Initiating Cluster")
      
      invisible(clusterCall(Cluster1, function() {
        source("General_Dependency.R")
        source("General_Function.R")
      }))
      
      invisible(clusterExport(Cluster1,list("Data.Window","Ingarch.DataPreparation","Ingarch.Prediction","Data_Window",
                                            "External","PastOb","PastMean","Distribution","Plot"),
                              envir = environment()))
      print("Starting Calculations")
    PredictionResult_AllCategory <- parLapply(Cluster1, Category,function(Category_RunVariable){
      
      PredictionResult <- tryCatch(
        expr = { Ingarch.Prediction(Data_Window = Data_Window,
                                    Data_WindowNoTransform = Data_WindowNoTransform,
                                    Category = Category_RunVariable,
                                    PredictionStep = PredictionStep,
                                    Frame = Frame,
                                    Plot = F,
                                    Distribution = Distribution,
                                    WindowMethod = WindowMethod,
                                    External = External,
                                    PastOb = PastOb,
                                    PastMean = PastMean)},
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
          expr = { Ingarch.Prediction(Data_Window = Data_Window,
                                      Data_WindowNoTransform = Data_WindowNoTransform,
                                      Category = Category_RunVariable,
                                      PredictionStep = PredictionStep,
                                      Frame = Frame,
                                      Plot = F,
                                      Distribution = Distribution,
                                      WindowMethod = WindowMethod,
                                      External = External,
                                      PastOb = PastOb,
                                      PastMean = PastMean)},
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
    if(is_empty(NA_Index)){
      ModelNames <- Category
    }else{
      ModelNames <- Category[-NA_Index]
    }
    
    PredictionResult_AllCategory <- PredictionResult_AllCategory[!is.na(PredictionResult_AllCategory)]
    Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllCategory,"result"))
    Result_Model <- UnlistListElement(PredictionResult_AllCategory,"model")
    names(Result_Model) <- ModelNames
    Result_Prediction$id <- Id_RunVariable
    Result_Prediction$windowMethod <- WindowMethod
    Result_Prediction$zeroHandling <- ZeroHandling
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







