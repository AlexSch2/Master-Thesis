
#Data preparation for count models
CountModel.DataPreparation <- function(Data,
                                       ZeroHandling = c("none", "zero_to_one"),
                                       HistoryLength = 1,
                                       TakeSubCategory = F,
                                       Category){
  
  ZeroHandling <- match.arg(ZeroHandling)

  Data_Prepared <- Data.Preparation(Data_Raw = Data,
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


#Prediction function for count models. Fits the model for the specified category and calculates the predicted value, 
#prediction errors and PREDICTIVE INTERVALS (NOT CIs)
CountModel.Prediction <- function(Data_Window,
                               Data_WindowNoTransform,
                               Category,
                               PredictionStep = 1,
                               Frame = 10,
                               Distribution = "poisson",
                               Plot = F,
                               WindowMethod = "extending",
                               External = FALSE,
                               PastOb = NA,
                               PastMean = NA,
                               ModelType = "ingarch"){
  
  NumberOfWindows <- length(Data_Window)
  
  
  #Calculating the prediction for each window
  Result <- lapply(c(1:NumberOfWindows),function(WindowIndex){
    
    
    #Extracting fitting values, true value and last known value
    TimeSeriesValue_Window <- as.data.frame(Data_Window[[WindowIndex]]$timeSeriesValue_window[c("week_date",Category)])
    TimeseriesValue_Future <- Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_future[[Category]]
    TimeSeriesValue_LastKnown <- tail(TimeSeriesValue_Window[[Category]],n=1)
    
    ##Using an INGARCH model
    if(ModelType == "ingarch"){
      
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
                                  level = 0.95,newxreg = XregFuture)
      
      ValuePredict <- PredictionResult$pred
      
      #Extracting the lower and upper prediction interval 
      PredictionInterval_Lower <- PredictionResult$interval[1,"lower"]
      PredictionInterval_Upper <- PredictionResult$interval[1,"upper"]
      
    }
    
    else if(ModelType =="inar_classic"){
      
      #Fitting the INAR(1) Model with Poisson Distribution and Predicting the 1-step Ahead prediction with the classical method
      Model <- EST_ZINAR(TimeSeriesValue_Window[[Category]],model="inar",innovation = Distribution)
      
      Alpha <- Model$parameters[1]
      Lambda <- Model$parameters[2]
      
      ValuePredict <- Alpha^PredictionStep * (TimeSeriesValue_LastKnown-(Lambda)/(1-Alpha))+Lambda/(1-Alpha)
      
      Distribution <- switch(Distribution,
                             Po = "Poisson",
                             NB = "Negative_binomial",
                             GI = "Gaussian_inverse")
      
      PredictionInterval_Lower <- NA
      PredictionInterval_Upper <- NA
      initMethod <- NA
    }
    
    else if(ModelType =="inar_bayes"){
      
      Model <- estimate_zinarp(TimeSeriesValue_Window[[Category]],p=1,innovation = Distribution,iter = 500)
      
      Alpha <- mean(Model$alpha)
      Lambda <- mean(Model$lambda)
      
      ValuePredict <- Alpha^PredictionStep * (TimeSeriesValue_LastKnown-(Lambda)/(1-Alpha))+Lambda/(1-Alpha)
      
      PredictionInterval_Lower <- NA
      PredictionInterval_Upper <- NA
      initMethod <- NA
    } 
    
    else if(ModelType =="zim") {
      
      Formula <- formula(paste("y",Category,"~","x",Category,"|1",sep=""))
      Data <- data.frame(y=TimeSeriesValue_Window[[Category]],
                         x=bshift(TimeSeriesValue_Window[[Category]]))
      names(Data) <- c(paste("y",Category,sep=""),paste("x",Category,sep=""))
      
      Model <- tryCatch(
        expr = {zeroinfl( Formula,data = Data ,dist = Distribution)},
        error = function (e) e
      )
      
      if (inherits(Model, "error")) {
        SkipWindow <- TRUE
        print("Skipping Window.")
      }
      
      names(TimeSeriesValue_LastKnown) <- paste("x",as.character(Category),sep="")
      ValuePredict <- predict(Model,newdata = data.frame(as.list(TimeSeriesValue_LastKnown)))
      
      PredictionInterval_Lower <- NA
      PredictionInterval_Upper <- NA
      initMethod <- NA  
      
    }    
    else {
      stop("Choose valid Model")
    }

    #Rounding Result
    ValuePredict <- round (ValuePredict)
    
    #Calculating the prediction error
    PredictionError <- as.numeric(ValuePredict - TimeseriesValue_Future)
    
  
    #Calculating the normed prediction error
    PredictionError_Normed <- PredictionError
    
    #Renaming the Distribution for coherent format
    Distribution <- str_to_title(Distribution)
    
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



#Wrapper function for the analysis of the data with an count model
CountModel.Analysis <- function(Data_Raw,
                             Id,
                             PredictionStep = 1,
                             Distribution = "poisson",
                             ModelType = c("ingarch","inar_classic","inar_bayes","zim"),
                             Category_Main = c("1", "2", "3", "4"),
                             TakeSubCategory = F,
                             Category_Sub = NULL,
                             Frame = 10,
                             WindowMethod = c("extending","fixed"),
                             ZeroHandling = c("none", "zero_to_one"),
                             PastOb = 1,
                             PastMean = 1,
                             External = FALSE,
                             HistoryLength = 1,
                             Multicore = TRUE,
                             NCores = 2
                             ) {
  
  #Checking Input
  stopifnot(ModelType %in% c("ingarch","inar_classic","inar_bayes","zim"))
  stopifnot("External,Multicore and TakeSubCategory must be boolean."=
              all(sum(sapply(c(External,Multicore,TakeSubCategory),is.logical))))
  stopifnot("ZeroHandling,WindowMethod,Distribution and Category_Main must be character vectors."=
              all(sum(sapply(c(ZeroHandling,Distribution,WindowMethod,Category_Main),is.character))))
  stopifnot("Id,PredictionStep,HistoryLength,NCores and Frame must be numeric."= 
              all(sum(sapply(c(Id,PredictionStep,HistoryLength,Frame,NCores),is.numeric))))
  
  switch(ModelType,
         "ingarch" = stopifnot("Distribution is not valid for chosen model. 
                               Check help page for tsglm() for valid choices." = Distribution %in% c("poisson","nbinom")),
         "inar_classic" = stopifnot("Distribution is not valid for chosen model. 
                                    Check help page for EST_ZINAR() for valid choices." = Distribution %in% c("Po","NB","GI")),
         "inar_bayes" = stopifnot("Distribution is not valid for chosen model. 
                                  Check help page for estimate_zinarp() for valid choices." = Distribution %in% c("Poisson","ZIP")),
         "zim" = stopifnot("Distribution is not valid for chosen model. 
                           Check help page for zeroinfl() for valid choices." = Distribution %in% c("poisson","negbin","geometric")))
  
  
  #Only return IDs with results
  Id_Result <- Id
  
  #Calculating Prediction results for all ids and each category
  PredictionResult_AllIDAllCategory <- lapply(Id,function(Id_RunVariable){
    print(paste("Calculating for ID:",Id_RunVariable))
    
    #Preparing raw data
    Data_Processed <- Data_Raw %>%
      filter(fridge_id == Id_RunVariable &
               main_category_id %in% as.integer(Category_Main))
    
    #Safe Catching if Category Main was wrongly specified
    if(dim(Data_Processed)[1]==0){
      stop("There was an error filtering Data_Raw with respect to Id and Category_Main. Check those inputs and 
            make sure they exist in the provided data.")
    }
    
    #If no Subcategory is specified, we take all
    if(!is.null(Category_Sub)){
      #Taking Subcategories      
      stopifnot("Subcategory must be part of main category."=Category_Sub %in% unique(Data_Processed$sub_category_id))
      Data_Processed <- Data_Processed %>% 
        filter(sub_category_id %in% as.integer(Category_Sub))
    }
    
    #Operating on Sub category Level
    if(TakeSubCategory){
      stopifnot("Only one main category can be chosen"=length(Category_Main)==1)
      Data_Processed <- Data_Processed %>%
        dplyr::select(week_date, main_category_id, sub_category_id ,sold) %>%
        arrange(week_date)
      
      Category <- sort(unique(Data_Processed$sub_category_id))
      
    } else {
      Data_Processed <- Data_Processed %>%
        dplyr::select(week_date, main_category_id,sold) %>%
        arrange(week_date)
      
      Category <- sort(unique(Data_Processed$main_category_id))
    }
    
    Data_Prepared <- CountModel.DataPreparation(Data = Data_Processed,
                                                ZeroHandling = ZeroHandling,
                                                HistoryLength = HistoryLength,
                                                TakeSubCategory = TakeSubCategory,
                                                Category = Category)
    
    
    Data_PreparedNoTransform <- CountModel.DataPreparation(Data = Data_Processed,
                                                           ZeroHandling = "none",
                                                           HistoryLength = HistoryLength,
                                                           TakeSubCategory = TakeSubCategory,
                                                           Category = Category)
    
    
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
      if(Frame < 5){
        Frame = 5
      }
    }
    
    #Creating fitting and prediction windows
    Data_Window <- Data.Window(Data_Prepared,
                               Frame = Frame,
                               Method=WindowMethod,
                               PredictionStep = PredictionStep)
    
    Data_WindowNoTransform <- Data.Window( Data_PreparedNoTransform,
                                           Frame = Frame,
                                           Method=WindowMethod,
                                           PredictionStep = PredictionStep)
  
    #Calculating Prediction results for each category 
    if(Multicore == TRUE){
      
      Cluster1 <- makeCluster(NCores)
      print("Initiating Cluster")
      invisible(clusterCall(Cluster1, function() {
          source("General_Dependency.R")
          source("General_Function.R")
      }))

      
      invisible(clusterExport(Cluster1,list("CountModel.DataPreparation","CountModel.Prediction","Data_Window",
                                            "Data_WindowNoTransform","External","PastOb","PastMean",
                                            "Distribution","WindowMethod","PredictionStep","ModelType"),
                              envir = environment()))
      print("Starting Calculations")
      
    PredictionResult_AllCategory <- parLapply(Cluster1, Category,function(Category_RunVariable){
      print("Calculating")
      PredictionResult <- tryCatch(
        expr = { CountModel.Prediction(Data_Window = Data_Window,
                                    Data_WindowNoTransform = Data_WindowNoTransform,
                                    Category = Category_RunVariable,
                                    PredictionStep = PredictionStep,
                                    Frame = Frame,
                                    Plot = F,
                                    Distribution = Distribution,
                                    WindowMethod = WindowMethod,
                                    External = External,
                                    PastOb = PastOb,
                                    PastMean = PastMean,
                                    ModelType = ModelType)},
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
          expr = { CountModel.Prediction(Data_Window = Data_Window,
                                      Data_WindowNoTransform = Data_WindowNoTransform,
                                      Category = Category_RunVariable,
                                      PredictionStep = PredictionStep,
                                      Frame = Frame,
                                      Plot = F,
                                      Distribution = Distribution,
                                      WindowMethod = WindowMethod,
                                      External = External,
                                      PastOb = PastOb,
                                      PastMean = PastMean,
                                      ModelType = ModelType)},
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
    
    if(is_empty(ModelNames)){
      return(NA)
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
  
  if(is_empty(PredictionResult_AllIDAllCategory)){
    print("No Results could be calculated for the given IDs")
    return(NA)
  }
  
  #Transforming data in nicer format
  Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllIDAllCategory,"result"))
  Result_Prediction$id <- as.factor(Result_Prediction$id)
  Result_Model <- UnlistListElement(PredictionResult_AllIDAllCategory,"model")
  names(Result_Model) <- unique(as.character(Result_Prediction$id))
  Result_Prediction$model <- ModelType
  
  return(list(result = Result_Prediction,
              model = Result_Model))
}







