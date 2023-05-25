#Helper functions for the CODA model




#Prepares the data for the VAR model (pivoting to wide format, handling the zeros and transforming to ilr coordinates)
#For now written with dplyr, may be optimised later
#Currently aggregating on the main categories
#If one vs all is chosen, pivot groups have to be supplied as a character

Coda.DataPreparation <- function(Data,
           ZeroHandling = c("all", "zeros_only", "none"),
           TSpace = FALSE,
           Log = FALSE,
           OneVsAll = FALSE,
           PivotGroup = "1",
           HistoryLength = 1,
           TakeSubCategory = F,
           Category) {

    Plus <- function(x,Value=0.5){
         x[is.na(x)] <- 0
      x <- x + Value
      return(x)
    }
    ZeroHandling <- match.arg(ZeroHandling)

    Data_Prepared <- Data.Preparation(Data_Raw = Data,
                             OneVsAll = OneVsAll,
                             PivotGroup = PivotGroup,
                             NA_to=0,
                             Category = Category,
                             HistoryLength = HistoryLength,
                             TakeSubCategory = TakeSubCategory)

    
    if (ZeroHandling == "none") {
      Data_Prepared$tsum <- rowSums(Data_Prepared[,-1])
      return(Data_Prepared)
    }
    
    if (ZeroHandling == "all") {
      Data_Prepared <- Data_Prepared %>% mutate(across(where(is.numeric),.fns = Plus))
    }
    else if (ZeroHandling == "zeros_only") {
      Data_Prepared[Data_Prepared == 0] <- 0.5
    }
    else {
      stop("Enter valid zero handling option")
    }
    
    Data_Ilr <- cbind(Data_Prepared$week_date, pivotCoord(as.data.frame(Data_Prepared[,-1])))
    
    if (TSpace) {
      if (Log) {
        Data_Ilr$tsum <- log(rowSums(Data_Prepared[,-1]))
      }
      else {
        Data_Ilr$tsum <- rowSums(Data_Prepared[,-1])
      }
    }
    
    names(Data_Ilr)[1] <- "week_date"
    
    return(Data_Ilr)
  }

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
#UPDATE: lag.max is set to 1 to ensure stability
#
#NOTE 2:
#
#To ensure stability we also implement the restriction to the max lag to hold n > 10*max(p,m) where 
# n...frame
# p...lag.max*2 (since we have p summands each with dimension 2)
# m... 2
# See Multivariate Linear Regression pdf
Coda.Prediction <- function(Data_Window, Data_WindowNoTransform, Data_NoTransform, PredictionStep,
                            OneVsAll,TSpace, Log, PivotGroup, Frame = 10, Category) {
  
  PredictionResult <- lapply(c(1:length(Data_Window)), function(WindowIndex) {
    
    
    # Selecting the fitting data and the data which should be predicted 
    TimeSeriesValue_Window <- as.data.frame(Data_Window[[WindowIndex]]$timeSeriesValue_window[,-1])
    Date <- Data_Window[[WindowIndex]]$timeSeriesValue_future[1, 1]
    
    Size <- dim(TimeSeriesValue_Window)[2]
    
    #If we have multivariate data, we fit a VAR model. Otherwise we fit an AR model
    ####TSPACE
    if (Size > 1) {
      Window_Length <- dim(TimeSeriesValue_Window)[1]
      
      Model <- VAR(TimeSeriesValue_Window, p=1 ,lag.max = NULL, ic= "AIC")
      ValuePredict <-  predict(Model, TimeSeriesValue_Window, n.ahead = PredictionStep)
      
      #Initialising result vectors
      ValuePredict_Vector <-matrix(data = NA,nrow = 1,ncol = Size)
      LowerBound_Vector<- UpperBound_Vector <- vector(mode="numeric",length=Size)
      
      #Filling up result vectors
      for (i in 1:Size) {
        ValuePredict_Vector[1, i] <-  ValuePredict[[1]][[i]][PredictionStep]
        LowerBound_Vector[i] <-  ValuePredict[[1]][[i]][PredictionStep+1]
        UpperBound_Vector[i] <-  ValuePredict[[1]][[i]][PredictionStep+2]
      }
      
      
      #Back transformations
      ValuePredict <- ValuePredict_Vector[-Size] %>%
        matrix(nrow = 1) %>%
        D2invPC()
      
      LowerBound <- LowerBound_Vector[-Size] %>%
        matrix(nrow = 1) %>%
        D2invPC()
      
      UpperBound <- UpperBound_Vector[-Size] %>%
        matrix(nrow = 1) %>%
        D2invPC()
      
      
      # Back transformations when we use the log of the total sum
      if (Log) {
        ValuePredict <- ValuePredict * exp(ValuePredict_Vector[Size])
        ValuePredict <-
          (append(ValuePredict, exp(ValuePredict_Vector[Size])))
        
        LowerBound <- LowerBound * exp(ValuePredict_Vector[Size])
        LowerBound <-
          (append(LowerBound, exp(ValuePredict_Vector[Size])))
        
        UpperBound <- UpperBound * exp(ValuePredict_Vector[Size])
        UpperBound <-
          (append(UpperBound, exp(ValuePredict_Vector[Size])))
        
      }
      # Normal back transformations
      else{
        ValuePredict <- ValuePredict * ValuePredict_Vector[Size]
        ValuePredict <-
          (append(ValuePredict, ValuePredict_Vector[Size]))
        
        LowerBound <- LowerBound * ValuePredict_Vector[Size]
        LowerBound <-
          (append(LowerBound, ValuePredict_Vector[Size]))
        
        UpperBound <- UpperBound * ValuePredict_Vector[Size]
        UpperBound <-
          (append(UpperBound, ValuePredict_Vector[Size]))
      }
      
      #Getting true value and last known values
      Window_Length <-
        dim(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_window)[1]
      ValueTrue <-
        as.numeric(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_future[, -1])
      ValueLastKnown <-
        as.numeric(tail(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_window[, -1], n =
                          1))
      
      ValuePredict_Naive <- ValueLastKnown
      
      
      if (OneVsAll) {
        Category <- factor(c(PivotGroup, "other", "tsum"))
      }
      else{
        Category <- factor(c(as.character(Category), "tsum"))
      }
      
    }
    ### We have a univariate time series.
    else{
      Window_Length <- length(TimeSeriesValue_Window)
      
      Model <- ar(TimeSeriesValue_Window, aic = F, order.max = 1)
      ValuePredict <- predict(Model, as.matrix(TimeSeriesValue_Window), n.ahead = PredictionStep)
      
      #Initialising result vectors
      ValuePredict_Vector <-matrix(data = NA,nrow = 1,ncol = Size)
      LowerBound_Vector<- UpperBound_Vector <- vector(mode="numeric",length=Size)
      
      #Filling up result vectors
      for (i in 1:Size) {
        ValuePredict_Vector[1, i] <-  ValuePredict[[1]][[i]][PredictionStep]
       # LowerBound_Vector[i] <-  ValuePredict[[1]][[i]][PredictionStep+1]
       # UpperBound_Vector[i] <-  ValuePredict[[1]][[i]][PredictionStep+2]
      }
      
      
      TSum <- as.numeric(tail(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_window$tsum, 1))
      
      #Back transformations
      
      ValuePredict <- ValuePredict_Vector %>%
        matrix(nrow = 1) %>%
        D2invPC()
      ValuePredict <- ValuePredict * TSum
      
      LowerBound <- NA
      UpperBound <- NA
      
      # LowerBound <- LowerBound_Vector %>%
      #   matrix(nrow = 1) %>%
      #   D2invPC()
      # LowerBound <- as.vector(LowerBound * TSum)
      # 
      # 
      # UpperBound <- UpperBound_Vector %>%
      #   matrix(nrow = 1) %>%
      #   D2invPC()
      # UpperBound <- as.vector(UpperBound * TSum)
      
      
      ValueTrue <-
        as.numeric(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_future[, -c(1, 4)])
      ValueLastKnown <-
        as.numeric(tail(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_window[, -c(1, 4)], n =
                          1))
      
      if (OneVsAll) {
        Category <- factor(c(PivotGroup, "other"))
      }
      else{
        Category <- factor(c(as.character(Category)))
      }
      
      ValuePredict_Naive <-
        as.numeric(tail(Data_WindowNoTransform[[WindowIndex]]$timeSeriesValue_window[, -c(1, 4)], 1))
    }
    
    ValuePredict <- round(as.numeric(ValuePredict))
    PredictionError <- as.numeric(ValuePredict - ValueTrue)
    PredictionError_Naive <- as.numeric(ValuePredict_Naive - ValueTrue)
    
    #Calculating the normed prediction error
    PredictionError_Normed <- PredictionError
    
    
    if(OneVsAll){
      
      return(
        list(
          prediction = data.frame(
            predictionError = PredictionError,
            predictionError_naive = PredictionError_Naive,
            predictionError_normed = PredictionError_Normed,
            valuePredict = ValuePredict,
            lowerBound = LowerBound,
            upperBound = UpperBound,
            valueTrue = ValueTrue,
            valueLastKnown = ValueLastKnown,
            valuePredict_naive = ValuePredict_Naive,
            category = Category,
            date = Date,
            pivotGroup = PivotGroup,
            window = WindowIndex,
            window_length = Window_Length,
            window_baseLength = Frame,
            tSpace = TSpace
          ),
          model = Model
        )
      )
    } 
    else {
      return(
        list(
          prediction = data.frame(
            predictionError = PredictionError,
            predictionError_naive = PredictionError_Naive,
            predictionError_normed = PredictionError_Normed,
            valuePredict = ValuePredict,
            lowerBound = LowerBound,
            upperBound = UpperBound,
            valueTrue = ValueTrue,
            valueLastKnown = ValueLastKnown,
            valuePredict_naive = ValuePredict_Naive,
            category = Category,
            date = Date,
            window = WindowIndex,
            window_length = Window_Length,
            window_baseLength = Frame,
            tSpace = TSpace
        ),
        model = Model
        )
      )
    }
    })
  
  
  Result_Prediction <- bind_rows(UnlistListElement(PredictionResult,"prediction"))
  Result_Model <- UnlistListElement(PredictionResult,"model")
  names(Result_Model) <- sapply(c(1:length(Result_Model)),function(i){paste("window",i,sep="")})
  
  #Calculation the normed prediction error
  for (catg in unique(Result_Prediction$category)) {
    x <- Result_Prediction %>% filter(category==catg)
    
    div <- sapply(c(1:dim(x)[1]), function(i) {
      return(Normation(
        x = x$valueTrue[1:i],
        y = x$valueLastKnown[1:i]
      ))
    })
    if (0 %in% div)
      div[div == 0] <- 0.5
    Result_Prediction[Result_Prediction$category == catg, "predictionError_normed"] <-
      Result_Prediction[Result_Prediction$category == catg, "predictionError_normed"] / div
    
    
  }
  
  
  return(list(result = Result_Prediction,
              model= Result_Model))
}



## Wrapper function for the coda analysis. Still WIP
## Standard CI is 95%

Coda.Analysis<-function(Data_Raw, 
                        Id, 
                        Frame=10, 
                        ZeroHandling = "zeros_only", 
                        PredictionStep = 1, 
                        Log = T,
                        TSpace = T,
                        OneVsAll = T , 
                        PivotGroup = NULL, 
                        HistoryLength = 1,
                        ModelType = "coda", 
                        WindowMethod ="extending",
                        Category_Main = c("1", "2", "3", "4"),
                        TakeSubCategory = F,
                        Category_Sub = NULL) {
  
  #Checking Input
  stopifnot("Model Type not correct."=ModelType %in% c("coda","coda_OneVsAll"))
  stopifnot("Log,TSpace,OneVsAll and TakeSubCategory must be boolean."=
              all(sapply(c(Log,TSpace,OneVsAll,TakeSubCategory),is.logical)))
  stopifnot("ZeroHandling,WindowMethod and Category_Main must be character vectors."=
              all(sapply(c(ZeroHandling,WindowMethod,Category_Main),is.character)))
  stopifnot("Id,PredictionStep,HistoryLength and Frame must be numeric."= 
              all(sapply(c(Id,PredictionStep,HistoryLength,Frame),is.numeric)))
  
  stopifnot("Frame and HistoryLength must both be greater than 0."=Frame>0 & HistoryLength >0)
  
  
  #Only return IDs with results
  Id_Result <- Id
  
  #one vs all for all pivot groups
  if(OneVsAll) {
  
  PredictionResult_AllIDAllPivotGroup <- lapply(Id,function(Id_RunVariable){
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
      stopifnot("Subcategory must be part of main category."=
                  Category_Sub %in% unique(Data_Processed$sub_category_id))
      Data_Processed <- Data_Processed %>% 
        filter(sub_category_id %in% as.integer(Category_Sub))
    }
  
    
    #Operating on Sub category Level
    if(TakeSubCategory){
      stopifnot("Only one main category can be chosen"=length(Category_Main)==1)
      Data_Processed <- Data_Processed %>%
        dplyr::select(week_date, main_category_id, sub_category_id ,sold) %>%
        arrange(week_date)
      
      Category <- unique(Data_Processed$sub_category_id)
      
    } else {
      Data_Processed <- Data_Processed %>%
        dplyr::select(week_date, main_category_id,sold) %>%
        arrange(week_date)
      
      Category <- unique(Data_Processed$main_category_id)
    }
    
    #Checking input
    if(is.null(PivotGroup)){
      PivotGroup <- as.character(Category)
    } else {
      stopifnot("PivotGroup must be character."=is.character(PivotGroup))
    }
    stopifnot("PivotGroup must be analysed categories."=PivotGroup %in% Category)
    
    
    PredictionResult_AllPivotGroup <- lapply(PivotGroup,function(PivotGroup_RunVariable){
      
        #Calculating the length of the timeseries
        TimeSeries_Length <- length(unique(Data_Processed$week_date))
        
        
        #Preparing transformed data
        Data_Prepared <- Coda.DataPreparation(Data_Processed, 
                                               ZeroHandling = ZeroHandling,
                                               TSpace = TSpace, 
                                               Log = Log,
                                               OneVsAll = T,
                                               PivotGroup = PivotGroup_RunVariable,
                                               HistoryLength = HistoryLength,
                                               TakeSubCategory = TakeSubCategory,
                                               Category = Category) %>%
          arrange(week_date)
        

        
        #If the Frame is given as a fraction, calculate the absolute length. We set 5 as the minimum length needed. 
        Frame_Help <- "fixed"
        if(dim(Data_Prepared)[1]<5){
          return(NA)
          }
        if(Frame < 1){
          Frame_Help <- as.character(Frame)
          Frame = round(Frame*dim(Data_Prepared)[1])
          if(Frame < 5){
            Frame = 5
          }
        }
        
        
        #Splitting transformed data into windows
        Data_Window <- Data.Window(Data_Prepared,
                                         Frame=Frame,
                                         Method = WindowMethod,
                                         PredictionStep = PredictionStep)
        
        
        
        #Preparing non transformed data 
        Data_NoTransform <- Coda.DataPreparation(Data_Processed,
                                                 ZeroHandling="none",
                                                 TSpace=TSpace,
                                                 Log=F,
                                                 OneVsAll = T,
                                                 PivotGroup = PivotGroup_RunVariable,
                                                 HistoryLength = HistoryLength,
                                                 TakeSubCategory = TakeSubCategory,
                                                 Category = Category) %>%
          arrange(week_date)
        
        #Splitting non transformed data into windows
        Data_WindowNoTransform <- Data.Window(Data_NoTransform,
                                            Frame=Frame,
                                            Method = WindowMethod,
                                            PredictionStep = PredictionStep)
        
        
        #Carrying out model fitting and prediction
        PredictionResult <- Coda.Prediction(Data_Window = Data_Window,
                                            Data_WindowNoTransform = Data_WindowNoTransform,
                                            Data_NoTransform = Data_NoTransform,
                                            PredictionStep = PredictionStep,
                                            OneVsAll = T,
                                            TSpace = TSpace,
                                            Log =  Log,
                                            PivotGroup = PivotGroup_RunVariable,
                                            Frame = Frame, 
                                            Category = Category)
        
        PredictionResult$result$id <- Id_RunVariable
        PredictionResult$result$windowMethod <- WindowMethod
        PredictionResult$result$model <- ModelType 
        PredictionResult$result$zeroHandling <- ZeroHandling
        PredictionResult$result$frame <- Frame_Help
        PredictionResult$result$history <- as.character(HistoryLength)
        PredictionResult$result$timeseriesLength <- as.character(TimeSeries_Length)
        PredictionResult$result$oneVsAll <- OneVsAll
      
          
        #Tidying up data
        Result_Prediction <- PredictionResult$result
        Result_Model <- PredictionResult$model
        
        return(list(result = Result_Prediction,
                    model = Result_Model))
        
     })
      
      #Removing NA (aka Timeseries which are too short)
      PredictionResult_AllPivotGroup <- PredictionResult_AllPivotGroup[!is.na(PredictionResult_AllPivotGroup)]
      
      if(length(PredictionResult_AllPivotGroup)==0){
        print(paste("Insufficient data. Skipping ID: ",Id_RunVariable))
        Id_Result <<- Id_Result[Id_RunVariable!=Id_Result]
        return(NA)
      }
    
      #Tidying up data
      Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllPivotGroup,"result"))
      Result_Prediction$id <- as.factor(Result_Prediction$id)
      Result_Model <- UnlistListElement(PredictionResult_AllPivotGroup,"model")
      names(Result_Model) <- PivotGroup
    
      return(list(result = Result_Prediction,
                  model = Result_Model))
    
      
    })
  
    #Removing NA (aka Timeseries which are too short)
  PredictionResult_AllIDAllPivotGroup <- PredictionResult_AllIDAllPivotGroup[!is.na(PredictionResult_AllIDAllPivotGroup)]
  
    if(length(PredictionResult_AllIDAllPivotGroup)==0)return(NA)
  
  
    #Tidying up data
    Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllIDAllPivotGroup,"result"))
    Result_Model <- UnlistListElement(PredictionResult_AllIDAllPivotGroup,"model")
    names(Result_Model) <- Id_Result
  
    return(list(result = Result_Prediction,
                model = Result_Model))
    
  }
  #Not one vs all
  else {
    
    PredictionResult_AllID <- lapply(Id,function(Id_RunVariable){
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
        stopifnot("Subcategory must be part of main category."=
                    Category_Sub %in% unique(Data_Processed$sub_category_id))
        Data_Processed <- Data_Processed %>% 
          filter(sub_category_id %in% as.integer(Category_Sub))
      }
      
      #Operating on Sub category Level
      if(TakeSubCategory){
        stopifnot("Only one main category can be chosen"=length(Category_Main)==1)
        Data_Processed <- Data_Processed %>%
          dplyr::select(week_date, main_category_id, sub_category_id ,sold) %>%
          arrange(week_date)
        
        Category <- unique(Data_Processed$sub_category_id)
        
      } else {
        Data_Processed <- Data_Processed %>%
          dplyr::select(week_date, main_category_id,sold) %>%
          arrange(week_date)
        
        Category <- unique(Data_Processed$main_category_id)
      }
        
        #Calculating the length of the timeseries
        TimeSeries_Length <- length(unique(Data_Processed$week_date))
        
        #Preparing transformed data
        Data_Prepared <- Coda.DataPreparation(Data_Processed, 
                                              ZeroHandling = ZeroHandling,
                                              TSpace = TSpace, 
                                              Log = Log,
                                              OneVsAll = F,
                                              HistoryLength = HistoryLength,
                                              TakeSubCategory = TakeSubCategory,
                                              Category = Category) %>% arrange(week_date)
        
        
        #If the Frame is given as a fraction, calculate the absolute length. We set 5 as the minimum length needed.
        Frame_Help <- "fixed"
        if(dim(Data_Prepared)[1]<5){
          return(NA)
        }
        if(Frame < 1){
          Frame_Help <- as.character(Frame)
          Frame = round(Frame*dim(Data_Prepared)[1])
          if(Frame < 5){
            Frame = 5
          }
        }
        
        
        #Splitting transformed data into windows
        Data_Window <- Data.Window(Data_Prepared,
                                         Frame= Frame ,
                                         Method = WindowMethod,
                                         PredictionStep = PredictionStep)
        
        
        
        #Preparing non transformed data 
        Data_NoTransform <- Coda.DataPreparation(Data_Processed,
                                                 ZeroHandling="none",
                                                 TSpace=TSpace,
                                                 Log=F, 
                                                 OneVsAll = F,
                                                 HistoryLength = HistoryLength,
                                                 TakeSubCategory = TakeSubCategory,
                                                 Category = Category) %>% arrange(week_date)
        #Splitting non transformed data into windows
        Data_WindowNoTransform <- Data.Window(Data_NoTransform,
                                         Frame = Frame,
                                         Method = WindowMethod,
                                         PredictionStep = PredictionStep)
        
        
        PredictionResult <- Coda.Prediction(Data_Window = Data_Window, 
                                              Data_WindowNoTransform = Data_WindowNoTransform, 
                                              Data_NoTransform = Data_NoTransform, 
                                              PredictionStep = PredictionStep,
                                              OneVsAll = F,
                                              TSpace = TSpace,
                                              Log =  Log, 
                                              Frame = Frame,
                                              Category = Category)
        PredictionResult$result$id <- Id_RunVariable
        PredictionResult$result$windowMethod <- WindowMethod
        PredictionResult$result$model <- ModelType 
        PredictionResult$result$zeroHandling <- ZeroHandling
        PredictionResult$result$frame <- Frame_Help
        PredictionResult$result$history <- as.character(HistoryLength)
        PredictionResult$result$timeseriesLength <- as.character(TimeSeries_Length)
        PredictionResult$result$oneVsAll <- OneVsAll
        
        #Tidying up data
        Result_Prediction <- PredictionResult$result
        Result_Model <- PredictionResult$model
        
        return(list(result = Result_Prediction,
                    model = Result_Model))
        
    })
    
    #Removing NA (aka Timeseries which are too short)
    PredictionResult_AllID <- PredictionResult_AllID[!is.na(PredictionResult_AllID)]
    
    if(length(PredictionResult_AllID)==0){
      print(paste("Insufficient data. Skipping ID: ",Id_RunVariable))
      Id_Result <<- Id_Result[Id_RunVariable!=Id_Result]
      return(NA)
    }
    
    #Tidying up data
    Result_Prediction <- bind_rows(UnlistListElement(PredictionResult_AllID,"result"))
    Result_Prediction$id <- as.factor(Result_Prediction$id)
    
    if(TakeSubCategory){
      Result_Prediction$main_category <- Category_Main
    }
    
    Result_Model <- UnlistListElement(PredictionResult_AllID,"model")
    names(Result_Model) <- unique(as.character(Result_Prediction$id))
    
    return(list(result = Result_Prediction,
                model = Result_Model))
    
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

