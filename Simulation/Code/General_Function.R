#General Helper Functions

#This function splits the time series into appropriate windows to fit the model
Data.Window <- function(Timeseries,Frame,Method = c("non-overlapping", "overlapping", "extending"),
                    PredictionStep = 1) {
  
  Method <- match.arg(Method)
  Timeseries_Length <- dim(Timeseries)[1]
  
  if (is.null(Timeseries_Length)) {
    Timeseries <- as.matrix(Timeseries, ncol = 1)
    Timeseries_Length <- dim(Timeseries)[1]
  }
  if (is.null(Timeseries_Length))
    stop("Enter valid timeseries")
  
  stopifnot(Timeseries_Length >= Frame)
  
  if (Method == "non-overlapping") {
    Window_Number <-
      floor(Timeseries_Length / Frame) - ifelse(Timeseries_Length %% Frame < PredictionStep, 1, 0) - 
      ifelse(Frame < PredictionStep, 1, 0) #div and prediction step
    
    StartIndex <-
      max(1,Timeseries_Length - Window_Number * Frame - PredictionStep)
    
    Timeseries <- Timeseries[StartIndex:Timeseries_Length, ]
    
    Result <- lapply(c(0:(Window_Number - 1)), function(i) {
      return(list(timeSeriesValue_window = Timeseries[(i * Frame + 1):((i + 1) * Frame), ],
                  timeSeriesValue_future = Timeseries[((i + 1) * Frame + PredictionStep), ]))
    })
    
    names(Result) <- c(1:Window_Number)
  }
  
  else if (Method == "overlapping") {
    #Length of the series - length of the window + prediction step equals the Number of windows
    Window_Number <- Timeseries_Length - Frame - PredictionStep + 1
    
    Result <- lapply(c(1:Window_Number), function(i) {
      return(list(timeSeriesValue_window = Timeseries[i:(Frame + i - 1), ],
                  timeSeriesValue_future = Timeseries[(Frame + i - 1 + PredictionStep), ]))
    })
    names(Result) <- c(1:Window_Number)
  }
  
  else if (Method == "extending") {
    #Length of the series - length of the first window + prediction step equals the number of windows
    Window_Number <- Timeseries_Length - Frame - PredictionStep + 1
    
    Result <- lapply(c(1:Window_Number), function(i) {
      return(list(timeSeriesValue_window = Timeseries[1:(Frame + i - 1), ],
                  timeSeriesValue_future = Timeseries[(Frame + i - 1 + PredictionStep), ]))
    })
    names(Result) <- c(1:Window_Number)
  }
  
  else {
    stop("Enter valid Method")
  }
  
  return(Result)
}



#Function to transform the data into the right format
Data.Preparation <- function(Data_Raw,
                             OneVsAll=F,
                             PivotGroup="1",
                             Category=c(1,2,3,4),
                             NA_to=0,
                             HistoryLength = 1,
                             TakeSubCategory = FALSE){
  
  #Sub or Main Category
  if(TakeSubCategory){
    Category_Var <- "sub_category_id"
    Data_Raw <- subset(Data_Raw,select = -main_category_id)
  }else{
    Category_Var <- "main_category_id"
#    Data_Raw <- subset(Data_Raw,select = -sub_category_id)
  }

  
  columns <- c("week_date", as.character(Category))
  
    Data_Raw <- Data_Raw %>%
    dplyr::filter(get(Category_Var) %in% Category) %>%
    group_by(across(all_of(Category_Var)), week_date) %>%
    dplyr::mutate(sold = sum(sold)) %>% 
    dplyr::select(any_of(c("week_date",Category_Var,"sold"))) %>%
    dplyr::distinct() %>%
    pivot_wider(names_from
                = Category_Var, values_from = sold) %>%
    ungroup() %>%
    setnafill(type = "const", fill = NA_to) %>%
    dplyr::select(any_of(columns)) %>%
    dplyr::mutate(across(.cols = as.character(Category), .fns = as.double))
  
  #Determining the length of the Timeseries
  DataRaw_Length <- dim(Data_Raw)[1]
  
  if(between(HistoryLength,0,1)){
    DataRaw_Start <- round(DataRaw_Length * (1-HistoryLength) + 1)
  }else {
    DataRaw_Start <- DataRaw_Length - HistoryLength
  }
  
  Data_Raw <- Data_Raw[DataRaw_Start:DataRaw_Length,]
  
  if (OneVsAll) {
    Data_Raw <- Data_Raw %>%
      dplyr::mutate(other = dplyr::select(.,-all_of(PivotGroup), -"week_date") %>% rowSums(na.rm =
                                                                                             F)) %>%
      dplyr::select("week_date", all_of(PivotGroup), "other")
    
  }
  
  return(Data_Raw)
  
}



## Normation function for the normed prediction error 
Normation <- function(x, y) {
  stopifnot(length(x)==length(y))
  i <- length(x)
  Result <- 0
  for (j in 1:i) {
    Summand <- (x[j] - y[j]) ^ 2 / i
    Result <- Result + Summand
    
  }
  return(Result)
}



##Helper function for cleaner code
UnlistListElement <- function(x,Element){
  
  List_Length <- length(x)
  Result <- lapply(c(1:List_Length),function(i){
    return(x[[i]][[Element]])
  })
  return(Result)
}


#Identity function
Idf <- function(x)x 



#MSE
Mse <- function(y,yhat){
  return(mean((y-yhat)^2))
}

#RMSE
Rmse <- function(y,yhat){
  return(sqrt(mean((y-yhat)^2)))
}



#Error calculation
Model.Error <- function(Model_Result,Fnct = "Rmse", Category = c(1,2,3,4)){
  
  f <- match.fun(Fnct)
  
  ErrorMeasure <- Model_Result$result %>%
    dplyr::filter(category %in% Category) %>%
    group_by(id, category) %>%
    dplyr::summarise(error = f(valuePredict,valueTrue),.groups = "keep")
  
  ErrorMeasure_Naive <- Model_Result$result %>%
    dplyr::filter(category %in% Category) %>%
    group_by(id, category) %>%
    dplyr::summarise(error_naive = f(valueLastKnown,valueTrue),.groups = "keep")
  
  Result <- full_join(ErrorMeasure,
                                   ErrorMeasure_Naive,by = c("id","category") )
  
  Result$model <- unique(Model_Result$result$model)
  
  return(Result)
}



#Function to compare the MSEs/RMSEs of each fridge
Model.ErrorOverall <- function(Error_Result,Fnct = "sum",SplitByGroup=T,Groups=list(Group1=c(1,2),Group2=c(3,4)),Category=c(1,2,3,4)){
  
  f <- match.fun(Fnct)
  
  if(SplitByGroup){
    
    for(i in 1:length(Groups))
    {
      Group <- Groups[[i]]
      #Calculating the summarised Naive Error and replacing 0 values with 0.000001
      ErrorNaiveSummarised_Group <- Error_Result %>%
        dplyr::filter(category %in% Group)%>%
        dplyr::group_by(id) %>%
        dplyr::summarise(error_naive = f(error_naive),.groups = "keep")
      ErrorNaiveSummarised_Group$error_naive[ErrorNaiveSummarised_Group$error_naive==0] <- 0.000001
      
      #Calculating the summarised Error
      ErrorSummarised_Group <- Error_Result %>%
        dplyr::filter(category %in% Group)%>%
        dplyr::group_by(id) %>%
        dplyr::summarise(error = f(error),.groups = "keep")
      
      #Joining the data frames
      ErrorMeasure_Group <- full_join(ErrorNaiveSummarised_Group,ErrorSummarised_Group,by="id")
      
      #Calculating the Error Measure
      ErrorMeasure_Group <- ErrorMeasure_Group %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(error = error/error_naive,.groups = "keep")
      ErrorMeasure_Group$model <- unique(Error_Result$model)
      ErrorMeasure_Group$group <- paste(Group,collapse=",")
      
      #Combining the Groups
      if(i==1){
        ErrorMeasure <- ErrorMeasure_Group
      }
      else{
        ErrorMeasure <- rbind(ErrorMeasure,ErrorMeasure_Group)
      }
    }
    
  }else{
    
    #Calculating the summarised Naive Error and replacing 0 values with 0.000001
    ErrorNaiveSummarised <- Error_Result %>%
      dplyr::filter(category %in% Category)%>%
      dplyr::group_by(id) %>%
      dplyr::summarise(error_naive = f(error_naive),.groups = "keep")
    ErrorNaiveSummarised$error_naive[ErrorNaiveSummarised$error_naive==0] <- 0.000001
    
    #Calculating the summarised Error
    ErrorSummarised <- Error_Result %>%
      dplyr::filter(category %in% Category)%>%
      dplyr::group_by(id) %>%
      dplyr::summarise(error = f(error),.groups = "keep")
    
    #Joining the data frames
    ErrorMeasure <- full_join(ErrorNaiveSummarised,ErrorSummarised,by="id")
    
    #Calculating the Error Measure
    ErrorMeasure<- ErrorMeasure%>%
      dplyr::group_by(id) %>%
      dplyr::summarise(error = error/error_naive,.groups = "keep")
    ErrorMeasure$model <- unique(Error_Result$model)
    ErrorMeasure$group <- "all"
    

  }

  return(ErrorMeasure)
}



#Function to calculate the cumulative MSE/RMSE from right to left
ErrorMeasure.Cumulated <- function(Data_Raw,Fnct="Rmse"){
  
  f <- match.fun(Fnct)
  
  CountIndex <- 1
  Model <- Category <- vector("character",length=dim(Data_Raw)[1])
  ErrorMeasure <- Window <- vector("numeric",length=dim(Data_Raw)[1])
  
  for(m in unique(Data_Raw$model)){
    for(cat in unique(Data_Raw$category)){
      
      data <- Data_Raw %>% filter(model==m & category ==cat)
      
      for(i in 1:dim(data)[1]){
        
        ErrorMeasure[CountIndex] <- f(data[1:i,"valueTrue"],data[1:i,"valuePredict"])
        Category[CountIndex] <- cat
        Model[CountIndex] <- m
        Window[CountIndex] <- dim(data)[1] - (i-1)
        CountIndex <- CountIndex + 1
        
      }
    }
  }
  Result <- data.frame(errorMeasure = ErrorMeasure,
                       category = Category,
                       model = Model,
                       window = Window,
                       id = unique(Data_Raw$id))
  return(Result)
}


#For loading RData files
Object.Load <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}



