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
Data.Preparation <- function(Data_Raw,OneVsAll=F,PivotGroup="1",Category=c(1,2,3,4),NA_to=0){
  
  columns <- c("week_date", as.character(Category))
  
  Data_Raw <- Data_Raw %>%
    dplyr::filter(main_category_id %in% Category) %>%
    group_by(main_category_id, week_date) %>%
    dplyr::mutate(sold = sum(sold)) %>%
    dplyr::distinct() %>%
    pivot_wider(names_from
                = main_category_id, values_from = sold) %>%
    ungroup() %>%
    setnafill(type = "const", fill = NA_to) %>%
    dplyr::select(any_of(columns)) %>%
    dplyr::mutate(across(.cols = as.character(Category), .fns = as.double))
  
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


#Error calculation
Model.Error <- function(Model_Result,Fnct = "Mse"){
  
  f <- match.fun(Fnct)
  
  ErrorMeasure <- Model_Result$results %>%
    dplyr::filter(category %in% c(1, 2, 3, 4)) %>%
    group_by(id, category) %>%
    dplyr::summarise(error = f(predicted_value,true_value),.groups = "keep")
  
  ErrorMeasure_Naive <- Model_Result$results %>%
    dplyr::filter(category %in% c(1, 2, 3, 4)) %>%
    group_by(id, category) %>%
    dplyr::summarise(error_naive = f(last_known_value,true_value),.groups = "keep")
  
  Result <- full_join(ErrorMeasure,
                                   ErrorMeasure_Naive,by = c("id","category") )
  
  Result$model <- unique(Model_Result$results$model)
  
  return(Result)
}



#Function to compare the MSEs of each fridge
Model.ErrorOverall <- function(Error_Result,Fnct = "mean"){
  
  f <- match.fun(Fnct)
  
  ErrorMeasure <- Error_Result %>%
                    dplyr::group_by(id) %>%
                    dplyr::summarise(error = f(error)/f(Error_Naive),.groups = "keep")
  ErrorMeasure$model <- unique(Error_Result$model)
  return(ErrorMeasure)
}



#Function to calculate the cumulative MSE from right to left
Mse.Cumulated <- function(Data_Raw){
  
  CountIndex <- 1
  Model <- Category <- vector("character",length=dim(Data_Raw)[1])
  Mse_Cumulated <- Window <- vector("numeric",length=dim(Data_Raw)[1])
  
  for(m in unique(Data_Raw$model)){
    for(cat in unique(Data_Raw$Category)){
      
      data <- Data_Raw %>% filter(model==m & category ==cat)
      
      for(i in 1:dim(data)[1]){
        
        Mse_Cumulated[CountIndex] <- mse(data[1:i,"true_value"],data[1:i,"predicted_value"])
        Category[CountIndex] <- cat
        Model[CountIndex] <- m
        Window[CountIndex] <- dim(data)[1] - (i-1)
        CountIndex <- CountIndex + 1
        
      }
    }
  }
  Result <- data.frame(mse_cumulated = Mse_Cumulated,
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


