#General Helper Functions

#This function splits the time series into appropriate windows to determine the optimal length for the model
windows <- function(timeseries,frame,method = c("non-overlapping", "overlapping", "extending"),
                    prediction_error_step = 1) {
  
  method <- match.arg(method)
  timeseries_length <- dim(timeseries)[1]
  if (is.null(timeseries_length)) {
    timeseries <- as.matrix(timeseries, ncol = 1)
    timeseries_length <- dim(timeseries)[1]
  }
  if (is.null(timeseries_length))
    stop("Enter valid timeseries")
  
  stopifnot(timeseries_length >= frame)
  
  if (method == "non-overlapping") {
    window_number <-
      floor(timeseries_length / frame) - ifelse(timeseries_length %% frame < prediction_error_step, 1, 0) - 
      ifelse(frame < prediction_error_step, 1, 0) #div and prediction step
    
    start_index <-
      max(1,timeseries_length - window_number * frame - prediction_error_step)
    
    timeseries <- timeseries[start_index:timeseries_length, ]
    
    res <- lapply(c(0:(window_number - 1)), function(i) {
      return(list(fitting = timeseries[(i * frame + 1):((i + 1) * frame), ],
                  prediction_value = timeseries[((i + 1) * frame + prediction_error_step), ]))
    })
    
    names(res) <- c(1:window_number)
  }
  
  else if (method == "overlapping") {
    #Length of the series - length of the window + prediction step equals the number of windows
    window_number <- timeseries_length - frame - prediction_error_step + 1
    
    res <- lapply(c(1:window_number), function(i) {
      return(list(fitting = timeseries[i:(frame + i - 1), ],
                  prediction_value = timeseries[(frame + i - 1 + prediction_error_step), ]))
    })
    names(res) <- c(1:window_number)
  }
  
  else if (method == "extending") {
    #Length of the series - length of the first window + prediction step equals the number of windows
    window_number <- timeseries_length - frame - prediction_error_step + 1
    
    res <- lapply(c(1:window_number), function(i) {
      return(list(fitting = timeseries[1:(frame + i - 1), ],
                  prediction_value = timeseries[(frame + i - 1 + prediction_error_step), ]))
    })
    names(res) <- c(1:window_number)
  }
  
  else {
    stop("Enter valid method")
  }
  
  return(res)
}



#Function to transform the data into the right format
data.preparation <- function(data_raw,one_vs_all=F,pivot_group="1",categories=c(1,2,3,4),nas_to=0){
  
  columns <- c("week_date",as.character(categories))
  
  data_raw <- data_raw %>%
    dplyr::filter(main_category_id %in% categories) %>%
    group_by(main_category_id, week_date) %>%
    dplyr::mutate(sold = sum(sold)) %>%
    dplyr::distinct() %>%
    pivot_wider(names_from
                = main_category_id, values_from = sold) %>%
    ungroup() %>%
    setnafill(type = "const", fill = nas_to) %>%
    dplyr::select(any_of(columns)) %>% 
    dplyr::mutate(across(.cols=as.character(categories),.fns = as.double))
  
  if (one_vs_all) {
    data_raw <- data_raw %>%
      dplyr::mutate(other = dplyr::select(., -all_of(pivot_group),-"week_date") %>% rowSums(na.rm =
                                                                                              F)) %>%
      dplyr::select("week_date", all_of(pivot_group), "other")
    
  }
  
  return(data_raw)
  
  
}



## Normation function for the normed prediction error 
normation <- function(x, y) {
  stopifnot(length(x)==length(y))
  i <- length(x)
  result <- 0
  for (j in 1:i) {
    summand <- (x[j] - y[j]) ^ 2 / i
    result <- result + summand
    
  }
  return(result)
}



##Helper function for cleaner code
unlist.element <- function(x,element){
  
  list.length <- length(x)
  res <- lapply(c(1:list.length),function(i){
    return(x[[i]][[element]])
  })
  return(res)
}


idf <- function(x)x 



#MSE
mse <- function(y,yhat){
  return(mean((y-yhat)^2))
}


#Error calculation
model.error <- function(model_result,fnct = "mse"){
  
  f <- match.fun(fnct)
  
  error_measures <- model_result$results %>%
    dplyr::filter(category %in% c(1, 2, 3, 4)) %>%
    group_by(id, category) %>%
    dplyr::summarise(error = f(predicted_value,true_value),.groups = "keep")
  
  error_measures_naive <- model_result$results %>%
    dplyr::filter(category %in% c(1, 2, 3, 4)) %>%
    group_by(id, category) %>%
    dplyr::summarise(naive_error = f(last_known_value,true_value),.groups = "keep")
  
  result <- full_join(error_measures,
                                   error_measures_naive,by = c("id","category") )
  
  result$model <- unique(model_result$results$model)
  
  return(result)
}

#Function to compare the MSEs of each fridge
model.error.overall <- function(error_result,fnct = "mean"){
  
  f <- match.fun(fnct)
  
  error_measures <- error_result %>%
                    dplyr::group_by(id) %>%
                    dplyr::summarise(error = f(error)/f(naive_error),.groups = "keep")
  error_measures$model <- unique(error_result$model)
  return(error_measures)
}


#Function to calculate the cumulative MSE from right to left
mse.cumulated <- function(data_raw){
  
  count_index <- 1
  model <- category <- vector("character",length=dim(data_raw)[1])
  mse_cum <- window <- vector("numeric",length=dim(data_raw)[1])
  
  for(m in unique(data_raw$model)){
    for(cat in unique(data_raw$category)){
      
      data <- data_raw %>% filter(model==m & category ==cat)
      
      for(i in 1:dim(data)[1]){
        
        mse_cum[count_index] <- mse(data[1:i,"true_value"],data[1:i,"predicted_value"])
        category[count_index] <- cat
        model[count_index] <- m
        window[count_index] <- dim(data)[1] - (i-1)
        count_index <- count_index + 1
        
      }
    }
  }
  result <- data.frame(mse_cum = mse_cum,
                       category = category,
                       model = model,
                       window = window,
                       id = unique(data_raw$id))
  return(result)
}


#For loading RData files
load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}


