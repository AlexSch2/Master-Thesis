#General Helper Functions

#This function splits the time series into appropriate windows to determine the optimal length for the model

windows <- function(timeseries,frame,method = c("non-overlapping", "overlapping"),
                    prediction_error_step = 1) {
  
  method <- match.arg(method)
  timeseries_length <- dim(timeseries)[1]
  
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
    #Length of the series - length of the window - prediction step equals the number of windows
    window_number <- timeseries_length - frame - prediction_error_step +
      1
    
    res <- lapply(c(1:window_number), function(i) {
      return(list(fitting = timeseries[i:(frame + i - 1), ],
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
  i <- length(x)
  result <- 0
  for (j in 1:i) {
    summand <- (x[j] - y[j]) ^ 2 / i
    result <- result + summand
    
  }
  return(result)
}


