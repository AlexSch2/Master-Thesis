#Data preperation

source("data_preperation.R")
source("coda_functions.R")

data_prepared_all_plus1<-data_prepared_all_plus1_tspace<-coda.data.preperation(test_data,zero_handling = "all")
data_prepared_only0_plus1<-data_prepared_only0_plus1_tspace<-coda.data.preperation(test_data,zero_handling = "zeros_only")


data_prepared_all_plus1_tspace["Sum"]<-rowSums(data_prepared_all_plus1[c("X1","X2","X3")])
data_prepared_only0_plus1_tspace["Sum"]<-rowSums(data_prepared_only0_plus1[c("X1","X2","X3")])

data_length<-dim(data_prepared_all_plus1)[1]

timeframes<-c(40,60,70)



optimal_parameters_all_plus1<-coda.tuning(data_prepared_all_plus1,timeframes = timeframes,
                                 lag.max = 6,information_criteria_lag = "AIC",
                                 information_criteria_frame = "prediction_error",
                                 prediction_error_step = 1,multicore = FALSE)

indexes_all_plus1<-seq(data_length-optimal_parameters_all_plus1$frame,data_length-1,1)

model_all_plus1<-VAR(data_prepared_all_plus1[indexes_all_plus1,-1],p=optimal_parameters_all_plus1$p)

prediction_error(model_all_plus1,fitted_data =data_prepared_all_plus1[indexes_all_plus1,-1],
                 true_values = data_prepared_all_plus1[data_length,-1],prediction_error_step = 1)




optimal_parameters_only0_plus1<-coda.tuning(data_prepared_only0_plus1,timeframes = timeframes,
                                          lag.max = 6,information_criteria_lag = "AIC",
                                          information_criteria_frame = "prediction_error",
                                          prediction_error_step = 1,multicore = FALSE)

indexes_only0_plus1<-seq(data_length-optimal_parameters_only0_plus1$frame,data_length-1,1)

model_only0_plus1<-VAR(data_prepared_all_plus1[indexes_only0_plus1,-1],p=optimal_parameters_all_plus1$p)

prediction_error(model_only0_plus1,fitted_data =data_prepared_only0_plus1[indexes_all_plus1,-1],
                 true_values = data_prepared_only0_plus1[data_length,-1],prediction_error_step = 1)



