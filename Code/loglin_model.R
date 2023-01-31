#Data preperation

source("data_preperation.R")

test_data_single_group<-test_data%>%
  filter(main_category_id==1)%>%
  dplyr::select(week_date,sold)


loglin_model<-tsglm(test_data_single_group$sold,model = list("past_obs"=2,"past_mean"=2),link = "log")
summary(loglin_model)
