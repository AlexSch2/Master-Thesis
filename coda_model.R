#Data preperation

source("data_preperation.R")
source("coda_functions.R")

ids<-c(1,2,3,4,6,7,8)


coda_models<-coda.analysis(weekly_category_data,ids)

coda.plots(coda_models,save=FALSE,plot.type = "dots")

