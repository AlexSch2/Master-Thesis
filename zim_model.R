#Data preperation

source("data_preperation.R")


#Choosing random fridge as test data
test_data<-weekly_category_data%>%filter(fridge_id==4)
View(test_data)

model_zim<-zim()


