#Data preperation

source("data_preperation.R")
source("dependencies.R")


#Choosing random fridge as test data
test_data<-weekly_category_data%>%filter(fridge_id==4)
View(test_data)

test_data_prep <- weekly_category_data %>%
  filter(fridge_id == 4) %>%
  dplyr::select(week_date, main_category_id, sold) %>%
  arrange(week_date) %>% data.preparation()

raw_length<-dim(test_data_prep)[1]

data_prep<-cbind(test_data_prep[-1,-1],test_data_prep[-raw_length,-1])

names(data_prep)<-c("y1","y2","y3","y4",
                    "x1","x2","x3","x4")

fit_length<-round(dim(test_data_prep)[1]*2/3)

data_fit<-data_prep[1:fit_length,]

data_fit<-dplyr::select(data_fit,y1,y2,y3,y4,x1,x2,x3,x4)


model_zim<-zim(y1~x1+x2+x3+x4, data=data_fit,control = zim.control(type="ginv"))

