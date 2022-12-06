#Data prep
source("dependencies.R")

#Selecting one random fridge
#HAS TO HAVE THIS FORM FROM NOW ON
test_data<-weekly_category_data%>%
  filter(fridge_id==4)%>%
  dplyr::select(week_date,main_category_id,sold)
