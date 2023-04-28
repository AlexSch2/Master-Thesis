dummydata<-data.frame(a=rnorm(10)+5,b=rnorm(10)+5,c=rnorm(10)+5)

dummydata$a[5]<-30

gooddata<-data.frame(a=rnorm(10)+5,b=rnorm(10)+5,c=rnorm(10)+5)

dummydata.irl<-pivotCoord(dummydata)
gooddata.irl<-pivotCoord(gooddata)

modeldumm<-VAR(dummydata.irl)
modelgood<-VAR(gooddata.irl)

dummy.pred<-predict(modeldumm,n.ahead = 1)
good.pred<-predict(modelgood,n.ahead=1)

dummy.pred<-matrix(c(0.4,-0.0031),ncol=2)
good.pred<-matrix(c(0.166,-0.0699),ncol=2)

pivotCoordInv(dummy.pred)
pivotCoordInv(good.pred)


data_raw$`4`[1:28]<-c(rep(c(3:16),2))
data_raw$`3`[1:28]<-c(rep(c(13:26),2))

raw_length<-dim(data_raw)[1]

data_prep<-cbind(data_raw[-1,-1],data_raw[-raw_length,-1])

names(data_prep)<-c("y1","y2","y3","y4",
                    "x1","x2","x3","x4")

fit_length<-round(dim(data_raw)[1]*2/3)

data_fit<-data_prep[1:fit_length,]


model<-vgam(cbind(y1,y2,y3,y4)~s(x1)+s(x2)+s(x3)+s(x4),data=data_fit,family = poissonff(link = "identitylink"),
            control = vgam.control(criterion = "coefficients",maxit = 30))


test<-coda.analysis(weekly_category_data = weekly_category_data,ids=c(4,6),one_vs_all = F,
                    pivot_groups = c("1","2"),tspace = T,take_log = T)
test2<-ingarch.analysis(weekly_category_data = weekly_category_data,ids=c(4,6),distribution = "poisson",categories = c("1","2"))
View(test2)

test<-ingarch.prediction(data_raw,1)

test<-unlist(ingarch_result[[2]],recursive = F)

View(test)


div <- sapply(c(1:dim(coda_result)[1]),function(i){
  
  return(normation(x = coda_result$true_value[1:i],
                   y = coda_result$naive_predicted_value[1:i]))
  })


model <- coda_result[["models"]][[1]][["window30"]]

predicted_value <-  predict(model, fitting_data, n.ahead = prediction_error_step)

B <- Bcoef(model)
Z <- as.matrix(model$datamat[, -c(1:model$K)])
B%*%Z
size = 2


div_coda <- sapply(c(1:dim(plot_data_coda)[1]),function(i){
  
  return(normation(x = plot_data_coda$true_value[1:i],
                   y = plot_data_coda$last_known_value[1:i]))}
)

if(0 %in% div_coda ) div_coda[div_coda==0] <- 0.5

div_ingarch <- sapply(c(1:dim(plot_data_ingarch)[1]),function(i){
  
  return(normation(x = plot_data_ingarch$true_value[1:i],
                   y = plot_data_ingarch$last_known_value[1:i]))}
)

if(0 %in% div_ingarch ) div_ingarch[div_ingarch==0] <- 0.5
result_prediction$prediction_error_normed <- result_prediction$prediction_error_normed/div


ingarch_model<- ingarch_result[["model"]][["4"]][["2"]][["window37"]]
summary(ingarch_model)
summary(Model)




res <- lapply(1:5,function(i){
  
  if(i%%2==0)return(NA)
  return(list(a=1,b=2))
  
})

res_index <- which(is.na(res)==TRUE)


test <- "namet"

 x <- data.frame(y=1)
x[paste(test)] <- 2
x




#Choosing random fridge as test data
test_data<-weekly_category_data%>%filter(fridge_id==8)
View(test_data)

test_data_prep <- weekly_category_data %>%
  filter(fridge_id == 8) %>%
  dplyr::select(week_date, main_category_id, sold) %>%
  arrange(week_date) %>% Data.Preparation()

raw_length<-dim(test_data_prep)[1]

data_prep<-cbind(test_data_prep[-1,-1],test_data_prep[-raw_length,-1])

names(data_prep)<-c("y1","y2","y3","y4",
                    "x1","x2","x3","x4")

fit_length<-round(dim(test_data_prep)[1]*1/2)

data_fit<-data_prep[1:fit_length,]

data_fit<-dplyr::select(data_fit,y1,y2,y3,y4,x1,x2,x3,x4)

data_fit <- data.frame(y4=c(1,rep(0,10)),x4=c(0,1,rep(0,9)))

model_zim<-zim(y1~x1+x2+x3+x4, data=data_fit,control = zim.control(type="ginv"))

model_zeroinfl <- zeroinfl(y4 ~x4,data=data_fit,dist = "poisson")

yhat <- predict(model_zeroinfl,newdata = data.frame(x4=0),type="zero")

test <- Zim.Analysis(Data_Raw = weekly_category_data,Id=10,Multicore = F,Category_Main = "4")

z <- lapply(1,function(x)return(NA))
