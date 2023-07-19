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
test_data<-weekly_category_data%>%filter(fridge_id==10)
View(test_data)

test_data_prep <- weekly_category_data %>%
  filter(fridge_id == 10) %>%
  dplyr::select(week_date, main_category_id, sold) %>%
  arrange(week_date) %>% Data.Preparation()

names(test_data_prep) <- c("week_date","y1","y2","y3","y4")

model_inar <- estimate_zinarp(test_data_prep$y1,p=1)
model_inar2 <- EST_ZINAR(test_data_prep$y1,model="inar",innovation = "Po")


raw_length<-dim(test_data_prep)[1]

data_prep<-cbind(test_data_prep[-1,-1],test_data_prep[-raw_length,-1])

names(data_prep)<-c("y1","y2","y3","y4",
                    "x1","x2","x3","x4")

model_zeroinfl <- zeroinfl(cbind(y3,y4)~x3+x4,data=data_prep,dist = "poisson")

yhat <- predict(model_zeroinfl,newdata = data.frame(week_date="2022-07-18",data.frame(x3=0,x4=0)))



y <- c(1:15)
ybshift <- bshift(y)


test<-results %>% group_by(model)%>%Model.Error(Fnct = "Mse",Category = c(3,4)) %>% 
  Model.ErrorOverall(Fnct = "sum",SplitByGroup = F,Category = c(3,4))



Model <- inarbayes_result$model[["8"]][["1"]]$window19
Alpha <- Model$alpha
Lambda <- Model$lambda
m <- length(Alpha)
TimeSeriesValue_LastKnown <- inarbayes_result$result %>% filter(id==8 & category == 1 & window ==19) %>%select(valueLastKnown)

ValuePredict <- TimeSeriesValue_LastKnown*(1/m *sum(Alpha^PredictionStep)) + (1/m*sum((1-Alpha^PredictionStep)/(1-Alpha)*Lambda))




x <- ccomp(test_data_prep[,-1],SZ=TRUE)
is.SZ(x)

z <- unclass(as.vector(test_data_prep[,2]))

model <- tsglm(ts=z[[1]],model=list(past_obs=1,past_mean=1),link="identity")
pred<-predict(model,n.ahead=1,type="shortest")
pred$pred


y <- z[[1]] - mean(z[[1]])
x.g <- garchFit(~garch(1,1),y,include.mean = F)
summary(x.g)
predict(x.g)


data(Pigs)
Pigs.GBM <- cmultRepl(Pigs,output = "p-counts")

a <- "B"
x <- 3
switch(a,
       "A"= stopifnot(x>10),
       "B"= stopifnot(x<5))

X <- matrix(c(26.91,8.08,12.59,31.58,6.45,14.39,
              39.73,26.20,0.00,15.22,6.80,12.05,
              10.76,31.36,0.00,12.74,31.34,6.70,
              10.85,46.40,31.89,10.86,0.00,0.00,
              7.57,11.35,30.24,6.39,13.65,30.80,
              38.09,7.62,23.68,9.70,20.91,0.00,
              27.67,7.15,13.05,32.04,6.54,13.55,
              44.41,15.04,7.95,0.00,10.82,21.78,
              11.50,30.33,6.85,13.92,30.82,6.58,
              19.04,42.59,0.00,38.37,0.00,0.00),byrow=TRUE,ncol=6)

X_lrEM <- lrEM(X,label=0,dl=rep(1,6),ini.cov="multRepl")
