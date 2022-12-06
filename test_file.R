#All sorts of testing code

data(syph)
plot(1:length(syph$a33),syph$a33,type="l")
count<-syph$a33
ar1<-bs(count>0)
trend<-1:length(count)/1000
m<-zim(count~ar1+trend|trend)

test_function<-function(ic){
  IC<-match.fun(ic)
  model<-VAR(fitting_data[,-1])
  return(IC(model))
}
test_function("AIC")

##coda fitting

#test<-coda.tuning(data=data_prepared_all_plus1,timeframes = c(50,60,70),lag.max = 6,information_criteria_lag="AIC",
#                  information_criteria_frame = "prediction_error",
#                  multicore = T,n_cores = 2)
#test


frame<-10
prediction_error_step<-1

data_time_windows<-windows(data_prepared_all_plus1,frame,method="overlapping",
                           prediction_error_step = prediction_error_step)

fitting_data<-data_time_windows[[1]]$fitting[,-1]
future_data<-data_time_windows[[1]]$prediction_value[,-1]

data_prepared_all_plus1<-coda.data.preperation(test_data)

model<-VAR(data_prepared_all_plus1[,-1])

predicted_values<-predict(model,data_prepared_all_plus1)

predicted_data<-pivotCoordInv(matrix(c(predict(model,fitting_data,n.ahead = 2)$fcst$X1[,1],
                                           predict(model,fitting_data,n.ahead = 2)$fcst$X2[,1],
                                           predict(model,fitting_data,n.ahead = 2)$fcst$X3[,1]),ncol=3,byrow=T))
View(predicted_data)

predicted_data<-c(predicted_data$fcst$X1[1],predicted_data$fcst$X2[1],predicted_data$fcst$X3[1])
sum((future_data-predicted_data)^2)

test<-apply(sapply(1:3,function(i)return(c(k=i,k_1=i+1))),1,mean)
test


coda_all_models<-list(all=list(model=model_all_plus1,data_type="all"),
                      only0=list(model=model_only0_plus1,data_type="only0"))


apply(sapply(c(1:3),function(i,drucken){if(drucken==TRUE)i+3},drucken=T),1,mean)

testsframe<-data.frame(id=c(1:10),x=letters[1:10])
for(i in 1:4){
  print(i)
  y<-data%>%filter(fridge_id==i)
  print(dim(y))
}

test<-coda.data.preperation(test_data,zero_handling = "zero")
View(test)


lapply(c(1:dim(combinations_zerohandling_tspace)[1]),function(x){
  paste("\n zero handling:",combinations_zerohandling_tspace$Var1[x],"Tspace:",combinations_zerohandling_tspace$Var2[x],sep=" ")
})


