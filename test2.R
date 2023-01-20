models<-lapply(c(1:56),function(index){
  
  fitting_data<-data_prepared_windows_test[[index]]$fitting[,-1]
  prediction_date<-data_prepared_windows_test[[index]]$prediction_value[1,1]
  
  model<-VAR(fitting_data,p=1)
  
  
  return(model)
})

tsums<-median(data_prepared_notransf$tsum,na.rm=TRUE)

determinants<-vector("numeric",56)

for(i in 1:56){
  model<-models[[i]]
  print(Bcoef(model))
  determinants[i]<-det(Bcoef(model)[,-3])
}

plot(determinants)
Bcoef(model28)
Bcoef(model29)

B<-Bcoef(model29)
lasty<-as.matrix(data_prepared_windows_test[[29]]$fitting[20,-1])
Z <- c(lasty, 1)

x<-B%*%Z
exp(x[1]*sqrt(1/2))*57
