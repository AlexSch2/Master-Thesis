x<-matrix(c(1:9),nrow=3)

<<<<<<< HEAD
y<-pivotCoord(x)
=======
for(index in 1:length(data_prepared_windows_test)){
  
x<-data_notransf_test_windows[[index]]$fitting[max(which(!is.na(data_notransf_test_windows[[index]]$fitting$tsum))),"tsum"]%>%as.numeric()
cat(index,":",x,"\n")
}
>>>>>>> 594fb9bc7157f30580dfea4e9564fbf5272b6bd9

pivotCoordInv(y)*c(12,15,18)
