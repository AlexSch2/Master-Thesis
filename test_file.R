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
