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
