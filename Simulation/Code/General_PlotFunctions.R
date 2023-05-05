
#This function plots the raw timeseries
Plot.Timeseries <- function(Data,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50) {
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data%>% filter(fridge_id == Id_RunVariable)
    
    #Should a Subcategory be plotted
    if(SubCategory){
      Data_Raw <- Data_Raw %>% filter(main_category_id ==as.character(MainCategory))
      Category <- as.character(unique(Data_Raw$sub_category_id))
      Subtitle_Text <- paste("and Main Category ", MainCategory)
    }
    else{
      Category <- as.character(unique(Data_Raw$main_category_id))
      Subtitle_Text <- ""
    }
    
    #Preparing data for plotting
   PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = Category,TakeSubCategory = SubCategory) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
   
    names(PlotData)[1] <- c("date")
    
    #Creating the plot
    Plot <- ggplot(PlotData, aes(x=date,y=valueTrue),col="black")+
      facet_wrap(vars(category),nrow=length(unique(PlotData$category)),scales = "free")+
      geom_line()+
      geom_point()+
      theme(text = element_text(size = TextSize))+
      ggtitle(paste("Timeseries",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")
      
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Raw_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}


#This plots the timeseries with the predictions made by the Ingarch model
Plot.TimeseriesIngarch <- function(Data,IngarchResult,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data %>% filter(fridge_id == Id_RunVariable)
    
    #Should a Subcategory be plotted
    if(SubCategory){
      Data_Raw <- Data_Raw %>% filter(main_category_id ==as.character(MainCategory))
      Category <- as.character(unique(Data_Raw$sub_category_id))
      Subtitle_Text <- paste("and Main Category ", MainCategory)
    }
    else{
      Category <- as.character(unique(Data_Raw$main_category_id))
      Subtitle_Text <- ""
    }
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = c(1,2,3,4),TakeSubCategory = SubCategory) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
    
    names(PlotData)[1] <- c("date")
    
    PlotDataIngarch <- IngarchResult$result %>% filter(id==Id_RunVariable)
    
    #Creating the plot
    MyColour <- setNames(c("blue"),
                         c("ingarch"))
    MyNames <- setNames(c("INGARCH"),
                        c("ingarch"))
    
    Plot <- ggplot(PlotDataIngarch,aes(x=date,y=valuePredict,col=model))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataIngarch$category)),scales = "free")+
      geom_point()+
      geom_line()+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with INGARCH Predictions",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Ingarch_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}

#This function plots the Time series for the ZIM Model 
Plot.TimeseriesZim <- function(Data,ZimResult,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data %>% filter(fridge_id == Id_RunVariable & main_category_id %in% c(3,4))
    
    #Should a Subcategory be plotted
    if(SubCategory){
      Data_Raw <- Data_Raw %>% filter(main_category_id ==as.character(MainCategory))
      Category <- as.character(unique(Data_Raw$sub_category_id))
      Subtitle_Text <- paste("and Main Category ", MainCategory)
    }
    else{
      Category <- as.character(unique(Data_Raw$main_category_id))
      Subtitle_Text <- ""
    }
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = c(3,4),TakeSubCategory = SubCategory) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
    
    names(PlotData)[1] <- c("date")
    
    PlotDataZim <- ZimResult$result %>% filter(id==Id_RunVariable)
    
    #Creating the plot
    MyColour <- setNames(c("green"),
                         c("zim"))
    MyNames <- setNames(c("ZIM"),
                        c("zim"))
    
    Plot <- ggplot(PlotDataZim,aes(x=date,y=valuePredict,col=model))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataZim$category)),scales = "free")+
      geom_point()+
      geom_line()+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with Zim Predictions",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Zim_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}

#This plots the timeseries with the predictions made by the Coda model
Plot.TimeseriesCoda <- function(Data,CodaResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data %>% filter(fridge_id == Id_RunVariable)
    
    Category <- as.character(unique(Data_Raw$main_category_id))
    Subtitle_Text <- ""
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = Category,TakeSubCategory = F) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
    
    names(PlotData)[1] <- c("date")
    
    PlotDataCoda <- CodaResult$result %>% filter(id==Id_RunVariable & category %in% c(1,2,3,4))
    
    #Creating the plot
    MyColour <- setNames(c("red" ),
                         c("coda"))
    MyNames <- setNames(c("CoDA"),
                        c("coda"))
    
    Plot <- ggplot(PlotDataCoda,aes(x=date,y=valuePredict,col=model))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataCoda$category)),scales = "free")+
      geom_point()+
      geom_line()+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with CoDA Predictions",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Coda_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}

#This function compares the ZIM Model with the INGARCH Model on chosen IDs
Plot.TimeseriesIngarchZim <- function(Data,IngarchResult,ZimResult,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50){
  
  for(Id_RunVariable in Id) {
    Data_Raw <- Data %>% filter(fridge_id == Id_RunVariable & main_category_id %in% c(1,2,3,4))
    
    #Should a Subcategory be plotted
    if(SubCategory){
      Data_Raw <- Data_Raw %>% filter(main_category_id ==as.character(MainCategory))
      Category <- as.character(unique(Data_Raw$sub_category_id))
      Subtitle_Text <- paste("and Main Category ", MainCategory)
    }
    else{
      Category <- as.character(unique(Data_Raw$main_category_id))
      Subtitle_Text <- ""
    }
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = c(1,2,3,4),TakeSubCategory = SubCategory) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue") %>%filter(category %in% c(3,4))
    
    names(PlotData)[1] <- c("date")
    
    PlotDataZim_Columns <- names(ZimResult$result)
    PlotDataIngarch_Columns <- names(IngarchResult$result)
    Names_Joined <- intersect(PlotDataIngarch_Columns,PlotDataZim_Columns)
    
    PlotDataZim <- ZimResult$result %>% filter(id==Id_RunVariable)%>% 
      dplyr::select(all_of(Names_Joined))
    PlotDataIngarch <- IngarchResult$result %>% filter(id==Id_RunVariable & category %in% c(3,4))%>% 
      dplyr::select(all_of(Names_Joined))
    
    #Creating the plot
    MyColour <- setNames(c("blue","green"),
                         c("ingarch","zim"))
    MyNames <- setNames(c("INGARCH","ZIM"),
                        c("ingarch","zim"))
    MyShape <- setNames(c(1,4),
                        c("zim","ingarch"))
    
    PlotDataBoth <- rbind(PlotDataZim,PlotDataIngarch)
    
    Plot <- ggplot(PlotDataBoth,aes(x=date,y=valuePredict,col=model,shape=model))+
      facet_wrap(vars(category),nrow=2,scales = "free")+
      geom_point(size=5)+
      geom_line()+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with Zim Predictions",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))+
      scale_shape_manual("Model", values = c(MyShape))
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("ZimIngarch_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}

#This function plots the timeseries with both models 
Plot.TimeseriesCodaIngarch <- function(Data,CodaResult,IngarchResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data%>% filter(fridge_id == Id_RunVariable)
    
    Category <- as.character(unique(Data_Raw$main_category_id))
    Subtitle_Text <- ""
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = Category,TakeSubCategory = F) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
    
    names(PlotData)[1] <- c("date")
    
    PlotDataCoda_Columns <- names(CodaResult$result)
    PlotDataIngarch_Columns <- names(IngarchResult$result)
    Names_Joined <- intersect(PlotDataIngarch_Columns,PlotDataCoda_Columns)
    
    PlotDataCoda <- CodaResult$result %>% filter(id == Id_RunVariable &  category %in% c(1, 2, 3, 4)) %>%
                                          dplyr::select(all_of(Names_Joined))
    
    PlotDataIngarch <- IngarchResult$result %>% filter(id==Id_RunVariable & category %in% c(1, 2, 3, 4))%>% 
                                                dplyr::select(all_of(Names_Joined))
    
    PlotDataBoth <- rbind(PlotDataCoda,PlotDataIngarch)
    
    #Creating the plot
    MyColour <- setNames(c("red", "blue"),
                        c("coda","ingarch"))
    MyNames <- setNames(c("CoDA","INGARCH"),
                        c("coda","ingarch"))
    
    Plot <- ggplot(PlotDataBoth,aes(x=date,y=valuePredict,col=model))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataCoda$category)),scales = "free")+
      geom_point()+
      geom_line()+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with both models",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Both_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}


#This function plots the timeseries with both models and their respective confidence bands
Plot.TimeseriesCodaIngarchPI <- function(Data,CodaResult,IngarchResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data %>% filter(fridge_id == Id_RunVariable)
    
    Category <- as.character(unique(Data_Raw$main_category_id))
    Subtitle_Text <- ""
    
    #Preparing data for plotting
    PlotData <- Data_Raw %>%
      dplyr::select(fridge_id,week_date, main_category_id, sub_category_id, sold) %>%
      Data.Preparation(Category = Category,TakeSubCategory = F) %>% 
      pivot_longer(cols= all_of(Category),names_to="category",values_to="valueTrue")
    
    names(PlotData)[1] <- c("date")
    
    PlotDataCoda_Columns <- names(CodaResult$result)
    PlotDataIngarch_Columns <- names(IngarchResult$result)
    Names_Joined <- intersect(PlotDataIngarch_Columns,PlotDataCoda_Columns)
    
    PlotDataCoda <- CodaResult$result %>% filter(id == Id_RunVariable &  category %in% c(1, 2, 3, 4)) %>%
      dplyr::select(all_of(Names_Joined))
    
    PlotDataIngarch <- IngarchResult$result %>% filter(id==Id_RunVariable & category %in% c(1,2,3,4))%>% 
      dplyr::select(all_of(Names_Joined))
    
    PlotDataBoth <- rbind(PlotDataCoda,PlotDataIngarch)
    
    #Creating the plot
    MyColour <- setNames(c("red", "blue"),
                         c("coda","ingarch"))
    MyNames <- setNames(c("CoDA","INGARCH"),
                        c("coda","ingarch"))
    
    Plot <- ggplot(PlotDataBoth,aes(x=date,y=valuePredict,col=model))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataCoda$category)),scales = "free")+
      geom_point()+
      geom_line(data=PlotDataBoth,aes(x=date,y=valuePredict,col=model))+
      geom_ribbon(aes(x=date,y=valuePredict,col=model,ymin=lowerBound,ymax=upperBound,fill=model),
                  data=PlotDataBoth,alpha=0.15,inherit.aes = F,linetype="dashed")+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with both models",sep=" "),subtitle = paste("Fridge ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Units Sold")+
      xlab("Time")+
      scale_color_manual("Model", values = c(MyColour),labels=c(MyNames))+
      guides(fill="none")
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("BothPI_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}


#This function plots comparision plots for Ingarch,Coda and Zim
Plot.MethodComparision <- function(IngarchResult,CodaResult,ZimResult,Split=F){
  
  
  CodaModelError <- Model.Error(CodaResult$result,Fnct = "Mse") %>% 
                        Model.ErrorOverall(Fnct = "sum",SplitByGroup = F,Category = c(3,4))
  IngarchModelError <- Model.Error(IngarchResult$result,Fnct = "Mse",Category = c(3,4)) %>% 
                        Model.ErrorOverall(Fnct = "sum",SplitByGroup = F,Category = c(3,4))
  ZimModelError <- Model.Error(ZimResult$result,Fnct = "Mse",Category = c(3,4)) %>% 
                        Model.ErrorOverall(Fnct = "sum",SplitByGroup = F,Category = c(3,4))

  CodaModelError_Split <- Model.Error(CodaResult$result,Fnct = "Mse",Category =c(3,4)) %>% 
    Model.ErrorOverall(Fnct = "sum",SplitByGroup = T,Groups = list(3,4))
  
  IngarchModelError_Split <- Model.Error(IngarchResult$result,Fnct = "Mse",Category =c(3,4)) %>% 
    Model.ErrorOverall(Fnct = "sum",SplitByGroup = T,Groups = list(3,4))
  
  ZimModelError_Split <- Model.Error(ZimResult$result,Fnct = "Mse",Category = c(3,4)) %>% 
    Model.ErrorOverall(Fnct = "sum",SplitByGroup = T,Groups = list(3,4))
  
  ModelError_Split <- rbind(CodaModelError_Split,IngarchModelError_Split,ZimModelError_Split)
  
  ModelError <- rbind(CodaModelError,IngarchModelError,ZimModelError)
  
  #Boxplot
  BoxPlot <- ggplot(ModelError,aes(x=model,y=error))+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,5))+
  geom_hline(yintercept =1,linewidth=2)+
  theme(text = element_text(size = 50),axis.text.x = element_text(size=30))+
  ggtitle(paste("Error measures",sep=" "),subtitle = "Standard Zim Model")
  
  ggsave(filename = here("Plots",paste("All_ErrorMeasure_combined_zoomed",ids_save,"zim_standard",".png",sep="")),plot=BoxPlot,height = 15,width = 20)
   

 #Quantile Plot
  length <- ingarch_result$result %>% group_by(id) %>% dplyr::summarise(n=unique(window_baseLength))
  
  TimeSeries_Length <- data.frame(id = unique(IngarchResult$result$id),
                                  length = length$n/
                                    (as.numeric(unique(IngarchResult$result$frame))*as.numeric(unique(IngarchResult$result$history))))
  
  MyColour <- setNames(c("red", "blue","green"),
                               c("coda","ingarch","zim"))
  
  i <- 1
  for(group in c("3","4")){
  
  if(i==1){
    IngarchErrorSorted_Split <- ModelError_Split %>% filter(model=="ingarch" & group == group)%>%arrange(.,error)
    IngarchQuantiles_Split  <- quantile(IngarchErrorSorted_Split $error,na.rm = T)
    
    IngarchErrorSorted_Split$index <- NA
    IngarchErrorSorted_Length <- sum(IngarchErrorSorted_Split$group == group)
    IngarchErrorSorted_Split[IngarchErrorSorted_Split$group == group,]$index <- seq(1:IngarchErrorSorted_Length)
    IngarchErrorSorted_Split <- inner_join(IngarchErrorSorted_Split,TimeSeries_Length,bye ="id")
    
    CodaErrorSorted_Split  <-  ModelError_Split   %>% filter(model=="coda" & group == group)%>%arrange(.,error)
    CodaQuantiles_Split  <- quantile(CodaErrorSorted_Split $error,na.rm = T)
    
    CodaErrorSorted_Split$index <- NA
    CodaErrorSorted_Length <- sum(CodaErrorSorted_Split$group == group)
    CodaErrorSorted_Split[CodaErrorSorted_Split$group == group,]$index <- seq(1:CodaErrorSorted_Length)
    CodaErrorSorted_Split <- inner_join(CodaErrorSorted_Split,TimeSeries_Length,bye ="id")
    
    ZimErrorSorted_Split  <-  ModelError_Split   %>% filter(model=="zim" & group == group)%>%arrange(.,error)
    zimQuantiles_Split  <- quantile(ZimErrorSorted_Split $error,na.rm = T)
    
    ZimErrorSorted_Split$index <- NA
    ZimErrorSorted_Length <- sum(ZimErrorSorted_Split$group == group)
    ZimErrorSorted_Split[ZimErrorSorted_Split$group == group,]$index <- seq(1:ZimErrorSorted_Length)
    ZimErrorSorted_Split <- inner_join(ZimErrorSorted_Split,TimeSeries_Length,bye ="id")
    
  }else{
    IngarchErrorSorted_Length <- sum(IngarchErrorSorted_Split$group == group)
    IngarchErrorSorted_Split[IngarchErrorSorted_Split$group == group,]$index <- seq(1:IngarchErrorSorted_Length)
    
    CodaErrorSorted_Length <- sum(CodaErrorSorted_Split$group == group)
    CodaErrorSorted_Split[CodaErrorSorted_Split$group == group,]$index <- seq(1:CodaErrorSorted_Length)
    
    ZimErrorSorted_Length <- sum(ZimErrorSorted_Split$group == group)
    ZimErrorSorted_Split[ZimErrorSorted_Split$group == group,]$index <- seq(1:ZimErrorSorted_Length)
    
    
    ErrorSorted_Split <- na.omit(rbind(CodaErrorSorted_Split,IngarchErrorSorted_Split,ZimErrorSorted_Split))
    ErrorSorted_Split$group <- as.factor(ErrorSorted_Split$group)
  }
    i <- i+1
 }
    
    Quantile_Help <- ErrorSorted_Split %>% 
      group_by(model,group) %>% 
      dplyr::count() %>% 
      dplyr::group_by(group)%>%
      dplyr::summarise(n=max(n))

    
    Quantile <- c(quantile(c(1:Quantile_Help$n[1])),quantile(c(1:Quantile_Help$n[2])))
    
    
    QuantilesIndex_Split <- data.frame(quant_ind=Quantile,group=rep(c(Quantile_Help$group[1],Quantile_Help$group[2]),each=5))

  QuantPlot_Split <- ggplot(ErrorSorted_Split ,aes(x=index,y=error,colour=model,size=length))+
    facet_wrap(vars(group),nrow=length(unique(IngarchErrorSorted_Split$group)),scales = "free")+
    geom_point()+
    scale_y_continuous(limits=c(0,5))+
    geom_hline(yintercept=1,linewidth=2)+
    # geom_point(data = CodaErrorSorted_Split,aes(x=index,y=error,colour=model),size=3)+
    geom_vline(aes(xintercept=quant_ind),data=QuantilesIndex_Split)+
    theme(text = element_text(size = 50))+
    scale_colour_manual("Legend", values = c(MyColour),aesthetics = "colour")+
    ggtitle(paste("Error measures sorted",sep=" "),subtitle = paste("Window length:",frame,"History:",HistoryLength,sep=" "))

  ggsave(filename = here("Plots",paste("Quantile_Plot_Split",ids_save,"_all_models",".png",sep="")),plot=QuantPlot_Split,height = 15,width = 20)

}

#This function plots the boxplot/quantile plot and histogram of the error measures either by group or in total
Plot.ErrorMeasureSingle <- function(ResultCombined,Variation= "history",Values,Split=T,
                                      Groups=list(Group1=c(1,2),Group2=c(3,4)),Category=c(1,2,3,4),LabelNames){
  
  label.function <- function(String)return(LabelNames[String])
  
  if(Split){
    
  }else{
    
    #Calculating the Error measure
    i <- 1
    for(Variation_RunVariable in Values){
      ResultData <- ResultCombined %>% filter(!!as.symbol(Variation) == Variation_RunVariable)
      ResultModelError_Single <- Model.Error(ResultData,Fnct = "Mse",Category = Category) %>% Model.ErrorOverall(Fnct = "sum",SplitByGroup = F)
      ResultModelError_Single[paste(Variation)] <- Variation_RunVariable
      
      
      if(i==1){
        ResultModelError_All <- ResultModelError_Single
      }
      else{
        ResultModelError_All <- rbind(ResultModelError_All,ResultModelError_Single)
      }
      i <- i+1
    }
    
    
    ModelErrorAll <- rbind(ResultModelError_All)
    ModelErrorAll$model <- as.factor(ModelErrorAll$model)
    if(unique(ModelErrorAll$model) =="coda"){
      levels(ModelErrorAll$model) <- list(CoDA="coda")
    }else if(unique(ModelErrorAll$model) =="ingarch"){
      levels(ModelErrorAll$model) <- list(INGARCH="ingarch")
    }else if(unique(ModelErrorAll$model) == "zim"){
      levels(ModelErrorAll$model) <- list(ZIM = "zim")
    } else{
      stop("Unknown model type.")
    }
    
    #Boxplot
    BoxPlot <- ggplot(ModelErrorAll ,aes(x=!!as.symbol(Variation),y=error))+
      geom_boxplot()+
      scale_y_continuous(limits=c(0,5))+
      geom_hline(yintercept =1,linewidth=2)+
      theme(text = element_text(size = 50),axis.text.x = element_text(size=30))+
      ggtitle(paste("Error Measure Boxplot",sep=" "))+
      scale_x_discrete(labels=LabelNames)+
      xlab(str_to_title(names(Variation)))+
      ylab("Error")
    
    ggsave(filename = here("Plots",paste("ErrorMeasure",unique(ModelErrorAll$model),"_Box",ids_save,"_Variation_",Variation,".png",sep="")),plot=BoxPlot ,height = 15,width = 20)
    
    
    #Quantile Plot
    MyShapes <- c(16,17)
    MyColour <- c("darkgreen","darkorange")
    names(MyShapes) <- Values
    names(MyColour) <- Values
    
    i <- 1
    for(Variation_RunVariable in Values){
      
      Result_Combined_Single <- ResultCombined %>% filter(!!as.symbol(Variation)==Variation_RunVariable)
      length <- Result_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(window_baseLength))
      
      TimeSeries_Length <- Result_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(timeseriesLength))
      
      names(TimeSeries_Length) <- c("id","Length")
      TimeSeries_Length$Length <- as.numeric(TimeSeries_Length$Length)
      
      ResultError_Sorted <- ModelErrorAll %>% filter(!!as.symbol(Variation)==Variation_RunVariable)%>%arrange(.,error)
      ResultQuantiles <- quantile(ResultError_Sorted$error,na.rm = T)
      ResultError_Sorted$index <- seq(1:dim(ResultError_Sorted)[1])
      ResultError_Sorted <- full_join(ResultError_Sorted,TimeSeries_Length,bye ="id")
      
      Quantiles_Index <- data.frame(quant_ind=findInterval(ResultQuantiles,ResultError_Sorted$error))
      
      if(i==1){
        ResultError_Sorted_All <- ResultError_Sorted
      }else{
        ResultError_Sorted_All  <- rbind(ResultError_Sorted_All ,ResultError_Sorted)
      }
      i <- i+1
    }
    
    QuantPlot <- ggplot(ResultError_Sorted_All,aes(x=index,y=error,colour=!!as.symbol(Variation),size=Length,shape=!!as.symbol(Variation)))+
      geom_point()+
      scale_y_continuous(limits=c(0,5))+
      geom_hline(yintercept=1,linewidth=2)+
      geom_vline(aes(xintercept=quant_ind),data=Quantiles_Index)+
      theme(text = element_text(size = 50))+
      ggtitle(paste("Error Measure Quantiles",sep=" "))+
      labs(colour=str_to_title(names(Variation)))+
      scale_shape_manual(names(Variation), values = c(MyShapes),labels=LabelNames)+
      scale_colour_manual(names(Variation), values = c(MyColour),labels=LabelNames)+
      guides(shape = guide_legend(override.aes = list(size = 5)))+
      guides(color = guide_legend(override.aes = list(size = 5)))+
      xlab("Index")+
      ylab("Error")
    
    ggsave(filename = here("Plots",paste("ErrorMeasure",unique(ModelErrorAll$model),"_Quant",ids_save,"_Variation_",Variation,".png",sep="")),plot=QuantPlot,height = 15,width = 20)
    
    #Histogram 
    MyLinetype <- c("solid", "dashed")
    names(MyLinetype) <- Values
    
    ModelErrorAll_Median <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation)) %>% summarise(Median=median(error,na.rm=T))
    ModelErrorAll_Mean <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation)) %>% summarise(Mean=mean(error,na.rm=T))
    
    ModelErrorAll_MM <- full_join(ModelErrorAll_Mean,ModelErrorAll_Median,by=unname(Variation)) %>% pivot_longer(cols=c("Mean","Median"),names_to="Type",
                                                                                                            values_to = "value")
    
    HistPlot <- ggplot(ModelErrorAll,aes(x=error,colour=!!as.symbol(Variation),fill=!!as.symbol(Variation),linetype=!!as.symbol(Variation)))+
      geom_histogram(bins=100,position = "identity",alpha=0.3,linewidth=1)+
      scale_x_continuous(limits=c(0,8))+
      scale_y_continuous(limits=c(0,12))+
      geom_vline(xintercept = 1,linewidth=2)+
#      geom_vline(aes(xintercept=value,colour=!!as.symbol(Variation),linetype=Type),data = ModelErrorAll_MM ,linewidth=2)+
      theme(text = element_text(size = 50))+
      ggtitle(paste("Error Measure Histogram",sep=" "))+
      scale_linetype_manual(names(Variation), values = c(MyLinetype),labels=LabelNames)+
      scale_colour_manual(names(Variation), values = c(MyColour),labels=LabelNames)+
      scale_fill_manual(names(Variation), values = c(MyColour),labels=LabelNames)+
      labs(colour=str_to_title(names(Variation)),fill=str_to_title(names(Variation)))+
      guides(linetype = guide_legend(override.aes = list(size = 5)))+
      ylab("Count")+
      xlab("Error")
    
    ggsave(filename = here("Plots",paste("ErrorMeasure",unique(ModelErrorAll$model),"_Histogram",ids_save,"_Variation_",Variation,".png",sep="")),plot=HistPlot,height = 15,width = 20)
    
  }
}



#This function plots the boxplot/quantile plot and histogram of the error measures either by group or in total
Plot.ErrorMeasureCombined <- function(CodaCombined,IngarchCombined,Variation= "history",Values,Split=T,
                                 Groups=list(Group1=c(1,2),Group2=c(3,4)),LabelNames){
  
  label.function <- function(String)return(LabelNames[String])
  
  if(Split){

  }else {
    
    #Calculating the Error measure
    i <- 1
    for(Variation_RunVariable in Values){
      CodaData <- Coda_Combined %>% filter(!!as.symbol(Variation) == Variation_RunVariable)
      CodaModelError_Single <- Model.Error(CodaData,Fnct = "Mse") %>% Model.ErrorOverall(Fnct = "sum",SplitByGroup = F)
      CodaModelError_Single[paste(Variation)] <- Variation_RunVariable
      
      IngarchData <- Ingarch_Combined %>% filter(!!as.symbol(Variation) == Variation_RunVariable)
      IngarchModelError_Single <- Model.Error(IngarchData,Fnct = "Mse") %>% Model.ErrorOverall(Fnct = "sum",SplitByGroup = F)
      IngarchModelError_Single[paste(Variation)] <- Variation_RunVariable
      
      if(i==1){
        CodaModelError_All <- CodaModelError_Single
        IngarchModelError_All <- IngarchModelError_Single
      }
      else{
        CodaModelError_All <- rbind(CodaModelError_All,CodaModelError_Single)
        IngarchModelError_All <- rbind(IngarchModelError_All,IngarchModelError_Single)
      }
      i <- i+1
    }
    ModelErrorAll <- rbind(CodaModelError_All,IngarchModelError_All)
    ModelErrorAll$model <- as.factor(ModelErrorAll$model)
    levels(ModelErrorAll$model) <- list(CoDA = "coda", INGARCH = "ingarch")
  
    #Boxplot
    BoxPlot <- ggplot(ModelErrorAll ,aes(x=model,y=error))+
      facet_wrap(vars(!!as.symbol(Variation)),ncol=length(Values),scales = "fixed",labeller = as_labeller(label.function))+
      geom_boxplot()+
      scale_y_continuous(limits=c(0,5))+
      geom_hline(yintercept =1,linewidth=2)+
      theme(text = element_text(size = 50),axis.text.x = element_text(size=30))+
      ggtitle(paste("Error Measure Boxplot",sep=" "))+
      xlab("Model")+
      ylab("Error")
    
      ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Box",ids_save,"_Variation_",Variation,".png",sep="")),plot=BoxPlot ,height = 15,width = 20)
  
       
    #Quantile Plot
    MyColour <- setNames(c("red", "blue"),
                         c("CoDA","INGARCH"))
    MyLinetype <- setNames(c("solid", "dashed"),
                           c("CoDA","INGARCH"))
    MyShape <- setNames(c(16,17),
                        c("CoDA","INGARCH"))
    i <- 1
    for(Variation_RunVariable in Values){

        Ingarch_Combined_Single <- Ingarch_Combined %>% filter(!!as.symbol(Variation)==Variation_RunVariable)
        length <- Ingarch_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(window_baseLength))

        TimeSeries_Length <- Ingarch_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(timeseriesLength))

        names(TimeSeries_Length) <- c("id","Length")
        TimeSeries_Length$Length <- as.numeric(TimeSeries_Length$Length)

        IngarchError_Sorted <- ModelErrorAll %>% filter(model=="INGARCH" & !!as.symbol(Variation)==Variation_RunVariable)%>%arrange(.,error)
        IngarchError_Sorted$index <- seq(1:dim(IngarchError_Sorted)[1])
        IngarchError_Sorted <- full_join(IngarchError_Sorted,TimeSeries_Length,bye ="id")

        CodaError_Sorted <- ModelErrorAll  %>% filter(model=="CoDA" & !!as.symbol(Variation)==Variation_RunVariable)%>%arrange(.,error)
        CodaError_Sorted$index <- seq(1:dim(CodaError_Sorted)[1])
        CodaError_Sorted <- full_join(CodaError_Sorted,TimeSeries_Length,bye ="id")

        Quantile <- quantile(c(1:max(dim(IngarchError_Sorted)[1],dim(CodaError_Sorted)[1])))
        Quantiles_Index <- data.frame(quant_ind=Quantile)
        Quantiles_Index[Variation] <- Variation_RunVariable
        if(i==1){
          CodaError_Sorted_All <- CodaError_Sorted
          IngarchError_Sorted_All <- IngarchError_Sorted
          Quantiles_Index_All <- Quantiles_Index
        }else{
          CodaError_Sorted_All  <- rbind(CodaError_Sorted_All ,CodaError_Sorted)
          IngarchError_Sorted_All <- rbind(IngarchError_Sorted_All,IngarchError_Sorted)
          Quantiles_Index_All <- rbind(Quantiles_Index_All,Quantiles_Index)
        }
        i <- i+1
    }
    Result_All <- rbind(CodaError_Sorted_All,IngarchError_Sorted_All)

    QuantPlot <- ggplot(Result_All,aes(x=index,y=error,colour=model,size=Length,shape=model))+
        facet_wrap(vars(!!as.symbol(Variation)),nrow=length(Values),scales = "free",labeller = as_labeller(label.function))+
        geom_point()+
        scale_y_continuous(limits=c(0,5))+
        scale_x_continuous(limits=c(0,max(Quantiles_Index_All$quant_ind)))+
#       geom_point(data = CodaError_Sorted_All,aes(x=index,y=error,colour=model,size=Length,shape=model))+
        geom_hline(yintercept=1,linewidth=2)+
        geom_vline(aes(xintercept=quant_ind),data=Quantiles_Index_All)+
        theme(text = element_text(size = 50))+
        scale_color_manual("Model", values = c(MyColour))+
        scale_shape_manual("Model", values = c(MyShape))+
        ggtitle(paste("Error Measure Quantiles",sep=" "))+
        guides(shape = guide_legend(override.aes = list(size = 5)))+
        guides(color = guide_legend(override.aes = list(size = 5)))+
        xlab("Index")+
        ylab("Error")
      
        ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Quant",ids_save,"_Variation_",Variation,".png",sep="")),plot=QuantPlot,height = 15,width = 20)
        
    #Histogram 
        ModelErrorAll_Median <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation),model) %>% summarise(Median=median(error,na.rm=T))
        ModelErrorAll_Mean <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation),model) %>% summarise(Mean=mean(error,na.rm=T))
        
        ModelErrorAll_MM <- full_join(ModelErrorAll_Mean,ModelErrorAll_Median,by=c("model",Variation)) %>% pivot_longer(cols=c("Mean","Median"),names_to="Type",values_to = "value")
        
        HistPlot <- ggplot(ModelErrorAll,aes(x=error,colour=model,fill=model,linetype=model))+
          facet_wrap(vars(!!as.symbol(Variation)),nrow=length(Values),scales = "free",labeller = as_labeller(label.function))+
          geom_histogram(bins=100,position = "identity",alpha=0.3,linewidth=1)+
          scale_x_continuous(limits=c(0,8))+
          scale_y_continuous(limits=c(0,12))+
          geom_vline(xintercept = 1,linewidth=2)+
#          geom_vline(aes(xintercept=value,colour=model,linetype=Type),data = ModelErrorAll_MM ,linewidth=2)+
          scale_color_manual("Model", values = c(MyColour))+
          scale_fill_manual("Model", values = c(MyColour))+
          scale_linetype_manual("Model", values = c(MyLinetype))+
          theme(text = element_text(size = 50))+
          ggtitle(paste("Error Measure Histogram",sep=" "))+
          guides(linetype = guide_legend(override.aes = list(size = 5)))+
          xlab("Error")+
          ylab("Count")
        
          ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Histogram",ids_save,"_Variation_",Variation,".png",sep="")),plot=HistPlot,height = 15,width = 20)
  }
}
