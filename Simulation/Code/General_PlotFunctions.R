
#This function plots the raw timeseries
Plot.Timeseries <- function(Data_Raw,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50) {
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data_Raw %>% filter(fridge_id == Id_RunVariable)
    
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
Plot.TimeseriesIngarch <- function(Data_Raw,IngarchResult,Id,Save=T,SubCategory=F,MainCategory=1,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data_Raw %>% filter(fridge_id == Id_RunVariable)
    
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


#This plots the timeseries with the predictions made by the Coda model
Plot.TimeseriesCoda <- function(Data_Raw,CodaResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data_Raw %>% filter(fridge_id == Id_RunVariable)
    
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


#This function plots the timeseries with both models 
Plot.TimeseriesCodaIngarch <- function(Data_Raw,CodaResult,IngarchResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data_Raw %>% filter(fridge_id == Id_RunVariable)
    
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
Plot.TimeseriesCodaIngarchPI <- function(Data_Raw,CodaResult,IngarchResult,Id,Save=T,TextSize=50){
  
  for(Id_RunVariable in Id) {
    
    Data_Raw <- Data_Raw %>% filter(fridge_id == Id_RunVariable)
    
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



#This function plots the boxplot/quantile plot and histogram of the error measures either by group or in total

Plot.ErrorMeasureCombined <- function(CodaCombined,IngarchCombined,Variation= "history",Values,Split=T,
                                 Groups=list(Group1=c(1,2),Group2=c(3,4))){
  
  if(Split){

  }else{
    
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
      facet_wrap(vars(!!as.symbol(Variation)),ncol=length(Values),scales = "fixed")+
      geom_boxplot()+
      scale_y_continuous(limits=c(0,5))+
      geom_hline(yintercept =1,linewidth=2)+
      theme(text = element_text(size = 50),axis.text.x = element_text(size=30))+
      ggtitle(paste("Error measure Boxplot",sep=" "))+
      xlab("Models")+
      ylab("Errors")
    
      ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Box",ids_save,"_Variation_",Variation,".png",sep="")),plot=BoxPlot ,height = 15,width = 20)
      
      
    #Quantile Plot
    MyColour <- setNames(c("red", "blue"),
                         c("CoDA","INGARCH"))
      
    i <- 1
    for(Variation_RunVariable in Values){
        
        Ingarch_Combined_Single <- Ingarch_Combined %>% filter(!!as.symbol(Variation)==Variation_RunVariable)
        length <- Ingarch_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(window_baseLength))
        
        TimeSeries_Length <- Ingarch_Combined_Single%>% group_by(id) %>% dplyr::summarise(n=unique(timeseriesLength))
        
        names(TimeSeries_Length) <- c("id","Length")
        TimeSeries_Length$Length <- as.numeric(TimeSeries_Length$Length)
        
        
        IngarchError_Sorted <- ModelErrorAll %>% filter(model=="INGARCH" & !!as.symbol(Variation)==Variation_RunVariable)%>%arrange(.,error)
        IngarchQuantiles <- quantile(IngarchError_Sorted$error,na.rm = T)
        IngarchError_Sorted$index <- seq(1:dim(IngarchError_Sorted)[1])
        IngarchError_Sorted <- full_join(IngarchError_Sorted,TimeSeries_Length,bye ="id")
        
        Quantiles_Index <- data.frame(quant_ind=findInterval(IngarchQuantiles,IngarchError_Sorted$error))
        
        CodaError_Sorted <- ModelErrorAll  %>% filter(model=="CoDA" & !!as.symbol(Variation)==Variation_RunVariable)%>%arrange(.,error)
        CodaQuantiles <- quantile(CodaError_Sorted$error,na.rm = T)
        CodaError_Sorted$index <- seq(1:dim(CodaError_Sorted)[1])
        CodaError_Sorted <- full_join(CodaError_Sorted,TimeSeries_Length,bye ="id")
        
        if(i==1){
          CodaError_Sorted_All <- CodaError_Sorted
          IngarchError_Sorted_All <- IngarchError_Sorted
        }else{
          CodaError_Sorted_All  <- rbind(CodaError_Sorted_All ,CodaError_Sorted)
          IngarchError_Sorted_All <- rbind(IngarchError_Sorted_All,IngarchError_Sorted)
        }
        i <- i+1
      }
      
      
    QuantPlot <- ggplot(IngarchError_Sorted_All,aes(x=index,y=error,colour=model,size=Length))+
        facet_wrap(vars(!!as.symbol(Variation)),nrow=length(Values),scales = "free")+
        geom_point()+
        scale_y_continuous(limits=c(0,5))+
        geom_point(data = CodaError_Sorted_All,aes(x=index,y=error,colour=model,size=Length))+
        geom_hline(yintercept=1,linewidth=2)+
        geom_vline(aes(xintercept=quant_ind),data=Quantiles_Index)+
        theme(text = element_text(size = 50))+
        scale_color_manual("Model", values = c(MyColour))+
        ggtitle(paste("Error Measure sorted",sep=" "))
      
        ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Quant",ids_save,"_Variation_",Variation,".png",sep="")),plot=QuantPlot,height = 15,width = 20)
        
        
    #Histogram 
        ModelErrorAll_Median <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation),model) %>% summarise(Median=median(error,na.rm=T))
        ModelErrorAll_Mean <- ModelErrorAll %>% dplyr::group_by(!!as.symbol(Variation),model) %>% summarise(Mean=mean(error,na.rm=T))
        
        ModelErrorAll_MM <- full_join(ModelErrorAll_Mean,ModelErrorAll_Median,by=c("model",Variation)) %>% pivot_longer(cols=c("Mean","Median"),names_to="Type",values_to = "value")
        
        HistPlot <- ggplot(ModelErrorAll,aes(x=error,colour=model,fill=model))+
          facet_wrap(vars(!!as.symbol(Variation)),nrow=length(Values),scales = "free")+
          geom_histogram(bins=100,position = "identity",alpha=0.3,linewidth=1)+
          scale_x_continuous(limits=c(0,8))+
          scale_y_continuous(limits=c(0,12))+
          geom_vline(xintercept = 1,linewidth=2)+
          geom_vline(aes(xintercept=value,colour=model,linetype=Type),data = ModelErrorAll_MM ,linewidth=2)+
          scale_color_manual("Legend", values = c(MyColour ))+
          scale_fill_manual("Legend", values = c(MyColour ))+
          theme(text = element_text(size = 50))+
          ggtitle(paste("Error Measure Histogram",sep=" "))
        
          ggsave(filename = here("Plots",paste("ErrorMeasureCombined_Histogram",ids_save,"_Variation_",Variation,".png",sep="")),plot=HistPlot,height = 15,width = 20)

  
  
  }

}
