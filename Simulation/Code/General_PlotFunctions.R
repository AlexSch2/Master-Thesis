
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



#This function plots the boxplots of the error measures either by group or in total

Plot.ErrorMeasureBox <- function(CodaResult,IngarchResult,Split=T,Groups=list(1,2,3,4)){
  
  if(Split){
    
    coda_model_error_split <-Model.Error(CodaResult, Fnct = "Mse")%>% 
      Model.ErrorOverall(Fnct = "sum", SplitByGroup = T)
    
    ingarch_model_error_split <- Model.Error(IngarchResult,Fnct = "Mse",Category = as.numeric(unique(IngarchResult$result$category))) %>% 
      Model.ErrorOverall(Fnct = "sum",
                         SplitByGroup = T,
                         Groups = Groups)
    
    model_error_split <-rbind(coda_model_error_split, 
                              ingarch_model_error_split)
    
    psb_zoomed<-ggplot(model_error,aes(x=model,y=error))+
      geom_boxplot()+
      scale_y_continuous(limits=c(0,3))+
      geom_hline(yintercept =1,linewidth=2)+
      theme(text = element_text(size = text_size),axis.text.x = element_text(size=30))+
      ggtitle(paste("Error measures",sep=" "),subtitle = paste("Window length:",frame, "History:",HistoryLength,sep=" "))
    
    if(save_plots){
      ggsave(filename = here("Plots",paste("Both_ErrorMeasure_combined_zoomed",ids_save,"_histlgth",HistoryLength,"win_lgth",frame,".png",sep="")),plot=psb_zoomed,height = 15,width = 20)
    }
    
  }
  coda_model_error <- Model.Error(coda_result,Fnct = "Mse") %>% Model.ErrorOverall(Fnct = "sum",SplitByGroup = F)
  ingarch_model_error <- Model.Error(ingarch_result,Fnct = "Mse",Category = unique(ingarch_result$result$category)) %>% Model.ErrorOverall(Fnct = "sum",SplitByGroup = F,Category = as.numeric(unique(ingarch_result$result$category)))
  model_error <- rbind(coda_model_error,ingarch_model_error)
  
  

  
}