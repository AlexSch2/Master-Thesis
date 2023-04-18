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
      ggtitle(paste("Timeseries",sep=" "),subtitle = paste("Fride ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Value")+
      xlab("Time")
      
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Raw_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}



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
    Plot <- ggplot(PlotDataIngarch,aes(x=date,y=valuePredict))+
      facet_wrap(vars(category),nrow=length(unique(PlotDataIngarch$category)),scales = "free")+
      geom_point(col="blue")+
      geom_line(col="blue")+
      geom_line(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      geom_point(data=PlotData,aes(x=date,y=valueTrue),col="black",inherit.aes = F)+
      theme(text = element_text(size =TextSize))+
      ggtitle(paste("Timeseries with Ingarch Predictions",sep=" "),subtitle = paste("Fride ID",Id_RunVariable,Subtitle_Text,sep=" "))+
      ylab("Value")+
      xlab("Time")
    
    
    #Saving of the plot
    if(Save){
      ggsave(filename = here("Plots",paste("Ingarch_Timeseries_ID",Id_RunVariable,".png",sep="")),plot=Plot,height = 15,width = 20)
    }
  }
}



