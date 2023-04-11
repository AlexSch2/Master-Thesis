#Data preperation

source("data_preperation.R")

test_data_single_group<-test_data%>%
    filter(main_category_id==1)%>%
    dplyr::select(week_date,sold)
   


ingarch_model<-tsglm(test_data_single_group$sold,model = list("past_obs"=2,"past_mean"=2))
summary(ingarch_model)






## Plotting a specified model result
Ingarch.ParameterPlot <- function(Ingarch_Result, Category, Element, Fnct = "idf", Save = TRUE, Plot_Type =
                                    c("default", "histogram")){
  
  
  Plot_Type <- match.arg(Plot_Type)
  Id_all <- unique(Ingarch_Result$result$id)
  Distribution_all <- unique(Ingarch_Result$result$distribution)
  
  
  
  for (Id_RunVariable in Id_all){
    for (Distribution_RunVariable in Distribution_All){
      
      if(Save){
        png(paste("Ingarch_plot_id", Id_RunVariable, Distribution_RunVariable, "category", Category, Plot_Type, Fnct, ".png", sep = ""), 
            height = 15,width = 20,units = "cm",res = 300)
      }
      
      Data_Plot <- Ingarch_Result$models[[paste(Id_RunVariable)]][[Category]]
      Data_Plot <- unlist(lapply(c(1:length(Data_Plot)),function(i)return(do.call(Fnct,list(Data_Plot[[i]][[Element]])))))
      
      if(Plot_Type == "default"){
        
        plot(Data_Plot,type = "l",ylab = paste("Distribution mean",Fnct,sep = " "),
             main = paste("Ingarch_", Distribution_RunVariable, "_id_", Id_RunVariable, sep = ""),
             sub = paste("Category: ", Category, sep = ""))
        
      }
      
      else if (Plot_Type =="histogram") {
        
        hist(Data_Plot,
             xlab = paste("Distribution mean", Fnct, sep = " "), 
             main = paste("Ingarch_", Distribution_RunVariable, "_id_", Id_RunVariable, sep = ""),
             sub = paste("Category: ", Category, sep = ""))
        
      }else {
        
        stop("Enter valid plot type") 
        
      }
      
    }
    
  }
  if(Save){
    dev.off()
  }
  
}