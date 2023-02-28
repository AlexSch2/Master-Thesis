ids <- unique(coda_result$results$id)
prediction_error_step <- 1
frame <- unique(coda_result$results$window_length_base)
window_method <- unique(coda_result$results$window_method)
save_results <- T

#Ingarch specifications
distr<-unique(ingarch_result$results$distribution)
zero_handling_ingarch <- unique(ingarch_result$results$zero_handling)
external <- unique(ingarch_result$results$external)
if(external){
  external_plot <-"with external factors"
  external_save <- "_ext_TRUE"
}else {
  external_plot <- ""
  external_save <- ""
}
past_means <- unique(ingarch_result$results$past_mean)
past_obs <- unique(ingarch_result$results$past_obs)
ingarch_settings <- expand_grid(past_means,past_obs)


#Coda specifications
#zero_handling_coda<-unique(coda_result$results$zero_handling)
tspace<-T
take_log<-T
one_vs_all<-T

if(!identical(ids,unique(ingarch_result$results$id)) || 
   frame != unique(ingarch_result$results$window_length_base) ||
   window_method != unique(ingarch_result$results$window_method)) {
  stop("Ingarch and Coda base specifications differ")
}


#Coda error measure plots
coda_model_error <- model.error(coda_result,fnct = "mse") %>% model.error.overall(fnct = "sum")
ingarch_model_error <- model.error(ingarch_result,fnct = "mse") %>% model.error.overall(fnct = "sum")

coda_model_error
ingarch_model_error

model_error <- rbind(coda_model_error,ingarch_model_error)
model_error$id <- as.factor(model_error$id)

ps<-ggplot(model_error,aes(x=id,y=error,col=model))+
  geom_point(size=8)+
  theme(text = element_text(size = text_size))+
  geom_hline(yintercept = 1)+
  scale_y_continuous(limits = c(0, 1.5))+
  ggtitle(paste("Fridge error measure",external_plot,sep=" "),subtitle = paste("Base window length:",frame,",pm:",past_means,"po:",past_obs,sep=" "))

if(save_plots){
  ggsave(filename = here("Plots",paste("Fridge_error_measure",paste(unique(model_error$id),collapse="-"),"_ing_pm",past_means,"_po",past_obs,external_save,"_win_lgth",frame,".png",sep="")),plot=ps,height = 15,width = 20)
}

for (FID in ids){

  #P########## Preparing Data
  plot_data_coda<-coda_result$results%>%
    dplyr::filter(category %in% c(1,2,3,4), id==FID)%>%
    dplyr::select(!c(naive_predicted_value,prediction_error_naive))

  all_dates_and_categories<-plot_data_coda[,c("category","prediction_date")]

  plot_data_ingarch <- ingarch_result$results %>% dplyr::filter(id==FID)
  plot_data_ingarch<-full_join(plot_data_ingarch,all_dates_and_categories)
  plot_data_ingarch$model<-"ingarch"

  if(one_vs_all) {
    plot_data <-
      bind_rows(plot_data_coda, plot_data_ingarch) %>% dplyr::select(!c(distribution, pivot_group))
  } else{
    plot_data <-
      bind_rows(plot_data_coda, plot_data_ingarch) %>% dplyr::select(!c(distribution))
  }


  data_raw <- weekly_category_data %>%
    filter(fridge_id == FID&
             main_category_id %in% c(1, 2, 3, 4)) %>%
    dplyr::select(week_date, main_category_id, sold) %>%
    data.preparation() %>%
    pivot_longer(cols=c("1","2","3","4"),names_to="category",values_to="true_value")
  names(data_raw)[1] <- c("prediction_date")


 ############ Comparing Timeseries with PI
  my_color <- setNames(c("red", "blue"),
                       c("coda","ingarch"))

  p1<-ggplot(plot_data,aes(x=prediction_date,y=predicted_value,col=model))+
    facet_wrap(vars(category),nrow=length(unique(plot_data$category)),scales = "free")+
    geom_point(size=1)+
    geom_ribbon(aes(x=prediction_date,y=predicted_value,col=model,ymin=lower_bound,ymax=upper_bound,fill=model),
                data=plot_data,alpha=0.15,inherit.aes = F,linetype="dashed")+
    geom_line(data=plot_data,
              aes(x=prediction_date,y=predicted_value,col=model))+
    geom_line(data=data_raw,aes(x=prediction_date,y=true_value),col="black",inherit.aes = F)+
    geom_point(data=data_raw,aes(x=prediction_date,y=true_value),col="black",inherit.aes = F)+
    scale_color_manual("Legend", values = c(my_color))+
    ggtitle(paste("Actual Timeseries vs. predicted ",sep=" "),subtitle = paste("Fridge ID:",FID,",base window length:",frame,",pm:",past_means,"po:",past_obs,sep=" "))+
    theme(text = element_text(size = text_size))

  if(save_plots){
    ggsave(filename = here("Plots",paste("ts_both_id_",FID,"_pi","_ing_pm",past_means,"_po",past_obs,external_save,"_win_lgth",frame,".png",sep="")),plot=p1,height = 15,width = 20)
  }
 ########## Normed prediction errors over time
    p3 <- ggplot(plot_data,aes(x=prediction_date,y=prediction_error_normed,col=model))+
      facet_wrap(vars(category),nrow=length(unique(plot_data$category)),scales = "free")+
      geom_point(size=1.5,aes(shape=model))+
      geom_line()+
      theme(text = element_text(size = text_size))+
      geom_line(aes(y=0),col="black")+
      ggtitle(paste("Normed prediction error over time ",sep=" "),subtitle = paste("Fridge ID:",FID,",base window length:",frame,",pm:",past_means,"po:",past_obs,sep=" "))+
      scale_color_manual("Legend", values = c(my_color))

    if(save_plots){
      ggsave(filename = here("Plots",paste("pred_err_norm_id",FID,"_ing_pm",past_means,"_po",past_obs,external_save,"_win_lgth",frame,".png",sep="")),plot=p3,height = 15,width = 20)

    }

  ############# Normed prediction error box plots

    p4<-ggplot(plot_data,aes(x=model,y=prediction_error_normed))+
      facet_wrap(vars(category),ncol=length(unique(plot_data$category)),scales = "fixed")+
      geom_boxplot()+
      ggtitle("Normed prediction error per method and category",subtitle = paste("Fridge ID:",FID,",base window length:",frame,",pm:",past_means,"po:",past_obs,sep=" "))+
      theme(text = element_text(size = text_size))+
      geom_hline(yintercept = 0,col="green")

    if(save_plots){
      ggsave(filename = here("Plots",paste("box_pred_err_norm_id",FID,"_ing_pm",past_means,"_po",past_obs,external_save,"_win_lgth",frame,".png",sep="")),plot=p4,height = 15,width = 20)

    }



    ##### Cumulated MSEs

    plot_mse_cum <- mse.cumulated(plot_data)
    plot_mse_cum$window <- factor(as.character(plot_mse_cum$window),levels=unique(plot_mse_cum$window))


    p8 <- ggplot(plot_mse_cum, aes(y=mse_cum,x=window,group=model,col=model))+
      facet_wrap(vars(category),nrow=length(unique(plot_mse_cum$category)),scales = "free")+
      geom_line()+
      geom_point(size=1.5)+
      theme(text = element_text(size = 30),axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=1))+
      ggtitle(paste("Cumulated MSE from right to left",sep=" "),subtitle = paste("Fridge ID:",FID,",base window length:",frame,",pm:",past_means,"po:",past_obs,sep=" "))

    p8

    if(save_plots){
      ggsave(filename = here("Plots",paste("mse_cum",FID,"_ing_pm",past_means,"_po",past_obs,external_save,"_win_lgth",frame,".png",sep="")),plot=p8,height = 15,width = 20)

    }

}








