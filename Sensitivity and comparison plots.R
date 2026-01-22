#In here we make the plots with all of the previous models and save them

#First call in all the needed packages
library(ggplot2)
library(readxl)
library(tidyverse)
library(gridExtra)


#We focus on ESAFR first
Main_model<-read_excel("./BESFR_estimates global_south.xlsx",sheet = "BESASFR")
  
model1<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_a.xlsx",sheet = "BESASFR")

model2<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx",sheet = "BESASFR")

model3<-read_excel("./BESFR_data qaul glmmodel_edu_spec_mean.xlsx",sheet = "BESASFR")

model4<-read_excel("./BESFR_ data qaul model_gamma_47.xlsx",sheet = "BESASFR")

model5<-read_excel("./BESFR_data qaul glmmodel_pred_prec.xlsx",sheet = "BESASFR")

model6<-read_excel("./BESFR_data qaul glmmodel_alpha_beta.xlsx",sheet = "BESASFR")

model7<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_e.xlsx",sheet = "BESASFR")

#Model Comparison plots for ESASFR

# looping over unique countries
Countries<-unique(Main_model$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(Main_model %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(Main_model %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Main Model")) +
    
    
    geom_ribbon(model1 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model1 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 1")) +
    geom_ribbon(model2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 2")) +
    geom_ribbon(model3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 3")) +
    geom_ribbon(model4 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model4 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 4")) +
    geom_ribbon(model5 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model5%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 5")) +
    geom_ribbon(model6%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                            ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model6%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 6 ")) +
    geom_ribbon(model7%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model7%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Model 7 ")) +
    
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 10, hjust=0.5),axis.text.x = element_text(angle=90))+
    labs(x="Age Group",y="ASFR",colour="Source") +
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}

pdf("./ESASFR_all_model comparison.pdf", width=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()





#Model Comparison for ASFR with UN

#Read in ASFR data
Main_model_asfr<-read_excel("./BESFR_estimates global_south.xlsx",sheet = "ASFR")

model1_asfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_a.xlsx",sheet = "ASFR")

model2_asfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx",sheet = "ASFR")

model3_asfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_mean.xlsx",sheet = "ASFR")

model4_asfr<-read_excel("./BESFR_ data qaul model_gamma_47.xlsx",sheet = "ASFR")

model5_asfr<-read_excel("./BESFR_data qaul glmmodel_pred_prec.xlsx",sheet = "ASFR")

model6_asfr<-read_excel("./BESFR_data qaul glmmodel_alpha_beta.xlsx",sheet = "ASFR")

model7_asfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_e.xlsx",sheet = "ASFR")

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")



for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(Main_model_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                          ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(Main_model_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Main Model")) +
    
    
    geom_ribbon(model1_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model1_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 1")) +
    geom_ribbon(model2_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model2_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 2")) +
    geom_ribbon(model3_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model3_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 3")) +
    geom_ribbon(model4_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model4_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 4")) +
    geom_ribbon(model5_asfr %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model5_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 5")) +
    geom_ribbon(model6_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model6_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 6 ")) +
    geom_ribbon(model7_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                     ymax=unlist(Upper_CI),group=1),alpha=0.07)+
    geom_line(model7_asfr%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=1,colour="Model 7 ")) +
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1, colour ="UN")) +
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}

pdf("./UN_ASFR_model_comparison.pdf", width=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()






#Now we read in data for TFR and compare with UN

Main_model_tfr<-read_excel("./BESFR_estimates global_south.xlsx",sheet = "TFR")

model1_tfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_a.xlsx",sheet = "TFR")

model2_tfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx",sheet = "TFR")

model3_tfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_mean.xlsx",sheet = "TFR")

model4_tfr<-read_excel("./BESFR_ data qaul model_gamma_47.xlsx",sheet = "TFR")

model5_tfr<-read_excel("./BESFR_data qaul glmmodel_pred_prec.xlsx",sheet = "TFR")

model6_tfr<-read_excel("./BESFR_data qaul glmmodel_alpha_beta.xlsx",sheet = "TFR")

model7_tfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_e.xlsx",sheet = "TFR")


#Read in UN values of tfr
tfr_UN<-read_excel("./UN_datasets3.xlsx",sheet="UN_tfr")
tfr_UN<-tfr_UN%>%filter(Country%in%Main_model$Country)





pdf("./TFR_UN_model_comparison.pdf",width = 12, onefile = TRUE)

ggplot() +
  geom_ribbon(Main_model_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                        ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(Main_model_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Main Model")) +
  
  geom_ribbon(model1_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                    ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model1_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 1")) +
  geom_ribbon(model2_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                   ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model2_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 2")) +
  geom_ribbon(model3_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                   ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model3_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 3")) +
  geom_ribbon(model4_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                    ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model4_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 4")) +
  geom_ribbon(model5_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                    ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model5_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 5")) +
  geom_ribbon(model6_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                   ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model6_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 6 ")) +
  geom_ribbon(model7_tfr,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                     ymax=unlist(Upper_CI),group=1),alpha=0.07)+
  geom_line(model7_tfr,mapping=aes(x=Year,y=unlist(Median),group=1,colour="Model 7 ")) +
  
  geom_line(tfr_UN, mapping=aes(x=Year,y=TFR_,group=1, colour ="UN's TFR")) +
  facet_wrap(~Country)+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  ggtitle(paste0("Total Fertility Rate")) + 
  labs(x="Year",y="TFR", colour="Source") +
  theme(plot.title = element_text(size = 10, hjust=0.5))

dev.off()








#We now plot for ESTFR
Main_model_estfr<-read_excel("./BESFR_estimates global_south.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model1_estfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_a.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model2_estfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model3_estfr<-read_excel("./BESFR_data qaul glmmodel_edu_spec_mean.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model4_estfr<-read_excel("./BESFR_ data qaul model_gamma_47.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model5_estfr<-read_excel("./BESFR_data qaul glmmodel_pred_prec.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model6_estfr<-read_excel("./BESFR_data qaul glmmodel_alpha_beta.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

model7_estfr<-read_excel("./BESFR_data qaul glmmodel_se_sd_c_y_e.xlsx",sheet = "BESTFR")%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))




pdf("./ESTFR_all_model comparison.pdf",width = 12, onefile = TRUE)

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(Main_model_estfr %>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                     ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(Main_model_estfr %>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Main Model")) +
    
    
    geom_ribbon(model1_estfr %>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model1_estfr %>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 1")) +
    geom_ribbon(model2_estfr%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model2_estfr%>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 2")) +
    geom_ribbon(model3_estfr%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model3_estfr%>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 3")) +
    geom_ribbon(model4_estfr %>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model4_estfr %>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 4")) +
    geom_ribbon(model5_estfr %>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                 ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model5_estfr%>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 5")) +
    geom_ribbon(model6_estfr%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model6_estfr%>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 6 ")) +
    geom_ribbon(model7_estfr%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                      ymax=unlist(Upper_CI),group=Education),alpha=0.07)+
    geom_line(model7_estfr%>% filter(Country==Country_),mapping=aes(x=Year,y=unlist(Median),group=Education,colour="Model 7 ")) +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 10, hjust=0.5),axis.text.x = element_text(angle=90))+ 
    labs(x="Year",y="ESTFR",colour="Estimates")+
    ggtitle(paste0("Education Specific Total Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}

dev.off()





#######################################################################################
#Plotting for ASFR main model
cleaned_DHS_ASFR<-read_excel("./GLM regional/glm_predict_all_reg3.xlsx",sheet="ASFR_5")

cleaned_DHS_ASFR<-cleaned_DHS_ASFR
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==15]<-"15-19"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==20]<-"20-24"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==25]<-"25-29"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==30]<-"30-34"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==35]<-"35-39"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==40]<-"40-44"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==45]<-"45-49"



colnames(cleaned_DHS_ASFR)[7]<-"Age Group"




UN_1<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(Main_model, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(Main_model,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) + 
  geom_abline()+
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(x="Main Model",y="UN")+
  theme_bw()+
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28))
  



#Plotting for ASFR other model

UN_others1<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model1_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model1_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 1")+
  geom_abline()+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   



UN_others2<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model2_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model2_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 2")+
  geom_abline()+
   theme_bw()+   
  theme(axis.text = element_text(size=24), 
        axis.title = element_text(size=28))   


UN_others3<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model3_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model3_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 3")+
  geom_abline()+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   


UN_others4<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model4_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model4_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 4")+
  geom_abline()+
   theme_bw()+   
  theme(axis.text = element_text(size=24),      
        axis.title = element_text(size=28))   




UN_others5<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model5_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model5_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 5")+
  geom_abline()+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   


UN_others6<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model6_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model6_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 6")+
  geom_abline()+
   theme_bw()+  
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   


UN_others7<-ggplot(ASFR_UN,aes(y=Asfr))+
  geom_point(model7_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model7_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
  xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="Model 7")+
  geom_abline()+
  theme_bw()+  
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   




##Plotting for ASFR with DHS main model
DHS_1<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(Main_model, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(Main_model,mapping=aes(x=Median, xend=Median, group=Country, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) + 
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(x="Main Model",y="DHS")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),      
        axis.title = element_text(size=28))   



#Plotting for ASFR other model

DHS_others1<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model1_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model1_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 1")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   



DHS_others2<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model2_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model2_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 2")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   


DHS_others3<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model3_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model3_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 3")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   


DHS_others4<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model4_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model4_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 4")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   




DHS_others5<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model5_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model5_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 5")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),       
        axis.title = element_text(size=28))   


DHS_others6<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model6_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model6_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 6")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_others7<-ggplot(cleaned_DHS_ASFR,aes(y=ASFR))+
  geom_point(model7_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_segment(model7_asfr,mapping=aes(x=Median, xend=Median, y=Lower_CI,yend=Upper_CI), size=3,alpha=0.03) +
  xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="DHS",x="Model 7")+
  theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   




##Plotting Education specific plots starting with No education DHS
cleaned_DHS_ESASFR<-read_excel("./GLM regional/glm_predict_all_reg3.xlsx",sheet="ESASFR_5")
cleaned_DHS_ESASFR<-cleaned_DHS_ESASFR%>%filter(Region%in%c("Africa","North Africa"))

DHS_no<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(Main_model%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(Main_model%>%filter(Education=="No Education"), mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Main Model")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_pri<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(Main_model%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(Main_model%>%filter(Education=="Primary Education"), mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Main Model")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_sec<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(Main_model%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(Main_model%>%filter(Education=="Secondary Education"), mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Main Model")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   


DHS_high<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(Main_model%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(Main_model%>%filter(Education=="Higher Education"), mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Main Model")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   





##Plotting Education specific plots starting with No education DHS other models

DHS_no_other1<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model1%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model1%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 1")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_no_other2<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model2%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model2%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 2")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),
        axis.title = element_text(size=28))   


DHS_no_other3<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model3%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model3%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 3")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_no_other4<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model4%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model4%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 4")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_no_other5<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model5%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model5%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 5")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_no_other6<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model6%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model6%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 6")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   



DHS_no_other7<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="No Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model7%>%filter(Education=="No Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model7%>%filter(Education=="No Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="No Education ASFR DHS",x="Model 7")+
  theme_bw()+   
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   




DHS_pri_other1<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model1%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model1%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 1")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_pri_other2<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model2%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model2%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 2")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_pri_other3<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model3%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model3%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 3")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_pri_other4<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model4%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model4%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 4")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   



DHS_pri_other5<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model5%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model5%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 5")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   



DHS_pri_other6<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model6%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model6%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 6")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_pri_other7<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Primary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model7%>%filter(Education=="Primary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model7%>%filter(Education=="Primary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Primary Education ASFR DHS",x="Model 7")+
  theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   




DHS_sec_other1<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model1%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model1%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 1")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_sec_other2<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model2%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model2%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 2")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   


DHS_sec_other3<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model3%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model3%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 3")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_sec_other4<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model4%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model4%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 4")+
   theme_bw()+  
  theme(axis.text = element_text(size=24), 
        axis.title = element_text(size=28))   



DHS_sec_other5<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model5%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model5%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 5")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_sec_other6<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model6%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model6%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 6")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   

DHS_sec_other7<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Secondary Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model7%>%filter(Education=="Secondary Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model7%>%filter(Education=="Secondary Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Secondary Education ASFR DHS",x="Model 7")+
  theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   



DHS_high_other1<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model1%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model1%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 1")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_high_other2<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model2%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model2%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 2")+
   theme_bw()+  
  theme(axis.text = element_text(size=24), 
        axis.title = element_text(size=28))   


DHS_high_other3<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model3%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model3%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 3")+
   theme_bw()+   
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   


DHS_high_other4<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model4%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model4%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 4")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_high_other5<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model5%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model5%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 5")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   



DHS_high_other6<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model6%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model6%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 6")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   


DHS_high_other7<-ggplot()+
  geom_line(cleaned_DHS_ESASFR%>%filter(Education=="Higher Education"),
            mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(model7%>%filter(Education=="Higher Education"), mapping = aes(x=Median,y=Median, group=Country),
             size=3, alpha=0.03)+
  geom_errorbarh(model7%>%filter(Education=="Higher Education"), 
                 mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  xlim(0,0.5)+ylim(0,0.5)+
  geom_abline()+
  labs(y="Higher Education ASFR DHS",x="Model 7")+
  theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   






#Arrange the plots and name them

library(gridExtra)

png("./ASFR all_model comparison.png",width=2850, height=1850,res=150)
grid.arrange(UN_1, DHS_1, UN_others1, DHS_others1, UN_others2, DHS_others2, UN_others3, DHS_others3, UN_others4, DHS_others4,
             UN_others5, DHS_others5, UN_others6, DHS_others6, UN_others7, DHS_others7,ncol = 3)
dev.off()


png("./ESASFR all_model comparison.png",width=4850, height=3550,res=150)
grid.arrange(DHS_no, DHS_no_other1, DHS_no_other2, DHS_no_other3, DHS_no_other4, DHS_no_other5, DHS_no_other6, DHS_no_other7,
             DHS_pri, DHS_pri_other1, DHS_pri_other2, DHS_pri_other3, DHS_pri_other4, DHS_pri_other5, DHS_pri_other6, DHS_pri_other7,
             DHS_sec, DHS_sec_other1, DHS_sec_other2, DHS_sec_other3, DHS_sec_other4, DHS_sec_other5, DHS_sec_other6, DHS_sec_other7,
             DHS_high, DHS_high_other1,  DHS_high_other2, DHS_high_other3, DHS_high_other4, DHS_high_other5, DHS_high_other6,DHS_high_other7,
             ncol=8)
dev.off()



#####
#UN and DHS on scatter


#Plotting for ASFR other model
cleaned_DHS_ASFR<-read_excel("./GLM regional/glm_predict_all_reg3.xlsx",sheet="ASFR_5")
cleaned_DHS_ASFR<-cleaned_DHS_ASFR%>%filter(Region%in%c("Africa","North Africa"))
colnames(cleaned_DHS_ASFR)[7]<-"Age Group"
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==15]<-"15-19" 
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==20]<-"20-24"
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==25]<-"25-29"
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==30]<-"30-34" 
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==35]<-"35-39"  
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==40]<-"40-44"  
cleaned_DHS_ASFR$`Age Group`[cleaned_DHS_ASFR$`Age Group`==45]<-"45-49"



asfr_un<-ASFR_UN


asfr<-full_join(asfr_un[,-5],cleaned_DHS_ASFR[,c(1:3,6,7,10)],
            by=c("Country","Country code","Year","Age Group"))


png("./UN DHS.png",width=2850, height=1550)

ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_point(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="UN",x="ASFR",colour="Source")+
  geom_abline()+
   theme_bw()+  
  scale_colour_manual(values = c("red1","black"))+
  theme(axis.text = element_text(size=24),      
        axis.title = element_text(size=28))   

dev.off()




DHS_UN_1<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(Main_model_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(Main_model_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Main Model",colour="Source")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),    
        axis.title = element_text(size=28))   




#Plotting for ASFR other model

DHS_UN_others1<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model1_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model1_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Model 1")+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   



DHS_UN_others2<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model2_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model2_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="ASFR",x="Model 2")+
  geom_abline()+
   theme_bw()+  
  theme(axis.text = element_text(size=24),        
        axis.title = element_text(size=28))   


DHS_UN_others3<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model3_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model3_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  labs(y="ASFR",x="Model 3")+
  geom_abline()+
   theme_bw()+ 
  theme(axis.text = element_text(size=24),      
        axis.title = element_text(size=28))   


DHS_UN_others4<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model4_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model4_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Model 4")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),   
        axis.title = element_text(size=28))   




DHS_UN_others5<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model5_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model5_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Model 5")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),  
        axis.title = element_text(size=28))   


DHS_UN_others6<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model6_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model6_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
   xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Model 6")+
   theme_bw()+  
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   

DHS_UN_others7<-ggplot()+
  geom_point(asfr, mapping = aes(x=ASFR,y=Asfr,group=interaction(Country,`Survey Year`),colour="DHS"))+
  geom_line(asfr, mapping = aes(x=Asfr,y=Asfr,group=interaction(Country,`Survey Year`),colour="UN"))+
  geom_point(model7_asfr, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(model7_asfr, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), alpha=0.03)+
  xlim(0,0.5)+
  ylim(0,0.5)+
  geom_abline()+
  labs(y="ASFR",x="Model 7")+
  theme_bw()+  
  theme(axis.text = element_text(size=24),     
        axis.title = element_text(size=28))   


png("./UN DHS ASFR all_model comparison.png",width=2850, height=1550)
grid.arrange(DHS_UN_1, DHS_UN_others1, DHS_UN_others2, DHS_UN_others3, DHS_UN_others4,
             DHS_UN_others5, DHS_UN_others6, DHS_UN_others7,ncol = 3)
dev.off()


