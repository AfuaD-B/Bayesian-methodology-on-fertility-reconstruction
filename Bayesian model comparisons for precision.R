#In this code, I compare the results of all the approaches we took for the precision of our model
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
#We call in our estimates and create a new column to help identify the models in a plot!

gamma47<-read_excel("./BESFR_ data qaul model_gamma_47.xlsx", sheet = "BESASFR")
gamma47$Prec_model<-"Gamma(1/sd(se)^2,sd(se)^2)"
gamma47<-gamma47%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))


gamma_alpha_beta<-read_excel("./BESFR_data qaul glmmodel_alpha_beta.xlsx", sheet = "BESASFR")
gamma_alpha_beta$Prec_model<-"Gamma((sd(se)/mean(se))^2,(sd(se)^2)/mean(se))"
gamma_alpha_beta<-gamma_alpha_beta%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))


gamma_pred_prec<-read_excel("./BESFR_data qaul glmmodel_pred_prec.xlsx", sheet = "BESASFR")
gamma_pred_prec$Prec_model<-"Gamma(1/sd(pred)^2,sd(pred)^2)"
gamma_pred_prec<-gamma_pred_prec%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))



gamma_edu_spec_mean<-read_excel("./BESFR_data qaul glmmodel_edu_spec_mean.xlsx", sheet = "BESASFR")
gamma_edu_spec_mean$Prec_model<-"Gamma(1/mean(edu_se),sd(edu_se)^2)"
gamma_edu_spec_mean<-gamma_edu_spec_mean%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))


gamma_edu_spec_sd<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx", sheet = "BESASFR")
gamma_edu_spec_sd$Prec_model<-"Gamma(1/sd(edu_se)^2,sd(edu_se)^2)"
gamma_edu_spec_sd<-gamma_edu_spec_sd%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))


gamma_edu_se_sd_cya<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx", sheet = "BESASFR")
gamma_edu_se_sd_cya$Prec_model<-"Gamma(1/sd(age_se)^2,sd(age_se)^2)"
gamma_edu_se_sd_cya<-gamma_edu_se_sd_cya%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))



gamma_edu_se_sd_cye<-read_excel("./BESFR_data qaul glmmodel_edu_spec_sd.xlsx", sheet = "BESASFR")
gamma_edu_se_sd_cye$Prec_model<-"Gamma(1/sd(edu_se)^2,sd(edu_se)^2)"
gamma_edu_se_sd_cye<-gamma_edu_se_sd_cye%>%mutate(Education=factor(Education, levels = c("No Education","Primary Education","Secondary Education","Higher Education")))

#Now we make the plot to show the differences in the respective models in terms of estimation!

Countries<-unique(gamma47$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(gamma47 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma47 %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    geom_ribbon(gamma_alpha_beta %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_alpha_beta %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    
    geom_ribbon(gamma_pred_prec %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_pred_prec %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    
    geom_ribbon(gamma_edu_spec_mean %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_edu_spec_mean %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    
    geom_ribbon(gamma_edu_spec_sd %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_edu_spec_sd %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    
    geom_ribbon(gamma_edu_se_sd_cye %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_edu_se_sd_cye %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    
    
    geom_ribbon(gamma_edu_se_sd_cya %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),ymax=unlist(Upper_CI),group=Education,fill = Prec_model),alpha=0.1)+
    geom_line(gamma_edu_se_sd_cya %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour=Prec_model)) +
    
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 14),
          legend.title = element_text(size=16))+
    labs(x="Age Group",y="ASFR",colour="Precision model",fill="Precision model") +
    ggtitle(paste0("Comparison of Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}

pdf("./BESASFR model comparision.pdf", width=15,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

