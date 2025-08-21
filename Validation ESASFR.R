#In this model the focus is on Education specific rates.
library(readxl)
library(MCMCvis)
library(tidyverse)
library(coda)
library(zoo)
library(rjags)
library(glm2)
library(ggplot2)
library(ggmcmc)
library(gridExtra)
library(matrixStats)

setwd("K:/project/BayesEdu/Fertility/Afua")


#Read in old Bayesian Estimates

old_data_Africa<-read_excel("K:/project/BayesEdu/Fertility/Afua/BESFR_estimates Africa.xlsx")
old_data_LA<-read_excel("K:/project/BayesEdu/Fertility/Afua/BESFR_estimates Latin America.xlsx")

#Join the data!
old_data<-full_join(old_data_Africa,old_data_LA)


dat_global_southedu2_<-read_excel("./All countries Bayesian codes/GLM regional/glm_predict_all_reg3.xlsx")%>%
  filter(!Country%in%c("Papua New Guinea","Uzbekistan","Ukraine"))
dat_global_southedu2_<-dat_global_southedu2_[order(dat_global_southedu2_$Country), -4]



TFR_africaedu<-read.csv("./cc_y_edu_all_paper_models.csv")%>%filter(Model.name=="UN-fully consistent",Chain=="1")
TFR_africaedu$Country[TFR_africaedu$Country=="C\xf4te D'Ivoire"]<-"Côte D'Ivoire"
TFR_africaedu<-as.data.frame(TFR_africaedu)
Encoding(TFR_africaedu$Country) <- "UTF-8"

TFR_global_southedu2_<-unique(full_join(dat_global_southedu2_[,c(1:4)],TFR_africaedu,by=c("Country","Year","Education")))

#ESFTR by level of education
estfr_d1_2<-TFR_global_southedu2_%>%filter(Education=="No Education")
estfr_d1_2<-estfr_d1_2[order(estfr_d1_2$Country), ]

estfr_d1_3<-TFR_global_southedu2_%>%filter(Education=="Primary Education")
estfr_d1_3<-estfr_d1_3[order(estfr_d1_3$Country), ]


estfr_d1_4<-TFR_global_southedu2_%>%filter(Education=="Secondary Education")
estfr_d1_4<-estfr_d1_4[order(estfr_d1_4$Country), ]

estfr_d1_1<-TFR_global_southedu2_%>%filter(Education=="Higher Education")
estfr_d1_1<-estfr_d1_1[order(estfr_d1_1$Country), ]




#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(!Year%in%c("1955-1960", "1960-1965", "1965-1970"))%>%filter(Country%in%unique(dat_global_southedu2_$Country))
asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)

###Bringing the WIC data in to be population of women by level of education

#This is for women with higher education
wice_dat_global_south_1<-read_excel("./WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_global_south_1<-wice_dat_global_south_1%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Data for women with no education
wice_dat_global_south_2<-read_excel("./WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_global_south_2<-wice_dat_global_south_2%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#women with primary education
wice_dat_global_south_3<-read_excel("./WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_global_south_3<-wice_dat_global_south_3%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Women with secondary education
wice_dat_global_south_4<-read_excel("./WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_global_south_4<-wice_dat_global_south_4%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


##The main education model code
mod_stringedu_global_south = "model {
  for (i in 1:length(period)){
  
    TFR_star[i]=(ASFR_star[i,1]+ASFR_star[i,2]+ASFR_star[i,3]+ASFR_star[i,4]+ASFR_star[i,5]+ASFR_star[i,6]+ASFR_star[i,7])%*%5
    
    ESTFR_1[i]~dnorm(ESTFR_star_1_[i],prec_estfr1[i])T(0,10)
    ESTFR_2[i]~dnorm(ESTFR_star_2_[i],prec_estfr2[i])T(0,10)
    ESTFR_3[i]~dnorm(ESTFR_star_3_[i],prec_estfr3[i])T(0,10)
    ESTFR_4[i]~dnorm(ESTFR_star_4_[i],prec_estfr4[i])T(0,10)
    
    
    ESTFR_star_1[i]~dnorm(ESTFR_star_1_[i],prec_estfr1[i])T(0,10)
    ESTFR_star_2[i]~dnorm(ESTFR_star_2_[i],prec_estfr2[i])T(0,10)
    ESTFR_star_3[i]~dnorm(ESTFR_star_3_[i],prec_estfr3[i])T(0,10)
    ESTFR_star_4[i]~dnorm(ESTFR_star_4_[i],prec_estfr4[i])T(0,10)
    
    
    ESTFR_star_1_[i]=(ESASFR_star_1[i,1]+ESASFR_star_1[i,2]+ESASFR_star_1[i,3]+ESASFR_star_1[i,4]+ESASFR_star_1[i,5]+ESASFR_star_1[i,6]+ESASFR_star_1[i,7])%*%5
    ESTFR_star_2_[i]=(ESASFR_star_2[i,1]+ESASFR_star_2[i,2]+ESASFR_star_2[i,3]+ESASFR_star_2[i,4]+ESASFR_star_2[i,5]+ESASFR_star_2[i,6]+ESASFR_star_2[i,7])%*%5
    ESTFR_star_3_[i]=(ESASFR_star_3[i,1]+ESASFR_star_3[i,2]+ESASFR_star_3[i,3]+ESASFR_star_3[i,4]+ESASFR_star_3[i,5]+ESASFR_star_3[i,6]+ESASFR_star_3[i,7])%*%5
    ESTFR_star_4_[i]=(ESASFR_star_4[i,1]+ESASFR_star_4[i,2]+ESASFR_star_4[i,3]+ESASFR_star_4[i,4]+ESASFR_star_4[i,5]+ESASFR_star_4[i,6]+ESASFR_star_4[i,7])%*%5
    
    
    for(j in 1:length(Age)){
      
      no_edu_diff_hi[i,j] = ESASFR_star_2[i,j] - ESASFR_star_1[i,j]
      no_edu_diff_pri[i,j] = ESASFR_star_2[i,j] - ESASFR_star_3[i,j]
      no_edu_diff_sec[i,j] = ESASFR_star_2[i,j] - ESASFR_star_4[i,j]
      
    
      w_1[i,j]=pop_wic_1[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_2[i,j]=pop_wic_2[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_3[i,j]=pop_wic_3[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_4[i,j]=pop_wic_4[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      
      
      ESASFR_dhs_1[i,j]~dnorm(ESASFR_star_1[i,j],prec_esasfr1[i,j])T(0,)
      ESASFR_dhs_2[i,j]~dnorm(ESASFR_star_2[i,j],prec_esasfr2[i,j])T(0,)
      ESASFR_dhs_3[i,j]~dnorm(ESASFR_star_3[i,j],prec_esasfr3[i,j])T(0,)
      ESASFR_dhs_4[i,j]~dnorm(ESASFR_star_4[i,j],prec_esasfr4[i,j])T(0,)
      
      
      
      ESASFR_dhs_1_pred[i,j]~dnorm(ESASFR_star_1[i,j],prec_esasfr1[i,j])T(0,)
      ESASFR_dhs_2_pred[i,j]~dnorm(ESASFR_star_2[i,j],prec_esasfr2[i,j])T(0,)
      ESASFR_dhs_3_pred[i,j]~dnorm(ESASFR_star_3[i,j],prec_esasfr3[i,j])T(0,)
      ESASFR_dhs_4_pred[i,j]~dnorm(ESASFR_star_4[i,j],prec_esasfr4[i,j])T(0,)
      
      
      ESASFR_old_1[i,j]~dnorm(ESASFR_star_1[i,j],prec_esasfr1[i,j])T(0,)
     
      ESASFR_old_2[i,j]~dnorm(ESASFR_star_2[i,j],prec_esasfr2[i,j])T(0,)
      
      ESASFR_old_3[i,j]~dnorm(ESASFR_star_3[i,j],prec_esasfr3[i,j])T(0,)
      
      ESASFR_old_4[i,j]~dnorm(ESASFR_star_4[i,j],prec_esasfr4[i,j])T(0,)

      ASFR_star[i,j]=ESASFR_star_1[i,j]*w_1[i,j] +  ESASFR_star_2[i,j]*w_2[i,j] +
      ESASFR_star_3[i,j]*w_3[i,j] + ESASFR_star_4[i,j]*w_4[i,j]
      
      
      ASFR_UN[i,j]~dnorm(ASFR_star[i,j],prec_asfr_UN)T(0,) 
          
      prec_esasfr1[i,j]~dgamma(1/s_esasfr1[i,1],2*s_esasfr1[i,1])
      prec_esasfr2[i,j]~dgamma(1/s_esasfr2[i,1],2*s_esasfr2[i,1])
      prec_esasfr3[i,j]~dgamma(1/s_esasfr3[i,1],2*s_esasfr3[i,1])
      prec_esasfr4[i,j]~dgamma(1/s_esasfr4[i,1],2*s_esasfr4[i,1])
    
    }
  prec_estfr1[i]~dgamma(1/tfr_sd,2*tfr_var)
  prec_estfr2[i]~dgamma(1/tfr_sd,2*tfr_var)
  prec_estfr3[i]~dgamma(1/tfr_sd,2*tfr_var)
  prec_estfr4[i]~dgamma(1/tfr_sd,2*tfr_var)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"

#Higher education
esasfr_glm_na1<-dat_global_southedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na1<-esasfr_glm_na1[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na2<-dat_global_southedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na2<-esasfr_glm_na2[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na3<-dat_global_southedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na3<-esasfr_glm_na3[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na4<-dat_global_southedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na4<-esasfr_glm_na4[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



####We arrange the data in such a way that is compatible with the written model code
dat_global_southedu_<-dat_global_southedu2_[order(dat_global_southedu2_$Country), ]%>%
  filter(Country%in%c(unique(asfr_UN$Country)))


dat_global_southedu2<-dat_global_southedu_[,-c(6,7)]%>%distinct()%>%spread(Education,Pred)

dat_global_southedu2_1<-dat_global_southedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_global_southedu2_1<-dat_global_southedu2_1[order(dat_global_southedu2_1$Country), ]


dat_global_southedu2_2<-dat_global_southedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_global_southedu2_2<-dat_global_southedu2_2[order(dat_global_southedu2_2$Country), ]

dat_global_southedu2_3<-dat_global_southedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_global_southedu2_3<-dat_global_southedu2_3[order(dat_global_southedu2_3$Country), ]

dat_global_southedu2_4<-dat_global_southedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_global_southedu2_4<-dat_global_southedu2_4[order(dat_global_southedu2_4$Country), ]


dat_global_southedu_se2<-dat_global_southedu2_[,-c(5,7)]%>%distinct()%>%spread(Education,SE)



#Old Bayesian estimates
old_data2<-full_join(old_data[-c(5,6)],dat_global_southedu_[,c(1:4)])%>%distinct()%>%spread(Education,Median)
dat_global_southedu2_1_<-old_data2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_global_southedu2_1_<-dat_global_southedu2_1_[order(dat_global_southedu2_1$Country), ]

dat_global_southedu2_2_<-old_data2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_global_southedu2_2_<-dat_global_southedu2_2_[order(dat_global_southedu2_2$Country), ]

dat_global_southedu2_3_<-old_data2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_global_southedu2_3_<-dat_global_southedu2_3_[order(dat_global_southedu2_3$Country), ]

dat_global_southedu2_4_<-old_data2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_global_southedu2_4_<-dat_global_southedu2_4_[order(dat_global_southedu2_4$Country), ]




set.seed(122)
jagsdat_global_south = as.list(dat_global_southedu2_1)
jagsdat_global_south$period=as.matrix(dat_global_southedu2_1[,2])
jagsdat_global_south$Age=as.matrix(dat_global_southedu2_1[1,3:9])
jagsdat_global_south$pop_wic_1=as.matrix(wice_dat_global_south_1[,4:10])
jagsdat_global_south$pop_wic_2=as.matrix(wice_dat_global_south_2[,4:10])
jagsdat_global_south$pop_wic_3=as.matrix(wice_dat_global_south_3[,4:10])
jagsdat_global_south$pop_wic_4=as.matrix(wice_dat_global_south_4[,4:10])
jagsdat_global_south$ESASFR_dhs_1=as.matrix(esasfr_glm_na1[,3:9])
jagsdat_global_south$ESASFR_dhs_2=as.matrix(esasfr_glm_na2[,3:9])
jagsdat_global_south$ESASFR_dhs_3=as.matrix(esasfr_glm_na3[,3:9])
jagsdat_global_south$ESASFR_dhs_4=as.matrix(esasfr_glm_na4[,3:9])

jagsdat_global_south$ESASFR_star_1=as.matrix(dat_global_southedu2_1[,3:9])
jagsdat_global_south$ESASFR_star_2=as.matrix(dat_global_southedu2_2[,3:9])
jagsdat_global_south$ESASFR_star_3=as.matrix(dat_global_southedu2_3[,3:9])
jagsdat_global_south$ESASFR_star_4=as.matrix(dat_global_southedu2_4[,3:9])

jagsdat_global_south$ESASFR_old_1=as.matrix(dat_global_southedu2_1_[,3:9])
jagsdat_global_south$ESASFR_old_2=as.matrix(dat_global_southedu2_2_[,3:9])
jagsdat_global_south$ESASFR_old_3=as.matrix(dat_global_southedu2_3_[,3:9])
jagsdat_global_south$ESASFR_old_4=as.matrix(dat_global_southedu2_4_[,3:9])


jagsdat_global_south$ESTFR_1=as.vector(unlist(estfr_d1_1[,6]))
jagsdat_global_south$ESTFR_2=as.vector(unlist(estfr_d1_2[,6]))
jagsdat_global_south$ESTFR_3=as.vector(unlist(estfr_d1_3[,6]))
jagsdat_global_south$ESTFR_4=as.vector(unlist(estfr_d1_4[,6]))


jagsdat_global_south$tfr_sd=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_global_south$tfr_var=var(na.omit(estfr_d1_1$ESTFR))

jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_global_south$ASFR_UN_var=(sd(asfr_UN_$Asfr)^2)


jagsdat_global_south$s_esasfr1=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:4)])%>%
                                                                     spread(`Age Group`,`Higher Education`))[,3:9]))))

jagsdat_global_south$s_esasfr2=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,5)])%>%
                                                                     spread(`Age Group`,`No Education`))[,3:9]))))

jagsdat_global_south$s_esasfr3=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,6)])%>%
                                                                     spread(`Age Group`,`Primary Education`))[,3:9]))))

jagsdat_global_south$s_esasfr4=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,7)])%>%
                                                                     spread(`Age Group`,`Secondary Education`))[,3:9]))))

jagsdat_global_south$in_dhs=as.matrix(esasfr_glm_na1[,2])


paramsedu_global_south =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4","ESASFR_dhs_1_pred","ESASFR_dhs_2_pred","ESASFR_dhs_3_pred","ESASFR_dhs_4_pred",
                    "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                    "var_asfr_UN","prec_esasfr1","prec_esasfr2","prec_esasfr3","prec_esasfr4","prec_estfr","sd_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


modedu_global_south = jags.model(textConnection(mod_stringedu_global_south), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south, 2e3)


mod_sim3edu_global_south = coda.samples(model=modedu_global_south,variable.names=paramsedu_global_south,
                                  n.iter=2e4)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_global_southedu_1<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_dhs_1_pred")
UPCIedupred1_global_southedu_1<-MCsumedupred1_global_southedu_1$`97.5%`
LWCIedupred1_global_southedu_1<-MCsumedupred1_global_southedu_1$`2.5%`
posterior_meansedupred1_global_southedu_1=MCsumedupred1_global_southedu_1$`50%`
posterior_meansedupred1_global_southedu_1<-as.data.frame(posterior_meansedupred1_global_southedu_1)
UPCIedupred1_global_southedu_1<-as.data.frame(UPCIedupred1_global_southedu_1)
LWCIedupred1_global_southedu_1<-as.data.frame(LWCIedupred1_global_southedu_1)
colnames(UPCIedupred1_global_southedu_1)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_1)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_1<-cbind(UPCIedupred1_global_southedu_1,LWCIedupred1_global_southedu_1)
posterior_quantilesedupred1_global_southedu_1<-as.data.frame(posterior_quantilesedupred1_global_southedu_1)
###


#No Education
MCsumedupred1_global_southedu_2<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_dhs_2_pred")
UPCIedupred1_global_southedu_2<-MCsumedupred1_global_southedu_2$`97.5%`
LWCIedupred1_global_southedu_2<-MCsumedupred1_global_southedu_2$`2.5%`
posterior_meansedupred1_global_southedu_2=MCsumedupred1_global_southedu_2$`50%`
posterior_meansedupred1_global_southedu_2<-as.data.frame(posterior_meansedupred1_global_southedu_2)
UPCIedupred1_global_southedu_2<-as.data.frame(UPCIedupred1_global_southedu_2)
LWCIedupred1_global_southedu_2<-as.data.frame(LWCIedupred1_global_southedu_2)
colnames(UPCIedupred1_global_southedu_2)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_2)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_2<-cbind(UPCIedupred1_global_southedu_2,LWCIedupred1_global_southedu_2)
posterior_quantilesedupred1_global_southedu_2<-as.data.frame(posterior_quantilesedupred1_global_southedu_2)


#Primary Education
MCsumedupred1_global_southedu_3<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_dhs_3_pred")
UPCIedupred1_global_southedu_3<-MCsumedupred1_global_southedu_3$`97.5%`
LWCIedupred1_global_southedu_3<-MCsumedupred1_global_southedu_3$`2.5%`
posterior_meansedupred1_global_southedu_3=MCsumedupred1_global_southedu_3$`50%`
posterior_meansedupred1_global_southedu_3<-as.data.frame(posterior_meansedupred1_global_southedu_3)
UPCIedupred1_global_southedu_3<-as.data.frame(UPCIedupred1_global_southedu_3)
LWCIedupred1_global_southedu_3<-as.data.frame(LWCIedupred1_global_southedu_3)
colnames(UPCIedupred1_global_southedu_3)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_3)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_3<-cbind(UPCIedupred1_global_southedu_3,LWCIedupred1_global_southedu_3)
posterior_quantilesedupred1_global_southedu_3<-as.data.frame(posterior_quantilesedupred1_global_southedu_3)


#Secondary Education
MCsumedupred1_global_southedu_4<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_dhs_4_pred")
UPCIedupred1_global_southedu_4<-MCsumedupred1_global_southedu_4$`97.5%`
LWCIedupred1_global_southedu_4<-MCsumedupred1_global_southedu_4$`2.5%`
posterior_meansedupred1_global_southedu_4=MCsumedupred1_global_southedu_4$`50%`
posterior_meansedupred1_global_southedu_4<-as.data.frame(posterior_meansedupred1_global_southedu_4)
UPCIedupred1_global_southedu_4<-as.data.frame(UPCIedupred1_global_southedu_4)
LWCIedupred1_global_southedu_4<-as.data.frame(LWCIedupred1_global_southedu_4)
colnames(UPCIedupred1_global_southedu_4)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_4)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_4<-cbind(UPCIedupred1_global_southedu_4,LWCIedupred1_global_southedu_4)
posterior_quantilesedupred1_global_southedu_4<-as.data.frame(posterior_quantilesedupred1_global_southedu_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_global_southedupred_1<-cbind("Mean"=posterior_meansedupred1_global_southedu_1,"Upper_CI"=posterior_quantilesedupred1_global_southedu_1[,1],
                            "Lower_CI"=posterior_quantilesedupred1_global_southedu_1[,2])
colnames(bayesdat_global_southedupred_1)[1]<-"Median"


bayesdat_global_southedupred_2<-cbind("Mean"=posterior_meansedupred1_global_southedu_2,"Upper_CI"=posterior_quantilesedupred1_global_southedu_2[,1],
                            "Lower_CI"=posterior_quantilesedupred1_global_southedu_2[,2])
colnames(bayesdat_global_southedupred_2)[1]<-"Median"



bayesdat_global_southedupred_3<-cbind("Mean"=posterior_meansedupred1_global_southedu_3,"Upper_CI"=posterior_quantilesedupred1_global_southedu_3[,1],
                            "Lower_CI"=posterior_quantilesedupred1_global_southedu_3[,2])
colnames(bayesdat_global_southedupred_3)[1]<-"Median"



bayesdat_global_southedupred_4<-cbind("Mean"=posterior_meansedupred1_global_southedu_4,"Upper_CI"=posterior_quantilesedupred1_global_southedu_4[,1],
                            "Lower_CI"=posterior_quantilesedupred1_global_southedu_4[,2])
colnames(bayesdat_global_southedupred_4)[1]<-"Median"



e_1<-gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdatpred_global_south_1_<-cbind(e_1,bayesdat_global_southedupred_1)


e_2<-gather(dat_global_southedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdatpred_global_south_2_<-cbind(e_2,bayesdat_global_southedupred_2)


e_3<-gather(dat_global_southedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdatpred_global_south_3_<-cbind(e_3,bayesdat_global_southedupred_3)



e_4<-gather(dat_global_southedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdatpred_global_south_4_<-cbind(e_4,bayesdat_global_southedupred_4)

bayesdatpred_global_south1<-rbind(bayesdatpred_global_south_1_,bayesdatpred_global_south_2_)
bayesdatpred_global_south1<-rbind(bayesdatpred_global_south1,bayesdatpred_global_south_3_)
bayesdatpred_global_south1<-rbind(bayesdatpred_global_south1,bayesdatpred_global_south_4_)


bayesdatpred_global_south_1<-bayesdatpred_global_south1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_glm_na11<-dat_global_southedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na11<-esasfr_glm_na11[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na12<-dat_global_southedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na12<-esasfr_glm_na12[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na13<-dat_global_southedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na13<-esasfr_glm_na13[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na14<-dat_global_southedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na14<-esasfr_glm_na14[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_global_south$ESASFR_dhs_1=as.matrix(esasfr_glm_na11[,3:9])
jagsdat_global_south$ESASFR_dhs_2=as.matrix(esasfr_glm_na12[,3:9])
jagsdat_global_south$ESASFR_dhs_3=as.matrix(esasfr_glm_na13[,3:9])
jagsdat_global_south$ESASFR_dhs_4=as.matrix(esasfr_glm_na14[,3:9])


modedu_global_south1 = jags.model(textConnection(mod_stringedu_global_south), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south1, 2e3)


mod_sim3edu_global_south1 = coda.samples(model=modedu_global_south1,variable.names=paramsedu_global_south,
                                  n.iter=2e4)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_global_southedu_11<-MCMCsummary(mod_sim3edu_global_south1, params ="ESASFR_dhs_1_pred")
UPCIedupred1_global_southedu_11<-MCsumedupred1_global_southedu_11$`97.5%`
LWCIedupred1_global_southedu_11<-MCsumedupred1_global_southedu_11$`2.5%`
posterior_meansedupred1_global_southedu_11=MCsumedupred1_global_southedu_11$`50%`
posterior_meansedupred1_global_southedu_11<-as.data.frame(posterior_meansedupred1_global_southedu_11)
UPCIedupred1_global_southedu_11<-as.data.frame(UPCIedupred1_global_southedu_11)
LWCIedupred1_global_southedu_11<-as.data.frame(LWCIedupred1_global_southedu_11)
colnames(UPCIedupred1_global_southedu_11)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_11)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_11<-cbind(UPCIedupred1_global_southedu_11,LWCIedupred1_global_southedu_11)
posterior_quantilesedupred1_global_southedu_11<-as.data.frame(posterior_quantilesedupred1_global_southedu_11)
###


#No Education
MCsumedupred1_global_southedu_21<-MCMCsummary(mod_sim3edu_global_south1, params ="ESASFR_dhs_2_pred")
UPCIedupred1_global_southedu_21<-MCsumedupred1_global_southedu_21$`97.5%`
LWCIedupred1_global_southedu_21<-MCsumedupred1_global_southedu_21$`2.5%`
posterior_meansedupred1_global_southedu_21=MCsumedupred1_global_southedu_21$`50%`
posterior_meansedupred1_global_southedu_21<-as.data.frame(posterior_meansedupred1_global_southedu_21)
UPCIedupred1_global_southedu_21<-as.data.frame(UPCIedupred1_global_southedu_21)
LWCIedupred1_global_southedu_21<-as.data.frame(LWCIedupred1_global_southedu_21)
colnames(UPCIedupred1_global_southedu_21)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_21)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_21<-cbind(UPCIedupred1_global_southedu_21,LWCIedupred1_global_southedu_21)
posterior_quantilesedupred1_global_southedu_21<-as.data.frame(posterior_quantilesedupred1_global_southedu_21)


#Primary Education
MCsumedupred1_global_southedu_31<-MCMCsummary(mod_sim3edu_global_south1, params ="ESASFR_dhs_3_pred")
UPCIedupred1_global_southedu_31<-MCsumedupred1_global_southedu_31$`97.5%`
LWCIedupred1_global_southedu_31<-MCsumedupred1_global_southedu_31$`2.5%`
posterior_meansedupred1_global_southedu_31=MCsumedupred1_global_southedu_31$`50%`
posterior_meansedupred1_global_southedu_31<-as.data.frame(posterior_meansedupred1_global_southedu_31)
UPCIedupred1_global_southedu_31<-as.data.frame(UPCIedupred1_global_southedu_31)
LWCIedupred1_global_southedu_31<-as.data.frame(LWCIedupred1_global_southedu_31)
colnames(UPCIedupred1_global_southedu_31)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_31)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_31<-cbind(UPCIedupred1_global_southedu_31,LWCIedupred1_global_southedu_31)
posterior_quantilesedupred1_global_southedu_31<-as.data.frame(posterior_quantilesedupred1_global_southedu_31)


#Secondary Education
MCsumedupred1_global_southedu_41<-MCMCsummary(mod_sim3edu_global_south1, params ="ESASFR_dhs_4_pred")
UPCIedupred1_global_southedu_41<-MCsumedupred1_global_southedu_41$`97.5%`
LWCIedupred1_global_southedu_41<-MCsumedupred1_global_southedu_41$`2.5%`
posterior_meansedupred1_global_southedu_41=MCsumedupred1_global_southedu_41$`50%`
posterior_meansedupred1_global_southedu_41<-as.data.frame(posterior_meansedupred1_global_southedu_41)
UPCIedupred1_global_southedu_41<-as.data.frame(UPCIedupred1_global_southedu_41)
LWCIedupred1_global_southedu_41<-as.data.frame(LWCIedupred1_global_southedu_41)
colnames(UPCIedupred1_global_southedu_41)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_41)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_41<-cbind(UPCIedupred1_global_southedu_41,LWCIedupred1_global_southedu_41)
posterior_quantilesedupred1_global_southedu_41<-as.data.frame(posterior_quantilesedupred1_global_southedu_41)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_global_southedupred_11<-cbind("Mean"=posterior_meansedupred1_global_southedu_11,"Upper_CI"=posterior_quantilesedupred1_global_southedu_11[,1],
                                "Lower_CI"=posterior_quantilesedupred1_global_southedu_11[,2])
colnames(bayesdat_global_southedupred_11)[1]<-"Median"


bayesdat_global_southedupred_21<-cbind("Mean"=posterior_meansedupred1_global_southedu_21,"Upper_CI"=posterior_quantilesedupred1_global_southedu_21[,1],
                                "Lower_CI"=posterior_quantilesedupred1_global_southedu_21[,2])
colnames(bayesdat_global_southedupred_21)[1]<-"Median"



bayesdat_global_southedupred_31<-cbind("Mean"=posterior_meansedupred1_global_southedu_31,"Upper_CI"=posterior_quantilesedupred1_global_southedu_31[,1],
                                "Lower_CI"=posterior_quantilesedupred1_global_southedu_31[,2])
colnames(bayesdat_global_southedupred_31)[1]<-"Median"



bayesdat_global_southedupred_41<-cbind("Mean"=posterior_meansedupred1_global_southedu_41,"Upper_CI"=posterior_quantilesedupred1_global_southedu_41[,1],
                                "Lower_CI"=posterior_quantilesedupred1_global_southedu_41[,2])
colnames(bayesdat_global_southedupred_41)[1]<-"Median"



e_11<-gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)
e_11<-e_11%>%mutate(Education="Higher Education")
bayesdatpred_global_south_11_<-cbind(e_11,bayesdat_global_southedupred_11)


e_21<-gather(dat_global_southedu2_2,"Age Group","ASFR",3:9)
e_21<-e_21%>%mutate(Education="No Education")
bayesdatpred_global_south_21_<-cbind(e_21,bayesdat_global_southedupred_21)


e_31<-gather(dat_global_southedu2_3,"Age Group","ASFR",3:9)
e_31<-e_31%>%mutate(Education="Primary Education")
bayesdatpred_global_south_31_<-cbind(e_31,bayesdat_global_southedupred_31)



e_41<-gather(dat_global_southedu2_4,"Age Group","ASFR",3:9)
e_41<-e_41%>%mutate(Education="Secondary Education")
bayesdatpred_global_south_41_<-cbind(e_41,bayesdat_global_southedupred_41)

bayesdatpred_global_south11<-rbind(bayesdatpred_global_south_11_,bayesdatpred_global_south_21_)
bayesdatpred_global_south11<-rbind(bayesdatpred_global_south11,bayesdatpred_global_south_31_)
bayesdatpred_global_south11<-rbind(bayesdatpred_global_south11,bayesdatpred_global_south_41_)


bayesdatpred_global_south_2<-bayesdatpred_global_south11%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_glm_na21<-dat_global_southedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na21<-esasfr_glm_na21[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na22<-dat_global_southedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na22<-esasfr_glm_na22[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na23<-dat_global_southedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na23<-esasfr_glm_na23[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na24<-dat_global_southedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na24<-esasfr_glm_na24[,-c(6,7)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_global_south$ESASFR_dhs_1=as.matrix(esasfr_glm_na21[,3:9])
jagsdat_global_south$ESASFR_dhs_2=as.matrix(esasfr_glm_na22[,3:9])
jagsdat_global_south$ESASFR_dhs_3=as.matrix(esasfr_glm_na23[,3:9])
jagsdat_global_south$ESASFR_dhs_4=as.matrix(esasfr_glm_na24[,3:9])


modedu_global_south2 = jags.model(textConnection(mod_stringedu_global_south), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south2, 2e3)


mod_sim3edu_global_south2 = coda.samples(model=modedu_global_south2,variable.names=paramsedu_global_south,
                                   n.iter=2e4)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_global_southedu_12<-MCMCsummary(mod_sim3edu_global_south2, params ="ESASFR_dhs_1_pred")
UPCIedupred1_global_southedu_12<-MCsumedupred1_global_southedu_12$`97.5%`
LWCIedupred1_global_southedu_12<-MCsumedupred1_global_southedu_12$`2.5%`
posterior_meansedupred1_global_southedu_12=MCsumedupred1_global_southedu_12$`50%`
posterior_meansedupred1_global_southedu_12<-as.data.frame(posterior_meansedupred1_global_southedu_12)
UPCIedupred1_global_southedu_12<-as.data.frame(UPCIedupred1_global_southedu_12)
LWCIedupred1_global_southedu_12<-as.data.frame(LWCIedupred1_global_southedu_12)
colnames(UPCIedupred1_global_southedu_12)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_12)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_12<-cbind(UPCIedupred1_global_southedu_12,LWCIedupred1_global_southedu_12)
posterior_quantilesedupred1_global_southedu_12<-as.data.frame(posterior_quantilesedupred1_global_southedu_12)
###


#No Education
MCsumedupred1_global_southedu_22<-MCMCsummary(mod_sim3edu_global_south2, params ="ESASFR_dhs_2_pred")
UPCIedupred1_global_southedu_22<-MCsumedupred1_global_southedu_22$`97.5%`
LWCIedupred1_global_southedu_22<-MCsumedupred1_global_southedu_22$`2.5%`
posterior_meansedupred1_global_southedu_22=MCsumedupred1_global_southedu_22$`50%`
posterior_meansedupred1_global_southedu_22<-as.data.frame(posterior_meansedupred1_global_southedu_22)
UPCIedupred1_global_southedu_22<-as.data.frame(UPCIedupred1_global_southedu_22)
LWCIedupred1_global_southedu_22<-as.data.frame(LWCIedupred1_global_southedu_22)
colnames(UPCIedupred1_global_southedu_22)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_22)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_22<-cbind(UPCIedupred1_global_southedu_22,LWCIedupred1_global_southedu_22)
posterior_quantilesedupred1_global_southedu_22<-as.data.frame(posterior_quantilesedupred1_global_southedu_22)


#Primary Education
MCsumedupred1_global_southedu_32<-MCMCsummary(mod_sim3edu_global_south2, params ="ESASFR_dhs_3_pred")
UPCIedupred1_global_southedu_32<-MCsumedupred1_global_southedu_32$`97.5%`
LWCIedupred1_global_southedu_32<-MCsumedupred1_global_southedu_32$`2.5%`
posterior_meansedupred1_global_southedu_32=MCsumedupred1_global_southedu_32$`50%`
posterior_meansedupred1_global_southedu_32<-as.data.frame(posterior_meansedupred1_global_southedu_32)
UPCIedupred1_global_southedu_32<-as.data.frame(UPCIedupred1_global_southedu_32)
LWCIedupred1_global_southedu_32<-as.data.frame(LWCIedupred1_global_southedu_32)
colnames(UPCIedupred1_global_southedu_32)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_32)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_32<-cbind(UPCIedupred1_global_southedu_32,LWCIedupred1_global_southedu_32)
posterior_quantilesedupred1_global_southedu_32<-as.data.frame(posterior_quantilesedupred1_global_southedu_32)


#Secondary Education
MCsumedupred1_global_southedu_42<-MCMCsummary(mod_sim3edu_global_south2, params ="ESASFR_dhs_4_pred")
UPCIedupred1_global_southedu_42<-MCsumedupred1_global_southedu_42$`97.5%`
LWCIedupred1_global_southedu_42<-MCsumedupred1_global_southedu_42$`2.5%`
posterior_meansedupred1_global_southedu_42=MCsumedupred1_global_southedu_42$`50%`
posterior_meansedupred1_global_southedu_42<-as.data.frame(posterior_meansedupred1_global_southedu_42)
UPCIedupred1_global_southedu_42<-as.data.frame(UPCIedupred1_global_southedu_42)
LWCIedupred1_global_southedu_42<-as.data.frame(LWCIedupred1_global_southedu_42)
colnames(UPCIedupred1_global_southedu_42)<-"Upper_CI"
colnames(LWCIedupred1_global_southedu_42)<-"Lower_CI"
posterior_quantilesedupred1_global_southedu_42<-cbind(UPCIedupred1_global_southedu_42,LWCIedupred1_global_southedu_42)
posterior_quantilesedupred1_global_southedu_42<-as.data.frame(posterior_quantilesedupred1_global_southedu_42)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_global_southedupred_12<-cbind("Mean"=posterior_meansedupred1_global_southedu_12,"Upper_CI"=posterior_quantilesedupred1_global_southedu_12[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_global_southedu_12[,2])
colnames(bayesdat_global_southedupred_12)[1]<-"Median"


bayesdat_global_southedupred_22<-cbind("Mean"=posterior_meansedupred1_global_southedu_22,"Upper_CI"=posterior_quantilesedupred1_global_southedu_22[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_global_southedu_22[,2])
colnames(bayesdat_global_southedupred_22)[1]<-"Median"



bayesdat_global_southedupred_32<-cbind("Mean"=posterior_meansedupred1_global_southedu_32,"Upper_CI"=posterior_quantilesedupred1_global_southedu_32[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_global_southedu_32[,2])
colnames(bayesdat_global_southedupred_32)[1]<-"Median"



bayesdat_global_southedupred_42<-cbind("Mean"=posterior_meansedupred1_global_southedu_42,"Upper_CI"=posterior_quantilesedupred1_global_southedu_42[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_global_southedu_42[,2])
colnames(bayesdat_global_southedupred_42)[1]<-"Median"



e_12<-gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)
e_12<-e_12%>%mutate(Education="Higher Education")
bayesdatpred_global_south_12_<-cbind(e_12,bayesdat_global_southedupred_12)


e_22<-gather(dat_global_southedu2_2,"Age Group","ASFR",3:9)
e_22<-e_22%>%mutate(Education="No Education")
bayesdatpred_global_south_22_<-cbind(e_22,bayesdat_global_southedupred_22)


e_32<-gather(dat_global_southedu2_3,"Age Group","ASFR",3:9)
e_32<-e_32%>%mutate(Education="Primary Education")
bayesdatpred_global_south_32_<-cbind(e_32,bayesdat_global_southedupred_32)



e_42<-gather(dat_global_southedu2_4,"Age Group","ASFR",3:9)
e_42<-e_42%>%mutate(Education="Secondary Education")
bayesdatpred_global_south_42_<-cbind(e_42,bayesdat_global_southedupred_42)

bayesdatpred_global_south31<-rbind(bayesdatpred_global_south_12_,bayesdatpred_global_south_22_)
bayesdatpred_global_south31<-rbind(bayesdatpred_global_south31,bayesdatpred_global_south_32_)
bayesdatpred_global_south31<-rbind(bayesdatpred_global_south31,bayesdatpred_global_south_42_)


bayesdatpred_global_south_3<-bayesdatpred_global_south31%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))




################################################################################################################################################################################################################
################################################################################################################################################################################################################

dat_global_southedu_<-dat_global_southedu_%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

Countries<-unique(dat_global_southedu_$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_ribbon(bayesdatpred_global_south_1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_global_south_1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="5%")) +
    
    
    geom_ribbon(bayesdatpred_global_south_2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_global_south_2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="10%")) +
    
    
    geom_ribbon(bayesdatpred_global_south_3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_global_south_3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="15%")) +
    geom_line(dat_global_southedu_%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=Pred,group=Education,colour="Initial values")) +
    facet_grid(Education~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}   

pdf("K:/project/BayesEdu/Fertility/Afua/All countries Bayesian codes/Sensitivity/Validation_easfr.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()