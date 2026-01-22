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

setwd(".")

dat_global_southedu2_<-read_excel("./glm_predict_all_reg3.xlsx")%>%
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
      
      
  
      
      ESASFR_star_1[i,j]~dnorm(ESASFR_dhs_1[i,j],prec_esasfr1[i,1])T(0,)
      ESASFR_star_2[i,j]~dnorm(ESASFR_dhs_2[i,j],prec_esasfr2[i,1])T(0,)
      ESASFR_star_3[i,j]~dnorm(ESASFR_dhs_3[i,j],prec_esasfr3[i,1])T(0,)
      ESASFR_star_4[i,j]~dnorm(ESASFR_dhs_4[i,j],prec_esasfr4[i,1])T(0,)
      
      
      ASFR_star[i,j]=ESASFR_star_1[i,j]*w_1[i,j] +  ESASFR_star_2[i,j]*w_2[i,j] +
      ESASFR_star_3[i,j]*w_3[i,j] + ESASFR_star_4[i,j]*w_4[i,j]
      
      
      ASFR_UN[i,j]~dnorm(ASFR_star[i,j],prec_asfr_UN)T(0,) 
      
  }
     prec_prec1[i,1]=1/prec_esasfr1[i,1]  
     prec_esasfr1[i,1]~dgamma(inv_sd_esasfr1[i,1],sdsasfr1[i,1])
     
     prec_prec2[i,1]=1/prec_esasfr2[i,1] 
     prec_esasfr2[i,1]~dgamma(inv_sd_esasfr2[i,1],sdsasfr2[i,1])
     
     prec_prec3[i,1]=1/prec_esasfr3[i,1]
     prec_esasfr3[i,1]~dgamma(inv_sd_esasfr3[i,1],sdsasfr3[i,1])
     
     prec_prec4[i,1]=1/prec_esasfr4[i,1]
     prec_esasfr4[i,1]~dgamma(inv_sd_esasfr4[i,1],sdsasfr4[i,1])
     
  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)

}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var

}"


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

dat_global_southeduse2_1<-dat_global_southedu_se2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_global_southeduse2_1<-dat_global_southeduse2_1[order(dat_global_southeduse2_1$Country), ]


dat_global_southeduse2_2<-dat_global_southedu_se2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_global_southeduse2_2<-dat_global_southeduse2_2[order(dat_global_southeduse2_2$Country), ]

dat_global_southeduse2_3<-dat_global_southedu_se2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_global_southeduse2_3<-dat_global_southeduse2_3[order(dat_global_southeduse2_3$Country), ]

dat_global_southeduse2_4<-dat_global_southedu_se2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_global_southeduse2_4<-dat_global_southeduse2_4[order(dat_global_southeduse2_4$Country), ]



set.seed(122)
jagsdat_global_south = as.list(dat_global_southedu2_1)
jagsdat_global_south$period=as.matrix(dat_global_southedu2_1[,2])
jagsdat_global_south$Age=as.matrix(dat_global_southedu2_1[1,3:9])
jagsdat_global_south$pop_wic_1=as.matrix(wice_dat_global_south_1[,4:10])
jagsdat_global_south$pop_wic_2=as.matrix(wice_dat_global_south_2[,4:10])
jagsdat_global_south$pop_wic_3=as.matrix(wice_dat_global_south_3[,4:10])
jagsdat_global_south$pop_wic_4=as.matrix(wice_dat_global_south_4[,4:10])

jagsdat_global_south$ESASFR_dhs_1=as.matrix(dat_global_southedu2_1[,3:9])
jagsdat_global_south$ESASFR_dhs_2=as.matrix(dat_global_southedu2_2[,3:9])
jagsdat_global_south$ESASFR_dhs_3=as.matrix(dat_global_southedu2_3[,3:9])
jagsdat_global_south$ESASFR_dhs_4=as.matrix(dat_global_southedu2_4[,3:9])


jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_global_south$ASFR_UN_var=(sd(na.omit(asfr_UN_$Asfr))^2)

jagsdat_global_south$ESTFR_1=as.vector(unlist(estfr_d1_1[,6]))
jagsdat_global_south$ESTFR_2=as.vector(unlist(estfr_d1_2[,6]))
jagsdat_global_south$ESTFR_3=as.vector(unlist(estfr_d1_3[,6]))
jagsdat_global_south$ESTFR_4=as.vector(unlist(estfr_d1_4[,6]))


jagsdat_global_south$tfr_mean=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_global_south$tfr_sd=var(na.omit(estfr_d1_1$ESTFR))

library(matrixStats)
jagsdat_global_south$sdsasfr1=as.matrix(rowSds(as.matrix(dat_global_southeduse2_1[,3:9]))^2)
jagsdat_global_south$sdsasfr2=as.matrix(rowSds(as.matrix(dat_global_southeduse2_2[,3:9]))^2)
jagsdat_global_south$sdsasfr3=as.matrix(rowSds(as.matrix(dat_global_southeduse2_3[,3:9]))^2)
jagsdat_global_south$sdsasfr4=as.matrix(rowSds(as.matrix(dat_global_southeduse2_4[,3:9]))^2)


jagsdat_global_south$inv_sd_esasfr1=as.matrix(1/(rowSds(as.matrix(dat_global_southeduse2_1[,3:9]))^2))
jagsdat_global_south$inv_sd_esasfr2=as.matrix(1/(rowSds(as.matrix(dat_global_southeduse2_2[,3:9]))^2))
jagsdat_global_south$inv_sd_esasfr3=as.matrix(1/(rowSds(as.matrix(dat_global_southeduse2_3[,3:9]))^2))
jagsdat_global_south$inv_sd_esasfr4=as.matrix(1/(rowSds(as.matrix(dat_global_southeduse2_4[,3:9]))^2))



paramsedu_global_south =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4","ESTFR_star_1_","ESTFR_star_2_","ESTFR_star_3_","ESTFR_star_4_",
                    "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                    "var_asfr_UN","prec_esasfr1","prec_esasfr2","prec_esasfr3","prec_esasfr4","prec_estfr","sd_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


modedu_global_south = jags.model(textConnection(mod_stringedu_global_south), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south, 2e3)


mod_sim3edu_global_south = coda.samples(model=modedu_global_south,variable.names=paramsedu_global_south,
                                  n.iter=2e4)

# get summary of posterior sample
#Higher Education
MCsumedu1_global_southedu_1<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_star_1")
UPCIedu1_global_southedu_1<-MCsumedu1_global_southedu_1$`97.5%`
LWCIedu1_global_southedu_1<-MCsumedu1_global_southedu_1$`2.5%`
posterior_meansedu1_global_southedu_1=MCsumedu1_global_southedu_1$`50%`
posterior_meansedu1_global_southedu_1<-as.data.frame(posterior_meansedu1_global_southedu_1)
UPCIedu1_global_southedu_1<-as.data.frame(UPCIedu1_global_southedu_1)
LWCIedu1_global_southedu_1<-as.data.frame(LWCIedu1_global_southedu_1)
colnames(UPCIedu1_global_southedu_1)<-"Upper_CI"
colnames(LWCIedu1_global_southedu_1)<-"Lower_CI"
posterior_quantilesedu1_global_southedu_1<-cbind(UPCIedu1_global_southedu_1,LWCIedu1_global_southedu_1)
posterior_quantilesedu1_global_southedu_1<-as.data.frame(posterior_quantilesedu1_global_southedu_1)
###


#No Education
MCsumedu1_global_southedu_2<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_star_2")
UPCIedu1_global_southedu_2<-MCsumedu1_global_southedu_2$`97.5%`
LWCIedu1_global_southedu_2<-MCsumedu1_global_southedu_2$`2.5%`
posterior_meansedu1_global_southedu_2=MCsumedu1_global_southedu_2$`50%`
posterior_meansedu1_global_southedu_2<-as.data.frame(posterior_meansedu1_global_southedu_2)
UPCIedu1_global_southedu_2<-as.data.frame(UPCIedu1_global_southedu_2)
LWCIedu1_global_southedu_2<-as.data.frame(LWCIedu1_global_southedu_2)
colnames(UPCIedu1_global_southedu_2)<-"Upper_CI"
colnames(LWCIedu1_global_southedu_2)<-"Lower_CI"
posterior_quantilesedu1_global_southedu_2<-cbind(UPCIedu1_global_southedu_2,LWCIedu1_global_southedu_2)
posterior_quantilesedu1_global_southedu_2<-as.data.frame(posterior_quantilesedu1_global_southedu_2)


#Primary Education
MCsumedu1_global_southedu_3<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_star_3")
UPCIedu1_global_southedu_3<-MCsumedu1_global_southedu_3$`97.5%`
LWCIedu1_global_southedu_3<-MCsumedu1_global_southedu_3$`2.5%`
posterior_meansedu1_global_southedu_3=MCsumedu1_global_southedu_3$`50%`
posterior_meansedu1_global_southedu_3<-as.data.frame(posterior_meansedu1_global_southedu_3)
UPCIedu1_global_southedu_3<-as.data.frame(UPCIedu1_global_southedu_3)
LWCIedu1_global_southedu_3<-as.data.frame(LWCIedu1_global_southedu_3)
colnames(UPCIedu1_global_southedu_3)<-"Upper_CI"
colnames(LWCIedu1_global_southedu_3)<-"Lower_CI"
posterior_quantilesedu1_global_southedu_3<-cbind(UPCIedu1_global_southedu_3,LWCIedu1_global_southedu_3)
posterior_quantilesedu1_global_southedu_3<-as.data.frame(posterior_quantilesedu1_global_southedu_3)


#Secondary Education
MCsumedu1_global_southedu_4<-MCMCsummary(mod_sim3edu_global_south, params ="ESASFR_star_4")
UPCIedu1_global_southedu_4<-MCsumedu1_global_southedu_4$`97.5%`
LWCIedu1_global_southedu_4<-MCsumedu1_global_southedu_4$`2.5%`
posterior_meansedu1_global_southedu_4=MCsumedu1_global_southedu_4$`50%`
posterior_meansedu1_global_southedu_4<-as.data.frame(posterior_meansedu1_global_southedu_4)
UPCIedu1_global_southedu_4<-as.data.frame(UPCIedu1_global_southedu_4)
LWCIedu1_global_southedu_4<-as.data.frame(LWCIedu1_global_southedu_4)
colnames(UPCIedu1_global_southedu_4)<-"Upper_CI"
colnames(LWCIedu1_global_southedu_4)<-"Lower_CI"
posterior_quantilesedu1_global_southedu_4<-cbind(UPCIedu1_global_southedu_4,LWCIedu1_global_southedu_4)
posterior_quantilesedu1_global_southedu_4<-as.data.frame(posterior_quantilesedu1_global_southedu_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_global_southedu_1<-cbind("Mean"=posterior_meansedu1_global_southedu_1,"Upper_CI"=posterior_quantilesedu1_global_southedu_1[,1],
                            "Lower_CI"=posterior_quantilesedu1_global_southedu_1[,2])
colnames(bayesdat_global_southedu_1)[1]<-"Median"


bayesdat_global_southedu_2<-cbind("Mean"=posterior_meansedu1_global_southedu_2,"Upper_CI"=posterior_quantilesedu1_global_southedu_2[,1],
                            "Lower_CI"=posterior_quantilesedu1_global_southedu_2[,2])
colnames(bayesdat_global_southedu_2)[1]<-"Median"

bayesdat_global_southedu_3<-cbind("Mean"=posterior_meansedu1_global_southedu_3,"Upper_CI"=posterior_quantilesedu1_global_southedu_3[,1],
                            "Lower_CI"=posterior_quantilesedu1_global_southedu_3[,2])
colnames(bayesdat_global_southedu_3)[1]<-"Median"

bayesdat_global_southedu_4<-cbind("Mean"=posterior_meansedu1_global_southedu_4,"Upper_CI"=posterior_quantilesedu1_global_southedu_4[,1],
                            "Lower_CI"=posterior_quantilesedu1_global_southedu_4[,2])
colnames(bayesdat_global_southedu_4)[1]<-"Median"



e_1<-gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdat_global_south_1_<-cbind(e_1,bayesdat_global_southedu_1)


e_2<-gather(dat_global_southedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdat_global_south_2_<-cbind(e_2,bayesdat_global_southedu_2)


e_3<-gather(dat_global_southedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdat_global_south_3_<-cbind(e_3,bayesdat_global_southedu_3)



e_4<-gather(dat_global_southedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdat_global_south_4_<-cbind(e_4,bayesdat_global_southedu_4)

bayesdat_global_south1<-rbind(bayesdat_global_south_1_,bayesdat_global_south_2_)
bayesdat_global_south1<-rbind(bayesdat_global_south1,bayesdat_global_south_3_)
bayesdat_global_south1<-rbind(bayesdat_global_south1,bayesdat_global_south_4_)


bayesdat_global_south_<-bayesdat_global_south1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

dat_global_southedu_<-dat_global_southedu_%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

# looping over unique countries
Countries<-unique(dat_global_southedu2$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_global_south_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_global_south_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(dat_global_southedu_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=Pred,
                                                                       group=Education,colour="Initial Values")) +
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16))+
    labs(x="Age Group",y="ASFR",colour="Source") +
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}


pdf("./ESASFR init vs data qual glmmodel_se_sd_c_y_e.pdf", width=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()





#Plot against DHS cleaned values
Cleaned_DHS_esasfr<-read_excel("./Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")

colnames(Cleaned_DHS_esasfr)[8]<-"Age Group"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==15]<-"15-19"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==20]<-"20-24"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==25]<-"25-29"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==30]<-"30-34"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==35]<-"35-39" 
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==40]<-"40-44"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==45]<-"45-49"


Cleaned_DHS_esasfr<-Cleaned_DHS_esasfr%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_global_south_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_global_south_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(Cleaned_DHS_esasfr%>% filter(Country==Country_, !Year %in% c("1955-1960","1960-1965","1965-1970")),
              mapping=aes(x=`Age Group`,y=ASFR,group=interaction(`Survey Year`,Education), colour="DHS")) +
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=18),
          legend.title = element_text(size=16),
          strip.text.x = element_text(size=15),
          strip.text.y = element_text(size=15),
          legend.text = element_text(size=14))+
    labs(x="Age Group",y="ASFR",colour="Source") +
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}


pdf("./ESASFR_dhs vs data qual glmmodel_se_sd_c_y_e.pdf", width=15,height=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


#################################################################################################################################################################
#Check with UN values
MCsum1_UN_1<-MCMCsummary(mod_sim3edu_global_south, params ="ASFR_star")
UPCI1_UN_1<-MCsum1_UN_1$`97.5%`
LWCI1_UN_1<-MCsum1_UN_1$`2.5%`
posterior_means1_UN_1=MCsum1_UN_1$`50%`
posterior_means1_UN_1<-as.data.frame(posterior_means1_UN_1)
UPCI1_UN_1<-as.data.frame(UPCI1_UN_1)
LWCI1_UN_1<-as.data.frame(LWCI1_UN_1)
colnames(UPCI1_UN_1)<-"Upper_CI"
colnames(LWCI1_UN_1)<-"Lower_CI"
posterior_quantiles1_UN_1<-cbind(UPCI1_UN_1,LWCI1_UN_1)
posterior_quantiles1_UN_1<-as.data.frame(posterior_quantiles1_UN_1)

bayesdat_UN_1<-cbind("Mean"=posterior_means1_UN_1,"Upper_CI"=posterior_quantiles1_UN_1[,1],"Lower_CI"=posterior_quantiles1_UN_1[,2])
colnames(bayesdat_UN_1)[1]<-"Median"
bayesdat_UN_1<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_1)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_ribbon(bayesdat_UN_1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                       ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN")) +
    facet_wrap(~Year)+
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}

pdf("./ASFR UN vs data qual glmmodel_se_sd_c_y_e.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

######################################################################################################################################

#Check with TFR values
MCsum1_TFR_1<-MCMCsummary(mod_sim3edu_global_south, params ="TFR_star")
UPCI1_TFR_1<-MCsum1_TFR_1$`97.5%`
LWCI1_TFR_1<-MCsum1_TFR_1$`2.5%`
posterior_means1_TFR_1=MCsum1_TFR_1$`50%`
posterior_means1_TFR_1<-as.data.frame(posterior_means1_TFR_1)
UPCI1_TFR_1<-as.data.frame(UPCI1_TFR_1)
LWCI1_TFR_1<-as.data.frame(LWCI1_TFR_1)
colnames(UPCI1_TFR_1)<-"Upper_CI"
colnames(LWCI1_TFR_1)<-"Lower_CI"
posterior_quantiles1_TFR_1<-cbind(UPCI1_TFR_1,LWCI1_TFR_1)
posterior_quantiles1_TFR_1<-as.data.frame(posterior_quantiles1_TFR_1)

bayesdat_TFR_1<-cbind("Mean"=posterior_means1_TFR_1,"Upper_CI"=posterior_quantiles1_TFR_1[,1],"Lower_CI"=posterior_quantiles1_TFR_1[,2])
colnames(bayesdat_TFR_1)[1]<-"Median"
bayesdat_TFR_1<-cbind(dat_global_southedu2_1[,c(1,2)],bayesdat_TFR_1)

#Read in TFR values of TFR
TFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="UN_tfr")
TFR_UN<-TFR_UN%>%filter(Region%in%c("global_south","North global_south"))

TFR_UN<-ASFR_UN%>%
  group_by(`Country code`,Year,Country,Region)%>%
  summarise(TFR_=(sum(Asfr)*5))



pdf("./TFR data qaul glmmodel_se_sd_c_y_e.pdf",width = 12, onefile = TRUE)

ggplot()+  
  geom_ribbon(bayesdat_TFR_1,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
  geom_line(bayesdat_TFR_1,mapping=aes(x=Year ,y=unlist(Median),group=1,colour="Bayesian Median")) +
  
  geom_line(TFR_UN, mapping=aes(x=Year,y=TFR_,group=1,
                                colour ="UN's TFR")) +
  facet_wrap(~Country)+
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
        axis.text = element_text(size = 16),
        legend.title = element_text(size=16))+
  ggtitle(paste0("Total Fertility Rate")) + 
  labs(x="Year",y="TFR", colour="Source") +
  theme(plot.title = element_text(size = 10, hjust=0.5))

dev.off()


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

# get summary of posterior sample for ESTFR

#Higher Education
MCsumedu2_global_south_1<-MCMCsummary(mod_sim3edu_global_south, params ="ESTFR_star_1")
UPCIedu2_global_south_1<-MCsumedu2_global_south_1$`97.5%`
LWCIedu2_global_south_1<-MCsumedu2_global_south_1$`2.5%`
posterior_meansedu2_global_south_1=MCsumedu2_global_south_1$`50%`
posterior_meansedu2_global_south_1<-as.data.frame(posterior_meansedu2_global_south_1)
UPCIedu2_global_south_1<-as.data.frame(UPCIedu2_global_south_1)
LWCIedu2_global_south_1<-as.data.frame(LWCIedu2_global_south_1)
colnames(UPCIedu2_global_south_1)<-"Upper_CI"
colnames(LWCIedu2_global_south_1)<-"Lower_CI"
posterior_quantilesedu2_global_south_1<-cbind(UPCIedu2_global_south_1,LWCIedu2_global_south_1)
posterior_quantilesedu2_global_south_1<-as.data.frame(posterior_quantilesedu2_global_south_1)


###
#No Education
MCsumedu2_global_south_2<-MCMCsummary(mod_sim3edu_global_south, params ="ESTFR_star_2")
UPCIedu2_global_south_2<-MCsumedu2_global_south_2$`97.5%`
LWCIedu2_global_south_2<-MCsumedu2_global_south_2$`2.5%`
posterior_meansedu2_global_south_2=MCsumedu2_global_south_2$`50%`
posterior_meansedu2_global_south_2<-as.data.frame(posterior_meansedu2_global_south_2)
UPCIedu2_global_south_2<-as.data.frame(UPCIedu2_global_south_2)
LWCIedu2_global_south_2<-as.data.frame(LWCIedu2_global_south_2)
colnames(UPCIedu2_global_south_2)<-"Upper_CI"
colnames(LWCIedu2_global_south_2)<-"Lower_CI"
posterior_quantilesedu2_global_south_2<-cbind(UPCIedu2_global_south_2,LWCIedu2_global_south_2)
posterior_quantilesedu2_global_south_2<-as.data.frame(posterior_quantilesedu2_global_south_2)


#Primary Education
MCsumedu2_global_south_3<-MCMCsummary(mod_sim3edu_global_south, params ="ESTFR_star_3")
UPCIedu2_global_south_3<-MCsumedu2_global_south_3$`97.5%`
LWCIedu2_global_south_3<-MCsumedu2_global_south_3$`2.5%`
posterior_meansedu2_global_south_3=MCsumedu2_global_south_3$`50%`
posterior_meansedu2_global_south_3<-as.data.frame(posterior_meansedu2_global_south_3)
UPCIedu2_global_south_3<-as.data.frame(UPCIedu2_global_south_3)
LWCIedu2_global_south_3<-as.data.frame(LWCIedu2_global_south_3)
colnames(UPCIedu2_global_south_3)<-"Upper_CI"
colnames(LWCIedu2_global_south_3)<-"Lower_CI"
posterior_quantilesedu2_global_south_3<-cbind(UPCIedu2_global_south_3,LWCIedu2_global_south_3)
posterior_quantilesedu2_global_south_3<-as.data.frame(posterior_quantilesedu2_global_south_3)


#Secondary Education
MCsumedu2_global_south_4<-MCMCsummary(mod_sim3edu_global_south, params ="ESTFR_star_4")
UPCIedu2_global_south_4<-MCsumedu2_global_south_4$`97.5%`
LWCIedu2_global_south_4<-MCsumedu2_global_south_4$`2.5%`
posterior_meansedu2_global_south_4=MCsumedu2_global_south_4$`50%`
posterior_meansedu2_global_south_4<-as.data.frame(posterior_meansedu2_global_south_4)
UPCIedu2_global_south_4<-as.data.frame(UPCIedu2_global_south_4)
LWCIedu2_global_south_4<-as.data.frame(LWCIedu2_global_south_4)
colnames(UPCIedu2_global_south_4)<-"Upper_CI"
colnames(LWCIedu2_global_south_4)<-"Lower_CI"
posterior_quantilesedu2_global_south_4<-cbind(UPCIedu2_global_south_4,LWCIedu2_global_south_4)
posterior_quantilesedu2_global_south_4<-as.data.frame(posterior_quantilesedu2_global_south_4)

#############################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_global_south_12<-cbind("Mean"=posterior_meansedu2_global_south_1,"Upper_CI"=posterior_quantilesedu2_global_south_1[,1],"Lower_CI"=posterior_quantilesedu2_global_south_1[,2])
colnames(bayesdat_global_south_12)[1]<-"Median"


bayesdat_global_south_22<-cbind("Mean"=posterior_meansedu2_global_south_2,"Upper_CI"=posterior_quantilesedu2_global_south_2[,1],"Lower_CI"=posterior_quantilesedu2_global_south_2[,2])
colnames(bayesdat_global_south_22)[1]<-"Median"

bayesdat_global_south_32<-cbind("Mean"=posterior_meansedu2_global_south_3,"Upper_CI"=posterior_quantilesedu2_global_south_3[,1],"Lower_CI"=posterior_quantilesedu2_global_south_3[,2])
colnames(bayesdat_global_south_32)[1]<-"Median"

bayesdat_global_south_42<-cbind("Mean"=posterior_meansedu2_global_south_4,"Upper_CI"=posterior_quantilesedu2_global_south_4[,1],"Lower_CI"=posterior_quantilesedu2_global_south_4[,2])
colnames(bayesdat_global_south_42)[1]<-"Median"




e_12<-dat_global_southedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_global_south_12_<-cbind(e_12,bayesdat_global_south_12)

e_22<-dat_global_southedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_global_south_22_<-cbind(e_22,bayesdat_global_south_22)


e_32<-dat_global_southedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_global_south_32_<-cbind(e_32,bayesdat_global_south_32)

e_42<-dat_global_southedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_global_south_42_<-cbind(e_42,bayesdat_global_south_42)


bayesdat_global_south2<-rbind(bayesdat_global_south_12_,bayesdat_global_south_22_)
bayesdat_global_south2<-rbind(bayesdat_global_south2,bayesdat_global_south_32_)
bayesdat_global_south2<-rbind(bayesdat_global_south2,bayesdat_global_south_42_)





#Plot with DHS cleaned data set
cleaned_DHS<-read_excel("./Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESTFR_5")
cleaned_DHS<-cleaned_DHS%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


# looping over unique countries
bayesdat_global_south2<-bayesdat_global_south2%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))



pdf("./ESTFR_dhs vs data qual glmmodel_se_sd_c_y_e.pdf",width = 12, onefile = TRUE)

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_global_south2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                          ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_global_south2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
    geom_point(cleaned_DHS%>% filter(Country==Country_, !Year %in% c("1955-1960","1960-1965","1965-1970")),
               mapping=aes(x=Year,y=TFR,group=interaction(`Survey Year`,Education), colour="DHS")) +
    theme(plot.title = element_text(size = 16, hjust=0.5),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 10, hjust=0.5))+ 
    labs(x="Year",y="ESTFR",colour="Estimates")+
    ggtitle(paste0("Education Specific Total Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}

dev.off()



#Save results in Excel

library(writexl)

bayesdat_global_southesasfr<-bayesdat_global_south_%>%dplyr::select("Country","Age Group","Education","Year","Upper_CI","Lower_CI","Median")
bayesdat_global_southestfr<-bayesdat_global_south2%>%dplyr::select("Country","Year","Education","Upper_CI","Lower_CI","Median")
bayesdat_global_southasfr<-bayesdat_UN_1%>%dplyr::select("Country","Year","Age Group","Upper_CI","Lower_CI","Median")
bayesdat_global_southtfr<-bayesdat_TFR_1%>%dplyr::select("Country","Year","Upper_CI","Lower_CI","Median")





write_xlsx(list("BESASFR"=bayesdat_global_southesasfr,"BESTFR"=bayesdat_global_southestfr,
                "ASFR"=bayesdat_global_southasfr,"TFR"=bayesdat_global_southtfr),
           path ="./BESFR_data qaul glmmodel_se_sd_c_y_e.xlsx", col_names=TRUE)



