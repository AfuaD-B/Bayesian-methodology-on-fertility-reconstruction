#In this model the focus is on Education specific rates, this is the validation for leaving out some countries.
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

dat_global_southedu2_<-read_excel("./All countries Bayesian codes/GLM regional/glm_predict_all_reg3.xlsx")%>%
  filter(!Country%in%c("Papua New Guinea","Uzbekistan","Ukraine"))
dat_global_southedu2_<-dat_global_southedu2_[order(dat_global_southedu2_$Country), -4]


cleaned_DHS_ASFR<-read_excel("./Cleaned Data All_DHS/Cleaned_DHS updated.xlsx",sheet="ASFR_5")

cleaned_DHS_ASFR<-cleaned_DHS_ASFR%>%filter(!Country%in%c("Papua New Guinea","Uzbekistan","Ukraine"))
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==15]<-"15-19"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==20]<-"20-24"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==25]<-"25-29"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==30]<-"30-34"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==35]<-"35-39"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==40]<-"40-44"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==45]<-"45-49"


#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(!Country%in%c("Papua New Guinea","Uzbekistan","Ukraine"), !Year%in%c("1955-1960", "1960-1965", "1965-1970"))


asfr_UN2<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  ifelse(Country=="Ghana", NA,Asfr))


asfr_UN2<-spread(asfr_UN2[,-c(1,5)],`Age Group`,Asfr)

TFR_global_southedu<-read.csv("K:/project/BayesEdu/Fertility/ESTFR/cc_y_edu_all_paper_models.csv")%>%filter(Model.name=="UN-fully consistent",Chain=="1")
TFR_global_southedu$Country[TFR_global_southedu$Country=="C\xf4te D'Ivoire"]<-"Côte D'Ivoire"
TFR_global_southedu<-as.data.frame(TFR_global_southedu)
Encoding(TFR_global_southedu$Country) <- "UTF-8"



for_join<-unique(dat_global_southedu2_[,c(1:4)])
TFR_global_southedu<-TFR_global_southedu%>%filter(Country%in%c(unique(dat_global_southedu2_$Country)),Year%in%c(unique(dat_global_southedu2_$Year)))
TFR_global_southedu2_<-unique(full_join(for_join,TFR_global_southedu,by=c("Country","Year","Education")))

#ESFTR by level of education
estfr_d1_2<-TFR_global_southedu2_%>%filter(Education=="No Education")
estfr_d1_2<-estfr_d1_2[order(estfr_d1_2$Country), ]

estfr_d1_3<-TFR_global_southedu2_%>%filter(Education=="Primary Education")
estfr_d1_3<-estfr_d1_3[order(estfr_d1_3$Country), ]


estfr_d1_4<-TFR_global_southedu2_%>%filter(Education=="Secondary Education")
estfr_d1_4<-estfr_d1_4[order(estfr_d1_4$Country), ]

estfr_d1_1<-TFR_global_southedu2_%>%filter(Education=="Higher Education")
estfr_d1_1<-estfr_d1_1[order(estfr_d1_1$Country), ]



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


##
mod_stringedu_global_south_na = "model {
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
      
      
  
      
      ESASFR_star_1[i,j]~dnorm(ESASFR_dhs_1[i,j],prec_esasfr1[i,j])T(0,)
     
      ESASFR_star_2[i,j]~dnorm(ESASFR_dhs_2[i,j],prec_esasfr2[i,j])T(0,)
      
      ESASFR_star_3[i,j]~dnorm(ESASFR_dhs_3[i,j],prec_esasfr3[i,j])T(0,)
      
      ESASFR_star_4[i,j]~dnorm(ESASFR_dhs_4[i,j],prec_esasfr4[i,j])T(0,)
      
         
    
    ASFR_star[i,j]=ESASFR_star_1[i,j]*w_1[i,j] +  ESASFR_star_2[i,j]*w_2[i,j] + ESASFR_star_3[i,j]*w_3[i,j] + ESASFR_star_4[i,j]*w_4[i,j]
      
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





####We arrange the data in such a way that is compatible with the written model code
dat_global_southedu_<-dat_global_southedu2_[order(dat_global_southedu2_$Country), ]%>%
  filter(Country%in%c(unique(asfr_UN_$Country)))


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



set.seed(122)
jagsdat_global_south = as.list(dat_global_southedu2_1)
jagsdat_global_south$period=as.matrix(dat_global_southedu2_1[,1])
jagsdat_global_south$Age=as.matrix(dat_global_southedu2_1[1,3:9])
jagsdat_global_south$pop_wic_1=as.matrix(wice_dat_global_south_1[,4:10])
jagsdat_global_south$pop_wic_2=as.matrix(wice_dat_global_south_2[,4:10])
jagsdat_global_south$pop_wic_3=as.matrix(wice_dat_global_south_3[,4:10])
jagsdat_global_south$pop_wic_4=as.matrix(wice_dat_global_south_4[,4:10])

jagsdat_global_south$ESASFR_dhs_1=as.matrix(dat_global_southedu2_1[,3:9])
jagsdat_global_south$ESASFR_dhs_2=as.matrix(dat_global_southedu2_2[,3:9])
jagsdat_global_south$ESASFR_dhs_3=as.matrix(dat_global_southedu2_3[,3:9])
jagsdat_global_south$ESASFR_dhs_4=as.matrix(dat_global_southedu2_4[,3:9])



jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN2[,3:9])
jagsdat_global_south$ASFR_UN_var=(sd(na.omit(asfr_UN_$Asfr))^2)

jagsdat_global_south$ESTFR_1=as.vector(unlist(estfr_d1_1[,6]))
jagsdat_global_south$ESTFR_2=as.vector(unlist(estfr_d1_2[,6]))
jagsdat_global_south$ESTFR_3=as.vector(unlist(estfr_d1_3[,6]))
jagsdat_global_south$ESTFR_4=as.vector(unlist(estfr_d1_4[,6]))


jagsdat_global_south$tfr_sd=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_global_south$tfr_var=var(na.omit(estfr_d1_1$ESTFR))



jagsdat_global_south$s_esasfr1=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:4)])%>%
                                                                     spread(`Age Group`,`Higher Education`))[,3:9]))))

jagsdat_global_south$s_esasfr2=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,5)])%>%
                                                                     spread(`Age Group`,`No Education`))[,3:9]))))

jagsdat_global_south$s_esasfr3=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,6)])%>%
                                                                     spread(`Age Group`,`Primary Education`))[,3:9]))))

jagsdat_global_south$s_esasfr4=as.matrix((rowSds(as.matrix(na.omit(as.data.frame(dat_global_southedu_se2[,c(1:3,7)])%>%
                                                                     spread(`Age Group`,`Secondary Education`))[,3:9]))))




paramsedu_global_south =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4","ESTFR_star_1_","ESTFR_star_2_","ESTFR_star_3_","ESTFR_star_4_",
                          "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                          "var_asfr_UN","prec_esasfr1","prec_esasfr2","prec_esasfr3","prec_esasfr4","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


modedu_global_south_na1 = jags.model(textConnection(mod_stringedu_global_south_na), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south_na1,2e3)


mod_sim3edu_global_south_na1 = coda.samples(model=modedu_global_south_na1,variable.names=paramsedu_global_south,
                                            n.iter=2e4)

# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na1<-MCMCsummary(mod_sim3edu_global_south_na1, params ="ASFR_star")
UPCI1_UN_na1<-MCsum1_UN_na1$`97.5%`
LWCI1_UN_na1<-MCsum1_UN_na1$`2.5%`
posterior_means1_UN_na1=MCsum1_UN_na1$`50%`
posterior_means1_UN_na1<-as.data.frame(posterior_means1_UN_na1)
UPCI1_UN_na1<-as.data.frame(UPCI1_UN_na1)
LWCI1_UN_na1<-as.data.frame(LWCI1_UN_na1)
colnames(UPCI1_UN_na1)<-"Upper_CI"
colnames(LWCI1_UN_na1)<-"Lower_CI"
posterior_quantiles1_UN_na1<-cbind(UPCI1_UN_na1,LWCI1_UN_na1)
posterior_quantiles1_UN_na1<-as.data.frame(posterior_quantiles1_UN_na1)

bayesdat_UN_na1<-cbind("Mean"=posterior_means1_UN_na1,"Upper_CI"=posterior_quantiles1_UN_na1[,1],
                       "Lower_CI"=posterior_quantiles1_UN_na1[,2])
colnames(bayesdat_UN_na1)[1]<-"Median"
bayesdat_UN_na1<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na1)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
# looping over unique countries
Countries<-unique(bayesdat_UN_na1$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1),
              colour ="black",
              colour ="UN") +
    geom_ribbon(bayesdat_UN_na1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Ghana_ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()






####DHS comparison
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,!Year%in%c("1960-1965","1965-1970","2015-2020")), 
              mapping=aes(x=Age ,y=ASFR,group=`Survey Year`,colour =`Survey Year`)) +
    
    geom_ribbon(bayesdat_UN_na1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na1%>% filter(Country==Country_),
              mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Ghana_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


######################################################################################################################################

##Try for 10%
#Read in UN data
asfr_UN3<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  ifelse(Country=="Bangladesh", NA,Asfr))


asfr_UN3<-spread(asfr_UN3[,-c(1,5)],`Age Group`,Asfr)


jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN3[,3:9])

modedu_global_south_na2 = jags.model(textConnection(mod_stringedu_global_south_na), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south_na2, 2e3)


mod_sim3edu_global_south_na2 = coda.samples(model=modedu_global_south_na2,variable.names=paramsedu_global_south,
                                            n.iter=2e4)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na2<-MCMCsummary(mod_sim3edu_global_south_na2, params ="ASFR_star")
UPCI1_UN_na2<-MCsum1_UN_na2$`97.5%`
LWCI1_UN_na2<-MCsum1_UN_na2$`2.5%`
posterior_means1_UN_na2=MCsum1_UN_na2$`50%`
posterior_means1_UN_na2<-as.data.frame(posterior_means1_UN_na2)
UPCI1_UN_na2<-as.data.frame(UPCI1_UN_na2)
LWCI1_UN_na2<-as.data.frame(LWCI1_UN_na2)
colnames(UPCI1_UN_na2)<-"Upper_CI"
colnames(LWCI1_UN_na2)<-"Lower_CI"
posterior_quantiles1_UN_na2<-cbind(UPCI1_UN_na2,LWCI1_UN_na2)
posterior_quantiles1_UN_na2<-as.data.frame(posterior_quantiles1_UN_na2)

bayesdat_UN_na2<-cbind("Mean"=posterior_means1_UN_na2,"Upper_CI"=posterior_quantiles1_UN_na2[,1],
                       "Lower_CI"=posterior_quantiles1_UN_na2[,2])
colnames(bayesdat_UN_na2)[1]<-"Median"
bayesdat_UN_na2<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na2)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN")) +
    
    geom_ribbon(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Bangladesh_ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()




##################DHS comparison

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,!Year%in%c("1960-1965","1965-1970","2015-2020")), 
              mapping=aes(x=Age,y=ASFR,group=`Survey Year`,colour =`Survey Year`)) +
    
    geom_ribbon(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Banglash_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

#########################################################################################################################
#########################################################################################################################

##Try for 15%
#Read in UN data
asfr_UN4<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  ifelse(Country=="Albania", NA,Asfr))


asfr_UN4<-spread(asfr_UN4[,-c(1,5)],`Age Group`,Asfr)


####We arrange the data in such a way that is compatible with the written model code
jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN4[,3:9])

modedu_global_south_na3 = jags.model(textConnection(mod_stringedu_global_south_na), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south_na3, 2e3)


mod_sim3edu_global_south_na3 = coda.samples(model=modedu_global_south_na3,variable.names=paramsedu_global_south,
                                            n.iter=2e4)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na3<-MCMCsummary(mod_sim3edu_global_south_na3, params ="ASFR_star")
UPCI1_UN_na3<-MCsum1_UN_na3$`97.5%`
LWCI1_UN_na3<-MCsum1_UN_na3$`2.5%`
posterior_means1_UN_na3=MCsum1_UN_na3$`50%`
posterior_means1_UN_na3<-as.data.frame(posterior_means1_UN_na3)
UPCI1_UN_na3<-as.data.frame(UPCI1_UN_na3)
LWCI1_UN_na3<-as.data.frame(LWCI1_UN_na3)
colnames(UPCI1_UN_na3)<-"Upper_CI"
colnames(LWCI1_UN_na3)<-"Lower_CI"
posterior_quantiles1_UN_na3<-cbind(UPCI1_UN_na3,LWCI1_UN_na3)
posterior_quantiles1_UN_na3<-as.data.frame(posterior_quantiles1_UN_na3)

bayesdat_UN_na3<-cbind("Mean"=posterior_means1_UN_na3,"Upper_CI"=posterior_quantiles1_UN_na3[,1],
                       "Lower_CI"=posterior_quantiles1_UN_na3[,2])
colnames(bayesdat_UN_na3)[1]<-"Median"
bayesdat_UN_na3<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na3)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN")) +
    
    geom_ribbon(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Albania_ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



##################DHS comparison

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,!Year%in%c("1960-1965","1965-1970","2015-2020")), 
              mapping=aes(x=Age,y=ASFR,group=`Survey Year`,colour =`Survey Year`)) +
    
    geom_ribbon(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Albania_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

#########################################################################################################################
#########################################################################################################################

##Try for 20%
#Read in UN data
asfr_UN5<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  ifelse(Country=="Colombia", NA,Asfr))


asfr_UN5<-spread(asfr_UN5[,-c(1,5)],`Age Group`,Asfr)


jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN5[,3:9])
modedu_global_south_na4 = jags.model(textConnection(mod_stringedu_global_south_na), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south_na4, 2e3)


mod_sim3edu_global_south_na4 = coda.samples(model=modedu_global_south_na4,variable.names=paramsedu_global_south,
                                            n.iter=2e4)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na4<-MCMCsummary(mod_sim3edu_global_south_na4, params ="ASFR_star")
UPCI1_UN_na4<-MCsum1_UN_na4$`97.5%`
LWCI1_UN_na4<-MCsum1_UN_na4$`2.5%`
posterior_means1_UN_na4=MCsum1_UN_na4$`50%`
posterior_means1_UN_na4<-as.data.frame(posterior_means1_UN_na4)
UPCI1_UN_na4<-as.data.frame(UPCI1_UN_na4)
LWCI1_UN_na4<-as.data.frame(LWCI1_UN_na4)
colnames(UPCI1_UN_na4)<-"Upper_CI"
colnames(LWCI1_UN_na4)<-"Lower_CI"
posterior_quantiles1_UN_na4<-cbind(UPCI1_UN_na4,LWCI1_UN_na4)
posterior_quantiles1_UN_na4<-as.data.frame(posterior_quantiles1_UN_na4)

bayesdat_UN_na4<-cbind("Mean"=posterior_means1_UN_na4,"Upper_CI"=posterior_quantiles1_UN_na4[,1],
                       "Lower_CI"=posterior_quantiles1_UN_na4[,2])
colnames(bayesdat_UN_na4)[1]<-"Median"
bayesdat_UN_na4<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na4)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN"),colour="black") +
    
    geom_ribbon(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Colombia_ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



##################DHS comparison

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,!Year%in%c("1960-1965","1965-1970","2015-2020")), 
              mapping=aes(x=Age,y=ASFR,group=`Survey Year`,colour =`Survey Year`)) +
    
    geom_ribbon(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    scale_color_brewer()+
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Colombia_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

#########################################################################################################################
#########################################################################################################################

##Try for 30%
#Read in UN data
asfr_UN6<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  ifelse(Country=="Philippines", NA,Asfr))


asfr_UN6<-spread(asfr_UN6[,-c(1,5)],`Age Group`,Asfr)



jagsdat_global_south$ASFR_UN=as.matrix(asfr_UN6[,3:9])

modedu_global_south_na5 = jags.model(textConnection(mod_stringedu_global_south_na), data=jagsdat_global_south,n.chains = 5)
update(modedu_global_south_na5, 2e3)


mod_sim3edu_global_south_na5 = coda.samples(model=modedu_global_south_na5,variable.names=paramsedu_global_south,
                                            n.iter=2e4)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na5<-MCMCsummary(mod_sim3edu_global_south_na5, params ="ASFR_star")
UPCI1_UN_na5<-MCsum1_UN_na5$`97.5%`
LWCI1_UN_na5<-MCsum1_UN_na5$`2.5%`
posterior_means1_UN_na5=MCsum1_UN_na5$`50%`
posterior_means1_UN_na5<-as.data.frame(posterior_means1_UN_na5)
UPCI1_UN_na5<-as.data.frame(UPCI1_UN_na5)
LWCI1_UN_na5<-as.data.frame(LWCI1_UN_na5)
colnames(UPCI1_UN_na5)<-"Upper_CI"
colnames(LWCI1_UN_na5)<-"Lower_CI"
posterior_quantiles1_UN_na5<-cbind(UPCI1_UN_na5,LWCI1_UN_na5)
posterior_quantiles1_UN_na5<-as.data.frame(posterior_quantiles1_UN_na5)

bayesdat_UN_na5<-cbind("Mean"=posterior_means1_UN_na5,"Upper_CI"=posterior_quantiles1_UN_na5[,1],
                       "Lower_CI"=posterior_quantiles1_UN_na5[,2])
colnames(bayesdat_UN_na5)[1]<-"Median"
bayesdat_UN_na5<-cbind(gather(dat_global_southedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na5)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN"),colour="black") +
    geom_ribbon(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Philippines_ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()






##################DHS comparison

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,!Year%in%c("1960-1965","1965-1970","2015-2020")), 
              mapping=aes(x=Age,y=ASFR,group=`Survey Year`,colour =`Survey Year`)) +
    
    geom_ribbon(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    scale_color_brewer()+
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/NA_Philippines_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



########################################3
#Make scatter plots

#Plotting for ASFR main model

UN_na1<-ggplot()+
  geom_line(ASFR_UN, mapping = aes(x=Asfr,y=Asfr,group=Country), size=3, alpha=0.03)+
  geom_point(bayesdat_UN_na1, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na1, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="UN", x = "5% NA")+
  theme_bw()

UN_na2<-ggplot()+
  geom_line(ASFR_UN, mapping = aes(x=Asfr,y=Asfr,group=Country), size=3, alpha=0.03)+
  geom_point(bayesdat_UN_na2, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na2, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="UN", x = "10% NA")+
  theme_bw()


UN_na3<-ggplot()+
  geom_line(ASFR_UN, mapping = aes(x=Asfr,y=Asfr,group=Country), size=3, alpha=0.03)+
  geom_point(bayesdat_UN_na3, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na3, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="UN", x = "15% NA")+
  theme_bw()




UN_na4<-ggplot()+
  geom_line(ASFR_UN, mapping = aes(x=Asfr,y=Asfr,group=Country), size=3, alpha=0.03)+
  geom_point(bayesdat_UN_na4, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na4, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="UN", x = "20% NA")+
  theme_bw()



UN_na5<-ggplot()+
  geom_line(ASFR_UN, mapping = aes(x=Asfr,y=Asfr,group=Country), size=3, alpha=0.03)+
  geom_point(bayesdat_UN_na5, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na5, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="UN", x = "30% NA")+
  theme_bw()


##Plotting for ASFR with DHS main model

DHS_na1<-ggplot()+
  geom_line(cleaned_DHS_ASFR, mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(bayesdat_UN_na1, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na1, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="DHS",x="5% NA")+
  theme_bw()


DHS_na2<-ggplot()+
  geom_line(cleaned_DHS_ASFR, mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(bayesdat_UN_na2, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na2, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="DHS",x="10% NA")+
  theme_bw()



DHS_na3<-ggplot()+
  geom_line(cleaned_DHS_ASFR, mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(bayesdat_UN_na3, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na3, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="DHS",x="15% NA")+
  theme_bw()


DHS_na4<-ggplot()+
  geom_line(cleaned_DHS_ASFR, mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(bayesdat_UN_na4, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na4, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="DHS",x="20% NA")+
  theme_bw()



DHS_na5<-ggplot()+
  geom_line(cleaned_DHS_ASFR, mapping = aes(x=ASFR,y=ASFR,group=interaction(Country,`Survey Year`)))+
  geom_point(bayesdat_UN_na5, mapping = aes(x=Median,y=Median, group=Country), size=3, alpha=0.03)+
  geom_errorbarh(bayesdat_UN_na5, mapping = aes(xmin=Lower_CI,xmax=Upper_CI,y=Median, group=Country), size=3, alpha=0.03)+
  labs(y="DHS",x="30% NA")+
  theme_bw()



library(gridExtra)

pdf("./All countries Bayesian codes/Validation/NA Country missing ASFR model comparison.pdf",width = 12,height=28, onefile = TRUE)
grid.arrange(UN_na1, DHS_na1, UN_na2, DHS_na2, UN_na3, DHS_na3, UN_na4, DHS_na4, UN_na5, DHS_na5, ncol = 2)
dev.off()




#Comparing UN and DHS ASFR values
Countries<-unique(dat_global_southedu2_$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,Year%in%c(unique(dat_global_southedu2_$Year))), mapping=aes(x=Age,y=ASFR,group=`Survey Year`,
                                                                                                                       colour =`Survey Year`)) +
    
    geom_line(ASFR_UN%>% filter(Country==Country_,Year%in%c(unique(dat_global_southedu2_$Year))), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                                                              colour ="UN")) +
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./All countries Bayesian codes/Validation/countries DHS vs UN.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



Countries<-unique(asfr_UN_$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_line(asfr_UN_[,-1]%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,colour ="UN")) +
    geom_ribbon(bayesdat_UN_na1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1)+
    geom_line(bayesdat_UN_na1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="5%")) +
    
    
    geom_ribbon(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1)+
    geom_line(bayesdat_UN_na2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="10%")) +
    
    
    geom_ribbon(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1)+
    geom_line(bayesdat_UN_na3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="15%")) +
    
    
    geom_ribbon(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1)+
    geom_line(bayesdat_UN_na4%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="20%")) +
    
    
    
    geom_ribbon(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=1),alpha=0.1)+
    geom_line(bayesdat_UN_na5%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="30%")) +
    
    facet_wrap(~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}   

pdf("./All countries Bayesian codes/Validation/NA Country UN.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



