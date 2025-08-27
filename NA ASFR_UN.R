library(dplyr)
#In this model the focus is on Education specific rates. We include UN births in the Education specific Bayesian model
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


dat_africaedu2_<-read_excel("./Bayesglmmodel_estimates.xlsx")%>%filter(Region%in%c("Africa","North Africa"))

cleaned_DHS_ASFR<-read_excel("./Cleaned Data All_DHS/Cleaned_DHS_Africa.xlsx",sheet="ASFR_5")

cleaned_DHS_ASFR<-cleaned_DHS_ASFR%>%filter(Region%in%c("Africa","North Africa"))
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==15]<-"15-19"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==20]<-"20-24"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==25]<-"25-29"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==30]<-"30-34"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==35]<-"35-39"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==40]<-"40-44"
cleaned_DHS_ASFR$Age[cleaned_DHS_ASFR$Age==45]<-"45-49"


#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region%in%c("Africa","North Africa"), !Year%in%c("1955-1960", "1960-1965", "1965-1970"))


asfr_UN2<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  replace(Asfr, sample(row_number(),  
                                        size = ceiling(0.05 * n()), replace = FALSE), NA))


asfr_UN2<-spread(asfr_UN2[,-c(1,5)],`Age Group`,Asfr)

TFR_africaedu<-read.csv("K:/project/BayesEdu/Fertility/ESTFR/cc_y_edu_all_paper_models.csv")%>%filter(Model.name=="UN-fully consistent",Chain=="1")
TFR_africaedu<-as.data.frame(TFR_africaedu)
Encoding(TFR_africaedu$Country) <- "UTF-8"



for_join<-unique(dat_africaedu2_[,c(1:3)])
TFR_africaedu<-TFR_africaedu%>%filter(Country%in%c(unique(dat_africaedu2_$Country)),Year%in%c(unique(dat_africaedu2_$Year)))
TFR_africaedu2_<-unique(full_join(for_join,TFR_africaedu,by=c("Country","Year","Education")))

#ESFTR by level of education
estfr_d1_2<-TFR_africaedu2_%>%filter(Education=="No Education")
estfr_d1_2<-estfr_d1_2[order(estfr_d1_2$Country), ]

estfr_d1_3<-TFR_africaedu2_%>%filter(Education=="Primary Education")
estfr_d1_3<-estfr_d1_3[order(estfr_d1_3$Country), ]


estfr_d1_4<-TFR_africaedu2_%>%filter(Education=="Secondary Education")
estfr_d1_4<-estfr_d1_4[order(estfr_d1_4$Country), ]

estfr_d1_1<-TFR_africaedu2_%>%filter(Education=="Higher Education")
estfr_d1_1<-estfr_d1_1[order(estfr_d1_1$Country), ]




###Bringing the WIC data in to be population of women by level of education
wice_dat_africa_1<-read_excel("./WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_africa_1<-wice_dat_africa_1%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


wice_dat_africa_2<-read_excel("./WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_africa_2<-wice_dat_africa_2%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


wice_dat_africa_3<-read_excel("./WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_africa_3<-wice_dat_africa_3%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


wice_dat_africa_4<-read_excel("./WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_africa_4<-wice_dat_africa_4%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


##The main education model code
mod_stringedu_africa_na1 = "model {
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
     prec_esasfr1[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr2[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr3[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr4[i,1]~dgamma(s_esasfr1,sesasfr1)

  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"


####We arrange the data in such a way that is compatible with the written model code
dat_africaedu_<-dat_africaedu2_[order(dat_africaedu2_$Country), ]

dat_africaedu2<-dat_africaedu_[,-c(5,7,8)]%>%distinct()%>%spread(Education,Pred)


dat_africaedu2_1<-dat_africaedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_africaedu2_1<-dat_africaedu2_1[order(dat_africaedu2_1$Country), ]


dat_africaedu2_2<-dat_africaedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_africaedu2_2<-dat_africaedu2_2[order(dat_africaedu2_2$Country), ]

dat_africaedu2_3<-dat_africaedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_africaedu2_3<-dat_africaedu2_3[order(dat_africaedu2_3$Country), ]

dat_africaedu2_4<-dat_africaedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_africaedu2_4<-dat_africaedu2_4[order(dat_africaedu2_4$Country), ]

dat_africaedu_<-dat_africaedu_[order(dat_africaedu_$Country), ]




library(matrixStats)

dat_africaedu_se2<-dat_africaedu2_[,-c(5,6,8)]%>%distinct()%>%spread(Education,SE)
dat_africaedu_se2<-dat_africaedu_se2[order(dat_africaedu_se2$Country), ]


set.seed(122)
jagsdat_africa = as.list(dat_africaedu2_1)
jagsdat_africa$period=as.matrix(dat_africaedu2_1[,2])
jagsdat_africa$Age=as.matrix(dat_africaedu2_1[1,3:9])
jagsdat_africa$pop_wic_1=as.matrix(wice_dat_africa_1[,4:10])
jagsdat_africa$pop_wic_2=as.matrix(wice_dat_africa_2[,4:10])
jagsdat_africa$pop_wic_3=as.matrix(wice_dat_africa_3[,4:10])
jagsdat_africa$pop_wic_4=as.matrix(wice_dat_africa_4[,4:10])
jagsdat_africa$ESASFR_dhs_1=as.matrix(dat_africaedu2_1[,3:9])
jagsdat_africa$ESASFR_dhs_2=as.matrix(dat_africaedu2_2[,3:9])
jagsdat_africa$ESASFR_dhs_3=as.matrix(dat_africaedu2_3[,3:9])
jagsdat_africa$ESASFR_dhs_4=as.matrix(dat_africaedu2_4[,3:9])

jagsdat_africa$ASFR_UN=as.matrix(asfr_UN2[,3:9])
jagsdat_africa$ASFR_UN_var=(sd(asfr_UN_$Asfr)^2)

jagsdat_africa$ESTFR_1=as.vector(unlist(estfr_d1_1[,5]))
jagsdat_africa$ESTFR_2=as.vector(unlist(estfr_d1_2[,5]))
jagsdat_africa$ESTFR_3=as.vector(unlist(estfr_d1_3[,5]))
jagsdat_africa$ESTFR_4=as.vector(unlist(estfr_d1_4[,5]))


jagsdat_africa$tfr_mean=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_africa$tfr_sd=var(na.omit(estfr_d1_1$ESTFR))



jagsdat_africa$sesasfr1=2*(sd(dat_africaedu_se2$`No Education`)^2)

jagsdat_africa$s_esasfr1=1/((sd(dat_africaedu_se2$`No Education`)))


paramsedu_africa =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4",
                    "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                    "var_asfr_UN","prec_esasfr1","prec_esasfr2","prec_esasfr3","prec_esasfr4","prec_estfr","sd_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")



modedu_africa_na1 = jags.model(textConnection(mod_stringedu_africa_na1), data=jagsdat_africa,n.chains = 3)
update(modedu_africa_na1, 2e4)


mod_sim3edu_africa_na1 = coda.samples(model=modedu_africa_na1,variable.names=paramsedu_africa,
                                   n.iter=2e4,thin = 200)

# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na1<-MCMCsummary(mod_sim3edu_africa_na1, params ="ASFR_star")
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
bayesdat_UN_na1<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na1)

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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.05_ASFR.pdf",width = 12, onefile = TRUE)
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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.05_ASFR_DHS.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


######################################################################################################################################

##Try for 10%
#Read in UN data
asfr_UN3<-asfr_UN_ %>%
  group_by(Country) %>%
  mutate(Asfr =  replace(Asfr, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))


asfr_UN3<-spread(asfr_UN3[,-c(1,5)],`Age Group`,Asfr)


##The main education model code
mod_stringedu_africa_na2 = "model {
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
    prec_esasfr1[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr2[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr3[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr4[i,1]~dgamma(s_esasfr1,sesasfr1)

  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"


jagsdat_africa$ASFR_UN=as.matrix(asfr_UN3[,3:9])

modedu_africa_na2 = jags.model(textConnection(mod_stringedu_africa_na2), data=jagsdat_africa,n.chains = 3)
update(modedu_africa_na2, 2e4)


mod_sim3edu_africa_na2 = coda.samples(model=modedu_africa_na2,variable.names=paramsedu_africa,
                                      n.iter=2e4,thin = 200)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na2<-MCMCsummary(mod_sim3edu_africa_na2, params ="ASFR_star")
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
bayesdat_UN_na2<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na2)

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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.10_ASFR.pdf",width = 12, onefile = TRUE)
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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.10_ASFR_DHS.pdf",width = 12, onefile = TRUE)
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
  mutate(Asfr =  replace(Asfr, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))


asfr_UN4<-spread(asfr_UN4[,-c(1,5)],`Age Group`,Asfr)


##The main education model code
mod_stringedu_africa_na3 = "model {
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
    prec_esasfr1[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr2[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr3[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr4[i,1]~dgamma(s_esasfr1,sesasfr1)

  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"


####We arrange the data in such a way that is compatible with the written model code
jagsdat_africa$ASFR_UN=as.matrix(asfr_UN4[,3:9])

modedu_africa_na3 = jags.model(textConnection(mod_stringedu_africa_na3), data=jagsdat_africa,n.chains = 3)
update(modedu_africa_na3, 2e4)


mod_sim3edu_africa_na3 = coda.samples(model=modedu_africa_na3,variable.names=paramsedu_africa,
                                      n.iter=2e4,thin = 200)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na3<-MCMCsummary(mod_sim3edu_africa_na3, params ="ASFR_star")
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
bayesdat_UN_na3<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na3)

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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.15_ASFR.pdf",width = 12, onefile = TRUE)
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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.15_ASFR_DHS.pdf",width = 12, onefile = TRUE)
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
  mutate(Asfr =  replace(Asfr, sample(row_number(),  
                                      size = ceiling(0.2 * n()), replace = FALSE), NA))


asfr_UN5<-spread(asfr_UN5[,-c(1,5)],`Age Group`,Asfr)


##The main education model code
mod_stringedu_africa_na4 = "model {
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
    prec_esasfr1[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr2[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr3[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr4[i,1]~dgamma(s_esasfr1,sesasfr1)

  
  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"

jagsdat_africa$ASFR_UN=as.matrix(asfr_UN5[,3:9])
modedu_africa_na4 = jags.model(textConnection(mod_stringedu_africa_na4), data=jagsdat_africa,n.chains = 3)
update(modedu_africa_na4, 2e4)


mod_sim3edu_africa_na4 = coda.samples(model=modedu_africa_na4,variable.names=paramsedu_africa,
                                      n.iter=2e4,thin = 200)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na4<-MCMCsummary(mod_sim3edu_africa_na4, params ="ASFR_star")
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
bayesdat_UN_na4<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na4)

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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.2_ASFR.pdf",width = 12, onefile = TRUE)
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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.2_ASFR_DHS.pdf",width = 12, onefile = TRUE)
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
  mutate(Asfr =  replace(Asfr, sample(row_number(),  
                                      size = ceiling(0.30 * n()), replace = FALSE), NA))


asfr_UN6<-spread(asfr_UN6[,-c(1,5)],`Age Group`,Asfr)


##The main education model code
mod_stringedu_africa_na5 = "model {
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
    prec_esasfr1[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr2[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr3[i,1]~dgamma(s_esasfr1,sesasfr1)
     prec_esasfr4[i,1]~dgamma(s_esasfr1,sesasfr1)

  
  prec_estfr1[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr2[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr3[i]~dgamma(tfr_mean,tfr_sd)
  prec_estfr4[i]~dgamma(tfr_mean,tfr_sd)
}
  prec_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"

jagsdat_africa$ASFR_UN=as.matrix(asfr_UN6[,3:9])

modedu_africa_na5 = jags.model(textConnection(mod_stringedu_africa_na5), data=jagsdat_africa,n.chains = 3)
update(modedu_africa_na5, 2e4)


mod_sim3edu_africa_na5 = coda.samples(model=modedu_africa_na5,variable.names=paramsedu_africa,
                                      n.iter=2e4,thin = 200)



# get summary of posterior sample
#################################################################################################################################################
##Check with UN values
MCsum1_UN_na5<-MCMCsummary(mod_sim3edu_africa_na5, params ="ASFR_star")
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
bayesdat_UN_na5<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_na5)

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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.30_ASFR.pdf",width = 12, onefile = TRUE)
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


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA_0.30_ASFR_DHS.pdf",width = 12, onefile = TRUE)
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

pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NA ASFR model comparison.pdf",width = 12,height=28, onefile = TRUE)
grid.arrange(UN_na1, DHS_na1, UN_na2, DHS_na2, UN_na3, DHS_na3, UN_na4, DHS_na4, UN_na5, DHS_na5, ncol = 2)
dev.off()




#Comparing UN and DHS ASFR values
Countries<-unique(dat_africaedu2_$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    
    geom_line(cleaned_DHS_ASFR%>% filter(Country==Country_,Year%in%c(unique(dat_africaedu2_$Year))), mapping=aes(x=Age,y=ASFR,group=`Survey Year`,
                                                                                            colour =`Survey Year`)) +
    
    geom_line(ASFR_UN%>% filter(Country==Country_,Year%in%c(unique(dat_africaedu2_$Year))), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                                   colour ="UN")) +
    
    facet_grid(`Survey Year`~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}


pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/DHS vs UN.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()




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

pdf("./African Model/03052023 SSA WPP22 benchmark ESTFR/NAs_ UN.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

