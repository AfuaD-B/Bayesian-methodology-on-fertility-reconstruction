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




se_dat<-read_excel("./Bayesglmmodel_estimates.xlsx")%>%filter(Region%in%c("Africa","North Africa"))


###########################################################################################################################
#We make here the density plot of the standard errors from the Bayesglm model

ggplot()+
  geom_density(aes(x=se_dat$SE))+
  labs(title="Density of bayesglm standard errors", x="Standard Error")+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size =20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        strip.text = element_text(size=22),
        plot.title = element_text(size=28))


ggplot()+
  geom_density(aes(x=1/((se_dat$SE)^2)))+
  labs(title="Density of bayesglm precision", x="Precision")+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size =20),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20),
        strip.text = element_text(size=22),
        plot.title = element_text(size=28))


###################################################################################################################################################################
##
#Make the prior plots

alpha=((sd(se_dat$SE))/(mean(se_dat$SE)))^2
beta=(sd(se_dat$SE))^2/(mean(se_dat$SE))
alpha_beta<-rgamma(100,alpha,beta)


sd_se<-(sd(se_dat$SE))^2
gamma47<-rgamma(100,1/sd_se,sd_se)



sd_pred<-(sd(se_dat$Pred))^2
gammapred<-rgamma(100,1/sd_pred,sd_pred)


#For education specific precisions
dat_africaedu_se2<-se_dat[,-c(5,6,8)]%>%distinct()%>%spread(Education,SE)

sesasfr1=(sd(dat_africaedu_se2$`Higher Education`))^2
sesasfr2=(sd(dat_africaedu_se2$`No Education`))^2
sesasfr3=(sd(dat_africaedu_se2$`Primary Education`))^2
sesasfr4=(sd(dat_africaedu_se2$`Secondary Education`))^2


loc1<-1/(mean(dat_africaedu_se2$`Higher Education`))
loc2<-1/(mean(dat_africaedu_se2$`No Education`))
loc3<-1/(mean(dat_africaedu_se2$`Primary Education`))
loc4<-1/(mean(dat_africaedu_se2$`Secondary Education`))

hiedu1<-rgamma(100,loc1,sesasfr1)
noedu1<-rgamma(100,loc2,sesasfr2)
priedu1<-rgamma(100,loc3,sesasfr3)
secedu1<-rgamma(100,loc4,sesasfr4)



#sd by edu

loc1_<-1/sesasfr1
loc2_<-1/sesasfr2
loc3_<-1/sesasfr3
loc4_<-1/sesasfr4

hiedu2<-rgamma(100,loc1_,sesasfr1)
noedu2<-rgamma(100,loc2_,sesasfr2)
priedu2<-rgamma(100,loc3_,sesasfr3)
secedu2<-rgamma(100,loc4_,sesasfr4)

library(ggthemes)
ggplot()+
  geom_density(aes(x=gamma47,color="Gamma(47,sd_se^2)",fill="Gamma(47,sd_se^2)"))+
  geom_density(aes(x=gammapred,color="Gamma(92,sd_pred^2)",fill="Gamma(92,sd_pred^2)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")+
  scale_color_colorblind()+
  scale_fill_colorblind()

ggplot()+
  geom_density(aes(x=alpha_beta,color="Gamma(alpha,beta)",fill="Gamma(alpha,beta)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")+
  scale_color_colorblind()+
  scale_fill_colorblind()

ggplot()+
  geom_density(aes(x=alpha_beta,color="Gamma(alpha,beta)",fill="Gamma(alpha,beta)"))+
  geom_density(aes(x=gamma47,color="Gamma(47,sd_se^2)",fill="Gamma(47,sd_se^2)"))+
  geom_density(aes(x=gammapred,color="Gamma(92,sd_pred^2)",fill="Gamma(92,sd_pred^2)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")+
  scale_color_colorblind()+
  scale_fill_colorblind()


ggplot()+
  geom_density(aes(x=noedu1,color="Gamma(1/mean(se_noedu),sd_se_noedu^2)",fill="Gamma(1/mean(se_noedu),sd_se_noedu^2)"))+
  geom_density(aes(x=noedu2,color="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)",fill="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation No Education",fill="Parameter Estimation No Education")+
  scale_color_colorblind()+
  scale_fill_colorblind()


ggplot()+
  geom_density(aes(x=priedu1,color="Gamma(1/mean(se_priedu),sd_se_priedu^2)",fill="Gamma(1/mean(se_priedu),sd_se_priedu^2)"))+
  geom_density(aes(x=priedu2,color="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)",fill="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation Primary Education",fill="Parameter Estimation Primary Education")+
  scale_color_colorblind()+
  scale_fill_colorblind()


ggplot()+
  geom_density(aes(x=secedu1,color="Gamma(1/mean(se_secedu),sd_se_secedu^2)",fill="Gamma(1/mean(se_secedu),sd_se_secedu^2)"))+
  geom_density(aes(x=secedu2,color="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)",fill="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation Secondary Education",fill="Parameter Estimation Secondary Education")+
  scale_color_colorblind()+
  scale_fill_colorblind()


ggplot()+
  geom_density(aes(x=hiedu1,color="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)",fill="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)"))+
  geom_density(aes(x=hiedu2,color="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)",fill="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation Higher Education",fill="Parameter Estimation Higher Education")+
  scale_color_colorblind()+
  scale_fill_colorblind()



#All in one
ggplot()+
  geom_density(aes(x=alpha_beta,color="Gamma(alpha,beta)",fill="Gamma(alpha,beta)"))+
  geom_density(aes(x=gamma47,color="Gamma(47,sd_se^2)",fill="Gamma(47,sd_se^2)"))+
  geom_density(aes(x=gammapred,color="Gamma(92,sd_pred^2)",fill="Gamma(92,sd_pred^2)"))+
  geom_density(aes(x=noedu1,color="Gamma(1/mean(se_noedu),sd_se_noedu^2)",fill="Gamma(1/mean(se_noedu),sd_se_noedu^2)"))+
  geom_density(aes(x=noedu2,color="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)",fill="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)"))+
  geom_density(aes(x=priedu1,color="Gamma(1/mean(se_priedu),sd_se_priedu^2)",fill="Gamma(1/mean(se_priedu),sd_se_priedu^2)"))+
  geom_density(aes(x=priedu2,color="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)",fill="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)"))+
  geom_density(aes(x=secedu1,color="Gamma(1/mean(se_secedu),sd_se_secedu^2)",fill="Gamma(1/mean(se_secedu),sd_se_secedu^2)"))+
  geom_density(aes(x=secedu2,color="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)",fill="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)"))+
  geom_density(aes(x=hiedu1,color="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)",fill="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)"))+
  geom_density(aes(x=hiedu2,color="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)",fill="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")


#Except Gamma alpha beta
ggplot()+
  geom_density(aes(x=gamma47,color="Gamma(47,sd_se^2)",fill="Gamma(47,sd_se^2)"))+
  geom_density(aes(x=gammapred,color="Gamma(92,sd_pred^2)",fill="Gamma(92,sd_pred^2)"))+
  geom_density(aes(x=noedu1,color="Gamma(1/mean(se_noedu),sd_se_noedu^2)",fill="Gamma(1/mean(se_noedu),sd_se_noedu^2)"))+
  geom_density(aes(x=noedu2,color="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)",fill="Gamma(1/sd_se_noedu^2,sd_se_noedu^2)"))+
  geom_density(aes(x=priedu1,color="Gamma(1/mean(se_priedu),sd_se_priedu^2)",fill="Gamma(1/mean(se_priedu),sd_se_priedu^2)"))+
  geom_density(aes(x=priedu2,color="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)",fill="Gamma(1/sd_se_priedu^2,sd_se_priedu^2)"))+
  geom_density(aes(x=secedu1,color="Gamma(1/mean(se_secedu),sd_se_secedu^2)",fill="Gamma(1/mean(se_secedu),sd_se_secedu^2)"))+
  geom_density(aes(x=secedu2,color="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)",fill="Gamma(1/sd_se_secedu^2,sd_se_secedu^2)"))+
  geom_density(aes(x=hiedu1,color="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)",fill="Gamma(1/mean(se_hiedu),sd_se_hiedu^2)"))+
  geom_density(aes(x=hiedu2,color="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)",fill="Gamma(1/sd_se_hiedu^2,sd_se_hiedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")



ggplot()+
  geom_density(aes(x=se_dat$ESASFR))





over0.25<-se_dat[,-9]%>%filter(SE>0.241)
unique(over0.25$ESASFR)


ggplot(over0.25,aes(x=SE, colour=Education, group=Education))+
  geom_density(lwd=1.5)+
  facet_wrap(~Country)+
  scale_colour_colorblind()

ggplot(over0.25,aes(x=SE, colour=Education, group=Education))+
  geom_density(lwd=1.5)+
  facet_wrap(~Year)+
  scale_colour_colorblind()





less0.25<-se_dat[,-9]%>%filter(SE<=0.241)
NAless0.25<-less0.25%>%filter(is.na(ESASFR))
max(NAless0.25$SE)
min(NAless0.25$SE)
ggplot(NAless0.25,aes(x=SE))+
  geom_density()




nnaless0.25<-se_dat[,-9]%>%filter(SE<=0.241,!is.na(ESASFR))
ggplot(nnaless0.25,aes(x=SE))+
  geom_density()


#SEs by country year education
library(matrixStats)
se_cy_edu1<-dat_africaedu_se2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
se_cy_edu1_<-as.matrix(rowSds(as.matrix(se_cy_edu1[,3:9]))^2)

se_cy_edu2<-dat_africaedu_se2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
se_cy_edu2_<-as.matrix(rowSds(as.matrix(se_cy_edu2[,3:9]))^2)

se_cy_edu3<-dat_africaedu_se2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
se_cy_edu3_<-as.matrix(rowSds(as.matrix(se_cy_edu3[,3:9]))^2)

se_cy_edu4<-dat_africaedu_se2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
se_cy_edu4_<-as.matrix(rowSds(as.matrix(se_cy_edu4[,3:9]))^2)



inv_se_cy_edu1<-1/se_cy_edu1_
inv_se_cy_edu2<-1/se_cy_edu2_
inv_se_cy_edu3<-1/se_cy_edu3_
inv_se_cy_edu4<-1/se_cy_edu4_



hiedu3<-rgamma(100,inv_se_cy_edu1,se_cy_edu1_)
noedu3<-rgamma(100,inv_se_cy_edu2,se_cy_edu2_)
priedu3<-rgamma(100,inv_se_cy_edu3,se_cy_edu3_)
secedu3<-rgamma(100,inv_se_cy_edu4,se_cy_edu4_)


ggplot()+
  geom_density(aes(x=noedu3,color="Gamma(1/sd(se_cy_noedu),sd_se_cy_noedu^2)",fill="Gamma(1/sd(se_cy_noedu),sd_se_cy_noedu^2)"))+
  geom_density(aes(x=priedu3,color="Gamma(1/sd_se_cy_priedu^2,sd_se_cy_priedu^2)",fill="Gamma(1/sd_se_cy_priedu^2,sd_se_cy_priedu^2)"))+
  geom_density(aes(x=secedu3,color="Gamma(1/sd(se_cy_secedu),sd_se_cy_secedu^2)",fill="Gamma(1/sd(se_cy_secedu),sd_se_cy_secedu^2)"))+
  geom_density(aes(x=hiedu3,color="Gamma(1/sd_se_cy_hiedu^2,sd_se_cy_hiedu^2)",fill="Gamma(1/sd_se_cy_hiedu^2,sd_se_cy_hiedu^2)"))+
  labs(x="Precisions",color="Parameter Estimation",fill="Parameter Estimation")

#Update this plot later
#posterior distribution, that is, our estimates

beasfr<-read_excel("./African Model/01022023 SSA WPP22 update/BESFR_data qaul bayesglmmodel_se_sd_c_y_e.xlsx",sheet="BESASFR")



