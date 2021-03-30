

# DO NOT RUN ! runned through 2.2_fonction_retrocession_carbon_tax.R
















# Il nous faut un facteur relativement élevé pour permettre 1/ au décile 1 d'être plus compensé que les autres en moyenne, 2/ qu'une solution existe pour redistribuer le solde de TCO aux autres déciles selon la méthode dégressive créée. 
# on veut un facteur en 2010 pour reproduire un écart entre dec_1/tuu_0 et le reste de la population en 2010 à l'horizon. En effet, si on veut compenser une part fixe d'une partie de la population à l'horizon, il faut itérer le paiement de la tco et la microsimulation pour prendre en compte les effets rebonds. 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(reldist)
library(matrixStats)

# Data --------------------------------------------------------------------


load(paste(M_data,"/Output/Step_0/menage_forme.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/EMS.RData",sep=""))
EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")


# La règle retenue est celle d’une compensation par UC pour le premier décile reflétant l’écart, calculé en 2010, entre les émissions de carbone directes moyennes par UC sur l’ensemble des ménages, et la médiane des mêmes émissions par UC du décile 1.
# 
# La rétrocession est ainsi étalonnée sur les dernières statistiques disponibles pour compenser entièrement la moitié des ménages du premier décile des surcoûts engendrés par la réforme calculés ex ante, c’est-à-dire sans prendre en compte les stratégies d’adaptation. 
# L’écart en question, de 2.31, correspond par construction à l’écart à la somme qui serait rétrocédée aux ménages du décile 1 en option de rétrocession forfaitaire (ils touchent 2.31 fois plus). Les rétrocessions des déciles 1 à 9 sont alors calculées pour chaque scénario, à chaque horizon et au fil des itérations macro-micro sous l’hypothèse que chaque décile touche une même fraction inférieure à 1 de ce que touche le décile inférieur, de sorte à solder le total de taxe à rétrocéder. 


# prix constant 2010 tCO2/ dépenses par cat
coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_21_2")%>%select(emission))/
  as.numeric(menage_forme %>%summarise(sum(pondmen*dep_Solides))) # en tC02

coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_22_2")%>%select(emission))/
  as.numeric(menage_forme %>% summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €

coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Var=="EMS_HH_24_2")%>%select(emission))/
  as.numeric(menage_forme %>%summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) #

menage_forme <-
  menage_forme %>%
  mutate(ems_CL=dep_Solides*coeff_CL_2010)%>%
  mutate(ems_Oil=(carb_lubr+dep_Fuel+dep_GPL)*coeff_Oil_2010)%>%
  mutate(ems_Gaz=(dep_Gaz+dep_Urbain)*coeff_Gaz_2010)
menage_forme <- menage_forme %>% mutate(emissions=ems_CL+ems_Oil+ems_Gaz)%>%
  mutate(ems_uc=emissions/coeffuc)

ems_uc_tot <- menage_forme %>% summarise(weighted.mean(x=ems_uc,w=pondmen))
# 3.474344
# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%filter(ems_uc>1)%>%summarise(weighted.mean(x=ems_uc,w=pondmen))
# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%filter(ems_uc>1)%>%summarise(weightedMedian(x=ems_uc,w=pondmen))



# Décile ------------------------------------------------------------------

# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%filter(ems_uc>1)%>%summarise(wtd.quantile(x=ems_uc,q=0.5,weight=pondmen))
# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%filter(ems_uc>1)%>%summarise(wtd.quantile(x=ems_uc,q=0.9,weight=pondmen))
# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%summarise(wtd.quantile(x=ems_uc,q=0.5,weight=pondmen))
# men_dec_1<-menage_forme %>% filter(decuc2==1)%>%summarise(weightedMedian(x=ems_uc,w=pondmen))

men_dec_1<-menage_forme %>% filter(decuc2==1)%>%summarise(wtd.quantile(x=ems_uc,q=0.95,weight=pondmen))

men_dec_1


men_dec_1/ems_uc_tot

facteur_dec1 <- as.numeric(men_dec_1/ems_uc_tot)
# 2.201544




# TUU ---------------------------------------------------------------------

men_tuu_0<-menage_forme %>% filter(tuu==0)%>%summarise(wtd.quantile(x=ems_uc,q=0.95,weight=pondmen))
men_tuu_0
men_tuu_0/ems_uc_tot

facteur_tuu0 <- as.numeric(men_tuu_0/ems_uc_tot)
# 3.32953


