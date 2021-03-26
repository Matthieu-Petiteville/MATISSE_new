

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(animation)
library(stargazer)
library(readxl)


setwd("D:/CIRED/Projet_Ademe/")
source("MATISSE/Step_5_Export_IMACLIM/compute_savings_share_enermix.R")




# Data --------------------------------------------------------------------



###
# 2010
###
load("MATISSE/Step_0_Mise_forme_BDF/Output/menage_forme_4.RData")

savings_rate_2010<-compute_savings_rate_export(menage_forme) #0.1055916
share_2010<-compute_share_export(menage_forme)
ener_mix_2010<-energie_mix(menage_forme,FC=NA)

###
# Mise à l'échelle
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_1.RData",sep=""))
load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))

epargne<-compute_savings_rate_export(menage_echelle)
share<-compute_share_export(menage_echelle)
ener_mix<-energie_mix(menage_echelle,FC=FC)
share_echelle <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_31
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_31.RData",sep=""))


share<-compute_share_export(menage_echelle_31)
epargne<-compute_savings_rate_export(menage_echelle_31)
ener_mix_31<-energie_mix(menage_echelle_31,FC=FC)
share_TC_DPE_31 <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_32
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_32.RData",sep=""))

share<-compute_share_export(menage_echelle_32)
epargne<-compute_savings_rate_export(menage_echelle_32)
ener_mix_32<-energie_mix(menage_echelle_32,FC=FC)
share_TC_DPE_32 <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_33
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_33.RData",sep=""))

share<-compute_share_export(menage_echelle_33)
epargne<-compute_savings_rate_export(menage_echelle_33)
ener_mix_33<-energie_mix(menage_echelle_33,FC=FC)
share_TC_DPE_33 <-t(data.frame(share,"epargne"=epargne))


###
# TC_DPE_34
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_DPE.RData",sep=""))

share<-compute_share_export(menage_echelle_TC_DPE)
epargne<-compute_savings_rate_export(menage_echelle_TC_DPE)
ener_mix_34<-energie_mix(menage_echelle_TC_DPE,FC=FC)
share_TC_DPE_34 <-t(data.frame(share,"epargne"=epargne))

###
# TC_VE
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE.RData",sep=""))

share<-compute_share_export(menage_echelle_TC_VE)
epargne<-compute_savings_rate_export(menage_echelle_TC_VE)
ener_mix_TC_VE<-energie_mix(menage_echelle_TC_VE,FC=FC)
share_TC_VE <-t(data.frame(share,"epargne"=epargne))


###
# Repond
###

load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle.RData",sep=""))


share<-compute_share_export(menage_echelle)
epargne<-compute_savings_rate_export(menage_echelle)
ener_mix_repond<-energie_mix(menage_echelle,FC=FC)
share_final <-t(data.frame(share,"epargne"=epargne))


# EVOLUTION ---------------------------------------------------------------



