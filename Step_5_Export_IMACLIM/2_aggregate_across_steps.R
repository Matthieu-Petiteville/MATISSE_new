

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(animation)
library(stargazer)
library(readxl)

source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))

# Data --------------------------------------------------------------------



###
# 2010
###
load(MatisseFiles$menage_forme_4_rd)
savings_rate_2010<-compute_savings_rate_export(menage_forme) #0.1055916
share_2010<-compute_share_export(menage_forme)
ener_mix_2010<-energie_mix(menage_forme,FC=NA)

###
# Mise à l'échelle
###

load(MatisseFiles$menage_echelle_1_rd)
load(MatisseFiles$FC_2010_horizon_rd)

epargne<-compute_savings_rate_export(menage_echelle)
share<-compute_share_export(menage_echelle)
ener_mix<-energie_mix(menage_echelle,FC=FC)
share_echelle <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_31
###

load(MatisseFiles$menage_echelle_1_rd)


share<-compute_share_export(menage_echelle_31)
epargne<-compute_savings_rate_export(menage_echelle_31)
ener_mix_31<-energie_mix(menage_echelle_31,FC=FC)
share_TC_DPE_31 <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_32
###

load(MatisseFiles$menage_echelle_32_rd)

share<-compute_share_export(menage_echelle_32)
epargne<-compute_savings_rate_export(menage_echelle_32)
ener_mix_32<-energie_mix(menage_echelle_32,FC=FC)
share_TC_DPE_32 <-t(data.frame(share,"epargne"=epargne))

###
# TC_DPE_33
###

load(MatisseFiles$menage_echelle_1_rd)

share<-compute_share_export(menage_echelle_33)
epargne<-compute_savings_rate_export(menage_echelle_33)
ener_mix_33<-energie_mix(menage_echelle_33,FC=FC)
share_TC_DPE_33 <-t(data.frame(share,"epargne"=epargne))


###
# TC_DPE_34
###

load(MatisseFiles$menage_echelle_TC_DPE_rd)

share<-compute_share_export(menage_echelle_TC_DPE)
epargne<-compute_savings_rate_export(menage_echelle_TC_DPE)
ener_mix_34<-energie_mix(menage_echelle_TC_DPE,FC=FC)
share_TC_DPE_34 <-t(data.frame(share,"epargne"=epargne))

###
# TC_VE
###

load(MatisseFiles$menage_echelle_TC_VE_rd)

share<-compute_share_export(menage_echelle_TC_VE)
epargne<-compute_savings_rate_export(menage_echelle_TC_VE)
ener_mix_TC_VE<-energie_mix(menage_echelle_TC_VE,FC=FC)
share_TC_VE <-t(data.frame(share,"epargne"=epargne))


###
# Repond
###

load(MatisseFiles$menage_echelle_final_rd)


share<-compute_share_export(menage_echelle)
epargne<-compute_savings_rate_export(menage_echelle)
ener_mix_repond<-energie_mix(menage_echelle,FC=FC)
share_final <-t(data.frame(share,"epargne"=epargne))


# EVOLUTION ---------------------------------------------------------------



