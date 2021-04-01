
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

# DATA --------------------------------------------------------------------
# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE")
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))

load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))


# Compute  ----------------------------------------------------------------

savings_rate<-compute_savings_rate_export(menage_echelle) 
share<-compute_share_export(menage_echelle)
ener_mix<-energie_mix(menage_echelle,FC=FC)
evol_energie<-compute_evol_energie(menage_echelle,scenario,horizon,scenario_classement,redistribution,Iter=0)



# Import Technical Change variables ---------------------------------------

load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Cout_bailleur_public.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/sBCE.RData",sep=""))


# EXPORT ------------------------------------------------------------------

# dénominateur : 
  
  denom=sum(share$share_A01[1]+
              share$share_A05[1]+
              share$share_A06[1]+
              share$share_A08[1]+
              share$share_A09[1]+
              share$share_A10[1]+
              share$share_A11[1]+
              share$share_A12[1]+
              share$share_A13[1])

export <-  t(data.frame(
  "share_A01"=share$share_A01[1]/denom,
  "ELEC"=evol_energie$Elec,       #A02
  "GAZ"=evol_energie$Gaz,         #A03
  "SOLIDES"=evol_energie$Solides, #A04
  "share_A05"=share$share_A05[1]/denom,
  "share_A06"=share$share_A06[1]/denom,
  "OIL"=evol_energie$Oil,         #A07
  "share_A08"=share$share_A08[1]/denom,
  "share_A09"=share$share_A09[1]/denom,
  "share_A10"=share$share_A10[1]/denom,
  "share_A11"=share$share_A11[1]/denom,
  "share_A12"=share$share_A12[1]/denom,
  "share_A13"=share$share_A13[1]/denom,
  "epargne"=savings_rate,
  "sBCE"=sBCE,
  "Renovation_BS"=Cout_bailleur_public))


write.csv(export,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/export_Iter_0.csv",sep=""))
menage_iteration<-menage_echelle
save(menage_iteration,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_iteration.RData",sep=""))

