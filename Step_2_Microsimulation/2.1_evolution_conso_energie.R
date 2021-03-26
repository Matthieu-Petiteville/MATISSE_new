
# OBJECTIF ----------------------------------------------------------------

#Mise à l'échelle des dépenses d'énergie détaillées de la forme source_usage. Calcul de la consommation physique (MWh)
# Vocabulaire 
# source = source d'énergie => Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain"

# usage : usages=c("ECS","chauff","clim","Cuisson","ecl","ElecSpe")

# activité : sommeil, trav_etud, trajet, etc



# Libraries ---------------------------------------------------------------

library(dplyr)
library(reshape2)
library(tidyverse)
sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")


# DONNEES -----------------------------------------------------------------
setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE")
source("Step_2_Microsimulation/2.2_fonction_evolution_conso_energie.R")
source("Step_5_Export_IMACLIM/compute_savings_share_enermix.R")
source("Step_2_Microsimulation/calc_energie_kWh_m2.R") # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom

# Menage_echelle micro-simulation 
load(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_2_1.RData",sep=""))
load(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))

load("D:/Stage_Petiteville/Projet_Ademe/MATISSE/Data/Data_interne/list_source_usage.RData")


# APPEL FONCTION Evolution Conso Energie ----------------------------------

menage_echelle <- evolution_conso_ener(menage_echelle,FC)



# Save --------------------------------------------------------------------

save(menage_echelle,file=paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_2.RData",sep=""))




# VERIF -------------------------------------------------------------------


# Verif Finale
for (source in sources){
  print(source)
  dep_source_verif=paste("dep",source,"verif",sep="_")
  print("Accord interne dep_ener")
  print(table(abs(menage_echelle[dep_source_verif]-rowSums(menage_echelle %>% select(ident_men, list_source_usage) %>% select(contains(source))))<10^(-6)))
}


table(abs(menage_echelle$dep_Elec-menage_echelle$dep_Elec_verif)<10^(-10))
table(abs(menage_echelle$dep_Gaz-menage_echelle$dep_Gaz_verif)<10^(-10))
table(abs(menage_echelle$dep_GPL-menage_echelle$dep_GPL_verif)<10^(-10))
table(abs(menage_echelle$dep_Fuel-menage_echelle$dep_Fuel_verif)<10^(-10))
table(abs(menage_echelle$dep_Solides-menage_echelle$dep_Solides_verif)<10^(-10))

#Epargne
compute_savings_rate_export(menage_echelle)


# SUCCESS -----------------------------------------------------------------

print("2_2_evolution_conso_energie : SUCCESS")

