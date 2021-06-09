
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
source(paste(M_home,"/Step_2_Microsimulation/2.2_fonction_evolution_conso_energie.R",sep=""))
source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom


# DONNEES -----------------------------------------------------------------
sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
# Menage_echelle micro-simulation 
load(MatisseFiles$menage_echelle_2_1_rd)
load(MatisseFiles$FC_2010_horizon_rd)
load(MatisseFiles$source_usage_rd)


# APPEL FONCTION Evolution Conso Energie ----------------------------------

menage_echelle <- evolution_conso_ener(menage_echelle,FC)


# Save --------------------------------------------------------------------

save(menage_echelle,file=MatisseFiles$menage_echelle_2_rd)


# VERIF -------------------------------------------------------------------


# Verif Finale
for (source in sources){dep_source_verif=paste("dep",source,"verif",sep="_")}

# 
# table(abs(menage_echelle$dep_Elec-menage_echelle$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle$dep_Gaz-menage_echelle$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle$dep_GPL-menage_echelle$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle$dep_Fuel-menage_echelle$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle$dep_Solides-menage_echelle$dep_Solides_verif)<10^(-10))

#Epargne
compute_savings_rate_export(menage_echelle)


# Clean -------------------------------------------------------------------
suppressWarnings(rm(sources,menage_echelle,FC,list_source_usage,dep_source_verif))
gc()



