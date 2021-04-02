
# LIBRARY -----------------------------------------------------------------

library(tidyverse)
source(paste(M_home,"/Common/tools.R",sep=""))

# DATA --------------------------------------------------------------------


## MICRO

load(paste(M_data,"/Output/Initial format/menage_forme.RData",sep=""))
source(paste(M_home,"/Step_1_Mise_echelle/1.2_fonction_mise_echelle_revenus.R",sep=""))

## MACRO
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/IMACLIM.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))



# Appel Fonction  ---------------------------------------------------------

Iter=0
menage_echelle <- mise_echelle_revenu(FC,menage_forme,Iter)


# Save files --------------------------------------------------------------

save(menage_echelle,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_1_1.RData",sep=""))

# Next Step ---------------------------------------------------------------
# Rétrocession de la taxe carbone


