
# LIBRARY -----------------------------------------------------------------

library(tidyverse)
source(paste(M_home,"/Common/tools.R",sep=""))

# DATA --------------------------------------------------------------------


## MICRO
# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE")

load(paste(M_data,"/Output/Step_0/menage_forme.RData",sep=""))
source(paste(M_home,"/Step_1_Mise_echelle/1.2_fonction_mise_echelle_revenus.R",sep=""))
# source("Step_5_Export_IMACLIM/compute_savings_share_enermix.R")


## MACRO
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/IMACLIM.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))



# Appel Fonction  ---------------------------------------------------------

Iter=0
menage_echelle <- mise_echelle_revenu(FC,menage_forme,Iter)


           

###
# TEST
###

#' 
#' 
#' #Cas particulier des ménages 2548 et 10828 qui ont des NaN en AID, IR et donc en RDB
#' menage_echelle <- 
#'   menage_echelle %>%
#'   mutate_when(RDBAI==0,list(RDB=0))
#' # cas Particulier du ménage 8234 qui paie 88% de son RDBAI en impôt en 2010, et 97% en 2035.Mise à zéro du RDB par commodité contre -3783 sinon. 
#' 
#' 
#' vérif pour quoi <0 ?
#' 
#' 
#' menage_echelle <- 
#'   menage_echelle %>%
#'   mutate_when(ident_men==8234,list(RDB=0))



# Save files --------------------------------------------------------------

save(menage_echelle,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_1_1.RData",sep=""))




# Next Step ---------------------------------------------------------------

# Rétrocession de la taxe carbone

# SUCCESS -----------------------------------------------------------------

print("Step 1 : 1_Mise_echelle_revenus : SUCCESS")




