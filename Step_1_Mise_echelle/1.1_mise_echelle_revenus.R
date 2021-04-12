
# LIBRARY -----------------------------------------------------------------
library(tidyverse)
source(paste(M_home,"/Common/tools.R",sep=""))

# DATA --------------------------------------------------------------------


## MICRO
load(MatisseFiles$menage_forme_rd)
source(paste(M_home,"/Step_1_Mise_echelle/1.2_fonction_mise_echelle_revenus.R",sep=""))

## MACRO
load(MatisseFiles$IMACLIM_rd)
load(MatisseFiles$FC_2010_horizon_rd)
load(MatisseFiles$Threeme_rd)

# Appel Fonction  ---------------------------------------------------------

Iter=0
menage_echelle <- mise_echelle_revenu(FC,menage_forme,Iter)


# Save files --------------------------------------------------------------

save(menage_echelle,file=MatisseFiles$menage_echelle_1_1_rd)



# Next Step ---------------------------------------------------------------
# Rétrocession de la taxe carbone



# Clean -------------------------------------------------------------------
rm(menage_forme,IMACLIM,FC,ThreeME,Iter,menage_echelle)
gc()

