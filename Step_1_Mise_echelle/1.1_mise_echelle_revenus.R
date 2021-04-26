#Objectif : calculer la première version de menage_echelle, qui est la version mise à 
#l'échelle de l'horizon de menage_forme, via la fonction mise_echelle_revenu


# Libraries -----------------------------------------------------------------

suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
source(paste(M_home, "/Common/tools.R", sep = ""))
source(paste(M_home, "/Step_1_Mise_echelle/1.2_fonction_mise_echelle_revenus.R", sep = ""))

# Data --------------------------------------------------------------------

load(MatisseFiles$menage_forme_rd)
load(MatisseFiles$FC_2010_horizon_rd)

# Appel Fonction  ---------------------------------------------------------

menage_echelle <- mise_echelle_revenu(FC, menage_forme)

# Save files --------------------------------------------------------------

save(menage_echelle, file = MatisseFiles$menage_echelle_1_1_rd)


# Clean -------------------------------------------------------------------
suppressWarnings(rm(menage_forme, FC, menage_echelle))
gc()

