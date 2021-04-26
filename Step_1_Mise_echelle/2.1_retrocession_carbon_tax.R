#Objectif : appliquer la rétrocession de la taxe carbone sur Menage_echelle_1

# Libraries -----------------------------------------------------------------

suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
source(paste(M_home, "/Step_1_Mise_echelle/2.2_fonction_retrocession_carbon_tax.R", sep = ""))


# Data --------------------------------------------------------------------

load(MatisseFiles$menage_echelle_1_1_rd)
load(MatisseFiles$IMACLIM_rd)


# Taxe Carbone ------------------------------------------------------------

# Taxe carbone totale prélevée aux ménages et rétrocédée
TCO_tot <- as.numeric(IMACLIM %>% 
                      filter(year == horizon) %>%
                      filter(model == "IMACLIM") %>%
                      filter(Variable == "TCO_RTCD_tot") %>%
                      select(value)) * 10^6

# Taxe carbone €/tonne CO2eq
TCO <- as.numeric(IMACLIM %>% 
                  filter(year == horizon) %>%
                  filter(model== "IMACLIM") %>%
                  filter(Variable == "TCO") %>%
                  select(value)) * 10^6


# Fonction Retrocession ----------------------------------------------------------

menage_echelle <- retrocession_carbon_tax(TCO, TCO_tot, menage_echelle)


# Save --------------------------------------------------------------------

save(menage_echelle, file = MatisseFiles$menage_echelle_1_rd)


# Clean -------------------------------------------------------------------

suppressWarnings(rm(menage_echelle, IMACLIM, TCO_tot, TCO))
gc()


