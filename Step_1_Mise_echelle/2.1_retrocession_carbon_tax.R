

# LIBRARY -----------------------------------------------------------------
library(pracma)
library(tidyverse)
source(paste(M_home,"/Step_1_Mise_echelle/2.2_fonction_retrocession_carbon_tax.R",sep=""))


# DATA --------------------------------------------------------------------

load(MatisseFiles$menage_echelle_1_1_rd)
load(MatisseFiles$IMACLIM_rd)
load(MatisseFiles$EMS_scen_rd)
load(MatisseFiles$menage_forme_rd)

# pour gagner du temps : 
facteur_dec1 <-  2.201544
facteur_tuu0 <-  3.32953

# Taxe Carbone ------------------------------------------------------------

# Taxe carbone totale prélevée aux ménages et rétrocédée
TCO_tot<-as.numeric(IMACLIM %>% 
                      filter(year==horizon) %>%
                      filter(model=="IMACLIM")%>%
                      filter(Variable=="TCO_RTCD_tot")%>%
                      select(value))*10^6

# Taxe carbone €/tonne CO2eq
TCO<-as.numeric(IMACLIM %>% 
                  filter(year==horizon) %>%
                  filter(model=="IMACLIM")%>%
                  filter(Variable=="TCO")%>%
                  select(value))*10^6


# Appel Fonction ----------------------------------------------------------
menage_echelle <- retrocession_carbon_tax(TCO,TCO_tot,menage_echelle)


# Save --------------------------------------------------------------------

save(menage_echelle,file=MatisseFiles$menage_echelle_1_rd)


# Clean -------------------------------------------------------------------
rm(menage_echelle,menage_forme,IMACLIM,EMS)
gc()


