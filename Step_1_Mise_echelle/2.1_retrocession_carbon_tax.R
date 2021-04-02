

# LIBRARY -----------------------------------------------------------------
library(pracma)
library(tidyverse)
source(paste(M_home,"/Step_1_Mise_echelle/2.2_fonction_retrocession_carbon_tax.R",sep=""))


# DATA --------------------------------------------------------------------

load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_1_1.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/IMACLIM.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/EMS.RData",sep=""))
load(paste(M_data,"/Output/Initial format/menage_forme.RData",sep=""))

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

save(menage_echelle,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_1.RData",sep=""))


# Clean -------------------------------------------------------------------
rm(menage_echelle,menage_forme,IMACLIM,EMS)
gc()


