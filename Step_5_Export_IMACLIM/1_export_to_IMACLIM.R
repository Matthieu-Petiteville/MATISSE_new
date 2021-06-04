
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))

# DATA --------------------------------------------------------------------

load(MatisseFiles$menage_echelle_final_rd)
load(MatisseFiles$FC_2010_horizon_rd)


# Compute  ----------------------------------------------------------------

savings_rate<-compute_savings_rate_export(menage_echelle) 
share<-compute_share_export(menage_echelle)
ener_mix<-energie_mix(menage_echelle,FC=FC)
evol_energie<-compute_evol_energie(menage_echelle,scenario,horizon,scenario_classement,redistribution,Iter=0)



# Import Technical Change variables ---------------------------------------

load(MatisseFiles$cout_baill_pub_rd)
load(MatisseFiles$sBCE_rd)


# Extract Données Solides -------------------------------------------------
suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl , sheet="scen AMS")))
ThreeME <- 
  scen %>% 
  select(-Def) %>% 
  gather(key=year , value=value , -c(1)) %>%
  filter(year==c(2010, horizon))

extract_solides <- 
  ThreeME %>% 
  filter(Var %in% c("CHM_21_2" , "CHD_21_2")) %>%
  group_by(year)   %>% 
  summarize(sum_charb = sum(value))

evol_solides <- extract_solides$sum_charb[2] / extract_solides$sum_charb[1] - 1 


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
  "SOLIDES"=evol_solides, #A04
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


write.csv(export,file=MatisseFiles$export_iter_0_csv)
menage_iteration<-menage_echelle
save(menage_iteration,file=MatisseFiles$menage_iteration_rd)

suppressWarnings((rm(menage_iteration, menage_echelle, FC, export, extract_solides, evol_solides, ThreeME, scen)))
gc()

