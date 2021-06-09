# OBJECTIF 
# Calcul de classes DPE calées sur 3ME et non sur Phebus.
# Réécrit la sortie das menage_forme_3

# Libraries ---------------------------------------------------------------

suppressMessages(library(reshape2, warn.conflicts=F , quietly = T))
suppressMessages(library(tidyverse, warn.conflicts=F , quietly = T))
suppressMessages(library(plyr, warn.conflicts=F , quietly = T))
source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_bonus_energies_kwh.R",sep=""))


# Data --------------------------------------------------------------------

#Récupération de menage_forme_3 et prétraitement
load(MatisseFiles$menage_forme_3_rd)
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_bonus_energies_kwh.R",sep=""))
menage_forme_ener <- energie_dom_surf(menage_forme)
menage_forme_ener <- menage_forme_ener %>% dplyr::mutate(kWh_rank =row_number(-energie_tot_surf)) 
menage_forme_ener <- menage_forme_ener[order(menage_forme_ener$kWh_rank),]
menage_forme_ener$cumsum_surf <- cumsum(menage_forme_ener$surfhab_d*menage_forme_ener$pondmen)
tot_bdf_surf <- sum(menage_forme_ener$surfhab_d*menage_forme_ener$pondmen)

#Stock de logement par DPE en 2010
load(MatisseFiles$dpe_stock_2010_rd)
dpe_stock_2010$adjusted_stock <- dpe_stock_2010$value / sum(dpe_stock_2010$value) * tot_bdf_surf
dpe_stock_2010 <- dpe_stock_2010[order(dpe_stock_2010$DPE, decreasing = TRUE),]
dpe_stock_2010$cumsum_surf <- cumsum(dpe_stock_2010$adjusted_stock)
dpe_stock_2010 <- dpe_stock_2010[order(dpe_stock_2010$DPE),]
menage_forme_ener$new_DPE = "A"
for(DPE in (2:nrow(dpe_stock_2010))){
  menage_forme_ener$new_DPE[which(menage_forme_ener$cumsum_surf < dpe_stock_2010[DPE,"cumsum_surf"])] <- dpe_stock_2010[DPE,"DPE"]
}

menage_forme$DPE_pred <- menage_forme_ener$new_DPE[match(menage_forme$ident_men,menage_forme_ener$ident_men)]
save(menage_forme, file = MatisseFiles$menage_forme_3_rd)