
setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE/")

# Source ------------------------------------------------------------------

source("Step_5_Export_IMACLIM/compute_savings_share_enermix.R")
load("Step_0_Mise_forme_BDF/Output/menage_forme_4.RData")


# Compute -----------------------------------------------------------------

savings_rate_2010<-compute_savings_rate_export(menage_forme) #0.1055916
share_2010<-compute_share_export(menage_forme)
ener_mix_2010<-energie_mix(menage_forme,FC=NA)


# Save --------------------------------------------------------------------

save(savings_rate_2010,file="Step_0_Mise_forme_BDF/Output/savings_rate_2010.RData") 
save(share_2010,file="Step_0_Mise_forme_BDF/Output/share_2010.RData")
save(ener_mix_2010,file="Step_0_Mise_forme_BDF/Output/ener_mix_2010.RData")
