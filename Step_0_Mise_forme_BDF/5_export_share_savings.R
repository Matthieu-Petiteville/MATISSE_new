# Source ------------------------------------------------------------------

source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
load(paste(M_data,"/Output/Initial format/menage_forme_4.RData",sep=""))


# Compute -----------------------------------------------------------------

savings_rate_2010<-compute_savings_rate_export(menage_forme) #0.1055916
share_2010<-compute_share_export(menage_forme)
ener_mix_2010<-energie_mix(menage_forme,FC=NA)


# Save --------------------------------------------------------------------

save(savings_rate_2010,file=paste(M_data,"/Output/Initial format/savings_rate_2010.RData",sep="")) 
save(share_2010,file=paste(M_data,"/Output/Initial format/share_2010.RData",sep=""))
save(ener_mix_2010,file=paste(M_data,"/Output/Initial format/ener_mix_2010.RData",sep=""))
