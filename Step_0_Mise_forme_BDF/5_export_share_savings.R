# Libraries ------------------------------------------------------------------

source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))


# Data --------------------------------------------------------------------

load(MatisseFiles$menage_forme_4_rd)


# Compute -----------------------------------------------------------------

savings_rate_2010 <- compute_savings_rate_export(menage_forme) #0.1055916
share_2010 <- compute_share_export(menage_forme)
ener_mix_2010 <- energie_mix(menage_forme, FC = NA)


# Save --------------------------------------------------------------------

save(savings_rate_2010, file = MatisseFiles$savings_rate_2010_rd) 
save(share_2010, file = MatisseFiles$share_2010_rd)
save(ener_mix_2010, file = MatisseFiles$ener_mix_2010_rd)


# Cleaning ----------------------------------------------------------------

suppressWarnings(rm(savings_rate_2010, share_2010, ener_mix_2010))
gc()
