# setwd(paste(M_home,"/")

# # test

scenario="AMS"
horizon=2035
scenario_classement="Optimiste"
redistribution="forfait"

####
# Step 0
####

source(paste(M_home,"/Step_0_Mise_forme_BDF/0_mise_forme_elast.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/1.1_imputation_DPE_2010.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/2_mise_forme_BDF.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_mise_forme_energies.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/4_pre-traitement_travaux_rehab_2010.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/5_export_share_savings.R",sep=""))


####
# Step 1
####

source(paste(M_home,"/Step_1_Mise_echelle/0_mise_forme_data_3ME.R",sep=""))
source(paste(M_home,"/Step_1_Mise_echelle/1.1_mise_echelle_revenus.R",sep=""))
source(paste(M_home,"/Step_1_Mise_echelle/2.1_retrocession_carbon_tax.R",sep=""))

####
# Step 2
####

source(paste(M_home,"/Step_2_Microsimulation/1.1_Microsimulation.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/2.1_evolution_conso_energie.R",sep=""))


####
# Step 3
####

source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_1_Achat_neuf_horizon.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_2_Achat_2010_horizon-1.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_3_Rehab_2010_horizon.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_4_Bascule_A.R",sep=""))


# source(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/3_2_1_VE_classement_horizon.R")
source(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/3_2_2_VE_2010_horizon.R",sep=""))


# # ####
# # # Step 4
# # ####
source(paste(M_home,"/Step_4_Reweighting/1_contraintes_macro.R")
source(paste(M_home,"/Step_4_Reweighting/2_contraintes_micro.R")
source(paste(M_home,"/Step_4_Reweighting/3_pondr.R")

# ####
# # Step 5
# ####
source(paste(M_home,"/Step_5_export_IMACLIM/1_export_to_IMACLIM.R")
