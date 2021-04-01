#Réalise la première étape Step_0 : mise en forme des données brutes


# Libraries ---------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))


# Main run ----------------------------------------------------------------

source(paste(M_home,"/Step_0_Mise_forme_BDF/0_mise_forme_elast.r",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/1.1_imputation_DPE_2010.r",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/2_mise_forme_BDF.r",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_mise_forme_energies.r",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/4_pre-traitement_travaux_rehab_2010.r",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/5_export_share_savings.r",sep=""))
