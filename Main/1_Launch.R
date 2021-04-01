# Objectif : lancer tous les étapes de la première itération d'un scénario à partir d'une boucle (jusqu'au renvoi dans IMACLIM-3ME)


# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))


# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------

for (scenario in scenario_v){
  for (horizon in horizon_v){
    for (scenario_classement in scenario_classement_v){
      for (redistribution in redistribution_v){
        
        Iter=0
        print(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep=""))
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
        source(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/3_2_2_VE_2010_horizon.R",sep=""))
        
        ####
        # Step 4
        ####
        
        source(paste(M_home,"/Step_4_Reweighting/1_contraintes_macro.R",sep=""))
        source(paste(M_home,"/Step_4_Reweighting/2_contraintes_micro.R",sep=""))
        source(paste(M_home,"/Step_4_Reweighting/3_pondr.R",sep=""))
        
        ####
        # Step 5
        ####
        
        source(paste(M_home,"/Step_5_export_IMACLIM/1_export_to_IMACLIM.R",sep=""))

        print(paste(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep="")," : DONE (",end[3]/60," min)",sep=""))

      }
    }
  }
}



