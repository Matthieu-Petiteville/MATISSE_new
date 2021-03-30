# Objectif : lancer tous les étapes de la première itération d'un scénario à partir d'une boucle (jusqu'au renvoi dans IMACLIM-3ME)


# Launch ------------------------------------------------------------------



for (scenario in c("AMS")){
  for (horizon in c(2035)){
    for (scenario_classement in c("Optimal_co2","Optimal_ener")){
      for (redistribution in c("decile")){
        
        
        Iter=0
        print(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep=""))
        
        beg <- proc.time()
        print(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S"))
        
        ####
        # Step 1
        ####
        
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_1_Mise_echelle/0_mise_forme_data_3ME.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_1_Mise_echelle/1.1_mise_echelle_revenus.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_1_Mise_echelle/2.1_retrocession_carbon_tax.R")
        
        ####
        # Step 2
        ####
        
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_2_Microsimulation/1.1_Microsimulation.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_2_Microsimulation/2.1_evolution_conso_energie.R")
        
        
        ####
        # Step 3
        ####
        
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_1_TC_DPE/3_1_1_Achat_neuf_horizon.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_1_TC_DPE/3_1_2_Achat_2010_horizon-1.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_1_TC_DPE/3_1_3_Rehab_2010_horizon.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_1_TC_DPE/3_1_4_Bascule_A.R")
        
        
        # source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_2_TC_VE/3_2_1_VE_classement_horizon.R")
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_2_TC_VE/3_2_2_VE_2010_horizon.R")
        
        ####
        # Step 4
        ####
        
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/1_contraintes_macro.R")
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/2_contraintes_micro.R")
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/3_pondr.R")
        
        ####
        # Step 5
        ####
        
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_5_export_IMACLIM/1_export_to_IMACLIM.R")
        
        
        end <- proc.time() - beg
        print(paste(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep="")," : DONE (",end[3]/60," min)",sep=""))
        
        
        
      }
    }
  }
}



