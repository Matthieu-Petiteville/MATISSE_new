# Pour ne lancer qu'une partie des étapes




# Boucles -----------------------------------------------------------------


scenario_classement=c("Optimiste","Pessimiste","Median","Opt_CO2","Pess_CO2","Med_CO2",
                      "Opt_ener","Pess_ener","Med_ener",
                      "Rich","Poor","Median","VAN_SNBC","VAN_free","VAN_SNBC_lim")


for (scenario in c("AMS")){
  for (horizon in c(2035)){
    for (scenario_classement in c("VAN_SNBC","VAN_free","VAN_SNBC_lim","Optimiste","Pessimiste","Median","Opt_CO2")){
      for (redistribution in c("forfait")){
        for (subvention in c("relat_115")){
          
        
        
        Iter=0
        print(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep=""))
        
        beg <- proc.time()
        print(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S"))
        
        ####
        # Step 4
        ####
        
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/1_contraintes_macro.R")
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/2_contraintes_micro.R")
        source("D:/CIRED/Projet_Ademe/Publication/Subventions_menages/MATISSE_Subv/Step_4_Reweighting/3_pondr.R")
        
        ####
        # Step 5
        ####
        
        # source("D:/CIRED/Projet_Ademe/MATISSE/Step_5_export_IMACLIM/1_export_to_IMACLIM.R")
        
        
        
        end <- proc.time() - beg
        print(paste(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep="")," : DONE (",end[3]/60," min)",sep=""))
        
        }
        
      }
    }
  }
}
