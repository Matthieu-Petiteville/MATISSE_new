

for (scenario in c("ssTCO","AME")){
  for (horizon in c(2035)){
    for (scenario_classement in c("Optimiste","Median","Pessimiste")){
      for (redistribution in c("niveau_vie")){
        
        
        Iter=0
        print(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep=""))
        
        beg <- proc.time()
        print(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S"))
        
       
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_1_TC_DPE/3_1_3_Rehab_2010_horizon.R")
        
        
        source("D:/CIRED/Projet_Ademe/MATISSE/Step_3_Technical_Change/3_2_TC_VE/3_2_2_VE_2010_horizon.R")
        
        
        
        end <- proc.time() - beg
        print(paste(paste("Scenario : ",scenario,", horizon : ", horizon, ", scenario technique :",scenario_classement, " ,scenario redistributif :",redistribution,sep="")," : DONE (",end[3]/60," min)",sep=""))
        
        
        
      }
    }
  }
}
