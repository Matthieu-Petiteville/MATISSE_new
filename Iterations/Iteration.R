
# 
# Iter=1
# horizon=2025
# scenario="AMS"
# Iter_last=ifelse(Iter==0,0,Iter-1)
# scenario_classement="Optimiste"
# redistribution="forfait"

library(data.table)
# 
# scenario="AMS"
args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
iter=args[3]
scenario_classement=args[4]
redistribution=args[5]
# scenario_classement="Optimiste"
Iter<-as.numeric(iter)
Iter_last=ifelse(Iter==0,0,Iter-1)
horizon=as.numeric(horizon)

# sink('D:/CIRED/Projet_Ademe/IMACLIM/hello.txt',append=F,type="output")
# cat(scenario," & ",Iter," & ", Iter_last," & ",horizon)
# sink(NULL)

if(Iter==0){
  print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
  print("Iter 0")
  Input_macro<-read.csv(file=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/export_Iter_0.csv",sep=""))
  # Output_micro<-read.csv(file="D:/CIRED/Projet_Ademe/IMACLIM/essai_output_micro.csv")
  file.remove("D:/CIRED/Projet_Ademe/IMACLIM/Input_macro.csv")
  data.table::fwrite(Input_macro,file="D:/CIRED/Projet_Ademe/IMACLIM/Input_macro.csv",dec=".",sep=";",col.names = F)
  
  imaclim_xl<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/IMACLIM 3ME.xlsm",sheet="Model")
  if (file.exists(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))){
    file.remove(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))}
  write_xlsx(imaclim_xl,path=paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""))
  
  print("Fin Iter_0")
  
  
  
}else{
  print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
  print(paste("ITER number : ",Iter,sep=" "))
  print("0_donnees_IMACLIM")
  source("D:/CIRED/Projet_Ademe/Iteration_macro/0_donnees_IMACLIM.R")
  print("1_Mise_echelle")
  source("D:/CIRED/Projet_Ademe/Iteration_macro/1_Mise_echelle.R")

  # source("D:/CIRED/Projet_Ademe/Iteration_macro/2_contraintes_macro.R")
  # 
  # source("D:/CIRED/Projet_Ademe/Iteration_macro/3_contraintes_micro.R")
  # 
  # source("D:/CIRED/Projet_Ademe/Iteration_macro/4_pondR.R")
  # 
  # source("D:/CIRED/Projet_Ademe/Iteration_macro/6_export_share_savings.R")
    }
# 
