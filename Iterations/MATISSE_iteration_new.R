
# Objectif : itération macro-micro

# Fonctionnement : Le code MATISSE_iteration est appelé par la macro VBA Sub Iteration du fichier IMACLIM 3ME.xlsx
# Pour l'itération notée 0, c'est-à-dire directement issue de la macro ThreeME, le code télécharge directement le résultat des 5 premières étapes exécutées en amont et stocké dans le fichier export_Iter_0.csv de chaque scénario
# les résultats micro agrégés sont importés dans "Input_macro.csv" sur lequel va boucler IMACLIM 3ME
# pour les itérations suivantes, ce script fait appel aux étapes itérées : étape 1 de mise à l'échelle, étape 2 de microsimulation et étape 5 d'export des résultats agrégés. 

# LIBRARY -----------------------------------------------------------------

library(data.table)
library(readxl)
library(writexl)
source(paste(Sys.getenv("MATISSE_HOME"),"/MATISSE_init.r",sep=""))

# PARAMETRES ITERATION ----------------------------------------------------



# ITERATION_0 -------------------------------------------------------------

if(Iter==0){
  
  print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
  print("Iter 0")
  Input_macro<-read.csv(file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/export_Iter_0.csv",sep=""))
  file.remove(paste(M_data,"/IMACLIM/Input_macro.csv",sep=""))
  data.table::fwrite(Input_macro,file=paste(M_data,"/IMACLIM/Input_macro.csv",sep=""),dec=".",sep=";",col.names = F)

  
  if(redistribution=="ssrec"){
    imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME_ssrec.xlsm",sep=""),sheet="Model")
  }else{
    imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME.xlsm",sep=""),sheet="Model")
  }
  
  excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep="")
  if (file.exists(excel_file_name)){file.remove(excel_file_name)}
  write_xlsx(imaclim_xl,path=excel_file_name)

  print("Fin Iter_0")


}else{
  # ITERATION 1 et suivantes ------------------------------------------------
  print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
  print(paste("ITER number : ",Iter,sep=" "))
 

  source(paste(M_home,"/Iterations/0_donnees_IMACLIM.R",sep=""))
  source(paste(M_home,"/Iterations/1_Step_1_5.R",sep=""))
  
}
