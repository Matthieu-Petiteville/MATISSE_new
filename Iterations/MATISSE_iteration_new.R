
# Objectif : itération macro-micro

# Fonctionnement : Le code MATISSE_iteration est appelé par la macro VBA Sub Iteration du fichier IMACLIM 3ME.xlsx
# Pour l'itération notée 0, c'est-à-dire directement issue de la macro ThreeME, le code télécharge directement le résultat des 5 premières étapes exécutées en amont et stocké dans le fichier export_Iter_0.csv de chaque scénario
# les résultats micro agrégés sont importés dans "Input_macro.csv" sur lequel va boucler IMACLIM 3ME
# pour les itérations suivantes, ce script fait appel aux étapes itérées : étape 1 de mise à l'échelle, étape 2 de microsimulation et étape 5 d'export des résultats agrégés. 

# LIBRARY -----------------------------------------------------------------

library(data.table)
library(readxl)
library(writexl)

Iter_last=ifelse(Iter==0,0,Iter-1)

# ITERATION_0 -------------------------------------------------------------

if(Iter==0){
  
  print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
  print("Iter 0")
  Input_macro <- read.csv(file= MatisseFiles$export_iter_0_csv)
  file.remove(MatisseFiles$input_macro_csv)
  data.table::fwrite(Input_macro,file=MatisseFiles$input_macro_csv,dec=".",sep=";",col.names = F)

  
  if(redistribution=="ssrec"){
    imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_ssrec_xl,sheet="Model")
  }else{
    imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_xl,sheet="Model")
  }
  
  excel_file_name <- MatisseFiles$IMACLIM_iter_xl
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
