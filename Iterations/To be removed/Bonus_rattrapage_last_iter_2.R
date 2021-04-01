
# Objectif : lorsque mauvais export des données, export du Output_macro_code.xlsx à la suite du script Bonus_rattrapage_last_iter.R

# Fonction appelée par la macro VBA "Sub rattrapage_Output_macro_code()" de IMACLIM 3ME (Module 1)


# Code --------------------------------------------------------------------



library(readxl)
library(tidyverse)
library(writexl)

args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
# iter=args[3]
scenario_classement=args[3]
redistribution=args[4]
# Iter<-as.numeric(iter)
# Iter_last=ifelse(Iter==0,0,Iter-1)
horizon=as.numeric(horizon)



### Output_macro
output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code.xlsx",skip=1,sheet=scenario)
# Remove previous file
if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""))){file.remove(paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""))}
# Write
write_xlsx(output_macro,path=paste("D:/CIRED/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""))
