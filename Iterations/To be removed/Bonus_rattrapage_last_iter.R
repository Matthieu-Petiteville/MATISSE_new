# Objectif : lorsque mauvais export des données, exécution du script 2_last_iter.

# Fonction appelée par la macro VBA "Sub rattrapage_Output_macro_code()" de IMACLIM 3ME (Module 1)


# LIBRARY -----------------------------------------------------------------

library(data.table)
library(readxl)
library(tidyverse)


# PARAMETRES ITERATION ----------------------------------------------------


args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
# iter=args[3]
scenario_classement=args[3]
redistribution=args[4]
# Iter<-as.numeric(iter)
# Iter_last=ifelse(Iter==0,0,Iter-1)
horizon=as.numeric(horizon)

print(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))

# for (i in 1:5)
# {
#   print(i)
#   date_time<-Sys.time()
#   while((as.numeric(Sys.time()) - as.numeric(date_time))<2.5){} #dummy while loop
# }
# print("End Wait")


# RECOVER NB Iteration ----------------------------------------------------


Nb_iter<-read_excel(path="D:/CIRED/Projet_Ademe/IMACLIM/Iterations_scenarios.xlsx")
Iter<-
  as.numeric(Nb_iter %>% 
  filter(Scenario==scenario & 
           Horizon==horizon & 
           Scenario_classement==scenario_classement & 
           Redistribution==redistribution)%>%
  select(Iter_finale))


# Read export_iter --------------------------------------------------------

export_iter<-read.csv(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/export_Iter_",Iter,".csv",sep=""))
data.table::fwrite(export_iter,file="D:/CIRED/Projet_Ademe/IMACLIM/export_iter.csv",dec=".",sep=";",col.names = F,row.names = F)
