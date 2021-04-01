
# LIBRARIES ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(writexl)
source(paste(M_home,"/Common/tools.R",sep=""))



# PARAMETRES ITERATION ----------------------------------------------------

args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
iter=args[3]
scenario_classement=args[4]
redistribution=args[5]
Iter<-as.numeric(iter) 
Iter_last<-Iter-1
horizon=as.numeric(horizon)


# Output_macro_code -------------------------------------------------------

# Read
output_macro<-read_excel(path = paste(M_data,"/IMACLIM/Output_macro_code.xlsx",sep=""),skip=1,sheet=scenario)

# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep="") 
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(output_macro,path=excel_file_name)

# Traitement
output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")


# FACTEUR de CROISSANCE FC ------------------------------------------------

FC <- 
  IMACLIM %>%
  filter(year==9999) %>%
  filter(model=="Marge")%>% #Careful, model here is "Marge" and not IMACLIM or ThreeME
  filter(Variable %in% c("revact",
                         "revpat",
                         "revchom",
                         "revsoc",
                         "revetranger",
                         "rdb",
                         "tauIR",
                         "tauAID",
                         "A01",
                         "A02",
                         "A03",
                         "A04",
                         "A05",
                         "A06",
                         "A07",
                         "A08",
                         "A09",
                         "A10",
                         "A11",
                         "A12",
                         "A13",
                         "A14")) %>%
  select(Variable,value)

print(FC)
# Save
save(FC,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_",Iter,"/Input/FC_2010_",as.character(horizon),".RData",sep=""))


# Output_micro ------------------------------------------------------------

# Read
output_micro<-read_excel(path = paste(M_data,"/IMACLIM/Output_micro.xlsx",sep=""),sheet=paste(scenario,horizon,sep=""))
# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/Output_micro_iter",Iter,".xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(output_micro,path=excel_file_name)



# IMACLIM.xlsx ------------------------------------------------------------

if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME_ssrec.xlsm",sep=""),sheet="Model")
}else{
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME.xlsm",sep=""),sheet="Model")
}
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep="")
if (file.exists(excel_file_name)){file.remove(excel_file_name)}
write_xlsx(imaclim_xl,path=excel_file_name)


# NOMBRE d'ITERATIONS -----------------------------------------------------

#Nb iterations
Nb_iter<-read_excel(path=paste(M_data,"/IMACLIM/Iterations_scenarios.xlsx",sep=""))
Nb_iter<-
  Nb_iter %>% 
  mutate_when(Scenario==scenario & 
                Horizon==horizon & 
                Scenario_classement==scenario_classement & 
                Redistribution==redistribution, 
              list(Iter_finale=ifelse(Iter==10,Iter-1,Iter)))

# Iteration_scenarios_n-1
# sécurité pour garder trace de Iterations_scenarios du scénario précédent en cas de bug
excel_file_name <- paste(M_data,"/IMACLIM/Iterations_scenarios_n-1.xlsx",sep="")
if(file.exists(excel_file_name)){
  file.remove(excel_file_name)}
file.rename(from=paste(M_data,"/IMACLIM/Iterations_scenarios.xlsx",sep=""),to=excel_file_name)


# Write
write_xlsx(Nb_iter,path=paste(M_data,"/IMACLIM/Iterations_scenarios.xlsx",sep=""))
write_xlsx(Nb_iter,path=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Iterations_scenarios.xlsx",sep=""))        


# RESULTS -----------------------------------------------------------------

#Results
unlink(paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""),force=T)

### Output_micro
# Read
output_micro<-read_excel(path = paste(M_data,"/IMACLIM/Output_micro.xlsx",sep=""),sheet=paste(scenario,horizon,sep=""))
# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro.xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(output_micro,path=excel_file_name)


### IMACLIM
# Read
if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME_ssrec.xlsm",sep=""),sheet="Model")
}else{
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME.xlsm",sep=""),sheet="Model")}
# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME.xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(imaclim_xl,path=excel_file_name)

### Output_macro
output_macro<-read_excel(path = paste(M_data,"/IMACLIM/Output_macro_code.xlsx",sep=""),skip=1,sheet=scenario)
# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(output_macro,path=excel_file_name)


## Menage_iteration
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_iteration.RData",sep=""))

save(menage_iteration,file=paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/menage_iteration.RData",sep=""))

