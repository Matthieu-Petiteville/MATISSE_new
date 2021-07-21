
# LIBRARIES ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(writexl)
source(paste(M_home,"/Common/tools.R",sep=""))



# PARAMETRES ITERATION ----------------------------------------------------

args<-commandArgs(trailingOnly=T)
# scenario=args[1]
# horizon=args[2]
# iter=args[3]
# scenario_classement=args[4]
# redistribution=args[5]
# Iter<-as.numeric(iter) 
Iter_last<-Iter-1
horizon=as.numeric(horizon)


# Output_macro_code -------------------------------------------------------

# Read
output_macro<-read_excel(path = MatisseFiles$output_macro_code_xl,skip=1,sheet=scenario)

# Remove previous file
if(file.exists(MatisseFiles$output_macro_iter_xl)){file.remove(MatisseFiles$output_macro_iter_xl)}
# Write
write_xlsx(output_macro,path=MatisseFiles$output_macro_iter_xl)

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
save(FC,file=MatisseFiles$FC_horizon_iter_rd)



# Output_micro ------------------------------------------------------------

# Read
output_micro<-read_excel(path = MatisseFiles$output_micro_xl,sheet=paste(scenario,horizon,sep=""))

# Remove previous file
if(file.exists(MatisseFiles$output_micro_iter_xl)){file.remove(MatisseFiles$output_micro_iter_xl)}
# Write
write_xlsx(output_micro,path=MatisseFiles$output_micro_iter_xl)



# IMACLIM.xlsx ------------------------------------------------------------

if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_ssrec_xl,sheet="Model")
}else{
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_xl,sheet="Model")
}
if (file.exists(MatisseFiles$IMACLIM_iter_xl)){file.remove(MatisseFiles$IMACLIM_iter_xl)}
write_xlsx(imaclim_xl,path=MatisseFiles$IMACLIM_iter_xl)


# NOMBRE d'ITERATIONS -----------------------------------------------------

#Nb iterations
Nb_iter<-read_excel(path=MatisseFiles$iterations_scen_xl)
Nb_iter<-
  Nb_iter %>% 
  mutate_when(Scenario==scenario & 
                Horizon==horizon & 
                Scenario_classement==scenario_classement & 
                Redistribution==redistribution, 
              list(Iter_finale=ifelse(Iter==10,Iter-1,Iter)))

# Iteration_scenarios_n-1
# sécurité pour garder trace de Iterations_scenarios du scénario précédent en cas de bug
if(file.exists(MatisseFiles$iterations_scen_n_1_xl)){file.remove(MatisseFiles$iterations_scen_n_1_xl)}
file.rename(from=MatisseFiles$iterations_scen_xl,to=MatisseFiles$iterations_scen_n_1_xl)


# Write
write_xlsx(Nb_iter,path=MatisseFiles$iterations_scen_xl)
write_xlsx(Nb_iter,path=MatisseFiles$iterations_scen_iter_xl)        

# RESULTS -----------------------------------------------------------------

#Results
unlink(MatisseFiles$folder_params,force=T)

### Output_micro
# Read
output_micro<-read_excel(path = MatisseFiles$output_micro_xl,sheet=paste(scenario,horizon,sep=""))
# Remove previous file
if(file.exists(MatisseFiles$output_micro_params_xl)){file.remove(MatisseFiles$output_micro_params_xl)}
# Write
write_xlsx(output_micro,path=MatisseFiles$output_micro_params_xl)


### IMACLIM
# Read
if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_ssrec_xl,sheet="Model")
}else{
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_xl,sheet="Model")
}

# Remove previous file
if(file.exists(MatisseFiles$IMACLIM_params_xl)){file.remove(MatisseFiles$IMACLIM_params_xl)}
# Write
write_xlsx(imaclim_xl,path=MatisseFiles$IMACLIM_params_xl)

### Output_macro
output_macro<-read_excel(path = MatisseFiles$output_macro_code_xl,skip=1,sheet=scenario)
# Remove previous file


if(file.exists(MatisseFiles$output_macro_params_xl)){file.remove(MatisseFiles$output_macro_params_xl)}
# Write
write_xlsx(output_macro,path=MatisseFiles$output_macro_params_xl)


## Menage_iteration
load(MatisseFiles$menage_iteration_iter_rd)
save(menage_iteration,file=MatisseFiles$menage_iteration_params_rd)

