# Import data
      # Mise en forme des hypothèses et trajectoires de ThreeME
        # Lecture depuis fichier excel, converti en Rdata format long
      # Lecture des données IMACLIM
# Output : ThreeME.RData


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)

setwd("D:/Stage_Petiteville/Projet_Ademe/")




# ThreeME -----------------------------------------------------------------


# To open Sorties_ThreeME, tab names
if(scenario=="AMS"){S="scen AMS"}
if(scenario=="AME"){S="scen AME"}
if(scenario=="ssTCO"){S="scen AMS ss TCO"}
if(scenario=="ssRES"){S="scen AMS ss residentiel"}
if(scenario=="ssVE"){ S="scen AMS ss VE"}


suppressWarnings(scen<-read_excel(path="D:/Stage_Petiteville/Projet_Ademe/IMACLIM/Sorties Three-ME.xlsx",sheet=S))

ThreeME<- scen %>% select(-Def)%>% gather(key=year, value=value, -c(1))

save(ThreeME, file=paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))





# IMACLIM -----------------------------------------------------------------


# VARIABLES MACRO
if(scenario_classement=="ssrec"){
  output_macro<-read_excel(path ="IMACLIM/Output_macro_code_iter_0_ssrec.xlsx",sheet=scenario,skip = 1)
} else {
  output_macro<-read_excel(path ="IMACLIM/Output_macro_code_iter_0.xlsx",sheet=scenario,skip = 1)
}
output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")

rm(output_macro)

save(IMACLIM,file=
       paste(scenario,"/",horizon,"/IMACLIM.RData",sep=""))



# Facteur_croissance_2010_horizon -----------------------------------------

FC <- 
  IMACLIM %>%
  filter(year==horizon) %>%
  filter(model=="IMACLIM")%>%
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

FC <- FC %>%
  mutate(value=as.numeric(value)) %>%
  spread(key=Variable,value=value) 


save(FC,file=paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))



# Emissions ---------------------------------------------------------------

EMS<-read_excel(path=paste("IMACLIM/EMS.xlsx",sep=""),range=paste(scenario,"!B1:AF5",sep=""),col_names=T)

save(EMS,file=paste(scenario,"/EMS.RData",sep=""))



# SUCCESS -----------------------------------------------------------------

print("Step 1 : 0_import_data_macro : SUCCESS")

