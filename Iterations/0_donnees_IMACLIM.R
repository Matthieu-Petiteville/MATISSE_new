
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)



# OUTPUT_MACRO_CODE ---------------------------------------------------------

# Read
output_macro<-read_excel(path = paste(M_data,"/IMACLIM/Output_macro_code.xlsx",sep=""),skip=1,sheet=scenario)

# Erase output_macro_code
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write output_macro_code
write_xlsx(output_macro,path=excel_file_name)
  

output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))


# IMACLIM -----------------------------------------------------------------

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")
rm(output_macro)
save(IMACLIM,file=
       paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM.RData",sep=""))



# OUTPUT_MICRO ------------------------------------------------------------
#Sauv Output_micro

# Read
output_micro<-read_excel(path = paste(M_data,"/IMACLIM/Output_micro.xlsx",sep=""),sheet=paste(scenario,horizon,sep=""))

# Remove previous file
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter_last,"/Output/Output_micro_iter",Iter_last,".xlsx",sep="")
if(file.exists(excel_file_name)){file.remove(excel_file_name)}
# Write
write_xlsx(output_micro,path=excel_file_name)

# SAVE IMACLIM.xlsx ------------------------------------------------------

###
# SANS RETROCESSION : IMACLIM_ssREC
###
#Possible d'optimiser en faisant des copy de fichiers simples (file.copy)

if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME_ssrec.xlsm",sep=""),sheet="Model")
}else{
  imaclim_xl<-read_excel(path = paste(M_data,"/IMACLIM/IMACLIM 3ME.xlsm",sep=""),sheet="Model")
}
excel_file_name <- paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep="")
if (file.exists(excel_file_name)){file.remove(excel_file_name)}
write_xlsx(excel_file_name)



