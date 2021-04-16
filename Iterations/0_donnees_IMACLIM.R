
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)


# OUTPUT_MACRO_CODE ---------------------------------------------------------

# Read
output_macro<-read_excel(path = MatisseFiles$output_macro_code_xl ,skip=1,sheet=scenario)

# Erase output_macro_code
if(file.exists(MatisseFiles$output_macro_iter_xl)){file.remove(MatisseFiles$output_macro_iter_xl)}
# Write output_macro_code
write_xlsx(output_macro,path=MatisseFiles$output_macro_iter_xl)
  

output_macro <-
  output_macro %>%
  gather(key=year_model,value=value,-c(1:3))


# IMACLIM -----------------------------------------------------------------

IMACLIM<-
  output_macro  %>%
  separate(col="year_model",into=c("year","model"),sep="_")
rm(output_macro)
save(IMACLIM,file= MatisseFiles$IMACLIM_iter_rd)



# OUTPUT_MICRO ------------------------------------------------------------
#Sauv Output_micro

# Read
output_micro<-read_excel(path = MatisseFiles$output_micro_xl,sheet=paste(scenario,horizon,sep=""))

# Remove previous file
if(file.exists(MatisseFiles$output_micro_iterlast_xl)){file.remove(MatisseFiles$output_micro_iterlast_xl)}
# Write
write_xlsx(output_micro,path=MatisseFiles$output_micro_iterlast_xl)

# SAVE IMACLIM.xlsx ------------------------------------------------------

###
# SANS RETROCESSION : IMACLIM_ssREC
###
#Possible d'optimiser en faisant des copy de fichiers simples (file.copy)

if(redistribution=="ssrec"){
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_ssrec_xl,sheet="Model")
  
}else{
  imaclim_xl<-read_excel(path = MatisseFiles$IMACLIM_3ME_xl,sheet="Model")
}
if (file.exists(MatisseFiles$IMACLIM_iter_xl)){file.remove(MatisseFiles$IMACLIM_iter_xl)}
write_xlsx(imaclim_xl,path=MatisseFiles$IMACLIM_iter_xl)



