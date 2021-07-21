
# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
M_analysis <- gsub("\\\\","/",Sys.getenv("MATISSE_ANALYSIS"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))

library(tidyverse)
library(scales)

# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
Iter=0
scenario <- "AMS"
horizon <- 2035
scenario_classement <- "Optimiste"
redistribution <- "forfait"
initializeMatisseFiles()


# Data --------------------------------------------------------------------

load(MatisseFiles$menage_iteration_params_rd)
load(MatisseFiles$FC_2010_horizon_rd)
coeff_dep_ems<-read_csv(MatisseFiles$coeff_dep_ems_csv)
load(MatisseFiles$coeff_ems_2010_rd)
load(MatisseFiles$IMACLIM_rd)
menage <- menage_iteration

s=scenario
h=horizon

# Taxe carbone €/tonne CO2eq
TCO<-as.numeric(IMACLIM %>% 
                  filter(year==horizon) %>%
                  filter(model=="IMACLIM")%>%
                  filter(Variable=="TCO")%>%
                  select(value))*10^6

FC_coeff <- 
  coeff_dep_ems %>%
  filter(year==h)%>%
  filter(scenario==s)%>%
  select(Variable,FC_coeff_emission)%>%
  spread(key=Variable,value=FC_coeff_emission)

menage <- 
  menage %>% 
  mutate(CL_2010 = 0)%>% 
  mutate(Oil_2010 = (carb_lubr + dep_Fuel + dep_GPL)/FC$A07)%>%
  mutate(Gaz_2010 = (dep_Gaz + dep_Urbain + dep_Solides)/FC$A03)

menage <- 
  menage %>% 
  mutate(TCO_CL = CL_2010 * coeff_ems_2010$coeff_CL_2010 * FC_coeff$C21 * TCO) %>%
  mutate(TCO_Oil = Oil_2010 * coeff_ems_2010$coeff_Oil_2010 * FC_coeff$C22 * TCO) %>%
  mutate(TCO_Gaz = Gaz_2010 * coeff_ems_2010$coeff_Gaz_2010 * FC_coeff$C24 * TCO)


gran_v <- (1:100)/100
tuu_vec <- c(sort(unique(menage$tuu)))
dec_vec <- c(sort(unique(menage$decuc2)))
model_line <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2 + length(gran_v)))
colnames(model_line) = c("Dec","Tuu", percent(gran_v))

res_df <- data.frame()
#Calculs
for(i in 1:length(dec_vec)){
  for(j in 1:length(tuu_vec)){
    
    sub_men <- menage %>% filter(tuu == tuu_vec[j], decuc2 == dec_vec[i]) %>% mutate(TCO_tot = TCO_CL + TCO_Oil + TCO_Gaz)
    sub_men$TCO_tot[which(sub_men$TCO_tot < 0)] <- 0
    sub_men$pondmen[which(sub_men$pondmen < 0)] <- 0
    sub_men <- sub_men[order(sub_men$TCO_tot),] 
    sub_men$cumsum_pct <- cumsum(sub_men$pondmen) / sum(sub_men$pondmen)

    temp_line <- model_line
    temp_line$Dec = dec_vec[i]
    temp_line$Tuu = tuu_vec[j]
    
    for(k in 1:length(gran_v)){
      l_idx <- which(sub_men$cumsum_pct < gran_v[k])
      if(length(l_idx) > 0 ){
        b_low <- max(l_idx)
        b_up  <- min(nrow(sub_men), b_low +1)
        
        temp_line[,2+k] = sub_men$TCO_tot[b_low] + (gran_v[k] - sub_men$cumsum_pct[b_low]) / 
                          (sub_men$cumsum_pct[b_up] - sub_men$cumsum_pct[b_low]) * 
                          (sub_men$TCO_tot[b_up] - sub_men$TCO_tot[b_low])
      }else{
        temp_line[,2+k] = NA 
      }
    }
    res_df <- rbind(res_df, temp_line)
    
    
  }
}


write.table(res_df,paste(M_analysis,"/StudyTCO/Tab_PctSurcomp.csv",sep=""), row.names = F, quote = F, sep = ";", dec = ",")
