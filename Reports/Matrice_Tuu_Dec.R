
# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
M_analysis <- gsub("\\\\","/",Sys.getenv("MATISSE_ANALYSIS"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))



# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
Iter=0
scenario <- "AMS"
horizon <- 2035
scenario_classement <- "Optimiste"
# scenario_classement_veh <- "Median"
# scenario_classement_bascule <<- "Optimiste"

redistribution <- "forfait"
# redistribution <- "niveau_vie"
# redistribution <- "decile"
# redistribution <- "tuu"
# redistribution <- "dectuu_grad"
# redistribution <- "seq_dectuu"
# redistribution <- "dectuu_seq_ucmat"


initializeMatisseFiles()

library(tidyverse)

load(MatisseFiles$menage_iteration_params_rd)
load(MatisseFiles$FC_2010_horizon_rd)
coeff_dep_ems<-read_csv(MatisseFiles$coeff_dep_ems_csv)
load(MatisseFiles$coeff_ems_2010_rd)
load(MatisseFiles$IMACLIM_rd)


# TCO<-as.numeric(read_excel(path=paste("D:/CIRED/Projet_Ademe/Results/",s,"/",h,"/",sc,"/",r,"/IMACLIM_3ME.xlsx",sep=""),range="C103",col_names=F))*10^6
menage <- menage_iteration

s=scenario
h=horizon


# Load datas --------------------------------------------------------------

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


# Calculations -------------------------------------------------------------

#Matrice
tuu_vec <- c(sort(unique(menage$tuu)), "Total")
dec_vec <- c(sort(unique(menage$decuc2)),"Total")
table_df <- data.frame(Tab_name = c("TCO_paid_mat", "RDB_mat", "men_mat", "uc_mat", 
                                    "TCO_rdb_mat", "TCO_men_mat", "TCO_uc_mat", 
                                    "TCO_rev_mat", 
                                    "TCO_net_mat", 
                                    "TCO_net_RDB_mat", "TCO_net_men_mat", "TCO_net_uc_mat", 
                                    "Pct_surcomp_mat", 
                                    "Pct_over5_TCO2RDB"),
                         Title = c("TCO paid", "RDB", "men", "uc", 
                                   "TCO/RDB", "TCO/men", "TCO/uc", 
                                   "TCO rev",  
                                   "TCO net",
                                   "TCO net/RDB", "TCO net/men", "TCO net/UC",
                                   "%Men Surcomp",
                                   "%Men TC >5% RDB"))
mat_list <- list()
for(tab in 1:nrow(table_df)){
  mat_list[[table_df$Tab_name[tab]]] <- matrix(data = NA, nrow = length(dec_vec), ncol = length(tuu_vec))
  rownames(mat_list[[table_df$Tab_name[tab]]]) <- dec_vec
  colnames(mat_list[[table_df$Tab_name[tab]]]) <- tuu_vec
}
TCO_mat_tot <- data.frame()

#Calculs
for(i in 1:length(dec_vec)){
  for(j in 1:length(tuu_vec)){

    if(dec_vec[i] == "Total" && tuu_vec[j] == "Total"){sub_men <- menage} 
    if(dec_vec[i] != "Total" && tuu_vec[j] == "Total"){sub_men <- menage %>% filter(decuc2 == dec_vec[i])} 
    if(dec_vec[i] == "Total" && tuu_vec[j] != "Total"){sub_men <- menage %>% filter(tuu == tuu_vec[j])} 
    if(dec_vec[i] != "Total" && tuu_vec[j] != "Total"){sub_men <- menage %>% filter(tuu == tuu_vec[j], decuc2 == dec_vec[i])}
    
    sub_men <- sub_men %>% mutate(TCO_tot = TCO_CL + TCO_Oil + TCO_Gaz)
    sub_men$TCO_tot[which(sub_men$TCO_tot < 0)] <- 0
    
    TCO_l <- sub_men %>%
      summarise(TCO_paid = sum(TCO_tot * pondmen),
                uc = sum(pondmen * coeffuc),
                men = sum(pondmen),
                TCO_rev = sum(rev_TCO * pondmen),
                RDB = sum(RDB * pondmen))
    
    
    mat_list[["TCO_paid_mat"]][i, j] = TCO_l$TCO_paid
    mat_list[["RDB_mat"]][i, j] = TCO_l$RDB
    mat_list[["men_mat"]][i, j] = TCO_l$men
    mat_list[["uc_mat"]][i, j] = TCO_l$uc
    mat_list[["TCO_rdb_mat"]][i, j] = TCO_l$TCO_paid / TCO_l$RDB  
    mat_list[["TCO_men_mat"]][i, j] = TCO_l$TCO_paid / TCO_l$men  
    mat_list[["TCO_uc_mat"]][i, j] = TCO_l$TCO_paid / TCO_l$uc  
    mat_list[["TCO_rev_mat"]][i, j] = - TCO_l$TCO_rev
    mat_list[["TCO_net_mat"]][i,j] = TCO_l$TCO_paid - TCO_l$TCO_rev
    mat_list[["TCO_net_RDB_mat"]][i,j] = (TCO_l$TCO_paid - TCO_l$TCO_rev) / TCO_l$RDB
    mat_list[["TCO_net_men_mat"]][i,j] = (TCO_l$TCO_paid - TCO_l$TCO_rev) / TCO_l$men
    mat_list[["TCO_net_uc_mat"]][i,j] = (TCO_l$TCO_paid - TCO_l$TCO_rev) / TCO_l$uc
    
    mat_list[["Pct_surcomp_mat"]][i,j] = sum(sub_men$pondmen[which(sub_men$TCO_tot < (TCO_l$TCO_rev / TCO_l$men ))]) / 
                                          sum(sub_men$pondmen)
    mat_list[["Pct_over5_TCO2RDB"]][i,j] = sum(sub_men$pondmen[which(sub_men$TCO_tot > sub_men$RDB * 0.05)]) / 
                                          sum(sub_men$pondmen)
    
  }
}

for(tab in 1:nrow(table_df)){
  temp_df <- cbind(data.frame(Type = rep(table_df$Title[tab], length(dec_vec)),
                              Dec = dec_vec, 
                              as.data.frame(mat_list[[table_df$Tab_name[tab]]])))
  TCO_mat_tot <- rbind(TCO_mat_tot, temp_df)
}
colnames(TCO_mat_tot) <- c("Type","Dec",tuu_vec)
write.table(TCO_mat_tot,paste(M_analysis,"/StudyTCO/Results.csv",sep=""), row.names = F, quote = F, sep = ";", dec = ",")



