# Changement de mode de chauffage et ECS
# Bascule du chauffage et de l'ECS des ménages de classe A en 100% électrique. 

# LIBRARIES ---------------------------------------------------------------
library(plyr)
library(tidyverse)
library(decisionSupport)
library(readxl)
library(car)

source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/Repayment.R",sep=""))
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))

# DATA --------------------------------------------------------------------

if(!exists("class_force_bascule")){class_force_bascule <- c()}
if(!exists("year_new_bascule")){year_new_bascule <- 2100}
if(!exists("bascule_min_jump")){class_force_bascule <- 7}


#Importer taux de croissance des prix et des revenus
load(MatisseFiles$FC_2010_horizon_rd)
load(MatisseFiles$menage_echelle_33_rd)
load(MatisseFiles$source_usage_rd)
# Import des prix d'énergie par classe de ménage : en €/MWh
prix_classe <- read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";",dec = ".", fill = TRUE)

sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")
list_dep=c("agriculture",
           "dep_Elec",
           "dep_Gaz",
           "dep_GPL",
           "dep_Fuel",
           "dep_Urbain",
           "dep_Solides",
           "BTP",
           "prod_veh",
           "carb_lubr",
           "transp_rail_air",
           "transp_routes_eau",
           "loisirs_com",
           "autres_services",
           "autres",
           "loyers",
           "veh_occasion",
           "Hors_budget")


menage_echelle<-menage_echelle_33

#ExtractDomEfficiency gradient from 3ME
S <- switch (scenario,
             "AMS" = "scen AMS",
             "AME" = "scen AME",
             "ssTCO" = "scen AMS ss TCO",
             "ssRES" = "scen AMS ss residentiel",
             "ssVE" = "scen AMS ss VE")
suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = S)))  

ThreeME <- 
  scen %>%
  select(-Def) %>%
  gather(key = year, value = value, -c(1)) %>%
  filter(year %in% c(2010, horizon))

ThreeME <- ThreeME[grep("^ENER_BUIL_H01_C._2.11630/BUIL_H01_C._2$",ThreeME$Var),]

dom_eff_df <- data.frame(classe = LETTERS[1:7])
dom_eff_df$Code <- paste("ENER_BUIL_H01_C",dom_eff_df$classe,"_2*11630/BUIL_H01_C",dom_eff_df$classe,"_2", sep="")
dom_eff_df <- dom_eff_df %>% 
  left_join(ThreeME %>% filter(year == 2010), by = c("Code" = "Var")) %>% 
  mutate(Value_init = value) %>% 
  select(-year, -value)
dom_eff_df <- dom_eff_df %>% 
  left_join(ThreeME %>% filter(year == horizon), by = c("Code" = "Var")) %>% 
  mutate(Value_hor = value) %>% 
  select(-year, -value)
dom_eff_df <- dom_eff_df %>%
  mutate(Dom_eff = Value_hor/ Value_init - 1)



# PREPARATION DONNEES PRIX ENERGIE ----------------------------------------


# prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)

# MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------

# Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025.
prix_classe_horizon<-prix_classe
# A02
prix_classe_horizon$prix_elec<- prix_classe$prix_elec * FC$A02
# A03
prix_classe_horizon$prix_gaz<- prix_classe$prix_gaz * FC$A03
# A04
prix_classe_horizon[c("prix_fuel","prix_gpl")]<- prix_classe[c("prix_fuel","prix_gpl")]* FC$A04
prix_classe_horizon[c("prix_bois","prix_chaleur")]<- prix_classe[c("prix_bois","prix_chaleur")]* FC$A03

# CLASSER MENAGES PAR CLASSE ----------------------------------------------


# Matrice des prix de chaque énergie pour chaque classe
prix_classe_mat <- data.matrix(prix_classe_horizon[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)


# Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
menage_echelle$classe_men <- 
  with(
    menage_echelle,
    as.integer(interaction(MI_corr, typmen_corr, quintileuc)))

menage_echelle$classe_men <- 
  as.factor(menage_echelle$classe_men)


# Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
dummies_classe_men <- model.matrix(~ classe_men, 
                                   data = menage_echelle, 
                                   contrasts.arg = list(
                                     classe_men = contrasts(
                                       menage_echelle$classe_men,
                                       contrasts = F)
                                   )
)


#Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
dummies_classe_men <- dummies_classe_men[,-1] 






# PRIX ENERGIE PAR MENAGE -------------------------------------------------


# Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
prix_menages_horizon_bis<-as.data.frame(prix_menages_horizon)

# Rajout colonne "ident_men" pour la fusion avec det_ener
prix_menages_horizon<-cbind(menage_echelle$ident_men,prix_menages_horizon_bis)
# renommer les colonnes
colnames(prix_menages_horizon)<-c("ident_men",
                               "prix_Elec",
                               "prix_Gaz",
                               "prix_Fuel",
                               "prix_GPL",
                               "prix_Solides",
                               "prix_Urbain")

# Rajout des prix et pondération de chaque ménage dans dep_ener

menage_echelle <- menage_echelle %>% left_join(prix_menages_horizon,by="ident_men")


# VOLUMES ENERGIE / MENAGE ------------------------------------------------

menage_echelle <- menage_echelle %>% mutate_when(is.na(Gaz_ECS),list(Gaz_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(GPL_ECS),list(GPL_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Fuel_ECS),list(Fuel_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Solides_ECS),list(Solides_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Urbain_ECS),list(Urbain_ECS=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Gaz_chauff),list(Gaz_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(GPL_chauff),list(GPL_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Fuel_chauff),list(Fuel_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Solides_chauff),list(Solides_chauff=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Urbain_chauff),list(Urbain_chauff=0))


###
## NEW 16/04
###
menage_echelle <- menage_echelle %>% mutate_when(is.na(GPL_Cuisson),list(GPL_Cuisson=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Fuel_Cuisson),list(Fuel_Cuisson=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Solides_Cuisson),list(Solides_Cuisson=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Urbain_Cuisson),list(Urbain_Cuisson=0))
menage_echelle <- menage_echelle %>% mutate_when(is.na(Gaz_Cuisson),list(Gaz_Cuisson=0))



# Selection menages --------------------------------------------------------
menage_echelle <- menage_echelle %>%
  mutate(DPE_jump = - match(DPE_horizon,LETTERS) + match(DPE_pred,LETTERS))

menage_echelle <- 
  menage_echelle %>% 
  mutate(bascule=0)
  mutate(bascule=0)%>%
  mutate_when(classe_arr %in% class_force_bascule, list(bascule=1)) %>%
  mutate_when(year_neuf > year_new_bascule , list(bascule = 1)) %>%
  mutate_when(year_rehab > 0 & DPE_jump > bascule_min_jump, list(bascule = 1))
  
  
# menage_echelle[1:20,c("DPE_dep","DPE_pred","DPE_horizon","DPE_jump", "bascule", "classe_arr", "year_rehab", "year_neuf")]


# Bascule -----------------------------------------------------------------

for (x in c("Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Gaz_chauff",
            "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Gaz_Cuisson","GPL_Cuisson","Solides_Cuisson","Urbain_Cuisson","Fuel_Cuisson")){
  
  if (str_detect(x,"Gaz")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Gaz}
  if (str_detect(x,"GPL")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_GPL}
  if (str_detect(x,"Fuel")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Fuel}
  if (str_detect(x,"Urbain")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Urbain}
  if (str_detect(x,"Solides")){menage_echelle[paste(x,"vol",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Solides}
}

menage_echelle$vol_basc_elec_ECS <- rowSums(menage_echelle%>%select(ends_with("ECS_vol")))
menage_echelle$vol_basc_elec_chauff <- rowSums(menage_echelle%>%select(ends_with("chauff_vol")))
menage_echelle$vol_basc_elec_cuisson <- rowSums(menage_echelle%>%select(ends_with("Cuisson_vol")))



menage_echelle <-
  menage_echelle %>%
  mutate(dep_basc_elec_ECS=vol_basc_elec_ECS*prix_Elec)%>%
  mutate(dep_basc_elec_chauff=vol_basc_elec_chauff*prix_Elec)%>%
  mutate(dep_basc_elec_cuisson=vol_basc_elec_cuisson*prix_Elec)


menage_echelle$dep_non_elec<-rowSums(menage_echelle %>% select(c("Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Gaz_chauff","GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Gaz_Cuisson","GPL_Cuisson","Solides_Cuisson","Urbain_Cuisson","Fuel_Cuisson")))

solde<- 
  menage_echelle %>%
  mutate(solde=dep_basc_elec_ECS+dep_basc_elec_chauff+dep_basc_elec_cuisson-dep_non_elec #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
  )%>%
  select(ident_men,solde,bascule)


solde<-
  solde %>% 
  mutate_when(bascule==0,list(solde=0))%>%
  select(-bascule)

# menage_echelle%>%filter(ident_men %in% c(sapply(solde%>%filter(classe_arr=="A" & solde==0)%>%select(ident_men),function(x) as.numeric(x))))%>% 
#   filter(!dep_basc_elec_ECS==0) %>%
#   filter( !dep_basc_elec_chauff==0)%>%
#   filter(!dep_non_elec==0)%>%
#   select(ident_men)
# 
# table(solde$solde==0)
# 
# FALSE  TRUE 
# 739  9550 
# > table(menage_echelle$classe_arr=="A")
# 
# FALSE  TRUE 
# 9422   867 
# > table(menage_echelle %%> filter(dep_non_elec==0)%>%select(classe_arr)=="A")
# Error: unexpected '>' in "table(menage_echelle %%>"
# > table(menage_echelle%>%filter(dep_non_elec==0)%>%select(classe_arr)=="A")
# 
# FALSE  TRUE 
# 2117   128 
# > 867-128
# [1] 739


menage_echelle <- 
  menage_echelle %>%
  mutate_when(bascule==1,list(Gaz_ECS=0,
                              GPL_ECS=0,
                              Fuel_ECS=0,
                              Solides_ECS=0,
                              Urbain_ECS=0,
                              Gaz_chauff=0,
                              GPL_chauff=0,
                              Fuel_chauff=0,
                              Solides_chauff=0,
                              Urbain_chauff=0,
                              Gaz_Cuisson=0,
                              GPL_Cuisson=0,
                              Solides_Cuisson=0,
                              Urbain_Cuisson=0,
                              Fuel_Cuisson=0,
                              Elec_chauff=Elec_chauff+dep_basc_elec_chauff,
                              Elec_ECS=Elec_ECS+dep_basc_elec_ECS,
                              Elec_Cuisson=Elec_Cuisson+dep_basc_elec_cuisson,
                              dep_Gaz=dep_Gaz-Gaz_ECS-Gaz_chauff-Gaz_Cuisson,
                              dep_GPL=dep_GPL-GPL_ECS-GPL_chauff-GPL_Cuisson,
                              dep_Fuel=dep_Fuel-Fuel_ECS-Fuel_chauff-Fuel_Cuisson,
                              dep_Solides=dep_Solides-Solides_ECS-Solides_chauff-Solides_Cuisson,
                              dep_Urbain=dep_Urbain-Urbain_ECS-Urbain_chauff-Urbain_Cuisson,
                              dep_Elec=dep_Elec+dep_basc_elec_chauff+dep_basc_elec_ECS+dep_basc_elec_cuisson))


#Ajustement efficacité domicile
usages <- c("ECS", "chauff", "clim", "Cuisson", "ecl", "ElecSpe")
sources <- c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dom_eff_DPE_vec <- dom_eff_df$Dom_eff[match(menage_echelle$DPE_horizon , dom_eff_df$classe)]

for(so in sources){
  dom_eff_compo_vec <- (1 + dom_eff_DPE_vec) * (1 + dom_effic_source$eff_gain[which(dom_effic_source$sources == so)])
  
  for(us in usages){
    my_col <- paste(so,us,sep="_")
    if(my_col %in% names(menage_echelle)){
      solde["solde"] <-  solde$solde + menage_echelle[,my_col] * (dom_eff_compo_vec - 1)
      menage_echelle[,my_col] <- menage_echelle[,my_col] * dom_eff_compo_vec
    }      
  }
  menage_echelle[paste("dep",so,sep="_")] <- menage_echelle[paste("dep",so,sep="_")] * dom_eff_compo_vec
}



# Reventilation -----------------------------------------------------------


sauv_avant_reventil<-menage_echelle
menage_echelle_34<-Ventil_solde(solde,menage_echelle,step="REHAB")
menage_echelle_34 <- menage_echelle %>% select(colnames(menage_echelle_33))

menage_ener_dom<-energie_dom_surf(menage_echelle_34)
menage_echelle_34<- 
  menage_echelle_34 %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(menage_ener_dom,by="ident_men")




# VERIFICATION ------------------------------------------------------------

# Verif Finale
for (source in sources){
  dep_source_verif=paste("dep",source,"verif",sep="_")
  menage_echelle_34[dep_source_verif]<-rowSums(menage_echelle_34 %>% select(ident_men, list_source_usage) %>% select(contains(source)))
}


# 
# 
# table(abs(menage_echelle_34$dep_Elec-menage_echelle_34$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle_34$dep_Gaz-menage_echelle_34$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle_34$dep_GPL-menage_echelle_34$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle_34$dep_Fuel-menage_echelle_34$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle_34$dep_Solides-menage_echelle_34$dep_Solides_verif)<10^(-10))
# 
# 
# 


# Ecraser colonnes surnuméraires ------------------------------------------



# inter<-intersect(colnames(menage_echelle), colnames(menage_echelle_33))
# colnames(menage_echelle_34 %>% select(-inter))

# dep_ener_horizon_43<-menage_echelle_43 %>% select(ident_men,not_inter,-DPE_2024)
# menage_echelle_34<-menage_echelle %>% select(inter)




# Export ------------------------------------------------------------------

menage_echelle_TC_DPE<-menage_echelle_34
save(menage_echelle_TC_DPE, file=MatisseFiles$menage_echelle_TC_DPE_rd)


# Clean -------------------------------------------------------------------
suppressWarnings(rm(dummies_classe_men,FC,menage_echelle,menage_echelle_33,menage_echelle_34,menage_echelle_TC_DPE,menage_ener_dom,prix_classe,
   prix_classe_horizon,prix_classe_mat,prix_menages_horizon,prix_menages_horizon_bis,sauv_avant_reventil,solde))
gc()


# Test --------------------------------------------------------------------
# 
# print(compute_share_export(menage_echelle_34))
# print(compute_savings_rate_export(menage_echelle_34))
# 
# 

# SUCCESS -----------------------------------------------------------------


# print("3_1_4 Bascule A : SUCCESS")



# VERIF -------------------------------------------------------------------


# table(abs(menage_echelle_TC_DPE$dep_Elec-menage_echelle_TC_DPE$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Gaz-menage_echelle_TC_DPE$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_GPL-menage_echelle_TC_DPE$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Fuel-menage_echelle_TC_DPE$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Solides-menage_echelle_TC_DPE$dep_Solides_verif)<10^(-10))
# 
