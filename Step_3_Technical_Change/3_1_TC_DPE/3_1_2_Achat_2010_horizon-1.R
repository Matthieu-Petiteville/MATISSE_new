# Constructions neuves entre 2010 et horizon :
  # Selection des ménages
  # Mise à jour budgets



# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(dplyr)

# DATA --------------------------------------------------------------------

# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE/")

load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_31.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
load(paste(M_data,"/Data/Data_interne/list_source_usage.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))
coeff_dep_ems<-read_csv(paste(M_data,"/IMACLIM/coeff_dep_ems.csv",sep=""))
load(paste(M_data,"/Data/Data_interne/coeff_ems_2010.RData",sep=""))

source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/Repayment.R",sep=""))
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))


# DONNEES MANUELLES -------------------------------------------------------

sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")
dep_sources_verif=paste("dep",sources,"verif",sep="_")
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




# DATA ThreeME ------------------------------------------------------------

# VOLUME CONSTRUCTION NEUF (en m2)

#Proposition d'optimisation (save 30s)
# timest <- Sys.time()
# l_indx <- intersect(which(ThreeME$year<horizon),which(ThreeME$year>=2010))
# NEWBUIL_H01_CA_2 <- ThreeME[l_indx,] %>% filter(Var=="NEWBUIL_H01_CA_2") %>% select(year,value)
# NEWBUIL_H01_CB_2 <- ThreeME[l_indx,] %>% filter(Var=="NEWBUIL_H01_CB_2") %>% select(year,value)
# NEWBUIL_H01_CC_2 <- ThreeME[l_indx,] %>% filter(Var=="NEWBUIL_H01_CC_2") %>% select(year,value)
# print(Sys.time() - timest)

NEWBUIL_H01_CA_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CA_2")%>%
  select(year,value)

NEWBUIL_H01_CB_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CB_2")%>%
  select(year,value)

NEWBUIL_H01_CC_2<-
  ThreeME %>% 
  filter(year<horizon & year >=2010) %>%
  filter(Var=="NEWBUIL_H01_CC_2")%>%
  select(year,value)



# Préparation données -----------------------------------------------------

menage_echelle<-menage_echelle_31


# Nouvelles variables : sont "exclus" les ménages qui ne peuvent bénéficier de la construction d'un logement neuf
  # Exclus 
      # les ménages c13711>10000 : les ménages qui achètent une maison à l'horizon ne peuvent pas avoir construit un logement peu de temps avant
      # Les ménages déjà en A
      # Les locataires (on garde stalog 1 et 2, propriétaires et propriétaires remboursant un emprunt)
#NEUF va indiquer les ménages sélectionnés pour rénover leur logement : 
# passer de DPE_pred à class_arr

menage_echelle<-
  menage_echelle %>%
  mutate(exclus=FALSE,NEUF=FALSE) %>% 
  mutate_when(year_neuf==horizon,list(exclus=TRUE), #on n'achète pas de logements neufs à moins de 25 ans d'écart
              c13711>10000,list(exclus=TRUE), 
              DPE_dep=="A",list(exclus=TRUE),
              stalog>2,list(exclus=TRUE)) %>%
  mutate_when(!year_neuf==horizon, list(classe_arr=DPE_dep))%>% 
  mutate(solde_ener=0)
  



# Classement ménages --------------------------------------------------------------
# Precision : utiliser mutate de dplyr et pas de plyr


# Un seul classement de 2010 à horizon-1
# décision => plus gros consommateur (resp petit) de 2035 (décision ex post de rénover pour optimiser les émissions macro)

menage_echelle<- menage_echelle %>% mutate_when(is.na(ener_dom),list(ener_dom=0))

menage_echelle<-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_opt =row_number(-ener_dom))

menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_pess =max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1)


menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>% 
  mutate_when(
    kWh_rank_med<=0,
    list(kWh_rank_med=-kWh_rank_med+1)) %>%
  ungroup()

menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc))

menage_echelle <-
  menage_echelle %>%
  dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1)


menage_echelle<-
  menage_echelle %>% 
  mutate(ener_dom_surf=ifelse(is.infinite(ener_dom_surf),0,ener_dom_surf))%>%
  dplyr::mutate(kWh_rank_opt_ener =row_number(-ener_dom_surf)) %>% 
  ungroup()

#Possible optimisation : passer en fonction le classement des ménages et le conditionner au type de scenario classement

# ems<-calc_ems(menage_echelle,FC)
menage_echelle$ems_tot_chauff_ecs<-calc_ems(menage_echelle,FC)

menage_echelle<-
  menage_echelle %>% 
  mutate(ems_tot_chauff_ecs_surf=ems_tot_chauff_ecs/surfhab_d)%>%
  mutate(ems_tot_chauff_ecs_surf=ifelse(is.infinite(ems_tot_chauff_ecs_surf),0,ems_tot_chauff_ecs_surf))%>%
  dplyr::mutate(kWh_rank_opt_co2 =row_number(-ems_tot_chauff_ecs_surf))
  
if(str_detect(scenario_classement,"Optimal_ener")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt_ener)
}
if(str_detect(scenario_classement,"Optimal_co2")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt_co2)
}


if(str_detect(scenario_classement,"Pessimiste")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_pess)
}
if(str_detect(scenario_classement,"Optimiste")){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt)
}
if(scenario_classement=="Median"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_med)
}
if(scenario_classement=="Rich"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_rich)
}
if(scenario_classement=="Poor"){
  menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_poor)
}


A<-menage_echelle%>%
  mutate(gap_rank_ener=abs(kWh_rank_opt-kWh_rank_opt_ener))%>%
  mutate(gap_rank_co2=kWh_rank_opt-kWh_rank_opt_co2)

A%>%
  summarise("gap_rank"=weighted.mean(gap_rank_ener,pondmen))
A%>%
  summarise("gap_rank"=weighted.mean(gap_rank_co2,pondmen,na.rm=T))


# SELECTION DES MENAGES ---------------------------------------------------

# les ménages exclus ne le sont pas du classement ex-ante, mais leur classement est ramené à 0. Ce qui a pour conséquence de laisser des trous dans le classement.
menage_echelle <- 
  menage_echelle %>%
  mutate_when(exclus,list(kWh_rank=0))



# ANNEE PAR ANNEE
# CADUC  #important pour traiter les surcoûts de construction année après année et ventiler les annualités entre intérêts et remboursement du principal

# ident_rehab=data.frame("Year"=c(),"list_ident"=c())
# A1<-menage_echelle
ident_r<-c()

# Pourquoi commencer en 2011 et pas en 2010 ? 
# Le parc DPE a été calé sur les volumes 2010, les nouvelles constructions de cette année là ont déjà été prises en compte. Ce ne sera en revanche pas le cas des 
# rénovations thermiques, nous avons corrigé la bdd des gros travaux de rénovations en Step_0.4
for (Y in 2011:(horizon-1)){
# for (Y in 2010:2023){
  # print(Y)
  ident_r<-c()
  
  # remise à 0 du principal de la dette année après année
  # menage_echelle <- menage_echelle %>% mutate(principal_dette=0)
  
  # Mat_gain_ener -----------------------------------------------------------

  # Extraction de la conso moyenne au m2 en kWH par classe DPE
  conso_moy_dep=data.frame("A"=0, "B"=0, "C"=0, "D"=0, "E"=0, "F"=0, "G"=0)
  for (i in LETTERS[1:7]){
    conso_moy_dep[i]<-
      as.numeric(
        ThreeME %>%
          filter(Var==
                   paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")
          ) %>%
          filter(year==Y) %>%
          select(value)
      )
  }

  
  #Fonction possible : déjà utilisée ailleurs (3.1.1)
  Mat_gain_ener<-data.frame("DPE_before"=sort(rep(LETTERS[1:7],7)),"DPE_after"=rep(LETTERS[1:7],7))
  Mat_gain_ener$value_after<-sapply(Mat_gain_ener$DPE_after,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value_before<-sapply(Mat_gain_ener$DPE_before,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value<-(Mat_gain_ener$value_after-Mat_gain_ener$value_before)/Mat_gain_ener$value_before
  Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after,value_before))

  # DONNEES THREEME ---------------------------------------------------------
  # travaux de rénovation énergétiques en volume par saut de classe (en M2)
  # Transition de dep vers arr
  
  # Dépenses en constructions neuves en valeur (M€ courants)
  # PNEWBUIL_H01_2_NEWBUIL_H01_2_Y <-
  #   as.numeric(
  #     ThreeME %>%
  #       filter(Var=="PNEWBUIL_H01_2*NEWBUIL_H01_2") %>%
  #       filter(year==Y) %>%
  #       select(value)
  #   )*10^6

  NEWBUIL_H01_2_Y<-
    as.numeric(
      ThreeME %>%
        filter(Var=="NEWBUIL_H01_2") %>%
        filter(year==Y) %>%
        select(value)
    )


  
  # BASCULE -----------------------------------------------------------------
  
  for (arr in LETTERS[1:3]){

    if(arr=="A"){stock_m2_trans=NEWBUIL_H01_CA_2 %>% filter(year==Y)%>%select(value)}
     
    if(arr=="B"){
        stock_m2_trans=NEWBUIL_H01_CB_2 %>% filter(year==Y)%>%select(value)
        menage_echelle <-
          menage_echelle %>%
          mutate_when(DPE_dep=="B",list(kWh_rank=0))
      }
    if(arr=="C"){
      stock_m2_trans=NEWBUIL_H01_CC_2 %>% filter(year==Y)%>%select(value)
      menage_echelle <-
        menage_echelle %>%
        mutate_when(DPE_dep=="B",list(kWh_rank=0),
                    DPE_dep=="C",list(kWh_rank=0))
    }
    
        sum=0

        i=1
        while(!i %in% menage_echelle$kWh_rank){i=i+1} # on cherche le prochain ménage classé et non exclus
        #Peut-être une opti ici : la logique while i est assez moche.
        #Possible utilisation de min(kWh_rank) pour accélérer, mais besoin de gérer les cas de kWh_rank=0 => Passage à NA ou Inf?

        while(sum<stock_m2_trans){
          sum =
            sum +
            as.numeric(menage_echelle %>% filter(kWh_rank==i) %>% summarise(sum(pondmen*surfhab_d)))

          # identifiant du ménage sélectionné
          im <- as.numeric(menage_echelle %>% filter(kWh_rank==i) %>% select(ident_men))
          ident_r <- c(ident_r,im)

          # Modification des variables NEUF et class_arr dans la base globale
          menage_echelle<- menage_echelle %>%
            mutate_when(ident_men==im,list(NEUF=TRUE,classe_arr=arr,year_neuf=Y,kWh_rank=0))

          i=i+1
          while(!i %in% menage_echelle$kWh_rank){i=i+1}
        }
  }
  
  
  
  
  
  # Vérif
  menage_echelle %>% filter(NEUF)%>%group_by(classe_arr)%>%summarise(sum(pondmen*surfhab_d))
  NEWBUIL_H01_CA_2 %>% filter(year==Y)%>%select(value)
  NEWBUIL_H01_CB_2 %>% filter(year==Y)%>%select(value)
  NEWBUIL_H01_CC_2 %>% filter(year==Y)%>%select(value)
  menage_echelle %>% filter(NEUF)%>%mutate(surfpond=pondmen*surfhab_d)%>%arrange(classe_arr,-ener_dom)%>%select(ident_men, classe_arr,surfpond,ener_dom) # permet de vérifier que le dernier ménage sélectionné permet de passer strictement au dessus des volumes de ThreeME
        
  

# Modification des budgets ------------------------------------------------

    for (dep in LETTERS[2:7]){
      for (arr in LETTERS[1:3]){
        

        rate_gain_ener<-as.numeric(
          Mat_gain_ener %>%
            filter(DPE_before==dep) %>%
            filter(DPE_after==arr) %>%
            select(value))

        #s'il existe (dim>0) un ménage sélectionné pour acheter du neuf de classe arr (classe_arr==arr) à l'année (year_neuf==7) partant de la classe dep (DPE_dep==dep)
        if(dim(menage_echelle %>% filter(year_neuf==Y & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0){
          menage_echelle <-
            menage_echelle %>%
            mutate_when(
              # Condition
              year_neuf==Y &
                DPE_dep==dep &
                classe_arr==arr,
              # Action
              list(
                #Energie
                Elec_ECS=Elec_ECS*(1+rate_gain_ener),
                Gaz_ECS=Gaz_ECS*(1+rate_gain_ener),
                GPL_ECS=GPL_ECS*(1+rate_gain_ener),
                Fuel_ECS=Fuel_ECS*(1+rate_gain_ener),
                Solides_ECS=Solides_ECS*(1+rate_gain_ener),
                Urbain_ECS=Urbain_ECS*(1+rate_gain_ener),
                Elec_chauff=Elec_chauff*(1+rate_gain_ener),
                Gaz_chauff=Gaz_chauff*(1+rate_gain_ener),
                GPL_chauff=GPL_chauff*(1+rate_gain_ener),
                Fuel_chauff=Fuel_chauff*(1+rate_gain_ener),
                Solides_chauff=Solides_chauff*(1+rate_gain_ener),
                Urbain_chauff=Urbain_chauff*(1+rate_gain_ener),
                Elec_clim=Elec_clim*(1+rate_gain_ener)
              ))
        }
        
        }
        }
  # print(compute_share_export(menage_echelle))
    }
  
  
rm(i,sum,stock_m2_trans)

# menage_echelle <- menage_echelle %>% select(-solde_int_prov,-solde_princ_prov)
  sauv_int<-menage_echelle
# menage_echelle<-sauv_int

    #Vérif OK
  # for (Y in seq(2011,horizon-1)){
  # print(Y)
  # print(menage_echelle %>% filter(year_neuf==Y)%>%group_by(classe_arr)%>%summarise(sum(pondmen*surfhab_d)))
  # print(as.numeric(NEWBUIL_H01_CA_2 %>% filter(year==Y)%>%select(value)))
  # print(as.numeric(NEWBUIL_H01_CB_2 %>% filter(year==Y)%>%select(value)))
  # print(as.numeric(NEWBUIL_H01_CC_2 %>% filter(year==Y)%>%select(value)))
  # menage_echelle %>% filter(NEUF)%>%mutate(surfpond=pondmen*surfhab_d)%>%arrange(classe_arr,-ener_dom)%>%select(ident_men, classe_arr,surfpond,ener_dom)
  # }
  # Au total
  menage_echelle %>% filter(year_neuf>2010 & year_neuf<2035)%>%group_by(classe_arr)%>%summarise(sum(pondmen*surfhab_d))
  NEWBUIL_H01_CA_2 %>% filter(year>2010)%>%summarise(sum(value))
  NEWBUIL_H01_CB_2 %>% filter(year>2010)%>%summarise(sum(value))
  NEWBUIL_H01_CC_2 %>% filter(year>2010)%>%summarise(sum(value))
  
  # SOLDE_ENER --------------------------------------------------------------
  
  # Mise à jour des totaux
  menage_echelle<-
    menage_echelle %>% 
    mutate(
      dep_Elec_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Elec"))),
      dep_Gaz_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Gaz"))),
      dep_GPL_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("GPL"))),
      dep_Fuel_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Fuel"))),
      dep_Urbain_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Urbain"))),
      dep_Solides_verif=rowSums(menage_echelle %>% select(list_source_usage) %>% select(starts_with("Solides")))
    )
  
  # Due à la fusion Sources et Dep_sources sont redondants, la mise à jour de Sources permet de déduire facilement le solde sur tous les sources d'énergie
  menage_echelle$solde_ener<-
    rowSums(menage_echelle[dep_sources_verif]) -
    rowSums(menage_echelle[dep_sources])
  
  
  # Test
  # A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men)
  # menage_echelle %>% filter(NEUF) %>% filter(!year_neuf==horizon) %>% filter(!ident_men %in% A$ident_men) %>% select(ident_men)
  # 5797
  # View(rbind(menage_echelle))
  
  menage_echelle<-
    menage_echelle %>% 
    mutate(
      dep_Elec=dep_Elec_verif,
      dep_Gaz=dep_Gaz_verif,
      dep_GPL=dep_GPL_verif,
      dep_Fuel=dep_Fuel_verif,
      dep_Solides=dep_Solides_verif,
      dep_Urbain=dep_Urbain_verif)
  
  # menage_echelle$dep_energie=rowSums(menage_echelle[dep_sources])
  # menage_echelle$dep_energie_logement=rowSums(menage_echelle[
  #   c("Elec_ECS","Gaz_ECS","GPL_ECS","Fuel_ECS","Solides_ECS","Urbain_ECS","Elec_chauff","Gaz_chauff",
  #     "GPL_chauff","Fuel_chauff","Solides_chauff","Urbain_chauff","Elec_clim")])
  
  
  # SOLDE_DETTE -------------------------------------------------------------
  solde<-menage_echelle %>% 
    mutate(solde=solde_ener) %>%
    select(ident_men,solde)
  
###
## Vérif
###
# A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men,solde_ener)
# A
# menage_echelle %>% filter(year_neuf<2035 & year_neuf>0) %>%filter(!ident_men %in% A$ident_men) %>% select(ident_men)
# # Verif : tous les ménages ayant un solde ener non nul sont les ménages neuf_horizon
# length(setdiff(A$ident_men,menage_echelle %>% filter(year_neuf<2035 & year_neuf>0)))
# dim(menage_echelle %>% filter(year_neuf<2035 & year_neuf>0)%>% select(ident_men))
# menage_echelle %>% filter(year_neuf<2035 & year_neuf>0) %>% filter(!ident_men %in% A$ident_men)%>%select(ident_men, year_neuf,solde_ener, dep_sources)


####
  
  
  # VENTILATION -------------------------------------------------------------
  
  # source("Technical_change/Econometrie_solde_budg_bouclage_autres.R")
  menage_echelle <- menage_echelle %>% mutate_when(year_neuf>0,list(NEUF=TRUE))
  menage_echelle_32<-Ventil_solde(solde,menage_echelle,step="REHAB")
  
  menage_ener_dom<-energie_dom_surf(menage_echelle_32)
  menage_echelle_32<- 
    menage_echelle_32 %>%
    select(-ener_dom_surf,-ener_dom) %>%
    left_join(menage_ener_dom,by="ident_men")
  
  
  
  
  
  
  #Test
  # Agriculture
  as.numeric(menage_echelle_32 %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle %>% summarise(sum(pondmen*agriculture)))-1
  #evolution des dépenses d'agriculture avant/après reventilation => devrait augmenter
  as.numeric(menage_echelle_32%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))-1
  #evolution des dépenses d'agriculture avant/après reventilation => devrait rester identique pour les ménages non concernés par la rénovation. 
  as.numeric(menage_echelle_32%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))-1
  
  # Dep_Elec
  as.numeric(menage_echelle_32 %>% summarise(sum(pondmen*dep_Elec)))/as.numeric(menage_echelle %>% summarise(sum(pondmen*dep_Elec)))-1
  #evolution des dépenses d'dep_Elec avant/après reventilation => devrait augmenter
  as.numeric(menage_echelle_32%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*dep_Elec)))/as.numeric(menage_echelle%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*dep_Elec)))-1
  #evolution des dépenses d'dep_Elec avant/après reventilation => devrait rester identique pour les ménages non concernés par la rénovation. 
  as.numeric(menage_echelle_32%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*dep_Elec)))/as.numeric(menage_echelle%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*dep_Elec)))-1
  
  
  # A2<-menage_echelle %>% select(-kWh_rank_pess,-kWh_rank_opt,-kWh_rank,-solde_dette,-solde_ener)
  
  
  
  # VERS LA PROCHAINE ETAPE -------------------------------------------------
  
  # ident_rehab=cbind(ident_rehab,c(Y,menage_echelle%>%filter(REHAB)%>%select(ident_men)))





# SAVE --------------------------------------------------------------------

# menage_echelle_32<-menage_echelle
  # %>% mutate(DPE_2024=DPE_dep) %>% select(-stalog,-propri,-REHAB,-DPE_dep,-classe_arr ,-kWh_rank_pess,-kWh_rank_opt,-kWh_rank,-REHAB,-classe_arr)
# load("Technical_change/TC_renovation_DPE/menage_echelle_32.RData")

  
  

# Parts Budgétaires -------------------------------------------------------

# print(compute_share_export(menage_echelle_32))
# print(compute_savings_rate_export(menage_echelle_32))  



# Maj_dep_preeng ----------------------------------------------------------

# menage_echelle_32 <- maj_dep_preeng(bdd1= menage_echelle_31,bdd2=menage_echelle_32)





# SAVE --------------------------------------------------------------------


# load(paste(scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_31.RData",scep=""))

#
# inter<-intersect(colnames(menage_echelle_32), colnames(menage_echelle_31))
# not_inter<-setdiff(colnames(menage_echelle_32), colnames(menage_echelle_31))

# menage_echelle_32<-menage_echelle_32 %>% select(inter)



save(menage_echelle_32, file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_32.RData",sep=""))

# 
# 
# # Suppression des bases ---------------------------------------------------
# 
# rm(
#   tot_Constr_neuf_10_24,
#   Constr_neuf_10_24,
#   sum, 
#   i,
#   scenario,
#   dep_sources,
#   len,
#   A,
#   A1,A3,A4,
#   ThreeME,
#   c13_2025,
#   dep_ener_2025,
#   ident_accedants,
#   im,
#   menage_echelle_prop,
#   menage_echelle,
#   solde,
#   rate_gain_ener,
#   list_source_usage,
#   j,arr,dep,
#   Mat_gain_ener_2025
# )
# 
# 
# 
# # VERIF prix au M2 --------------------------------------------------------
# 
# # CCL : impossible de vérifier que les prix de construction au m2 
# # sont cohérents avec les données de THREEME. 
# # La variable prixrp de DEPMEN est trop parcellaire.
# 
# 

# Clean -------------------------------------------------------------------
suppressWarnings(rm(coeff_dep_ems,coeff_ems_2010,conso_moy_dep,DPE_A,DPE_B,menage_echelle,menage_echelle_32,
                  menage_echelle_31,ThreeME,FC,A,Mat_gain_ener,menage_ener_dom,sauv_int,solde))
gc()
  

