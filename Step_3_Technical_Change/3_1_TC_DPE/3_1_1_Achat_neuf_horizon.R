# OBJECTIF : Les ménages achetant un logement en 2010 le sont également à l'horizon, on sélectionne parmi ceux là les ménages achetant un logement neuf.




# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
source(paste(M_home,"/Common/tools.R",sep=""))

source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/Repayment.R",sep=""))

coeff_dep_ems<-read_csv(paste(M_data,"/IMACLIM/coeff_dep_ems.csv",sep=""))
load(paste(M_data,"/Data/Data_interne/coeff_ems_2010.RData",sep=""))
TCO<-as.numeric(read_excel(path=paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep=""),range="C103",col_names=F))*10^6
# TCO ne dépend pas de la rétrocession, c'est du calibrage

source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/calc_ems.R",sep=""))


# DATA --------------------------------------------------------------------

# Donnes ThreeME : m2 et valeurs d'achats de logement neufs trajectoire
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))
load(paste(M_data,"/Data/Data_interne/list_source_usage.RData",sep=""))

#horizon
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_2.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))


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



sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
dep_sources=paste("dep",sources,sep="_")
dep_sources_verif=paste("dep",sources,"verif",sep="_")

# PREPARATION DATA --------------------------------------------------------
menage_echelle_1 <- menage_echelle
menage_echelle <- menage_echelle %>% mutate(DPE_dep=DPE_pred)


# DONNEES THREE_ME  -------------------------------------------------

# Constructions neuves en m2 : NEWBUIL_H01_2
NEWBUIL_H01_2_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )

NEWBUIL_H01_2_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_2") %>%
      filter(year==2010) %>%
      select(value)
  )

# Constructions neuves en m2 par classe énergétique : A	
NEWBUIL_H01_CA_2<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_CA_2") %>%
      filter(year==horizon) %>%
      select(value)
  )


# Constructions neuves en m2 par classe énergétique : B	
NEWBUIL_H01_CB_2<-
  as.numeric(
    ThreeME %>%
      filter(Var=="NEWBUIL_H01_CB_2") %>%
      filter(year==horizon) %>%
      select(value)
  )


# Données de gains énergétiques --------------------------------------------

# Consommation énergétique des logements de classe CA en KWh/m2 #ENER_BUIL_H01_CA_2*11630/BUIL_H01_CA_2
conso_moy_dep=data.frame("A"=0, "B"=0, "C"=0, "D"=0, "E"=0, "F"=0, "G"=0)
for (i in LETTERS[1:7]){
  conso_moy_dep[i]<-
    as.numeric(
      ThreeME %>% 
        filter(Var==
                 paste("ENER_BUIL_H01_C",i,"_2*11630/BUIL_H01_C",i,"_2",sep="")
        ) %>% 
        filter(year==horizon) %>% 
        select(value)
    )
}


# Matrice des gains énergétique des passages de DPE Mat_gain_ener
Mat_gain_ener<-data.frame("DPE_before"=sort(rep(LETTERS[1:7],7)),"DPE_after"=rep(LETTERS[1:7],7))

Mat_gain_ener$value_after<-sapply(Mat_gain_ener$DPE_after,function(x) as.numeric(conso_moy_dep[x]))
Mat_gain_ener$value_before<-sapply(Mat_gain_ener$DPE_before,function(x) as.numeric(conso_moy_dep[x]))
Mat_gain_ener$value<-(Mat_gain_ener$value_after-Mat_gain_ener$value_before)/Mat_gain_ener$value_before
Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after,value_before))







# Classement --------------------------------------------------------------

menage_echelle<- menage_echelle %>% mutate_when(is.na(ener_dom),list(ener_dom=0))


# on classe les ménages pour chaque DPE par ordre décroissant de consommation énergétique pour le logement en MWh (absolu => maximisation du gain agrégé)
menage_echelle<-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_opt = row_number(-ener_dom)) %>% 
  ungroup()

# Pessimiste
# on classe les ménages par ordre croissant de conso éner pour le logement, les premiers seront les derniers
menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>% 
  ungroup()


menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_med = kWh_rank_pess-kWh_rank_opt) %>% 
  mutate_when(
    kWh_rank_med<=0,
    list(kWh_rank_med=-kWh_rank_med+1)) %>%
  ungroup()


menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_rich=row_number(-RDB/coeffuc)) %>% 
  ungroup()

menage_echelle <-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_poor=max(kWh_rank_rich)-kWh_rank_rich+1) %>% 
  ungroup()




menage_echelle<-
  menage_echelle %>% 
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_opt_ener =row_number(-ener_dom_surf)) %>% 
  ungroup()

menage_echelle$ems_tot_chauff_ecs<-calc_ems(menage_echelle,FC)

menage_echelle<-
  menage_echelle %>% 
  mutate(ems_tot_chauff_ecs_surf=ems_tot_chauff_ecs/surfhab_d)%>%
  group_by(DPE_dep) %>% 
  dplyr::mutate(kWh_rank_opt_co2 =row_number(-ems_tot_chauff_ecs_surf)) %>% 
  ungroup()

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




# SELECTION MENAGES ACCEDANT horizon Not VAN -----------------------------------


# Les dépenses d'achat de logement (c13711) ne sont pas à l'échelle dans c05_forme_2010, mais on récupère uniquement les identifiants ménages

ident_accedants <- 
  menage_echelle %>% 
  filter(c13711>10000) %>%
  filter(ener_dom_surf>0) %>% #personne ne fait de travaux si pas de conso d'énergie  
  select(ident_men, pondmen,surfhab_d,DPE_dep,kWh_rank,c13711,ener_dom)%>% 
  filter(!DPE_dep=="A") %>% # les ménages déjà en A exclus, sinon on se retrouve avec dep=A, arr=B
  # left_join(depmen %>% select(ident_men, anacq, ancons),by="ident_men") %>%
  # filter(ancons<=9)%>% #on exclut les ancons==99
  mutate(year_neuf=0)%>%
  mutate(classe_arr=DPE_dep)%>%
  dplyr::arrange(.,kWh_rank)
  # A VERIFIER JUSTIFICATION

#Justification : on adopte un système similaire aux autres étapes : sélection uniquement sur le classemenet énergétique, le système s'appuyant sur ancons et anacq
# est plus complexe à justifier : est-ce qu'on considère plus probable qu'un ménage achetant un logement neuf et efficace à l'horizon soit un ménage qui ait acheté 
# un logement ancien dans le passé ou justement de privilégier des ménages déjà efficaces dans des logements les plus récents possibles ? 
# les deux stratégies se justifient, pour éviter de choisir on adopte la stratégie du "rateau" déjà employée pour les rénovations et les achats avant l'horizon. 
## old version :
  # dplyr::arrange(.,-neuf_horizon,-ancons,DPE_dep,-anacq,kWh_rank) #on classe sur l'année de construction (du plus récent au plus vieux, de la meilleure classe DPE à la pire,de la plus récente année d'acquisition à la plus ancienne, sur la consommation d'énergie surfacique en fonction du scenario de classement)
# 



# SELECTION DES MENAGES ---------------------------------------------------

count=0


## DPE A
#
DPE_A=0
while(DPE_A<NEWBUIL_H01_CA_2){
  count=count+1
  # print(ident_accedants[count,]$ident_men)
  DPE_A <- ident_accedants[1:count,] %>% summarise(sum(pondmen*surfhab_d))
  ident_accedants[count,]$classe_arr<-"A"
  ident_accedants[count,]$year_neuf<-horizon
}
count_A=count

#
## DPE B
#
DPE_B=0
if(scenario_classement=="VAN"){
  ident_accedants_bis <-
    ident_accedants_bis %>%
    left_join(menage_echelle%>%select(ident_men,kWh_rank_van_B),by="ident_men")%>%
    mutate(kWh_rank=kWh_rank_van_B)%>%
    select(-kWh_rank_van_B)%>%
    dplyr::arrange(.,-year_jorizon,-ancons,DPE_dep,-anacq,kWh_rank)
# en reclassant sur neuf_horizon, on assure que les ménages déjà convertis parce que rentables en A le sont toujours, et qu'on repart dans les ménages non
# convertis classés par VAN_B
    }



# 15/02/2021, attention, version bugué, qui compte dans le total des constructions B, les résidences déjà en B
# while(DPE_B<NEWBUIL_H01_CB_2){
#   count=count+1
#   while(ident_accedants[count,]$DPE_dep=="B"){count=count+1} #on veut éviter de sélectionner une transition B->B
#   # print(ident_accedants[count,]$ident_men)
#   DPE_B <- ident_accedants[(count_A+1):count,] %>% summarise(sum(pondmen*surfhab_d))
#   ident_accedants[count,]$classe_arr<-"B"
#   ident_accedants[count,]$year_neuf<-horizon
#   
# }

# nouvelle version
DPE_B<-0
while(DPE_B<NEWBUIL_H01_CB_2){
  count=count+1
  # print(count)
  while(ident_accedants[count,]$DPE_dep=="B"){count=count+1} #on veut éviter de sélectionner une transition B->B
  # print(ident_accedants[count,]$ident_men)
  DPE_B <- DPE_B+ ident_accedants[count,] %>% summarise(sum(pondmen*surfhab_d))
  ident_accedants[count,]$classe_arr<-"B"
  ident_accedants[count,]$year_neuf<-horizon
  
}

ident_accedants <- 
  ident_accedants %>% 
  select(ident_men,classe_arr,year_neuf)
  

menage_echelle <- 
  menage_echelle %>%
  left_join(ident_accedants, by="ident_men") %>%
  mutate_when(is.na(year_neuf),list(year_neuf=0),
              is.na(classe_arr),list(classe_arr=DPE_dep)) %>%
  mutate(solde_ener=0)

  # mutate(solde_dette=0,solde_ener=0,solde_int=0,solde_princ=0,principal_dette=0,solde_int_total=0,solde_princ_total=0)
##
# TEST
# menage_echelle %>% filter(neuf_horizon)%>%group_by(classe_arr)%>%summarise(sum(pondmen*surfhab_d))

# NOUVELLES CLASSES DPE ---------------------------------------------------

# table(menage_echelle$DPE_dep)
# table(menage_echelle$classe_arr)
# 
# 


# ALTERATION CONSO ENERGIE ------------------------------------------------

for (dep in LETTERS[2:7]){
  # classe de départ
  for (arr in LETTERS[1:2]){ #NB :à partir de 2020 on ne construit que des classes A et B (arrêt des classes C en 2019)
    # classe arrivée
    if(dep>arr){
      # print(paste('dep is ',dep," , arr is ",arr,sep=""))

      # Coefficient de gain énergétique (multiplié par 1/2 pour centrer les consommations des constructions de fin et de début d'année)
      rate_gain_ener<-as.numeric(
        Mat_gain_ener %>% 
          filter(DPE_before==dep) %>% 
          filter(DPE_after==arr) %>% 
          select(value))*1/2 # (coeff 0.5 car en moyenne des gains sur 6 mois 
      # (rénovation en début ou fin d'année))
      
      #s'il existe un ménage dans cette situation dep->arr à l'horizon
      if(dim(menage_echelle %>% filter(year_neuf==horizon & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0) 
        {
      menage_echelle <- 
        menage_echelle %>% 
        mutate_when(
          # Condition
          year_neuf==horizon &
            DPE_dep==dep & 
            classe_arr==arr,
          # Action
          list(
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
}




# Maj dépenses énergie ----------------------------------------------------


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


##
# Vérif
##
# A<-menage_echelle %>% filter(abs(solde_ener)>10^(-9))%>% select(ident_men,solde_ener)
# A
# menage_echelle %>% filter(year_neuf==horizon) %>%filter(!ident_men %in% A$ident_men) %>% select(ident_men)
# # Verif : tous les ménages ayant un solde ener non nul sont les ménages neuf_horizon
# length(setdiff(A$ident_men,ident_accedants %>% filter(year_neuf==horizon)))
# dim(menage_echelle %>% filter(year_neuf==horizon)%>% select(ident_men))
####

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


# # Test => #Dep_elec et Elec sont cohérents
# table((menage_echelle$dep_Elec-menage_echelle$dep_Elec_verif)<10^(-9))
# table((menage_echelle$dep_Gaz-menage_echelle$dep_Gaz_verif)<10^(-9))
# table((menage_echelle$dep_Fuel-menage_echelle$dep_Fuel_verif)<10^(-9))
# table((menage_echelle$dep_GPL-menage_echelle$dep_GPL_verif)<10^(-9))
# table((menage_echelle$dep_Urbain-menage_echelle$dep_Urbain_verif)<10^(-9))
# table((menage_echelle$dep_Solides-menage_echelle$dep_Solides_verif)<10^(-9))


# Reventilation -------------------------------------------------------------

solde<-menage_echelle %>% 
  mutate(solde=ifelse(abs(solde_ener)<10^(-9),0,solde_ener))%>%
  select(ident_men,solde)

#verif
# dim(solde %>% filter(solde<0))
# dim(menage_echelle %>% filter(year_neuf==horizon)%>% select(ident_men))


sauv<-menage_echelle

menage_echelle_31<-Ventil_solde(solde,menage_echelle,step="REHAB")


## 
# Vérification
##
# menage_echelle %>% summarise(sum(pondmen*solde_ener))
# -199 378 805. (AMS 2035 Opt)

# Agriculture
as.numeric(menage_echelle_31 %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle %>% summarise(sum(pondmen*agriculture)))-1
#evolution des dépenses d'agriculture avant/après reventilation => devrait augmenter
as.numeric(menage_echelle_31%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle%>%filter(year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))-1
#evolution des dépenses d'agriculture avant/après reventilation => devrait rester identique pour les ménages non concernés par la rénovation. 
as.numeric(menage_echelle_31%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))/as.numeric(menage_echelle%>%filter(!year_neuf==horizon) %>% summarise(sum(pondmen*agriculture)))-1



menage_ener_dom<-energie_dom_surf(menage_echelle)

menage_echelle_31<- 
  menage_echelle %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(menage_ener_dom,by="ident_men")





# Test
table(abs(menage_echelle_31$dep_Elec_verif-menage_echelle_31$dep_Elec)<10^(-9))
table((menage_echelle_31$dep_Gaz-menage_echelle_31$dep_Gaz_verif)<10^(-9))
table((menage_echelle_31$dep_Fuel-menage_echelle_31$dep_Fuel_verif)<10^(-9))
table((menage_echelle_31$dep_GPL-menage_echelle_31$dep_GPL_verifrif)<10^(-9))
table((menage_echelle_31$dep_Urbain-menage_echelle_31$dep_Urbain_verif)<10^(-9))
table((menage_echelle_31$dep_Solides-menage_echelle_31$dep_Solides_verif)<10^(-9))
# Les problèmes c'est les valeurs négatives.

# View(menage_echelle_31%>%filter((Elec-dep_Elec)>10^(-9))%>% select(ident_men,contains("Elec")))

# View(rbind(menage_echelle %>% filter(ident_men==12268),menage_echelle_31 %>% filter(ident_men==12268)%>% select(colnames(menage_echelle))))




# SAVE --------------------------------------------------------------------


save(menage_echelle_31, file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_31.RData",sep=""))




#TEST share
compute_share_export(menage_echelle_31)
compute_savings_rate_export(menage_echelle_31)

# pour comparaison
compute_share_export(menage_echelle_1)
compute_savings_rate_export(menage_echelle_1)





# SUCCESS -----------------------------------------------------------------

# print("3_1_1_Achat_horizon : SUCCESS")

