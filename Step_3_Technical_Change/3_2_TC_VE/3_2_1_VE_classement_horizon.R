


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(plyr)
# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE")
source(paste(M_home,"/Common/tools.R",sep=""))


# DATA --------------------------------------------------------------------

# code permettant de créer dans le dossier D:\Stage_Petiteville\Projet_Ademe\VE\Codes => to be verified
# Base appariée BDF-ENTD
load(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/appariement_bdf_entd.RData",sep=""))


# Bases étapes précédentes (Step 3.1)
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_DPE.RData",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))


## BDF
auto<-as.data.frame(read_excel(paste(M_data,"/Data/BDF_2010/AUTOMOBILE.xlsx",sep="")),stringsAsFactors=F)



# PREPARATION DATA --------------------------------------------------------

# Menage_echelle
menage_echelle<-menage_echelle_TC_DPE

# AUTO pour VE
# Identification des voitures électriques en 2010 sur lesquels on construira le stock 2010 de ThreeME
auto_elec<- 
  auto %>% 
  select(ident_men,carbu)%>%
  filter(carbu==4) %>% # carbu 4 = electrique
  filter(ident_men %in% menage_echelle$ident_men) %>%
  left_join(menage_echelle %>% select(ident_men, pondmen),by="ident_men")







# test de sensibilité ? 
# Tester avec 100% ? de kilomètres éligibles ? 
# si pas de différence 80/100% => on peut faire sauter le lien bdf-entd


#justifier la barrière de kilomètre éligible













# APPARIEMENT BDD ------------------------------------------------


# combien de ménages dans ENTD ???


appariement_bdf_entd <- 
  appariement_bdf_entd %>%
  dplyr::rename(percent_pkm_eligible=percent_W_mean_eligible) 



# Cellulage des ménages sur trois variables : tuu, quintile UC et si le nombre le permet typmen
# Lors de l'appariement, si dans les cellule tuu X Quintile x Typmen, la moyenne du nombre d'élements est inférieure à 100 pour les 5 valeurs de 
# typmen alors on considère qu'il n'est pas pertinent de séparer les typmen et on a calculé le pourcentage de km éligible pour tous les typmen dans 
# la cellule tuu X Quintile.
# On a ainsi formé 85 cellules au lieu des 225 possibles (5 quintile * 9 tuu * 5 typmen5)  


# Pour chaque cellule de menage_echelle on attribue la valeur de percent_pkm_eligible de la cellule équivalente dans ENTD. 
for (i in 1:nrow(appariement_bdf_entd)){ #on parcourt toutes les cellules
  Classe<-appariement_bdf_entd[i,]
  if(is.na(Classe$typmen5)) { #si Classe$typmen5 est NA, cela veut dire que la cellule est définie par tuu X quintile
    menage_echelle <-
      menage_echelle %>%
      mutate_when(quintileuc==Classe$niveau_vie_quintile &
                    tuu==Classe$tuu,
                  list(percent_pkm_eligible=Classe$percent_pkm_eligible))
    
    
  } else {
    menage_echelle <-
      menage_echelle %>%
      mutate_when(quintileuc==Classe$niveau_vie_quintile &
                    tuu==Classe$tuu &
                    typmen5==Classe$typmen5,
                  list(percent_pkm_eligible=Classe$percent_pkm_eligible))
    
  }
  i=i+1
}


rm(i,Classe,appariement_bdf_entd)









# Le potentiel de transition au VE est calculé en absolu du point de vue aggrégé 
# comme la potentielle diminution de la consommation de carburant (directement 
# proportionnelle aux émissions) des trajets éligibles aux VE du ménage
menage_echelle <- 
  menage_echelle %>%
  mutate_when(!is.na(nbvehic)& !is.na(percent_pkm_eligible) & !is.na(carb_lubr) & carb_lubr>0 & nbvehic>0,
              list(
                potentiel_VE=carb_lubr * percent_pkm_eligible)) 





# CLASSEMENT --------------------------------------------------------------

# Classement des ménages possédant une voiture ou plus et des dépenses en carburant strictement positives.
# on n'exclut pas les ménages achetant une auto en 2010 (prod_veh>0) pour la sélection 2010-(n-1). En revanche, nouveau classement à l'horizon n uniquement sur les ménages avec prod_veh>0 (exclus ceux qui ont déjà un VE pour respecter les trajectoires ADEME)

menage_echelle <- 
  menage_echelle %>%
  mutate_when(!is.na(nbvehic)& !is.na(carb_lubr) & carb_lubr>0 & nbvehic>0,
              list(VE_rank_opt =row_number(-potentiel_VE)))

menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(VE_rank_pess =max(menage_echelle %>% filter(!is.na(VE_rank_opt)) %>% select(VE_rank_opt))-VE_rank_opt+1)


menage_echelle <-
  menage_echelle %>% 
  dplyr::mutate(VE_rank_med =VE_rank_pess-VE_rank_opt) %>% 
  mutate_when(
    !is.na(VE_rank_med) & VE_rank_med<=0,
    list(VE_rank_med=-VE_rank_med+1))


menage_echelle <-
  menage_echelle %>% 
  mutate_when(!is.na(VE_rank_opt),list(VE_rank_rich=row_number(-RDB/coeffuc))) %>% 
  mutate_when(!is.na(VE_rank_opt),list(VE_rank_poor=max(VE_rank_rich)-VE_rank_rich+1))




# View(menage_echelle%>% select(ident_men, nbvehic,carb_lubr,percent_pkm_eligible,potentiel_VE,VE_rank_pess,VE_rank_opt,VE_rank_med,VE_rank_rich,VE_rank_poor)%>%arrange(potentiel_VE))
# View(menage_echelle %>% select(VE_rank_opt,VE_rank_med,VE_rank_pess,VE_rank_rich,VE_rank_poor)%>% arrange(VE_rank_opt))




# MCREVOI_D ----------------------------------------------------------------

#Pour tous les ménages classés, on indique le montant actuel du remboursement de l'emprunt au cours des 12 derniers mois pour le dernier véhicule acheté par le ménage
#pour chaque ménage on sélectionne le véhicule le plus récent, on indique si oui ou non le ménage a un remboursement en cours
# RECVOI Année d'achat du véhicule
# MCREVOI_D Montant définitif remboursé du crédit pour le véhicule au cours des 12 derniers mois
# KM_AUTO Km parcourus pendant la semaine de tenue des carnets

# Renvoie l'identifiant, l'année d'achat et le montant du crédit remboursé sur l'année pour le véhicule le plus récent de chaque ménage
max_recvoi<-plyr::ddply(auto %>% select(ident_men,recvoi,mcrevoi_d), .(ident_men), function(x) x[which.max(x$recvoi),])

# Kilométrage effectué par le ménage tous véhicules confondus : km_auto est une donnée hebdomadaire
# on corrige les données abberante de 99998 ou 99999 km en une semaine pour une véhicule
auto <- 
  auto %>%
  mutate(km_auto=ifelse(km_auto>=99998,NA,km_auto))

km_auto_sum<-plyr::ddply(auto %>% select(ident_men,km_auto), .(ident_men), function(x) sum(x$km_auto)*52)
colnames(km_auto_sum)<-c("ident_men","km_auto")


menage_echelle <- 
  menage_echelle %>% 
  left_join(max_recvoi %>% select(ident_men,mcrevoi_d),by="ident_men") %>% 
  mutate_when(is.na(mcrevoi_d),list(mcrevoi_d=0))%>%
  left_join(km_auto_sum,by="ident_men")





# KM_AUTO -----------------------------------------------------------------
# Somme de tous les Km-véhicules parcourus par classe de performance et motorisation (e.g. KM_AUTO_H01_CE_22_2 (Classe F, fioul)
KM_AUTO_TOT<-as.numeric(
  ThreeME %>%
    filter(year==2010) %>%
    filter(str_detect(Var, 'KM_AUTO_H01_C'))%>%
    summarise(sum(value))
)*1000

# Stock de véhicules particuliers en milliers
AUTO_H01_2<-as.numeric(
  (ThreeME %>% 
     filter(Var=="AUTO_H01_2") %>%
     filter(year==2010) %>%
     select(value))[1,])*1000

#vkm moyen
KM_AUTO_2010<-KM_AUTO_TOT/AUTO_H01_2

# Cohérence avec menage_echelle ? Oui 3.7% d'écart
menage_echelle %>% filter(!is.na(nbvehic) & nbvehic>0)%>%summarise("avg_vkm"=weighted.mean(km_auto/nbvehic,pondmen,na.rm=T))
# avg_vkm
# 1 12497.89 
#### vs ####
# KM_AUTO_2010
# [1] 12984.12


menage_echelle <- 
  menage_echelle %>%
  mutate_when(is.na(km_auto),list(km_auto=KM_AUTO_2010*nbvehic)) 
# nouvelle moyenne de vkm : 12813.62



#Hypothèse : on choisit de négliger l'évolution du km moyen par véhicule au cours de la projection. En effet le taux de croissance des vkm moyens est de 0.78% en 2035,  0.29% en 2030, 0.074% en 2025. 
# KM_AUTO

##### VERIFICATION

# KM_AUTO_TOT<-as.numeric(
#   ThreeME %>%
#     filter(year==horizon) %>%
#     filter(str_detect(Var, 'KM_AUTO_H01_C'))%>%
#     summarise(sum(value))
# )*1000
# 
# 
# AUTO_H01_2<-as.numeric(
#   (ThreeME %>% 
#      filter(Var=="AUTO_H01_2") %>%
#      filter(year==horizon) %>%
#      select(value))[1,])*1000
# 
# #vkm moyen (quelque soit motorisation)
# KM_AUTO<-KM_AUTO_TOT/AUTO_H01_2
# 
# ratio_TC_KM<-KM_AUTO/KM_AUTO_2010
# ratio_TC_KM



# BONUS-MALUS -------------------------------------------------------------
# Dans les dépenses de veh neuf en 2010 est inclus un bonus-malus (BM),
# Celui-ci passe par la micro-simulation car il fait partie de la décision d'achat
# Nous traitons le BM (après micro-simulation) de 2010 comme un pourcentage du prix de vente, sur prix moyen du veh dans 3ME
# par cohérence, nous rajoutons également le BM des achats à l'horizon comme un pourcentage du prix de vente sur prix moyen du veh dans 3ME
# Justiciation (2020/07/26) Campons sur le traitement du Bonus-Malus (BM) en relatif dans le prix de vente
# -> parce qu'on n'a pas d'autre solution en 2010 (pour enlever le bonus de 2010 et rajouter celui de l'horizon)
# -> à l'horizon on traite le BM aussi en relatif pour rendre cohérent avec 2010 => on considère le prix comme le meilleur proxy pour les émissions (voiture chère => voiture polluante => gros malus)

####
#2010
####
BM_net_2010 <- 0 #Bonus-malus net pour les Véhicules thermiques
TOT_VTH_nv_2010<-0
for (x in LETTERS[1:7]){
  # Volumes de ventes de veh thermiques de classe X
  Vol<-as.numeric(ThreeME %>% 
                    filter(year==2010)%>%
                    filter(Var==paste("NEWAUTO_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  TOT_VTH_nv_2010<-TOT_VTH_nv_2010+Vol
  # Voluem de Bonus-Malus pour les véhicules th de classe X
  BM<- as.numeric(ThreeME %>% 
                    filter(year==2010)%>%
                    filter(Var==paste("BONUS_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  BM_net_2010<-BM_net_2010+Vol*BM
  
}
BM_avg_2010<-BM_net_2010/TOT_VTH_nv_2010
# Malus de 175euros en moyenne par véhicule thermique vendu en 2010


TOT_VTH_euros_2010 <-as.numeric( # en Millions €
  ThreeME %>% 
    filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
    filter(year==2010) %>%
    select(value)
) * 10^6


P_VTH_2010 <- TOT_VTH_euros_2010/TOT_VTH_nv_2010
# prix moyen d'achat d'un VT en 2010 : 22976.2 euros
#Bonus-malus relatif au prix d'un VT
BM_rel_2010<-BM_avg_2010/P_VTH_2010 #Malus égal à 0.7% du prix d'achat



####
#Horizon
###

BM_net_horizon <- 0 #Bonus-malus net pour les Véhicules thermiques
TOT_VTH_nv_horizon<-0
for (x in LETTERS[1:7]){
  Vol<-as.numeric(ThreeME %>% 
                    filter(year==horizon)%>%
                    filter(Var==paste("NEWAUTO_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  TOT_VTH_nv_horizon<-TOT_VTH_nv_horizon+Vol
  BM<- as.numeric(ThreeME %>% 
                    filter(year==horizon)%>%
                    filter(Var==paste("BONUS_TH_H01_C",x,"_2",sep=""))%>%
                    select(value))*1000
  BM_net_horizon<-BM_net_horizon+Vol*BM
  
}
BM_avg<-BM_net_horizon/TOT_VTH_nv_horizon
# 5500 euros de malus par veh th en 2035 AMS

TOT_VTH_euros <-as.numeric( # en Millions €
  ThreeME %>% 
    filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
    filter(year==horizon) %>%
    select(value)
) * 10^6

#Prix moyen d'un VTH à l'horizon
P_VTH <- TOT_VTH_euros/TOT_VTH_nv_horizon

#Bonus-malus relatif au prix d'un VT
BM_rel_th_horizon<-BM_avg/P_VTH
# BM_rel : 13.6% en 2035 (AMS)


# La partie du bonus-malus net de 2010 a été mise à l'échelle via la croissance des revenus et 
#l'évolution des prix, mais elle représente toujours BM_rel_2010 du prix payé par les consommateurs,
# soit 0.76%. Le pourcentage duquel on augmente le prix payé par les VT est donc de BM_rel-0.76%
# BM_rel_net<-BM_rel - BM_rel_2010

Bonus_VE_horizon<-as.numeric(ThreeME %>% 
                               filter(year==horizon)%>%
                               filter(Var=="BONUS_ELEC_H01_2")%>%
                               select(value))*1000



# est ce qu'on enregistre l'écho du bonus-malus pour les achats avant l'horizon ?
# question de cohérence : serait appréciable => trop compliqué d'aller modifier les remboursement des ménages avant l'horizon, faudrait le faire pour les VE et VTH



# SUCCESS -----------------------------------------------------------------

print("3_2_1_VE_classement_horizon : SUCCESS")


