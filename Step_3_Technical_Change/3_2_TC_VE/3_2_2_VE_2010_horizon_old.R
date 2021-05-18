
# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(FinancialMath)
library(readxl)

source(paste(M_home,"/Step_3_Technical_Change/Repayment.R",sep=""))
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/3_2_1_VE_classement_horizon.R",sep=""))

# DATA --------------------------------------------------------------------

load(MatisseFiles$source_usage_rd)
load(MatisseFiles$FC_2010_horizon_rd)

# Paramètre de ventilation du volume de vente des Véhicules particuliers aux privés ou entreprises (flotte)
# Issu des données fournies par l'Ademe (source autoactu.com), régression par Franck Nadaud sur données 2005-2019
# pour estimer l'évolution jusqu'en 2035, stabilisation de la part d'achats des VP par des particuliers 
# (dire d'experts ADEME, cf Stéphane BARBUSSE)

ventes_VP <- read_excel(path=MatisseFiles$ventes_vp_xl)


sources=c("Elec","Gaz","Fuel","GPL","Urbain","Solides")
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



# CLASSEMENT --------------------------------------------------------------
if(str_detect(scenario_classement,"Optimal_ener")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_opt)
}
if(str_detect(scenario_classement,"Optimal_co2")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_opt)
}


if(str_detect(scenario_classement,"Pessimiste")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_pess)
}
if(str_detect(scenario_classement,"Optimiste")){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_opt)
}
if(scenario_classement=="Median"){
  
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_med)
}
if(scenario_classement=="Rich"){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_rich)
}
if(scenario_classement=="Poor"){
  menage_echelle <- menage_echelle %>% mutate(VE_rank=VE_rank_poor)
}


# VE 2010 --------------------------------------------------------------------

# Les VE déjà achetés dans BDF sont comptés dans les achats de 2010
menage_echelle <- 
  menage_echelle %>%
  mutate(VE=FALSE) %>%
  mutate(new_VE=FALSE) %>%
  mutate(year_VE=0)%>%
  mutate(year_VE_sauv="")  %>%#garder en mémoire les achats successifs de VE
  mutate_when(
    ident_men %in% auto_elec$ident_men,
    list(VE=TRUE,year_VE=2010,VE_rank=0,year_VE_sauv="2010")
  )  %>%
  mutate_when(solv>0.328,list(VE_rank=0)) #si ménage pas solvable, exclus de l'achat de VE #solv màj fin étape 4_3 (ligne 740)
  # mutate(exclus=FALSE)%>%
  # mutate_when(solv>0.3,list(exclus=TRUE))

# => prendre une moyenne/au mieux : un maximum de la solv ex post

# TESTS -------------------------------------------------------------------

#Differents cas Entre 6 et 13 ans pour les ménages qu'on sélectionne pour le VE => on ne choisit que des ménages n'ayant pas de remboursement en cours sur leur véhicule le plus récent
# test=5850 #bascule en 2014 #seule incidence sur le budget, solde_elec et solde_carb, Rcons au même niveau (reventilation), même épargne
# test=1497 #bascule en 2023 mcrevoi_d=0 # incidence sur le budget, Rcons constant, epargne 
# test=14604 #bascule 2024, mcrevoi_d=260

# Before<-menage_echelle %>% mutate(
#   solde_carb=0,
#   solde_elec=0,
#   solde_dette=0,
#   solde_veh=0,
#   solde_rev_capital=0
# )


# METHODE -----------------------------------------------------------------


# on sélectionne les ménages tous les ans tout du long sans modification de leur budget avant horizon-13 => c'est la date à laquelle un achat de VE va influencer le budget du ménage. 
# 13 ans après l'achat d'un VE, on renouvelle en priorité les ménages possédant déjà un VE et puis on fait de nouvelles sélections. 
# Si j'achète un VE à l'année n, alors le remplacement de celui ci intervient à la 14e année, soit à n+13

# Entre 6 et 13 ans pour les ménages qu'on sélectionne pour le VE => on ne choisit que des ménages n'ayant pas de remboursement en cours sur leur véhicule le plus récent
# 
# Entre 0 et 6 ans, si le ménage rembourse un emprunt sur le véhicule le plus récent alors on calcule la différence entre le remboursement d'achat d'un VE à cette date et le remboursement d'achat d'un VT à cette date et on applique le ratio au remboursement en cours. Le montant en plus est rajouté aux dépenses de frais financiers du ménage. 
# Si le ménage n'a pas de remboursement, on ampute l'intégralité du remboursement du VE à son budget
# 
# A l'horizon, si le ménage paie le remboursement d'un emprunt alors on applique le ratio de remboursement du surcoût, 
# Si ne paie pas d'emprunt, alors c'est que désépargne et donc on ampute les revenus du capital 
# Ex : horizon 2025, si un ménage possède un véhicule acheté en 2022, mais sans rembourser d'emprunt, et qu'en 2022, le surcoût d'un VE (incl bonus) par rapport VT (incl malus) était de 5k€, alors il lui reste à ré-épargner (6-2025-2022)/6=3/6 de 5000€, ie 2500€, si t=3%, alors on utilise la même formule que pour le remboursement d'un emprunt classique à 3% (avec paiement des intérêt)


# BASCULE DES MENAGES 2011-horizon -------------------------------------------

IM=c()
#Classement par lequel on va commencer
# on s'assure que le classement i est bien présent dans la base et qu'on n'a pas dépassé le maximum
i=1
while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}

menage_echelle <- 
  menage_echelle %>% 
  mutate(
    solde_carb=0,
    solde_elec=0,
    solde_dette=0,
    solde_veh=0,
    solde_malus=0,
    solde_rev_capital=0,
    solde_int=0,
    solde_princ=0,
  )

table_solv_year=c()
table_solv_year_ind=c()
menages_insolvables_suppr=c()



# LOOP on Y ---------------------------------------------------------------

for (Y in 2011:horizon){
# for (Y in 2011:2034){
  # print(Y)
  
  sauv_menage_echelle_annee_precedente<-menage_echelle
  
  
  menage_echelle <-
    menage_echelle %>%
    mutate(new_VE=FALSE)
  
  
# COUT VE -----------------------------------------------------------------

  # Ventil VP
  ventil_VP <- as.numeric(ventes_VP %>% filter(Year==Y)%>%select(Particuliers))
  
  
  # Coût d'un VE en électricité par kilomètre
  cout_VE_tot <-as.numeric( # en M€
    ThreeME %>% 
      filter(Var=="EXP_AUTO_H01_CA_23_2*PEXP_23_H01_2") %>%
      filter(year==Y) %>%
      select(value)
  ) * 10^6
  
  vkm_VE_tot<-as.numeric(
    ThreeME %>% 
      filter(Var=="KM_AUTO_H01_CA_23_2") %>%
      filter(year==Y) %>%
      select(value)
  ) *1000
  
  cout_VE_km<-cout_VE_tot/vkm_VE_tot
  
  #Taux d'intérêt
  R_I_AUTO_H01_CG_2<-as.numeric(
    ThreeME%>%
      filter(Var=="R_I_AUTO_H01_CG_2") %>%
      filter(year==Y)%>%
      select(value)
  )
  # Durée emprunt
  R_RMBS_AUTO_ELEC_H01_CA<-as.numeric(
    ThreeME%>%
      filter(Var=="R_RMBS_AUTO_ELEC_H01_CA") %>%
      filter(year==Y)%>%
      select(value)
  )
  # duree_emprunt<-1/taux_emprunt
  
  ## PRIX UNITAIRE d'un VE
  # Données ThreeME de stock (en milliers)
  TOT_VE_nv <-as.numeric( #en Milliers
    (ThreeME %>% 
       filter(Var=="NEWAUTO_ELEC_H01_2") %>%
       filter(year==Y) %>%
       select(value))[1,]*1000*ventil_VP) # en milliers

  TOT_VE_euros <-as.numeric( # en Millions €
    (ThreeME %>% 
       filter(Var=="PAUTO_ELEC_H01_2*NEWAUTO_ELEC_H01_2") %>%
       filter(year==Y) %>%
       select(value)
    )[1,]*ventil_VP) * 10^6
  # plusieurs lignes avec le même nombre de variable "PAUTO_ELEC_H01_2*NEWAUTO_ELEC_H01_2" dans ThreeME
  P_VE <- TOT_VE_euros/TOT_VE_nv
  
  
  ## PRIX UNITAIRE d'un Veh_thermique
  # Données ThreeME de stock (en milliers)
  TOT_VTH_nv<-as.numeric( #en Milliers
    (ThreeME %>% 
       filter(Var=="NEWAUTO_TH_H01_2") %>%
       filter(year==Y) %>%
       select(value))[1,]
  ) * 1000*ventil_VP
  
  TOT_VTH_euros <-as.numeric( # en Millions €
    ThreeME %>% 
      filter(Var=="PNEWAUTO_TH_H01_2*NEWAUTO_TH_H01_2") %>%
      filter(year==Y) %>%
      select(value)
  ) * 10^6*ventil_VP
  
  
  P_VTH <- TOT_VTH_euros/TOT_VTH_nv
  
  

  
 
  
  while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}
  
 ## RENOUVELLEMENT
   #on renouvelle tout d'abord le stock de VE achetés 13 ans plus tôt, à partir de 2023, qui est la 14e année après l'achat d'un VE en 2010
  # (stock suffisant, vérif dans Divers/2020-07-13 Verif renouvellement stock VE.R)
  if(Y>2022){
    menage_echelle <-
    menage_echelle %>%
    mutate_when(year_VE==Y-13,
                list(year_VE=Y,new_VE=TRUE))}
  
  
  
  # Somme des achats de VE par renouvellement ou stock préalable (si Y=2010)
  sum=menage_echelle %>% filter(year_VE==Y) %>% summarise(sum(pondmen))
  if(is.na(sum)){sum=0}
  
  # La trajectoire de ThreeME prévoit de plus en plus de ventes de VE, le renouvellement de la flotte ne saurait entièrement occuper les ventes de l'année Y 
     # while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T) ){i=i+1}
  # Condition : pas encore assez de ventes de VE et encore des ménages éligibles
  
## SELECTION
  while (sum<TOT_VE_nv & i<max(menage_echelle$VE_rank,na.rm=T)){
   
    ###
    # Cas 1
    ####
    # DESCRIPTION :
        # Pour les ménages qu'on sélectionne entre 6 et 13 ans 
        # => emprunt remboursé par hypothèse
        # ils doivent avoir fini de rembourser leur éventuel crédit 
        # ou reconstitué leur épargne, on enlève donc les ménages où crevoi==1
        # si horizon 2035 => les ménages achetant entre 2010 et 2016 
        # ne doivent pas avoir d'emprunt en cours
    ####
    
    if(Y>horizon-13 & Y<=horizon-6 || Y>horizon-13-13 & Y<=horizon-6-13){ #
      # ces ménages n'ont pas de rembousement en cours
      
      # Passer à i+1 tant que [OU (i n'est pas classé et inférieur au max) OU (le ménage i a des remboursement en cours) OU (le ménage achète un veh à l'horizon)]
      while(
        (!i %in% menage_echelle$VE_rank  & 
         i<max(menage_echelle$VE_rank,na.rm=T)) || 
        menage_echelle %>% filter(VE_rank==i) %>%select(mcrevoi_d)>0 ||
        menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)>=5*10^(3)){
        i=i+1}
      
      
      # Selection du ménage im (identifiant_menage)
      if(menage_echelle %>% filter(VE_rank==i) %>%select(mcrevoi_d)==0){ #si on sorti du while en sortant du classement par le max
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i)  %>% select(ident_men))
      }else{
        im<-NA}
    }
    ### Fin boucle cas 1
    
    
    
    ###
    # Cas 2
    ####
    # DESCRIPTION :
    # Horizon et horizon-13
    # à horizon-13 on sélectionne des ménages qui seront de facto sélectionné
    # pour l'horizon, ils doivent avoir des dépenses non nulles de veh dans 
    # leur budget (On considère dep non nulles à partir de 5000 euros ;
    # par exemple Twizzy = 7500euros (bonus non inclus))
    ####
    
    if(Y==horizon-13 || Y==horizon){
      while((!i %in% menage_echelle$VE_rank  & i<max(menage_echelle$VE_rank,na.rm=T)|| menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)<5*10^(3))){i=i+1}
      
      if(menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)>=5*10^(3)){
        im<-as.numeric(menage_echelle %>% filter(VE_rank==i)  %>% select(ident_men))
      }else{im<-NA}
    }
    

    ###
    # Cas 3
    ####
    # DESCRIPTION :
    # Pas d'achet de véhicule à l'horizon
    # ménages entre (horizon-6)-13 horizon-13
    # ménages entre horizon-6 et horizon exclus
    ####
   
    if((Y<horizon-13 & Y>(horizon-6)-13)|| (Y>horizon-6 & Y<horizon)){

      
      while((!i %in% menage_echelle$VE_rank  & i<max(menage_echelle$VE_rank,na.rm=T)|| menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)>=5*10^(3))){i=i+1}
      
      if(menage_echelle %>% filter(VE_rank==i) %>%select(prod_veh)<5*10^(3)){
      im<-as.numeric(menage_echelle %>% filter(VE_rank==i) %>% select(ident_men))
      }else{
        im=NA}
    }
    ###### Fin des Cas de sélection
    
    
    
    
    ## Somme des ventes
    
    # si im=NA c'est qu'aucun ménage en remplissait les conditions
    if(!is.na(im)){
     sum =
      sum +
      as.numeric(menage_echelle %>% filter(ident_men==im) %>% select(pondmen))
    
   
    IM<-c(IM,im)
    # print(im)
    menage_echelle<- 
      menage_echelle %>% 
      mutate_when(ident_men==im,list(VE=TRUE,
                                     new_VE=TRUE,
                                     year_VE=Y,
                                     year_VE_sauv=paste(year_VE_sauv,Y,sep="_"),VE_rank=0))
    
    }
    
    
    while(!i %in% menage_echelle$VE_rank & i<max(menage_echelle$VE_rank,na.rm=T)){i=i+1}
  } ##### Fin selection ménages
  
  
  
  


  #######
  # BUDGETS -----------------------------------------------------------------
  #######
  
  
    ####
    # Cas 1
    ####
    # DESCRIPTION 
    #Ces ménages n'ont pas de remboursement en cours, selon les hypothèses de
    # ThreeME, ces ménages ont déjà fini de rembourser leur épargne ou leur dette
    # les modifications de budgets sont le carburant et l'électricité.
    ####
  if(Y>horizon-13 & Y<=horizon-6){
  
    
    # Elec et carburant
  menage_echelle <- 
    menage_echelle %>% 
    mutate_when(
      # Condition : achat VE neuf pour ménage avec plusieurs véhicules
      new_VE& nbvehic>1,
      # Action
      list(
        solde_carb = - carb_lubr * percent_pkm_eligible,
        solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible,
        Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
      ),
      new_VE& nbvehic==1,
      list(
        solde_carb = - carb_lubr, #pas de report modal dans le cas d'un unique véhicule électrifié
        solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
        dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible,
        Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
      )
      )
  }
  
    
    
    ####
    # Commun Cas 3 et 2
    ####
    if(Y>horizon-6){
      
      repayment_VE<-as.numeric(int_princ(loan=P_VE,
                                         # n=1/as.numeric(R_RMBS_AUTO_ELEC_H01_CA),
                                         n=6,
                                         year_purchase = Y,
                                         horizon=horizon,
                                         i=  R_I_AUTO_H01_CG_2  ,
                                         pf=1))
      
      repayment_VTH<-as.numeric(int_princ(loan=P_VTH, 
                                          # n=1/as.numeric(R_RMBS_AUTO_ELEC_H01_CA),
                                          n=6,
                                          year_purchase = Y,
                                          horizon=horizon,
                                          i=  R_I_AUTO_H01_CG_2  ,
                                          pf=1))
      
      #Intérêts sur annuités totales (intérêt + remboursement du principal)
      ratio_int_over_annuite<-repayment_VTH[1]/sum(repayment_VTH)}
    
    

    
    ###
    # Cas 3
    ####
  if(Y>horizon-6 & Y<horizon) {
    # ménages qui rembourse une dette
    menage_echelle <- 
      menage_echelle %>% 
      mutate_when(
        # Condition : achat VE neuf pour ménage avec plusieurs véhicules => carburant et électricité
        new_VE & nbvehic>1,
        # Action
        list(
          solde_carb = - carb_lubr * percent_pkm_eligible,
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
        ),
        new_VE& nbvehic==1,
        list(
          solde_carb = - carb_lubr, #pas de report modal dans le cas d'un unique véhicule électrifié
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible,
          dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible
        ),
        
      # Pour la dette
      new_VE & mcrevoi_d>0 , #si ont déjà une dette en cours => màaj du remboursement et ventilation entre remboursement du principal et intérêt
      list(solde_int=mcrevoi_d*P_VE/P_VTH*ratio_int_over_annuite,
           solde_princ=mcrevoi_d*P_VE/P_VTH*(1-ratio_int_over_annuite)
      ),
      new_VE & is.na(mcrevoi_d), #alors ça signifie que le ménage a payé son véhicule par désépargne. Il faut donc augmenter le montant désépargné des revenus du capital. Hypothèse : prix moyen du VE
      list(solde_rev_capital=sum(repayment_VE)) #négatif qui va venir amputer les revenus du capital
      ) 
  }

  
  
   
    ###
    # Cas 2
    ####
  if(Y==horizon) {
    menage_echelle <- 
      menage_echelle %>% 
      mutate_when(
        # Condition : achat VE neuf pour ménage avec plusieurs véhicules => carburant et électricité
        new_VE & nbvehic>1,
        # Action
        list(
          solde_carb = - carb_lubr * percent_pkm_eligible*1/2,
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          solde_veh=prod_veh*(1+BM_rel_2010) *(P_VE/P_VTH-1) # Update budget A06 des ménages basculant : A06×(prix_moyen VE)/(prix moyen VP TH) #SANS MALUS
          # pour garder la dispersion (petits ou gros veh)
          #on enlève le malus de 2010 avant d'appliquer le ratio P_VE/P_VTH qui prend en compte les bonus-malus de l'horizon
        ),
        new_VE& nbvehic==1,
        list(
          solde_carb = - carb_lubr*1/2, #pas de report modal dans le cas d'un unique véhicule électrifié
          solde_elec = cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec = dep_Elec + cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          dep_Elec_verif=dep_Elec_verif+ cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          Elec_ElecSpe = Elec_ElecSpe +  cout_VE_km* km_auto* percent_pkm_eligible*1/2,
          solde_veh=prod_veh*(1+BM_rel_2010) *(P_VE/P_VTH-1)
          # Update budget A06 des ménages basculant : A06×(prix_moyen VE)/(prix moyen VP TH)
          # pour garder la dispersion (petits ou gros veh)
        ),
        
        # Pour la dette
        new_VE & mcrevoi_d>0 ,
        list(solde_int=mcrevoi_d*P_VE/P_VTH*ratio_int_over_annuite*1/2,
             solde_princ=mcrevoi_d*P_VE/P_VTH*(1-ratio_int_over_annuite)*1/2
             ),
        new_VE & is.na(mcrevoi_d), #alors ça signifie que le ménage a payé son véhicule par désépargne. Il faut donc augmenter le montant désépargner des revenus du capital
        list(solde_rev_capital=sum(repayment_VE)) #négatif qui va venir amputer les revenus du capital
      ) 
  }
  ######### Fin modif budget
  ####
  
  
  # SOLVABILITE ex-post -----------------------------------------------------
  
  # NB : on repart de la solvabilité calculée en step 3.1
  # calcul solvabilité
  menage_echelle <- 
    menage_echelle %>%
    mutate(solv=ifelse(new_VE==T,solv + (solde_int+solde_princ)/RDB,solv))
  
  # Les ménages insolvables après la transition sont écartés
  menages_insolvables <- 
    menage_echelle %>% 
    filter(new_VE & solv>0.33)%>%
    select(year_VE,ident_men,solv)
  
  sauv_menages_insolvables <- 
    sauv_menage_echelle_annee_precedente %>% 
    filter(ident_men %in%menages_insolvables$ident_men)

  menages_insolvables_suppr <- 
    rbind(
      menages_insolvables_suppr, 
      menages_insolvables
      )

  # les ménages insolvables sont "rebootés" à l'itération précédente, avant leur rénovation
  if(dim(menages_insolvables)[1]>0){
    menage_echelle <-
      rbind(menage_echelle %>%
              filter(!ident_men %in% menages_insolvables$ident_men),
            sauv_menages_insolvables)%>%
      arrange(ident_men)
  }
  

  
  
  # MEMOIRE VE -----------------------------------------------------------
  
  
  # annuités sur RDB
  # Endettement aggrégé des ménages achetant un VE année après année 
  table_solv_year<-rbind(table_solv_year,
                         c(Y,
                           as.numeric(menage_echelle %>% mutate(solv_add=(solde_int+solde_princ)/RDB)%>%filter(new_VE)%>%summarise(weighted.mean(solv_add,w=pondmen)))))
  
  # Solvabilité des ménages rénovant chaque année (obj : vérifier la solvavilité des ménages rénovation après rénovation)
  table_solv_year_ind<-rbind(table_solv_year_ind,
                             menage_echelle %>% 
                               filter(year_VE==Y)%>%
                               select(year_VE,
                                      ident_men,
                                      solv,
                                      solde_int,
                                      solde_princ,
                                      RDB))
  
  
  
  

  # ITERATION ---------------------------------------------------------------
  
  
  # On reprend à 1 ou plus parce qu'à moins de 6 ans de l'horizon on peut sélectionner les ménages ayant un crédit en cours => on parcourt de nouveau tous les ménages
  i=min(menage_echelle%>%filter(VE_rank>0)%>%select(VE_rank))
  print(dim(menage_echelle %>% filter(year_VE==Y)%>%select(ident_men))[1])
  
  } # FIN BOUCLE Y ## Année suivante



sauv_int<-menage_echelle




# Amélioration de la consommation des veh thermiques ----------------------

# Efficacité des moteurs => diminution homothétique de la consommation en carburant restante de tous les ménages

# 
# 
# conso_fuel_horizon <-as.numeric(
#   ThreeME %>%
#   filter(year==horizon) %>%
#   filter(Var=="EXP_AUTO_H01_CA_22_2/AUTO_H01_CA_22_2") %>%
#   select(value)
# )
# 
# conso_fuel_2010 <-as.numeric(
#   ThreeME %>%
#     filter(year==2010) %>%
#     filter(Var=="EXP_AUTO_H01_CA_22_2/AUTO_H01_CA_22_2") %>%
#     select(value)
# )
# (conso_fuel_2025-conso_fuel_2010)/conso_fuel_2010
# (conso_fuel_2030-conso_fuel_2010)/conso_fuel_2010
# 
# gain_conso <- (conso_fuel_horizon-conso_fuel_2010)/conso_fuel_2010
# # -10.3% 2025
# # -14.8% 2030
# # -30.7% 2035

# Diminution des usages (télétravail et voirie)
forcage_vkm<-read_excel(path=MatisseFiles$forcage_km_xl,sheet="value")
gain_vkm<-as.numeric(forcage_vkm %>% filter(year==horizon)%>%select(gain_vkm))


#Malus pour les VT
# attention malus BM_rel<0 pour indiquer un surcoût, on rajoute un moins
menage_echelle <-
  menage_echelle %>%
  mutate_when((prod_veh>5*10^(3)),
              list(solde_malus=prod_veh*(1+BM_rel_2010)*(-BM_rel_th_horizon)))%>% #solde_malus positif => va venir diminuer les autres consommations
  mutate_when(new_VE,
              list(solde_malus=0)) #on ne rajoute pas le bonus des VE, déjà intégré dans les prix de ThreeME
  

Bonus_VE_tot<-Bonus_VE_horizon *  menage_echelle %>% filter(year_VE==horizon)%>%summarise(sum(pondmen))
Bonus_malus_net<-Bonus_VE_tot-menage_echelle %>% summarise(sum(solde_malus*pondmen))
write.csv(c(Bonus_malus_net,scenario,scenario_classement,horizon, redistribution),file=MatisseFiles$bm_net_csv)


# Variable New_VT

menage_echelle <-
  menage_echelle %>%
  mutate(new_VT=FALSE)

menage_echelle <-
  menage_echelle %>%
  mutate_when((prod_veh>5*10^(3)&carb_lubr>0 & new_VE==FALSE),list(new_VT=TRUE))


# Créer les soldes budgétaires
menage_echelle <-
  menage_echelle %>%
  mutate(carb_lubr = carb_lubr+solde_carb) #on applique en deux temps les gains de carburant pour s'assurer que les gains vkm ne s'appliquent qu'aux vlm résiduels des ménages achetant un VE => le carb_lubr dans le nouveau calcul de solde_carb ne prend en compte que le "nouveau" carb_lubr

menage_echelle <-
  menage_echelle %>%
  mutate(solde_carb=solde_carb+carb_lubr*gain_vkm) %>%
  mutate(
    carb_lubr=carb_lubr+carb_lubr*gain_vkm,
    prod_veh=prod_veh+solde_veh+solde_malus,
    autres_services=autres_services+solde_int,
    Hors_budget=Hors_budget+solde_princ,
    # solde_int_total=solde_int_total+solde_int,
    # solde_princ_total=solde_princ_total+solde_princ,
    rev_patrimoine=rev_patrimoine+solde_rev_capital,
    RDB=RDB+solde_rev_capital)


save(menage_echelle,file=MatisseFiles$menage_echelle_TC_VE_pre_revent_rd)

# Reventilation -----------------------------------------------------------


solde<-menage_echelle %>% 
  mutate(solde=
           solde_elec+
           solde_carb+
           solde_rev_capital+
           solde_int+
           # solde_veh+ #on ne compte pas le surcoût du véhicule, solde_int est là pour ça.
           solde_malus
           ) %>%
  select(ident_men,solde)


sauv_int<-menage_echelle

menage_echelle_VE<-Ventil_solde(solde,menage_echelle,step="VE")




# Evolution energies : source_usage ---------------------------------------

menage_echelle_VE <- 
  menage_echelle_VE %>%
  mutate(ifelse(dep_Elec_verif>0,Elec_ElecSpe=Elec_ElecSpe*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Elec_verif>0,Elec_clim=Elec_clim*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Elec_verif>0,Elec_ecl=Elec_ecl*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Elec_verif>0,Elec_ECS=Elec_ECS*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Elec_verif>0,Elec_chauff=Elec_chauff*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Elec_verif>0,Elec_Cuisson=Elec_Cuisson*dep_Elec/dep_Elec_verif,0))%>%
  mutate(ifelse(dep_Gaz_verif>0,Gaz_ECS=Gaz_ECS*dep_Gaz/dep_Gaz_verif,0))%>%
  mutate(ifelse(dep_Gaz_verif>0,Gaz_chauff=Gaz_chauff*dep_Gaz/dep_Gaz_verif,0))%>%
  mutate(ifelse(dep_Gaz_verif>0,Gaz_Cuisson=Gaz_Cuisson*dep_Gaz/dep_Gaz_verif,0))%>%
  mutate(ifelse(dep_Gaz_verif>0,Gaz_ECS=Gaz_ECS*dep_Gaz/dep_Gaz_verif,0))%>%
  mutate(ifelse(dep_GPL_verif>0,GPL_ECS=GPL_ECS*dep_GPL/dep_GPL_verif,0))%>%
  mutate(ifelse(dep_GPL_verif>0,GPL_chauff=GPL_chauff*dep_GPL/dep_GPL_verif,0))%>%
  mutate(ifelse(dep_GPL_verif>0,GPL_Cuisson=GPL_Cuisson*dep_GPL/dep_GPL_verif,0))%>%
  mutate(ifelse(dep_Solides_verif>0,Solides_Cuisson=Solides_Cuisson*dep_Solides/dep_Solides_verif,0))%>%
  mutate(ifelse(dep_Solides_verif>0,Solides_chauff=Solides_chauff*dep_Solides/dep_Solides_verif,0))%>%
  mutate(ifelse(dep_Solides_verif>0,Solides_ECS=Solides_ECS*dep_Solides/dep_Solides_verif,0))%>%
  mutate(ifelse(dep_Fuel_verif>0,Fuel_ECS=Fuel_ECS*dep_Fuel/dep_Fuel_verif,0))%>%
  mutate(ifelse(dep_Fuel_verif>0,Fuel_Cuisson=Fuel_Cuisson*dep_Fuel/dep_Fuel_verif,0))%>%
  mutate(ifelse(dep_Fuel_verif>0,Fuel_chauff=Fuel_chauff*dep_Fuel/dep_Fuel_verif,0))%>%
  mutate(ifelse(dep_Urbain_verif>0,Urbain_ECS=Urbain_ECS*dep_Urbain/dep_Urbain_verif,0))%>%
  mutate(ifelse(dep_Urbain_verif>0,Urbain_Cuisson=Urbain_Cuisson*dep_Urbain/dep_Urbain_verif,0))%>%
  mutate(ifelse(dep_Urbain_verif>0,Urbain_chauff=Urbain_chauff*dep_Urbain/dep_Urbain_verif,0))%>%
  select(-paste("dep",sources,"verif",sep="_"))


for (source in sources){
menage_echelle_VE[paste("dep",source,"verif",sep="_")]<-rowSums(menage_echelle_VE %>% select(contains(paste(source,"_",sep="")))) 
}

####
# VERIF
####

# table(abs(menage_echelle$dep_Elec-menage_echelle$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle$dep_Gaz-menage_echelle$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle$dep_GPL-menage_echelle$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle$dep_Fuel-menage_echelle$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle$dep_Solides-menage_echelle$dep_Solides_verif)<10^(-10))


# table(abs(menage_echelle_TC_DPE$dep_Elec-menage_echelle_TC_DPE$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Gaz-menage_echelle_TC_DPE$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_GPL-menage_echelle_TC_DPE$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Fuel-menage_echelle_TC_DPE$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle_TC_DPE$dep_Solides-menage_echelle_TC_DPE$dep_Solides_verif)<10^(-10))







# VERIF -------------------------------------------------------------------
# 
# A4<-menage_echelle_VE %>% filter(ident_men==test)


# ener_surf

menage_echelle_dom<-energie_dom_surf(menage_echelle_VE)
menage_echelle_VE<- 
  menage_echelle_VE %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(menage_echelle_dom,by="ident_men")




# ETUDE DE IM -------------------------------------------------------------

# # Ménages ayant acheté un VE
# 
# load("2010/menage_calibr_2010.RData")
# menage_calibr_2010 %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# # 1829912
# menage_calibr_2010 %>% filter(ident_men %in% IM) %>% filter(prod_veh>0) %>% summarise(sum(pondmen))
# # 1     601 595.2
# # => 33% des ménages qu'on a sélectionné ont des dépenses non nulles en prod veh. Non négligeable. 

# IM1<-IM
# IM1 : identifiants des ménages VE==TRUE sans contrainte sur prod_veh>0
# IM : identifiants des ménages VE==TRUE avec contraintes sur prod_veh=0

# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM1)) %>% summarise(sum(pondmen))
# 
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM1)) %>% summarise(sum(pondmen))/
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# 1    0.6707697 # 67% des ménages sélectionnés en commun. Et on est plus près des chiffres de ThreeME sur les ventes de veh thermiques


# IM2<-IM
# IM2 c'est en partant de menage_echelle_41 après les achats de maisons neuves en 2025. 
#IM C'est avec contraintes sur prod_veh=0 et une fois refait les rehab et achats de DPE
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM2)) %>% summarise(sum(pondmen))
# 
# menage_echelle %>% filter(ident_men %in% intersect(IM,IM2)) %>% summarise(sum(pondmen))/
# menage_echelle %>% filter(ident_men %in% IM) %>% summarise(sum(pondmen))
# 1    0.9900978



# SAVE FILE ---------------------------------------------------------------

menage_echelle_TC_VE<-menage_echelle_VE

save(menage_echelle_TC_VE, file=MatisseFiles$menage_echelle_TC_VE_rd)


menage_echelle <- menage_echelle_TC_VE %>% select(-typmen5,-
percent_pkm_eligible,-potentiel_VE,-VE_rank_pess,- VE_rank_opt,-VE_rank,-solde_dette,-solde_elec,-solde_carb,-solde_veh)
save(menage_echelle,file=MatisseFiles$menage_echelle_rd)




# Clean -------------------------------------------------------------------
suppressWarnings(rm(auto,auto_elec,Bonus_malus_net,Bonus_VE_tot,forcage_vkm,km_auto_sum,max_recvoi,menage,menage_echelle,menage_echelle_TC_DPE,menage_echelle_VE,
  menage_temp,menages_insolvables,menages_insolvables_suppr,sauv_int,sauv_menage_echelle_annee_precedente,sauv_menages_insolvables,solde,table_solv_year,
  table_solv_year_ind,ThreeME,ventes_VP, FC, menage_echelle_dom,menage_echelle_TC_VE))
gc()



# SUCCESS -----------------------------------------------------------------

# print("3_2_2_VE_2010_horizon : SUCCESS")




# ANALYSE EX-POST ---------------------------------------------------------


# Question : à combien limiter la solvabilité ex-ante en sélection des ménages pour éviter que ceux ci ne soient sur-endettés à la fin ?

#ménages surdenttés
# menage_echelle %>% filter(year_VE>0)%>%filter(solv>=0.33 & solv<999)%>%select(ident_men,year_VE,solv,RDB,solde_int, solde_princ) #=> 0 après traitement (voir proposition plus bas)


#Moyenne année après année de l'ajout d'endettement pour les ménages proprio rénovant
# table_solv_year_filter<-as.data.frame(table_solv_year)%>%filter(V1>=2030)
# mean(table_solv_year_filter[-1,2]) #=> 0.001951428 #24/07/2020 : en imposant solv<=0.33 on obtient un endettement additionnel de 0.2% => d'où limite à 32.8% d'endettement pour solvabilité ex-ante. Les 15 cas hors limites voient leur achat de VE annulé. 


# Contrôle des ménages supprimés pour cause de solvabilité au cours du code
# menages_insolvables_suppr



# VERIFICATION - SENSIBILITE %_km_eligibles -------------------------------

# Etude de sensibilité avec 100% de km éligible
# 1/ -0.07pts sur la part du carburant représente 48 milliards, soit une chute de 3.10% de la conso des ménages de carburant. Ce n'est pas négligeable. 
# 2/ Avec 100% de km éligible, des ménages avec deux véhicules dont un VE n'ont plus aucune dépense de carburant. 

# CCL : on garde l'estimation par cellule de km éligibles. 

# 
# # menage_km_eligible<- menage_echelle
# 
# menage_echelle %>% summarise(sum(pondmen*carb_lubr))
# # Carb_km_eligible
# # 50300321809
# 
# list_dep_14=c("agriculture",
#               "dep_Elec",
#               "dep_Gaz",
#               "dep_autres_energies",
#               "BTP",
#               "prod_veh",
#               "carb_lubr",
#               "transp_rail_air",
#               "transp_routes_eau",
#               "loisirs_com",
#               "autres_services",
#               "autres",
#               "loyers",
#               "veh_occasion")
# menage_bis<- menage_echelle %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
# menage_bis$Rcons_bis <- rowSums(menage_bis[list_dep_14])
# menage_bis %>% summarise(sum(Rcons_bis*pondmen))
# # Rcons_km_eligible
# # 1.855404e+12
# 
# 
# compute_share_export(menage_echelle)
# # share_km_eligible
# # share_A01    share_A02    share_A03    share_A04    share_A05    share_A06    share_A07
# # share 1.400302e-01 1.599364e-02 7.482537e-03 4.074589e-03 7.413391e-02 3.799413e-02 2.385681e-02
# # Vol   2.952435e+11 3.372142e+10 1.577638e+10 8.590970e+09 1.563059e+11 8.010783e+10 5.030032e+10
# # share_A08    share_A09    share_A10    share_A11    share_A12    share_A13
# # share 1.548047e-02 2.751363e-03 1.107707e-01 2.386330e-01 1.662550e-01 1.625436e-01
# # Vol   3.263944e+10 5.801047e+09 2.335519e+11 5.031401e+11 3.505365e+11 3.427113e+11
# compute_savings_rate_export(menage_echelle)
# # 0.1035153
# # savings_km_eligible
