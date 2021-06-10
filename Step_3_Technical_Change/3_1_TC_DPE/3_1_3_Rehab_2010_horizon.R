#Including enertot change

# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(decisionSupport)
library(readxl)
library(car)
library(dplyr)

source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/Repayment.R",sep=""))
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/calc_ems.R",sep=""))
source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))



# DATA --------------------------------------------------------------------

TCO<-as.numeric(read_excel(path=MatisseFiles$IMACLIM_3ME_scen_horiz_xl,range="C103",col_names=F))*10^6

# Base ménage
load(MatisseFiles$menage_echelle_32_rd)
load(MatisseFiles$FC_2010_horizon_rd)
load(MatisseFiles$source_usage_rd)
load(MatisseFiles$Threeme_rd)
load(MatisseFiles$coeff_ems_2010_rd)
coeff_dep_ems<-read_csv(MatisseFiles$coeff_dep_ems_csv)


# DONNEES MANUELLES -------------------------------------------------------

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




# PREPARATION -------------------------------------------------------------

### Renommer datatable
menage_echelle<-menage_echelle_32
# rm(menage_echelle_32)


### Recoder variable
# STALOG
menage_echelle$stalog_bis<-car::recode(menage_echelle$stalog," 1:2 =1 ; 3=3 ; 4:5= 2 ; 6=3")
# PROPRI
menage_echelle$propri_bis<-car::recode(menage_echelle$propri," 1 =3 ; 2=2 ; 3:4= 3 ; 5:6=1; 7=3")
# suppression NA
menage_echelle <- menage_echelle %>%
  mutate(stalog_propri=0)%>%
  mutate_when(is.na(propri),list(stalog_propri=stalog_bis))%>%
  mutate_when(!is.na(propri),list(stalog_propri=as.numeric(paste(stalog_bis,propri_bis,sep=""))))


## Hypothèse : rénovation prioritaire selon statut logement
# priority order for rehabilitation : 
## 1 : propriétaires
## 22 : locataires HLM
## 21 : locataires bailleurs privés
## 23 : locataires bailleurs autres
## 2 : locataire bailleur inconnu
## 3 : autre statut : usufruit, logé à titré gratuit
priority<-c(1,22,21,23,2,3)


# solvabilité des ménages
menage_echelle <- 
  menage_echelle %>%
  mutate(solv=ifelse(RDB==0,999,(c13511+c13221+c13211)/RDB))






# SELECTION MENAGES -------------------------------------------------------

# Les ménages ayant acheté des logements neufs sont soit à exclure, soit exclut d'office car étant de classe A. 
# En fonction de l'hypothèse de classe DPE des nouvelles constructions. Le cas ici. 


menage_echelle<-
  menage_echelle %>%
  mutate(DPE_stalog_propri= paste(DPE_dep,stalog_propri,sep="_"))%>%
  mutate(exclus=FALSE,
         REHAB=FALSE,
         year_rehab=0,
         solde_dette_BP=0,
         hausse_loyer=0,
         solde_loyer=0,
         hausse_loyer_sum=0) %>%
  mutate_when(year_neuf>0,list(exclus=TRUE),
              c13711>10000,list(exclus=TRUE)) %>% #si c13711>0 alors le ménage vient d'acheter ce logement, si les propriétaires précédents ont fait rénover, on ne sait pas sur qui faire porter le coût de cette réno
  mutate_when(year_neuf==0, list(classe_arr=DPE_dep))%>%
  mutate(solde_int=0,solde_ener=0,principal_dette=0,solde_princ=0,subvention=0)%>% 
  mutate_when(solv>0.297 & stalog<=2,list(exclus=TRUE))%>% #seule la solvabilité des propriétaires est importante, les locataires insolvables peuvent quand même être rénovés
  mutate(count_rehab="")
# 
# #voir si on peut les intégrer
# mutate_when(ident_men==8063,list(exclus=TRUE))%>% #menage trop fragile
# mutate_when(ident_men==11672,list(exclus=TRUE))%>% #menage trop fragile #RDB=1867 (AMS 2035)
# mutate_when(ident_men==6369,list(exclus=TRUE))%>% #(ménage qui bugge en AMS 2035 Median forfait => 2033, avec -263% de solde_int) # RDB= 177
# mutate_when(ident_men==10583,list(exclus=TRUE))%>% #RDB=39



Cout_bailleur_prive=c()
Dette_bailleur_prive=c()
Solde_Ener_tot=c()
ident_rehab=c()
table_solv_year=c()
table_solv_year_ind=c()
menages_insolvables_suppr=c()




# LOOP on YEARS -----------------------------------------------------------
#important pour que les ménages puissent faire plusieurs REHAB
# va surtout avantager les ménages locataires pour qui la solvabilité n'est pas un obstacle (ne portent pas le coût de la réno)


###
## BOUCLE SUR Y
###

for (Y in 2010:horizon){
  print(Y)
  ident_r<-c()
  
  sauv_menage_echelle_annee_precedente<-menage_echelle
  
  
  
  
  # DONNEES THREEME ---------------------------------------------------------
  
  # taux de remboursement des rénovations énergétiques
  R_RMBS_NEWBUIL_H01_CA<-as.numeric(
    ThreeME %>%
      filter(Var=="R_RMBS_REHAB_H01_CB") %>%
      filter(year==Y) %>%
      select(value)
  )
  
  # Taux d'intérêts des emprunts liés aux travaux de réhabilitation des logements en %
  R_I_REHAB_H01_CG_2  <-  as.numeric(
    ThreeME %>%
      filter(Var=="R_I_REHAB_H01_CG_2") %>%
      filter(year==Y) %>%
      select(value)
  )
  
  
  
  
  # MATRICE DES COÛTS (ANNEE Y) ---------------------------------------------
  Cost_m2=c()
  # travaux de rénovation énergétiques en volume par saut de classe (en M2) 
  # Transition de L vers M
  
  for (dep in LETTERS[1:7]){
    # classe DPE de départ
    
    for (arr in LETTERS[1:7]){
      # classe DPE d'arrivée
      # On vérifie que la transition est une amélioration (M "mieux" que L)
      
      if(dep>arr){
        
        #extraction stock m2 passant de M à L en 2010 (ThreeME)
        stock_m2<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("REHAB_H01_C",dep,"_C",arr,"_2",sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        
        #extraction coût des travaux pour passer de M à L en 2010 (ThreeME) en M€
        stock_euros<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("PREHAB_H01_C",dep,"_C",arr,"_2*","REHAB_H01_C",dep,"_C",arr,"_2",sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        
        #   stock_euros/stock_m2 = coût de la réhabiliation par m2 (en €/m2)
        # Création matrice Cost_m2 : 
        #   DPE_départ | DPE_arrivée | coût_m2 | coût_total_transition | m2_total_transition
        Cost_m2=rbind(Cost_m2,c(dep,arr,stock_euros/stock_m2*(10^6),stock_euros,stock_m2))
        
      }
    }
  }
  colnames(Cost_m2)<-
    c(
      "classe_dep",
      "classe_arr",
      "cost_m2",
      "transition_tot_Meuros",
      "transition_tot_m2"
    )
  
  # convertir en data.frame
  Cost_m2<-as.data.frame(Cost_m2,stringsAsFactors=F)
  Cost_m2$cost_m2<-as.numeric( Cost_m2$cost_m2)
  Cost_m2$transition_tot_m2<-as.numeric( Cost_m2$transition_tot_m2)
  Cost_m2$transition_tot_Meuros<-as.numeric( Cost_m2$transition_tot_Meuros)
  
  
  
  # MATRICE GAIN ENER (ANNEE Y) ---------------------------------------------
  
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
  
  Mat_gain_ener<-data.frame("DPE_before"=sort(rep(LETTERS[1:7],7)),"DPE_after"=rep(LETTERS[1:7],7))
  Mat_gain_ener$value_after<-sapply(Mat_gain_ener$DPE_after,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value_before<-sapply(Mat_gain_ener$DPE_before,function(x) as.numeric(conso_moy_dep[x]))
  Mat_gain_ener$value<-(Mat_gain_ener$value_after-Mat_gain_ener$value_before)/Mat_gain_ener$value_before
  Mat_gain_ener <- Mat_gain_ener %>% select(-c(value_after,value_before))
  
  
  
  
  
  
  
  # DPE_STALOG_PROPRI : saut de classe  ------------------------------------------
  
  order=paste(rep(LETTERS[1:7],each=6),rep(priority,6),sep="_")
  order_value<-rep(0,length(order))
  
  # order_value = longueur de chaque classe DPE_stalog_propri pour pouvoir incrémenter le classement médian
  for (i in 1:length(order)){
    table_order_value<-menage_echelle %>% filter(DPE_stalog_propri==order[i])
    order_value[i]<-ifelse(is.null(dim(table_order_value)),0,length(table_order_value$DPE_stalog_propri))        
  }
  order_value[is.na(order_value)] <- 0
  order_value<-as.numeric(order_value)
  order_ter<-as.numeric(order_value)
  
  # Pour chaque DPE_stalog_propri
  # On va classer par la suite les ménages par DPE, au sein de chaque DPE, classement optimiste et pessimiste en classant par DPE_stalog_propri et par conso d'énergie. 
  # On passe d'un classement au sein de chaque classe DPE_stalog_propri => on passe à un classement par DPE en respectant la priorité stalog_propri. 
  # 6 DPE et 6 stalog_propri
  for (j in 0:6){ # on parcourt tous les DPE
    for (i in 2:6){ # on parcourt les 6 catégories stalog_propri
      a<-6*j+1
      b<-6*j+i-1 # pas la peine d'aller jusqu'à 6*j+i, on veut le seuil des 5 premiers stalog_propri pour l'ajouter au 6e
      order_ter[6*j+i]<-sum(order_value[a:b])
    }
    order_ter[6*j+1]<-0
  }
  order<-as.character(order)
  order<-data.frame(order,order_ter)
  colnames(order)<-c("DPE_stalog_propri","rank_add")
  order$DPE_stalog_propri<-as.character(order$DPE_stalog_propri)
  
  menage_echelle <- menage_echelle%>%left_join(order,by="DPE_stalog_propri")
  
  
  
  
  
  # CLASSEMENT MENAGES ------------------------------------------------------
  # is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  menage_echelle<-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_opt =row_number(-ener_dom)) %>% 
    ungroup()
  
  
  
  menage_echelle<-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_opt =row_number(-ener_dom)) %>% 
    ungroup()  
  
  
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_pess =max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>% 
    ungroup()
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_med =kWh_rank_pess-kWh_rank_opt) %>% 
    mutate_when(
      kWh_rank_med<=0,
      list(kWh_rank_med=-kWh_rank_med+1)) %>%
    ungroup()
  
  menage_echelle<-
    menage_echelle %>% 
    mutate(ener_dom_surf=ifelse(is.infinite(ener_dom_surf),0,ener_dom_surf))%>%
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_opt_ener =row_number(-ener_dom_surf)) %>% 
    ungroup()
  
  menage_echelle<-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_pess_ener =max(kWh_rank_opt_ener,na.rm=T)-kWh_rank_opt_ener+1) %>% 
    ungroup()
  
  
  menage_echelle <-
    menage_echelle %>% 
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_med_ener = kWh_rank_pess_ener-kWh_rank_opt_ener) %>% 
    mutate_when(
      kWh_rank_med_ener<=0,
      list(kWh_rank_med_ener=-kWh_rank_med_ener+1)) %>%
    ungroup()  
  
  
  menage_echelle$ems_tot_chauff_ecs<-calc_ems(menage_echelle,FC)
  
  menage_echelle<-
    menage_echelle %>% 
    mutate(ems_tot_chauff_ecs_surf=ems_tot_chauff_ecs/surfhab_d)%>%
    mutate(ems_tot_chauff_ecs_surf=ifelse(is.infinite(ems_tot_chauff_ecs_surf),0,ems_tot_chauff_ecs_surf))%>%
    group_by(DPE_stalog_propri) %>% 
    dplyr::mutate(kWh_rank_opt_co2 =row_number(-ems_tot_chauff_ecs_surf)) %>% 
    ungroup()
  
  ###### 
  # # Test
  # View(menage_echelle %>% arrange(DPE_stalog_propri,kWh_rank_opt)%>%select(ident_men,DPE_stalog_propri,kWh_rank_opt,kWh_rank_pess,kWh_rank_med,ener_dom,rank_add,rank_add_med))
  # View(menage_echelle %>%filter(DPE_dep=="B") %>%arrange(DPE_stalog_propri,kWh_rank_opt)%>%select(ident_men,DPE_stalog_propri,kWh_rank_opt,kWh_rank_pess,kWh_rank_med,ener_dom,rank_add,rank_add_med))
  ######
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(kWh_rank_opt=kWh_rank_opt+rank_add,
           kWh_rank_pess=kWh_rank_pess+rank_add,
           kWh_rank_med=kWh_rank_med+rank_add,
           kWh_rank_opt_ener=kWh_rank_opt_ener+rank_add,
           kWh_rank_opt_co2=kWh_rank_opt_co2+rank_add) %>%
    select(-rank_add)
  
  
  
  
  if(str_detect(scenario_classement,"Optimal_ener")){
    menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt_ener)
  }
  if(str_detect(scenario_classement,"Optimal_co2")){
    menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_opt_co2)
  }
  if(str_detect(scenario_classement,"Pess_ener")){
    menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_pess_ener)
  }
  if(str_detect(scenario_classement,"Med_ener")){
    menage_echelle <- menage_echelle %>% mutate(kWh_rank=kWh_rank_med_ener)
  }
  
  
  ######
  # Test
  # View(menage_echelle %>%filter(DPE_dep=="B") %>%arrange(DPE_stalog_propri,kWh_rank_opt)%>%select(ident_men,DPE_stalog_propri,kWh_rank_opt,kWh_rank_pess,kWh_rank_med,ener_dom))
  # Test
  # for (i in LETTERS[1:7]){
  # print(i)
  # print(table(table(menage_echelle %>% filter(DPE_dep==i)%>%select(kWh_rank_med))))
  # }
  ######
  
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
  
  
  
  
  
  # Sélection 2e tour -----------------------------------------------------------------
  
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(stalog==6,list(kWh_rank=0)) %>%  #on exclut les ménages logés gratuitement
    mutate_when(stalog==3,list(kWh_rank=0)) %>% # on les ménages Usufruitier, y compris en viager
    mutate_when(exclus,list(kWh_rank=0)) %>%
    mutate_when(solv>0.297 & stalog<=2,list(exclus=TRUE))%>% #on exclut ceux qui sont passés au dessus de 0.297
    mutate_when(year_neuf>0,list(kWh_rank=0)) #on exclut ceux qui 
  
  
  
  # BASCULE DES MENAGES --------------------------------------------------------------
  
  
  for (dep in LETTERS[1:7]){
    #dep : classe DPE de départ
    
    # Création d'une table pour la boucle par DPE de départ
    menage_echelle_classe <- 
      menage_echelle %>% 
      filter(DPE_dep==dep) %>% 
      arrange(kWh_rank) %>% 
      mutate(surfpond=surfhab_d*pondmen)
    
    i=1 #compteur de rang
    while(!i %in% menage_echelle_classe$kWh_rank & i<max(menage_echelle_classe$kWh_rank,na.rm=T)){i=i+1}
    #NB: pas de remise à 0 du compteur de rang en changeant de classe d'arrivée : excepté en VAN (boucle séparée) 
    # le classement ne dépend que de la classe DPE de départ, les ménages sont sélectionnés vers la classe la plus 
    # haute possible, pour chaque classe_arr, on reprend juste après le ménage sélectionné pour la transition supérieure. 
    
    for (arr in LETTERS[1:7]){
      # arr : classe DPE d'arrivée
      # On vérifie que la transition est une amélioration (arr "mieux" que dep)
      
      if(dep>arr){
        
        # extraction du prix au m2 de la transition en question : dep -> arr
        stock_m2_trans <-
          as.numeric(
            (Cost_m2 %>% 
               filter(classe_dep==dep) %>% 
               filter(classe_arr==arr)%>% 
               select(transition_tot_m2)
            )[1,])
        
        
        
        ##
        # BOUCLE WHILE sur surface rénovée
        ###
        sum <- 0
        # Deux conditions : 
        # 1/ on remplit les objectifs de ThreeME en volumes de m2
        # 2/ on s'assure d'avoir encore des ménages classés
        while(sum<stock_m2_trans & i<max(menage_echelle_classe$kWh_rank,na.rm=T)){
          
          sum =
            sum +
            as.numeric((menage_echelle_classe %>% filter(kWh_rank==i) %>% select(surfpond))[1,]) 
          
          # identifiant du ménage sélectionné 
          im<-as.numeric((menage_echelle_classe %>% filter(kWh_rank==i) %>% select(ident_men))[1,])
          ident_r<-c(ident_r,im)
          
          # Modification des variables REHAB et class_arr dans la base globale
          
          menage_echelle<- menage_echelle %>% 
            mutate(REHAB=ifelse(ident_men==im,TRUE,REHAB))%>%
            mutate(year_rehab=ifelse(ident_men==im,Y,year_rehab))%>%
            mutate(classe_arr=ifelse(ident_men==im,arr,classe_arr))
          
          
          
          ###
          # ITERATION
          ###
          # la non prise en compte des constructions neuves 
          # fait disparaîtres certains rangs du classement
          i=i+1
          while(!i %in% menage_echelle_classe$kWh_rank & i<max(menage_echelle_classe$kWh_rank,na.rm=T)){i=i+1}
          # Test : si jamais c'est la seconde condition de la boucle while, on atteint la fin du classement
          test=ifelse(i>=max(menage_echelle_classe$kWh_rank,na.rm=T),"max rank atteint",1)
          if(!test==1){print(paste(test," || year: ",Y,"; dep: ",dep,"; arr: ",arr,sep=""))}
        }
      }
    }
  }
  
  # supression bases superflues
  rm(menage_echelle_classe,i,sum,stock_m2_trans)
  
  
  
  
  
  # BUDGET : PRINCIPAL & SUBVENTION & ENERGIES  -----------------------------
  
  for (dep in LETTERS[1:7]){
    # classe de départ
    
    for (arr in LETTERS[1:7]){
      # classe arrivée
      if(dep>arr){
        
        # Extraction coût de la transition au m2 (dep->arr)
        cost_m2 <-
          as.numeric((
            Cost_m2 %>% 
              filter(classe_dep==dep)%>% 
              filter(classe_arr==arr)%>% 
              select(cost_m2))[1,]
          )
        
        # Taux de subvention des travaux par l'Etat, identique selon les transitions
        subvention_rate<-as.numeric(
          ThreeME %>% 
            filter(Var==paste("R_SUB_H01_C",dep,"_C",arr,sep="")) %>%
            filter(year==Y) %>%
            select(value)
        )
        # print(paste(Y," - subvention rate : ",subvention_rate,sep=""))
        
        # Coefficient de gain énergétique 
        rate_gain_ener<-as.numeric(
          Mat_gain_ener %>% 
            filter(DPE_before==dep) %>% 
            filter(DPE_after==arr) %>% 
            select(value))
        
        if(Y==horizon){rate_gain_ener=rate_gain_ener/2}         
        # on moyenne les économies d'énergies pour tenir des rénovations tout au long de l'année.
        
        ## MODIFICATION DES BUDGETS
        if(dim(menage_echelle %>% filter(year_rehab==Y & DPE_dep==dep & classe_arr==arr) %>% select(ident_men))[1]>0)
        {
          menage_echelle <- 
            menage_echelle %>% 
            mutate_when(
              # Condition
              year_rehab==Y &
                DPE_dep==dep & 
                classe_arr==arr,
              # Action
              list(
                principal_dette=cost_m2*surfhab_d*(1-subvention_rate),
                subvention=cost_m2*surfhab_d*subvention_rate,
                
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
                Elec_clim=Elec_clim*(1+rate_gain_ener),
                Elec_ElecSpe=Elec_ElecSpe*(1+rate_gain_ener),
                Elec_ecl=Elec_ecl*(1+rate_gain_ener),
                Elec_Cuisson=Elec_Cuisson*(1+rate_gain_ener),
                Gaz_Cuisson=Gaz_Cuisson*(1+rate_gain_ener),
                GPL_Cuisson=GPL_Cuisson*(1+rate_gain_ener),
                Fuel_Cuisson=Fuel_Cuisson*(1+rate_gain_ener),
                Solides_Cuisson=Solides_Cuisson*(1+rate_gain_ener),
                Urbain_Cuisson= Urbain_Cuisson*(1+rate_gain_ener)   
              ))
        }
      }
    }
  }
  
  
  
  
  
  # AGREGATS RENOVATION LOCATAIRES ------------------------------------------
  
  # Calcul de la somme du montant des travaux avant annulation des ménages loctaires
  # (qui ne le paient pas directement)
  # NB: on annule le montant du principal mais pas la subvention qui sert à calculer la subvention de l'Etat plus bas. 
  
  Dette_bailleur_prive <-
    menage_echelle %>%
    filter(year_rehab==Y) %>%
    filter(stalog>=4 & stalog<=5) %>% #stalog 4 ou 5 (Locataires / sous-locataires, co-locataire)
    filter(propri==5 || propri==6) %>% #propriétaire = membre de la famille ou un autre particulier
    summarise(sum(principal_dette*pondmen))
  
  Cout_bailleur_public <-as.numeric(
    menage_echelle %>%
      filter(year_rehab==horizon) %>%
      filter(stalog>=4 & stalog<=5) %>%
      filter(propri==2) %>%
      summarise(sum((principal_dette)*pondmen))) 
  
  
  
  
  
  # HAUSSE DE LOYERS : BAILLEURS PRIVES -------------------------------------
  
  
  #Gestion des hausses de loyer
  tot_rev504<-as.numeric(menage_echelle %>% summarise(sum(rev504*(FC$revpat)*pondmen)))
  
  #solde_dette_BP_BP solde dette bailleurs privés
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(!is.na(rev504) & rev504>0, 
                list(solde_dette_BP=rev504*(FC$revpat)/tot_rev504*(
                  as.numeric(Dette_bailleur_prive))))
  
  sauv_int_vent<-menage_echelle
  
  if(!Dette_bailleur_prive[1]==0){ 
    # cond. pas de bailleurs privés payant une rénovation
    for (im in (menage_echelle %>% filter(!is.na(rev504) & rev504>0))$ident_men){ 
      #on parcourt tous les propriétaires bailleurs
      
      menage_echelle<-
        menage_echelle %>%
        mutate_when(ident_men==im,list(hausse_loyer=amort.period(Loan=solde_dette_BP,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),i=R_I_REHAB_H01_CG_2,pf=1)[2]))
      # la hausse de loyers prend en compte le paiement des intérêts et le remboursement du principal
      # la hausse de loyers, hausse de revenu du ménage propriétaire se somme aux hausses des années précédentes
      # Hyp : tous les locataires ont changé sur 15 ans pour justifier une hausse de loyer (interdicition d'augmenter les loyers d'un même locataire sous motif de rénovation énergétique)
      menage_echelle<-menage_echelle %>%
        mutate_when(ident_men==im,
                    list(hausse_loyer_sum=hausse_loyer_sum+hausse_loyer,
                         solde_int =  solde_int+as.numeric(int_princ(loan=solde_dette_BP,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),year_purchase=Y,horizon=horizon,i=R_I_REHAB_H01_CG_2,pf=1)[1]),
                         solde_princ = solde_princ+as.numeric(int_princ(loan=solde_dette_BP,n=as.numeric(1/R_RMBS_NEWBUIL_H01_CA),year_purchase=Y,horizon=horizon,i=R_I_REHAB_H01_CG_2,pf=1)[2])))
      
    }
    
    # répartition des hausses de loyer sur les ménages rénovant
    Hausses_loyer <- menage_echelle %>% summarise(sum(pondmen*hausse_loyer))
    if(Hausses_loyer>0){
      menage_echelle <- 
        menage_echelle %>%
        mutate_when(is.na(propri),list(propri=999999)) %>%
        mutate_when(REHAB & stalog>=4 & stalog<=5 & propri<=6 & propri>=5, 
                    list(solde_loyer=solde_loyer+as.numeric(Hausses_loyer/Dette_bailleur_prive*principal_dette)))
    }
    # la hausse de loyer répercutée sur les ménages vaut le montant des travaux (principal_dette) au pro-rata de la hausse des revenus locatif des bailleurs par rapport au montant réel des travaux. 
    # Produit en croix Hausse_loyer_locataire/montant travaux locataire=Hausse_loyer_agrégée_bailleur/montant_agrégé_travaux_bailleurs
  }
  
  
  
  
  
  
  # Cas_particulier : LOCATAIRES --------------------------------------------
  
  #On utilise une dernière fois le principal_dette des locataires pour la répartition des hausses de loyer, ensuite on le met à zéro pour calculer 
  # les solde_intérêt et solde_remboursement du principal pour les ménages propriétaires. 
  
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(year_rehab==Y & stalog>=4, list(principal_dette=0))
  
  
  
  
  
  # SOLDE_INT & SOLDE_PRINC -------------------------------------------------
  
  
  # Remboursement emprunt pendant 25 ans, Quand l'horizon est à 2035, les rénovations 2010 sont déjà remboursées, pas de solde_int et solde_princ
  
  if((horizon-Y)<25){
    
    #int_price importé depuis Repayment.R, fonction maison
    # Adaptation de la fonction amort.period du package FinancialMath permettant de renvoyer un vecteur comportant le montant payé à l'année n dédié au remboursement du principal et celui des intérêts. 
    # solde_int représente le montant d'intérêt payé à l'horizon, on somme les intérêts de toutes les rénovations successives.
    
    menage_echelle$solde_int <-menage_echelle$solde_int+sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X, 
                                                                                                                                n=1/  R_RMBS_NEWBUIL_H01_CA,
                                                                                                                                year_purchase = Y,
                                                                                                                                horizon=horizon,
                                                                                                                                i=  R_I_REHAB_H01_CG_2  ,
                                                                                                                                pf=1)[1]))
    menage_echelle$solde_princ<-menage_echelle$solde_princ+sapply(menage_echelle$principal_dette, function(X) as.numeric(int_princ(loan=X,
                                                                                                                                   n=1/  R_RMBS_NEWBUIL_H01_CA,
                                                                                                                                   year_purchase = Y,
                                                                                                                                   horizon=horizon,
                                                                                                                                   i=  R_I_REHAB_H01_CG_2  ,
                                                                                                                                   pf=1
    )[2]))
    
  }
  
  
  
  
  
  # SOLVABILITE ex-post -----------------------------------------------------
  
  # calcul solvabilité
  menage_echelle <- 
    menage_echelle %>%
    mutate(solv=(c13511+c13221+c13211+solde_int+solde_princ)/RDB)
  
  # Les ménages insolvables après la transition sont écartés
  menages_insolvables <- menage_echelle %>% filter(REHAB & solv>0.33 & stalog<=2)%>%select(ident_men)
  sauv_menages_insolvables <- sauv_menage_echelle_annee_precedente %>% filter(ident_men %in%menages_insolvables$ident_men)
  
  menages_insolvables_suppr <- c(menages_insolvables_suppr, menage_echelle %>% filter(REHAB & solv>0.33 & stalog<=2)%>%select(year_rehab, ident_men))
  
  # les ménages insolvables sont "rebootés" à l'itération précédente, avant leur rénovation
  if(dim(menages_insolvables)[1]>0){
    menage_echelle <-
      rbind(menage_echelle %>%
              filter(!ident_men %in% menages_insolvables$ident_men),
            sauv_menages_insolvables)%>%
      arrange(ident_men)
  }
  
  
  
  
  # MEMOIRE REHAB -----------------------------------------------------------
  
  # Mémoire des rénovations de chaque ménage
  menage_echelle <- menage_echelle %>% mutate(count_rehab=ifelse(year_rehab==Y,paste(count_rehab,Y,sep="_"),count_rehab))
  
  
  # pour analyse ex-post, mémoire des statistiques des ménages après chaque round de rénovation 
  # Toutes les rénovations dans ident_rehab
  
  ident_rehab<-rbind(ident_rehab, 
                     menage_echelle %>% 
                       filter(year_rehab==Y)%>%
                       select(ident_men,
                              DPE_dep,
                              classe_arr,
                              year_rehab,
                              solde_int, 
                              solde_princ,
                              principal_dette))
  ident_rehab<-ident_rehab %>% dplyr::arrange(ident_men)
  #par exemple ménage 3851 rénove deux fois en AMS 2035 forfait Optimiste. En 2020 de E à D, en 2032 de D à C
  # Les solde_int et solde_princ sont additif, pas le principal_dette. 
  # ident_men DPE_dep classe_arr year_rehab solde_int solde_princ principal_dette
  # 1      3851 E       D                2020     4047.       8643.          16669.
  # 2      3851 D       C                2032     7889.      11216.           6108.
  
  # annuités sur RDB
  # Endettement aggrégé des ménages rénovant année après année (propriétaires only)
  table_solv_year<-rbind(table_solv_year,
                         c(Y,
                           as.numeric(menage_echelle %>% mutate(solv_add=(solde_int+solde_princ)/RDB)%>%filter(principal_dette>0)%>%filter(stalog<=2)%>%summarise(weighted.mean(solv_add,w=pondmen)))))
  
  # Solvabilité des ménages rénovant chaque année (obj : vérifier la solvavilité des ménages rénovation après rénovation)
  table_solv_year_ind<-rbind(table_solv_year_ind,
                             menage_echelle %>% 
                               filter(year_rehab==Y & stalog<=2)%>%
                               select(year_rehab,
                                      ident_men,
                                      solv,
                                      solde_int,
                                      solde_princ,
                                      RDB,
                                      DPE_dep,
                                      classe_arr))
  
  
  
  
  
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
  
  
  
  # UPDATE : energie surfacique (kWh) ---------------------------------------
  
  # on calcule l'énergie surfacique et énergie domestique en volume par les variables "Elec","Gaz", qui sont màj à chaque itération. 
  menage_ener_dom<-energie_dom_surf(menage_echelle)
  menage_echelle<- 
    menage_echelle %>%
    select(-ener_dom_surf,-ener_dom) %>%
    left_join(menage_ener_dom,by="ident_men")
  
  
  
  # REMISE A ZERO -----------------------------------------------------------
  
  # remise à 0 de principal_dette et des classes DPE 
  # sauf à l'horizon, où le principal dette rentre dans le BTP A05
  #pour la rénovation de l'année suivante, les ménages doivent partir de leur nouvelle classe DPE.
  
  if(!Y==horizon){
    menage_echelle<-
      menage_echelle %>% 
      mutate(DPE_dep=classe_arr)%>% 
      mutate(principal_dette=0)%>%
      mutate(subvention=0)}
  
  
  #FIN BOUCLE sur Y 
}

####
####
####


# POST-TRAITEMENT BUDGET ---------------------------------------------------------


sauv_int<-menage_echelle  

menage_echelle<-
  menage_echelle %>% 
  mutate(
    dep_Elec=dep_Elec_verif,
    dep_Gaz=dep_Gaz_verif,
    dep_GPL=dep_GPL_verif,
    dep_Fuel=dep_Fuel_verif,
    dep_Solides=dep_Solides_verif,
    dep_Urbain=dep_Urbain_verif)



menage_echelle <- 
  menage_echelle %>%
  mutate(autres_services=autres_services+solde_int,
         Hors_budget=Hors_budget+solde_princ,
         loyers=loyers+solde_loyer,
         rev_patrimoine=rev_patrimoine+hausse_loyer_sum,
         rev504=rev504+hausse_loyer_sum)

# NB :  On ne reventile pas solde_princ (le principal: en effet ce n'est pas une vraie dépense mais une utilisation de l'épargne. 


solde<- 
  menage_echelle %>%
  mutate(solde=
           solde_ener+  #<0 => économie
           solde_int+ #>0
           solde_loyer+ #>0
           -hausse_loyer_sum) %>% #<0 => comme une économie 
  select(ident_men,solde)



# BUDGETS à l'HORIZON -----------------------------------------------------

# Budget travaux horizon pour propriétaires occupants
#à l'horizon, le BTP est d'ores et déjà augmenté du montant des travaux
menage_echelle<-
  menage_echelle %>%
  mutate_when(year_rehab==horizon & stalog<3, 
              list(BTP=BTP+principal_dette))


# Budget travaux horizon pour propriétaires bailleurs
for (im in (menage_echelle %>% filter(!is.na(rev504) & rev504>0))$ident_men){
  menage_echelle<-
    menage_echelle %>%
    mutate_when(ident_men==im,list(BTP=BTP+solde_dette_BP ))
  #coût des rénovations à l'horizon pour les propriétaires bailleurs. 
  # NB : si pas de rénovation par des bailleurs privés à l'horizon, solde_dette_BP=0
}






# SUBVENTION --------------------------------------------------------------

# on ne compte pas la subvention car déjà prise en compte dans TVA (SUBVENTION)
# pour les propriétaires, pour les locataires (on compte la subvention via les locataires même si ce sont les propriétaires bailleurs qui la touchent)
Subvention<-
  as.numeric(
    menage_echelle %>%
      filter(year_rehab==horizon) %>%
      summarise(sum(subvention*pondmen)))


# ce taux est une baisse de TVA
# les investissements des offices HLM sont dans la colonne "I" et sont soumis au prix pI (non différencié par agent), 
#le prix total doit donc prendre en compte tous les remises de TVA pour tous les agents (et pas seulement privés)
sBCE<-as.numeric(Subvention/(Subvention+menage_echelle%>%summarise(sum(pondmen*BTP))))

save(sBCE,file=MatisseFiles$sBCE_rd)
save(Subvention,file=MatisseFiles$sub_rehab_rd)
save(Cout_bailleur_public,file=MatisseFiles$cout_baill_pub_rd)
save(menage_echelle,file=MatisseFiles$menage_echelle_33_pre_revent_rd)

# REVENTILATION -----------------------------------------------------------

# Pas de ventilation sur les postes énergétiques 
# Que ce soit pour les ménages rénovants ou les propriétaires bailleurs
# ThreeMe prend en compte les effets rebonds pour les budgets post renovation énergétique (solde_ener+solde_int) => ThreeME est aveugle sur les transferts entre ménage, difficile à dire si les effets rebonds concernent aussi ces soldes. Solution (22/06/2020) => on considère que les effets rebonds macro prennent déjà en compte ces effets
# 

sauv_avant_reventil<-menage_echelle
menage_echelle_33 <- Ventil_solde(solde,menage_echelle,step="REHAB")

###
# Test
###
Solde_Ener_tot<-
  rbind(Solde_Ener_tot,
        c(Y,
          menage_echelle %>%
            filter(REHAB) %>%
            filter(stalog>=4 & stalog<=5) %>%
            filter(propri==2) %>%
            summarise(sum(solde_ener*pondmen))))



# UPADTE Energie Surfacique (kWh) -----------------------------------------
menage_ener_dom<-energie_dom_surf(menage_echelle)
menage_echelle_33<- 
  menage_echelle_33 %>%
  select(-ener_dom_surf,-ener_dom) %>%
  left_join(menage_ener_dom,by="ident_men")





# SAVE --------------------------------------------------------------------


menage_echelle <- menage_echelle %>% mutate(DPE_horizon=classe_arr)

inter<-intersect(colnames(menage_echelle_33), colnames(menage_echelle_32))
not_inter<-setdiff(colnames(menage_echelle_33), colnames(menage_echelle_32))

menage_echelle_33<-menage_echelle %>% select(inter,year_rehab,DPE_horizon,solv)
save(menage_echelle_33, file=MatisseFiles$menage_echelle_33_rd)



# Clean -------------------------------------------------------------------
suppressWarnings(rm(coeff_dep_ems,coeff_ems_2010,conso_moy_dep,Cost_m2,Dette_bailleur_prive,FC,Hausses_loyer,ident_rehab,
                    Mat_gain_ener,menage_echelle,menage_echelle_32,menage_echelle_33,menage_ener_dom,menages_insolvables,menages_insolvables_suppr,
                    order,sauv_avant_reventil,sauv_int,sauv_int_vent,sauv_menage_echelle_annee_precedente,sauv_menages_insolvables,
                    solde,Solde_Ener_tot, table_order_value,table_solv_year,table_solv_year_ind,ThreeME))
gc()




# EPARGNE & PARTS BUDGETAIRES ---------------------------------------------
# 
# 
# print(compute_share_export(menage_echelle_33))
# print(compute_savings_rate_export(menage_echelle_33))
# 

# SUCCESS -----------------------------------------------------------------

# 
# print("3_1_3_Rehab_2010_horizon : SUCCESS")
# 







# VERIF -------------------------------------------------------------------
# 
# # Augmentation des loyers en 2010 et 2024 ? 
# # 2010
# weighted.mean(menage_echelle_32$rev_patrimoine,menage_echelle_32$pondmen)
# # 2014
# weighted.mean(menage_echelle_33$rev_patrimoine,menage_echelle_33$pondmen)
# # %
# weighted.mean(menage_echelle_32$rev_patrimoine,menage_echelle_32$pondmen)/
# weighted.mean(menage_echelle_33$rev_patrimoine,menage_echelle_33$pondmen)
# 
# # ou
# (weighted.mean(menage_echelle_33$rev_patrimoine,menage_echelle_33$pondmen)-
#   weighted.mean(menage_echelle_32$rev_patrimoine,menage_echelle_32$pondmen)) /
#   weighted.mean(menage_echelle_32$rev_patrimoine,menage_echelle_32$pondmen)
# # +3% sur les loyers à cause de la rénovation énergétique sur la période 2010-2024



# ENERGIE -----------------------------------------------------------------
# 
# table(abs(menage_echelle_33$dep_Elec-menage_echelle_33$dep_Elec_verif)<10^(-10))
# table(abs(menage_echelle_33$dep_Gaz-menage_echelle_33$dep_Gaz_verif)<10^(-10))
# table(abs(menage_echelle_33$dep_GPL-menage_echelle_33$dep_GPL_verif)<10^(-10))
# table(abs(menage_echelle_33$dep_Fuel-menage_echelle_33$dep_Fuel_verif)<10^(-10))
# table(abs(menage_echelle_33$dep_Solides-menage_echelle_33$dep_Solides_verif)<10^(-10))
# 
# 


# TESTS -------------------------------------------------------------------

# table(menage_echelle$year_rehab)
# 
# # Devrait être vide
# menage_echelle %>% filter(year_neuf>0 & year_rehab>0) %>% select(ident_men)
#                                
# 



# RENOVATION MULTIPLES MENAGES --------------------------------------------


# table(menage_echelle %>%group_by(ident_men)%>% filter(year_rehab>0)%>%summarise(nchar(count_rehab))%>%ungroup()%>%select(-ident_men))



# Analyse ex-post SOLV ----------------------------------------------------

# Question : à combien limiter la solvabilité ex-ante en sélection des ménages pour éviter que ceux ci ne soient sur-endettés à la fin ?

#ménages surdenttés
# menage_echelle %>% filter(ident_men %in% ident_rehab$ident_men)%>%filter(stalog<=2)%>%filter(solv>=0.33 & solv<999)%>%select(ident_men,year_rehab,count_rehab,solv,count_rehab,RDB,solde_int, solde_princ) #=> 0 après traitement (voir proposition plus bas)


#Moyenne année après année de l'ajout d'endettement pour les ménages proprio rénovant
# table_solv_year
# mean(table_solv_year[-1,2]) #=> 0.03229769


# TESTER Rénovation successives des ménages
# table_solv_year_ind %>% filter(ident_men==1542)
# table_solv_year_ind %>% filter(ident_men==6494)
# table_solv_year_ind %>% filter(ident_men==8946)


# Proposition (13/07/2020)
# 1/ la somme des ajouts d'endettement pour les ménages rénovant s'élève en moyenne à 3.3%. Limiter la rénovation aux ménages dont la solvabilité ex-ante ne dépasse pas 0.33-0.033=0.297
# 2/ si des ménages dépassent le seuil d'endettement, leur enlever la rénovation. La repondération corrigera le déficit de m2 de la transition. (concerne 12 ménages)
# => on les corrige au fur et à mesure, en reprenant le vecteur ménage de l'itération (année) précédente 
# Le but c'est aussi de pouvoir se laisser de la souplesse pour créer des différences quand on modélisera des prêts à taux zéro et des garanties d'état pour les ménages insolvables. 

####
# Question : est-ce que les ménages insolvables supprimés de la liste sont les mêmes tous les ans ? 
# menages_insolvables_suppr
