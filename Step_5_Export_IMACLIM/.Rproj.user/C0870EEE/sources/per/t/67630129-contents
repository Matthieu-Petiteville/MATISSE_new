# Three functions to compute the budget shares, saving rate and energy mix for the 2010 BDF
# RDB_exp et Rcons_exp se distinguent de RDB et Rcons des bases de données puisqu'elles sont corrigées pour coller aux définitions macro-économiques sans être nécessairement porteuse de sens au niveau micro (e.g. la prise en compte des loyers imputés dans l'épargne de chaque ménage ne fait pas sens mais pour un ménage représentatif qui englobe les transferts entre ménage au niveau macro ça l'est)


library(tidyverse)
library(readxl)
source("D:/Stage_Petiteville/Projet_Ademe/Code_global_ADEME/mutate_when.R")

# TAUX d'EPARGNE ----------------------------------------------------------



compute_savings_rate_export<-function(menage){
  
  menage$AID[is.na(menage$AID)]<-0
  menage$rev801[is.na(menage$rev801)]<-0
  menage$impot_revenu[is.na(menage$impot_revenu)]<-0  
  menage$c13711[is.na(menage$c13711)]<-0
  
  # Inclut les loyers imputés, au niveau macro on compte les loyers imputés comme du revenu et des dépenses puisqu'il s'agit de transferts entre ménages,
  # on compte aussi les revenus exceptionnels autres que les transferts entre ménages
  menage$RDB_exp <- rowSums(menage[c("rev_activites","rev_patrimoine","rev_sociaux","rev_TCO","rev801","rev60x","rev999")])-rowSums(menage[c("impot_revenu","AID")])
  
  
  #=> on prend en compte les loyers imputés, 1/ pris en compte dans les parts budgétaires qu'on remonte à la macro 2/ pris en compte dans l'épargne macro
  #	§ REV601 Gains aux jeux de hasard 
  # § REV602 Sommes versées par une compagnie
  # § d’assurance
  # § REV603 Dommages et intérêts 
  # § REV604 Indemnités de licenciement, primes de
  # § départ
  # § REV605 Prime à l’amélioration de l’habitat X
  # § REV606 Déblocage de participation, vente d’actions,
  # § d’obligations
  # § REV699 Autres ressources exceptionnelles
  
   # On inclut les dépenses de construction neuves dans les budgets
  menage <-
    menage %>%
    mutate(loyers=loyers+rev801)
  
  # On n'inclut pas le Hors Budget ni les véhicules d'occasion.
  menage$Rcons_exp<-rowSums(menage[c("agriculture",
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
                                     "loyers")])
  
  Rcons_exp_tot <- as.numeric(menage %>% summarise(sum(pondmen*Rcons_exp)))
  
  # Achats immobiliers à des professionnels : 
  ###
    # Au niveau agrégé : il s'agit de ventiler les achats de logement (c13711) entre les achats à des particuliers (qui se retrouvent sous forme de revenu chez certains ménages : rev800) et les achats à des professionnels (qui sont présents dans la comptabilité nationale)
  #c13711 a été maj en step_2 Microsimulation : c13711*FC$A05 * FC$surface
  # rev800 idem : rev800*FC$A05 * FC$surface
	Achats_immobiliers_pro = menage %>% summarise(sum(pondmen*c13711,na.rm=T)) - menage %>% summarise(sum(pondmen*rev800,na.rm=T))
  
	# Achats de véhicules à des professionnels
	###  
	  # Au niveau agrégé : on ventile les achats de véhicules d'occasion (secteur 14) entre achats à des particuliers (donnant lieu à un revenu pour certains ménages : rev850) et à des professionnels (comptés dans la compta nat)
	# rev850 màj en step_2 Microsimulation comme A06 (elast_rev et elast_prix)
  Vente_VP_pro = menage %>% summarise(sum(pondmen*veh_occasion,na.rm=T)) - menage %>% summarise(sum(pondmen*rev850,na.rm=T))
  
   
  #agrégé
  RDB_exp_agreg <- menage %>% summarise("epargne"=sum(pondmen*RDB_exp))
  Rcons_exp_agreg <- Rcons_exp_tot + Achats_immobiliers_pro + Vente_VP_pro
  epargne_agreg <- RDB_exp_agreg - Rcons_exp_agreg
  taux_epargne_agreg <-  epargne_agreg/RDB_exp_agreg
    
  #Traitement 1
  # =< 15/05/2020 => adopté : somme revenus - somme depenses /somme_revenus_pas_entre_menages
  

  
  return(taux_epargne_agreg)
    
}

#NB : 
# • Il s'agit du traitement des ménages à revenus négatifs (RDB incluant les loyers imputés), ils sont 18, représentent 0.16% de la population. Et pourtant leur traitement peut faire varier le taux d'épargne agrégé en 2010 de 0.4 pts ... (en traitant bien le problème précédent des loyers imputés au dénominateur)
# ○ Traitement 1 : on considère que le taux d'épargne agrégé c'est le somme des revenus agrégés (y compris les revenus négatifs) - la somme des consommations sur la somme des revenus (y compris négatifs) => en 2010, taux d'épargne vaut 0.1186414
# 			§ Les revenus négatifs viennent diminuer la masse de revenu des ménages en agrégé



# PARTS BUDGETAIRES -------------------------------------------------------


compute_share_export<-function(menage){
  
  menage$AID[is.na(menage$AID)]<-0
  menage$rev801[is.na(menage$rev801)]<-0
  menage$impot_revenu[is.na(menage$impot_revenu)]<-0  
  menage$c13711[is.na(menage$c13711)]<-0
  
  # On inclut les dépenses de construction neuves dans les budgets travaux (A05) & les loyers imputés dans les loyers (A13)
  menage <-
    menage %>%
    mutate(loyers=loyers+rev801)%>%
    mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
  
  # On n'inclut pas le Hors Budget ni les véhicules d'occasion.
  list_dep<-c("agriculture",
              "dep_Elec",
              "dep_Gaz",
              "dep_autres_energies",
              "BTP",
              "prod_veh",
              "carb_lubr",
              "transp_rail_air",
              "transp_routes_eau",
              "loisirs_com",
              "autres_services",
              "autres",
              "loyers")
  
  menage$Rcons_exp <- rowSums(menage[list_dep])
  
  
  # Parts pour chaque ménage
  # Volumes agrégés de dépenses 
  Vol<-rep(1,13)
  col=rep(1,13)
  for (i in 1:13){
    k=list_dep[i]
    l=paste("share_A0",i,sep="")
    col[i]=l
    # print(k)
    if(i<10){
      menage[paste("share_A0",i,sep="")]<-menage[k]/menage$Rcons_exp
      Vol[i]<-as.numeric(menage %>% summarise(sum(get(k)*pondmen)))
    }
    else{ menage[paste("share_A",i,sep="")]<-menage[k]/menage$Rcons_exp
    Vol[i]<-as.numeric(menage %>% summarise(sum(get(k)*pondmen)))
    l=paste("share_A",i,sep="")
    col[i]=l
    }
  }
  
  Achats_immobiliers_pro = menage %>% summarise(sum(pondmen*c13711,na.rm=T)) - menage %>% summarise(sum(pondmen*rev800,na.rm=T))
  Vente_VP_pro = menage %>% summarise(sum(pondmen*veh_occasion,na.rm=T)) - menage %>% summarise(sum(pondmen*rev850,na.rm=T))
  
  Vol[5]<-Vol[5]+as.numeric(Achats_immobiliers_pro)
  Vol[11]<-Vol[11]+as.numeric(Vente_VP_pro)
  # Pour vérif
  Vol_tot <- sum(Vol)
  # Vol[1]/Vol_tot #devra correspondre à la part agrégée des dépenses en agriculture
  
  share <- Vol/Vol_tot
  Parts=as.data.frame(rbind(share,Vol))
  colnames(Parts)=col
  return(Parts)
}

# MIX ENERGETIQUE ---------------------------------------------------------




energie_mix<-function(menage,FC){
  # pour 2010, FC=NA
  
  
  # LIBRARIES ---------------------------------------------------------------
  
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(tidyverse)
  library(readxl)
  
  # IMPORT DATA -------------------------------------------------------------
  
  setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE/")
  load("Data/Data_interne/list_source_usage.RData")
  
  # Import des prix d'énergie par classe de ménage : en €/MWh
  prix_classe <- read.csv2("Data/BDFE_delauretis/Prix_energie_par_classe.csv", header = TRUE, sep = ";",dec = ".", fill = TRUE)
  
  # PREPARATION DONNEES PRIX ENERGIE ----------------------------------------

  # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
  prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)
  
  # MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------
  
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025. 
  if(!is.na(FC[1])){
  prix_classe_horizon<-prix_classe
  
  # A02
  prix_classe_horizon$prix_elec<- prix_classe$prix_elec * as.numeric(FC$A02)
  
  # A03
  prix_classe_horizon$prix_gaz<- prix_classe$prix_gaz * as.numeric(FC$A03)
  
  # A04
  prix_classe_horizon[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]<- prix_classe[c("prix_fuel","prix_gpl","prix_bois","prix_chaleur")]* as.numeric(FC$A04)
  }
  
  
  # CLASSER MENAGES PAR CLASSE ----------------------------------------------
  
  # Matrice des prix de chaque énergie pour chaque classe
  prix_classe_mat <- data.matrix(prix_classe[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)
  
  # Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60) par croisement de MI_corr x typmen_corr x quintileuc
  menage$classe_men <- 
    with(
      menage,
      as.integer(interaction(MI_corr, typmen_corr, quintileuc)))
  
  menage$classe_men <- 
    as.factor(menage$classe_men)
  
  
  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  dummies_classe_men <- model.matrix(~ classe_men, 
                                     data = menage, 
                                     contrasts.arg = list(
                                       classe_men = contrasts(
                                         menage$classe_men,
                                         contrasts = F)
                                     ))
  dummies_classe_men <- dummies_classe_men[,-1]  #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  
  # PRIX ENERGIE PAR MENAGE -------------------------------------------------
  
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
  prix_menages_bis<-as.data.frame(prix_menages)
  
  # Rajout colonne "ident_men" pour la fusion avec det_ener
  prix_menages<-cbind(menage$ident_men,prix_menages_bis)
  # renommer les colonnes
  colnames(prix_menages)<-c("ident_men",
                                 "prix_Elec",
                                 "prix_Gaz",
                                 "prix_Fuel",
                                 "prix_GPL",
                                 "prix_Solides",
                                 "prix_Urbain")
  
  # Rajout des prix et pondération de chaque ménage dans dep_ener
  menage <- menage %>% left_join(prix_menages,by="ident_men")
 
  
  # VOLUMES ENERGIE / MENAGE ------------------------------------------------
   # Pour convertir les dépenses en volumes (kWh), division par le prix moyen en 2010 de chaque source d'énergie.
  
  menage$vol_Elec<-menage$dep_Elec/menage$prix_Elec
  menage$vol_Gaz<-menage$dep_Gaz/menage$prix_Gaz
  menage$vol_GPL<-menage$dep_GPL/menage$prix_GPL
  menage$vol_Fuel<-menage$dep_Fuel/menage$prix_Fuel
  menage$vol_Solides<-menage$dep_Solides/menage$prix_Solides
  menage$vol_Urbain<-menage$dep_Urbain/menage$prix_Urbain
  menage$vol_tot<- menage %>% select(starts_with("vol_")) %>% rowSums() 
  
  sources=c("Elec","Gaz","GPL","Fuel","Solides","Urbain")
  dep_sources=paste("dep",sources,sep="_")
  
  # VOLUMES ENERGIE MACRO-ECONOMIQUES ----------------------------------------
  menage <- menage %>% mutate(Energie_pond=vol_tot*pondmen)
  
  sum(menage$Energie_pond)
  # 420 TWh en 2010 consommés par les ménages
  
  menage$volpond_Elec<-menage$dep_Elec/menage$prix_Elec*menage$pondmen
  menage$volpond_Gaz<-menage$dep_Gaz/menage$prix_Gaz*menage$pondmen
  menage$volpond_GPL<-menage$dep_GPL/menage$prix_GPL*menage$pondmen
  menage$volpond_Fuel<-menage$dep_Fuel/menage$prix_Fuel*menage$pondmen
  menage$volpond_Solides<-menage$dep_Solides/menage$prix_Solides*menage$pondmen
  menage$volpond_Urbain<-menage$dep_Urbain/menage$prix_Urbain*menage$pondmen
  
  # Par énergie # la somme donne bien 420 TWh
  sum(menage$volpond_Elec)
  sum(menage$volpond_Gaz)
  sum(menage$volpond_GPL)
  sum(menage$volpond_Fuel)
  sum(menage$volpond_Solides)
  sum(menage$volpond_Urbain)
  
  # en parts
  sum(menage$volpond_Elec)/sum(menage$Energie_pond)
  sum(menage$volpond_Gaz)/sum(menage$Energie_pond)
  sum(menage$volpond_GPL)/sum(menage$Energie_pond)
  sum(menage$volpond_Fuel)/sum(menage$Energie_pond)
  sum(menage$volpond_Solides)/sum(menage$Energie_pond)
  sum(menage$volpond_Urbain)/sum(menage$Energie_pond)
  
  
  Mix_ener<-c(  sum(menage$volpond_Elec)/sum(menage$Energie_pond),
                sum(menage$volpond_Gaz)/sum(menage$Energie_pond),
                sum(menage$volpond_GPL)/sum(menage$Energie_pond),
                sum(menage$volpond_Fuel)/sum(menage$Energie_pond),
                sum(menage$volpond_Solides)/sum(menage$Energie_pond),
                sum(menage$volpond_Urbain)/sum(menage$Energie_pond))
  Mix_ener<-t(data.frame(Mix_ener))
  colnames(Mix_ener)<-sources
  return(Mix_ener)
  
  # #Si jamais besoin de redonner l'énergie en volume par ménage en 2010 # décommenter
  # 
  # # ENERGIE DOMESTIQUE ------------------------------------------------------
  # 
  # dep_source_usage<-menage[c("ident_men",list_source_usage)]
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]/menage$prix_Elec
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]/menage$prix_Gaz
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]/menage$prix_GPL
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]/menage$prix_Fuel
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]/menage$prix_Solides
  # 
  # dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]<-
  #   dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]/menage$prix_Urbain
  # 
  # 
  # 
  # 
  # # Energie de chauffage
  # dep_source_usage$ener_chauff<-
  #   dep_source_usage %>% select(contains("_chauff")) %>% rowSums()
  # 
  # # Energie Eau Chaude Sanitaire
  # dep_source_usage$ener_ECS<-
  #   dep_source_usage %>% select(contains("_ECS")) %>% rowSums()
  # 
  # # Energie de climatisation
  # dep_source_usage$ener_clim<-
  #   dep_source_usage %>% select(contains("_clim")) %>% rowSums()
  # 
  # 
  # # Energie domestique (chauff+ECS+clim)
  # dep_source_usage$ener_dom<-
  #   dep_source_usage$ener_chauff+
  #   dep_source_usage$ener_ECS+
  #   dep_source_usage$ener_clim
  # # sum(dep_source_usage$ener_dom)
  # 
  # # # Energie domestique surfacique
  # # dep_source_usage$ener_dom_surf<-dep_source_usage$ener_dom/menage$surfhab_d
  # # 
  # mean(dep_source_usage$ener_dom_surf) #MWh
  # # EN 2025
  # # [1] 0.08071984
  # # EN 2010
  # # > mean(dep_source_usage$ener_dom_surf) #MWh
  # # [1] 0.1145678
  # 
  # # Tracer la distribution d'énergie surfacique
  # # g<-ggplot(dep_source_usage,aes(x=dep_source_usage$ener_dom_surf*1000))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  # #   geom_density(alpha=.7, fill="#FF6666")+xlim(0,1000)
  # # g
  # 
  # 
  # 
  # 
  # 
  # # EXPORTER DONNEES ENER_DOM  ----------------------------------------------
  # 
  # # menage_2025<-dep_source_usage %>% select(ident_men,list_source_usage,ener_dom_surf)
  # # menage_2025[list_source_usage]<-menage_2025[list_source_usage]*1000
  # # menage<-
  # #   menage %>%
  # #   left_join(dep_source_usage %>% select(ident_men,ener_dom_surf),by="ident_men")
  # 
  # # dep_source_usage<<-dep_source_usage %>% mutate(ener_dom_surf=ener_dom_surf*1000) %>% select(ident_men,ener_dom_surf)
  # rm(dep_sources,menage)
  # 
  # # save(menage_2025,file="2025/menage_2025.RData")
  # 

}



compute_evol_energie<-function(menage,s,h,sc,r,Iter){
  #comme compute_share_export, renvoie des parts pour 13 biens et services, pour les quatre biens énergétiques (A02,A03,A04 et A07, renvoie la progression en dépenses réelles (divisée par Indice de Stone) depuis 2010)
  

# Data --------------------------------------------------------------------

  load("D:/Stage_Petiteville/Projet_Ademe/MATISSE/Step_0_Mise_forme_BDF/Output/menage_forme_4.RData")
  try(
    detach("package:plyr"), 
    silent=T
  )
# Indices de Prix ---------------------------------------------------------

  #Colonne dans Output_macro
  if(horizon==2025){X="AH"}
  if(horizon==2030){X="AI"}
  if(horizon==2035){X="AJ"}
  
  
  # # Indices de prix ---------------------------------------------------------
  if(Iter>0){
    #attention décalage avec Output_macro_code => skip first line lors de l'enregistrement
    IP_A02<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"13",sep=""),col_names = F))
    
    IP_A03<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"14",sep=""),col_names = F))
    
    IP_A04<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"15",sep=""),col_names = F))
    
    IP_A07<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),range=paste(X,"18",sep=""),col_names = F))
  }
  
  if(Iter==0){
    IP_A02<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"14",sep=""),col_names = F))
    
    IP_A03<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"15",sep=""),col_names = F))
    
    IP_A04<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"16",sep=""),col_names = F))
    
    IP_A07<-as.numeric(read_excel(paste("D:/Stage_Petiteville/Projet_Ademe/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),sheet=s,range=paste(X,"19",sep=""),col_names = F))
  }
  
  

# Dépenses d'énergie agrégées ---------------------------------------------

  # BDFE 2010
  energie_2010<- 
    menage_forme %>% 
    select(ident_men, pondmen,carb_lubr, dep_Elec, dep_Gaz,dep_Solides, dep_Fuel,dep_GPL,dep_Urbain,carb_lubr)%>%
    mutate(Oil_2010=dep_Fuel+dep_GPL+carb_lubr) %>% 
    mutate(Elec_2010=dep_Elec)%>%
    mutate(Gaz_2010=dep_Gaz+dep_Urbain)%>% 
    mutate(Solides_2010=dep_Solides)%>%
    select(ident_men,pondmen,Elec_2010,Gaz_2010,Oil_2010,Solides_2010) %>% 
    gather(key=ener, value=value, -c(1:2))%>%
    group_by(ener) %>% 
    summarise("Dep_energie_agg_2010"=sum(pondmen*value))

 # Horizon 
  energie_horizon<- 
    menage %>%
    select(ident_men, pondmen, carb_lubr,dep_Elec, dep_Gaz,dep_Solides, dep_Fuel,dep_GPL,dep_Urbain)%>%
    mutate(Oil=(dep_Fuel+dep_GPL)/IP_A04+carb_lubr/IP_A07) %>%
    mutate(Elec=dep_Elec/IP_A02)%>%
    mutate(Gaz=dep_Gaz/IP_A03+dep_Urbain/IP_A04)%>%
    mutate(Solides=dep_Solides/IP_A04)%>%
    select(ident_men,pondmen,Elec,Gaz,Oil,Solides) %>% 
    gather(key=ener, value=value, -c(1:2)) %>% 
    group_by(ener) %>% 
    summarise("Dep_energie_agg_horizon"=sum(pondmen*value))
  
  # Merge
  energie<-cbind(energie_horizon,energie_2010[,2])%>%mutate(evol=Dep_energie_agg_horizon/ Dep_energie_agg_2010-1)
  
return(energie%>%select(ener,evol)%>%spread(key=ener,value=evol))
  
  
}
