microsimulation_depenses<-function(menage_echelle,menage_forme,FC){
  
  # calcul des parts budgétaires
  
  list_dep=c("agriculture",
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
             "loyers",
             "veh_occasion"
  )
  menage_echelle<- menage_echelle %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
  menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])
  
  
  for (i in 1:14){
    k=list_dep[i]
    print(k)
    if(i<10){
      menage_echelle[paste("share_A0",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis
  }else{ 
      menage_echelle[paste("share_A",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis
    }
  }
  
  
  
  # # Vérif
  # table(rowSums(menage_echelle %>% select(starts_with("share_A"))))
  
  
  # Calcul taux de croissance du RDB pour chaque ménage
  menage_echelle$TC_RDB_nominal <- (menage_echelle$RDB - menage_forme$RDB)/menage_forme$RDB
  
  ### test
  1+as.numeric(menage_echelle %>% summarise(weighted.mean(TC_RDB_nominal,na.rm=T)))
  # [1]  2.402514
  # FC$rdb
  # [1] 2.375752
  # CCL : cohérent
  ####
  
  
  # Le passage du revenu nominal au revenu reel est assuré par le déflateur de Stone, produit des prix à la puissance des parts budgétaires
  # on calcule le déflateur de stone par ménage => on ne peut donc pas considérer les dépenses de construction neuve ou les achats de véhicules d'occasion 
  # à des professionels (traitement agrégé). 
  # revenu reel 2010 = revenu nominal 2010
  menage_echelle$IP_stone <-  1
  menage_echelle$IP_stone <-          
    FC$A01**menage_echelle$share_A01 *
    FC$A02**menage_echelle$share_A02*
    FC$A03**menage_echelle$share_A03*
    FC$A04**menage_echelle$share_A04*
    FC$A05**menage_echelle$share_A05*
    FC$A06**menage_echelle$share_A06*
    FC$A07**menage_echelle$share_A07*
    FC$A08**menage_echelle$share_A08*
    FC$A09**menage_echelle$share_A09*
    FC$A10**menage_echelle$share_A10*
    FC$A11**menage_echelle$share_A11*
    FC$A12**menage_echelle$share_A12*
    FC$A13**menage_echelle$share_A13*
    FC$A14**menage_echelle$share_A14
  
  #### test
  # menage_echelle %>% summarise(weighted.mean(IP_stone,w=pondmen)) #1.817183 (AMS, 2035, Opt, forfait)
  #IMACLIM AMS 2035 1.775
  ####
  
  menage_echelle$RDB_reel <-  menage_echelle$RDB/menage_echelle$IP_stone
  menage_echelle <-
    menage_echelle %>%
    mutate_when(is.na(RDB_reel),list(RDB_reel=0))
  
  menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_forme$RDB)/menage_forme$RDB
  
  # menage_echelle %>% filter(TC_RDB_reel<0)%>%select(ident_men, TC_RDB_reel,TC_RDB_nominal,RDBAI,RDB)
  # menage_echelle %>% filter(TC_RDB_nominal<as.numeric(min(FC %>% select(starts_with("rev"))))-1)%>%select(ident_men, TC_RDB_reel,TC_RDB_nominal,RDBAI,RDB)
  # ident_men_TC_RDB<-menage_echelle %>% filter(TC_RDB_nominal<as.numeric(min(FC %>% select(starts_with("rev"))))-1)%>%select(ident_men)
  # avec les hypothèses du modèle, personne ne gagne jamais moins à l'horizon qu'en 2010
  # => les ménages avec des TC_rdb bizarres => année exceptionnels, on ne peut pas considérer que ce fut la tendance pendant 15 à 25 ans.
  # => pour rentrer dans les clous => s'ils ont une année exceptionnelle, leur conso croit en tendance depuis 2010 comme le rdb macro déflaté de leur IP_stone. Comme si leurs revenus à prix courant avaient progressés comme le revenu macro. 
  min_TC_rev<-min(FC %>% select(starts_with("rev")))-1
  menage_echelle <-
    menage_echelle %>% 
    mutate(TC_RDB_reel=ifelse(TC_RDB_nominal<=min_TC_rev,FC$rdb/IP_stone-1,TC_RDB_reel))
  
  # Mise échelle dep --------------------------------------------------------
  
  iter=TRUE #Pour la première itération
  nb_iter_RDB=0
  

  while(iter & nb_iter_RDB<61){
    sauv_menage_echelle<-menage_echelle
    nb_iter_RDB=nb_iter_RDB+1
    print(nb_iter_RDB)
    list_dep_autres_ener=c("dep_GPL","dep_Fuel","dep_Urbain", "dep_Solides")
    
    
    list_cat=c("agriculture",
               "elec",
               "gaz_ville",
               "autres_energies_dom",
               "BTP",
               "prod_veh",
               "carb_lubr",
               "transp_rail_air",
               "transp_routes_eau",
               "loisirs_com",
               "autres_services",
               "autres",
               "loyers",
               "veh_occasion")
    
    #Traitement à part des dépenses issues de l'enquête EDF, application des élasticités correspondantes de BDF pour l'électricité, le gaz, les autres énergies
    # et les carburants.
    #formule : en notant D la dépense, P le prix, RDB le revenu brut disponible, l'apostrophe désignant la variable mise à l'échelle :
    # D'=D*(1+elast_prix*Delta_prix/prix+elast_rev*Delta_RDB/RDB) * P'/P
    # or P'/P=TC_prix
    # or Delta_prix=1+TC_prix
    # Les valeurs de TC pour les prix ne sont pas des taux de croissance mais des ratios de prix (P'/P)
    # En revanche TC_RDB est un vrai taux de croissance
    
    # A02
    menage_echelle$dep_Elec <-
      menage_forme$dep_Elec*
      (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) *
      (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*
      FC$A02
    
    # A03
    menage_echelle$dep_Gaz<-
      menage_forme$dep_Gaz*
      (1+menage_echelle$elast_prix_A03*(FC$A03/menage_echelle$IP_stone-1)) *
      (1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*
      FC$A03
    
    # A04
    menage_echelle[list_dep_autres_ener]<-
      menage_forme[list_dep_autres_ener]*
      (1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *
      (1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*
      FC$A04
    
    # Dep_logement
    menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])
    
    
    for (i in c(1,5:9)){
      A0I<-paste("A0",i,sep="")
      elast_prix<-paste("elast_prix_A0",i,sep="")
      elast_rev<-paste("elast_rev_A0",i,sep="")
      
      menage_echelle[list_cat[i]]<-
        menage_forme[list_cat[i]]*
        (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
        (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
    }
    
    for (i in c(10:14)){
      A0I<-paste("A",i,sep="")
      elast_prix<-paste("elast_prix_A",i,sep="")
      elast_rev<-paste("elast_rev_A",i,sep="")
      
      menage_echelle[list_cat[i]]<-
        menage_forme[list_cat[i]]*
        (1+menage_echelle[elast_prix]*(-1+as.numeric(FC[A0I])/menage_echelle$IP_stone))*
        (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
    }
    
    
    
    # # #Traitement des hors budgets :
    # # #pas d'élasticité prix, élasticité revenu =1 par hypothèse, taux de croissance du rdb,
    # menage_echelle$Hors_budget<-
    #   menage_forme$Hors_budget*(1 + menage_echelle$TC_RDB_nominal)
    
    # #pas besoin de le mettre à jour ici !!!!
    # menage_echelle$rev801 <-
    #   menage_forme$rev801*
    #   (1+menage_echelle$elast_prix_A13*(-1+as.numeric(FC$A13)/menage_echelle$IP_stone))*
    #   (1+menage_echelle$elast_rev_A13*menage_echelle$TC_RDB_reel)*as.numeric(FC$A13)
    #
    
    
    list_dep=c("agriculture",
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
               "loyers",
               "veh_occasion"
    )
    
    
    menage_echelle<- menage_echelle %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
    menage_echelle$Rcons_bis <- rowSums(menage_echelle[list_dep])
    ## Attention on n'a pas rajouté les constructions neuves à l'horizon, elles ne sont pas encore attribuées
    
    
    for (i in 1:14){
      k=list_dep[i]
      if(i<10){
        menage_echelle[paste("share_A0",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
      else{menage_echelle[paste("share_A",i,sep="")]<-menage_echelle[k]/menage_echelle$Rcons_bis}
    }
    
    
    #test
    # print(table(rowSums(menage_echelle %>% select(starts_with("share_A")))))
    
    # revenu reel 2010 = revenu nominal 2010
    menage_echelle$IP_stone <-
      FC$A01**menage_echelle$share_A01*
      FC$A02**menage_echelle$share_A02*
      FC$A03**menage_echelle$share_A03*
      FC$A04**menage_echelle$share_A04*
      FC$A05**menage_echelle$share_A05*
      FC$A06**menage_echelle$share_A06*
      FC$A07**menage_echelle$share_A07*
      FC$A08**menage_echelle$share_A08*
      FC$A09**menage_echelle$share_A09*
      FC$A10**menage_echelle$share_A10*
      FC$A11**menage_echelle$share_A11*
      FC$A12**menage_echelle$share_A12*
      FC$A13**menage_echelle$share_A13*
      FC$A14**menage_echelle$share_A14
    
    menage_echelle$RDB_reel <-  menage_echelle$RDB/menage_echelle$IP_stone
    menage_echelle <-
      menage_echelle %>%
      mutate_when(is.na(RDB_reel),list(RDB_reel=0))
    
    menage_echelle$TC_RDB_reel <- (menage_echelle$RDB_reel-menage_forme$RDB)/menage_forme$RDB
    
    menage_echelle <-
      menage_echelle %>%
      mutate(TC_RDB_reel=ifelse(TC_RDB_nominal<=min_TC_rev,FC$rdb/IP_stone-1,TC_RDB_reel))
    
    
    tol=abs((menage_echelle$RDB_reel-sauv_menage_echelle$RDB_reel)/sauv_menage_echelle$RDB_reel)
    max(tol,na.rm=T)
    # indice du max
    # i_max<-which.max(tol)
    # (menage_echelle %>% select(ident_men))[i_max,]
    print(max(tol,na.rm=T))
    if(max(tol,na.rm=T)>10^-3){iter=TRUE} else {iter=FALSE}
    
    # TESTER PLUS => 10^-6
    ###
    ###
    ###
    
  }
  
  
  # en sortie de boucle, on utilise le dernier IP_stone, celui qui permet la convergence du RDB_reel pour calculer les dépenses maj
  
  # A02
  menage_echelle$dep_Elec <-
    menage_forme$dep_Elec*
    (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * 
    (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*
    FC$A02
  
  # A03
  menage_echelle$dep_Gaz<-
    menage_forme$dep_Gaz*
    (1+menage_echelle$elast_prix_A03*(FC$A03/menage_echelle$IP_stone-1)) *
    (1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*
    FC$A03
  
  # A04
  menage_echelle[list_dep_autres_ener]<-menage_forme[list_dep_autres_ener]*
    (1+menage_echelle$elast_prix_A04*(FC$A04/menage_echelle$IP_stone-1)) *
    (1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*
    FC$A04
  
  # Dep_logement
  menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])
  
  
  for (i in c(1,5:9)){
    A0I<-paste("A0",i,sep="")
    elast_prix<-paste("elast_prix_A0",i,sep="")
    elast_rev<-paste("elast_rev_A0",i,sep="")
    
    menage_echelle[list_cat[i]]<- 
      menage_forme[list_cat[i]]*
      (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
      (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
  }
  
  for (i in c(10:14)){
    A0I<-paste("A",i,sep="")
    elast_prix<-paste("elast_prix_A",i,sep="")
    elast_rev<-paste("elast_rev_A",i,sep="")
    
    menage_echelle[list_cat[i]]<- 
      menage_forme[list_cat[i]]*
      (1+menage_echelle[elast_prix]*(as.numeric(FC[A0I])/menage_echelle$IP_stone-1))*
      (1+menage_echelle[elast_rev]*menage_echelle$TC_RDB_reel)*as.numeric(FC[A0I])
  }
  
  # rev801, loyers imputés sont mis à jour en utilisant la même élasticité que le secteur 13 des loyers
  menage_echelle$rev801 <- 
    menage_forme$rev801*
    (1+menage_echelle$elast_prix_A13*(as.numeric(FC$A13)/menage_echelle$IP_stone-1))*
    (1+menage_echelle$elast_rev_A13*menage_echelle$TC_RDB_reel)*as.numeric(FC$A13)
  
  
  # les ventes de veh suivent le neuf (plutôt que le rdb) => rev850 issus de ventes de veh par les ménages suit donc le secteur A06
  menage_echelle$rev850 <- 
    menage_forme$rev850*
    (1+menage_echelle$elast_prix_A06*(-1+as.numeric(FC$A06)/menage_echelle$IP_stone))*
    (1+menage_echelle$elast_rev_A06*menage_echelle$TC_RDB_reel)*as.numeric(FC$A06)
  
  
  return(menage_echelle)
  
}