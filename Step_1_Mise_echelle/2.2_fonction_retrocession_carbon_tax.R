retrocession_carbon_tax <- function(TCO,TCO_tot,menage_echelle){
  
  
  try(
    detach("package:plyr"), 
    silent=T
  )
  
  #TCO par UC => TCO à redistribuer par unité de consommation en moyenne
  TCO_tot_UC<-TCO_tot/as.numeric(menage_echelle %>% summarise(sum(pondmen*coeffuc)))
  
  
  #RDB total
  RDB_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDB)))
  
  
  
  
  if(redistribution=="ssrec"){
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO=0)
  }
  
  if(redistribution=="forfait"){
    
    # total des UC sur le territoire
    sum_pond<-as.numeric(menage_echelle%>%summarise(sum(pondmen*coeffuc)))
    
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO=coeffuc/sum_pond* TCO_tot)
    #vérif
    menage_echelle %>% summarise(sum(pondmen*rev_TCO)) # = TCO_tot
    
  }
  
  if(redistribution=="niveau_vie"){ #rétrocession niveau_vie sur rev_tot=RDBAI
    
    #rétrocession niveau_vie sur rev_tot=RDBAI
    # => on est bien neutre sur le plan distribution du revenu #rétrocession neutre (et pas la réforme)
    
    #RDB*nombre d'UC dans le pays
    RDBAI_UC_tot <- as.numeric(menage_echelle %>% summarise(sum(pondmen*RDBAI/coeffuc)))
    
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO=RDBAI/coeffuc/RDBAI_UC_tot * TCO_tot)
    
    # menage_echelle %>% summarise(sum(pondmen*rev_TCO))
    # menage_echelle %>% summarise(mean(rev_TCO)) 
    
    TCO_rtcd2<-menage_echelle %>%
      group_by(decuc2)%>%
      summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
    TCO_rtcd2 #weighted mean par décile
    
    menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  }
  
  
  if(redistribution=="decile"){ 
    # detach("package:plyr")
    
    
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2)%>%
      summarise(sum(pondmen*coeffuc))
    
    Tab_dec<-Tab_dec%>%mutate(decuc2=as.numeric(decuc2)-1) #pour se rapprocher de la solution avec tuu, on décale les déciles d'un. Le décile 1, devient 0, annule la pente de la régression
    

    # Emissions agrégées par UC
    # Tab_emissions<-menage_forme_2010%>%group_by(decuc2)%>%summarise(sum(pondmen*TCO_paid/coeffuc))
    # Tab_dec[i,2]=> la somme des UC sur le décile, on exclut le décile 10. Le décile 1 va toucher une somme égale à sa somme des UC multipliée par le versement par 
    # UC du décile 1 défini ci-avant par facteur_dec1 fois TCO_tot_UC, qui est le versement moyen par UC dans toute la pop. 
    # on épuise l'ensemble de la TCO à rétrocéder (TCO_tot) d'où la racine
    x_dec= bisect(function(x) (Tab_dec[1,2]*(1-x)^0+ #décile 1
                                 Tab_dec[2,2]*(1-x)^1+
                                 Tab_dec[3,2]*(1-x)^2+
                                 Tab_dec[4,2]*(1-x)^3+
                                 Tab_dec[5,2]*(1-x)^4+
                                 Tab_dec[6,2]*(1-x)^5+
                                 Tab_dec[7,2]*(1-x)^6+
                                 Tab_dec[8,2]*(1-x)^7+
                                 Tab_dec[9,2]*(1-x)^8)* #décile 9
                    as.numeric(facteur_dec1*TCO_tot_UC)-TCO_tot,0,1)$root
    # x_dec=0.1825126
    
    #fonction de distribution avec i le decuc2 
    distrib<-function(i,coeffuc){
      i<-as.double(i)-1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i==9,return(0),return(coeffuc*((1-x_dec)**i)*as.numeric(TCO_tot_UC*facteur_dec1)))
    }
    
    
    menage_echelle<-
      menage_echelle %>%
      group_by(1:n())%>% #on groupe ménage par ménage
      mutate(rev_TCO=as.numeric(distrib(i=decuc2,coeffuc=as.numeric(coeffuc))))%>%
      ungroup()
    
    menage_echelle<-menage_echelle %>% ungroup()
    
    TCO_rtcd2<-menage_echelle %>%
      group_by(decuc2)%>%
      summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
    TCO_rtcd2 #weighted mean rev_tco par décile
    
    TCO_rtcd3<-menage_echelle %>%
      group_by(decuc2)%>%
      summarise(weighted.mean(x=rev_TCO/coeffuc,w=pondmen,na.rm=T))
    TCO_rtcd3 #weighted mean du rev_TCO par uc par décile
    # 
    menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  }
  
  
  
  if(redistribution=="tuu"){
   
    #en tuu on exclut Paris (tuu=9)
    
    Tab_tuu<-
      menage_echelle %>%
      dplyr::group_by(tuu)%>%
      summarise(sum(pondmen*coeffuc))
    
    x_tuu= bisect(function(x) (Tab_tuu[1,2]*(1-x)^0+
                                 Tab_tuu[2,2]*(1-x)^1+
                                 Tab_tuu[3,2]*(1-x)^2+
                                 Tab_tuu[4,2]*(1-x)^3+
                                 Tab_tuu[5,2]*(1-x)^4+
                                 Tab_tuu[6,2]*(1-x)^5+
                                 Tab_tuu[7,2]*(1-x)^6+
                                 Tab_tuu[8,2]*(1-x)^7)*
                    as.numeric(facteur_tuu0*TCO_tot_UC)-TCO_tot,0,1)$root
    
    # x_tuu=0.490601
    distrib<-function(i,coeffuc){
      i<-as.double(i) 
      ifelse(i==8,return(0),return(coeffuc*((1-x_tuu)**i)*as.numeric(TCO_tot_UC*facteur_tuu0)))
    }
    
    menage_echelle<-
      menage_echelle %>%
      group_by(1:n())%>%
      mutate(rev_TCO=distrib(tuu,as.numeric(coeffuc)))%>%
      ungroup()
    
    menage_echelle<-menage_echelle %>% ungroup()
    
    TCO_rtcd2<-menage_echelle %>%
      group_by(tuu)%>%
      summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
    TCO_rtcd2
    
    TCO_rtcd3<-menage_echelle %>%
      group_by(tuu)%>%
      summarise(weighted.mean(x=rev_TCO/coeffuc,w=pondmen,na.rm=T))
    TCO_rtcd3 #weighted mean du rev_TCO par uc par tuu
    # 
    menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot
  }
  
  
  # # Test
  # #
  # menage_echelle_bis<-menage_echelle
  # #
  # menage_echelle_bis$RDB <-
  #   menage_echelle$RDB+
  #   menage_echelle$rev_TCO
  # 
  # weighted.mean((menage_echelle_bis$RDB-menage_echelle$RDB)/menage_echelle$RDB,menage_echelle$pondmen,na.rm=T)
  # # +1.1% de croissance du RDB des ménages dues à la TCO rétrocédé en AMS 2035
  
  
  
  menage_echelle$RDB <-
    menage_echelle$RDB+
    menage_echelle$rev_TCO
  
  return(menage_echelle)
}