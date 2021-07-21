# Objjectif : fonction de rétrocession de la taxe carbone en fonction de
# redistribution le paramètre de type de redist

retrocession_carbon_tax <- function(TCO, TCO_tot, menage_echelle){
  

  # Libraries ---------------------------------------------------------------
  suppressMessages(library(pracma , warn.conflicts=F , quietly = T))
  try(detach("package:plyr"),silent=T)
  
  # Pour gagner du temps 
  # facteur_dec1 <-  2.201544
  # facteur_tuu0 <-  3.32953
  #Ci-dessous, valeurs post FixCharb
  facteur_dec1 <-  2.172059
  facteur_tuu0 <-  3.294676

  #TCO par UC => TCO à redistribuer par unité de consommation en moyenne
  TCO_tot_UC <- TCO_tot / as.numeric(menage_echelle %>% 
                                     summarise(sum(pondmen * coeffuc)))

  #RDB total
  RDB_tot <- as.numeric(menage_echelle %>%
                        summarise(sum(pondmen * RDB)))

  #Application des différentes formules de rétrocession en fonction de la variable redistribution
  if(redistribution == "ssrec"){ #Pas de retrocession
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO = 0)
  }
  if(redistribution=="forfait"){ #Retrocession forfaitaire par UC
    sum_pond <- as.numeric(menage_echelle %>%
                           summarise(sum(pondmen * coeffuc)))
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO = coeffuc / sum_pond * TCO_tot)
  }
  if(redistribution == "niveau_vie"){ #Rétrocession niveau_vie sur rev_tot=RDBAI
    #rétrocession niveau_vie sur rev_tot=RDBAI
    #=> on est bien neutre sur le plan distribution du revenu
    #rétrocession neutre (et pas la réforme)
    RDBAI_UC_tot <- as.numeric(menage_echelle %>% 
                               summarise(sum(pondmen * RDBAI / coeffuc)))
    menage_echelle <- 
      menage_echelle %>%
      mutate(rev_TCO = RDBAI / coeffuc / RDBAI_UC_tot * TCO_tot)
  }
  if(redistribution=="decile"){ #Retrocession par décile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))
    
    Tab_dec <- 
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1) 
    #pour se rapprocher de la solution avec tuu, on décale les déciles d'un. 
    #Le décile 1, devient 0, annule la pente de la régression

    # Emissions agrégées par UC
    # Tab_emissions<-menage_forme_2010%>%group_by(decuc2)%>%summarise(sum(pondmen*TCO_paid/coeffuc))
    # Tab_dec[i,2]=> la somme des UC sur le décile, on exclut le décile 10. Le décile 1 va toucher une somme égale à sa somme des UC multipliée par le versement par 
    # UC du décile 1 défini ci-avant par facteur_dec1 fois TCO_tot_UC, qui est le versement moyen par UC dans toute la pop. 
    # on épuise l'ensemble de la TCO à rétrocéder (TCO_tot) d'où la racine
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                               Tab_dec[2, 2] * (1 - x)^1 +
                               Tab_dec[3, 2] * (1 - x)^2 +
                               Tab_dec[4, 2] * (1 - x)^3 +
                               Tab_dec[5, 2] * (1 - x)^4 +
                               Tab_dec[6, 2] * (1 - x)^5 +
                               Tab_dec[7, 2] * (1 - x)^6 +
                               Tab_dec[8, 2] * (1 - x)^7 +
                               Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                               as.numeric(facteur_dec1 * TCO_tot_UC) - TCO_tot, 0, 1)$root
    # x_dec=0.1825126
    
    #fonction de distribution avec i le decuc2 
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) * 
                                         as.numeric(TCO_tot_UC * facteur_dec1)))
    }

    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()
    
    menage_echelle <- 
      menage_echelle %>% 
      ungroup()
  }
  if(redistribution=="tuu"){ #Retrocession par tuu, sauf Paris (tuu=9) (idem décile)
   
    Tab_tuu<-
      menage_echelle %>%
      dplyr::group_by(tuu) %>%
      summarise(sum(pondmen * coeffuc))
    
    x_tuu= bisect(function(x) (Tab_tuu[1, 2] * (1 - x)^0 +
                               Tab_tuu[2, 2] * (1 - x)^1 +
                               Tab_tuu[3, 2] * (1 - x)^2 +
                               Tab_tuu[4, 2] * (1 - x)^3 +
                               Tab_tuu[5, 2] * (1 - x)^4 +
                               Tab_tuu[6, 2] * (1 - x)^5 +
                               Tab_tuu[7, 2] * (1 - x)^6 +
                               Tab_tuu[8, 2] * (1 - x)^7) *
                               as.numeric(facteur_tuu0 * TCO_tot_UC) - TCO_tot, 0, 1)$root
    
    # x_tuu=0.490601
    distrib <- function(i, coeffuc){
      i <- as.double(i) 
      ifelse(i == 8, return(0), return(coeffuc * ((1 - x_tuu) ** i) * 
                                         as.numeric(TCO_tot_UC * facteur_tuu0)))
    }
    
    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>%
      mutate(rev_TCO = distrib( , as.numeric(coeffuc))) %>%
      ungroup()
    
    menage_echelle <- 
      menage_echelle %>%
      ungroup()
  }
  if(redistribution == "dectuu_grad"){
    
    part_dec <- 0.04
    part_tuu <- (1-9*part_dec)/(10+part_dec)
    grad_vec <- (10 - 1:10)^4
    grad_vec <- grad_vec * part_dec / sum(grad_vec)
    tuu_vec <- (9 - 0:8)^4
    tuu_df <- menage_echelle %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc))
    tuu_df$pct_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
    tuu_vec <- tuu_vec * tuu_df$pct_pond_uc 
    tuu_vec <- tuu_vec * part_tuu / sum(tuu_vec)
    
    menage_echelle$dec_fac <- grad_vec[menage_echelle$decuc2]
    menage_echelle$tuu_fac <- tuu_vec[menage_echelle$tuu + 1]
    menage_echelle$grad_fac <- (1 + menage_echelle$dec_fac) * (1 + menage_echelle$tuu_fac) - 1 
    
    menage_echelle <- menage_echelle %>%
      dplyr::group_by(tuu, decuc2) %>%
      mutate(sum_pond_dec_tuu = sum(pondmen)) %>%
      ungroup()
    
    menage_echelle$rev_TCO <- TCO_tot * menage_echelle$grad_fac / menage_echelle$sum_pond_dec_tuu
    
    menage_echelle <- menage_echelle %>% select(-dec_fac, tuu_fac, grad_fac)
  }
  if(redistribution == "seq_dectuu"){
    
    #Premier step : idem redistribution == decile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))
    
    Tab_dec <- 
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1) 
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                                 Tab_dec[2, 2] * (1 - x)^1 +
                                 Tab_dec[3, 2] * (1 - x)^2 +
                                 Tab_dec[4, 2] * (1 - x)^3 +
                                 Tab_dec[5, 2] * (1 - x)^4 +
                                 Tab_dec[6, 2] * (1 - x)^5 +
                                 Tab_dec[7, 2] * (1 - x)^6 +
                                 Tab_dec[8, 2] * (1 - x)^7 +
                                 Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                    as.numeric(facteur_dec1 * TCO_tot_UC) - TCO_tot, 0, 1)$root

    #fonction de distribution avec i le decuc2 
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) * 
                                         as.numeric(TCO_tot_UC * facteur_dec1)))
    }
    
    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO_dec = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()
    
  
    #Deuxième step : on répartit la TC obtenue dans chaque décile parmi les TUU
    dec_vec <- sort(unique(menage_echelle$decuc2))
    tuu_vec <- sort(unique(menage_echelle$tuu))
    #Choix d'un vecteur de distribution par tuu
    tuu_df <- data.frame(tuu = tuu_vec)
    tuu_df$scale <- (9 - tuu_df$tuu) ^ 3
    tuu_df <- tuu_df %>% left_join(menage_echelle %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
    tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
    tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))
    
    res_df <- tibble()
    for(dec_i in dec_vec){
      dec_tuu_df <- tuu_df
      sub_men <- menage_echelle %>% filter(decuc2 == dec_i)
      TC_rev_temp <- sum(sub_men$rev_TCO_dec * sub_men$pondmen)
      dec_tuu_df <- dec_tuu_df %>% mutate(TC_tuu = TC_rev_temp * factor)
      dec_tuu_df <- dec_tuu_df %>% 
                    left_join(sub_men %>% group_by(tuu) %>% summarise(subsum_pond_uc = sum(pondmen * coeffuc)), by = "tuu") %>%
                    mutate(TC_uc = TC_tuu / subsum_pond_uc)
      sub_men$TC_uc <- dec_tuu_df$TC_uc[match(sub_men$tuu, dec_tuu_df$tuu)]
      sub_men <- sub_men %>% mutate(rev_TCO = TC_uc * coeffuc)
      res_df <- rbind(res_df, sub_men %>% select(ident_men, rev_TCO))
    }
    
    menage_echelle$rev_TCO <- res_df$rev_TCO[match(menage_echelle$ident_men, res_df$ident_men)]
    
    print(paste("Somme TC"  = sum(menage_echelle$rev_TCO * menage_echelle$pondmen)))
    menage_echelle <- menage_echelle %>% select(- rev_TCO_dec)
  }
  if(redistribution == "dectuu_seq_ucmat"){
    
    #Premier step : idem redistribution == decile
    Tab_dec<-
      menage_echelle %>%
      dplyr::group_by(decuc2) %>%
      summarise(sum(pondmen * coeffuc))
    
    Tab_dec <- 
      Tab_dec %>%
      mutate(decuc2 = as.numeric(decuc2) - 1) 
    x_dec= bisect(function(x) (Tab_dec[1, 2] * (1 - x)^0 + #décile 1
                                 Tab_dec[2, 2] * (1 - x)^1 +
                                 Tab_dec[3, 2] * (1 - x)^2 +
                                 Tab_dec[4, 2] * (1 - x)^3 +
                                 Tab_dec[5, 2] * (1 - x)^4 +
                                 Tab_dec[6, 2] * (1 - x)^5 +
                                 Tab_dec[7, 2] * (1 - x)^6 +
                                 Tab_dec[8, 2] * (1 - x)^7 +
                                 Tab_dec[9, 2] * (1 - x)^8) * #décile 9
                    as.numeric(facteur_dec1 * TCO_tot_UC) - TCO_tot, 0, 1)$root
    
    #fonction de distribution avec i le decuc2 
    distrib <- function(i, coeffuc){
      i <- as.double(i) - 1  #on translate le decuc2 pour être raccord avec plus haut
      ifelse(i == 9, return(0), return(coeffuc * ((1 - x_dec) ** i) * 
                                         as.numeric(TCO_tot_UC * facteur_dec1)))
    }
    
    menage_echelle <-
      menage_echelle %>%
      group_by(1:n()) %>% #on groupe ménage par ménage
      mutate(rev_TCO_dec = as.numeric(distrib(i = decuc2, coeffuc = as.numeric(coeffuc)))) %>%
      ungroup()
    
    
    #Deuxième step : on répartit la TC obtenue dans chaque décile parmi les TUU
    dec_vec <- sort(unique(menage_echelle$decuc2))
    tuu_vec <- sort(unique(menage_echelle$tuu))
    #Choix d'un vecteur de distribution par tuu
    tuu_df <- data.frame(tuu = tuu_vec)
    tuu_df$scale <- (9 - tuu_df$tuu) ^ 0.5
    tuu_df <- tuu_df %>% left_join(menage_echelle %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
    tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
    tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))
    
    res_df <- tibble()
    for(dec_i in dec_vec){
      sub_men <- menage_echelle %>% filter(decuc2 == dec_i)

      #Choix d'un vecteur de distribution par tuu
      tuu_df <- data.frame(tuu = tuu_vec)
      tuu_df$scale <- (9 - tuu_df$tuu) ^ 0.5
      tuu_df <- tuu_df %>% left_join(sub_men %>% group_by(tuu) %>% summarise(sum_pond_uc = sum(pondmen * coeffuc)))
      tuu_df$weight_pond_uc <- tuu_df$sum_pond_uc / sum(tuu_df$sum_pond_uc)
      tuu_df$factor <- tuu_df$scale * tuu_df$weight_pond_uc / (sum(tuu_df$scale * tuu_df$weight_pond_uc))
      dec_tuu_df <- tuu_df
      
      TC_rev_temp <- sum(sub_men$rev_TCO_dec * sub_men$pondmen)
      dec_tuu_df <- dec_tuu_df %>% mutate(TC_tuu = TC_rev_temp * factor)
      dec_tuu_df <- dec_tuu_df %>% 
        left_join(sub_men %>% group_by(tuu) %>% summarise(subsum_pond_uc = sum(pondmen * coeffuc)), by = "tuu") %>%
        mutate(TC_uc = TC_tuu / subsum_pond_uc)
      sub_men$TC_uc <- dec_tuu_df$TC_uc[match(sub_men$tuu, dec_tuu_df$tuu)]
      sub_men <- sub_men %>% mutate(rev_TCO = TC_uc * coeffuc)
      res_df <- rbind(res_df, sub_men %>% select(ident_men, rev_TCO))
    }
    
    menage_echelle$rev_TCO <- res_df$rev_TCO[match(menage_echelle$ident_men, res_df$ident_men)]
    
    print(paste("Somme TC"  = sum(menage_echelle$rev_TCO * menage_echelle$pondmen)))
    menage_echelle <- menage_echelle %>% select(- rev_TCO_dec)
  }
  menage_echelle$RDB <- menage_echelle$RDB + menage_echelle$rev_TCO
  
  return(menage_echelle)
  
}


# Verification ------------------------------------------------------------

#vérif
# menage_echelle %>% summarise(sum(pondmen*rev_TCO)) # = TCO_tot


# menage_echelle %>% summarise(sum(pondmen*rev_TCO))
# menage_echelle %>% summarise(mean(rev_TCO)) 


# TCO_rtcd2 <-
#   menage_echelle %>%
#   group_by(decuc2) %>%
#   summarise(weighted.mean(x = rev_TCO, w = pondmen, na.rm = T))
# TCO_rtcd2 
#weighted mean par décile
# menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot

# TCO_rtcd2 <- menage_echelle %>%
#   group_by(decuc2) %>%
#   summarise(weighted.mean(x = rev_TCO, w = pondmen, na.rm = T))
# TCO_rtcd2 

# TCO_rtcd3<-menage_echelle %>%
#   group_by(decuc2)%>%
#   summarise(weighted.mean(x = rev_TCO / coeffuc, w = pondmen,na.rm=T))
# TCO_rtcd3

# menage_echelle%>%summarise(sum(pondmen*rev_TCO)) - TCO_tot
# 
# 
# TCO_rtcd2<-menage_echelle %>%
#   group_by(tuu)%>%
#   summarise(weighted.mean(x=rev_TCO,w=pondmen,na.rm=T))
# TCO_rtcd2
# 
# TCO_rtcd3<-menage_echelle %>%
#   group_by(tuu)%>%
#   summarise(weighted.mean(x=rev_TCO/coeffuc,w=pondmen,na.rm=T))
# TCO_rtcd3 #weighted mean du rev_TCO par uc par tuu
# # 

# menage_echelle%>%summarise(sum(pondmen*rev_TCO))-TCO_tot

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
