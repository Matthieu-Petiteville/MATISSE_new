mise_echelle_revenu<-function(FC,menage_forme){
  
  
# Mise à l'échelle des revenus  -------------------------------------------
  
  menage_echelle <- menage_forme
  menage_echelle<-
    menage_echelle %>%
    mutate(rev_total = rev_sociaux +  rev_activites + rev_patrimoine)
#on n'inclut pas les revenus exceptionnels récurrents rev700, rev701, rev999 
#car on n'a pas de facteur de mise à l'échelle, on veut calculer une moyenne 
#pondérée du FC des revenus de chaque ménage.
  

# Taux de croissance des revenus par ménage -----------------------------------------------------------

  # FC_rdb_menage est la moyenne pondérée des taux de croissance de ménage pour màj certaines dépenses et revenus
  menage_echelle<-
    menage_echelle %>%
    mutate(FC_rdb_menage = (FC$revsoc*rev_sociaux_autres +
                            FC$revchom*chomage +
                            FC$revact*rev_activites_sans_etranger +
                            FC$revetranger*rev_etranger +
                            FC$revpat*rev_patrimoine) / rev_total)
  
  
# Correction --------------------------------------------------------------
  
  # Trois ménages ont des revenus nuls en 2010, ident_men=2548, 4710 et 10828
  menage_echelle<-
    menage_echelle %>%
    mutate_when(is.na(FC_rdb_menage) , list(FC_rdb_menage=FC$rdb)) 
  

# Mise à l'échelle --------------------------------------------------------


  menage_echelle <-
    menage_echelle %>%
    mutate(retraites = retraites * (FC$revsoc)) %>%
    mutate(chomage = chomage * (FC$revchom)) %>%
    mutate(rev_activites_sans_etranger = rev_activites_sans_etranger * (FC$revact)) %>%
    mutate(rev_etranger = rev_etranger * (FC$revetranger)) %>%
    mutate(rev_exceptionnel = rev_exceptionnel * (FC_rdb_menage)) %>%
    mutate(rev_patrimoine = rev_patrimoine * (FC$revpat)) %>%
    mutate(rev_sociaux_autres = rev_sociaux_autres * (FC$revsoc)) %>%
    mutate(rev700=rev700 * (FC_rdb_menage)) %>%
    mutate(rev701=rev701 * (FC_rdb_menage)) %>%
    mutate(rev999=rev999 * (FC_rdb_menage)) %>%
    mutate(rev60x=rev60x * (FC_rdb_menage))
  # NB : les revenus exceptionnels et autres revenus des ménages sont mise à l'échelle 
  # comme la moyenne de leurs revenus (FC_rdb_menage), ménage par ménage
  # NB2: rev801, rev800 et rev850 sont mis à jours comme des dépenses (resp loyers, BTP et prod_veh)
  
  # On n'a plus égalité entre les masses sommables en 2010 car taux de màj différents
  table(menage_echelle$rev_sociaux_autres+menage_echelle$chomage==menage_echelle$rev_sociaux)
  
  # on corrige
  menage_echelle <- 
    menage_echelle %>%
    mutate(rev_sociaux = rev_sociaux_autres+chomage) %>% 
    mutate(rev_activites = rev_activites_sans_etranger + rev_etranger) %>%
    select(-rev_total) 
  #rev_total a été utilisé comme variable de construction du FC_RDB_menage, 
  #pas un vrai revenu total (avant impôt)
  
  
  menage_echelle$RDBAI <- rowSums(menage_echelle[c("rev_activites","rev_sociaux","rev_patrimoine","rev700","rev701","rev999")])
  # menage_echelle %>% summarise(sum(RDBAI*pondmen)) 2.259077e+12 contre DISPINC_VAL_0 à 3.346e+12,
  #3.32e+12 non compensé par les rev_tco de la taxe carbone rétrocédée pas encore attribuée aux ménages.
  #2.25/3.32=0.67 => on reste dans les bons ordres de grandeur par rapport à la sous-déclaration de 2010. 
  
  
  menage_echelle$taux_AID <- menage_echelle$taux_AID * (FC$tauAID)
  menage_echelle$taux_IR <- menage_echelle$taux_IR * (FC$tauIR)
  
  # Quand RDBAI=0, taux_IR et taux_AID sont Inf ou Nan.
  menage_echelle <- 
    menage_echelle %>%
    mutate(taux_AID = ifelse(is.na(taux_AID),0,taux_AID)) %>%
    mutate(taux_AID = ifelse(is.infinite(taux_AID),0,taux_AID)) %>%
    mutate(taux_IR = ifelse(is.na(taux_IR),0,taux_IR)) %>%
    mutate(taux_IR = ifelse(is.infinite(taux_IR),0,taux_IR))
  
  
  menage_echelle %>% filter(taux_IR == 0 & impot_revenu > 0) %>% select(ident_men)
  menage_echelle %>% filter(taux_AID == 0 & AID>0) %>% select(ident_men)
  #exemple du ménage 2548, AID=733 en 2010 mais RDBAI=0, donc taux_AID=0, on garde le même montant d'AID qu'en 2010.
  # same for menage 10828

  menage_echelle <- 
    menage_echelle %>%
    mutate(impot_revenu = ifelse(taux_IR == 0 , impot_revenu , taux_IR*RDBAI)) %>%
    mutate(AID = ifelse(taux_AID == 0 , AID , taux_AID*RDBAI))
  
  menage_echelle$RDB <- menage_echelle$RDBAI - rowSums(menage_echelle[c("impot_revenu","AID")])
  
  
  return(menage_echelle)
}