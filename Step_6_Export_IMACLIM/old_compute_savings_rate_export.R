

compute_savings_rate_export<-function(menage){
  
  menage$AID[is.na(menage$AID)]<-0
  menage$rev801[is.na(menage$rev801)]<-0
  menage$impot_revenu[is.na(menage$impot_revenu)]<-0  
  menage$c13711[is.na(menage$c13711)]<-0
  
  # Inclut les loyers imputés, au niveau macro on compte les loyers imputés comme du revenu et des dépenses puisqu'il s'agit de transferts entre ménages, on compte aussi les revenus exceptionnels autres que les transferts entre ménages
  menage$RDB_exp <- rowSums(menage[c("rev_activites","rev_patrimoine","rev_sociaux","rev_TCO","rev801","rev60x","rev999")])-rowSums(menage[c("impot_revenu","AID")])
  
  menage <-
    menage %>%
    mutate(loyers=loyers+rev801)
  
  
  #On ne prend pas directement en compte les dépenses de logements neufs : pas de surcoût à compter, on peut directement prendre la différence entre les dépenses en achats agrégées et les revenus issus de ces ventes agrégés.
  # menage <- 
  #   menage %>%
  #   mutate(c13711_neuf=0)%>%
  #   mutate_when(year_neuf==horizon,list(c13711_neuf=c13711))%>%
  #   mutate(loyers=loyers+rev801)
  
  
  
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
  #rev800 mis à jour par FC$A05 * FC$surface en Step_2
  # => Pas besoin d'utiliser la sélection des ménages de 4_1_Neuf_horizon puisque pas de surcoût à la construction energy efficient. 
  Achats_immobiliers_pro = menage %>% summarise(sum(pondmen*c13711,na.rm=T)) - menage %>% summarise(sum(pondmen*rev800,na.rm=T))
  
  # Achats de véhicules à des professionnels
  ###  
  # Au niveau agrégé : on ventile les achats de véhicules d'occasion (secteur 14) entre achats à des particuliers (donnant lieu à un revenu pour certains ménages : rev850) et à des professionnels (comptés dans la compta nat)
  #rev850 mis à jour par le rdb en Step_1
  Vente_VP_pro = menage %>% summarise(sum(pondmen*veh_occasion,na.rm=T)) - menage %>% summarise(sum(pondmen*rev850,na.rm=T))
  
  RDB_exp_agreg <- menage %>% summarise(sum(pondmen*RDB_exp))
  Rcons_exp_agreg <- Rcons_exp_tot + Achats_immobiliers_pro + Vente_VP_pro
  epargne_agreg <- RDB_exp_agreg - Rcons_exp_agreg
  taux_epargne_agreg <-  epargne_agreg/RDB_exp_agreg
  
  return(taux_epargne_agreg)
}
