#Fonction permettant de sélectionner les ménages à basculer


select_bascule <- function(sub_men_ech, Sum_horizon_Mat, scenario_classement_bascule, redistribution){

  #Variable de classement pour bascule
  sub_men_ech <-
    sub_men_ech %>% 
    dplyr::mutate(kWh_rank_opt = row_number(dep_surf_to_rank)) %>% 
    dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>% 
    dplyr::mutate(kWh_rank_med = kWh_rank_pess-kWh_rank_opt) %>% 
    mutate_when(kWh_rank_med<=0,list(kWh_rank_med=-kWh_rank_med+1))
  
  if(str_detect(scenario_classement_bascule,"Pessimiste")){
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank=kWh_rank_pess)
  }
  if(str_detect(scenario_classement_bascule,"Optimiste")){
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank=kWh_rank_opt)
  }
  if(scenario_classement_bascule=="Median"){
    sub_men_ech <- sub_men_ech %>% mutate(kWh_rank=kWh_rank_med)
  }      
  
  cpt = 1
  nb_basc <- 0
  if(redistribution == "forfait"){
    #En mode forfait, on calibre sur les consommations de fioul ou gaz
    while(Sum_horizon_Mat$left_to_bascule[ener_ind] > 0 && cpt <= max(sub_men_ech$kWh_rank)){
      men_line <- which(sub_men_ech$kWh_rank == cpt)
      if(sub_men_ech[men_line,dep_ener]>0 && sub_men_ech[men_line,bascule_ener] == 0){
        sub_men_ech[men_line,bascule_ener] <- 1
        Sum_horizon_Mat$left_to_bascule[ener_ind] = Sum_horizon_Mat$left_to_bascule[ener_ind] - sub_men_ech[men_line,dep_ener] / 10^6
        nb_basc <- nb_basc + 1
      }
      cpt = cpt + 1
    }
    target_basc_m2 <- NA
  }else{
    #En mode non forfait, on utilise le décompte des m2 que l'on a calibré en mode forfait
    ener_basc_ind <- which(target_basc_m2$type == bascule_ener)
    while(target_basc_m2$left_to_bascule[ener_basc_ind] > 0 && cpt <= max(sub_men_ech$kWh_rank)){
      men_line <- which(sub_men_ech$kWh_rank == cpt)
      if(sub_men_ech[men_line,dep_ener]>0 && sub_men_ech[men_line,bascule_ener] == 0){
        sub_men_ech[men_line,bascule_ener] <- 1
        target_basc_m2$left_to_bascule[ener_basc_ind] = target_basc_m2$left_to_bascule[ener_basc_ind] - 
                                                        sub_men_ech$surfhab_d[men_line] * sub_men_ech$pondmen[men_line]
        nb_basc <- nb_basc + 1
      }
      cpt = cpt + 1
    }
  }

  return(list(sub_men_ech = sub_men_ech, 
              Sum_horizon_Mat = Sum_horizon_Mat, 
              nb_basc = nb_basc,
              target_basc_m2 = target_basc_m2))

}