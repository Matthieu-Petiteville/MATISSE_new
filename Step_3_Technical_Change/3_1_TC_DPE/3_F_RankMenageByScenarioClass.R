#Permet de générer la colonne kWh_rank dans ménage qui classe, par groupe de DPE et Stalog, les ménages dans l'ordre de priorité
#défini par scenario_classement

RankMenageByScenarioClass <- function(menage,scenario_classement){

  #Source
  source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/calc_ems.R",sep=""))

  #Cases
  if(scenario_classement == "Optimiste"){
    menage<-
      menage %>% 
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank =row_number(-ener_dom)) %>% 
      ungroup()
  }
  if(scenario_classement == "Pessimiste"){
    menage <-
      menage %>% 
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank =row_number(-ener_dom)) %>% 
      dplyr::mutate(kWh_rank =max(kWh_rank,na.rm=T)-kWh_rank+1) %>% 
      ungroup()
  }
  if(scenario_classement == "Median"){
    menage_temp <- menage %>% 
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank_opt =row_number(-ener_dom))  %>% 
      ungroup()  %>% 
      select(c(ident_men,DPE_stalog_propri,kWh_rank_opt,))
    
    menage_temp <- menage_temp %>% 
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank_pess = max(kWh_rank_opt,na.rm=T)-kWh_rank_opt+1) %>%
      ungroup()
    
    menage_temp <-
      menage_temp %>% 
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank = kWh_rank_pess-kWh_rank_opt) %>% 
      mutate_when(kWh_rank <=0,list(kWh_rank=-kWh_rank+1)) %>%
      ungroup() 
  
    menage <- menage %>%
      dplyr::mutate(kWh_rank = menage_temp$kWh_rank)
  }
  if(scenario_classement == "Optimal_ener"){
    menage<-
      menage %>% 
      mutate(ener_dom_surf=ifelse(is.infinite(ener_dom_surf),0,ener_dom_surf))%>%
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank =row_number(-ener_dom_surf)) %>% 
      ungroup()
  }
  if(scenario_classement == "Optimal_co2"){
    menage$ems_tot_chauff_ecs <- calc_ems(menage,FC)
    menage <-
      menage %>% 
      mutate(ems_tot_chauff_ecs_surf=ems_tot_chauff_ecs/surfhab_d)%>%
      mutate(ems_tot_chauff_ecs_surf=ifelse(is.infinite(ems_tot_chauff_ecs_surf),0,ems_tot_chauff_ecs_surf))%>%
      group_by(DPE_stalog_propri) %>% 
      dplyr::mutate(kWh_rank =row_number(-ems_tot_chauff_ecs_surf)) %>% 
      ungroup() %>%
      select(-c(ems_tot_chauff_ecs))
  }
  
  if(scenario_classement=="Rich"){
    menage <-
      menage %>% 
      group_by(DPE_dep) %>% 
      dplyr::mutate(kWh_rank=row_number(-RDB/coeffuc)) %>% 
      ungroup()    
  }
  if(scenario_classement=="Poor"){
    menage <-
      menage %>% 
      group_by(DPE_dep) %>% 
      dplyr::mutate(kWh_rank=row_number(-RDB/coeffuc)) %>% 
      dplyr::mutate(kWh_rank=max(kWh_rank)-kWh_rank+1) %>% 
      ungroup()    
  }
  menage <- 
    menage %>%
    mutate(kWh_rank=kWh_rank+rank_add) %>%
    select(-rank_add)


}
