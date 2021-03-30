calc_ems<-function(menage,FC){
  
  s=scenario
  h=horizon
  # r="ssrec"
  # sc="Optimiste"
  
  
  
  FC_coeff <- 
    coeff_dep_ems %>%
    filter(year==h)%>%
    filter(scenario==s)%>%
    select(Variable,FC_coeff_emission)%>%
    spread(key=Variable,value=FC_coeff_emission)
  
  menage <- 
    menage %>% 
    mutate(CL_2010=(Solides_chauff+Solides_ECS)/FC$A04)%>% 
    mutate(Oil_2010=(Fuel_chauff+Fuel_ECS+GPL_chauff+GPL_ECS)/FC$A07)%>%
    mutate(Gaz_2010=(Gaz_chauff+Gaz_ECS+Urbain_ECS+Urbain_chauff)/FC$A03)

  menage <- 
    menage %>% 
    mutate(TCO_CL=CL_2010*coeff_ems_2010$coeff_CL_2010*FC_coeff$C21*TCO)%>%
    mutate(TCO_Oil=Oil_2010*coeff_ems_2010$coeff_Oil_2010*FC_coeff$C22* TCO)%>%
    mutate(TCO_Gaz=Gaz_2010*coeff_ems_2010$coeff_Gaz_2010*FC_coeff$C24*TCO)
  
  menage<-
    menage %>%
    mutate(ems_CL=TCO_CL/TCO,
           ems_Oil=TCO_Oil/TCO,
           ems_Gaz=TCO_Gaz/TCO)%>%
    mutate(ems_tot_chauff_ecs=ems_CL+ems_Oil+ems_Gaz)
  
  # return(menage %>% select(ident_men,ems_tot_chauff_ecs))
  
  return(menage$ems_tot_chauff_ecs)
}