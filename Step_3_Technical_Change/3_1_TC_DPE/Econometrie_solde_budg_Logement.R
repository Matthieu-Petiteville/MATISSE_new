
# LIBRARIES ---------------------------------------------------------------
library(tidyverse)


# FUNCTION ----------------------------------------------------------------

#solde représente le solde budgétaire, positif ou négatif, issu d'un réarrangement 
# du budget pour intégrer le TC
# Vecteur colonne, nombre de ligne correspondant au nombre de ménage de 2025 : ident_men et colonne


Ventil_solde <- function(solde,menage,step){
  
  
  # DONNEES -----------------------------------------------------------------
  
  
  menage_bis<-menage
  menage_bis <- menage_bis %>% left_join(solde,by="ident_men")
  source(paste(M_home,"/Common/tools.R",sep=""))
  load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))
  
  
  list_dep_14=c("agriculture",
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
                "veh_occasion")
  

  
  # Ventilation -------------------------------------------------------------
  
  
  menage_bis<- menage_bis %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
  menage_bis$Rcons_bis <- rowSums(menage_bis[list_dep_14])
  menage_bis <- menage_bis %>% mutate_when(is.na(Rcons_bis),list(Rcons_bis=0)) #ménage 10828
  

  for (i in 1:14){
    k=list_dep_14[i]
    if(i<10){
      menage_bis[paste("share_A0",i,sep="")]<-menage_bis[k]/menage_bis$Rcons_bis}
    else{
      menage_bis[paste("share_A",i,sep="")]<-menage_bis[k]/menage_bis$Rcons_bis
    }
  }
  
  
  # table(rowSums(menage_bis %>% select(starts_with("share_A"))))
  #sécurité
  menage_bis <- 
    menage_bis %>%
    mutate_when(is.na(share_A01),list(share_A01=0),
                is.na(share_A02),list(share_A02=0),
                is.na(share_A03),list(share_A03=0),
                is.na(share_A04),list(share_A04=0),
                is.na(share_A05),list(share_A05=0),
                is.na(share_A06),list(share_A06=0),
                is.na(share_A07),list(share_A07=0),
                is.na(share_A08),list(share_A08=0),
                is.na(share_A09),list(share_A09=0),
                is.na(share_A10),list(share_A10=0),
                is.na(share_A11),list(share_A11=0),
                is.na(share_A12),list(share_A12=0),
                is.na(share_A13),list(share_A13=0),
                is.na(share_A14),list(share_A14=0)
    )
  
  
  menage_bis$IP_stone <-
    FC$A01**menage_bis$share_A01*
    FC$A02**menage_bis$share_A02*
    FC$A03**menage_bis$share_A03*
    FC$A04**menage_bis$share_A04*
    FC$A05**menage_bis$share_A05*
    FC$A06**menage_bis$share_A06*
    FC$A07**menage_bis$share_A07*
    FC$A08**menage_bis$share_A08*
    FC$A09**menage_bis$share_A09*
    FC$A10**menage_bis$share_A10*
    FC$A11**menage_bis$share_A11*
    FC$A12**menage_bis$share_A12*
    FC$A13**menage_bis$share_A13*
    FC$A14**menage_bis$share_A14
  
  
  
  menage_bis$RDB_reel <-  menage_bis$RDB/menage_bis$IP_stone
  menage_bis<-
    menage_bis %>%
    mutate_when(is.na(RDB_reel),list(RDB_reel==0))
  # si le RDB_reel est >0 ou ==0, alors on considère le ratio solde/Rcons_bis 
  # qui est le meilleur proxy du comportement de consommation du ménage
  # -menage_bis$solde/menage_bis$Rcons_bis
  # L'IP stone ne rentre pas dans ce dernier calcul, diviserait à la fois le solde et le Rcons

  iter=TRUE #Pour la première itération
  nb_iter_RDB=0


  while(iter & nb_iter_RDB<60){
    sauv_menage<-    menage_bis
    nb_iter_RDB=nb_iter_RDB+1
    list_dep_autres_ener=c("dep_GPL","dep_Fuel","dep_Urbain", "dep_Solides")
    
   #on ne reventile pas sur les biens incluant énergie sauf étape 3.4
    
    # On ne reventile jamais sur les biens ener, effet rebond pris en compte dans les chiffres de Gael qu'on traduit en 3.1, 3.2 et 3.3, 3.4 n'est qu'un effet qu'on applique expst
    if(step=="REHAB"){
    
    
    ##
    ### BIENS HORS ENERGIE
    ##
    
    # on repart des dépenses de ménage (base input) pour ne pas empiler les itérations 
    for (i in c(1,5:9)){
      elast_rev<-paste("elast_rev_A0",i,sep="")
      menage_bis[list_dep_14[i]]<-  menage[list_dep_14[i]]*(1+menage_bis[elast_rev]*ifelse(menage_bis$RDB_reel<=0 | abs(menage_bis$solde)>menage_bis$RDB
                                                                                           ,-menage_bis$solde/menage_bis$Rcons_bis,
                                                                                           -menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
    }
    
    for (i in 10:14){
      elast_rev<-paste("elast_rev_A",i,sep="")
      menage_bis[list_dep_14[i]]<-  menage[list_dep_14[i]]*(1+ menage_bis[elast_rev]*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
    }
    # NB : le Hors budget est exclus de la reventilation, nous ne connaissons pas le comportement des agents vis-à-vis cet agrégat qui regroupe des dépenses exceptionnelles et/ou importantes
    }
    
    if(step=="VE"){
      
      ##
      ### BIENS HORS CARBURANT
      ## 
      
      
      # A02
      menage_bis$dep_Elec<-  
        menage$dep_Elec*
        (1+menage_bis$elast_rev_A02*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
      
      # A03
      menage_bis$dep_Gaz<-  
        menage$dep_Gaz*
        (1+menage_bis$elast_rev_A03*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
      
      # A04
      
      menage_bis[list_dep_autres_ener]<-  
        menage[list_dep_autres_ener]*
        (1+menage_bis$elast_rev_A04*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
      
      # Dep_logement
      menage_echelle$dep_energie_logement<-rowSums(menage_echelle[c("dep_Elec","dep_Gaz",list_dep_autres_ener)])
      
      for (i in c(1,5,6,8,9)){#bien 7 exclus
        elast_rev<-paste("elast_rev_A0",i,sep="")
        menage_bis[list_dep_14[i]]<-  menage[list_dep_14[i]]*(1+menage_bis[elast_rev]*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
      }
      
      for (i in 10:14){
        elast_rev<-paste("elast_rev_A",i,sep="")
        menage_bis[list_dep_14[i]]<-  menage[list_dep_14[i]]*(1+ menage_bis[elast_rev]*ifelse(abs(menage_bis$solde)>menage_bis$RDB,-menage_bis$solde/menage_bis$Rcons_bis,-menage_bis$solde/menage_bis$IP_stone/menage_bis$RDB_reel))
      }
      
      
    }
    
    
    #On annule les dépenses NA
    menage_bis<-menage_bis%>%
      mutate_when(is.na( agriculture ),list( agriculture =0))%>%
      mutate_when(is.na( dep_Elec ),list( dep_Elec =0))%>%
      mutate_when(is.na( dep_Gaz ),list( dep_Gaz =0))%>%
      mutate_when(is.na( dep_GPL ),list( dep_GPL =0))%>%
      mutate_when(is.na( dep_Fuel ),list( dep_Fuel =0))%>%
      mutate_when(is.na( dep_Urbain ),list( dep_Urbain =0))%>%
      mutate_when(is.na( dep_Solides ),list( dep_Solides =0))%>%
      mutate_when(is.na( BTP ),list( BTP =0))%>%
      mutate_when(is.na( prod_veh ),list( prod_veh =0))%>%
      mutate_when(is.na( carb_lubr ),list( carb_lubr =0))%>%
      mutate_when(is.na( transp_rail_air ),list( transp_rail_air =0))%>%
      mutate_when(is.na( transp_routes_eau ),list( transp_routes_eau =0))%>%
      mutate_when(is.na( loisirs_com ),list( loisirs_com =0))%>%
      mutate_when(is.na( autres_services ),list( autres_services =0))%>%
      mutate_when(is.na( autres ),list( autres =0))%>%
      mutate_when(is.na( loyers ),list( loyers =0))%>%
      mutate_when(is.na( veh_occasion ),list( veh_occasion =0))%>%
      mutate_when(is.na( Hors_budget ),list( Hors_budget =0))
    
    # menage_bis$agriculture[which(menage_bis$agriculture<0)]<-0
    # menage_bis$dep_Elec[which(menage_bis$dep_Elec<0)]<-0
    # menage_bis$dep_Gaz[which(menage_bis$dep_Gaz<0)]<-0
    # menage_bis$dep_GPL[which(menage_bis$dep_GPL<0)]<-0
    # menage_bis$dep_Fuel[which(menage_bis$dep_Fuel<0)]<-0
    # menage_bis$dep_Urbain[which(menage_bis$dep_Urbain<0)]<-0
    # menage_bis$dep_Solides[which(menage_bis$dep_Solides<0)]<-0
    # menage_bis$BTP[which(menage_bis$BTP<0)]<-0
    # menage_bis$prod_veh[which(menage_bis$prod_veh<0)]<-0
    # menage_bis$carb_lubr[which(menage_bis$carb_lubr<0)]<-0
    # menage_bis$transp_rail_air[which(menage_bis$transp_rail_air<0)]<-0
    # menage_bis$transp_routes_eau[which(menage_bis$transp_routes_eau<0)]<-0
    # menage_bis$loisirs_com[which(menage_bis$loisirs_com<0)]<-0
    # menage_bis$autres_services[which(menage_bis$autres_services<0)]<-0
    # menage_bis$autres[which(menage_bis$autres<0)]<-0
    # menage_bis$loyers[which(menage_bis$loyers<0)]<-0
    # menage_bis$veh_occasion[which(menage_bis$veh_occasion<0)]<-0
    # menage_bis$Hors_budget[which(menage_bis$Hors_budget<0)]<-0
    
    menage_bis<- menage_bis %>% mutate(dep_autres_energies=dep_Fuel+dep_GPL+dep_Solides+dep_Urbain)
    menage_bis$Rcons_bis <- rowSums(menage_bis[list_dep_14])
     menage_bis <- menage_bis %>% mutate_when(is.na(Rcons_bis),list(Rcons_bis=0)) #ménage 10828
    
    for (i in 1:14){
      k=list_dep_14[i]
      if(i<10){
        menage_bis[paste("share_A0",i,sep="")]<-menage_bis[k]/menage_bis$Rcons_bis}
      else{menage_bis[paste("share_A",i,sep="")]<-menage_bis[k]/menage_bis$Rcons_bis}
    }
    
    #sécurité
    menage_bis <- 
      menage_bis %>%
      mutate_when(is.na(share_A01),list(share_A01=0),
                  is.na(share_A02),list(share_A02=0),
                  is.na(share_A03),list(share_A03=0),
                  is.na(share_A04),list(share_A04=0),
                  is.na(share_A05),list(share_A05=0),
                  is.na(share_A06),list(share_A06=0),
                  is.na(share_A07),list(share_A07=0),
                  is.na(share_A08),list(share_A08=0),
                  is.na(share_A09),list(share_A09=0),
                  is.na(share_A10),list(share_A10=0),
                  is.na(share_A11),list(share_A11=0),
                  is.na(share_A12),list(share_A12=0),
                  is.na(share_A13),list(share_A13=0),
                  is.na(share_A14),list(share_A14=0)
                  )
    
    
    menage_bis$IP_stone <-
      FC$A01**menage_bis$share_A01*
      FC$A02**menage_bis$share_A02*
      FC$A03**menage_bis$share_A03*
      FC$A04**menage_bis$share_A04*
      FC$A05**menage_bis$share_A05*
      FC$A06**menage_bis$share_A06*
      FC$A07**menage_bis$share_A07*
      FC$A08**menage_bis$share_A08*
      FC$A09**menage_bis$share_A09*
      FC$A10**menage_bis$share_A10*
      FC$A11**menage_bis$share_A11*
      FC$A12**menage_bis$share_A12*
      FC$A13**menage_bis$share_A13*
      FC$A14**menage_bis$share_A14
    
    
    menage_bis$RDB_reel <-  menage_bis$RDB/menage_bis$IP_stone
    tol=abs((menage_bis$RDB_reel-sauv_menage$RDB_reel)/sauv_menage$RDB_reel)
    
    
    max(tol,na.rm=T)
    print(max(tol,na.rm=T))
    if(max(tol,na.rm=T)>10^-6){iter=TRUE} else {iter=FALSE}
    
  }
  

  
  # RETURN ------------------------------------------------------------------
  
  print(paste("nb_iter_RDB: ",nb_iter_RDB))    
  return(menage_bis %>% select(colnames(menage)))
  
  
  
}


