


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)

initializeMatisseFiles()

# Data --------------------------------------------------------------------

if(!exists("align_bascule_on_3ME")){align_bascule_on_3ME <- F}

if(redistribution != "forfait"){
  load(MatisseFiles$menage_calibre_ele_forfait_rd)
  load(MatisseFiles$menage_calibre_forfait_rd)
  rapport_depElec <- mean(menage_echelle_calibre_ele$dep_Elec / menage_echelle_calibre$dep_Elec, na.rm = T)
}


if(align_bascule_on_3ME){
  load(MatisseFiles$menage_forme_rd)
  load(MatisseFiles$FC_2010_horizon_rd)
  
  #3ME
  S <- switch (scenario,
               "AMS" = "scen AMS",
               "AME" = "scen AME",
               "ssTCO" = "scen AMS ss TCO",
               "ssRES" = "scen AMS ss residentiel",
               "ssVE" = "scen AMS ss VE")
  suppressMessages(suppressWarnings(scen <- read_excel(path = MatisseFiles$sortie_3me_xl, sheet = S)))
  ThreeME <- 
    scen %>%
    select(-Def) %>%
    gather(key = year, value = value, -c(1))
  ThreeMe_year <- ThreeME %>% filter(year %in% c(2010,horizon))
  Ext_3ME <- ThreeMe_year[grep("^EXP_BUIL_H01_C._22.*$", ThreeMe_year$Var),] %>% mutate(type = "fio") %>% select(-Var)
  Ext_3ME <- rbind(Ext_3ME, ThreeMe_year[grep("^EXP_BUIL_H01_C._23.*$", ThreeMe_year$Var),] %>% mutate(type = "ele") %>% select(-Var))
  Ext_3ME <- rbind(Ext_3ME, ThreeMe_year[grep("^EXP_BUIL_H01_C._24.*$", ThreeMe_year$Var),] %>% mutate(type = "gaz") %>% select(-Var))
  
  Sum_3ME <- Ext_3ME %>%
    mutate(type_year = paste(type,year,sep="_")) %>%
    group_by(type_year) %>%
    summarise(sum_Me = sum(value)) %>%
    mutate(type = substr(type_year,1,3),  year = substr(type_year,5,9)) %>%
    filter (type == "ele") %>%
    select(-type_year)
  
  
  #Menage 2010 puis calcul des valeurs cible par type d'énergie à l'horizon
  Sum_Mat_2010 <- sum(menage_forme$dep_Elec * menage_forme$pondmen) / 10^6
  Sum_horizon_Mat <- sum(menage_echelle_calibre$dep_Elec * menage_echelle_calibre$pondmen)/ FC$A02 / 10^6
  
  while(abs(Sum_horizon_Mat/Sum_Mat_2010 - Sum_3ME[2,"sum_Me"]/Sum_3ME[1,"sum_Me"])>0.001){
    
    #Facteur efficacité elec
    effic_ele <- as.numeric(Sum_3ME[2,"sum_Me"]/Sum_3ME[1,"sum_Me"]*Sum_Mat_2010/Sum_horizon_Mat)
    print(paste("Effic_ele =",effic_ele))
    print(paste("3MEho =",Sum_3ME[2,"sum_Me"]))
    print(paste("3ME2010 =",Sum_3ME[2,"sum_Me"]))
    print(paste("Mat2010 =",Sum_Mat_2010))
    print(paste("Matho =",Sum_horizon_Mat))
    
    
    solde<- 
      menage_echelle_calibre %>%
      mutate(solde=0)%>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
      select(ident_men,solde)
  
    #Ajustement efficacité domicile et solde
    usages <- c("ECS", "chauff", "clim", "Cuisson", "ecl", "ElecSpe")
    sources <- c("Elec")
    for(so in sources){
      for(us in usages){
        my_col <- paste(so,us,sep="_")
        if(my_col %in% names(menage_echelle_calibre)){
          solde["solde"] <-  solde$solde + menage_echelle_calibre[,my_col] * as.numeric(effic_ele - 1)
          menage_echelle_calibre[,my_col] <- menage_echelle_calibre[,my_col] * effic_ele
        }      
      }
      menage_echelle_calibre[paste("dep",so,sep="_")] <- menage_echelle_calibre[paste("dep",so,sep="_")] * effic_ele
    }
    print(paste("Applied elec efficiency to calibrate to 3ME ", percent(1-effic_ele, accuracy = 0.01),sep=""))
    
    #Reventil
    menage_echelle_calibre <- Ventil_solde(solde,menage_echelle_calibre,step="REHAB")
    Sum_horizon_Mat <- sum(menage_echelle_calibre$dep_Elec * menage_echelle_calibre$pondmen)/ FC$A02 / 10^6
    
  }
}

menage_echelle_calibre_ele <- menage_echelle_calibre
save(menage_echelle_calibre_ele,file = MatisseFiles$menage_echelle_final_calibre_ele_rd)

