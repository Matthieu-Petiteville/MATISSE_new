


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/calc_ems.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep="")) # importe  bdd 3 variables : ident_men,ener_dom_surf,ener_dom
source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/Econometrie_solde_budg_Logement.R",sep=""))
source(paste(M_home,"/Step_5_AlignOn3ME/1_func_SelectBascule.R",sep=""))


# Data --------------------------------------------------------------------

if(!exists("align_bascule_on_3ME")){align_bascule_on_3ME <- F}
load(MatisseFiles$menage_echelle_final_rd)

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
            select(-type_year)
    
    #Menage 2010 puis calcul des valeurs cible par type d'énergie à l'horizon
    menage_col <- colnames(menage_echelle)
    
    Sum_Mat <- data.frame(sum_Me = sum((menage_forme$dep_Fuel + menage_forme$dep_GPL) * menage_forme$pondmen) / 10^6,
                          type = "fio",
                          year = 2010)
    Sum_Mat <- rbind(Sum_Mat,
                     data.frame(sum_Me = sum(menage_forme$dep_Elec * menage_forme$pondmen) / 10^6,
                                type = "ele",
                                year = 2010))
    Sum_Mat <- rbind(Sum_Mat,
                     data.frame(sum_Me = sum((menage_forme$dep_Gaz + menage_forme$dep_Urbain + menage_forme$dep_Solides) * menage_forme$pondmen) / 10^6,
                                type = "gaz",
                                year = 2010))
    
    #Sum horizon
    Sum_horizon_Mat <- data.frame(sum_Me = sum((menage_echelle$dep_Fuel + menage_echelle$dep_GPL) * menage_echelle$pondmen)/  FC$A07 / 10^6,
                          type = "fio",
                          year = horizon)
    Sum_horizon_Mat <- rbind(Sum_horizon_Mat,
                     data.frame(sum_Me = sum(menage_echelle$dep_Elec * menage_echelle$pondmen)/ FC$A02 / 10^6,
                                type = "ele",
                                year = horizon))
    Sum_horizon_Mat <- rbind(Sum_horizon_Mat,
                     data.frame(sum_Me = sum((menage_echelle$dep_Gaz + menage_echelle$dep_Urbain + menage_echelle$dep_Solides) * menage_echelle$pondmen)/ FC$A03 / 10^6,
                                type = "gaz",
                                year = horizon))
    
    for(ener in Sum_Mat$type){
      sub_3ME <- Sum_3ME[which(Sum_3ME$type == ener),]
      sub_Mat <- Sum_Mat[which(Sum_Mat$type == ener),]
      Sum_Mat <- rbind(Sum_Mat,
                       data.frame(sum_Me = sub_Mat[1,"sum_Me"] * sub_3ME[2,"sum_Me"]/sub_3ME[1,"sum_Me"],
                                  type = ener ,
                                  year = horizon))
    }
    
    Sum_horizon_Mat <- Sum_horizon_Mat %>%
                      left_join(Sum_Mat %>%
                                  filter(year == horizon) %>%
                                  mutate(target_sum_Me = sum_Me) %>%
                                  select(target_sum_Me, type), by = "type")
    Sum_horizon_Mat <- Sum_horizon_Mat %>%
                        mutate(diff_Me = sum_Me - target_sum_Me) %>%
                        mutate(left_to_bascule = diff_Me)
    
  #Si redistribution autre que forfait, on utilise la somme des m² basculés pour prévoir le nombre de bascules
  if(redistribution != "forfait"){
    if(!file.exists(MatisseFiles$menage_calibre_forfait_rd)){
      print("Careful : missing the file for reference bascule : redistribution forfait")
    }else{
      load(MatisseFiles$menage_calibre_forfait_rd)
      if(!("bascule_fio" %in% colnames(menage_echelle_calibre)) || !("bascule_gaz" %in% colnames(menage_echelle_calibre))){
        print("Careful : missing column bascule_fio/bascule_gaz in ref file for redistribution forfait")
      }else{
        tot_basc_m2 <- menage_echelle_calibre %>%
          group_by(bascule_fio, bascule_gaz) %>%
          summarise(sum_m2 = sum(pondmen * surfhab_d), .groups = 'drop')
        target_basc_m2 <<- data.frame(type = c("bascule_fio",
                                              "bascule_gaz"), 
                                     sum_m2 = c(sum(tot_basc_m2$sum_m2[which(tot_basc_m2$bascule_fio == 1)]),
                                                sum(tot_basc_m2$sum_m2[which(tot_basc_m2$bascule_gaz == 1)])))
        target_basc_m2$left_to_bascule <- target_basc_m2$sum_m2
      }
    }
  }
}
  

# MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------

if(align_bascule_on_3ME){
  # Import des prix d'énergie par classe de ménage : en €/MWh
  prix_classe <- read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";",dec = ".", fill = TRUE)
  # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
  prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025.
  prix_classe_horizon<-prix_classe
  # A02
  prix_classe_horizon$prix_elec<- prix_classe$prix_elec * FC$A02
  # A03
  prix_classe_horizon$prix_gaz<- prix_classe$prix_gaz * FC$A03
  # A04
  prix_classe_horizon[c("prix_fuel","prix_gpl")]<- prix_classe[c("prix_fuel","prix_gpl")]* as.numeric(FC$A04)
  prix_classe_horizon[c("prix_bois","prix_chaleur")]<- prix_classe[c("prix_bois","prix_chaleur")]* as.numeric(FC$A03)
  # Matrice des prix de chaque énergie pour chaque classe
  prix_classe_mat <- data.matrix(prix_classe_horizon[,c("prix_elec","prix_gaz","prix_fuel","prix_gpl","prix_bois","prix_chaleur")], rownames.force = NA)
  
  
  # Attribution d'un numéro de classe de ménage à chaque ligne de appmen (de 1 à 60)
  menage_echelle$classe_men <- 
    with(
      menage_echelle,
      as.integer(interaction(MI_corr, typmen_corr, quintileuc)))
  
  menage_echelle$classe_men <- 
    as.factor(menage_echelle$classe_men)
  
  
  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  dummies_classe_men <- model.matrix(~ classe_men, 
                                     data = menage_echelle, 
                                     contrasts.arg = list(
                                       classe_men = contrasts(
                                         menage_echelle$classe_men,
                                         contrasts = F)
                                     )
  )
  
  #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  dummies_classe_men <- dummies_classe_men[,-1] 
  
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages_horizon <- as.data.frame(dummies_classe_men %*% prix_classe_mat)
  prix_menages_horizon_bis<-as.data.frame(prix_menages_horizon)
  # Rajout colonne "ident_men" pour la fusion avec det_ener
  prix_menages_horizon<-cbind(menage_echelle$ident_men,prix_menages_horizon_bis)
  # renommer les colonnes
  colnames(prix_menages_horizon)<-c("ident_men",
                                    "prix_Elec",
                                    "prix_Gaz",
                                    "prix_Fuel",
                                    "prix_GPL",
                                    "prix_Solides",
                                    "prix_Urbain")
  
  # Rajout des prix et pondération de chaque ménage dans dep_ener
  menage_echelle <- menage_echelle %>% left_join(prix_menages_horizon,by="ident_men")
}

# Choose bascule -------------------------------------------------------------
if(align_bascule_on_3ME){ 
  menage_echelle <- 
    menage_echelle %>% 
    mutate(bascule_fio = 0, bascule_gaz = 0) %>%
    mutate(dep_e_fio = (dep_Fuel + dep_GPL) * pondmen /  FC$A07) %>%
    mutate(dep_e_ele = dep_Elec * pondmen /  FC$A02) %>%
    mutate(dep_e_gaz = (dep_Gaz + dep_Urbain) * pondmen/  FC$A03) %>%
    mutate(DPE_jump = - match(DPE_horizon,LETTERS) + match(DPE_pred,LETTERS)) %>%
    mutate(dep_surf_fio = (dep_Fuel + dep_GPL) /  FC$A07 / surfhab_d) %>%
    mutate(dep_surf_gaz = (dep_Gaz) /  FC$A04 / surfhab_d)
  menage_echelle$dep_surf_fio[which(is.na(menage_echelle$dep_surf_fio))] <- 0
  menage_echelle$dep_surf_gaz[which(is.na(menage_echelle$dep_surf_gaz))] <- 0
  
  # Bascule FioulOnly first puis GazOnly
  for(ener in c("fio","gaz")){
    print(ener)
    ener_ind <- which(Sum_horizon_Mat$type == ener)
    dep_ener <- paste("dep_e_",ener,sep="")
    bascule_ener <- paste("bascule_", ener, sep="")
    dep_surf_ener <- paste("dep_surf_", ener, sep = "")
    align_yearnew_bascule_ener <- align_yearnew_bascule[[ener]]
    
    #On bascule par classe DPE finale 
    for(classe in align_class_bascule){
      sub_men_ech <- menage_echelle %>% filter(DPE_horizon == classe)
      sub_men_ech$dep_surf_to_rank <- -sub_men_ech[,dep_surf_ener]
      
      select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat, scenario_classement_bascule, redistribution)
      Sum_horizon_Mat <- select_res$Sum_horizon_Mat
      sub_men_ech     <- select_res$sub_men_ech
      nb_basc         <- select_res$nb_basc
      target_basc_m2  <- select_res$target_basc_m2
      
      print(paste("Classe ",classe," : ",nb_basc," bascules out of ", nrow(sub_men_ech)," menages",sep=""))
      men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
      menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
      menage_echelle[men_to_bascule_idx,dep_ener] <- 0
    }
    
    
    #On bascule par nouvelles constructions
    for(year_new in horizon:align_yearnew_bascule_ener){
      sub_men_ech <- menage_echelle %>% filter(year_neuf == year_new)
      sub_men_ech$dep_surf_to_rank <- -sub_men_ech[,dep_surf_ener]
      
      select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat, scenario_classement_bascule, redistribution)
      Sum_horizon_Mat <- select_res$Sum_horizon_Mat
      sub_men_ech     <- select_res$sub_men_ech
      nb_basc         <- select_res$nb_basc
      target_basc_m2  <- select_res$target_basc_m2
      
      print(paste("NewBuild year :", year_new," : ",nb_basc," bascules out of ", nrow(sub_men_ech)," menages",sep=""))
      men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
      menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
      menage_echelle[men_to_bascule_idx,dep_ener] <- 0
      
    }
    
    #On bascule par saut de classes DPE décroissantes
    for(jump in align_jump_bascule){
      sub_men_ech <- menage_echelle %>% filter(DPE_jump == jump)
      sub_men_ech$dep_surf_to_rank <- -1 * sub_men_ech[,dep_surf_ener]
      
      select_res <- select_bascule(sub_men_ech, Sum_horizon_Mat, scenario_classement_bascule, redistribution)
      Sum_horizon_Mat <- select_res$Sum_horizon_Mat
      sub_men_ech     <- select_res$sub_men_ech
      nb_basc         <- select_res$nb_basc
      target_basc_m2  <- select_res$target_basc_m2
      
      print(paste("DPE Jump :", jump," : ",nb_basc," bascules out of ", nrow(sub_men_ech)," menages",sep=""))
      men_to_bascule_idx <- match(sub_men_ech$ident_men[which(sub_men_ech[,bascule_ener] == 1)], menage_echelle$ident_men)
      menage_echelle[men_to_bascule_idx,bascule_ener] <- 1
      menage_echelle[men_to_bascule_idx,dep_ener] <- 0  
    }
  }
}

#Check
# sub_men_ech[1:10,c("bascule_fio","bascule_gaz","dep_e_fio","dep_e_gaz")]
# nrow(menage_echelle %>% filter(DPE_jump == 0, dep_e_gaz > 1))
# nrow(menage_echelle %>% filter(DPE_jump == 0))
# menage_echelle %>% filter(DPE_jump == 0, dep_e_gaz > 1) %>% summarise(sum(pondmen))
# menage_echelle %>% filter(DPE_jump == 0) %>% summarise(sum(pondmen))


# Do bascule Fioul --------------------------------------------------------
if(align_bascule_on_3ME){
  vec_fio <- c("GPL_ECS","Fuel_ECS","GPL_chauff","Fuel_chauff","GPL_Cuisson","Fuel_Cuisson")
  
  for (x in vec_fio){
    if (str_detect(x,"GPL")){menage_echelle[paste(x,"vol_fio",sep="_")]<-menage_echelle[x]/menage_echelle$prix_GPL}
    if (str_detect(x,"Fuel")){menage_echelle[paste(x,"vol_fio",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Fuel}
  }
  menage_echelle$vol_basc_elec_ECS_fio <- rowSums(menage_echelle %>% select(ends_with("ECS_vol_fio")))
  menage_echelle$vol_basc_elec_chauff_fio <- rowSums(menage_echelle %>% select(ends_with("chauff_vol_fio")))
  menage_echelle$vol_basc_elec_cuisson_fio <- rowSums(menage_echelle %>% select(ends_with("Cuisson_vol_fio")))
  
  menage_echelle <-
    menage_echelle %>%
    mutate(dep_basc_elec_ECS_fio = vol_basc_elec_ECS_fio*prix_Elec)%>%
    mutate(dep_basc_elec_chauff_fio = vol_basc_elec_chauff_fio*prix_Elec)%>%
    mutate(dep_basc_elec_cuisson_fio = vol_basc_elec_cuisson_fio*prix_Elec)
  
  menage_echelle$dep_fio<-rowSums(menage_echelle %>% select(vec_fio))
  
  solde_fio <- 
    menage_echelle %>%
    mutate(solde_fio = dep_basc_elec_ECS_fio + dep_basc_elec_chauff_fio + dep_basc_elec_cuisson_fio - dep_fio) %>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
    mutate_when(bascule_fio == 0, list(solde_fio = 0))%>%
    select(ident_men, solde_fio, bascule_fio)
  
  
  
  menage_echelle <-
    menage_echelle %>%
    mutate_when(
      bascule_fio == 1,
      list(
        Gaz_ECS = Gaz_ECS,
        GPL_ECS = 0,
        Fuel_ECS = 0,
        Solides_ECS = Solides_ECS,
        Urbain_ECS = Urbain_ECS,
        Gaz_chauff = Gaz_chauff,
        GPL_chauff = 0,
        Fuel_chauff = 0,
        Solides_chauff = Solides_chauff,
        Urbain_chauff = Urbain_chauff,
        Gaz_Cuisson = Gaz_Cuisson,
        GPL_Cuisson = 0,
        Solides_Cuisson = Solides_Cuisson,
        Urbain_Cuisson = Urbain_Cuisson,
        Fuel_Cuisson = 0,
        Elec_chauff = Elec_chauff + dep_basc_elec_chauff_fio,
        Elec_ECS = Elec_ECS + dep_basc_elec_ECS_fio,
        Elec_Cuisson = Elec_Cuisson + dep_basc_elec_cuisson_fio,
        dep_Gaz = dep_Gaz,
        dep_GPL = dep_GPL - GPL_ECS - GPL_chauff -  GPL_Cuisson,
        dep_Fuel = dep_Fuel - Fuel_ECS - Fuel_chauff - Fuel_Cuisson,
        dep_Solides = dep_Solides,
        dep_Urbain = dep_Urbain,
        dep_Elec = dep_Elec + dep_basc_elec_chauff_fio + dep_basc_elec_ECS_fio + dep_basc_elec_cuisson_fio
      )
    )
}

# Do bascule Gaz --------------------------------------------------------
if(align_bascule_on_3ME){
  vec_gaz <- c("Gaz_ECS","Urbain_ECS","Gaz_chauff","Urbain_chauff","Gaz_Cuisson","Urbain_Cuisson")
  
  for (x in vec_gaz){
    if (str_detect(x,"Gaz")){menage_echelle[paste(x,"vol_gaz",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Gaz}
    if (str_detect(x,"Urbain")){menage_echelle[paste(x,"vol_gaz",sep="_")]<-menage_echelle[x]/menage_echelle$prix_Urbain}
  }
  menage_echelle$vol_basc_elec_ECS_gaz <- rowSums(menage_echelle %>% select(ends_with("ECS_vol_gaz")))
  menage_echelle$vol_basc_elec_chauff_gaz <- rowSums(menage_echelle %>% select(ends_with("chauff_vol_gaz")))
  menage_echelle$vol_basc_elec_cuisson_gaz <- rowSums(menage_echelle %>% select(ends_with("Cuisson_vol_gaz")))
  
  menage_echelle <-
    menage_echelle %>%
    mutate(dep_basc_elec_ECS_gaz = vol_basc_elec_ECS_gaz*prix_Elec)%>%
    mutate(dep_basc_elec_chauff_gaz = vol_basc_elec_chauff_gaz*prix_Elec)%>%
    mutate(dep_basc_elec_cuisson_gaz = vol_basc_elec_cuisson_gaz*prix_Elec)
  
  menage_echelle$dep_gaz<-rowSums(menage_echelle %>% select(vec_gaz))
  
  solde_gaz <- 
    menage_echelle %>%
    mutate(solde_gaz = dep_basc_elec_ECS_gaz + dep_basc_elec_chauff_gaz + dep_basc_elec_cuisson_gaz - dep_gaz) %>% #<0 => économie si les dépenses équivalentes en élec sont plus faibles que les dépenses en gaz,fuel, solides,etc
    mutate_when(bascule_gaz == 0, list(solde_gaz = 0))%>%
    select(ident_men, solde_gaz, bascule_gaz)
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate_when(bascule_gaz==1,list(Gaz_ECS=0,
                                    GPL_ECS=GPL_ECS,
                                    Fuel_ECS=Fuel_ECS,
                                    Solides_ECS=Solides_ECS,
                                    Urbain_ECS=0,
                                    Gaz_chauff=0,
                                    GPL_chauff=GPL_chauff,
                                    Fuel_chauff=Fuel_chauff,
                                    Solides_chauff=Solides_chauff,
                                    Urbain_chauff=0,
                                    Gaz_Cuisson=0,
                                    GPL_Cuisson=GPL_Cuisson,
                                    Solides_Cuisson=Solides_Cuisson,
                                    Urbain_Cuisson=0,
                                    Fuel_Cuisson=Fuel_Cuisson,
                                    Elec_chauff=Elec_chauff+dep_basc_elec_chauff_gaz,
                                    Elec_ECS=Elec_ECS+dep_basc_elec_ECS_gaz,
                                    Elec_Cuisson=Elec_Cuisson+dep_basc_elec_cuisson_gaz,
                                    dep_Gaz=dep_Gaz - Gaz_ECS - Gaz_chauff - Gaz_Cuisson,
                                    dep_GPL=dep_GPL,
                                    dep_Fuel=dep_Fuel,
                                    dep_Solides=dep_Solides - Solides_ECS - Solides_chauff - Solides_Cuisson,
                                    dep_Urbain=dep_Urbain - Urbain_ECS - Urbain_chauff - Urbain_Cuisson,
                                    dep_Elec=dep_Elec + dep_basc_elec_chauff_gaz + dep_basc_elec_ECS_gaz + dep_basc_elec_cuisson_gaz))
  
  solde <- solde_fio %>%
          left_join(solde_gaz, by = "ident_men") %>%
          mutate(solde = solde_fio + solde_gaz) %>%
          select(c("ident_men","solde"))
}



# Reventilation -----------------------------------------------------------
if(align_bascule_on_3ME){
  menage_echelle <- menage_echelle %>% select(c(menage_col,"bascule_fio","bascule_gaz"))
  menage_echelle_calibre <- Ventil_solde(solde,menage_echelle,step="REHAB")
}else{
  menage_echelle_calibre <- menage_echelle
}
  
save(menage_echelle_calibre,file = MatisseFiles$menage_echelle_final_calibre_rd)

# menage_ener_dom <- energie_dom_surf(menage_echelle_calibre,T)
# menage_echelle_calibre<- 
#   menage_echelle_calibre %>%
#   select(-ener_dom_surf,-ener_dom) %>%
#   left_join(menage_ener_dom %>% select(-MI_corr),by="ident_men")

#Vérifier que enerdom ne change pas

