
# Objectif : # Conversion des dépenses énergétiques en consommation en volume d'énergie (kWh) en volumes d'énergie



# Libraries ---------------------------------------------------------------

suppressMessages(library(reshape2 , warn.conflicts=F , quietly = T))
suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
suppressMessages(library(readxl , warn.conflicts=F , quietly = T))


energie_dom_surf<-function(menage){
  
  
  # Data -------------------------------------------------------------

  load(MatisseFiles$source_usage_rd)
# Import des prix d'énergie par classe de ménage : en €/MWh
  prix_classe <- read.csv2(MatisseFiles$prix_class_csv, header = TRUE, sep = ";",dec = ".", fill = TRUE)
  

  # Prepare data prix/energie ----------------------------------------

  # prix energie par classe de ménage : dans l'ordre : par quintile, type de ménage et type de logement (individuel ou collectif)
  prix_classe <- arrange(prix_classe,quintile,typmen_corr,MI)

  # MISE A L'ECHELLE PRIX ENERGIE PAR CLASSE --------------------------------
  
  # Hypothèse : les prix des énergies varient de la même façon pour toutes les classes de ménage entre 2010 et 2025. 
  prix_classe_horizon <- prix_classe
  # A02
  prix_classe_horizon$prix_elec <- prix_classe$prix_elec
  # A03
  prix_classe_horizon$prix_gaz <- prix_classe$prix_gaz 
  # A04
  prix_classe_horizon[c("prix_fuel", "prix_gpl", "prix_bois", "prix_chaleur")] <- 
    prix_classe[c("prix_fuel", "prix_gpl", "prix_bois", "prix_chaleur")]
  
  
  
  # CLASSER MENAGES PAR CLASSE ----------------------------------------------
  
  
  # Matrice des prix de chaque énergie pour chaque classe
  price_cols <- c("prix_elec", "prix_gaz", "prix_fuel", "prix_gpl", "prix_bois", "prix_chaleur")
  prix_classe_mat <- data.matrix(prix_classe_horizon[, price_cols], rownames.force = NA)
  
  # Attribution d'un numéro de classe de ménage à chaque ligne de prix_classe (de 1 à 60)
  menage$classe_men <- with(menage, as.integer(interaction(MI_corr, typmen_corr, quintileuc)))
  menage$classe_men <- as.factor(menage$classe_men)

  # Traduction de la variable classe_men en matrice d'indicatrices d'appartenance à chacune des 60 classes
  dummies_classe_men <- model.matrix(~ classe_men, 
                                     data = menage, 
                                     contrasts.arg = list(
                                         classe_men = contrasts(
                                         menage$classe_men,
                                         contrasts = F)))
  
  
  #Suppresssion de la colonne "Intercept", résidu de la méthode de régression employée pour construire les indicatrices
  dummies_classe_men <- dummies_classe_men[, -1] 

  # Prix énergie par ménage -------------------------------------------------
  
  
  # Produit matriciel entre les indicatrices et les classes (n ménages x 60 classes) %*% (60 classes x 6 sources énergie)
  prix_menages_horizon <- as.data.frame(dummies_classe_men  %*% prix_classe_mat)
  prix_menages_horizon_bis <- as.data.frame(prix_menages_horizon)
  
  # Rajout colonne "ident_men" pour la fusion avec menage
  prix_menages_horizon <- cbind(menage$ident_men, prix_menages_horizon_bis)
  # renommer les colonnes
  colnames(prix_menages_horizon) <- c("ident_men",
                                    "prix_Elec",
                                    "prix_Gaz",
                                    "prix_Fuel",
                                    "prix_GPL",
                                    "prix_Solides",
                                    "prix_Urbain")
  
  # Rajout des prix et pondération de chaque ménage dans menage
  menage <- 
    menage %>% 
    left_join(prix_menages_horizon,by="ident_men")
  
  # Volumes énergie/ménage ------------------------------------------------
  
  # Pour convertir les dépenses en volumes, division par le prix moyen à l'horizon de chaque source d'énergie.
  menage$vol_Elec <- menage$dep_Elec / menage$prix_Elec
  menage$vol_Gaz <- menage$dep_Gaz / menage$prix_Gaz
  menage$vol_GPL <- menage$dep_GPL / menage$prix_GPL
  menage$vol_Fuel <- menage$dep_Fuel / menage$prix_Fuel
  menage$vol_Solides <- menage$dep_Solides / menage$prix_Solides
  menage$vol_Urbain <- menage$dep_Urbain / menage$prix_Urbain
  
  menage$vol_tot <- 
    menage %>%
    select(starts_with("vol_")) %>% 
    rowSums() 

  
  # Volumes énergie macro-économiques ----------------------------------------

  menage %>% 
    summarise(sum(vol_tot*pondmen))

  
  # Energie surfacique totale -----------------------------------------------

  menage <- 
    menage %>%
    mutate(energie_tot_surf=ifelse(surfhab_d>0,vol_tot/surfhab_d,0))
  
  
  menage %>% 
    summarise(weighted.mean(x=energie_tot_surf,w=pondmen,na.rm=T))
  # 0.1677032 en 2010

  # ENERGIE DOMESTIQUE ------------------------------------------------------
  
  dep_source_usage<-menage[c("ident_men",list_source_usage)]
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Elec")))]/menage$prix_Elec
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Gaz")))]/menage$prix_Gaz
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("GPL")))]/menage$prix_GPL
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Fuel")))]/menage$prix_Fuel
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Solides")))]/menage$prix_Solides
  
  dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]<-
    dep_source_usage[colnames(dep_source_usage %>% select(contains("Urbain")))]/menage$prix_Urbain
  
  
  
  
  # Energie de chauffage
  menage$ener_chauff<-
    dep_source_usage %>% select(contains("_chauff")) %>% rowSums()
  
  # Energie Eau Chaude Sanitaire
  menage$ener_ECS<-
    dep_source_usage %>% select(contains("_ECS")) %>% rowSums()
  
  # Energie de climatisation
  menage$ener_clim<-
    dep_source_usage %>% select(contains("_clim")) %>% rowSums()
  
  
  # Energie domestique (chauff+ECS+clim)
  menage$ener_dom<-
    menage$ener_chauff+
    menage$ener_ECS+
    menage$ener_clim
  
  
  # Energie domestique surfacique
  # menage$ener_dom_surf<-dep_source_usage$ener_dom/menage$surfhab_d
  menage <- 
    menage %>%
    mutate(ener_dom_surf=ifelse(surfhab_d>0,ener_dom/surfhab_d,0))
  
  
  menage %>% summarise(weighted.mean(x=ener_dom_surf,w=pondmen,na.rm=T)) #MWh
  # EN 2035 AMS avant rénovation
  #  0.1305881 (+22% par rapport à 2010, cohérent avec volume total)
  # [1] 
  # EN 2010
  # [1] 0.1070417 
  
  # Tracer la distribution d'énergie surfacique
  # g<-ggplot(dep_source_usage,aes(x=dep_source_usage$ener_dom_surf*1000))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  #   geom_density(alpha=.7, fill="#FF6666")+xlim(0,1000)
  # g
  
  
  
  
  
  # EXPORTER DONNEES ENER_DOM  ----------------------------------------------
  
  # menage_2025<-dep_source_usage %>% select(ident_men,list_source_usage,ener_dom_surf)
  # menage_2025[list_source_usage]<-menage_2025[list_source_usage]*1000
  # menage<-
  #   menage %>%
  #   left_join(dep_source_usage %>% select(ident_men,ener_dom_surf),by="ident_men")
  
  menage_ener_dom<-menage %>% select(ident_men,ener_dom_surf,ener_dom,vol_tot,energie_tot_surf,DPE_pred,surfhab_d,pondmen)
  
  return(menage_ener_dom)
  
  
  # save(menage_2025,file="2025/menage_2025.RData")
  
  
  # SUCCESS -----------------------------------------------------------------
  
  print("calc_energie_kWh_m2 : SUCCESS")
}
