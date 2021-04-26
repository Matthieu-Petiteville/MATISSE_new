# OBJECTIF 
# A partir du fichier appmen_depensesactiv_2010, recomposition des valeurs source_usage (eg "Elec_ECS") par ménage
# 
# Vocabulaire 
##############################################################################
# source = source d'énergie => Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain"
# usage : usages=c("ECS","chauff","clim","Cuisson","ecl","ElecSpe")
# activité : sommeil, trav_etud, trajet, etc
##############################################################################



# Libraries ---------------------------------------------------------------

suppressMessages(library(reshape2, warn.conflicts=F , quietly = T))
suppressMessages(library(tidyverse, warn.conflicts=F , quietly = T))
suppressMessages(library(plyr, warn.conflicts=F , quietly = T))
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_bonus_energies_kwh.R",sep=""))


# Data -----------------------------------------------------------------

load(MatisseFiles$appmen_intensites_2010_rd)
# NB : dans appmen_intensites : dépenses détaillées par source_activite et par usage_activite. Variables souhaitée : source_usage. 
load(MatisseFiles$menage_forme_2_rd)
# compute_savings_rate_export(menage_forme)


# Sources, usages, activités --------------------------------------------

usages <- c("ECS", "chauff", "clim", "Cuisson", "ecl", "ElecSpe")
activites <- c("sommeil", "physio", "repas_dom", "repas_hors", "trav_etu", "achats", 
            "travdom_alim", "travdom_vetem", "travdom_maison", "assistance", "loisirs_lecture",
            "loisirs_tele", "pleinair_sport", "trajets_domtrav", "trajets_autres")
sources <- c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
reventil_sources <- paste("reventil", sources, sep="_")
dep_sources <- paste("dep", sources, sep="_")


# Préparation données -----------------------------------------------------

# Selection des ménages en commun dans appmen_intensities et menage_forme
appmen_intensites <- appmen_intensites[which(appmen_intensites$ident_men %in% intersect(menage_forme$ident_men, appmen_intensites$ident_men)), ]


# Somme dépenses par activité, source, usage --------------------------------------------

# Selection des colonnes "ident_men" et de la forme "usages_activite"
dep_usage_activite <- appmen_intensites[c(1, 187:257)] 

# Selection des colonnes "ident_men" et de la forme "source_activite"
dep_source_activite <- appmen_intensites[c(1,258:347)]

# Pour chacun des listes source et usage, sommer toutes les activites en dépenses énergétiques.
for (act in activites){
  dep_usage_activite[act] <- 
    dep_usage_activite %>% 
    select(contains(act)) %>% 
    rowSums()
  dep_source_activite[act] <- 
    dep_source_activite %>% 
    select(contains(act)) %>% 
    rowSums()
}

# Sommer pour chaque ménage les dépenses pour chaque source d'énergie
for (source in sources){
  dep_source_activite[source] <- dep_source_activite %>% 
    select(contains(source)) %>% 
    rowSums()  
}

# Sommer pour chaque ménage les dépenses pour chaque usage d'énergie (non exhaustif)
for (usage in usages){
  dep_usage_activite[usage] <- 
    dep_usage_activite %>% 
    select(starts_with(usage)) %>% 
    rowSums()
}


# Dépense énergétique ----------------------------------------------------------------

#On a vérifié que la somme sur les activités étaient les mêmes dont on excluent les act de dep_usages_activites, 
dep_usage_activite2 <-
  dep_usage_activite %>% 
  select(- all_of(activites))

dep_ener<-
  dep_source_activite %>% 
  left_join(dep_usage_activite2 , by = c("ident_men"))


# Ventilation -------------------------------------------------------------

# OBJECTIF : obtenir des variables sources_usages, typiquement : Fuel_Cuisson ou Elec_chauff
# PRINCIPE : on multiplie les dépenses liées à un usage et une activité
# (le chaffage pour le sommeil) qu'on multiplie par le pourcentage lié à une source d'énergie (gaz et sommeil) : on somme cette quantité pour toutes les activités. 
# Les seules exceptions sont ElecSpé, Clim et ecl puisque ventiler des dépenses d'ElecSpé sur de l'Urbain ou du Fuel n'a pas de sens

# la quantité d'ElecSpé est retirée de la consommation électrique des ménages 
# on suppose que ElecSpé, Clim et ECL sont nécessairement en ELEC, 
# on va donc ventiler les autres usages sur le reste d'Elec  
dep_ener$Elec<-
  dep_source_activite$Elec - 
  dep_usage_activite$ElecSpe - 
  dep_usage_activite$clim - 
  dep_usage_activite$ecl

dep_ener$Elec_ElecSpe <- dep_usage_activite$ElecSpe
dep_ener$Elec_clim <- dep_usage_activite$clim
dep_ener$Elec_ecl <- dep_usage_activite$ecl
usages_bis <- c("ECS", "chauff", "Cuisson") #exclusion ElecSpe, Clim, ecl

# pour sommer nécessité d'enlever les "NA" et de les remplacer par des 0
dep_ener[is.na(dep_ener)] <- 0

# On crée des séries artificielles à zéro pour s'assurer de la bonne exécution des boucles (tous les couples usages_activités existent)
for (act in activites){
  for (usage in usages){
    usage_act <- paste(usage, act, sep="_")
    if (!usage_act %in% colnames(dep_ener)){
      dep_ener[usage_act]<-0
    }
  }
}

#fonction inverse qui renvoie 0 si le dénominateur est nul
inverse <- function(x) {ifelse(x == 0, 0, 1/x)}
#On inverse les totaux d'activités pour la simplicité des calculs ci-après
for (act in activites){
  source_act_list <- c()
  for (source in sources){
    source_act <- paste(source, act, sep="_")
    source_act_list <- c(source_act_list, source_act)
  }
  #correspond aux dépenses énergétiques pour cette catégories hors ElecSpe, clim et ecl, qu'il nous reste à ventiler sur un usage
  dep_ener[act] <- 
    rowSums(dep_ener[source_act_list]) -
    dep_ener[paste("ElecSpe", act, sep="_")] -
    dep_ener[paste("clim", act, sep="_")] -
    dep_ener[paste("ecl", act, sep="_")]
  
  dep_ener[paste(act,"bis", sep="_")] <- apply(dep_ener[act], 1, inverse) #on inverse la quantité pour le calcul du ratio source_act/act
}

list_source_usage <- c("Elec_ElecSpe","Elec_clim","Elec_ecl") #déjà traités
for (usage in usages_bis){ #usages_bis = ECS, chauff, Cuisson
  for (source in sources){
    source_usage <- paste(source, usage, sep="_")
    Table <- dep_ener
    list_table <- c()
    for (act in activites){
      usage_act <- paste(usage, act, sep="_")
      source_usage_act <- paste(source, usage, act, sep="_")
      source_act <- paste(source, act, sep="_")
      
      # X = ratio de l'usage d'une source d'énergie pour une activité sur le total de cette activité e.g. xx% de l'énergie nécessaire au "repas_dom" provient de l'électricité. 
      if(source == "Elec"){ # on traite l'Elec à part parce qu'il faut retrancher les usages nécessairement électrifiés
        X <- (Table[source_act] -
              Table[paste("ElecSpe", act, sep="_")] -
              Table[paste("clim", act, sep="_")] -
              Table[paste("ecl", act, sep="_")]) * # Il reste donc Elec_ECS_act+Elec_chauff_act+Elec_cuisson_act
              Table[paste(act, "bis", sep="_")]
      }else{
        X <- Table[source_act] * Table[paste(act, "bis", sep="_")]  #e.g. Fuel_physio/physio
      }
      
      Table[paste(source, act, "ratio", sep="_")] <- X
      
      # On fait l'hypothèse que les différents usages liés à une activité utilisent le même mix énergétique que cette activité
      # Fuel_chauff_physio =chauff_physio * Fuel_physio/physio, id est : si 48% de l'énergie physio est fourni par le fuel alors 48% du chauff_physio est fournit par le fuel également
      ifelse(usage_act %in% colnames(Table),
             Table[source_usage_act] <- Table[usage_act] * Table[paste(source, act, "ratio", sep="_")], #if yes
             Table[source_usage_act] <- 0 # if no
      )
      list_table <- c(list_table, source_usage_act)
    }
    
    # print(source_usage)
    list_source_usage <- c(list_source_usage, source_usage)
    dep_ener[source_usage] <- rowSums(Table[list_table]) # on somme tous les source_usage_act pour chaque activité

  }
}

# on rajoute de nouveau ElecSpe au total de l'elec (cf ligne 170)
dep_ener$Elec <- dep_ener$Elec + dep_ener$ElecSpe + dep_ener$clim + dep_ener$ecl
# on rajoute de nouveau ElecSpe aux totaux d'activités # cf ligne 207
for (act in activites){
  source_act_list <- c()
  for (source in sources){
    source_act <- paste(source, act, sep="_")
    source_act_list <- c(source_act_list, source_act)
  }
  dep_ener[act] <- rowSums(dep_ener[source_act_list])
}

#Format for later use
dep_ener_2010 <- dep_ener
dep_ener <- dep_ener_2010[c("ident_men", list_source_usage, sources)]
menage_forme<-
  menage_forme %>%
  left_join(dep_ener %>% select("ident_men",all_of(list_source_usage)), by = "ident_men")
menage_forme$dep_Elec_verif <- dep_ener$Elec
menage_forme$dep_Gaz_verif <- dep_ener$Gaz
menage_forme$dep_GPL_verif <- dep_ener$GPL
menage_forme$dep_Solides_verif <- dep_ener$Solides
menage_forme$dep_Fuel_verif <- dep_ener$Fuel
menage_forme$dep_Urbain_verif <- dep_ener$Urbain
compute_savings_rate_export(menage_forme) #0.1206374
# compute_share_export(menage_forme)


# Save --------------------------------------------------------------------

save(list_source_usage, file = MatisseFiles$source_usage_rd)
save(dep_ener_2010, file = MatisseFiles$dep_ener_2010_rd)
save(menage_forme,file=MatisseFiles$menage_forme_3_rd)


# Clean -------------------------------------------------------------------

suppressWarnings(rm(Table, list_table, appmen_intensites, dep_ener, dep_ener_2010, dep_source_activite,
                    dep_usage_activite, dep_usage_activite2, menage_forme, X, act, activites, dep_sources,
                    list_source_usage, reventil_sources, source, source_act, source_act_list, source_usage,
                    source_usage_act, sources, usage, usage_act, usages, usages_bis, dep_ener_verif1, 
                    dep_source, menage_forme_ener))
gc()


# Verifications -----------------------------------------------------------

# Cohérence avec menage_forme (dep_elec, dep_gaz, etc) => OUI
# table(dep_source_activite$Elec-menage_forme$dep_Elec<10^-10)
# table(dep_source_activite$Elec==menage_forme$dep_Elec)
# table(dep_source_activite$Gaz==menage_forme$dep_Gaz)
# table(dep_source_activite$GPL==menage_forme$dep_GPL)
# table(dep_source_activite$Fuel==menage_forme$dep_Fuel)
# table(dep_source_activite$Solides==menage_forme$dep_Solides)
# # table(dep_source_activite$Solides-menage_forme$dep_bois-menage_forme$dep_charbon<10^(-12.8)) # les 217 erreurs <e-13
# table(dep_source_activite$Urbain==menage_forme$dep_Urbain)


# Somme des activités par les sources
# sum(dep_source_activite[activites]) # en €
# [1] 13932919

# Somme des activités par les usages
# sum(dep_usage_activite[activites])  # en €
# [1] 13932919

# Somme des dépenses par les usages
# sum(rowSums(dep_usage_activite[usages])) # en €
# [1] 13 932 919

# Somme des dépenses par les sources
# sum(rowSums(dep_source_activite[sources])) # en €
# [1] 13 932 919

# for (source in sources){
#   print(source)
#   print(sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with(source))))==sum(dep_ener[source]))
# }
# sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with("Elec"))))
# # [1] 7028660
# sum(dep_ener$Elec)
# # [1] 7028660
# sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with("Gaz"))))
# # [1] 7827271
# sum(dep_ener$Gaz)
# # [1] 7028660
# sum(rowSums(dep_ener[list_source_usage] %>% select(starts_with("Fuel"))))
# [1] 2132795
# sum(dep_ener$Fuel)
# # [1] 2132795


# sum(rowSums(dep_ener[list_source_usage]))
# sum(rowSums(dep_ener[usages]))
# sum(rowSums(dep_ener[sources]))
# sum(rowSums(dep_ener[activites]))
# Somme ok : 13932919


# 
# for (source in sources){
#   print(source)
#   print(sum(rowSums(dep_ener[source])))
# }


# > sum(rowSums(dep_ener[list_source_usage]))
# [1] 13932919
# > sum(rowSums(dep_ener[usages]))
# [1] 13932919
# > sum(rowSums(dep_ener[sources]))
# [1] 13932919
# > sum(rowSums(dep_ener[activites]))
# [1] 13932919
# 
# [1] "Elec"
# [1] 7030068
# [1] "Gaz"
# [1] 3584273
# [1] "GPL"
# [1] 643398
# [1] "Fuel"
# [1] 2132795
# [1] "Solides"
# [1] 536834
# [1] "Urbain"
# [1] 8587

# dep_ener_verif1<-dep_ener %>% select(ident_men, list_source_usage)
# for (source in sources){
#   dep_ener_verif1[source]<-rowSums(dep_ener_verif1 %>% select(contains(source)))
#   print(source)
#   print(table(dep_ener_verif1[source]-dep_ener[source]<10^(-5)))
# 
#   dep_source=paste("dep",source,sep="_")
#   print(table(dep_ener[source]-menage_forme[dep_source]<10^(-9)))
# }
# 
# dep_ener_verif1<-dep_ener %>% select(ident_men, list_source_usage)
# # verif avec menage_forme => aucun ménage n'a perdu ou gagné de l'énergie, on l'a juste répartie => OK
# for (source in sources){
#   dep_source=paste("dep",source,sep="_")
#   dep_ener_verif1[source]<-rowSums(dep_ener_verif1 %>% select(contains(source)))
#   print(source)
#   print(table(dep_ener_verif1[source]-menage_forme[dep_source]<10^(-9)))
# }
# 

# # Pas de valeurs négatives dans cette sélection
# table(dep_ener<0)




# verif
# menage_forme %>% mutate(dep_v=dep_Urbain+dep_Solides+dep_GPL+dep_Gaz+dep_Elec+dep_Fuel)%>% summarise(sum(dep_v))
# menage_forme %>% mutate(dep_verif=dep_Urbain_verif+dep_Solides_verif+dep_GPL_verif+dep_Gaz_verif+dep_Elec_verif+dep_Fuel_verif)%>% summarise(sum(dep_verif))

## Avec pondmen
# menage_forme %>% mutate(dep_v=dep_Urbain+dep_Solides+dep_GPL+dep_Gaz+dep_Elec+dep_Fuel)%>% summarise(sum(dep_v*pondmen))
# menage_forme %>% mutate(dep_verif=dep_Urbain_verif+dep_Solides_verif+dep_GPL_verif+dep_Gaz_verif+dep_Elec_verif+dep_Fuel_verif)%>% summarise(sum(dep_verif*pondmen))


# menage_forme_ener <- energie_dom_surf(menage_forme)
# menage_forme_ener %>%
#   dplyr::group_by(DPE_pred) %>%
#   summarise("Mwh_m2" = weighted.mean(x = ener_dom_surf, w = pondmen))
# menage_forme_ener %>% group_by(DPE_pred)%>%
#   summarise("Mwh_m2"=weighted.mean(x=energie_tot_surf , w = pondmen))
# 
