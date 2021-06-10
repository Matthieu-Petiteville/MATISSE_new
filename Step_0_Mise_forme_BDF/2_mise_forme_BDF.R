# 
# ########## CALCUL AGGREGATS RESSOURCES ET DEPENSES PAR CLASSE DE MENAGES POUR REPONDERATION #########
# 
# Objectif du code : adapter la base BDF et la nomenclature au projet ADEME et THREE_ME, creer des variables calculees


# Libraries -----------------------------------------------------------------

suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
suppressMessages(library(readxl , warn.conflicts=F , quietly = T))
suppressMessages(library(car , warn.conflicts=F , quietly = T))
suppressMessages(library(reshape2 , warn.conflicts=F , quietly = T))
source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))

# Data --------------------------------------------------------------------

#Donnees Brutes BDF 2010
menage <- suppressWarnings(suppressMessages(as.data.frame(read_excel(MatisseFiles$menage_bdf_xl) , stringsAsFactors=F)))
individu <- suppressWarnings(suppressMessages(read_excel(MatisseFiles$indiv_bdf_xl)))
c05 <- suppressMessages(read_csv2(MatisseFiles$c05_bdf_csv))
depmen <- suppressWarnings(suppressMessages(read_csv2(MatisseFiles$depmen_bdf_csv)))

# Typologie de vulnérabilité
typo_vuln <- suppressMessages(read_excel(MatisseFiles$typovuln_xl , sheet="identmen"))

#BDFE S. De Lauretis
load(MatisseFiles$appmen_depact_2010_rd)
appmen_depensesactiv_2010 <- appmen_depensesactiv

# Output étape précédente, estimation des DPE
load(MatisseFiles$menage_dpe_rd)


# Selection ménages -------------------------------------------------------

# Exclusion DOM 
menage <-
  menage %>% 
  filter(zeat>0)
menage$ident_men <- as.numeric(menage$ident_men)

#Selection des ménages présents dans la base appmen de Simona pour bénéficier des données énergies EDF
menage <- menage[which(menage$ident_men %in% menage_DPE$ident_men), ]
appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage$ident_men), ]

# Selection des ménages dans base c05 
c05$ident_men <-  as.numeric(c05$ident_men)
c05 <- c05[which(c05$ident_men %in% menage$ident_men), ]

# Selection des ménages dans base individus
individu$ident_men <- as.numeric(individu$ident_men)
individu <- individu[which(individu$ident_men %in% menage$ident_men), ]

# RECODER VARIABLES ---------------------------------------------------------------
# creation variables type menage corrige, quintile UC et maison individuelle pour recreer classes

# TYPMEN
menage <- within(menage, {
  typmen_corr <- ifelse(typmen5 == 1 & agepr <= 65, 1,         
                  # celib <= 65
                  ifelse(typmen5 == 1 & agepr > 65, 2,   
                  # celib > 65
                  ifelse(typmen5 == 2, 5,                
                  # famille monoparentale
                  ifelse(typmen5 == 3 & agepr <= 65, 3,  
                  # couple sans enfants, <=65
                  ifelse(typmen5 == 3 & agepr > 65, 4,   
                  # couple sans enfants, > 65
                  ifelse(typmen5 == 4, 6,                
                  # couple avec enfants
                  ifelse(typmen5 == 5 & nenfants > 0, 6, 
                  # ménages complexes : 6,3 ou 4
                  ifelse(typmen5 == 5 & nenfants == 0 & agepr <= 65, 3,
                  ifelse(typmen5 == 5 & nenfants == 0 & agepr > 65, 4, NA
                 )))))))))
})


# Quintiles uc
menage$quintileuc <- car::recode(menage$decuc2," 1:2 = 1 ; 3:4 = 2 ; 5:6 = 3 ; 7:8 = 4 ; 9:10 = 5 ")

# TYPLOG / MI
menage$MI_corr <- car::recode(menage$typlog, "1:2 = 1 ; 3:6 = 0")


# Agreger variables individus ---------------------------------------------

#A partir de la table individus, calcul du nombre de retraités, de chômeurs => l'objectif est de corriger les donnes de la table menage dont les variables nactifs et nactoccup ne sont pas fiables (nombre negatif de chomeurs en les sommant)
nbactoccup <- 
  individu %>% 
  filter(situa<=2) %>%
  group_by(ident_men) %>%
  summarise("nbactoccup"=n())
nbchomeurs <- 
  individu %>%
  filter(situa==4) %>%
  group_by(ident_men) %>%
  summarise("nbchomeurs"=n())
nbretraites <- 
  individu %>% 
  filter(situa==5) %>% 
  group_by(ident_men) %>%
  summarise("nbretraites"=n())

# Creation menage_forme ---------------------------------------------------
# menage_forme regroupe l'essentiel des variables utilisées dans la suite du code
# On utilise decuc2 car défini sur la france métropolitaine. 
# DECUC2 :Décile de revenu par unité de consommation - Calculé sur la France métropolitaine 
# d’une part et sur l’ensemble des DOM d’autre part


menage_forme <-
  menage %>% 
  select(ident_men, pondmen, quintileuc, typmen_corr, typmen5, MI_corr, npers, coeffuc,
         chomage, retraites, decuc2, tuu, agepr, codcspr, zeat, salaires, revindep, rev_etranger) %>%
  left_join(nbchomeurs, by="ident_men") %>%
  left_join(nbactoccup, by="ident_men") %>%
  left_join(nbretraites, by="ident_men") %>%
  left_join(depmen %>% select(ident_men, surfhab_d, stalog, propri, vag), by="ident_men") %>% 
  left_join(menage_DPE, by="ident_men")

# Correction nb chomeurs 
# il existe 2605 ménages dont nbchomeurs est NA mais où il existe des revenus du chômage. 
menage_forme <- 
  menage_forme %>%
  #npers_identifiés assure qu'on ne compte pas plus de personnes dans le ménage qu'il n'en contient
  mutate(npers_identifiees = ifelse(is.na(nbchomeurs), 0, nbchomeurs) +
           ifelse(is.na(nbactoccup), 0, nbactoccup) +
           ifelse(is.na(nbretraites), 0, nbretraites)) %>%
  # #est considéré comme contenant un chomeur, un ménage qui tire des revenus d'une allocation chomage
  # NB : avec le système de pré-retraites ne fonctionne pas => surestime le nombre de chomeurs
  # mutate(nbchomeurs=ifelse(is.na(nbchomeurs) & chomage==0,0,ifelse(is.na(nbchomeurs) & !chomage==0 & (npers-npers_identifiees)>0,1,nbchomeurs)))%>%
  #actifs occupés
  mutate(nbactoccup = ifelse(is.na(nbactoccup) & (salaires + revindep + rev_etranger) == 0, 0, 
                     ifelse(is.na(nbactoccup) & !((salaires + revindep + rev_etranger) == 0) & (npers - npers_identifiees) > 0, 1, nbactoccup))) %>%
  #actifs occupés
  mutate(nbretraites = ifelse(is.na(nbretraites) & retraites == 0, 0, 
                              ifelse(is.na(nbretraites) & !retraites == 0 & (npers-npers_identifiees) > 0, 1, nbretraites))) %>%
  # le reste des NA est mis à 0 (signifie que le foyer est "plein", la somme des catégories renseignées suffit à égal npers)
  mutate(nbchomeurs = ifelse(is.na(nbchomeurs), 0, nbchomeurs),
         nbactoccup = ifelse(is.na(nbactoccup), 0, nbactoccup),
         nbretraites = ifelse(is.na(nbretraites), 0, nbretraites)) %>%
  select(-npers_identifiees, -revindep, -salaires, -rev_etranger)
  
# Actifs
menage_forme <- 
  menage_forme %>% 
  mutate(nbactifs = nbchomeurs + nbactoccup)


### Verif
menage_forme %>% mutate(nbpers_foyer = nbactifs + nbretraites) %>% 
  mutate(diff_npers = npers - nbpers_foyer) %>% filter(diff_npers < 0) %>% summarise(n())
menage_forme %>% summarise(sum(pondmen*nbactoccup))
menage_forme %>% summarise(sum(pondmen*nbchomeurs))
menage_forme %>% summarise(sum(pondmen*nbactifs))
menage_forme %>% summarise(sum(pondmen*nbretraites))
# En 2014, selon l'enquête Emploi, la population active est estimée à 28,6 millions de personnes de 15 ans ou plus en France métropolitaine. Elle regroupe 25,8 millions d'actifs ayant un emploi et 2,8 millions de personnes au chômage.
# En fin d’année 2014, 15,8 millions de personnes, vivant en France ou à l’étranger, sont retraitées de droit direct d’au moins un régime français


#correction de la surface du ménage 8925 (surfhab_d=NA, surfhab=60)
surf_8925 <- as.numeric(depmen %>% filter(ident_men == 8925) %>% select(surfhab))
menage_forme <- menage_forme %>% mutate(surfhab_d = ifelse(ident_men == 8925, surf_8925, surfhab_d))

# calcul nombre inactifs
menage_forme$nbinact <- menage_forme$npers - menage_forme$nbretraites - menage_forme$nbactifs


# Rajout nombre de véhicule 
auto <- suppressWarnings(read_excel(paste(M_data,"/Data/BDF_2010/AUTOMOBILE.xlsx",sep=""),sheet="AUTO_METROPOLE")) #uniquement AUTOMOBILE
menage_forme <-
  menage_forme %>%
  left_join(auto %>% select(ident_men, nbvehic) %>% distinct(),by = "ident_men")


# REVENUS -----------------------------------------------------------------

#categories de revenus definies dans excel
def_rev<- suppressWarnings(read_excel(MatisseFiles$def_rev_xl))

# Revenus de l'activité salariale et/ou independante + revenus de l'étranger
menage_forme$rev_activites <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVACT"), ]$rev)])

# Revenus de l'activité salariale et/ou independante SANS revenus de l'étranger 
menage_forme$rev_activites_sans_etranger <- rowSums(menage[c("salaires", "revindep")])

# Revenus de l'étranger (inclus dans revact)
menage_forme$rev_etranger <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVETRANGER"), ]$rev)])

# Revenus exceptionnels
menage_forme$rev_exceptionnel <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVEXC"), ]$rev)])

# Revenus du patrimoine
menage_forme$rev_patrimoine <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVPAT"), ]$rev)])

# Revenus sociaux (tous)
menage_forme$rev_sociaux <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVSOC"), ]$rev)])

# Revenus sociaux (excl chômage pour calage)
menage_forme$rev_sociaux_autres <- rowSums(menage[intersect(names(menage), def_rev[which(def_rev$REV_CAT=="REVSOC_AUTRES"), ]$rev)])

# Revenus pour le RDB, revenus réguliers, rentrant dans le revenu disponible brut. rev700
menage_forme$rev700 <- menage$rev700 #Sommes reçues régulièrement d'un autre ménage (qui doit les verser obligatoirement)
menage_forme$rev701 <- menage$rev701 #Sommes reçues régulièrement d'un autre ménage (qui les verse librement)
menage_forme$rev999 <- menage$rev999 #Autres ressources
# Revenus pour le RDB_macro (RDB_new)
menage_forme$rev801 <- menage$rev801 #Loyers imputés (pour les propriétaires et logés gratuitement)
menage_forme$rev800 <- menage$rev800 #Vente de logements, terrains, garages => sert dans calcul épargne 2010
menage_forme$rev850 <- menage$rev850 #Vente de véhicule
menage_forme$rev60x <-
  menage$rev601 + #Gains aux jeux de hasard
  menage$rev602 + #Sommes versées par une compagnie d’assurance
  menage$rev603 + #Dommages et intérêts
  menage$rev604 + #Indemnités de licenciement, primes de départ
  menage$rev605 + #Prime à l’amélioration de l’habitat
  menage$rev606 + #Déblocage de participation, vente d’actions,  d’obligations
  menage$rev699 #Autres ressources exceptionnelles
  #NB : on ne compte pas le rev600, héritages et donc qui sont des transferts entre ménages.
# Revenus locatif pour TC_DPE 3.3
menage_forme$rev504 <- menage$rev504 #Vente de véhicule
#on rajoute la colonne correspondant au recyclage d'une partie des revenus de la taxe carbone dans les scénarios. 
menage_forme$rev_TCO <-0

# #Revenus totaux 
# # (revact + revsoc + revpat + rev700 +rev 701 + rev 999)
menage_forme$RDBAI <- rowSums(menage_forme[c("rev_activites", "rev_patrimoine", 
                                             "rev_sociaux", "rev700", "rev701", "rev999","rev_TCO")])


# MISE EN FORME DEPENSES ENERGIE --------------------------------------------------------

#les depenses energetiques ne proviennent pas de BDF mais de l'ENL 2013. 
# On les importe donc de appmen_depensesactiv_2010 : base appairée BDFE (de Lauretis, 2017)
# On sélectionne les variables suivantes pour "Elec" : 
# [1] "Elec_sommeil"         "Elec_physio"          "Elec_repas_dom"      
# [4] "Elec_repas_hors"      "Elec_trav_etu"        "Elec_achats"         
# [7] "Elec_travdom_alim"    "Elec_travdom_vetem"   "Elec_travdom_maison" 
# [10] "Elec_assistance"      "Elec_loisirs_lecture" "Elec_loisirs_tele"   
# [13] "Elec_pleinair_sport"  "Elec_trajets_domtrav" "Elec_trajets_autres" 

#A02
menage_forme$dep_Elec <- rowSums(appmen_depensesactiv_2010[grep("Elec_", names(appmen_depensesactiv_2010))])
#A03
menage_forme$dep_Gaz <- rowSums(appmen_depensesactiv_2010[grep("Gaz_", names(appmen_depensesactiv_2010))])
#A04
menage_forme$dep_GPL <- rowSums(appmen_depensesactiv_2010[grep("GPL_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Fuel <- rowSums(appmen_depensesactiv_2010[grep("Fuel_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Urbain <- rowSums(appmen_depensesactiv_2010[grep("Urbain_", names(appmen_depensesactiv_2010))])
menage_forme$dep_Solides <- rowSums(appmen_depensesactiv_2010[grep("Solides_", names(appmen_depensesactiv_2010))]) 



# Mise en forme des dépenses - Nomenclature ADEME ---------------------------------------------------------

Nomenclature_ADEME_COICOP <-  suppressWarnings(read_excel(MatisseFiles$nom_coicop_3me_xl))

#A01
menage_forme$agriculture <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A01"),]$COICOP_2011)])

# Pas d'intérêt à sommer les dépenses BDF d'énergie, on utilise les données ENL
# A02 électricité, A03 Gaz, A04 autres énergies

#A05
menage_forme$BTP <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A05"),]$COICOP_2011)])

#A06
menage_forme$prod_veh <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A06"),]$COICOP_2011)])

#A07
menage_forme$carb_lubr <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A07"),]$COICOP_2011)])

#A08
menage_forme$transp_rail_air <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A08"),]$COICOP_2011)])

#A09
menage_forme$transp_routes_eau <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A09"),]$COICOP_2011)])

#A10
menage_forme$loisirs_com <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A10"),]$COICOP_2011)])

#A11
menage_forme$autres_services <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A11"),]$COICOP_2011)])

#A12
menage_forme$autres <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A12"),]$COICOP_2011)])

#A13  # sans loyers imputés
menage_forme$loyers <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A13"),]$COICOP_2011)])

#A14
menage_forme$veh_occasion <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="A14"),]$COICOP_2011)])


#Hors postes dépenses
menage_forme$Hors_budget <- 
  rowSums(c05[intersect(names(c05),Nomenclature_ADEME_COICOP[which(Nomenclature_ADEME_COICOP$ADEME=="Hors_budget"),]$COICOP_2011)])

# Rajouter c13711: Achats de logements, garages, parkings, box et terrains,
# compris dans le Hors Budget
# 13211 Remboursements de prêts pour la résidence principale (yc garage et dépendance)
# 13221 Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance)
# 13511 Remboursements de crédits à la consommation (voiture, gros travaux, biens durables)
menage_forme <-
  menage_forme %>%
  left_join(c05 %>% select(c13711, c13211, c13221, c13511, ident_men), by = "ident_men")

### test
menage_forme_bis <-
  menage_forme %>%
  left_join(c05 %>% select(c02311, c12811, c13211, c13221, c13511, c13611, c13722, c12911, ident_men), by = "ident_men")

###

# Dans le Hors Budget on trouve 
# *Stupéfiants c02311 => 0 pour tous les ménages
# *Autres dépenses occasionnées par une cérémonie c12811
# *Dépenses SAI des personnes vivant hors du domicile au moins un jour par semaine c12911 => ?
# *Remboursements de prêts pour la résidence principale (yc garage et dépendance) c13211 => utilisation de l'épargne
# *Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance) c13221 => utilisation de l'épargne
# *Aides et dons (occasionnels ou réguliers) en argent offerts par le ménage et pensions alimentaires c13311 => transferts entre ménages
# *Remboursements de crédits à la consommation (voiture, gros travaux, biens durables) c13511 => utilisation de l'épargne
# *Prélèvements de l'employeur c13611 => 0 pour tous les ménages
# *Achats de logements, garages, parkings, box et terrains c13711
# *Epargne salariale : achat d’actions de l’entreprise c13722 => utilisation de l'épargne



# VARIABLES CALCULEES MENAGES ---------------------------------------------------------------

#IR
menage_forme$impot_revenu <- 
  c05$c13141

# Autres impôts directs (AID)
menage_forme$AID <- rowSums(c05[c("c13111","c13121","c13151","c13161")])

# Revenu Brut Disponible (RDB) 
menage_forme$RDB <- menage_forme$RDBAI - rowSums(menage_forme[c("impot_revenu", "AID")])

# Taux d'imposition
menage_forme$taux_IR <- ifelse(menage_forme$RDB == 0, 0, menage_forme$impot_revenu/menage_forme$RDBAI)
menage_forme$taux_AID <- ifelse(menage_forme$RDB == 0, 0, menage_forme$AID/menage_forme$RDBAI)



# Typologie vulnérabilités -------------------------------------------------
# Rajout des typologies de vulnérabilité dans la base menage_forme
typo_vuln <- 
  typo_vuln %>% 
  separate(col="IDENTMEN", into=c("year","ident_men"),sep="2011")
typo_vuln$ident_men <- as.numeric(typo_vuln$ident_men)

# Selection des ménages
typo_vuln_bis <- 
  typo_vuln %>% 
  filter(ident_men %in% menage_forme$ident_men)
# rajout typo
menage_forme <-
  menage_forme %>%
  left_join(., typo_vuln_bis[c("ident_men", "typo2010f")], by = "ident_men")


# Save --------------------------------------------------------------------

save(menage_forme, file = MatisseFiles$menage_forme_2_rd)

# Clean ----------------------------------------

suppressWarnings(rm(nbactoccup, nbchomeurs, nbretraites, Nomenclature_ADEME_COICOP, auto, 
                    def_rev, individu, menage_DPE, appmen_depensesactiv, menage_forme, 
                    typo_vuln,typo_vuln_bis, appmen_depensesactiv_2010, c05, depmen, 
                    menage, menage_forme_bis, surf_8925))
gc()





# Verif -------------------------------------------------------------------

# compute_savings_rate_export_2010(menage_forme)
# # [1] 0.1206374
# compute_share_export_2010(menage_forme)
# # share_A01    share_A02    share_A03    share_A04    share_A05    share_A06    share_A07    share_A08    share_A09
# # share 1.719074e-01 2.029005e-02 1.034595e-02 8.863427e-03 6.268319e-02 3.493252e-02 4.033831e-02 1.304480e-02 2.233635e-03
# # Vol   1.504183e+11 1.775371e+10 9.052662e+09 7.755465e+09 5.484755e+10 3.056582e+10 3.529587e+10 1.141415e+10 1.954422e+09
# # share_A10    share_A11    share_A12    share_A13
# # share 9.575175e-02 2.008130e-01 1.494116e-01 1.893844e-01
# # Vol   8.378241e+10 1.757106e+11 1.307345e+11 1.657106e+11
# ETAPE SUIVANTE
# source("Mise_forme_BDF/3_mise_forme_energies.R")


# ETAPE SUIVANTE



# A SUPPRIMER !!
##
##

# # DESAGREGER GROS TRAVAUX -------------------------------------------------
# 
# source("Mise_forme_BDF/2_1_desagregation_reno_rehab.R")
# 
# 
# solde<-
#   Gros_travaux_2010 %>% 
#   select(ident_men) %>%
#   mutate(solde=-Gros_travaux_2010$GT_REHAB) #solde négatif car Gros_travaux_2010$GT_REHAB>0 et on veut que le solde se comporte comme une économie
# 
# rm(Gros_travaux_2010)
# 
# menage_forme$BTP <- 
#   menage_forme$BTP + 
#   solde$solde
# 
# save(menage_forme,file="2010/menage_forme_essai.RData")
# save(solde,file="2010/solde.RData")
# 
# menage_forme$decuc2<-as.numeric(as.character(menage_forme$decuc2))
# 
# 
# #Les gros travaux sont directement retirés de la consommation et viennent augmenter le flux d'épargne. 
# # Le fichier 2_2_ventilation est obsolète. 
# 
# ##
# ##
# ## /!\ à vérifier tout de même que les agrégats tombent justes pour certains ménages typiques. 
# ##
# ##
# ##
# ##

# 
# list_dep=c("agriculture",
#            "dep_Elec",
#            "dep_Gaz",
#            "dep_GPL",
#            "dep_Fuel",
#            "dep_Urbain",
#            "dep_Solides",
#            "BTP",
#            "prod_veh",
#            "carb_lubr",
#            "transp_rail_air",
#            "transp_routes_eau",
#            "loisirs_com",
#            "autres_services",
#            "autres",
#            "loyers",
#            "veh_occasion",
#            "Hors_budget")
# menage_forme$Rcons <- rowSums(menage_forme[list_dep])
# 
# 
# #pour test 
# menage_forme_bis<-menage_forme
# 
# for (k in list_dep){
#   menage_forme[paste("part",k,sep="_")]<-menage_forme[k]/menage_forme$Rcons
# }
# 
# 
# # Epargne
# menage_forme$epargne <- 
#   menage_forme$RDB - 
#   menage_forme$Rcons + 
#   menage_forme$rev_exceptionnel
# 
# # taux épargne
# menage_forme$taux_epargne <- 
#   ifelse(menage_forme$RDB==0,0,menage_forme$epargne/menage_forme$RDB)
# 
# 
# menage_forme$ratio_S <-  menage_forme$epargne / menage_forme$Rcons  # probleme pour RDB = 0 (ratio_S = -1)
# 
# 
# 
# # 
# # source("Mise_forme_BDF/2_2_ventilation_rehab.R")
# # 
# # Ventil_solde(solde,menage_forme)
# # # renvoie menage_forme_bis
# # test2<-menage_forme_bis
# # 
# # # # menage 1221 : on annule completement ses dépenses de gros travaux. Reventilation de plus de 8000€.
# # # # menage 1549 : partage entre REHAB et RENO (81% rehab, 19% reno), reventilation de 35000€. 
# # # # menage 7792 : Delta de 45373, Beta de 44208
# # #  # menage 12815 : Beta de 96027
# # i=12815
# # # ccl : aucune dépense ne devient négative. Principe ok. 
# # 
# # 
# # ### TEST 
# # # décommenter
# # # View(cbind(c("init","final","final - init"),
# #   # rbind(test1 %>% filter(ident_men==i),test2 %>% filter(ident_men==i),test2%>% filter(ident_men==i)-test1%>% filter(ident_men==i))))
# 
# 
# 
# 
# # Taux effort financier ---------------------------------------------------
# 
# c05_forme_2010 <- c05[which(c05$ident_men %in% menage_forme$ident_men),]
# 
# 
# menage_forme_bis <- 
#   menage_forme_bis %>% mutate(taux_effort_financier=(c05_forme_2010$c13211+c05_forme_2010$c13221+c05_forme_2010$c13511+c05_forme_2010$c12611)/RDB) %>% mutate_when(taux_effort_financier<0,list(taux_effort_financier=0))
# 
# 
# # Dépenses contraintes ----------------------------------------------------
# 
# #on laisse les c05 des dépenses contraintes séparées pour pouvoir imputer l'augmentation de la part des dépenses contraintes à une catégorie en particulier
# 
# dep_preeng_code<-read_excel("D:/CIRED/Projet_Ademe/Donnees_brutes/Nomenclature_CIRED_ADEME/nomenclature_dep_preeng_coicop5.xlsx",sheet="nomenclature_code",col_names = T)
# 
# 
# 
# c05_dep_preeng<-c05_forme_2010 %>% select(ident_men,dep_preeng_code$COICOP)
# 
# 
# menage_forme_bis <-
#   menage_forme_bis %>%
#   left_join(c05_dep_preeng,by="ident_men")
#  
#  
# # SAVE FILE ---------------------------------------------------------------
# 
# # Création menage_forme_2010
# menage_forme_2010 <-menage_forme_bis
# appmen_depensesactiv_2010 <- appmen_depensesactiv_2010[which(appmen_depensesactiv_2010$ident_men %in% menage_forme$ident_men),]
# 
# 
# # SAVE
# save(menage_forme_2010,file="2010/menage_forme_2010.RData")
# save(appmen_depensesactiv_2010,file="2010/appmen_depensesactiv_2010.RData")
# save(c05_forme_2010,file="2010/c05_forme_2010.RData")
# 
# source("Mise_forme_BDF/3_1_update_dep_ener_2010.R")
# 
# 
# # # Parts_budgétaires -------------------------------------------------------
# # 
# # #NB : hors budget exclus, parts budgétaires sur les 14 postes de la nomenclature, 
# # source("Code_Global_Ademe/compute_share.R")
# # share_2010<-compute_share(menage_forme_2010)
# # share_2010
# 


# SUCCESS -----------------------------------------------------------------

print("Step 0 : 2_mise_forme_BDF : SUCCESS")

