
# Libraries -----------------------------------------------------------------

suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
source(paste(M_home,"/Step_2_Microsimulation/1.2_fonction_microsimulation.R",sep=""))
source(paste(M_home,"/Step_6_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Common/tools.R",sep=""))


# Data --------------------------------------------------------------------

load(MatisseFiles$menage_forme_rd)
load(MatisseFiles$menage_echelle_1_rd)
load(MatisseFiles$FC_2010_horizon_rd)
load(MatisseFiles$Threeme_rd)
load(MatisseFiles$elast_rd)


# Extraction des élasticités de Elast --------------------------------------------------------------

# Nom de tous les catégories de "A01" à "A014")
list_A <- paste("A", formatC(1:14, width = 2, flag = 0), sep="")
list_elast_rev <- paste("elast_rev",list_A,sep="_")
list_elast_prix <- paste("elast_prix",list_A,sep="_")

Elast_prix <-
  Elast %>% 
  filter(typ_elast == "prix") %>% 
  select(CODADEME, Typo, Decile, elast) %>%
  dplyr::rename(., elast_prix = elast) %>%
  spread(key = CODADEME, value = elast_prix) %>%
  mutate(Decile = as.numeric(Decile))
colnames(Elast_prix) <- c("Typo", "Decile", list_elast_prix) 

Elast_rev <-
  Elast %>% filter(typ_elast == "rev") %>% 
  mutate(elast_rev = elast) %>%
  select(CODADEME, Typo, Decile, elast_rev) %>%
  spread(key = CODADEME, value = elast_rev) %>%
  mutate(Decile = as.numeric(Decile)) 
colnames(Elast_rev) <- c("Typo", "Decile", list_elast_rev) 

#Ajout des élasticités par ménage en fonction de typo et décile
menage_echelle$decuc2 <- as.numeric(menage_echelle$decuc2)
menage_echelle<- 
  menage_echelle %>% 
  left_join(Elast_prix, by = c("typo2010f" = "Typo", "decuc2" = "Decile")) %>%
  left_join(Elast_rev , by = c("typo2010f" = "Typo", "decuc2" = "Decile")) 


# Fonction microsimulation des dépenses -----------------------------

menage_echelle <- microsimulation_depenses(menage_echelle, menage_forme, FC)


# Mise à l'échelle des surfaces -------------------------------------------

# On ne peut pas mettre les surfaces à jour comme les surfaces agrégées, sinon en repondérant pour augmenter le nombre de ménages on aura une trop grande surface.
# On met donc à l'échelle par l'évolution de la surface par habitant dans 3ME.

BUIL_H01_2_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="BUIL_H01_2") %>%
      filter(year==2010) %>%
      select(value)
  )
POP_TOT_2010<-
  as.numeric(
    ThreeME %>%
      filter(Var=="POP_TOT") %>%
      filter(year==2010) %>%
      select(value)
  )

surfhab_hab_2010<-BUIL_H01_2_2010/POP_TOT_2010

BUIL_H01_2_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="BUIL_H01_2") %>%
      filter(year==horizon) %>%
      select(value)
  )



POP_TOT_horizon<-
  as.numeric(
    ThreeME %>%
      filter(Var=="POP_TOT") %>%
      filter(year==horizon) %>%
      select(value)
  )

surfhab_hab_horizon<-BUIL_H01_2_horizon/POP_TOT_horizon


FC$surface<-surfhab_hab_horizon/surfhab_hab_2010

save(FC,file=MatisseFiles$FC_2010_horizon_rd)

menage_echelle <- 
  menage_echelle %>%
  mutate(surfhab_d=surfhab_d*FC$surface)



# Mise à l'échelle des dépenses immobilières ------------------------------
# 13211 Remboursements de prêts pour la résidence principale (yc garage et dépendance)
# 13221 Remboursements des autres prêts immobiliers (résidence secondaire et autre logement yc dépendance)
# 13511 Remboursements de crédits à la consommation (voiture, gros travaux, biens durables)

menage_echelle <- 
 menage_echelle %>%
  mutate(c13711=c13711*FC$A05 * FC$surface)%>%
  mutate(rev800=rev800*FC$A05 * FC$surface)%>%
  mutate(c13211=c13211*FC$A05 * FC$surface)%>%
  mutate(c13221=c13221*FC$A05 * FC$surface)%>%
  mutate(c13211=c13211*FC$A05 * FC$surface)%>%
  mutate(c13511=c13511*(1+TC_RDB_nominal))#bien joué Franck

# Save --------------------------------------------------------------------

save(menage_echelle,file=MatisseFiles$menage_echelle_2_1_rd)


# Epargne -----------------------------------------------------------------

# print(compute_savings_rate_export(menage_echelle))
# print(compute_share_export(menage_echelle))
# 

# Success -----------------------------------------------------------------

# print("Step 2 : 1_Microsimulation : SUCCESS")


# Clean -------------------------------------------------------------------
suppressWarnings(rm(menage_forme,menage_echelle,FC,ThreeME,Elast,list_A,list_elast_rev,list_elast_prix,Elast_prix,Elast_rev,
   BUIL_H01_2_2010,POP_TOT_2010,surfhab_hab_2010,BUIL_H01_2_horizon,POP_TOT_horizon,surfhab_hab_horizon))
gc()

