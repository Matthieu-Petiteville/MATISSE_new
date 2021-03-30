
# Library -----------------------------------------------------------------
library(tidyverse)

# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE")
#function
source(paste(M_home,"/Step_2_Microsimulation/1.2_fonction_microsimulation.R",sep=""))
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))
source(paste(M_home,"/Common/tools.R",sep=""))


# DATA --------------------------------------------------------------------

## Menages
# 2010
load(paste(M_data,"/Output/Initial format/menage_forme.RData",sep=""))
# mise à l'échelle
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_1.RData",sep=""))

## MACRO
# FC
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))

# ThreeME
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))



# ELASTICITES -------------------------------------------------------------
# Importer élasticités prix et revenus de chaque TYPO x DECILE
load(paste(M_data,"/Data/Econometrie_demande/elasticite_demande.RData",sep="")) #Elast

# Nom de tous les catégories de "A01" à "A014")
Cat<-names(FC)[1:14]
list_A=c()
for (i in 1:9){list_A=c(list_A,paste("A0",i,sep=""))}
for (i in 10:14){list_A=c(list_A,paste("A",i,sep=""))}
# list_A
list_elast_rev<-paste("elast_rev",list_A,sep="_")
list_elast_prix<-paste("elast_prix",list_A,sep="_")


# Elasticité prix --------------------------------------------------------------

Elast_prix <-
  Elast %>% 
  filter(typ_elast=="prix") %>% 
  select(CODADEME,Typo, Decile,elast) %>%
  dplyr::rename(.,elast_prix=elast) %>%
  spread(key=CODADEME,value=elast_prix)%>%
  mutate(Decile=as.numeric(Decile))

colnames(Elast_prix)<-c("Typo","Decile",list_elast_prix) 





# Elasticité revenu ------------------------------------------------------------

Elast_rev <-
  Elast %>% filter(typ_elast=="rev") %>% 
  mutate(elast_rev=elast) %>%
  select(CODADEME,Typo, Decile,elast_rev)%>%
  spread(key=CODADEME,value=elast_rev) %>%
  mutate(Decile=as.numeric(Decile)) 

colnames(Elast_rev)<-c("Typo","Decile",list_elast_rev) 



# Elasticités des Ménages -------------------------------------------------

menage_echelle$decuc2 <- as.numeric(menage_echelle$decuc2)

menage_echelle<- 
  menage_echelle %>% 
  left_join(Elast_prix,by=c("typo2010f"="Typo","decuc2"="Decile")) %>%
  left_join(Elast_rev,by=c("typo2010f"="Typo","decuc2"="Decile")) 





# APPEL Fonction MICROSIMULATION des DEPENSES -----------------------------

menage_echelle<-microsimulation_depenses(menage_echelle,menage_forme,FC)


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

save(FC,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""))


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

save(menage_echelle,file=paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_2_1.RData",sep=""))


# Epargne -----------------------------------------------------------------

print(compute_savings_rate_export(menage_echelle))
print(compute_share_export(menage_echelle))


# Success -----------------------------------------------------------------

print("Step 2 : 1_Microsimulation : SUCCESS")


