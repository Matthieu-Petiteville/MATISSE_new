
# L'objectif de cette fonction est d'estimer une fonction de régression logistique permettant d'estimer les classes de DPE des ménages de BDF,le résultat se trouve dans menages_DPE, colonne DPE_pred
# Pas besoin de faire fonctionner un autre code que celui-ci, il se charger de sourcer tous les scripts R qu'il requiert. (1.2_estimateur_DPE_phebus)


# LIBRARIES  ----------------------------------------------------------------

library(tidyverse)
library(car)
library(readxl)

# CHARGER STOCK DPE ---------------------------------------------------------------

#Stock de DPE en 2010 pour caler les variables agrégées => dpe_stock_2010

#LOAD
suppressWarnings(scen <- read_excel(path=paste(M_data,"/IMACLIM/Sorties Three-ME.xlsx",sep=""),sheet="scen AMS"))
#Voir si possible d'opti avec un read csv au lieu de read excel - timing excel 55s timing csv 0.06s


# LOAD DONNEES BDF -------------------------------------------------------------

# Base INSEE menage
menage<-
  read.csv(paste(M_data,"/Data/BDF_2010/menage.csv",sep=""),
           header=TRUE,
           sep=";",
           dec=".",
           stringsAsFactors = FALSE)
#2.2s

depmen<-
  read.csv(paste(M_data,"/Data/BDF_2010/depmen.csv",sep=""),
           header=TRUE,
           sep=";",
           dec=".",
           stringsAsFactors = FALSE)
#3.3s


# load menage_calibr_2010 avec ménages pré_selectionnés
load(paste(M_data,"/Data/BDFE_delauretis/menage_calibr_2010.RData",sep=""))
#0.15s



# Traitement ThreeME ------------------------------------------------------
ThreeME<- scen %>% select(-Def)%>% gather(key=year, value=value, -c(1))%>%filter(year==2010)


#VARIABLES
dpe_stock_2010<- ThreeME %>% 
  filter(Var %in% 
           c("BUIL_H01_CA_2" , "BUIL_H01_CB_2" , "BUIL_H01_CC_2" , 
             "BUIL_H01_CD_2" , "BUIL_H01_CE_2" , "BUIL_H01_CF_2" , "BUIL_H01_CG_2"))

#CONVERT DATAFRAME 
# (7 obs, 3 variables)
dpe_stock_2010 <- as.data.frame(dpe_stock_2010)


#Extraire nom de la classe de DPE
dpe_stock_2010<- dpe_stock_2010 %>% 
  mutate(DPE= str_replace_all(Var, pattern="BUIL_H01_C",replacement="")) %>% 
  mutate(DPE= str_replace_all(DPE, patter="_2",replacement=""))

save(dpe_stock_2010,file=paste(M_data,"/Output/Step_0/dpe_stock_2010.RData",sep=""))



# ESTIMATEUR DPE SUR DONNEES SOCIO-ECO ----------------------------------------------------------

# Load estm_dpe_acp, 
# list 27 paramètres
source(paste(M_home , "/Step_0_Mise_forme_BDF/1.2_estimateur_dpe_phebus.R", sep=""))



# CREER BASE DE DONNEES --------------------------------------------------

#appariement entre la base ménage (variables explicatives) et la nouvelle estimation de DPE 
appariement_menages_DPE<-
  menage %>% 
  select(ident_men,typlog,tuu,decuc2)


pond<-
  menage_calibr_2010 %>% 
  select(ident_men,pondmen)

rm(menage_calibr_2010)

depmen <- 
  depmen %>% 
  select(ident_men, stalog,sourcp,ancons,surfhab_d)

appariement_menages_DPE<- 
  pond %>% 
  left_join(.,appariement_menages_DPE, by="ident_men") %>% 
  left_join(.,depmen, by="ident_men")

#pour équivalence variables Phébus/BDF se référer au document "2018-09-26 Mapping données Phébus x BDF.odt"
# BATI_PERIODE + ESTOC + Revenu_Insee_quintile + EHST + RP_TAILLE_UU + is_elec + MI





# RECODAGE DES VARIABLES --------------------------------------------------

# recodage des variables pour correspondante entre BDF et Phébus

## ANCONS
appariement_menages_DPE$BATI_PERIODE<- car::recode(appariement_menages_DPE$ancons, "1 = 1 ; 2:3=2 ; 4:6=3 ; 7:8=4 ; 9:10=5")


## STALOG
appariement_menages_DPE$ESTOC<- car::recode(appariement_menages_DPE$stalog, "1:2 = 1 ; 3=2 ; 4:5=3 ; 6=4")

## DECU1
appariement_menages_DPE <- appariement_menages_DPE%>%rename(replace=c("decuc2"="Revenu_Insee_quintile"))


## Surfhab_d
appariement_menages_DPE$EHST <- appariement_menages_DPE$surfhab_d
appariement_menages_DPE[which(appariement_menages_DPE$EHST==999),"EHST"]<-0
appariement_menages_DPE[which(is.na(appariement_menages_DPE$EHST)),"EHST"]<-0

## MI_corr 
appariement_menages_DPE$MI <- car::recode(appariement_menages_DPE$typlog,"1:2=1 ; 3:6=0")


## TUU (menage)
appariement_menages_DPE$RP_TAILLE_UU <- as.numeric(appariement_menages_DPE$tuu)


## sourcp
appariement_menages_DPE$is_elec <- appariement_menages_DPE$sourcp
appariement_menages_DPE[which(appariement_menages_DPE$is_elec>1),"is_elec"]<-0
appariement_menages_DPE[which(is.na(appariement_menages_DPE$is_elec)),"is_elec"]<-0

# select col
appariement_menages_DPE<-appariement_menages_DPE %>% select(ident_men, pondmen, BATI_PERIODE,ESTOC,Revenu_Insee_quintile,EHST,MI, RP_TAILLE_UU,is_elec)


# Estimation --------------------------------------------------------------

# estime la probabilité d'appartenance de chaque ménage à chaque classe DPE
pred <- predict(
  estm_dpe_acp,
  appariement_menages_DPE,
  type="probs",
  na.pass=TRUE)


colnames(pred)<-c("A","B","C","D","E","F","G")

appariement_menages_DPE[c("A","B","C","D","E","F","G")]<-pred
appariement_menages_DPE$DPE_pred<-19
# table(is.na(appariement_menages_DPE))



# STOCKS M2 ---------------------------------------------------------------

# Mise à l'échelle des stocks de m2
stock_m2_bdf <- appariement_menages_DPE %>% summarise(sum(EHST*pondmen))
stock_m2_threeME<- dpe_stock_2010 %>% summarise(sum(value))
dpe_stock_2010_3ME<- dpe_stock_2010 %>% mutate(value=value*as.numeric(stock_m2_bdf/stock_m2_threeME))

#verification, ratio=1
stock_m2_threeME2<- dpe_stock_2010_3ME %>% summarise(sum(value))
stock_m2_bdf/stock_m2_threeME2


# ATTRIBUTION DPE ---------------------------------------------------------

# On parcourt les DPE dans l'ordre décroissant. On attribue les DPE élevées aux ménages qui ont le plus de chance de s'y situer.
# On les x ménages les plus probablement dans la classe considérée pour atteindre le stock de m2 de cette classe dans ThreeME. 
# les ménages déjà sélectionnées se voient attribuer une probabilité de -1 d'appartenance à toutes les DPE pour les exclure du choix.
for (i in LETTERS[1:7]){
# for (i in c("A","B","G","F","C","D","E")){
  count=0
  while(count<dpe_stock_2010_3ME %>% filter(DPE==i) %>% select(value)){
    indice=which.max(appariement_menages_DPE[,i])
    appariement_menages_DPE$DPE_pred[indice]<-i
    appariement_menages_DPE[indice,
                            LETTERS[1:7]]<- -1
    count=count+appariement_menages_DPE$EHST[indice]*appariement_menages_DPE$pondmen[indice]
  }
}

## VERIF
# distribution dans l'ordre A-> B -> C # for (i in LETTERS[1:7]){
# ap1 <- appariement_menages_DPE%>%select(ident_men, DPE_pred)
# distribution dans l'ordre for (i in c("A","B","G","F","C","D","E")){ # par volume croissant
# ap2 <- appariement_menages_DPE%>%select(ident_men, DPE_pred)

# comp<- ap1 %>% left_join(ap2,by="ident_men")
# comp$DPE_pred.x<-sapply(comp$DPE_pred.x,function(X){as.numeric(match(X,  LETTERS[1:7]))})
# comp$DPE_pred.y<-sapply(comp$DPE_pred.y,function(X){as.numeric(match(X,  LETTERS[1:7]))})
# comp$diff_DPE<-comp$DPE_pred.x-comp$DPE_pred.y
# table(comp$diff_DPE)
# > table(comp$diff_DPE)
# 
# -3   -2   -1    0    1    2    3 
# 1   50 1853 6893 1207  283    2 
# ggplot(comp,aes(x=diff_DPE))+geom_histogram()
# => ccl: ne change pas démesurément, on garde attribution par ordre décroissants A-> G 


# Vérification graphique : Distribution des DPE


detach('package:plyr')
list_dpe_pred<- 
  appariement_menages_DPE %>% 
  mutate(surf_pond=EHST*pondmen) %>% 
  dplyr::group_by(DPE_pred) %>% 
  summarise(sum(surf_pond))
colnames(list_dpe_pred)<-c("DPE_pred","surf_pond")



dat=data.frame(
  "cat_DPE"=rep(c("A" , "B" , "C" , "D" , "E" , "F" , "G"),2),
  
  "DPE"=c(dpe_stock_2010$value,
          list_dpe_pred$surf_pond),
  
  "statut"=c(rep("DPE_réel",7),
             rep("DPE_pred",7))
)


print(ggplot(dat,aes(fill=statut,x=cat_DPE,y=DPE))+geom_bar(stat="identity",position="dodge")+ggtitle("Multinomial logit"))




# SAVE DPE ----------------------------------------------------------------
menage_DPE<-appariement_menages_DPE %>% select(ident_men,DPE_pred)

save(menage_DPE,file=paste(M_data,"/Output/Step_0/menage_DPE.RData",sep=""))

# SUCCESS -----------------------------------------------------------------

print("Step 0 : 1.1_imputation_DPE_2010 : SUCCESS")
