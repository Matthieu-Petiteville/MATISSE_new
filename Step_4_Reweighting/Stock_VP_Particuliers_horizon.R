# Problème : la ligne 267 de Sorties_ThreeME donne le stock de véhicules particuliers. Or ces VP n'appartiennent pas tous à des particuliers. 
# On ne peut pas considérer l'évolution de ce stock comme représentative du stock de VP des particuliers puisque la part d'achats des VP par des particuliers chute de moitié entre 2010 et 2035.


# Library -----------------------------------------------------------------
library(readxl)
library(tidyverse)

# Data --------------------------------------------------------------------

# setwd("D:/Stage_Petiteville/Projet_Ademe/MATISSE/")
ventes_VP <- read_excel(path=paste(M_data,"/Data/ThreeME/Ventes_VP.xlsx",sep=""))
load(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""))
load(paste(M_data,"/Output/Step_0/menage_forme.RData",sep=""))

Ventes <- 
  ThreeME %>%
  filter(Var %in% c("NEWAUTO_TH_H01_2","NEWAUTO_ELEC_H01_2"))%>%
  mutate(Year=as.numeric(year))%>%
  filter(Year<=2035 & Year >=2010)%>%
  select(year,Var,value)%>%
  spread(key=Var,value=value)%>%
  mutate(Ventes_VP=NEWAUTO_TH_H01_2+NEWAUTO_ELEC_H01_2)%>%
  mutate(Ventes_VP=Ventes_VP*1000)%>%
  mutate(Year=as.numeric(year))%>%
  select(Year,Ventes_VP)%>%
  left_join(ventes_VP,by="Year")%>%
  mutate(Ventes_Particuliers=Ventes_VP*Particuliers)

Stock_VP <- 
  ThreeME %>%
  filter(Var=="AUTO_H01_2")%>%
  mutate(Year=as.numeric(year))%>%
  filter(Year<=2035 & Year >=2010)%>%
  select(Year,value)


# Calcul ------------------------------------------------------------------

# Stock des véhicules particuliers détenus pas des particuliers en 2010
Stock_VP_P <- as.numeric(menage_forme %>%summarise(sum(nbvehic*pondmen,na.rm=T)))
part_VP <- Stock_VP_P/as.numeric(Stock_VP%>%filter(Year==2010)%>%select(value))/1000
Ventes_particuliers <- as.numeric(Ventes %>% filter(Year==2010)%>%select(Ventes_Particuliers))
Stock_list<-c(2010,Stock_VP_P,part_VP,Ventes_particuliers)


#Hypothese ThreeME : durée de vie de 13 ans des véhicules de toute sorte
for (Y in 2011:horizon){
  # Stock de VP des Particuliers
  Stock_VP_P <- 
    Stock_VP_P *(1-1/13) + 
    as.numeric(Ventes%>% filter(Year==Y)%>%select(Ventes_Particuliers))
  # Part du stock de VP
  part_VP <- Stock_VP_P/as.numeric(Stock_VP%>%filter(Year==Y)%>%select(value))/1000
  Ventes_particuliers <- as.numeric(Ventes %>% filter(Year==Y)%>%select(Ventes_Particuliers))
  
  Stock_list<-rbind(Stock_list,c(Y,Stock_VP_P,part_VP,Ventes_particuliers))
}
Stock_list<-as.data.frame(Stock_list,row.names = FALSE)
colnames(Stock_list)<-c("Year","Stock_VP_Particuliers","Part_VP_Particuliers","Ventes_particuliers")

