
# Objectif : sortir les coefficients d'émissions des consommations d'énergie physique des ménages en 2010
# Ne pas faire tourner sà chaque itération.


# Library -----------------------------------------------------------------
library(readxl)
library(tidyverse)

# Data --------------------------------------------------------------------

Scenarios<-c("AMS","AME","ssTCO","ssRES","ssVE")
Horizons<-c(2025,2030,2035)

Tab<-rep(0,8)

for (s in Scenarios){
  for (h in Horizons){
    
    
    # Coeff -------------------------------------------------------------------
    
    #EMS
    EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste(s,"!B1:AF5",sep=""),col_names=T)
    
    EMS<-EMS %>% gather(key=year,value=emission,-1)%>%filter(!Var=="EMS_HH_2")%>%select(-Var)
    # EMS #tonnes C02
    
    EMS_h <- 
      EMS %>%
      filter(year==h)%>%
      mutate(Variable=c("C21","C22","C24"))
    
    EMS_2010 <- 
      EMS %>%
      filter(year==2010)%>%
      mutate(Variable=c("C21","C22","C24"))
    
    
    # Dépenses    
   
    # PAS BESOIN DE diviser par l'indice des prix => ce qu'on tire de Output_macro_code est déjà en volume !
    
    output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code_new.xlsx",sheet=s,skip=1)

    output_macro <-
      output_macro %>%
      gather(key=year_model,value=value,-c(1:3))%>%
      separate(col="year_model",into=c("year","model"),sep="_")

    
    IP_2010<-
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year %in% c(2010))%>%
      filter(Variable %in% c("IPC21","IPC22","IPC24"))%>%
      select(Variable,value)%>%
      spread(key=Variable,value=value)
    
      
    ThreeME_2010 <-
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year==2010)%>%
      filter(Variable %in% c("C21","C22","C24"))%>%
      mutate(depenses=as.numeric(value))%>%
      # mutate(depenses=ifelse(Variable=='C21',depenses/as.numeric(IP_2010$IPC21),ifelse(Variable=='C22',depenses/as.numeric(IP_2010$IPC22),depenses/as.numeric(IP_2010$IPC24))))%>%
      left_join(EMS_2010,by=c("year"="year","Variable"="Variable"))%>%
      mutate(coeff_emission=emission/depenses)
    
    # Attention les indices de prix 3ME sont en base 2006

    IP_h<- 
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year==h)%>%
      filter(Variable %in% c("IPC21","IPC22","IPC24"))%>%
      select(Variable,value)%>%
      mutate(value=as.numeric(value))%>%
      spread(key=Variable,value=value)
    
    # IP_h$IPC21<-IP_h$IPC21/as.numeric(IP2010$IPC21)
    # IP_h$IPC22<-IP_h$IPC22/as.numeric(IP2010$IPC22)
    # IP_h$IPC24<-IP_h$IPC24/as.numeric(IP2010$IPC24)
    
    ThreeME_h <-
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year==h)%>%
      filter(Variable %in% c("C21","C22","C24"))%>%
      mutate(depenses=as.numeric(value))%>%
      # mutate(depenses=ifelse(Variable=='C21',depenses/as.numeric(IP_h$IPC21),ifelse(Variable=='C22',depenses/as.numeric(IP_h$IPC22),depenses/as.numeric(IP_h$IPC24))))%>%
      left_join(EMS_h,by=c("year"="year","Variable"="Variable"))%>%
      mutate(coeff_emission=emission/depenses)
    
    
    FC <- 
      ThreeME_h %>%
      mutate(FC_coeff_emission=
               ThreeME_h$coeff_emission/
               ThreeME_2010$coeff_emission)%>%
      select(-c(depenses,emission))%>%
      mutate(scenario=s)
    
    Tab<-rbind(Tab,FC)
    
    
    # Export ------------------------------------------------------------------
    
  }}

Tab<-Tab[-1,]


write_csv(Tab,path="D:/CIRED/Projet_Ademe/IMACLIM/coeff_dep_ems.csv")


# 
# 
# # Coefficient MICRO émissions 2010 ----------------------------------------------

EMS<-read_excel(path=paste("D:/CIRED/Projet_Ademe/IMACLIM/EMS.xlsx",sep=""),range=paste("AMS!B1:AF5",sep=""),col_names=T)
EMS<-EMS %>% gather(key=year,value=emission,-1)
colnames(EMS)<-c("Variable","year","emission")

EMS<-EMS%>%filter(!Variable=="EMS_HH_2")
EMS #tonnes C02

load("D:/CIRED/Projet_Ademe/MATISSE/Step_0_Mise_forme_BDF/Output/menage_forme.RData")


# prix constant 2010 tCO2/ dépenses par cat
coeff_CL_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_21_2")%>%select(emission))/as.numeric(menage_forme %>%
                                                                                                                       summarise(sum(pondmen*dep_Solides))) # en tC02/Millions €

coeff_Oil_2010<-as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_22_2")%>%select(emission))/ as.numeric(menage_forme %>%
                                                                                                                        summarise(sum(pondmen*(carb_lubr+dep_Fuel+dep_GPL)))) # en tC02/Millions €

coeff_Gaz_2010<- as.numeric(EMS%>%filter(year==2010)%>%filter(Variable=="EMS_HH_24_2")%>%select(emission))/as.numeric(menage_forme %>%
                                                                                                                        summarise(sum(pondmen*(dep_Gaz+dep_Urbain)))) # en tC02/Millions €


coeff_ems_2010<-as.data.frame(t(c(coeff_CL_2010,coeff_Oil_2010,coeff_Gaz_2010)))
colnames(coeff_ems_2010)<-c("coeff_CL_2010","coeff_Oil_2010","coeff_Gaz_2010")
save(coeff_ems_2010,file="D:/CIRED/Projet_Ademe/MATISSE/Data/Data_interne/coeff_ems_2010.RData")
