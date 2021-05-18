
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
    
    output_macro<-read_excel(path = "D:/CIRED/Projet_Ademe/IMACLIM/Output_macro_code.xlsx",sheet=s,skip=1)
    
    output_macro <-
      output_macro %>%
      gather(key=year_model,value=depenses,-c(1:3))%>%
      separate(col="year_model",into=c("year","model"),sep="_")
    
    ThreeME_2010 <-
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year==2010)%>%
      filter(Variable %in% c("C21","C22","C24"))%>%
      mutate(depenses=as.numeric(depenses))%>%
      left_join(EMS_2010,by=c("year"="year","Variable"="Variable"))%>%
      mutate(coeff_emission=emission/depenses)
    
    
    ThreeME_h <-
      output_macro %>%
      filter(model=="ThreeME")%>%
      filter(year==h)%>%
      filter(Variable %in% c("C21","C22","C24"))%>%
      mutate(depenses=as.numeric(depenses))%>%
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




# Coefficient MICRO émissions 2010 ----------------------------------------------

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
