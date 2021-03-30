# 
# Objectif : vérifier si les codes ont été tourné en cas d'interruption du code en cours de boucle

# Code --------------------------------------------------------------------


list_scenarii<-c()

for (i in c(1,2)){
  
  
  if(i==1){
    
    Scenarios=c("AMS","AME")
    Redistribution=c("niveau_vie","forfait","decile","tuu","ssrec")
    Horizon=c("2025","2030","2035")
    Scenario_classement=c("Optimiste","Median","Pessimiste")
    
  }else{
    
    Scenarios=c("ssTCO","ssRES","ssVE")
    Redistribution=c("niveau_vie","forfait","decile","ssrec")
    Horizon=c("2025","2030","2035")
    Scenario_classement=c("Optimiste","Pessimiste","Median")
    
    
  }
  
  for (s in Scenarios){
    for (h in Horizon){
      for (sc in Scenario_classement){
        for (r in Redistribution){
          
          Here=F
          if(file.exists(paste("D:/CIRED/Projet_Ademe/Results/",s,"/",h,"/",sc,"/",r,"/menage_iteration.RData",sep=""))){Here=T}
          
          list_scenarii<-rbind(
            list_scenarii,
            c(s,h,sc,r,Here))
          
          
        }}}}
  }
  
list_scenarii<-as.data.frame(list_scenarii)
  colnames(list_scenarii)<-c("scenario","horizon","scenario_classement","redistribution","available")
  
  table(list_scenarii$available)
  list_scenarii %>% filter(available==F)
  