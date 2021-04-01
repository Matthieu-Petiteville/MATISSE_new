CopyIMACLIM_results <- function(){
  
  source(paste(M_home,"/Common/default_values.r",sep=""))
  
  for (scenario in scenario_v){
    for (horizon in horizon_v){
      file.copy(paste("D:/Stage_Petiteville/Projet_Ademe/Output/Projet_Ademe/Results/",scenario ,"/",horizon,"/Optimiste/ssrec/IMACLIM_3ME.xlsx",sep=""),
                paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep=""))   
  
    }
  }
  
}