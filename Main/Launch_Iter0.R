# Objectif : lancer tous les étapes de la première itération d'un scénario à partir d'une boucle 
#(jusqu'au renvoi dans IMACLIM-3ME)


# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))


# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
Iter=0
step_to_run = c(1,2,3,4,5)
ForceRerun = FALSE

for (scenario in scenario_v){
  for (horizon in horizon_v){
    for (scenario_classement in scenario_classement_v){
      for (redistribution in redistribution_v){
        
        #Reglage des folders
        initializeMatisseFiles()
        
        #Run the full Matisse_Loop, steps 1 to 5, for Iter=0
        #Logs into current_log
        if(!ForceRerun){
          for(step_it in step_to_run){
            if(iSLineInStepTracker(step_it))
              step_to_run = step_to_run[which(step_to_run != step_it)]
          }
        }
        
        if(length(step_to_run)>0){
          Matisse_Loop(step_to_run =  step_to_run, ForceRerun = ForceRerun)
        }else{
          AddLogs("MAIN","No step to run : stopping")
        }
        
      }
    }
  }
}




StandardValues4Test <- function(){
  
  #Sets up default values for the 4 parameters of Matisse to allow a 'defaut' run
  scenario <- scenario_v[1]
  horizon <- horizon_v[1]
  scenario_classement <- scenario_classement_v[1]
  redistribution <- redistribution_v[1]
  
}


AMS2035Default <- function(){
  
  #Sets up default values for the 4 parameters of Matisse to allow a 'defaut' run
  scenario <- "AMS"
  horizon <- 2035
  scenario_classement <- "Median"
  redistribution <- "niveau_vie"
  
}

