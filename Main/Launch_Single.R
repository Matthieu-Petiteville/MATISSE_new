# Objectif : lancer tous les étapes pour un jeu de params


# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))


# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
Iter=0
step_to_run = c(1,2,3)
ForceRerun = FALSE
scenario <- "AMS"
horizon <- 2035
scenario_classement <- "Median"
redistribution <- "niveau_vie"


#Run the full Matisse_Loop, steps 1 to 5, for Iter=0
#Logs into current_log
if(!ForceRerun){
  for(step_it in step_to_run){
    if(iSLineInStepTracker(step_it))
      step_to_run = step_to_run[which(step_to_run != step_it)]
  }
}

Matisse_Loop(step_to_run =  step_to_run, ForceRerun = ForceRerun)
