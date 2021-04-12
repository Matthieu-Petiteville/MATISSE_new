# Objectif : lancer tous les étapes de la première itération d'un scénario à partir d'une boucle (jusqu'au renvoi dans IMACLIM-3ME)


# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))
source(paste(M_home,"/Parallelize/ParallelizeMatisse.r",sep=""))
MaxRscript = 3

CurrentRScript = 0
# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
for (scenario in scenario_v){
  for (horizon in horizon_v){
    for (scenario_classement in scenario_classement_v){
      for (redistribution in redistribution_v){
        
        CleanParallelizeFolder()
        CurrentRScript <- CountActiveRScripts()
        
        #Boucle pour éviter d'avoir trop de scripts ouverts
        while(CurrentRScript>=MaxRscript){
          Sys.sleep(10)
          CleanParallelizeFolder()
          CurrentRScript <- CountActiveRScripts()
        } 
        
        
        #Run the full Matisse_Loop, steps 1 to 5, for Iter=0
        #Logs into current_log
        Iter=0
        AddLogs("PARAL","Running script for step 1")
        step_to_run = c()
        for(step_it in 1:3){
          if(iSLineInStepTracker(step_it))
            step_to_run = c(step_to_run,step_it)
        }
       Parallelize_Matisse_Loop(step_to_run =  step_to_run, ForceRerun = FALSE)

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