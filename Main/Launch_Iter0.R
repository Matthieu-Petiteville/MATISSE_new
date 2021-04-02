# Objectif : lancer tous les étapes de la première itération d'un scénario à partir d'une boucle (jusqu'au renvoi dans IMACLIM-3ME)


# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))


# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------

for (scenario in scenario_v){
  for (horizon in horizon_v){
    for (scenario_classement in scenario_classement_v){
      for (redistribution in redistribution_v){
        
        #Run the full Matisse_Loop, steps 1 to 5, for Iter=0
        #Logs into current_log
        Iter=0
        Matisse_Loop(1:5)
      
      }
    }
  }
}



