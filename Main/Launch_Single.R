# Objectif : lancer tous les étapes pour un jeu de params

# Launch ------------------------------------------------------------------
M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
M_analysis <- gsub("\\\\","/",Sys.getenv("MATISSE_ANALYSIS"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))


# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
Iter=0
scenario <- "AMS"
horizon <- 2035
scenario_classement <- "Optimiste"
scenario_classement_veh <- "Median"
scenario_classement_bascule <<- "Optimiste"
# redistribution <- "forfait"
#redistribution <- "niveau_vie"
#redistribution <- "decile"
#redistribution <- "tuu"
# redistribution <- "dectuu_grad"
# redistribution <- "seq_dectuu"
redistribution <- "dectuu_seq_ucmat"


#Bascule standard
class_force_bascule <<- c()
year_new_bascule <<- 2100
bascule_min_jump <<- 7

#Bascule align
align_bascule_on_3ME <<- T
align_class_bascule <<- c("A")
align_jump_bascule <<- 6:0
align_yearnew_bascule <<- list(fio = 2021, gaz = 2021)

#Reglage des folders
Create_Output_Folders(paste(M_data,"/Output/Projet_Ademe",sep="") , scenario , horizon , scenario_classement ,redistribution , iter_v = 0:10)
initializeMatisseFiles()

# Loop on scenarios/horizon/classement/redistribution -------------------------------------------------------
step_to_run = c(1,2,3,4)
ForceRerun = TRUE
Matisse_Loop(step_to_run =  step_to_run, ForceRerun = ForceRerun)

step_to_run = c(5,6)
ForceRerun = TRUE
Matisse_Loop(step_to_run =  step_to_run, ForceRerun = ForceRerun)
