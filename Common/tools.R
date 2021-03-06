
# Chronometre -------------------------------------------------------------

#Basic function starting the chrono for benchmark of solutions
ChrStart <- function(){
  chrono_timest <<- Sys.time()
}

#Basic function printing the chrono for benchmark of solutions
ChrPrint <- function(){
  print(Sys.time()-chrono_timest)
}

#Basic function stopping the chrono and removing the variable for benchmark of solutions
ChrEnd <- function(){
  print(Sys.time()-chrono_timest)
  glob_var <- ls(pos = ".GlobalEnv")
  rm(list = glob_var[grep("chrono_timest", glob_var)] , pos=".GlobalEnv")
}







# MutateWhen --------------------------------------------------------------

#Similar to dplyr mutate
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}
# Source : https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-on-a-subset-of-rows






# Folder Management -------------------------------------------------------

#Create folder if it doesn't exist, no warning
CreateFolder <- function(folder_name){
  if(!dir.exists(folder_name)){dir.create(folder_name)}
}


Create_Output_Folders <- function(MainFolder , scen_v , hori_v , scen_class_v , redis_v , iter_v){
  
  #Create all folders and path for MATISSE set up and checks
  CreateFolder(MainFolder)
  CreateFolder(paste(MainFolder,"/Results",sep=""))
  
  for(scenario in scen_v){
    CreateFolder(paste(MainFolder,"/",scenario,sep=""))
    CreateFolder(paste(MainFolder,"/Results/",scenario,sep=""))
    for(horizon in hori_v){
      CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,sep=""))
      CreateFolder(paste(MainFolder,"/Results/",scenario,"/",horizon,sep=""))
      for(scenario_classement in scen_class_v){
        CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,sep=""))
        CreateFolder(paste(MainFolder,"/Results/",scenario,"/",horizon,"/",scenario_classement,sep=""))
        for(redistribution in redis_v){
          CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""))
          CreateFolder(paste(MainFolder,"/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""))
          for(Iteration in iter_v){
            CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,sep=""))
            CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,"/Input",sep=""))
            CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,"/Output",sep=""))
          }
          CreateFolder(paste(MainFolder,"/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change",sep=""))
        }
      }
    }    
  }
  
  
}




# Logs --------------------------------------------------------------------

#Create log file if it doesn't exist
CreateLogFile <- function(){
    file_name <- paste(M_data,"/Logs/","log_",Sys.Date(),".log",sep="") 
    if(!file.exists(file_name)){file.create(file_name)}
    write("Timestamp;Params;LogType;Log",file = file_name,append = TRUE)
    return(file_name)
}


#Add standardized logs
AddLogs <- function(LogType,LogTxt){
  print(paste(Sys.time(),paste("[",paste(scenario,horizon,scenario_classement,redistribution,sep="/"),"]",sep=""),LogType,LogTxt,sep=";"))
}




# StepTracker -------------------------------------------------------------

#Add line to StepTracker
AddLineToStepTracker <- function(step_done){
  
  step_tracker_file <- paste(M_data,"/Logs/StepTracker.csv",sep="")
  step_tracker_line = data.frame(TimeStamp = as.character(Sys.time()),scenario = scenario,horizon = horizon,scenario_classement = scenario_classement,
                                 redistribution = redistribution,iteration = Iter, step_done = step_done)
  
  if(file.exists(step_tracker_file)){
    step_tracker <- read.csv(step_tracker_file)
    step_tracker <- rbind(step_tracker,step_tracker_line)
  }else{
    step_tracker = step_tracker_line  
  }
  
  write.csv(step_tracker,step_tracker_file,row.names = FALSE,quote = FALSE)

}

#Checks if line is already in step tracker, the file that records all steps already run
iSLineInStepTracker <- function(step_to_check){

  library(tidyverse)
  scenario_var <- scenario
  horizon_var <- horizon
  class_var <- scenario_classement
  redis_var <- redistribution
  
  step_tracker_file <- paste(M_data,"/Logs/StepTracker.csv",sep="")
  if(!file.exists(step_tracker_file)){return(FALSE)}
  step_tracker <- read.csv(step_tracker_file)
  test_step <- step_tracker %>%
    filter(scenario == scenario_var,horizon == horizon_var,
           scenario_classement == class_var , redistribution == redis_var ,
           iteration == Iter, step_done == step_to_check)
  
  if(nrow(test_step)>0){return(TRUE)}else{return(FALSE)}
  
}

#Shows the StepTracker with the current set of params
ShowStepTracker <- function(){
  
  scenario_var <- scenario
  horizon_var <- horizon
  class_var <- scenario_classement
  redis_var <- redistribution
  
  step_tracker_file <- paste(M_data,"/Logs/StepTracker.csv",sep="")
  if(!file.exists(step_tracker_file)){return(FALSE)}
  step_tracker <- read.csv(step_tracker_file)
  
  step_tracker <- step_tracker %>%
    filter(scenario == scenario_var,horizon == horizon_var,
           scenario_classement == class_var , redistribution == redis_var,
           iteration == Iter)
  
  print(step_tracker)
  
}

