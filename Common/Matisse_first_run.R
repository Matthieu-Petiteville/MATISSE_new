Initialize_output_folders <- function(){
  
  #Create all folders and path for MATISSE set up and checks
  
  source(paste(M_home,"/Common/default_values.r",sep=""))
  source(paste(M_home,"/Common/tools.r",sep=""))  
  
  CreateFolder(paste(M_data,"/Output",sep=""))
  CreateFolder(paste(M_data,"/Logs",sep=""))
  CreateFolder(paste(M_data,"/Output/Initial format",sep=""))
  CreateFolder(paste(M_data,"/Output/Projet_Ademe",sep=""))
  CreateFolder(paste(M_data,"/Output/Projet_Ademe/Results",sep=""))
  
  for(scenario in scenario_v){
    CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,sep=""))
    CreateFolder(paste(M_data,"/Output/Projet_Ademe/Results/",scenario,sep=""))
      for(horizon in horizon_v){
      CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,sep=""))
      CreateFolder(paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,sep=""))
      for(scenario_classement in scenario_classement_v){
        CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,sep=""))
        CreateFolder(paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,sep=""))
        for(redistribution in redistribution_v){
          CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""))
          CreateFolder(paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""))
          for(Iteration in Iteration_v){
            CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,sep=""))
            CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,"/Input",sep=""))
            CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iteration,"/Output",sep=""))
          }
          CreateFolder(paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change",sep=""))
        }
      }
    }    
  }
  

}

Check_missing_files <- function(){

#Does a full check on mandatory files to run MATISSE
  
  for(mandatory_data_file in mandatory_data_files){
    if(!file.exists(paste(M_data,mandatory_data_file,sep=""))){cat("Missing file ",M_data,mandatory_data_file,"\n",sep=" ")}
  }
  for(scenario in scenario_v){
    for(horizon in horizon_v){
      file_name <- paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep="")
      if(!file.exists(file_name)){cat("Missing file ",file_name,"\n",sep=" ")}
    }  
  }

}

CopyImaclimFiles <- function(){
  
  for(scenario in scenario_v){
    for(horizon in horizon_v){
      file_name_from <- paste("D:/Stage_Petiteville/Projet_Ademe/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep="")
      file_name_to <- paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep="")
      file.copy(file_name_from,file_name_to)
    }  
  }
  

}
