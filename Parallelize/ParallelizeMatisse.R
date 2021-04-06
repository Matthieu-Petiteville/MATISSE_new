# Function that will allow to parallelize the run of the Matisse loop function by opening
# multiple R script windows to run each instand independantly. 
# The r scripts are kept in M_data/Parallelize. They are created at the start of
# the function below and they are removed by the function itself. Succes or not of the
# script is dependant on Matisse_loop.

Parallelize_Matisse_Loop <- function(step_to_run = 1:5, ForceRerun = FALSE) {
  
  library(readr)

  #Rédaction d'un fichier de script r et de son contenu + un fichier bat
  script_file <- paste(M_data,"/Parallelize/MatisseLoop_",
                       paste(scenario,horizon,scenario_classement,redistribution,
                             paste(step_to_run,collapse = "-"),sep="_")
                       ,sep="")
  script_bat <- paste(script_file,".bat",sep="")
  script_file <- paste(script_file,".r",sep="")
  
  script_text <- c(paste("source(\"",M_home,"/Matisse_init.R\")",sep=""),
                   paste("source(\"",M_home,"/Main/Matisse_Loop.R\")",sep=""),
                   paste("scenario <- ","\"",scenario,"\"",sep=""),
                   paste("horizon <- ",horizon,sep =""),
                   paste("scenario_classement <- ","\"",scenario_classement,"\"",sep=""),
                   paste("redistribution <- ","\"",redistribution,"\"",sep =""),
                   paste("Iter <- ","\"",Iter ,"\"",sep ="")
                   )
  bat_text <- paste(paste(Sys.getenv("R_HOME"),"/bin/Rscript.exe ",script_file,sep=""))
  
  if(length(step_to_run)>1){
    script_text <- c(script_text,
                     paste("Matisse_Loop ( step_to_run = ",
                           paste("c(",paste(step_to_run,collapse = ","),")",sep=""),
                      " , ForceRerun = ",ForceRerun," )",sep=""))
  }else{
    script_text <- c(script_text,
                     paste("Matisse_Loop ( step_to_run = ",step_to_run[1],
                           " , ForceRerun = ",ForceRerun," )",sep=""))
  }
  script_text <- c(script_text,
                   paste("file.remove(\"",script_bat,"\")",sep=""))
  
  #Creation des fichiers bat et scripts
  if(file.exists(script_file)){file.remove(script_file)}
  write_file(paste(script_text,collapse="\n"),
        script_file)
  if(file.exists(script_bat)){file.remove(script_bat)}
  write_file(paste(bat_text,collapse="\n"),
             script_bat)
  
  shell.exec(script_bat)

}

CleanParallelizeFolder <- function(){
  
  files_list <- dir(paste(M_data,"/Parallelize",sep=""))
  for(file_it in files_list){
    if (substr(file_it,nchar(file_it)-1,nchar(file_it))==".r"){
      bat_file <- paste(substr(file_it,1,nchar(file_it)-2),".bat",sep="")
      if(!file.exists(paste(M_data,"/Parallelize/",bat_file,sep=""))){
        file.remove(paste(M_data,"/Parallelize/",file_it,sep=""))
      }
    }
  }  

}

CountActiveRScripts <- function(){
  
  files_list <- dir(paste(M_data,"/Parallelize",sep=""))
  Act_count = 0
  for(file_it in files_list){
    if (substr(file_it,nchar(file_it)-1,nchar(file_it))==".r"){
      Act_count <- Act_count +1
    }
  }
  return(Act_count)
}

