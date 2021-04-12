#Validation of data vs data already run

M_home <- gsub("\\\\","/",Sys.getenv("MATISSE_HOME"))
M_data <- gsub("\\\\","/",Sys.getenv("MATISSE_DATA"))
source(paste(M_home,"/Common/tools.R",sep=""))
source(paste(M_home,"/Common/default_values.r",sep=""))
source(paste(M_home,"/Main/Matisse_Loop.r",sep=""))
library(tidyverse)



#Sets up default values for the 4 parameters of Matisse to allow a 'defaut' run
scenario <- scenario_v[1]
horizon <- horizon_v[1]
scenario_classement <- scenario_classement_v[1]
redistribution <- redistribution_v[1]
Iter = 0


folder_reference <- paste("D:/Stage_Petiteville/Projet_Ademe/Output/Projet_Ademe/",
                          scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change/",sep="")
folder_new <- paste(M_data,"/Output/Projet_Ademe/",
                    scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change/",sep="")

# folder_reference <- paste("D:/Stage_Petiteville/Projet_Ademe/Output/Step_0/",sep="")
# folder_new <- paste(M_data,"/Output/Initial format/",sep="")




ref_files <- list.files(folder_reference,all.files = TRUE, recursive = TRUE)
new_files <- list.files(folder_new,all.files = TRUE, recursive = TRUE)

common_files <- c()
for(files_it in new_files){
  if(files_it %in% ref_files){
    common_files <- c(common_files,files_it)
  }
}


#Creation de csv

for(files_it in common_files){
  # files_it = common_files[3]
  
  if(length(grep(".RData",files_it))>0){
    ref_file_name <- load(paste(folder_reference,"/",files_it,sep=""))
    ref_data <- as.data.frame(eval(parse(text = ref_file_name)))
    new_file_name <- load(paste(folder_new,"/",files_it,sep=""))
    new_data <- as.data.frame(eval(parse(text = new_file_name)))
  
    print(files_it)
    for(name_it in names(new_data)){
      if(name_it %in% names(ref_data)){
        l_idx <- which(new_data[,name_it]!=ref_data[,name_it])
        if(length(l_idx)==0 && name_it != "ident_men"){
          ref_data <- ref_data %>% select(-c(name_it))
          new_data <- new_data %>% select(-c(name_it))
        }
      }else{
        new_data <- new_data %>% select(-c(name_it))
      }
    }
  
    if(ncol(ref_data)>1){
      write.table(ref_data,file = paste(M_data,"/Validation/ref_",gsub(".RData","",files_it),".csv",sep=""),
                quote = FALSE,row.names = FALSE,sep=";",dec=",")
      write.table(new_data,file = paste(M_data,"/Validation/new_",gsub(".RData","",files_it),".csv",sep=""),
                quote = FALSE,row.names = FALSE,sep=";",dec=",")
    
    }
  }
}