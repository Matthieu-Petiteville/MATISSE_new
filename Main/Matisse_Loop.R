# Fonction permettant de lancer tout ou partie des différents steps

Matisse_Loop <- function(step_to_run = 1:5, ForceRerun = FALSE){

  AddLogs("MAIN",paste("Running MatisseLoop with steps",paste(step_to_run,collapse = "/"),sep =" "))

  ####
  # Step 1
  ####
  if(1 %in% step_to_run){
    if(ForceRerun || !iSLineInStepTracker(1)){
      AddLogs("MAIN","Running step 1")
  
      source(paste(M_home,"/Step_1_Mise_echelle/0_mise_forme_data_3ME.R",sep=""))
      AddLogs("MAIN","Completed step 1.0")
      
      source(paste(M_home,"/Step_1_Mise_echelle/1.1_mise_echelle_revenus.R",sep=""))
      AddLogs("MAIN","Completed step 1.1")
      
      source(paste(M_home,"/Step_1_Mise_echelle/2.1_retrocession_carbon_tax.R",sep=""))
      AddLogs("MAIN","Completed step 1.2")
      AddLineToStepTracker(1)
    }else{
      AddLogs("MAIN","Not running step 1 : it's already in StepTracker and ForceRerun is FALSE")
    }
  }

  ####
  # Step 2
  ####
  if(2 %in% step_to_run){
    if(ForceRerun || !iSLineInStepTracker(2)){
      AddLogs("MAIN","Running step 2")
    
      source(paste(M_home,"/Step_2_Microsimulation/1.1_Microsimulation.R",sep=""))
      AddLogs("MAIN","Completed step 2.1")
  
      source(paste(M_home,"/Step_2_Microsimulation/2.1_evolution_conso_energie.R",sep=""))
      AddLogs("MAIN","Completed step 2.2")
    }else{
      AddLogs("MAIN","Not running step 2 : it's already in StepTracker and ForceRerun is FALSE")
    }
  }

  ####
  # Step 3
  ####
  if(3 %in% step_to_run){
    if(ForceRerun || !iSLineInStepTracker(3)){
      AddLogs("MAIN","Running step 3")
      
      source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_1_Achat_neuf_horizon.R",sep=""))
      AddLogs("MAIN","Completed step 3.1.1")
  
      source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_2_Achat_2010_horizon-1.R",sep=""))
      AddLogs("MAIN","Completed step 3.1.2")
  
      source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_3_Rehab_2010_horizon.R",sep=""))
      AddLogs("MAIN","Completed step 3.1.3")
  
      source(paste(M_home,"/Step_3_Technical_Change/3_1_TC_DPE/3_1_4_Bascule_A.R",sep=""))
      AddLogs("MAIN","Completed step 3.1.4")
      
      source(paste(M_home,"/Step_3_Technical_Change/3_2_TC_VE/3_2_2_VE_2010_horizon.R",sep=""))
      AddLogs("MAIN","Completed step 3.2")
    }else{
      AddLogs("MAIN","Not running step 3 : it's already in StepTracker and ForceRerun is FALSE")
    }
  }

  ####
  # Step 4
  ####
  if(4 %in% step_to_run){
    if(ForceRerun || !iSLineInStepTracker(4)){
      AddLogs("MAIN","Running step 4")
      
      source(paste(M_home,"/Step_4_Reweighting/1_contraintes_macro.R",sep=""))
      AddLogs("MAIN","Completed step 4.1")
  
      Iter = 0
      source(paste(M_home,"/Step_4_Reweighting/2_contraintes_micro.R",sep=""))
      AddLogs("MAIN","Completed step 4.2")
  
      source(paste(M_home,"/Step_4_Reweighting/3_pondr.R",sep=""))
      AddLogs("MAIN","Completed step 4.3")
    }else{
      AddLogs("MAIN","Not running step 4 : it's already in StepTracker and ForceRerun is FALSE")
    }
  }

  ####
  # Step 5
  ####
  if(5 %in% step_to_run){
    if(ForceRerun || !iSLineInStepTracker(5)){
      AddLogs("MAIN","Running step 5")
  
      source(paste(M_home,"/Step_5_export_IMACLIM/1_export_to_IMACLIM.R",sep=""))
      AddLogs("MAIN","Completed step 5")
    }else{
      AddLogs("MAIN","Not running step 5 : it's already in StepTracker and ForceRerun is FALSE")
    }
  }

  
}
