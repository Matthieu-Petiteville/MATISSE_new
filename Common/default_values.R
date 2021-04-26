
# Values for param set ----------------------------------------------------
horizon_v <- c(2025,2030,2035)
scenario_v <- c("AMS","AME","ssRES","ssTCO","ssVE")
scenario_classement_v <- c("Median","Optimiste","Pessimiste","Optimal_co2","Optimal_ener","Poor","Rich")
redistribution_v <- c("decile","forfait","niveau_vie","ssrec","tuu")
Iteration_v <- 0:10


# Definition of the MatisseFiles object ------------------------------------------

initializeMatisseFiles <- function(){

  #Variable globale (<<-) qui définit les fichiers utilisés par Matisse
  #Peut ensuite être modifiée pour forcer l'utilisation d'autres fichiers
  MatisseFiles <<- list(
    #Fixed files
    ## External source
    "elast_xl"                 = paste(M_data,"/Data/Econometrie_demande/elasticite_demande_finale.xlsx",sep=""),
    "elast_rd"                 = paste(M_data,"/Data/Econometrie_demande/elasticite_demande.RData",sep=""),
    "menage_csv"               = paste(M_data,"/Data/BDF_2010/menage.csv",sep="") ,
    "depmen_bdf_csv"           = paste(M_data,"/Data/BDF_2010/depmen.csv",sep=""),
    "menage_calibr_2010_rd"    = paste(M_data,"/Data/BDFE_delauretis/menage_calibr_2010.RData",sep=""),
    "phebus_csv"               = paste(M_data,"/Data/PHEBUS/Clode_dpe_energie_decideur_revenu.csv",sep=""),
    "menage_bdf_xl"            = paste(M_data,"/Data/BDF_2010/menage.xlsx",sep=""),
    "indiv_bdf_xl"             = paste(M_data,"/Data/BDF_2010/individu.xlsx",sep=""),
    "c05_bdf_csv"              = paste(M_data,"/Data/BDF_2010/c05.csv",sep=""),
    "depmen_bdf_csv"           = paste(M_data,"/Data/BDF_2010/depmen.csv",sep=""),
    "typovuln_xl"              = paste(M_data,"/Data/Econometrie_demande/datacp_typovuln_bdf_2011.xlsx",sep=""),
    "appmen_depact_2010_rd"    = paste(M_data,"/Data/BDFE_delauretis/appmen_depensesactiv_2010.RData",sep=""),
    "auto_bdf_xl"              = paste(M_data,"/Data/BDF_2010/AUTOMOBILE.xlsx",sep=""),
    "def_rev_xl"               = paste(M_data,"/Data/Nomenclature_CIRED_ADEME/Definition_revenus.xlsx",sep=""),
    "nom_coicop_3me_xl"        = paste(M_data,"/Data/Nomenclature_CIRED_ADEME/Nomenclature_coicop_threeme.xlsx",sep=""),
    "source_usage_rd"          = paste(M_data,"/Data/Data_interne/list_source_usage.RData",sep=""),
    "prix_class_csv"           = paste(M_data,"/Data/BDFE_delauretis/Prix_energie_par_classe.csv",sep=""),
    "appmen_intensites_2010_rd"= paste(M_data,"/Data/BDFE_delauretis/appmen_intensites_2010.RData",sep=""),
    "coeff_ems_2010_rd"        = paste(M_data,"/Data/Data_interne/coeff_ems_2010.RData",sep=""),
    "ventes_vp_xl"             = paste(M_data,"/Data/ThreeME/Ventes_VP.xlsx",sep=""),
    "forcage_km_xl"            = paste(M_data,"/Data/ThreeME/forcage_vkm_teletravail_voirie.xlsx",sep=""),
    "insee_proj_men_xl"        = paste(M_data,"/Donnees_brutes/INSEE/INSEE - projection men.xlsx",sep=""),
    "insee_proj_pop_xl"        = paste(M_data,"/Donnees_brutes/INSEE/INSEE - projection pop.xlsx",sep=""),
    "pop_INSEE_rd"             = paste(M_data,"/Donnees_brutes/INSEE/pop_INSEE.RData",sep=""),
    
    ##IMACLIM
    "sortie_3me_xl"                     = paste(M_data,"/IMACLIM/Sorties Three-ME.xlsx",sep="") ,
    "output_macro_code_iter_0_ssrec_xl" = paste(M_data , "/IMACLIM/Output_macro_code_iter_0_ssrec.xlsx",sep=""),
    "output_macro_code_iter_0_xl"       = paste(M_data , "/IMACLIM/Output_macro_code_iter_0.xlsx",sep=""),
    "EMS_xl"                            = paste(M_data,"/IMACLIM/EMS.xlsx",sep=""),
    "coeff_dep_ems_csv"                 = paste(M_data,"/IMACLIM/coeff_dep_ems.csv",sep=""),
    "input_macro_csv"                   = paste(M_data,"/IMACLIM/Input_macro.csv",sep=""),
    "IMACLIM_3ME_ssrec_xl"              = paste(M_data,"/IMACLIM/IMACLIM 3ME_ssrec.xlsm",sep=""),
    "IMACLIM_3ME_xl"                    = paste(M_data,"/IMACLIM/IMACLIM 3ME.xlsm",sep=""),
    "output_macro_code_xl"              = paste(M_data,"/IMACLIM/Output_macro_code.xlsx",sep=""),
    
    
    ##Output Initial format
    "dpe_stock_2010_rd"       = paste(M_data,"/Output/Initial format/dpe_stock_2010.RData",sep=""),
    "menage_dpe_rd"           = paste(M_data,"/Output/Initial format/menage_DPE.RData",sep=""),
    "menage_forme_2_rd"       = paste(M_data,"/Output/Initial format/menage_forme_2.RData",sep=""),
    "dep_ener_2010_rd"        = paste(M_data,"/Output/Initial format/dep_ener_2010.RData",sep=""),
    "menage_forme_3_rd"       = paste(M_data,"/Output/Initial format/menage_forme_3.RData",sep=""),
    "menage_forme_4_rd"       = paste(M_data,"/Output/Initial format/menage_forme_4.RData",sep=""),
    "menage_forme_rd"         = paste(M_data,"/Output/Initial format/menage_forme.RData",sep=""),
    "savings_rate_2010_rd"    = paste(M_data,"/Output/Initial format/savings_rate_2010.RData",sep=""),
    "share_2010_rd"           = paste(M_data,"/Output/Initial format/share_2010.RData",sep=""),
    "ener_mix_2010_rd"        = paste(M_data,"/Output/Initial format/ener_mix_2010.RData",sep=""),
    
    #Conditional files
    "IMACLIM_rd"                         = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/IMACLIM.RData",sep=""),
    "Threeme_rd"                         = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/ThreeME.RData",sep=""),
    "FC_2010_horizon_rd"                 = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_0/Input/FC_2010_",horizon,".RData",sep=""),
    "EMS_scen_rd"                        = paste(M_data,"/Output/Projet_ADEME/",scenario,"/EMS.RData",sep=""),
    "menage_echelle_1_1_rd"              = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","/Iteration_0/Output/menage_echelle_1_1.RData",sep=""),
    "menage_echelle_1_rd"                = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_1.RData",sep=""),
    "menage_echelle_2_1_rd"              = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_2_1.RData",sep=""),
    "menage_echelle_2_rd"                = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_echelle_2.RData",sep=""),
    "IMACLIM_3ME_scen_horiz_xl"          = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/","Optimiste","/","ssrec","/IMACLIM_3ME.xlsx",sep=""),
    "menage_echelle_31_rd"               = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_31.RData",sep=""),
    "menage_echelle_32_rd"               = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_32.RData",sep=""),
    "sBCE_rd"                            = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/sBCE.RData",sep=""),
    "sub_rehab_rd"                       = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Subvention_rehab.RData",sep=""),
    "cout_baill_pub_rd"                  = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/Cout_bailleur_public.RData",sep=""),
    "menage_echelle_33_pre_revent_rd"    = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_33_avant_reventil.RData",sep=""),
    "menage_echelle_33_rd"               = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_33.RData",sep=""),
    "menage_echelle_TC_DPE_rd"           = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_DPE.RData",sep=""),
    "bm_net_csv"                         = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/BM_net.csv",sep=""),
    "menage_echelle_TC_VE_pre_revent_rd" = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE_avant_reventil.RData",sep=""),
    "menage_echelle_TC_VE_rd"            = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Technical_change","/menage_echelle_TC_VE.RData",sep=""),
    "menage_echelle_rd"                  = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/menage_echelle.RData",sep=""),
    "agreg_best_rd"                      = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/agreg_best.RData",sep=""),
    "menage_calibr_2010_final_rd"        = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/menage_calibr_2010.RData",sep=""),
    "menage_contraintes_rd"              = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_contraintes.RData",sep=""),
    "pond_init_rd"                       = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/pond_init.RData",sep=""),
    "diff_pond_rd"                       = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/diff_pond.RData",sep=""),
    "pond_final_rd"                      = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/pond_final.RData",sep=""),
    "agreg_final_rd"                     = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/agreg_final.RData",sep=""),
    "menage_echelle_final_rd"            = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/menage_echelle.RData",sep=""),
    "agreg_final_csv"                    = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/agreg_final.csv",sep=""),
    "pond_final_heurist_scen_horiz_rd"   = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/pond_final_heuristique_",scenario,"_",horizon,".RData",sep=""),
    "pond_final_heurist_scen_horiz_txt"  = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/READ_ME_pond_final_heuristique_",scenario,"_",horizon,".txt",sep=""),
    "export_iter_0_csv"                  = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/export_Iter_0.csv",sep=""),
    "menage_iteration_rd"                = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Output/menage_iteration.RData",sep=""),
    "IMACLIM_iter_xl"                    = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM_3ME_iter",Iter,".xlsx",sep=""),
    "output_macro_iter_xl"               = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Output_macro_code_iter",Iter,".xlsx",sep=""),
    "IMACLIM_iter_rd"                    = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/IMACLIM.RData",sep=""),
    "output_micro_xl"                    = paste(M_data,"/IMACLIM/Output_micro.xlsx",sep=""),
    "output_micro_iterlast_xl"           = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",ifelse(Iter==0,0,Iter-1),"/Output/Output_micro_iter",ifelse(Iter==0,0,Iter-1),".xlsx",sep=""),
    "output_micro_iter_xl"               = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/Output_micro_iter",Iter,".xlsx",sep=""),
    "FC_horizon_iter_rd"                 = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/","Iteration_",Iter,"/Input/FC_2010_",as.character(horizon),".RData",sep=""),
    "iterations_scen_xl"                 = paste(M_data,"/IMACLIM/Iterations_scenarios.xlsx",sep=""),
    "iterations_scen_n_1_xl"             = paste(M_data,"/IMACLIM/Iterations_scenarios_n-1.xlsx",sep=""),
    "iterations_scen_iter_xl"            = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/Iterations_scenarios.xlsx",sep=""),
    "folder_params"                      = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,sep=""),
    "output_micro_params_xl"             = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_micro.xlsx",sep=""),
    "IMACLIM_params_xl"                  = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/IMACLIM_3ME.xlsx",sep=""),
    "output_macro_params_xl"             = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Output_macro_code.xlsx",sep=""),
    "menage_iteration_iter_rd"           = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_iteration.RData",sep=""),
    "menage_iteration_iterlast_rd"       = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",ifelse(Iter==0,0,Iter-1),"/Input/menage_iteration.RData",sep=""),
    "menage_iteration_params_rd"         = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/menage_iteration.RData",sep=""),
    "FC_horiz_marge_rd"                  = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/FC_",horizon,"_marge.RData",sep=""),
    "input_macro_check_csv"              = paste(M_data,"/IMACLIM/Input_macro_check.csv",sep=""),
    "export_iter_csv"                    = paste(M_data,"/Output/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Output/export_Iter_",Iter,".csv",sep=""),
    "output_pdf"                         = paste(M_data,"/Output/Projet_Ademe/Results/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Report.pdf",sep="")
  )

} 



# Files required ----------------------------------------------------------

mandatory_data_files <- c(
  "/IMACLIM/Sorties Three-ME.xlsx",
  "/IMACLIM/Output_macro_code_iter_0_ssrec.xlsx",
  "/IMACLIM/Output_macro_code_iter_0.xlsx",
  "/IMACLIM/EMS.xlsx",
  "/IMACLIM/coeff_dep_ems.csv",
  "/Data/Econometrie_demande/elasticite_demande_finale.xlsx",
  "/Data/BDF_2010/menage.csv",
  "/Data/BDF_2010/depmen.csv",
  "/Data/BDFE_delauretis/menage_calibr_2010.RData",
  "/Data/PHEBUS/Clode_dpe_energie_decideur_revenu.csv",
  "/Data/BDF_2010/menage.xlsx",
  "/Data/BDF_2010/individu.xlsx",
  "/Data/BDF_2010/c05.csv",
  "/Data/Econometrie_demande/datacp_typovuln_bdf_2011.xlsx",
  "/Data/BDFE_delauretis/appmen_depensesactiv_2010.RData",
  "/Data/BDF_2010/AUTOMOBILE.xlsx",
  "/Data/Nomenclature_CIRED_ADEME/Definition_revenus.xlsx",
  "/Data/Nomenclature_CIRED_ADEME/Nomenclature_coicop_threeme.xlsx",
  "/Data/Data_interne/list_source_usage.RData",
  "/Data/BDFE_delauretis/Prix_energie_par_classe.csv",
  "/Data/BDFE_delauretis/appmen_intensites_2010.RData",
  "/Data/Econometrie_demande/elasticite_demande.RData",
  "/Data/Data_interne/coeff_ems_2010.RData",
  "/Data/ThreeME/Ventes_VP.xlsx",
  "/Donnees_brutes/INSEE/INSEE - projection men.xlsx",
  "/Donnees_brutes/INSEE/INSEE - projection pop.xlsx",
  "/Donnees_brutes/INSEE/pop_INSEE.RData"
)

