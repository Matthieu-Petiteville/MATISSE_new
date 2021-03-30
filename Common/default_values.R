horizon_v <- c(2025,2030,2035)
scenario_v <- c("AMS","AME","ssRES","ssTCO","ssVE")
scenario_classement_v <- c("Median","Optimiste","Pessimiste","Optimal_co2","Optimal_ener","Poor","Rich")
redistribution_v <- c("decile","forfait","niveau_vie","ssrec","tuu")
Iteration_v <- 0:10


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

