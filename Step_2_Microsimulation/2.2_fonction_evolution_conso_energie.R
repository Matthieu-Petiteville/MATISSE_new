evolution_conso_ener<-function(menage_echelle,FC){
  
  
  # Importer les noms des variables existantes (Elec_clim et pas Solides_clim)
  load(paste(M_data,"/Data/Data_interne/list_source_usage.RData",sep=""))
  
  ## Source ##
  sources=c("Elec", "Gaz", "GPL", "Fuel", "Solides", "Urbain")
  
  
  ##
  # Verif préliminaire entre dep_Elec_verif et les composantes par usage
  ###
  for (source in sources){
    dep_source_verif=paste("dep",source,"verif",sep="_")
  }
  
  
  
  # MISE A L'ECHELLE --------------------------------------------------------
  
  
  
  # A02 : Electricité
  menage_echelle[c("Elec_ElecSpe","Elec_clim","Elec_ecl","Elec_ECS","Elec_chauff", "Elec_Cuisson","dep_Elec_verif" )]<-
    menage_echelle[c("Elec_ElecSpe","Elec_clim","Elec_ecl","Elec_ECS","Elec_chauff", "Elec_Cuisson","dep_Elec_verif")]*
    (1+menage_echelle$elast_prix_A02*(FC$A02/menage_echelle$IP_stone-1)) * (1+menage_echelle$elast_rev_A02*menage_echelle$TC_RDB_reel)*FC$A02
  
  # A03 : Gaz
  menage_echelle[c("Gaz_ECS","Gaz_chauff","Gaz_Cuisson","dep_Gaz_verif")]<-
    menage_echelle[c("Gaz_ECS","Gaz_chauff","Gaz_Cuisson","dep_Gaz_verif")]*(1+menage_echelle$elast_prix_A03*(-1+FC$A03/menage_echelle$IP_stone)) *
    (1+ menage_echelle$elast_rev_A03*menage_echelle$TC_RDB_reel)*FC$A03
  
  # A04 : Autres energies domestiques
  
  menage_echelle[c("dep_GPL_verif","GPL_Cuisson","GPL_chauff", "GPL_ECS")]<-
    menage_echelle[c("dep_GPL_verif","GPL_Cuisson","GPL_chauff", "GPL_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  
  menage_echelle[c("dep_Fuel_verif","Fuel_Cuisson","Fuel_chauff","Fuel_ECS")]<-
    menage_echelle[c("dep_Fuel_verif","Fuel_Cuisson","Fuel_chauff","Fuel_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  
  menage_echelle[c("dep_Solides_verif","Solides_Cuisson", "Solides_chauff","Solides_ECS")]<-
    menage_echelle[c("dep_Solides_verif","Solides_Cuisson", "Solides_chauff","Solides_ECS")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  
  menage_echelle[c("Urbain_ECS","Urbain_chauff","Urbain_Cuisson","dep_Urbain_verif")]<-
    menage_echelle[c("Urbain_ECS","Urbain_chauff","Urbain_Cuisson","dep_Urbain_verif")]*(1+menage_echelle$elast_prix_A04*(-1+FC$A04/menage_echelle$IP_stone)) *(1+ menage_echelle$elast_rev_A04*menage_echelle$TC_RDB_reel)*FC$A04
  
  
  
  # Energie surfacique ------------------------------------------------------
  
  
  menage_ener_dom<-energie_dom_surf(menage_echelle)
  
  menage_echelle<- 
    menage_echelle %>%
    # select(-ener_dom_surf,-ener_dom)%>%
    left_join(menage_ener_dom,by="ident_men")
  
}