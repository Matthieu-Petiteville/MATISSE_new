

# LIBRARY -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(pracma)
library(data.table)
source(paste(M_home,"/Common/tools.R",sep=""))

# LOAD SCRIPT -------------------------------------------------------------

## Step 1
#Mise à l'échelle des revenus
source(paste(M_home,"/Step_1_Mise_echelle/1.2_fonction_mise_echelle_revenus.R",sep=""))
# Rétrocession de la taxe carbone
source(paste(M_home,"/Step_1_Mise_echelle/2.2_fonction_retrocession_carbon_tax.R",sep=""))
source(paste(M_home,"/Step_1_Mise_echelle/2.3_emission_gap_2010.R",sep=""))

## Step 2
# Microsimulation des dépenses
source(paste(M_home,"/Step_2_Microsimulation/1.2_fonction_microsimulation.R",sep=""))
# Evolution des consos d'énergie
source(paste(M_home,"/Step_2_Microsimulation/2.2_fonction_evolution_conso_energie.R",sep=""))
source(paste(M_home,"/Step_2_Microsimulation/calc_energie_kWh_m2.R",sep=""))

## Step 5
source(paste(M_home,"/Step_5_Export_IMACLIM/compute_savings_share_enermix.R",sep=""))




# DATA --------------------------------------------------------------------

## MICRO
load(MatisseFiles$menage_iteration_iterlast_rd)
save(menage_iteration,file=MatisseFiles$menage_iteration_iter_rd)
menage_iter_last<-menage_iteration

## MACRO
load(MatisseFiles$IMACLIM_iter_rd)


# CROISSANCE MARGINALE : FC -----------------------------------------------

#Facteurs de croissance (FC) entre les itérations. A la marge.
FC <- 
  IMACLIM %>%
  filter(year==9999) %>%
  filter(model=="Marge")%>% 
  #Careful, model here is "Marge" and not IMACLIM or ThreeME
  filter(Variable %in% c("revact",
                         "revpat",
                         "revchom",
                         "revsoc",
                         "revetranger",
                         "rdb",
                         "tauIR",
                         "tauAID",
                         "A01",
                         "A02",
                         "A03",
                         "A04",
                         "A05",
                         "A06",
                         "A07",
                         "A08",
                         "A09",
                         "A10",
                         "A11",
                         "A12",
                         "A13",
                         "A14")) %>%
  select(Variable,value)


print(FC)

FC <- FC %>%
  mutate(value=as.numeric(value)) %>%
  spread(key=Variable,value=value) 

#Save
save(FC,file= MatisseFiles$FC_horiz_marge_rd)




###
# STEP 1 ------------------------------------------------------------------
###
print(" STEP 1  : MISE à l'ECHELLE des REVENUS")

# MISE à l'ECHELLE des REVENUS --------------------------------------------

menage_iteration <- mise_echelle_revenu(FC,menage_iteration)


# RETROCESSION TCO --------------------------------------------------------


# Taxe carbone totale prélevée aux ménages et rétrocédée
TCO_tot<-as.numeric(IMACLIM %>% 
                      filter(year==horizon) %>%
                      filter(model=="IMACLIM")%>%
                      filter(Variable=="TCO_RTCD_tot")%>%
                      select(value))*10^6

# Taxe carbone €/tonne CO2eq
TCO<-as.numeric(IMACLIM %>% 
                  filter(year==horizon) %>%
                  filter(model=="IMACLIM")%>%
                  filter(Variable=="TCO")%>%
                  select(value))*10^6

# Appel Fonction ----------------------------------------------------------

menage_iteration <- retrocession_carbon_tax(TCO,TCO_tot,menage_iteration)





###
# STEP 2 ------------------------------------------------------------------
###
print(" STEP 2  : MICROSIMULATION DEPENSES")


# MICROSIMULATION DEPENSES ------------------------------------------------

menage_iteration<-microsimulation_depenses(menage_iteration,menage_iter_last,FC)

# Màj dettes et loyers 
# Pas de màj des surfaces dans les itérations (information de ThreeME)
menage_iteration <- 
  menage_iteration %>%
  mutate(c13711=c13711*FC$A05)%>%
  mutate(rev800=rev800*FC$A05)%>%
  mutate(c13211=c13211*FC$A05)%>%
  mutate(c13221=c13221*FC$A05)%>%
  mutate(c13211=c13211*FC$A05)%>%
  mutate(c13511=c13511*(1+TC_RDB_nominal))#bien joué Franck 


# APPEL FONCTION Evolution Conso Energie ----------------------------------

menage_iteration <- evolution_conso_ener(menage_iteration,FC)

# Save files --------------------------------------------------------------

save(menage_iteration,file= MatisseFiles$menage_iteration_iter_rd)






###
# STEP 5 ------------------------------------------------------------------
###
print(" STEP 5  : EXPORT IMACLIM")


# Export csv --------------------------------------------------------------

savings_rate<-compute_savings_rate_export(menage_iteration) 
share<-compute_share_export(menage_iteration)
ener_mix<-energie_mix(menage_iteration,FC=FC)
evol_energie<-compute_evol_energie(menage_iteration,scenario,horizon,scenario_classement,redistribution,Iter)


load(MatisseFiles$cout_baill_pub_rd)

load(MatisseFiles$sub_rehab_rd)

sBCE<-as.numeric(Subvention/(Subvention+menage_iteration%>%summarise(sum(pondmen*BTP))))
Cout_bailleur_public<-as.numeric(Cout_bailleur_public)

denom=sum(share$share_A01[1]+
            share$share_A05[1]+
            share$share_A06[1]+
            share$share_A08[1]+
            share$share_A09[1]+
            share$share_A10[1]+
            share$share_A11[1]+
            share$share_A12[1]+
            share$share_A13[1])

export <-  t(data.frame(
  "share_A01"=share$share_A01[1]/denom,
  "ELEC"=evol_energie$Elec,       #A02
  "GAZ"=evol_energie$Gaz,         #A03
  "SOLIDES"=evol_energie$Solides, #A04
  "share_A05"=share$share_A05[1]/denom,
  "share_A06"=share$share_A06[1]/denom,
  "OIL"=evol_energie$Oil,         #A07
  "share_A08"=share$share_A08[1]/denom,
  "share_A09"=share$share_A09[1]/denom,
  "share_A10"=share$share_A10[1]/denom,
  "share_A11"=share$share_A11[1]/denom,
  "share_A12"=share$share_A12[1]/denom,
  "share_A13"=share$share_A13[1]/denom,
  "epargne"=savings_rate,
  "sBCE"=sBCE,
  "Renovation_BS"=Cout_bailleur_public))



exp<-cbind(rownames(export),export[,1])
exp_df<-data.table(exp)

print(export)
print(exp_df)
print("Wait")
# for (i in 1:5)
# {
#   print(i)
#   date_time<-Sys.time()
#   while((as.numeric(Sys.time()) - as.numeric(date_time))<2.5){} #dummy while loop
# }
# print("End Wait")

# SAVE --------------------------------------------------------------------
excel_file_name <- MatisseFiles$input_macro_csv
file.remove(excel_file_name)
data.table::fwrite(exp_df,file=excel_file_name,dec=".",sep=";",col.names = F,row.names = F)
write.csv(exp,file=MatisseFiles$input_macro_check_csv)
write.csv(export,file=MatisseFiles$export_iter_csv)

# SUCCESS -----------------------------------------------------------------

print(paste("End Iteration : ",Iter,sep=""))
