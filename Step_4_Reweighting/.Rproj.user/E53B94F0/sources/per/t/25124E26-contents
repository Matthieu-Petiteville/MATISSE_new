# Iter=0
# contraintes_micro <- function(Iter){

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------

setwd("D:/CIRED/Projet_Ademe")
load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_0/Input/menage_calibr_2010.RData",sep=""))



load(paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))



source("D:/CIRED/Projet_Ademe/Code_global_Ademe/mutate_when.R")

# Ajout Data DPE ----------------------------------------------------------

# menage_echelle
menage_echelle$DPE_horizon<-as.factor(menage_echelle$DPE_horizon)
dummies_dpe_horizon <- model.matrix(~ DPE_horizon, data = menage_echelle, 
                                 contrasts.arg = list(DPE_horizon = contrasts(menage_echelle$DPE_horizon, contrasts = F)))[,-1]
colnames(dummies_dpe_horizon)<-paste("DPE_m2",LETTERS[1:7],sep="_")
dummies_dpe_horizon<-data.frame("ident_men"=menage_echelle$ident_men,dummies_dpe_horizon)
menage_echelle <-
  menage_echelle %>%
  left_join(dummies_dpe_horizon,by='ident_men') %>%
  mutate_at(vars(starts_with("DPE_m2_")),function(x) as.numeric(x*menage_echelle$surfhab_d))
# head(menage_echelle %>% select(starts_with("DPE_m2_")))
# On exclut G pour le bouclage avec surfhah_d

# AGREGATE DATA -----------------------------------------------------------

menage_contraintes <-
  menage_calibr_2010 %>%
  select(ident_men,list_contraintes_menage)%>%
  left_join(
    menage_echelle %>%
           select(ident_men,
                  npers,
                  nbactoccup,
                  nbchomeurs,
                  rev_activites_sans_etranger,
                  rev_patrimoine,
                  chomage,
                  rev_sociaux_autres,
                  rev_etranger,
                  surfhab_d,
                  DPE_m2_A,
                  DPE_m2_B,
                  DPE_m2_C,
                  DPE_m2_D,
                  DPE_m2_E,
                  DPE_m2_F,
                  # nbvehic,
                  # VE,
                  new_VT,
                  new_VE
                      ),    
    by="ident_men") 

  
# menage_contraintes <-
#   menage_contraintes %>%
#   mutate_when(is.na(nbactoccup),list(nbactoccup=0),
#               is.na(nbchomeurs),list(nbchomeurs=0),
#               is.na(chomage),list(chomage=0)
              # ,
              # is.na(nbvehic),list(nbvehic=0)
              # )

#verifier que les les ménages avec NA en nbactoccup  que pas de revenus du travail + pareil pour chomage (rev_chomage==0), dep_carb



# POND_INIT ---------------------------------------------------------------

pond_init <-
  menage_echelle %>%
  select(pondmen)


# SAVE FILES --------------------------------------------------------------

save(menage_contraintes,file=
       paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/menage_contraintes.RData",sep=""))
save(pond_init,file=
       paste("D:/CIRED/Projet_Ademe/",scenario,"/",horizon,"/",scenario_classement,"/",redistribution,"/Iteration_",Iter,"/Input/pond_init.RData",sep=""))
# save(menage_echelle,file=
       # paste("2025/Iteration_",Iter,"/Input/menage_echelle.RData",sep=""))
# }
print("Repondération : 2_contraintes_micro : SUCCESS")

