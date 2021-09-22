


# pondR <- function(Iter,m2VP){
  
#Comment 06/05/2019 : en faisant tourner le code sans les contraintes INSEE ni le taux d'épargne, on obtient des résultats tels qu'attendus
#(pas de ménage qui tombe à une pondération nulle), 25 minutes pour faire tourner l'algorithme. 
# a faire : vérifier que les agrégats INSEE ne sont pas trop loin des agrégats à respecter, s'ils sont proches c'est embêtant, 
#cela veut dire que les contraintes INSEE sont trop fortes pour respecter les contraintes macro. 
# Iter=1


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(quadprog)


# DATA --------------------------------------------------------------------
load(MatisseFiles$menage_contraintes_rd)
load(MatisseFiles$pond_init_rd)
load(MatisseFiles$menage_echelle_rd)
load(MatisseFiles$agreg_best_rd)
load(MatisseFiles$menage_forme_rd)

# Pond_init issues autre scenario ------------------------------------------

     
# pond_init<-pond_init*as.numeric(FC_pondmen)
  
  #-------------------------------------------------------------------------------
  #             Importation
  #-------------------------------------------------------------------------------
  # (A) BASE DE DONNEE
  data<-menage_contraintes %>% select(-ident_men)
  ones<-cbind(rep(1,dim(data)[1]))
  data<-as.matrix(cbind(ones,data))

  # (W) POIDS INITIAUX
  pond_init <- as.matrix(pond_init)
  # (S) AGREGAT INITIAUX
  Amat <- t(data)
  agreg_init <- (Amat %*% pond_init)

  # (T) AGREGAT A RESPECTER
  agreg_best <- t(agreg_best)

  
  #-------------------------------------------------------------------------------
  #             Transformation
  #-------------------------------------------------------------------------------
  # (Vmat) MATRICE DIAGONALE DES POIDS A MINIMISER
  Vmat <- as.numeric(t(1/((pond_init)^2)))
  Vmat <- as.matrix(diag(Vmat))
  n = nrow(Vmat)

  
  
  # (dvec) VECTEUR A MINIMISER
  dvec <- t(vector("numeric",n))
  
  # (Amat) CONTRAINTE MATRICIELLE
  
  # (bvec) CONTRAINTE D EGALITE (valeurs de départ)
  bvec <- agreg_best - agreg_init
  
  # test (permet de comparer les tables cible et de départ avec une distance en % way to go)
  View(cbind(rownames(agreg_init),rownames(agreg_best)))
  View(cbind("Init"=agreg_init, "Best"=agreg_best,"Way to go"=bvec/agreg_init))

  # (uvec) CONTRAINTE D INEGALITE 
  uvec <- -1 * pond_init
  
  A<-cbind(t(Amat),diag(n))
  b<-rbind(bvec,-pond_init)

    
  #-------------------------------------------------------------------------------
  #             Solveur
  #-------------------------------------------------------------------------------
  print("Repondération : Work in Progress")
  print(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S"))
  # Start the clock!
  ptm <- proc.time()
  #For memory issues
  memory.limit(size = 32000)
  sol<-solve.QP(Vmat,dvec,A,b,meq=length(agreg_best))

  # Stop the clock
  print(proc.time() - ptm)
  
  sol$iterations
  head(sol$solution)
  
  print(paste("Repondation : DONE (",(proc.time() - ptm)[3]/60," min)",sep=""))
  diff_pond<-sol$solution
  save(diff_pond,file=MatisseFiles$diff_pond_rd)
  

  #-------------------------------------------------------------------------------
  #             Output
  #-------------------------------------------------------------------------------
  # PONDERATIONS FINALES
  pond_final <- pond_init + sol$solution
  print(pond_final[1:5])
  
  # ponderations initiales
  print(pond_init[1:5])
  
  # ecart de variation nominal
  print(sol$solution[1:5])
  
  # sauvegarde
  save(pond_final, file = MatisseFiles$pond_final_rd)

  # save(pond_init,file=
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_init_erreur2.RData",sep=""))
  # save(pond_final, file =
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_final_erreur2.RData",sep=""))
  # save(pond_final, file =
  #        paste("2025/Iteration_",Iter,"/Output/erreur2/pond_final_erreur2.RData",sep=""))
  # save(agreg_init, file =
  #        paste("2025/Iteration_",Iter,"/Input/erreur2/agreg_init_erreur2.RData",sep=""))
  
  # AGREGATS FINAUX
  agreg_final <- (Amat %*% pond_final)
  print("Init")
  head(agreg_init)
  print("Best")
  head(agreg_best)
  print("Final")
  head(agreg_final)
  
  # DENSITE des modifications de PONDERATIONS
  diff<-(pond_final-pond_init)/pond_init
  # max(diff)
  # mean(diff)*100
  # diff<-as.data.frame(diff)
  # library(ggplot2)
  # g<-ggplot(diff,aes(pondmen))+geom_density()+scale_x_continuous(limits = c(-2.5, 2.5))
  # # g
  # g<-ggplot(diff,aes(pondmen))+geom_density()+scale_x_continuous(limits = c(-1, 1))
  # # g
  
  pond<-data.frame(menage_echelle%>%select(ident_men),"init"=pond_init,"final"=pond_final,"diff"=diff)
  colnames(pond)<-c("ident_men","init","final","diff")
  
  
  menage_echelle <- 
    menage_echelle %>%
    mutate(pondmen = pond_final)
  
  # sauvegarde
  save(agreg_final, file = MatisseFiles$agreg_final_rd)
  save(menage_echelle,file = MatisseFiles$menage_echelle_final_rd)
  write.csv2(t(agreg_final),file = MatisseFiles$agreg_final_csv               )
  
  
  print(
    paste("Repondération Itération n°",Iter," : SUCCESS",sep="")
  )
  
# }

  

# Export pond_final pour optimisations suivantes --------------------------

  if(!file.exists(MatisseFiles$pond_final_heurist_scen_horiz_rd)){
    
    # Save pond
    pond_final_heuristique<-pond_final
    save(pond_final_heuristique, file = MatisseFiles$pond_final_heurist_scen_horiz_rd)

    # Write wich scenario it comes from
    sink(MatisseFiles$pond_final_heurist_scen_horiz_txt)
    cat(paste("Scenario : ",paste(scenario,horizon,scenario_classement, redistribution,sep=" - "),sep=""))
    cat("\n")
    cat(as.character(strptime(Sys.time(),format="%Y-%m-%d %H:%M:%S")))
    sink()
  }

  

# Clean -------------------------------------------------------------------
  suppressWarnings(rm(A,agreg_best,agreg_final,agreg_init,Amat,b,bvec,data,diff,dvec,ener_mix,evol_energie,export,FC, menage_contraintes,
                      menage_echelle,menage_forme,menage_iteration,ones,pond, pond_final, pond_final_heuristique, pond_init, savings_rate, share,
                      sol,uvec, Vmat))
  gc()
  
  
  
  
  # Export parts budgétaires + taux_épargne
 #  
 # share_2010<-compute_share(menage_forme_2010)
 # share_2025<-compute_share(menage_echelle_2025)
 # Parts=as.data.frame(rbind(share_2010,share_2025))
 # colnames(Parts)=col
 # rownames(Parts)<-c(2010,2025)
 # View(Parts)
 #  
 #  #taux épargne
 #  epargne_2010<-sum(menage_forme_2010$taux_epargne*menage_forme_2010$pondmen*menage_forme_2010$RDB /
 #                      sum(menage_forme_2010$RDB*menage_forme_2010$pondmen),
 #                      na.rm = T)
 #  epargne_2025<-sum(menage_echelle_2025$taux_epargne*menage_echelle_2025$pondmen*menage_echelle_2025$RDB /
 #                    sum(menage_echelle_2025$RDB*menage_echelle_2025$pondmen),
 #                    na.rm = T)
 #  # epargne_mise_échelle<-sum(A2$taux_epargne*A2$pondmen*A2$RDB /
 #  #                             sum(A2$RDB*A2$pondmen),
 #  #                           na.rm = T)
 #  epargne_2010
 #  # epargne_mise_échelle
 #  epargne_2025
