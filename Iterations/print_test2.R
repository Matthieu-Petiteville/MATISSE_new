
args<-commandArgs(trailingOnly=T)
scenario=args[1]
horizon=args[2]
iter=args[3]
scenario_classement=args[4]
redistribution=args[5]
Iter<-as.numeric(iter)
Iter_last=ifelse(Iter==0,0,Iter-1)
horizon=as.numeric(horizon)


sink("D:/CIRED/Projet_Ademe/IMACLIM/TEST8.txt")
cat("HELLOOOOOOO")
cat("\n")
cat(paste(scenario,horizon, scenario_classement,redistribution,sep=" & "))
sink()