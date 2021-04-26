
GenerateStandardReport <- function(){

library(tidyverse)
detach("package:plyr")
library(readxl)
library(gridExtra)
	

#Table distrib DPE 2010 Matisse
load(MatisseFiles$menage_forme_rd)
source(paste(M_home,"/Step_0_Mise_forme_BDF/3_bonus_energies_kwh.R",sep=""))
menage_forme_ener <- energie_dom_surf(menage_forme)
menage_forme_ener$DPE_pred <- menage_forme$DPE_pred
menage_forme_ener$quintileuc <- menage_forme$quintileuc
sum_conso_2010_Matisse <- menage_forme_ener %>%
            group_by(DPE_pred) %>%
            summarise(conso_tot = sum(energie_tot_surf * surfhab_d * pondmen),surf_tot = sum(surfhab_d* pondmen)) %>%
            mutate(ener_tot_surf = conso_tot/surf_tot*1000)

#Table distrib DPE 2010 3ME
load(MatisseFiles$Threeme_rd)
ThreeME <- ThreeME %>% filter(ThreeME$year == 2010)
l_idx <- grep("^ENER_BUIL_H01_C._2.11630/BUIL_H01_C._2$",ThreeME$Var)
ener_tot_surf <- ThreeME[l_idx,"value"]/10000
l_idx <- grep("^BUIL_H01_C._2$",ThreeME$Var)
surf_tot <- ThreeME[l_idx,"value"]
sum_conso_2010_3ME <- as.tibble(data.frame(DPE_pred = LETTERS[1:7],
									conso_tot = rep(0,7), 
									surf_tot = surf_tot$value, 
									ener_tot_surf = ener_tot_surf$value))
sum_conso_2010_3ME$conso_tot = sum_conso_2010_3ME$surf_tot * sum_conso_2010_3ME$ener_tot_surf

pdf(MatisseFiles$output_pdf)
grid.table(sum_conso_2010_Matisse)
grid.table(sum_conso_2010_3ME)
dev.off()


}