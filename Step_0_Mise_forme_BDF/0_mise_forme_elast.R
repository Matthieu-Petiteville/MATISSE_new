
# Mise en forme des élasticités
# Lecture depuis fichier excel, converti en Rdata format long
# Output : elasticite_demande.RData


# Libraries ---------------------------------------------------------------

suppressMessages(library(tidyverse , warn.conflicts=F , quietly = T))
suppressMessages(library(readxl, warn.conflicts=F , quietly = T))

# Data --------------------------------------------------------------------

Elast<- read_excel(MatisseFiles$elast_xl)

Elast <-
  Elast %>%
  gather(key = CODADEME_typ , value = elast,-c(1,2)) %>%
  separate(CODADEME_typ , into = c("CODADEME","typ_elast") , sep="_")

colnames(Elast) <- c("Decile" , "Typo" , "CODADEME" , "typ_elast" , "elast")

# Save --------------------------------------------------------------------

save(Elast,file=MatisseFiles$elast_rd)

# Clean ----------------------------------------------------------------

suppressWarnings(rm(Elast))
gc()