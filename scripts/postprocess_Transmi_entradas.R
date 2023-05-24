##========================================#
## Setup ------------
##========================================#
library(dplyr)
library(tidyverse)
library(mgcv)
library(MASS)
library(lubridate)

##========================================#
## Read processed data ------------
##========================================#
TM_prop_sal <- read_rds("../data/processed_data/transportdata/TM_prop_sal.rds") %>%
    drop_na(periodo) %>%
    mutate(key = paste(periodo,no_estacion,sep="_"))%>%
    dplyr::select(key, fraction_sal)
TM_prop_entr <- read_rds("../data/processed_data/transportdata/TM_prop_entr.rds") %>%
    drop_na(periodo) %>%
    mutate(key = paste(periodo,no_estacion,sep="_"))

TM_prop_E_S <- TM_prop_entr %>%
    full_join(TM_prop_sal, key = "key") %>%
    mutate(fraction_ent = replace_na(fraction_ent,0),
           fraction_sal = replace_na(fraction_sal,0)) %>%
    mutate(denom = fraction_ent * (1 - fraction_sal)) %>%
    drop_na(denom) %>%
    drop_na(periodo) %>%
    dplyr::select(-key)

TM_prop_E_S %>% count(periodo)
Destinos <- array(0, dim = c(158,158,18))

for (period in 1:18) {
  h <- period + 4
  Prop_E_S <- TM_prop_E_S %>%
      filter(periodo == period) %>%
      dplyr::select(-periodo) 
  No_estaciones <- Prop_E_S$no_estacion
  Matriz_llegadas <-  data.matrix(Prop_E_S[,3])
  Matriz_salidas <-  data.matrix(Prop_E_S[,4])
  Denom <- data.matrix(Prop_E_S[,5])
  Orig_Dest <- Matriz_salidas %*% t(Matriz_llegadas)
  nfila = nrow(Orig_Dest)
  for (i in 1:nfila) {
      Orig_Dest[i,] <- Orig_Dest[i,]/Denom[i]
      Orig_Dest[i,i] <- 0
  }
  rownames(Orig_Dest) <- No_estaciones
  colnames(Orig_Dest) <- No_estaciones
  write_rds(Orig_Dest,paste("../data/processed_data/transportdata/results/Matriz_",h,".rds",sep = ""))
}
