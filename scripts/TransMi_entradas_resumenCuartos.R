#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Viajes Transmilenio
#> Author: Hernando Diaz
#> 2020
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Setup 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(dplyr)
library(tidyverse)
library(mgcv)
library(MASS)
library(lubridate)
library(rstudioapi) #> load it
#> the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
#> The next line set the working directory to the relevant one:
setwd(dirname(current_path ))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Read data-------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Llegadas
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
TM_data_entradas = read_csv2('data/validacionTroncal_validacionTroncal20200206.csv') %>%
  #>TM_data_entradas = read_csv2('data/validacionTroncal_validacionTroncal20200206.csv',
  #>                               n_max = 10000) %>%
  dplyr::select(c(hora = horatransaccion,
                  nombreestacion)) %>%
  mutate(h = hour(hora),
         periodo = h - 4) %>%
  filter(h > 4 & h < 23) 

TM_data_entradas%>%count(h)

TM_entradas_horas <- TM_data_entradas %>%
  group_by(periodo,nombreestacion) %>%
  summarise(entradas = n()) %>%
  ungroup()%>%
  mutate(no_estacion = as.integer(str_sub(nombreestacion, start = 2, end = 6)),
         estacion = str_sub(nombreestacion, start = 9, end = 20)) %>%
  dplyr::select(-nombreestacion)
TM_total_horas <- TM_entradas_horas %>%
  group_by(periodo) %>%
  summarize(Total_p = sum(entradas)) %>%
  ungroup()
TM_prop_entr <- TM_entradas_horas %>%
  filter(periodo < 19) %>%
  left_join(TM_total_horas, by = "periodo") %>%
  mutate(fraction_ent = entradas/Total_p) %>%
  dplyr::select(-c(entradas,Total_p)) %>%
  arrange(no_estacion)
write_rds(TM_prop_entr,"data/TM_prop_entr.rds")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Salidas
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
dia <- "Dia_6"
mes <- "Febrero"
TM_salidas = read_delim('data/consolidado-de-salidas-sistema-troncal-por-franja-horaria-enero-abril-2020.csv',
                        delim=";", locale = locale(encoding = 'ISO-8859-1')) %>%
  rename(Acceso = `Acceso de Estacion`,
         Intervalo = INTERVALO) %>%
  filter(Intervalo >= hms("05:45:00")) %>%
  mutate(no_estacion = as.integer(str_sub(Estacion, start = 2, end = 6)),
         estacion = str_sub(Estacion, start = 9, end = 20))
TM_salidas1 <- TM_salidas %>%
  dplyr::select(-c(`Total general`, `X38`,`,,,`)) %>%
  pivot_longer(-c(Zona,Estacion, estacion,Acceso,Intervalo, Mes, no_estacion), names_to = "Dia",
               values_to = "Salidas") 
TM_salidas_horas <- TM_salidas1 %>%
  filter(Dia == dia & Mes == mes) %>%
  mutate(h = hour(Intervalo),
         m = minute(Intervalo),
         cuarto = ((h-5)*60+m) %/% 15,
         periodo = (cuarto - 3)%/%4 + 1) %>%
  group_by(periodo,Estacion) %>%
  summarise(salidas = sum(Salidas)) %>%
  ungroup()%>%
  mutate(no_estacion = as.integer(str_sub(Estacion, start = 2, end = 6)),
         estacion = str_sub(Estacion, start = 9, end = 20)) %>%
  dplyr::select(-Estacion)
TM_total_horas <- TM_salidas_horas %>%
  group_by(periodo) %>%
  summarize(Total_p = sum(salidas)) %>%
  ungroup()
TM_prop_sal <- TM_salidas_horas %>%
  filter(periodo < 19) %>%
  left_join(TM_total_horas, by = "periodo") %>%
  mutate(fraction_sal = salidas/Total_p) %>%
  dplyr::select(-c(salidas,Total_p)) %>%
  arrange(no_estacion)
write_rds(TM_prop_sal,"data/TM_prop_sal.rds")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#> Correr desde aquí para obtener matrices
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

TM_prop_sal <- read_rds("data/TM_prop_sal.rds") %>%
  drop_na(periodo) %>%
  mutate(key = paste(periodo,no_estacion,sep="_"))%>%
  select(key, fraction_sal)
TM_prop_entr <- read_rds("data/TM_prop_entr.rds") %>%
  drop_na(periodo) %>%
  mutate(key = paste(periodo,no_estacion,sep="_"))
TM_prop_E_S <- TM_prop_entr %>%
  full_join(TM_prop_sal, key = "key") %>%
  mutate(fraction_ent = replace_na(fraction_ent,0),
         fraction_sal = replace_na(fraction_sal,0)) %>%
  mutate(denom = fraction_ent * (1 - fraction_sal)) %>%
  drop_na(denom) %>%
  drop_na(periodo) %>%
  select(-key)
TM_prop_E_S %>% count(periodo)
Destinos <- array(0, dim = c(158,158,18))
for (period in 1:18) {
  h <- period + 4
  Prop_E_S <- TM_prop_E_S %>%
    filter(periodo == period) %>%
    select(-periodo) 
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
  write_rds(Orig_Dest,paste("Results/Matriz_",h,".rds",sep = ""))
}
