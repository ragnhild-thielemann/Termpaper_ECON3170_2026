
library(tidyverse)
library(tibble)
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_produksjonstype.R")


#Oversikt mellom koden i API-et og kraftproduksjonen
psr_oversikt <- tibble::tibble(kode = c("B04","B06","B11","B12",
                                       "B15","B16","B17","B18"),
                               navn = c("Gass","Olje","Elvekraft",
                                                   "Vannkraft med magasin", "Annen fornybar",
                                                   "Solkraft","Vindkraft til havs","Vindkraft pa land"))


                               
                               
prisomrader <- c("NO1","NO2","NO3","NO4","NO5")
total_produksjon = tibble()

for (sone in prisomrader){
  for (i in 1:nrow(psr_oversikt)){
    produksjon <- hent_produksjon_ENTSOE(
      start_dato = "2026-09-20",
      slutt_dato = Sys.Date(),
      prisomrade = paste("Norway",sone),
      api_key = api_key,
      psr_type = psr_oversikt$kode[i]) |>
      mutate(produksjonstype = psr_oversikt$navn[i])
    
  if (nrow(total_produksjon) == 0){
    total_produksjon <- produksjon
  } else{
    total_produksjon <- total_produksjon |>
      bind_rows(produksjon)
  }
}}


nrow(total_produksjon)
saveRDS(
  total_produksjon,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/total_produksjon.rds")

