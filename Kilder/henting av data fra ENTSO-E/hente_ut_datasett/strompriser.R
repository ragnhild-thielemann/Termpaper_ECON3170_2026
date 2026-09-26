

library(tidyverse)
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_hentemarkedspriser_ENTSO-E.R")


# Henter ut strømprisene i Norge fra 2020 til 2026

prissoner <- c("NO1","NO2","NO3","NO4","NO5")
strompris_norge <- tibble()


for (sone in prissoner){
  
  priser_sone <- hent_markedspriser(
    start_dato = "2026-09-01",
    slutt_dato = Sys.Date(),
    prisomrade= paste("Norway",sone),
    variabel = "A44",
    api_key = api_key
  )|>
    dplyr::select(datetime, price)|>
    rename(!!paste0("pris_", sone) := price)
  
  View(priser_sone)
  if (nrow(strompris_norge) == 0) { #Dersom tibbelen er tom, opprettes den med utgangpunkt i den første oversikten over priser-sone
    strompris_norge <- priser_sone
  } else {
    strompris_norge <- strompris_norge |>
      left_join(
        priser_sone,
        by = "datetime"
      )
  }}

#Vi lagrer to tibbler - en i wide og en i long format, da de har ulike bruksområder. 

strompris_norge_wide <- strompris_norge|>
  arrange(datetime)

strompris_norge_long <- strompris_norge |>
  pivot_longer(cols = -datetime,
               names_prefix = "pris_",
               names_to = "prisomrade",
               values_to = "pris") |>
  arrange(datetime)
#lagrer hele datafilen som en RDS-fil på datamaskinen
saveRDS(
  strompris_norge_wide,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_norge_wide.rds")

saveRDS(
  strompris_norge_long,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_norge_long.rds")


