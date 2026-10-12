

library(tidyverse)
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_hentemarkedspriser_ENTSO-E.R")
#her bør man bruke 

# Henter ut strømprisene i Norge fra 2020 til 2026

# Prisomrader vi vil hente
prissoner <- c(
  "Norway NO1",
  "Norway NO2",
  "Norway NO3",
  "Norway NO4",
  "Norway NO5",
  "Netherlands NL",
  "Denmark DK1",
  "Denmark DK2",
  "Germany DE"
)


strompriser  <- tibble()


for (sone in prissoner){
  
  priser_sone <- hent_markedspriser(
    start_dato = "2026-10-11",
    slutt_dato = "2026-10-12",
    prisomrade = sone,
    variabel = "A44",
    api_key = api_key
  )|>
    dplyr::select(datetime, price)|>
    dplyr::rename(!!paste0("pris_",stringr::word(sone, -1)) := price)

  if (nrow(priser_sone) == 0) {
    warning("Ingen prisdata returnert for: ", sone)
    next
  }
  if (nrow(strompriser) == 0) { #Dersom tibbelen er tom, opprettes den med utgangpunkt i den første oversikten over priser-sone
    strompriser <- priser_sone
  } else {
    strompriser  <- strompriser  |>
      dplyr::full_join(
        priser_sone,
        by = "datetime"
      )
  }}

#'Vi har noen celler som mangler verdier for pris. 
#'For at dette ikke skal bli tomme celler, antar vi at prisen på det gitte tidspunktet, 
#'i det gitte intervallet er gjennomsnittet av de to foregående prisene. 
#'
#'Derfor lager vi en funksjon som beregner dette gjennomsnittet for de tomme cellene
#'
#'Funksjonen fungerer bare på long-formater
strompriser


#Vi lagrer to tibbler - en i wide og en i long format, da de har ulike bruksområder. 

strompris_wide <- strompriser |>
  arrange(datetime)

strompris_long <- strompriser  |>
  pivot_longer(cols = -datetime,
               names_prefix = "pris_",
               names_to = "prisomrade",
               values_to = "pris") |>
  arrange(datetime)

View(strompriser)
### Kontroll av datasettet

glimpse(strompris_wide) #skjekker at de ser fornuftige ut, og at transformasjonen har gått bra
glimpse(strompris_long)

#Ser hvilke prisområder som er med
strompris_long |>
  count(prisomrade, name = "antall_observasjoner")

## Kontrollerer mangelende priser


strompris_long |>
  group_by(prisomrade) |>
  summarise(
    antall = n(),
    manglende_priser = sum(is.na(pris)),
    andel_manglende = mean(is.na(pris)),
    .groups = "drop"
  )


#lagrer  datafilene som en RDS-fil på datamaskinen


saveRDS(
  strompris_wide,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_wide.rds")

saveRDS(
  strompris_long,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_long.rds")


