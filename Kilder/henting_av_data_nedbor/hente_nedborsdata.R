library(data.table)
library(tidyverse)
library(dtplyr)

source("Kilder/henting_av_data_nedbor/funksjon_hente_nedborsdata.R")



readRenviron("Kilder/.Renviron")

api_key <- Sys.getenv("nedbor") #henter ut API-nøkkelen

print(api_key)
if (api_key == "") {
  stop("Fant ikke ENTSOE_TOKEN. Sjekk .Renviron.")
}

#'lager en tibble med kordinatene til prissonene. Funksjonen vår regner ut de 10 nærmeste målestasjonene til
#'det gitte kordinatet, slik at vi får spredt observasjonene våre. 
#'

koordinater_for_prissoner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  longitude = c(10.75, 6.72, 10.40, 14.40, 5.32),
  latitude  = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

nedbor_alle <- koordinater_for_prissoner |>
  
  #henter ut data for alle prissonene
  mutate(
    data = pmap(
      list(longitude, latitude),
      \(lon, lat) {
        hent_nedbor(
          key = api_key,
          
          #lengdegrad og breddegrad
          longitude = lon,
          latitude = lat,
          start_date = "2020-01-01",
          end_date = Sys.Date()
        )
      }
    )
  ) |>
  #velger de relavante kolonnene
  select(prisomrade, data) |>
  
  #'API-et for værdataen er på JSON-format, som betyr at dataen kommer som nestede lister. 
  #'For at vi skal jobbe med det, må det unnestes til en tibble
  unnest(data)



nedbor_behandlet <- nedbor_alle |>
  lazy_dt() |>
  rename(datetime = dato)|>
  summarise(nedbor = sum(nedbor_mm, na.rm = TRUE), .by = c(dato, prisomrade))|>
  as_tibble()

saveRDS(
  nedbor_behandlet,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/nedbor_total.rds"
  
)