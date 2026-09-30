library(tidyverse)
library(dtplyr)
library(data.table)
library(docstring)

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_hentemarkedspriser_ENTSO-E.R")


# Henter ut strømprisene i Norge fra 2020 til 2026

prissoner <- c("NO1", "NO2", "NO3", "NO4", "NO5")

strompris_norge <- tibble()


for (sone in prissoner) {
  
  priser_sone <- hent_markedspriser(
    start_dato = "2020-01-01",
    slutt_dato = Sys.Date(),
    prisomrade = paste("Norway", sone),
    variabel = "A44",
    api_key = api_key
  ) |>
    dplyr::select(datetime, price) |>
    dplyr::rename(!!paste0("pris_", sone) := price)
  
  if (nrow(strompris_norge) == 0) {
    
    strompris_norge <- priser_sone
    
  } else {
    
    strompris_norge <- strompris_norge |>
      left_join(
        priser_sone,
        by = "datetime"
      )
  }
}




lag_pris_estimat <- function(pris_tibble) {
  #' Funksjon for å estimere manglende priser
  #'Estimatet beregnes som gjennomsnittet av de to foregående
  #' prisobservasjonene i samme prisområde.
  pris_tibble |>
    lazy_dt() |>
    arrange(prisomrade, datetime) |>
    group_by(prisomrade) |>
    mutate(
      pris_modellert = if_else(
        is.na(pris),
        (lag(pris, 1) + lag(pris, 2)) / 2,
        pris
      )
    ) |>
    ungroup() |>
    as_tibble()
}


# Wide-format
# Beholdes fordi dette formatet er praktisk for enkelte analyser og plott.

strompris_norge_wide <- strompris_norge |>
  lazy_dt() |>
  arrange(datetime) |>
  as_tibble()


# Long-format

strompris_norge_long <- strompris_norge |>
  #' lazy_dt() gjør at dtplyr oversetter operasjonene til data.table-kode.
  #' dette gjør behandlingen raskere
  lazy_dt() |>
  pivot_longer(
    cols = -datetime,
    names_prefix = "pris_",
    names_to = "prisomrade",
    values_to = "pris"
  ) |>
  arrange(datetime) |>
  as_tibble()


# Beregner estimerte priser for manglende observasjoner

strompris_norge_long <- lag_pris_estimat(strompris_norge_long)


# Lagrer dataene som RDS-filer

saveRDS(
  strompris_norge_wide,
  "Datasett/strompris_norge_wide_fra_2020.rds"
)

saveRDS(
  strompris_norge_long,
  "Datasett/strompris_norge_long_fra_2020.rds"
)

