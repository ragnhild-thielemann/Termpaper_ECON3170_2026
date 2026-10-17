library(tidyverse)
library(dtplyr)

source("Kilder/henting av data fra ENTSO-E/funksjon_hentemarkedspriser_ENTSO-E.R")

# Henter strømpriser for Great Britain (GB)
strompriser_gb <- hent_markedspriser(
  start_dato = "2020-01-01",
  slutt_dato = Sys.Date(),
  prisomrade = "Great_Britain GB",
  variabel = "A44",
  api_key = api_key
) |>
  dplyr::select(datetime, price) |>
  dplyr::rename(pris_gb = price) |>
  dplyr::arrange(datetime)

# Kontroller at data ble hentet
if (nrow(strompriser_gb) == 0) {
  stop("Ingen prisdata returnert for Great Britain.")
}

# Kontroller manglende verdier
strompriser_gb |>
  summarise(
    antall_observasjoner = n(),
    manglende_priser = sum(is.na(pris_gb)),
    andel_manglende = mean(is.na(pris_gb))
  )

# Aggregerer prisene til timesnivå
strompris_gb_time <- strompriser_gb |>
  lazy_dt() |>
  mutate(datetime = floor_date(datetime, "hour")) |>
  summarise(
    pris = mean(pris_gb, na.rm = TRUE),
    .by = datetime
  ) |>
  as_tibble() |>
  arrange(datetime)

# Kontroller datasettet
glimpse(strompris_gb_time)
summary(strompris_gb_time$pris)

# Lagrer datasettet
saveRDS(
  strompris_gb_time,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_gb_time.rds"
)

summary(strompriser_gb)