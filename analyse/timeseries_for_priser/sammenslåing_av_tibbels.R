# Importerer bibliotekene
library(httr)
library(jsonlite)
library(lubridate)
library(zoo)
library(rugarch)
library(plm)
library(vars)
library(tidyverse)
library(data.table)
library(dtplyr)
library(scales)


# Leser inn datasett
vannreservoar <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/vannreservoar.rds"
)

total_produksjon <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/total_produksjon.rds"
)

strompris_norge_long_fra_2020 <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_norge_long_fra_2020.rds"
)

forventing_mot_forbruk_norge_2020 <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/forventing_mot_forbruk_norge_2020.rds"
)


# Gjør strømprisen om til timesdata
strompris_norge_long_fra_2020_time <-
  strompris_norge_long_fra_2020 |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    times_pris = mean(pris_modellert),
    .by = c(datetime, prisomrade)
  ) |>
  drop_na()|>
  as_tibble()


# Gjør produksjonen om til timesdata
total_produksjon_time <-
  total_produksjon |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    timeproduksjon = mean(production_MW, na.rm = TRUE),
    .by = c(datetime, produksjonskilde, prisomrade)
  ) |>
  mutate(total_produksjon = sum(timeproduksjon, na.rm = TRUE),.by = c(datetime,prisomrade))|>
  filter(produksjonskilde == "Vannkraft med magasin") |>
  rename(vannkraft = timeproduksjon   )|>
  as_tibble()

# Gjør forbruket om til timesdata

forventing_mot_forbruk_norge_2020_time <-
  forventing_mot_forbruk_norge_2020 |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    faktisk_forbruk = mean(faktisk_forbruk_MW), prognose_forbruk = mean(prognose_forbruk_MW),
    .by = c(datetime, prisomrade)
  ) |>
  drop_na()|>
  as_tibble()

#lager en stor tibble, der jeg lagrer alle variablene i en tibbel. Dette gjør analysen senere murlig
total_tibble <- total_produksjon_time |>
  lazy_dt() |>
  left_join(
    strompris_norge_long_fra_2020_time,
    by = c("datetime", "prisomrade")
  ) |>
  left_join(
    forventing_mot_forbruk_norge_2020_time,
    by = c("datetime", "prisomrade")
  ) |>
  as_tibble()

View(total_tibble)