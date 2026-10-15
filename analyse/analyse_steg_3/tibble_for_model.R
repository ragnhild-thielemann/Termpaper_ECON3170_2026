
library(dtplyr)
library(data.table)
library(tidyverse)

# Leser inn datasett
vannreservoar <- readRDS(
  "Datasett/vannreservoar.rds"
)

total_produksjon <- readRDS(
  "Datasett/total_produksjon.rds"
)
strompris_norge_time <- readRDS(
  "Datasett/strompris_norge_long_time.rds"
) |>
  mutate(pris_norge = times_pris)

forventing_mot_forbruk <- readRDS(
  "Datasett/forventing_mot_forbruk_norge.rds"
)

tilsig <- readRDS(
  "Datasett/tilsig_total_denviskalbruke.rds"
)

# Aggregerer europeiske strømpriser til dagsnivå
strompris_europa_time <- readRDS(
  "Datasett/strompris_long_europa_time.rds"
) |>
  mutate(datetime = as.Date(datetime)) |>
  summarise(
    pris_europa = mean(pris, na.rm = TRUE),
    .by = datetime
  )


nrow(total_produksjon)
nrow(forventing_mot_forbruk)
nrow(tilsig)
nrow(strompris_norge_time)
nrow(vannreservoar)
nrow(strompris_europa_time)

# Hvor mye minne bruker datasettene?
format(object.size(total_produksjon), units = "MB")
format(object.size(tilsig), units = "MB")
format(object.size(strompris_norge_time), units = "MB")
# Kobler sammen datasettene
total_tibble <- total_produksjon |>
  dplyr::select(
    prisomrade,
    datetime,
    produksjonskilde,
    production_MW
  ) |>
  
  left_join(
    forventing_mot_forbruk |>
      rename(forbruk = faktisk_forbruk_MW),
    by = join_by(datetime, prisomrade)
  ) |>
  
  left_join(
    tilsig,
    by = join_by(datetime == dato, prisomrade)
  ) |>
  
  # Konverterer til dagsnivå før daglige data kobles på
  mutate(datetime = as.Date(datetime)) |>
  
  left_join(
    strompris_norge_time |>
      mutate(datetime = as.Date(datetime)),
    by = join_by(datetime, prisomrade)
  ) |>
  
  left_join(
    vannreservoar |>
      mutate(datetime = as.Date(datetime)) |>
      dplyr::select(datetime, prisomrade, fyllingsgrad),
    by = join_by(datetime, prisomrade)
  ) |>
  
  left_join(
    strompris_europa_time,
    by = join_by(datetime)
  ) |>
  
  # Sorterer kronologisk før utfylling
  arrange(prisomrade, datetime) |>
  


  rename(produksjon = production_MW) |>
  as_tibble()

View(total_tibble)
summary(total_produksjon$datetime)
