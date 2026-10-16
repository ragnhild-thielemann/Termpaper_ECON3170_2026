
library(dtplyr)
library(data.table)
library(tidyverse)

# Leser inn datasett
vannreservoar <- readRDS( #ukesoppløst
  "Datasett/vannreservoar.rds"
)

total_produksjon <- readRDS( #dagsoppløst
  "Datasett/total_produksjon.rds"
)

strompris_norge_time <- readRDS( #timesoppløst
  "Datasett/strompris_norge_long_time.rds"
) |>
  mutate(pris_norge = times_pris)

forventing_mot_forbruk <- readRDS(#timesoppløst
  "Datasett/forventing_mot_forbruk_norge_2020.rds"
)
forventing_mot_forbruk
tilsig <- readRDS( #dagsoppløst
  "Datasett/tilsig_total_denviskalbruke.rds"
)

# Aggregerer europeiske strømpriser til dagsnivå
strompris_europa_time <- readRDS(#timesoppløst
  "Datasett/strompris_long_europa_time.rds"
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
  left_join(strom_norge_)
  
  # Konverterer til dagsnivå før daglige data kobles på
  mutate(datetime = as.Date(datetime))
    
View(total_tibble)
summary(total_produksjon$datetime)
