

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

strompris_norge_long_fra_2020 <- readRDS(
  "Datasett/strompris_norge_long_time.rds"
)

forventing_mot_forbruk_norge_2020 <- readRDS(
  "Datasett/forventing_mot_forbruk_norge.rds"
)

tilsig <- readRDS(
  "Datasett/tilsig_total.rds")
View(tilsig)

strompris_europa_time <- readRDS(
  "Datasett/strompris_long_europa_time.rds")
strompris_europa_time

total_tibble <- total_produksjon |>
  
  #Velger relevante kolonner for analysen
  dplyr::select(prisomrade, datetime, produksjonskilde,production_MW)|>
  
  #Legger til forventet forbruk
  left_join(forventing_mot_forbruk_norge_2020|>
              rename(forbruk = faktisk_forbruk_MW), by = join_by(datetime, prisomrade))|>
  fill(forbruk, .by = prisomrade, .direction = "down")|>
  
  #legger til nedbør
  left_join(nedbor_total, by = join_by(datetime == dato, prisomrade)) |>
  fill(nedbor, .by = prisomrade, .direction = "down")|>
  
  #legger til strømpris
  left_join(strompris_norge_long_fra_2020_time, by = join_by(datetime, prisomrade))|>
  
  #da vannreservoarene er på ukesintervaller, må vi endre på formatet til datoene. 
  #Deretter joines tibbelsene
  mutate(datetime = as.Date(datetime)) |>
  left_join(vannreservoar|>
              dplyr::select(datetime, prisomrade, fyllingsgrad), by = join_by(datetime, prisomrade))|>
  fill(fyllingsgrad, .by = prisomrade, .direction = "down")|>
  drop_na()|>
  rename(produksjon = production_MW)
total_tibble
