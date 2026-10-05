


library(tidyverse)

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

nedbor_total <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/nedbor_total.rds"
)
strompris_norge_long_fra_2020_time <- strompris_norge_long_fra_2020|>
  mutate(datetime = floor_date(datetime, "hour"))|>
  summarise(pris = mean(pris_modellert, na.rm = TRUE), .by = c(datetime,prisomrade))

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

