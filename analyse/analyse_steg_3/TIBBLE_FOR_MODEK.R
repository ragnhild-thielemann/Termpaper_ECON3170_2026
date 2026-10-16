
library(tidyverse)
library(lubridate)

# --------------------------------------------------
# 1. Les inn data
# --------------------------------------------------

vannreservoar <- readRDS("Datasett/vannreservoar.rds")

total_produksjon <- readRDS("Datasett/total_produksjon.rds")

strompris_norge_time <- readRDS(
  "Datasett/strompris_norge_long_time.rds"
)

forventing_mot_forbruk <- readRDS(
  "Datasett/forventing_mot_forbruk_norge_2020.rds"
)

tilsig <- readRDS(
  "Datasett/tilsig_total_denviskalbruke.rds"
)

strompris_europa_time <- readRDS(
  "Datasett/strompris_long_europa_time.rds"
)|>
  summarise(times_pris = mean(pris, na.rm = TRUE),.by = datetime)

# --------------------------------------------------
# 2. Norske timespriser: utgangspunkt for tidsaksen
# --------------------------------------------------

pris_norge <- strompris_norge_time |>
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    dato = as.Date(datetime)
  ) |>
  rename(pris_norge = times_pris) |>
  distinct(datetime, prisomrade, .keep_all = TRUE)


# --------------------------------------------------
# 3. Forbruk: én rad per time og prisområde
# --------------------------------------------------

forbruk_time <- forventing_mot_forbruk |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime, prisomrade) |>
  summarise(
    forbruk = mean(faktisk_forbruk_MW, na.rm = TRUE),
    .groups = "drop"
  )


# --------------------------------------------------
# 4. Produksjon: dagsoppløst
# Beholder én rad per produksjonskilde
# --------------------------------------------------

produksjon_dag <- total_produksjon |>
  mutate(dato = as.Date(datetime)) |>
  group_by(dato, prisomrade, produksjonskilde) |>
  summarise(
    produksjon = mean(production_MW, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = produksjonskilde,
    values_from = produksjon,
    names_prefix = "produksjon_"
  )


# --------------------------------------------------
# 5. Tilsig: dagsoppløst
# --------------------------------------------------


# Tilsig er dagsoppløst
tilsig_dag <- tilsig |>
  mutate(dato = as.Date(dato)) |>
  group_by(dato, prisomrade) |>
  summarise(
    tilsig = mean(vannforing, na.rm = TRUE),
    .groups = "drop"
  )


# --------------------------------------------------
# 6. Magasin: ukesoppløst
# Utvider til dagsnivå og videre til timer
# --------------------------------------------------
magasin_observert <- vannreservoar |>
  mutate(dato = as.Date(datetime)) |>
  group_by(dato, prisomrade) |>
  summarise(
    fyllingsgrad = mean(fyllingsgrad, na.rm = TRUE),
    .groups = "drop"
  )

magasin_dag <- crossing(
  prisomrade = unique(pris_norge$prisomrade),
  dato = seq(
    min(pris_norge$dato, na.rm = TRUE),
    max(pris_norge$dato, na.rm = TRUE),
    by = "day"
  )
) |>
  left_join(
    magasin_observert,
    by = c("dato", "prisomrade")
  ) |>
  arrange(prisomrade, dato) |>
  group_by(prisomrade) |>
  fill(fyllingsgrad, .direction = "down") |>
  ungroup()

# --------------------------------------------------
# 7. Europeiske priser: behold timesnivå
# Lager én kolonne per europeisk prisområde
# --------------------------------------------------

pris_europa <- strompris_europa_time |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) 


# --------------------------------------------------
# 8. Koble sammen til timesformat
# --------------------------------------------------
total_tibble <- pris_norge |>
  left_join(
    forbruk_time,
    by = c("datetime", "prisomrade")
  ) |>
  left_join(
    produksjon_dag,
    by = c("dato", "prisomrade")
  ) |>
  left_join(
    tilsig_dag,
    by = c("dato", "prisomrade")
  ) |>
  left_join(
    magasin_dag,
    by = c("dato", "prisomrade")
  ) |>
  left_join(
    pris_europa,
    by = "datetime"
  ) |>
  arrange(prisomrade, datetime) |>
  as_tibble()

# --------------------------------------------------
# 9. Kontroller resultatet
# --------------------------------------------------

glimpse(total_tibble)

nrow(total_tibble)

summary(total_tibble)

# --------------------------------------------------
# 10. Lagre resultatet
# --------------------------------------------------


saveRDS(total_tibble,
        "Datasett/total_tibble.rds")