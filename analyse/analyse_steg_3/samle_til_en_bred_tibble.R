
library(tidyverse)
library(lubridate)
library(dtplyr)

# --------------------------------------------------
# 1. Les inn data
# --------------------------------------------------

vannreservoar <- readRDS("Datasett/vannreservoar.rds")
total_produksjon <- readRDS("Datasett/total_produksjon.rds")
strompris_norge_time <- readRDS("Datasett/strompris_norge_long_time.rds")
forventing_mot_forbruk <- readRDS("Datasett/forventing_mot_forbruk_norge_2020.rds")
tilsig <- readRDS("Datasett/tilsig_total_denviskalbruke.rds")
strompris_europa_time <- readRDS("Datasett/strompris_long_europa_time.rds")
strompris_gb <- readRDS("Datasett/strompris_gb_time.rds")


# --------------------------------------------------
# 2. Norske strømpriser i long-format
# Én rad per time og prisområde
# --------------------------------------------------

pris_norge <- strompris_norge_time |>
  lazy_dt() |>
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    dato = as.Date(datetime)
  ) |>
  group_by(datetime, prisomrade, dato) |>
  summarise(
    pris_norge = mean(times_pris, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pris_norge = if_else(
      is.nan(pris_norge),
      NA_real_,
      pris_norge
    )
  ) |>
  as_tibble()


# --------------------------------------------------
# 3. Norske strømpriser i wide-format
# Én rad per tidspunkt, én kolonne per prisområde
# --------------------------------------------------

pris_norge_wide <- pris_norge |>
  dplyr::select(datetime, prisomrade, pris_norge) |>
  pivot_wider(
    names_from = prisomrade,
    values_from = pris_norge,
    names_prefix = "pris_"
  )


# --------------------------------------------------
# 4. Forbruk
# --------------------------------------------------

forbruk_time <- forventing_mot_forbruk |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime, prisomrade) |>
  summarise(
    forbruk = mean(faktisk_forbruk_MW, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    forbruk = if_else(is.nan(forbruk), NA_real_, forbruk)
  )


# --------------------------------------------------
# 5. Produksjon
# Beholder de tre viktigste produksjonskildene
# --------------------------------------------------

produksjon_time <- total_produksjon |>
  mutate(
    produksjonskilde = replace_na(produksjonskilde, "Annet"),
    produksjonskilde = if_else(
      produksjonskilde == "Vannkraft med magasin",
      "Magasinkraftverk",
      produksjonskilde
    ),
    produksjonskilde = if_else(
      produksjonskilde %in% c(
        "Magasinkraftverk",
        "Elvekraft",
        "Vindkraft pa land"
      ),
      produksjonskilde,
      "Annet"
    ),
    datetime = as.POSIXct(datetime, tz = "UTC")
  ) |>
  group_by(datetime, prisomrade, produksjonskilde) |>
  summarise(
    produksjon = mean(production_MW, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    produksjon = if_else(is.nan(produksjon), NA_real_, produksjon)
  ) |>
  pivot_wider(
    names_from = produksjonskilde,
    values_from = produksjon,
    names_prefix = "produksjon_"
  )


# --------------------------------------------------
# 6. Tilsig
# Én rad per dato og prisområde
# --------------------------------------------------

tilsig_dag <- tilsig |>
  mutate(dato = as.Date(dato)) |>
  group_by(dato, prisomrade) |>
  summarise(
    tilsig = mean(vannforing, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tilsig = if_else(is.nan(tilsig), NA_real_, tilsig)
  )


# --------------------------------------------------
# 7. Magasinfylling
# Én rad per dato og prisområde
# --------------------------------------------------

magasin_dag <- vannreservoar |>
  mutate(dato = as.Date(datetime)) |>
  group_by(dato, prisomrade) |>
  summarise(
    fyllingsgrad = mean(fyllingsgrad, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    fyllingsgrad = if_else(
      is.nan(fyllingsgrad),
      NA_real_,
      fyllingsgrad
    )
  ) |>
  arrange(prisomrade, dato) |>
  group_by(prisomrade) |>
  tidyr::complete(
    dato = seq.Date(min(dato), max(dato), by = "day")
  ) |>
  tidyr::fill(fyllingsgrad, .direction = "down") |>
  ungroup()


# --------------------------------------------------
# 8. Felles kontinental strømpris
# Én gjennomsnittspris per tidspunkt
# --------------------------------------------------

pris_europa <- strompris_europa_time |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime) |>
  summarise(
    europeisk_pris = mean(pris, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    europeisk_pris = if_else(
      is.nan(europeisk_pris),
      NA_real_,
      europeisk_pris
    )
  )


# --------------------------------------------------
# 9. Britisk strømpris
# Én gjennomsnittspris per tidspunkt
# --------------------------------------------------

pris_gb <- strompris_gb |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime) |>
  summarise(
    britisk_pris = mean(pris, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    britisk_pris = if_else(
      is.nan(britisk_pris),
      NA_real_,
      britisk_pris
    )
  )


# --------------------------------------------------
# 10. Sett sammen hoveddatasettet
# Beholder long-format for analyse og wide-priser
# --------------------------------------------------

tibble1 <- pris_norge |>
  left_join(
    forbruk_time,
    by = c("datetime", "prisomrade"),
    relationship = "one-to-one"
  ) |>
  left_join(
    produksjon_time,
    by = c("datetime", "prisomrade"),
    relationship = "one-to-one"
  ) |>
  left_join(
    tilsig_dag,
    by = c("dato", "prisomrade"),
    relationship = "many-to-one"
  ) |>
  left_join(
    magasin_dag,
    by = c("dato", "prisomrade"),
    relationship = "many-to-one"
  ) |>
  left_join(
    pris_europa,
    by = "datetime",
    relationship = "many-to-one"
  ) |>
  left_join(
    pris_gb,
    by = "datetime",
    relationship = "many-to-one"
  ) |>
  left_join(
    pris_norge_wide,
    by = "datetime",
    relationship = "many-to-one"
  ) |>
  arrange(prisomrade, datetime)


# --------------------------------------------------
# 11. Lag analysetibler i long-format
# --------------------------------------------------

# NO1 og NO2: bruk felles kontinental pris

analyse_NO1_NO2 <- tibble1 |>
  filter(prisomrade %in% c("NO1", "NO2")) |>
  dplyr::select(
    -starts_with("pris_NO"),
    -britisk_pris
  ) |>
  arrange(prisomrade, datetime)


# NO3: bruk britisk pris

analyse_NO3 <- tibble1 |>
  filter(prisomrade == "NO3") |>
  dplyr::select(
    -starts_with("pris_NO"),
    -europeisk_pris
  ) |>
  arrange(datetime)


# NO4 og NO5: ingen utenlandsk prisvariabel

analyse_NO4_NO5 <- tibble1 |>
  filter(prisomrade %in% c("NO4", "NO5")) |>
  dplyr::select(
    -starts_with("pris_NO"),
    -europeisk_pris,
    -britisk_pris
  ) |>
  arrange(prisomrade, datetime)


# --------------------------------------------------
# 12. Kontroller datasett
# --------------------------------------------------

glimpse(pris_norge_wide)
glimpse(tibble1)

glimpse(analyse_NO1_NO2)
glimpse(analyse_NO3)
glimpse(analyse_NO4_NO5)

# Sjekk antall rader

nrow(tibble1) == nrow(pris_norge)

nrow(analyse_NO1_NO2)
nrow(analyse_NO3)
nrow(analyse_NO4_NO5)

# Sjekk duplikater i hoveddatasettet

tibble1 |>
  count(datetime, prisomrade) |>
  filter(n > 1)

# Sjekk hvilke kolonner som finnes i wide-tabellen

colnames(pris_norge_wide)


# --------------------------------------------------
# 13. Lagre datasett
# --------------------------------------------------

saveRDS(pris_norge_wide, "Datasett/pris_norge_wide.rds")
saveRDS(tibble1, "Datasett/tibble7.rds")

saveRDS(analyse_NO1_NO2, "Datasett/analyse_NO1_NO2.rds")
saveRDS(analyse_NO3, "Datasett/analyse_NO3.rds")
saveRDS(analyse_NO4_NO5, "Datasett/analyse_NO4_NO5.rds")

