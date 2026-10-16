
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


# --------------------------------------------------
# 2. Norske strømpriser
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
# 3. Forbruk
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
# 4. Produksjon
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
# 5. Tilsig
# Én rad per dato og prisområde
# --------------------------------------------------

tilsig_dag <- tilsig |>
  mutate(dato = as.Date(dato)) |>
  group_by(dato, prisomrade) |>
  summarise(
    tilsig = mean(tilsig, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tilsig = if_else(is.nan(tilsig), NA_real_, tilsig)
  )

# --------------------------------------------------
# 6. Magasinfylling
# Én rad per dato og prisområde
# --------------------------------------------------

magasin_dag <- vannreservoar |>
  mutate(
    dato = as.Date(datetime)
  ) |>
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
  fill(fyllingsgrad, .direction = "down") |>
  ungroup()

# --------------------------------------------------
# 7. Europeiske priser
# Én rad per tidspunkt
# --------------------------------------------------

pris_europa <- strompris_europa_time |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime) |>
  summarise(
    times_pris = mean(pris, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    times_pris = if_else(is.nan(times_pris), NA_real_, times_pris)
  )

# --------------------------------------------------
# 8. Sett sammen tibble1
# Utgangspunktet er norske timespriser
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
  arrange(prisomrade, datetime)

# --------------------------------------------------
# 9. Kontroller resultatet
# --------------------------------------------------

glimpse(tibble1)

nrow(tibble1)

# Skal være TRUE
nrow(tibble1) == nrow(pris_norge)

# Sjekk at ingen kombinasjoner er duplisert
tibble1 |>
  count(datetime, prisomrade) |>
  filter(n > 1)

# --------------------------------------------------
# 10. Lagre
# --------------------------------------------------

saveRDS(tibble1, "Datasett/tibble1.rds")
