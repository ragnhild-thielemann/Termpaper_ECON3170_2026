
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
  mutate(
    dato = as.Date(dato)
  ) |>
  group_by(dato, prisomrade) |>
  summarise(
    tilsig = mean(vannforing, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    tilsig = if_else(
      is.nan(tilsig),
      NA_real_,
      tilsig
    )
  )

# --------------------------------------------------
# 6. Magasinfylling
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
  # Fyll inn ukene mellom observasjonene
  tidyr::complete(
    dato = seq.Date(min(dato), max(dato), by = "day")
  ) |>
  tidyr::fill(fyllingsgrad, .direction = "down") |>
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
    europeisk_pris = if_else(is.nan(times_pris), NA_real_, times_pris)
  )

# --------------------------------------------------
# 7. Britiske priser
# Én rad per tidspunkt
# --------------------------------------------------

pris_gb <- strompris_gb |>
  
  #konverterer til samme tidssone
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime) |>
  summarise(
    times_pris = mean(pris, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    britisk_pris = if_else(is.nan(times_pris), NA_real_, times_pris)
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
  left_join(
    pris_gb,
    by = "datetime",
    relationship = "many-to-one"
  ) |>
  
  arrange(prisomrade, datetime)

# --------------------------------------------------
# 9. Kontroller resultatet
# --------------------------------------------------

# --------------------------------------------------
# 9. Undersok manglende observasjoner
# --------------------------------------------------

variabler_sjekk <- c(
  "pris_norge",
  "forbruk",
  "fyllingsgrad",
  "tilsig",
  "produksjon_Magasinkraftverk",
  "produksjon_Elvekraft",
  "produksjon_Vindkraft pa land",
  "europeisk_pris",
  "britisk_pris"
)

# Behold bare variabler som faktisk finnes i datasettet
variabler_sjekk <- intersect(
  variabler_sjekk,
  names(tibble1)
)

# Manglende verdier fordelt pa prisomrade
mangler_per_omrade <- tibble1 |>
  group_by(prisomrade) |>
  summarise(
    across(
      all_of(variabler_sjekk),
      ~ sum(!is.finite(.x))
    ),
    antall_observasjoner = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(variabler_sjekk),
    names_to = "variabel",
    values_to = "antall_mangler"
  ) |>
  mutate(
    andel_mangler_prosent =
      100 * antall_mangler / antall_observasjoner
  ) |>
  arrange(variabel, prisomrade)

print(mangler_per_omrade, n = Inf)

# --------------------------------------------------
# 10. Manglende verdier totalt
# --------------------------------------------------

mangler_totalt <- tibble1 |>
  summarise(
    across(
      all_of(variabler_sjekk),
      ~ sum(!is.finite(.x))
    ),
    antall_observasjoner = n()
  ) |>
  pivot_longer(
    cols = all_of(variabler_sjekk),
    names_to = "variabel",
    values_to = "antall_mangler"
  ) |>
  mutate(
    andel_mangler_prosent =
      100 * antall_mangler / antall_observasjoner
  ) |>
  arrange(desc(andel_mangler_prosent))

print(mangler_totalt, n = Inf)

# --------------------------------------------------
# 11. Undersok hvor mange observasjoner som gjenstar
# etter at modellvariablene er brukt
# --------------------------------------------------

observasjoner_modell <- tibble1 |>
  group_by(prisomrade) |>
  summarise(
    antall_opprinnelig = n(),
    
    komplett_med_britisk_pris = sum(
      if_all(
        all_of(c(
          "pris_norge",
          "britisk_pris",
          "forbruk",
          "fyllingsgrad",
          "produksjon_Magasinkraftverk"
        )),
        ~ is.finite(.x)
      )
    ),
    
    komplett_med_europeisk_pris = sum(
      if_all(
        all_of(c(
          "pris_norge",
          "europeisk_pris",
          "forbruk",
          "fyllingsgrad",
          "produksjon_Magasinkraftverk"
        )),
        ~ is.finite(.x)
      )
    ),
    
    komplett_uten_utlandsk_pris = sum(
      if_all(
        all_of(c(
          "pris_norge",
          "forbruk",
          "fyllingsgrad",
          "produksjon_Magasinkraftverk"
        )),
        ~ is.finite(.x)
      )
    ),
    
    .groups = "drop"
  )

print(observasjoner_modell, n = Inf)

# --------------------------------------------------
# 12. Lagre datasett og mangelanalyser
# --------------------------------------------------

saveRDS(tibble1, "Datasett/tibble7.rds")

View(tibble1)

View(mangler_per_omrade)
View(mangler_totalt)
View(observasjoner_modell)
