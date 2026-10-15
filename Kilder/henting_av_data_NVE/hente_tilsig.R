
library(data.table)
library(tidyverse)
library(dtplyr)

source("Kilder/henting_av_data_NVE/funksjon_tilsig.R")

# Nøkkelen ligger i Kilder/.Renviron (som er ignorert av git) på formen:
# NVE_HYDAPI=din-nøkkel
readRenviron("Kilder/.Renviron")
api_key <- Sys.getenv("nve")

if (api_key == "") {
  stop("Fant ikke NVE_HYDAPI. Sjekk Kilder/.Renviron.")
}

# ---------------------------------------------------------
# 1. Henter alle stasjoner med døgnverdier for vannføring
# ---------------------------------------------------------

stasjoner <- hent_stasjoner(api_key)

# Sjekk hva som faktisk kom tilbake, slik at vi ikke kjører koden, dersom den ikke fungerer
glimpse(stasjoner)
summary(stasjoner$regulert)

# ---------------------------------------------------------
# 2. Velger stasjoner nær hver prissone. Samme koordinater som
#    for nedbør, slik at datasettene kan slås sammen.
#    Husk at stasjonene bør velges med omhu: uregulerte felt gir
#    naturlig tilsig, regulerte felt påvirkes av kraftverkene.
#    Gå gjennom listen under og sjekk at stasjonene er rimelige.
# ---------------------------------------------------------

koordinater_for_prissoner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  longitude = c(10.75, 6.72, 10.40, 14.40, 5.32),
  latitude  = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

valgte_stasjoner <- koordinater_for_prissoner |>
  mutate(
    stasjoner = pmap(
      list(longitude, latitude),
      \(lon, lat) naermeste_stasjoner(stasjoner, lon, lat, antall = 5)
    )
  ) |>
  select(prisomrade, stasjoner) |>
  unnest(stasjoner)

# Se på utvalget før du henter data
print(valgte_stasjoner, n = Inf)

# ---------------------------------------------------------
# 3. Henter vannføring for hver valgt stasjon
#    (en liten pause mellom kallene for å være snill mot API-et)
# ---------------------------------------------------------

hent_en <- function(id) {
  Sys.sleep(0.5)
  hent_vannforing(
    api_key,
    station = id,
    start_date = "2026-10-01",
    end_date = Sys.Date()
  )
}

tilsig_alle <- valgte_stasjoner |>
  mutate(data = map(station, hent_en)) |>
  select(prisomrade, station, data) |>
  unnest(data, names_sep = "_") |>
  rename(dato = data_dato, vannforing = data_vannforing) |>
  select(prisomrade, station, dato, vannforing)

# ---------------------------------------------------------
# 4. Samler til én verdi per prissone per dag.
#    Stasjonene har ulik størrelse, så vi lager også en indeks der
#    hver stasjons vannføring deles på stasjonens eget gjennomsnitt.
#    Da veier ikke store elver tyngre bare fordi de er store.
# ---------------------------------------------------------


tilsig_behandlet <- tilsig_alle |>
  lazy_dt() |>
  
  #finner vannførignen mot gjennomsnittlig vannføring for hver målestasjon
  mutate(indeks = vannforing / mean(vannforing, na.rm = TRUE),
    .by = station
  ) |>

  as_tibble()

View(tilsig_behandlet)
# Rask kontroll: dekning per prissone
tilsig_behandlet |>
  summarise(
    fra = min(datetime), til = max(datetime),
    dager = n(), snitt_stasjoner = mean(antall_stasjoner),
    .by = prisomrade
  )

# Relativ sti, slik at skriptet fungerer uansett hvor prosjektet ligger
saveRDS(tilsig_behandlet, "Datasett/tilsig_total_ferdig.rds")

