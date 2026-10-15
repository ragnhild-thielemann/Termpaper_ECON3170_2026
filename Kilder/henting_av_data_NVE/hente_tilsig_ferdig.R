
library(tidyverse)
library(data.table)

source("Kilder/henting_av_data_NVE/funksjon_tilsig.R")
readRenviron("Kilder/.Renviron")

api_key <- Sys.getenv("nve")

if (api_key == "") {
  stop("Fant ikke NVE API-nokkel. Sjekk Kilder/.Renviron.")
}

# ---------------------------------------------------------
# 1. Hent stasjoner og velg stasjoner nær hver prissone
# ---------------------------------------------------------

stasjoner <- hent_stasjoner(
  api_key,
  kun_aktive = TRUE
)

koordinater_for_prissoner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  longitude = c(10.75, 6.72, 10.40, 14.40, 5.32),
  latitude = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

valgte_stasjoner <- koordinater_for_prissoner |>
  mutate(
    stasjoner = pmap(
      list(longitude, latitude),
      \(lon, lat) naermeste_stasjoner(
        stasjoner,
        longitude = lon,
        latitude = lat,
        antall = 5
      )
    )
  ) |>
  select(prisomrade, stasjoner) |>
  unnest(stasjoner) |>
  distinct(prisomrade, station, .keep_all = TRUE)

# Lagre oversikt over stasjonene
saveRDS(
  valgte_stasjoner,
  "Datasett/tilsig_stasjonsoversikt.rds"
)

# ---------------------------------------------------------
# 2. Hent vannføring for én stasjon om gangen
# ---------------------------------------------------------

hent_en_stasjon <- function(id) {
  
  Sys.sleep(0.5)
  
  data <- hent_vannforing(
    api_key = api_key,
    station = id,
    start_date = "2020-01-01",
    end_date = Sys.Date()
  )
  
  if (nrow(data) == 0) {
    message("Ingen data for stasjon ", id)
    return(NULL)
  }
  
  data |>
    transmute(
      dato,
      !!paste0("stasjon_", id) := vannforing
    )
}

tilsig_liste <- map(
  valgte_stasjoner$station,
  hent_en_stasjon
) |>
  compact()

# ---------------------------------------------------------
# 3. Bind sammen på wide-format
# ---------------------------------------------------------

tilsig_wide <- reduce(
  tilsig_liste,
  full_join,
  by = "dato"
) |>
  arrange(dato)

