

library(data.table)
library(tidyverse)
library(dtplyr)

source("Kilder/henting_av_data_NVE/funksjon_tilsig.R")

readRenviron("Kilder/.Renviron")
api_key <- Sys.getenv("nve")

if (api_key == "") {
  stop("Fant ikke NVE_HYDAPI. Sjekk Kilder/.Renviron.")
}

# ---------------------------------------------------------
# 1. Alle stasjoner med døgnverdier for vannføring.
#    kun_aktive = FALSE tar også med nedlagte stasjoner.
# ---------------------------------------------------------

stasjoner <- hent_stasjoner(api_key, kun_aktive = TRUE)

nrow(stasjoner)

# ---------------------------------------------------------
# 2. Henter hver stasjon for seg og lagrer den som en egen fil.
#    Dette gjør at vi kan stoppe og fortsette uten å begynne på nytt,
#    og at en enkelt stasjon som feiler ikke ødelegger alt. Dette er
#    gunstig da det er et datasett per målestasjon, som vi til slutt 
#    kombinerer.
# ---------------------------------------------------------

mappe <- "Datasett/tilsig_stasjoner"
dir.create(mappe, recursive = TRUE, showWarnings = FALSE)

start_date <- "2026-10-10"
end_date <- Sys.Date()

hent_og_lagre <- function(id) {
  
  fil <- file.path(mappe, paste0(id, ".rds"))
  
  # Hopper over stasjoner vi allerede har hentet
  if (file.exists(fil)) return(invisible("finnes"))
  
  Sys.sleep(0.5)  #gjør at apiet kan roe seg <3
  
  data <- tryCatch( #prøver å hente ut data for den gitte stajsonen
    hent_vannforing(api_key, station = id,
                    start_date = start_date, end_date = end_date),
    error = function(e) {
      message("Feilet for ", id, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(data)) return(invisible("feilet"))
  
  saveRDS(data, fil)
  invisible("hentet")
}

status <- map_chr(seq_along(stasjoner$station), function(i) {
  if (i %% 25 == 0) message("Stasjon ", i, " av ", nrow(stasjoner))
  hent_og_lagre(stasjoner$station[i])
})

table(status)   # hentet / finnes / feilet. Kjør på nytt hvis noen feilet.

# ---------------------------------------------------------
# 3. Leser inn alle stasjonene og slår dem sammen
# ---------------------------------------------------------

tilsig_stasjoner <- list.files(mappe, pattern = "\\.rds$", full.names = TRUE) |>
  map_dfr(readRDS) |>
  filter(!is.na(vannforing))

# Hvor mange dager har hver stasjon? Mange har hull
dekning <- tilsig_stasjoner |>
  summarise(dager = n(), fra = min(dato), til = max(dato), .by = station) |>
  left_join(stasjoner, by = "station")

summary(dekning$dager)

saveRDS(tilsig_stasjoner, "Datasett/tilsig_alle_stasjoner.rds")
saveRDS(dekning, "Datasett/tilsig_stasjonsoversikt.rds")

# ---------------------------------------------------------
# 4. Plasserer hver stasjon i en prissone.
#    Dette er en grov løsning, som bruker Haversines formel
#    for å måle avstander. Dette er vi klar over i vidre analyse.
# ---------------------------------------------------------

soner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  lon_sone = c(10.75, 6.72, 10.40, 14.40, 5.32),
  lat_sone = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

avstand_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  6371 * 2 * asin(sqrt(
    sin((lat2 - lat1) * rad / 2)^2 +
      cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  ))
}

stasjon_til_sone <- dekning |>
  select(station, latitude, longitude, regulert, felt_km2) |>
  cross_join(soner) |>
  
  mutate(avstand = avstand_km(longitude, latitude, lon_sone, lat_sone)) |>
  
  #Hver stasjon blir tilordnet det den er nærmest. Dette er ikke nødvendigvis
  #den målestasjonen som den generer strøm til
  slice_min(avstand, n = 1, by = station) |>
  select(station, prisomrade, avstand, regulert, felt_km2)

# Antall stasjoner per sone
count(stasjon_til_sone, prisomrade)

# ---------------------------------------------------------
# 5. Slår sammen de to tabellene for stasjonene og prissonene.
#   Dette gjør at vi kan sammenligne barsert på tilsig til 
#   magasinene i de ulike prissonene. 
# ---------------------------------------------------------

tilsig_behandlet <- tilsig_stasjoner |>
  inner_join(stasjon_til_sone, by = "station") |>
  lazy_dt() |>
  mutate(
    indeks = vannforing / mean(vannforing, na.rm = TRUE),
    .by = station
  ) |>
  rename(datetime = dato) |>
  arrange(prisomrade, datetime) |>
  as_tibble()

saveRDS(tilsig_behandlet, "Datasett/tilsig_total.rds")


