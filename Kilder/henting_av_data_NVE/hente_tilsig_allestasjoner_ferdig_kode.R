library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(purrr)

source("Kilder/henting_av_data_NVE/funksjon_tilsig.R")
readRenviron("Kilder/.Renviron")

api_key <- Sys.getenv("nve")

if (api_key == "") {
  stop("Fant ikke NVE API-nokkel. Sjekk Kilder/.Renviron.")
}

# --------------------------------------------------------
# 1. Hent stasjoner og velg de nærmeste for hver prissone
# --------------------------------------------------------

stasjoner <- hent_stasjoner(
  api_key,
  kun_aktive = TRUE
)

# ---------------------------------------------------------
# 1. Definer sentrumspunkter for prisomradene
# ---------------------------------------------------------

koordinater_for_prissoner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  longitude = c(10.75, 6.72, 10.40, 14.40, 5.32),
  latitude  = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

# ---------------------------------------------------------
# 2. Finn naermeste sentrumspunkt for hver malestasjon
# ---------------------------------------------------------

stasjon_til_prissone <- stasjoner |>
  filter(
    !is.na(longitude),
    !is.na(latitude)
  ) |>
  mutate(
    station = as.character(station),
    prisomrade = purrr::map2_chr(
      longitude,
      latitude,
      \(lon, lat) {
        
        # Beregn avstand fra stasjonen til alle fem punktene
        rad <- pi / 180
        
        dlat <- (
          koordinater_for_prissoner$latitude - lat
        ) * rad
        
        dlon <- (
          koordinater_for_prissoner$longitude - lon
        ) * rad
        
        a <- sin(dlat / 2)^2 +
          cos(lat * rad) *
          cos(koordinater_for_prissoner$latitude * rad) *
          sin(dlon / 2)^2
        
        avstand_km <- 6371 * 2 *
          asin(pmin(1, sqrt(a)))
        
        # Tilordne stasjonen til det naermeste punktet
        koordinater_for_prissoner$prisomrade[
          which.min(avstand_km)
        ]
      }
    )
  ) |>
  select(station, prisomrade, navn, latitude, longitude)

# Kontroller fordelingen
stasjon_til_prissone |>
  count(prisomrade)

# Se alle stasjonene og deres tildelte prisomrade


# Kontroller hvilke stasjoner som skal hentes

# ---------------------------------------------------------
# 2. Hent data stasjon for stasjon
# ---------------------------------------------------------

# Hovedtibbel som ligger lokalt i R-sessionen
tilsig_total <- tibble(
  station = character(),
  dato = as.Date(character()),
  vannforing = numeric()
)

# Oversikt over eventuelle feil
feilede_stasjoner <- tibble(
  station = character(),
  
  feil = character()
)

# Går gjennom målestasjonene én etter én
for (i in seq_len(nrow(stasjoner))) {
  
  stasjon <- stasjoner[i, ]
  
  id <- as.character(stasjon$station)

  message(
    "Henter stasjon ", id,
    " (", i, " av ", nrow(stasjoner), ")"
  )
  
  resultat <- tryCatch({
    
    Sys.sleep(0.5)
    
    data <- hent_vannforing(
      api_key = api_key,
      station = id,
      start_date = "2020-01-01",
      end_date = Sys.Date()
    )
    
    if (nrow(data) == 0) { #stopper å lete, dersom det ikke retuneres noe
      stop("Ingen data returnert")
    }
    
    data |>
      mutate(station = as.character(station)) |>
      dplyr::select(station, dato, vannforing)
    
  }, error = function(e) {
    
    message(
      "Feil for stasjon ", id, ": ",
      conditionMessage(e)
    )
    
    feilede_stasjoner <<- bind_rows(
      feilede_stasjoner,
      tibble(
        station = id,
        prisomrade = sone,
        feil = conditionMessage(e)
      )
    )
    
    NULL
  })
  
  # Fortsetter til neste stasjon dersom kallet feilet
  if (is.null(resultat)) {
    next
  }
  
  # Legger dataene til hovedtibbelen i minnet
  tilsig_total <- bind_rows(
    tilsig_total,
    resultat
  )
  
 
}
tilsig_total

# Koordinater for prisomradene
koordinater_for_prissoner <- tibble(
  prisomrade = c("NO1", "NO2", "NO3", "NO4", "NO5"),
  longitude = c(10.75, 6.72, 10.40, 14.40, 5.32),
  latitude = c(59.91, 58.66, 63.43, 67.28, 60.39)
)

# Velg de fem naermeste stasjonene for hvert prisomrade
stasjoner_prissone <- koordinater_for_prissoner |>
  mutate(
    stasjoner = purrr::map2(
      longitude,
      latitude,
      \(lon, lat) naermeste_stasjoner(
        stasjoner,
        longitude = lon,
        latitude = lat,
        antall = 5
      )
    )
  ) |>
  dplyr::select(prisomrade, stasjoner) |>
  tidyr::unnest(stasjoner) |>
  dplyr::select(station, prisomrade) |>
  mutate(station = as.character(station)) |>
  distinct()






tilsig_total <- tilsig_total |>
  mutate(station = as.character(station)) |>
  select(-any_of("prisomrade")) |>
  left_join(
    stasjon_til_prissone |>
      select(station, prisomrade),
    by = "station"
  )

# Kontroller hvor mange stasjoner som ble koblet
tilsig_total |>
  summarise(
    antall_stasjoner = n_distinct(station),
    antall_med_prissone = n_distinct(
      station[!is.na(prisomrade)]
    ),
    antall_uten_prissone = n_distinct(
      station[is.na(prisomrade)]
    )
  )

# Kontroller fordelingen per prisomrade
tilsig_total |>
  distinct(station, prisomrade) |>
  count(prisomrade)
View(tilsig_total)


saveRDS(tilsig_total,
        "Datasett/tilsig_total_denviskalbruke.rds")

