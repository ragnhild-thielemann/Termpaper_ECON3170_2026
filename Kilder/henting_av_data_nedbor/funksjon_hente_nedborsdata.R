
library(tidyverse)
library(httr)

hent_nedbor <- function(key,
                        longitude,
                        latitude,
                        start_date = "2020-01-01",
                        end_date = Sys.Date(),
                        antall_stasjoner = 10) {
  
  # URL til Frost API for å finne værstasjoner
  url_stasjoner <- "https://frost.met.no/sources/v0.jsonld"
  
  response_stasjoner <- GET(
    url_stasjoner,
    authenticate(key, ""),
    query = list(
      types = "SensorSystem",
      geometry = paste0(
        "nearest(POINT(",
        longitude, " ", latitude,
        "))"
      ),
      nearestmaxcount = antall_stasjoner
    )
  )
  
  # Sjekker at API-kallet var vellykket
  stop_for_status(response_stasjoner)
  
  # Henter ut stasjonsinformasjonen fra API-responsen
  stasjoner_for_observasjon <- httr::content(
    response_stasjoner,
    as = "parsed"
  )$data
  
  # Gjør stasjonsinformasjonen om til en tibble
  stations <- purrr::map_dfr(
    stasjoner_for_observasjon,
    function(x) {
      
      tibble(
        station = x$id,
        navn = x$name,
        avstand_km = x$distance,
        hoyde_moh = x$masl,
        kommune = x$municipality
      )
      
    }
  )
  
  # URL til API-et for å hente observasjoner
  observations_url <- "https://frost.met.no/observations/v0.jsonld"
  
  ver_response <- GET(
    observations_url,
    authenticate(key, ""),
    query = list(
      sources = paste(stations$station, collapse = ","),
      referencetime = paste0(start_date, "/", end_date),
      elements = "sum(precipitation_amount P1D)",
      timeoffsets = "default",
      levels = "default"
    )
  )
  
  # Sjekker at API-kallet var vellykket
  stop_for_status(ver_response)
  
  # Henter ut observasjonene
  data <- httr::content(
    ver_response,
    as = "parsed"
  )$data
  
  # Gjør observasjonene om til en tibble
  nedbor <- purrr::map_dfr(
    data,
    function(x) {
      
      tibble(
        station = x$sourceId,
        dato = as.Date(x$referenceTime),
        nedbor_mm = purrr::map_dbl(
          x$observations,
          ~ as.numeric(.x$value)|>
            arrange(dato)
        )
      )
      
    }
  )
  
  return(nedbor)
}
