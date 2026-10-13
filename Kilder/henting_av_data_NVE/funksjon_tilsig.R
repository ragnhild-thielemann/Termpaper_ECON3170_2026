

library(tidyverse)
library(httr2)
library(lubridate)

# ---------------------------------------------------------
# Funksjoner for å hente vannføring (tilsig) fra NVE sitt HydAPI
# Dokumentasjon: https://hydapi.nve.no/swagger/index.html?urls.primaryName=V1
# Parameter 1001 = vannføring (m3/s), oppløsning 1440 = døgn (argumentet heter oppl)
# ---------------------------------------------------------

hydapi_url <- "https://hydapi.nve.no/api/v1"

# Hjelpefunksjon: gjør et kall mot HydAPI og returnerer JSON som liste
hydapi_get <- function(endepunkt, api_key, query = list()) {
  
  resp <- request(paste0(hydapi_url, "/", endepunkt)) |>
    req_headers("X-API-Key" = api_key, Accept = "application/json") |>
    req_url_query(!!!query) |>
    req_retry(max_tries = 3) |>   # prøver på nytt hvis API-et svikter midlertidig
    req_perform()
  
  resp_body_json(resp, simplifyVector = FALSE)
}


# ---------------------------------------------------------
# 1. Henter stasjonene som har vannføring i døgnoppløsning
# ---------------------------------------------------------

hent_stasjoner <- function(api_key,
                           parameter = 1001,
                           oppl = 1440,
                           kun_aktive = TRUE) {
  
  svar <- hydapi_get(
    "Stations",
    api_key,
    query = list(Active = if (kun_aktive) "OnlyActive" else "All")
  )
  
  # Hjelper som gir NA hvis feltet mangler, slik at map_dfr ikke feiler
  felt <- function(x, navn) x[[navn]] %||% NA
  
  stasjoner <- purrr::map_dfr(svar$data, function(x) {
    
    # Finner serien for vannføring og sjekker at den finnes i riktig oppløsning
    serier <- x$seriesList %||% list()
    # NB: bruker == og ikke identical(), siden JSON gir heltall (1001L)
    # mens parameter her er et desimaltall (1001)
    treff <- purrr::keep(serier, ~ isTRUE(.x$parameter == parameter))
    
    har_serie <- length(treff) > 0 &&
      any(purrr::map_lgl(
        treff[[1]]$resolutionList %||% list(),
        ~ isTRUE(.x$resTime == oppl)
      ))
    
    tibble(
      station = felt(x, "stationId"),
      navn = felt(x, "stationName"),
      elv = felt(x, "riverName"),
      latitude = as.numeric(felt(x, "latitude")),
      longitude = as.numeric(felt(x, "longitude")),
      felt_km2 = as.numeric(felt(x, "drainageBasinArea")),
      regulert = as.numeric(felt(x, "regulationArea")),
      har_serie = har_serie
    )
  })
  
  # Diagnose: hvor mange stasjoner kom tilbake, og hvor mange har serien vi vil ha?
  message(
    "Stasjoner fra API: ", nrow(stasjoner),
    ", med vannføring i valgt oppløsning: ", sum(stasjoner$har_serie, na.rm = TRUE)
  )
  
  stasjoner |>
    filter(har_serie, !is.na(latitude), !is.na(longitude)) |>
    select(-har_serie)
}


# ---------------------------------------------------------
# 2. Finner de n nærmeste stasjonene til et punkt (haversine)
# ---------------------------------------------------------

naermeste_stasjoner <- function(stasjoner,
                                longitude,
                                latitude,
                                antall = 5,
                                maks_avstand_km = 150) {
  
  rad <- pi / 180
  
  stasjoner |>
    mutate(
      avstand_km = 6371 * 2 * asin(sqrt(
        sin((latitude - !!latitude) * rad / 2)^2 +
          cos(!!latitude * rad) * cos(latitude * rad) *
          sin((longitude - !!longitude) * rad / 2)^2
      ))
    ) |>
    filter(avstand_km <= maks_avstand_km) |>
    arrange(avstand_km) |>
    slice_head(n = antall)
}


# ---------------------------------------------------------
# 3. Henter døgnverdier av vannføring for én stasjon
# ---------------------------------------------------------

hent_vannforing <- function(api_key,
                            station,
                            start_date = "2020-01-01",
                            end_date = Sys.Date(),
                            parameter = 1001,
                            oppl = 1440) {
  
  svar <- hydapi_get(
    "Observations",
    api_key,
    query = list(
      StationId = station,
      Parameter = parameter,
      ResolutionTime = oppl,
      ReferenceTime = paste0(start_date, "/", end_date)
    )
  )
  
  # Ingen data i perioden gir en tom tibble i stedet for feil
  if (length(svar$data) == 0 || length(svar$data[[1]]$observations) == 0) {
    return(tibble(station = character(), dato = as.Date(character()),
                  vannforing = numeric()))
  }
  
  purrr::map_dfr(svar$data[[1]]$observations, function(o) {
    tibble(
      station = station,
      dato = as.Date(substr(o$time, 1, 10)),
      vannforing = as.numeric(o$value %||% NA)
    )
  }) |>
    arrange(dato)
}

