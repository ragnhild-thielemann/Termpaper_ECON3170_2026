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
# 1. Hent tilsig for én målestasjon
# --------------------------------------------------------

hent_tilsig_stasjon <- function(
    station,
    start_dato = "2026-10-10",
    slutt_dato = Sys.Date(),
    api_key = api_key
) {
  
  message("Henter tilsig for stasjon ", station)
  
  tryCatch({
    
    data <- hent_vannforing(
      api_key = api_key,
      station = station,
      start_date = start_dato,
      end_date = slutt_dato
    )
    
    if (is.null(data) || nrow(data) == 0) {
      return(tibble())
    }
    
    data |>
      mutate(station = as.character(station), .before = 1)
    
  }, error = function(e) {
    
    message(
      "Feilet for stasjon ", station, ": ",
      conditionMessage(e)
    )
    
    tibble()
  })
}


# --------------------------------------------------------
# 2. Hent oversikt over aktive målestasjoner
# --------------------------------------------------------

stasjoner <- hent_stasjoner(
  api_key,
  kun_aktive = TRUE
)


# --------------------------------------------------------
# 3. Hent alle stasjonene og bind sammen dataene
# --------------------------------------------------------

tilsig_stasjoner <- purrr::map_dfr(
  stasjoner$station,
  function(id) {
    
    Sys.sleep(0.5)
    
    hent_tilsig_stasjon(
      station = id,
      start_dato = "2020-01-01",
      slutt_dato = Sys.Date(),
      api_key = api_key
    )
  }
) |>
  filter(!is.na(vannforing))


# --------------------------------------------------------
# 4. Lagre samlet tibble
# --------------------------------------------------------

saveRDS(
  tilsig_stasjoner,
  "Datasett/tilsig_alle_stasjoner.rds"
)