
library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(docstring)
library(lubridate)



hent_vannreservoar <- function(start_date ,
                               end_date= Sys.Date(),
                               areas = c("NO1", "NO2", "NO3", "NO4", "NO5")) { #har initialverdier for funksjonen, som kan endres om ønskelig
  
  # URL til NVE sitt API
  url <- paste0(
    "https://biapi.nve.no/magasinstatistikk/",
    "api/Magasinstatistikk/HentOffentligData"
  )
  
  # Gjør dataen om til en tibble, slik at vi kan behandle det som en tibble
  data <- jsonlite::fromJSON(url)
  
  # Bearbeider datasettet, slik at vi får det på et tidy format
  reservoarer <- data |>
    dplyr::filter(
      iso_aar >= year(start_date),
      iso_aar <= year(end_date)
    ) |>
    dplyr::mutate(
      prisomrade = paste0("NO", omrnr),
    ) |>
    dplyr::filter(
      prisomrade %in% areas
    ) |>
    dplyr::select(
      datetime = dato_Id,
      prisomrade,
      fyllingsgrad,
      fyllingsgrad_forrige_uke
    ) |>
    dplyr::mutate(endring = fyllingsgrad-fyllingsgrad_forrige_uke)|>
    dplyr::arrange(
      datetime,
      prisomrade
    )
  
  return(reservoarer)
}