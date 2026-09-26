
library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteeickode.R")
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")


hent_produksjon_ENTSOE <- function(
    start_dato,
    slutt_dato,
    prisomrade = "Norway NO1",
    api_key = api_key,
    psr_type = "B04"
) {
  
  land <- stringr::word(prisomrade, 1)
  omrade <- stringr::word(prisomrade, -1)
  
  eic_code <- hent_eic_kode(
    land,
    omrade,
    api_key
  )
  
  start_dato <- as.Date(start_dato)
  slutt_dato <- as.Date(slutt_dato)
  
  maaneder <- seq(
    from = as.Date(format(start_dato, "%Y-%m-01")),
    to = as.Date(format(slutt_dato, "%Y-%m-01")),
    by = "month"
  )
  
  hent_maaned <- function(maaned) {
    
    maaned_start <- max(maaned, start_dato)
    
    maaned_slutt <- min(
      seq(
        maaned,
        by = "month",
        length.out = 2
      )[2] - 1,
      slutt_dato
    )
    
    period_start <- paste0(
      format(maaned_start, "%Y%m%d"),
      "0000"
    )
    
    period_end <- paste0(
      format(maaned_slutt + 1, "%Y%m%d"),
      "0000"
    )
    
    response <- httr2::request(
      "https://web-api.tp.entsoe.eu/api"
    ) |>
      httr2::req_url_query(
        securityToken = api_key,
        documentType = "A75",
        processType = "A16",
        in_Domain = eic_code,
        periodStart = period_start,
        periodEnd = period_end
      ) |>
      httr2::req_perform()
    
    xml <- httr2::resp_body_xml(response)
    
    time_series <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    resultat <- lapply(time_series, function(ts) {
      
      # Finn produksjonstype
      psr <- xml2::xml_text(
        xml2::xml_find_first(
          ts,
          ".//*[local-name()='MktPSRType']/*[local-name()='psrType']"
        )
      )
      
      # Vi beholder bare ønsket produksjonstype
      if (psr != psr_type) {
        return(NULL)
      }
      
      interval_start <- xml2::xml_text(
        xml2::xml_find_first(
          ts,
          ".//*[local-name()='start']"
        )
      )
      
      resolution <- xml2::xml_text(
        xml2::xml_find_first(
          ts,
          ".//*[local-name()='resolution']"
        )
      )
      
      minutes <- dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT30M" ~ 30,
        resolution == "PT60M" ~ 60,
        resolution == "PT1H"  ~ 60,
        TRUE ~ NA_real_
      )
      
      points <- xml2::xml_find_all(
        ts,
        ".//*[local-name()='Point']"
      )
      
      position <- as.integer(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            ".//*[local-name()='position']"
          )
        )
      )
      
      quantity <- as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            ".//*[local-name()='quantity']"
          )
        )
      )
      
      start_datetime <- as.POSIXct(
        interval_start,
        format = "%Y-%m-%dT%H:%MZ",
        tz = "UTC"
      )
      
      tibble::tibble(
        datetime = start_datetime +
          minutes * 60 * (position - 1),
        production_MW = quantity,
        prisomrade = omrade
      )
    })
    
    dplyr::bind_rows(resultat)
  }
  
  resultat <- lapply(
    maaneder,
    hent_maaned
  ) |>
    dplyr::bind_rows() |>
    dplyr::distinct(datetime, .keep_all = TRUE) |>
    dplyr::arrange(datetime)
  
  resultat
}


