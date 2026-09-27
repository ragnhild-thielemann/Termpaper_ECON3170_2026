

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteeickode.R")
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")


library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(purrr)


hent_produksjon_ENTSOE <- function(
    start_dato,
    slutt_dato,
    prisomrade = "Norway NO1",
    api_key = api_key,
    psr_type = NULL
) {
  
  # ------------------------------------------------------------
  # 1. Klargjør input
  # ------------------------------------------------------------
  
  start_dato <- as.Date(start_dato)
  slutt_dato <- as.Date(slutt_dato)
  
  if (start_dato > slutt_dato) {
    stop("start_dato kan ikke være etter slutt_dato.")
  }
  
  # Finn land og prisområde
  land <- stringr::word(prisomrade, 1)
  omrade <- stringr::word(prisomrade, -1)
  
  # Finn EIC-kode
  eic_code <- hent_eic_kode(
    land = land,
    omrade = omrade,
    api_key = api_key
  )
  
  # ------------------------------------------------------------
  # 2. Lag månedsintervaller
  # ------------------------------------------------------------
  
  maaneder <- seq(
    from = floor_date(start_dato, "month"),
    to = floor_date(slutt_dato, "month"),
    by = "month"
  )
  
  
  # ------------------------------------------------------------
  # 3. Funksjon som henter én måned
  # ------------------------------------------------------------
  
  hent_maaned <- function(maaned) {
    
    maaned_start <- max(
      maaned,
      start_dato
    )
    
    maaned_slutt <- min(
      ceiling_date(maaned, "month") - days(1),
      slutt_dato
    )
    
    # ENTSO-E bruker UTC og format YYYYMMDDHHMM
    period_start <- paste0(
      format(maaned_start, "%Y%m%d"),
      "0000"
    )
    
    period_end <- paste0(
      format(maaned_slutt + days(1), "%Y%m%d"),
      "0000"
    )
    
    
    # ----------------------------------------------------------
    # API-kall
    # ----------------------------------------------------------
    
    response <- tryCatch(
      {
        request("https://web-api.tp.entsoe.eu/api") |>
          req_url_query(
            securityToken = api_key,
            documentType = "A75",
            processType = "A16",
            in_Domain = eic_code,
            periodStart = period_start,
            periodEnd = period_end
          ) |>
          req_timeout(120) |>
          req_retry(
            max_tries = 3,
            backoff = ~ 2^.x
          ) |>
          req_perform()
      },
      error = function(e) {
        
        warning(
          "Feil ved henting av ",
          format(maaned, "%Y-%m"),
          ": ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
    if (is.null(response)) {
      return(NULL)
    }
    
    
    # ----------------------------------------------------------
    # 4. Les XML
    # ----------------------------------------------------------
    
    xml <- resp_body_xml(response)
    
    time_series <- xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    if (length(time_series) == 0) {
      warning(
        "Ingen TimeSeries funnet for ",
        format(maaned, "%Y-%m")
      )
      
      return(NULL)
    }
    
    
    # ----------------------------------------------------------
    # 5. Behandle hver TimeSeries
    # ----------------------------------------------------------
    
    map_dfr(time_series, function(ts) {
      
      # Produksjonstype
      psr <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='MktPSRType']/*[local-name()='psrType']"
        )
      )
      
      # Hvis psr_type er spesifisert,
      # behold bare denne typen
      if (!is.null(psr_type) && psr != psr_type) {
        return(NULL)
      }
      
      
      # Starttidspunkt
      interval_start <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='Period']/*[local-name()='timeInterval']/*[local-name()='start']"
        )
      )
      
      # Oppløsning
      resolution <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='Period']/*[local-name()='resolution']"
        )
      )
      
      minutes <- dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT30M" ~ 30,
        resolution %in% c("PT60M", "PT1H") ~ 60,
        TRUE ~ NA_real_
      )
      
      if (is.na(minutes)) {
        warning(
          "Ukjent tidsoppløsning: ",
          resolution
        )
        return(NULL)
      }
      
      
      # --------------------------------------------------------
      # 6. Hent alle punktene
      # --------------------------------------------------------
      
      points <- xml_find_all(
        ts,
        ".//*[local-name()='Point']"
      )
      
      if (length(points) == 0) {
        return(NULL)
      }
      
      position <- as.integer(
        xml_text(
          xml_find_all(
            points,
            ".//*[local-name()='position']"
          )
        )
      )
      
      quantity <- as.numeric(
        xml_text(
          xml_find_all(
            points,
            ".//*[local-name()='quantity']"
          )
        )
      )
      
      
      # --------------------------------------------------------
      # 7. Lag datasett
      # --------------------------------------------------------
      
      start_datetime <- as.POSIXct(
        interval_start,
        format = "%Y-%m-%dT%H:%MZ",
        tz = "UTC"
      )
      
      tibble(
        datetime = start_datetime +
          minutes * 60 * (position - 1),
        production_MW = quantity,
        psr_type = psr,
        prisomrade = omrade
      )
    })
  }
  
  
  # ------------------------------------------------------------
  # 8. Hent alle måneder
  # ------------------------------------------------------------
  
  resultat <- map_dfr(
    maaneder,
    hent_maaned
  )
  
  
  # ------------------------------------------------------------
  # 9. Kontroller resultat
  # ------------------------------------------------------------
  
  if (nrow(resultat) == 0) {
    warning("Ingen produksjonsdata ble hentet.")
    return(resultat)
  }
  
  
  # Fjern eventuelle duplikater
  resultat <- resultat |>
    distinct(
      datetime,
      psr_type,
      .keep_all = TRUE
    ) |>
    arrange(datetime)
  
  
  resultat
}

