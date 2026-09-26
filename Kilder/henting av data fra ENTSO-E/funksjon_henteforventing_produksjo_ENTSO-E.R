library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteeickode.R")
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")


# ==========================================================
# FUNKSJON: Hent faktisk og prognostisert strømforbruk
# ==========================================================

hent_forbruk_ENTSOE <- function(
    start_dato,
    slutt_dato,
    prisomrade = "Norway NO1",
    variabel = "A16",
    api_key = api_key
) {
  
  # --------------------------------------------------------
  # 1. Finn EIC-koden til prisområdet
  # --------------------------------------------------------
  
  land <- stringr::word(prisomrade, 1) #vi deler opp strengen med Land og prissone, slik at disse kan behandles separat. 
  omrade <- stringr::word(prisomrade, -1)
  
  eic_code <- hent_eic_kode(
    land,
    omrade,
    api_key
  )
  
  
  # --------------------------------------------------------
  # 2. Konverter datoer
  # --------------------------------------------------------
  
  start_dato <- as.Date(start_dato)
  slutt_dato <- as.Date(slutt_dato)
  
  
  # --------------------------------------------------------
  # 3. Lag månedlige intervaller
  # --------------------------------------------------------
  
  maaneder <- seq(
    from = as.Date(format(start_dato, "%Y-%m-01")),
    to = as.Date(format(slutt_dato, "%Y-%m-01")),
    by = "month"
  )
  
  
  # --------------------------------------------------------
  # 4. Funksjon som henter én type data
  # --------------------------------------------------------
  
  hent_type <- function(
    maaned,
    process_type,
    navn
  ) {
    
    # Start og slutt for denne måneden
    maaned_start <- max(
      maaned,
      start_dato
    )
    
    maaned_slutt <- min(
      seq(
        maaned,
        by = "month",
        length.out = 2
      )[2] - 1,
      slutt_dato
    )
    
    
    # ------------------------------------------------------
    # ENTSO-E bruker UTC-tid
    # ------------------------------------------------------
    
    period_start <- paste0(
      format(maaned_start, "%Y%m%d"),
      "0000"
    )
    
    period_end <- paste0(
      format(maaned_slutt + 1, "%Y%m%d"),
      "0000"
    )
    
    
    message(
      "Henter ",
      navn,
      ": ",
      maaned_start,
      " -> ",
      maaned_slutt
    )
    
    
    # ------------------------------------------------------
    # Send forespørsel
    # ------------------------------------------------------
    
    response <- httr2::request(
      "https://web-api.tp.entsoe.eu/api"
    ) |>
      httr2::req_url_query(
        securityToken = api_key,
        documentType = "A65",
        processType = process_type,
        outBiddingZone_Domain = eic_code,
        periodStart = period_start,
        periodEnd = period_end
      ) |>
      httr2::req_perform()
    
    
    # ------------------------------------------------------
    # Les XML
    # ------------------------------------------------------
    
    xml <- httr2::resp_body_xml(response)
    
    
    # Finn TimeSeries
    time_series <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    
    message(
      "Fant ",
      length(time_series),
      " TimeSeries"
    )
    
    
    # Hvis ingen data
    if (length(time_series) == 0) {
      
      return(
        tibble::tibble(
          datetime = as.POSIXct(
            character(),
            tz = "UTC"
          ),
          value = numeric()
        )
      )
    }
    
    
    # ------------------------------------------------------
    # Hent hver TimeSeries
    # ------------------------------------------------------
    
    resultat <- lapply(
      time_series,
      function(ts) {
        
        
        # --------------------------------------------------
        # Finn starttidspunkt
        # --------------------------------------------------
        
        interval_start <- xml2::xml_text(
          xml2::xml_find_first(
            ts,
            ".//*[local-name()='start']"
          )
        )
        
        
        # --------------------------------------------------
        # Finn oppløsning
        # --------------------------------------------------
        
        resolution <- xml2::xml_text(
          xml2::xml_find_first(
            ts,
            ".//*[local-name()='resolution']"
          )
        )
        
        
        # --------------------------------------------------
        # Gjør oppløsningen om til minutter
        # --------------------------------------------------
        
        minutes <- dplyr::case_when(
          
          resolution == "PT15M" ~ 15,
          resolution == "PT30M" ~ 30,
          resolution == "PT60M" ~ 60,
          resolution == "PT1H"  ~ 60,
          
          TRUE ~ NA_real_
        )
        
        
        if (is.na(minutes)) {
          
          stop(
            "Ukjent oppløsning: ",
            resolution
          )
        }
        
        
        # --------------------------------------------------
        # Finn alle Point
        # --------------------------------------------------
        
        points <- xml2::xml_find_all(
          ts,
          ".//*[local-name()='Point']"
        )
        
        
        if (length(points) == 0) {
          return(NULL)
        }
        
        
        # --------------------------------------------------
        # Position
        # --------------------------------------------------
        
        position <- as.integer(
          xml2::xml_text(
            xml2::xml_find_all(
              points,
              ".//*[local-name()='position']"
            )
          )
        )
        
        
        # --------------------------------------------------
        # Hent mengde
        # --------------------------------------------------
        
        quantity <- as.numeric(
          xml2::xml_text(
            xml2::xml_find_all(
              points,
              ".//*[local-name()='quantity']"
            )
          )
        )
        
        
        # --------------------------------------------------
        # Starttidspunkt
        # --------------------------------------------------
        
        start_datetime <- as.POSIXct(
          interval_start,
          format = "%Y-%m-%dT%H:%MZ",
          tz = "UTC"
        )
        
        
        # --------------------------------------------------
        # Lag tidspunkt
        # --------------------------------------------------
        
        datetime <- start_datetime +
          minutes * 60 * (position - 1)
        
        
        # --------------------------------------------------
        # Returner data
        # --------------------------------------------------
        
        tibble::tibble(
          datetime = datetime,
          value = quantity
        )
      }
    )
    
    
    dplyr::bind_rows(resultat)
  }
  
  
  # ========================================================
  # 5. Hent faktisk forbruk
  # ========================================================
  
  faktisk <- lapply(
    maaneder,
    function(m) {
      
      hent_type(
        maaned = m,
        process_type = variabel,
        navn = "faktisk forbruk"
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::distinct(datetime, .keep_all = TRUE) |>
    dplyr::rename(
      faktisk_forbruk_MW = value
    )
  
  
  # ========================================================
  # 6. Hent day-ahead prognose
  # ========================================================
  
  prognose <- lapply(
    maaneder,
    function(m) {
      
      hent_type(
        maaned = m,
        process_type = "A01",
        navn = "day-ahead prognose"
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::distinct(datetime, .keep_all = TRUE) |>
    dplyr::rename(
      prognose_forbruk_MW = value
    )
  
  
  # ========================================================
  # 7. Slå sammen
  # ========================================================
  
  resultat <- full_join(
    faktisk,
    prognose,
    by = "datetime"
  )
  
  
  # ========================================================
  # 8. Beregn prognosefeil
  # ========================================================
  
  resultat <- resultat |>
    mutate(
      
      # Faktisk - prognose
      prognosefeil_MW =
        faktisk_forbruk_MW -
        prognose_forbruk_MW,
      
      # Absolutt prognosefeil
      absolutt_prognosefeil_MW =
        abs(prognosefeil_MW),
      
      # Relativ prognosefeil
      prognosefeil_prosent =
        100 *
        prognosefeil_MW /
        prognose_forbruk_MW
    ) |>
    
    arrange(datetime)
  
  
  return(resultat)
}


