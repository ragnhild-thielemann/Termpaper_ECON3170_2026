# ============================================================
# ENTSO-E strømpriser
# Funksjon for å hente strømpriser
# ============================================================

library(httr2)
library(xml2)
library(dplyr)
library(tibble)


# ------------------------------------------------------------
# 1. API-nøkkel
# ------------------------------------------------------------


readRenviron("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/.Renviron")

api_key <- Sys.getenv("europa") #henter ut nøkkelen


if (api_key == "") {
  stop("Fant ikke ENTSOE_TOKEN. Sjekk .Renviron.")
}


# ------------------------------------------------------------
# 2. Funksjon for å hente strømpriser
# ------------------------------------------------------------

hent_priser <- function(
    start,
    end,
    country,
    zone,
    eic_code,
    api_key
) {
  
  # Gjør datoene om til Date
  start <- as.Date(start)
  end <- as.Date(end)
  
  
  # ----------------------------------------------------------
  # Lag tidspunkt for API-kallet
  # ----------------------------------------------------------
  
  period_start <- paste0(
    format(start, "%Y%m%d"),
    "0000"
  )
  
  # +1 fordi vi vil inkludere hele sluttdatoen
  period_end <- paste0(
    format(end + 1, "%Y%m%d"),
    "0000"
  )
  
  
  message(
    "Henter ", country, " ", zone, ": ",
    start, " -> ", end
  )
  
  
  # ----------------------------------------------------------
  # API-kall
  # ----------------------------------------------------------
  
  url <- "https://web-api.tp.entsoe.eu/api"
  
  response <- request(url) |>
    req_url_query(
      securityToken = api_key,
      documentType = "A44",
      in_Domain = eic_code,
      out_Domain = eic_code,
      periodStart = period_start,
      periodEnd = period_end
    ) |>
    req_perform()
  
  
  # ----------------------------------------------------------
  # Kontroller HTTP-status
  # ----------------------------------------------------------
  
  if (resp_status(response) != 200) {
    stop(
      "API-kallet feilet for ",
      country,
      ". HTTP-status: ",
      resp_status(response)
    )
  }
  
  
  # ----------------------------------------------------------
  # Les XML
  # ----------------------------------------------------------
  
  xml <- resp_body_xml(response)
  
  
  # ----------------------------------------------------------
  # Finn TimeSeries
  # ----------------------------------------------------------
  
  time_series <- xml_find_all(
    xml,
    ".//*[local-name()='TimeSeries']"
  )
  
  
  if (length(time_series) == 0) {
    warning(
      "Ingen TimeSeries for ",
      country,
      " ",
      zone
    )
    
    return(tibble())
  }
  
  
  # ----------------------------------------------------------
  # Hent data fra hver TimeSeries
  # ----------------------------------------------------------
  
  resultat <- lapply(
    time_series,
    function(ts) {
      
      # ----------------------------------------------
      # Start og slutt
      # ----------------------------------------------
      
      interval_start <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='start']"
        )
      )
      
      interval_end <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='end']"
        )
      )
      
      
      # ----------------------------------------------
      # Oppløsning
      # ----------------------------------------------
      
      resolution <- xml_text(
        xml_find_first(
          ts,
          ".//*[local-name()='resolution']"
        )
      )
      
      
      minutes <- case_when(
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
      
      
      # ----------------------------------------------
      # Prisobservasjoner
      # ----------------------------------------------
      
      points <- xml_find_all(
        ts,
        ".//*[local-name()='Point']"
      )
      
      
      if (length(points) == 0) {
        return(NULL)
      }
      
      
      # ----------------------------------------------
      # Position
      # ----------------------------------------------
      
      position <- as.integer(
        xml_text(
          xml_find_all(
            points,
            ".//*[local-name()='position']"
          )
        )
      )
      
      
      # ----------------------------------------------
      # Pris
      # ----------------------------------------------
      
      price <- as.numeric(
        xml_text(
          xml_find_all(
            points,
            ".//*[local-name()='price.amount']"
          )
        )
      )
      
      
      # ----------------------------------------------
      # Kontroller
      # ----------------------------------------------
      
      if (length(position) != length(price)) {
        stop(
          "Antall positioner og priser er forskjellig."
        )
      }
      
      
      # ----------------------------------------------
      # Starttidspunkt
      # ----------------------------------------------
      
      start_datetime <- as.POSIXct(
        interval_start,
        format = "%Y-%m-%dT%H:%MZ",
        tz = "UTC"
      )
      
      
      if (is.na(start_datetime)) {
        stop(
          "Klarte ikke å tolke tidspunkt: ",
          interval_start
        )
      }
      
      
      # ----------------------------------------------
      # Lag tidspunkt
      # ----------------------------------------------
      
      datetime <- start_datetime +
        minutes * 60 * (position - 1)
      
      
      # ----------------------------------------------
      # Lag tibble
      # ----------------------------------------------
      
      tibble(
        datetime = datetime,
        country = country,
        zone = zone,
        price = price,
        position = position,
        resolution = resolution
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Slå sammen alle TimeSeries
  # ----------------------------------------------------------
  
  resultat <- bind_rows(resultat)
  
  
  # ----------------------------------------------------------
  # Fjern eventuelle duplikater
  # ----------------------------------------------------------
  
  resultat <- resultat |>
    distinct(
      datetime,
      country,
      zone,
      .keep_all = TRUE
    ) |>
    arrange(datetime)
  
  
  # ----------------------------------------------------------
  # Melding
  # ----------------------------------------------------------
  
  message(
    "Hentet ",
    nrow(resultat),
    " observasjoner for ",
    country,
    " ",
    zone
  )
  
  
  return(resultat)
}


# ============================================================
# 3. TEST: Østerrike, august 2024
# ============================================================

priser_at <- hent_priser(
  start = "2024-08-01",
  end = "2024-08-31",
  country = "Austria",
  zone = "AT",
  eic_code = "10YAT-APG------L",
  api_key = api_key
)


# ============================================================
# 4. Se på resultatet
# ============================================================

print(priser_at, n = 20)


# ============================================================
# 5. Kontroller datasettet
# ============================================================

message("")
message("========== KONTROLL ==========")

message(
  "Antall observasjoner: ",
  nrow(priser_at)
)

message(
  "Første tidspunkt: ",
  min(priser_at$datetime)
)

message(
  "Siste tidspunkt: ",
  max(priser_at$datetime)
)

message(
  "Laveste pris: ",
  min(priser_at$price, na.rm = TRUE)
)

message(
  "Høyeste pris: ",
  max(priser_at$price, na.rm = TRUE)
)

message(
  "Antall manglende priser: ",
  sum(is.na(priser_at$price))
)

message(
  "Antall unike tidspunkt: ",
  n_distinct(priser_at$datetime)
)