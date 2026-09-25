library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(lubridate)


# ------------------------------------------------------------
# 1. API-nøkkel
# ------------------------------------------------------------

readRenviron("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/.Renviron")

api_key <- Sys.getenv("europa") #henter ut nøkkelen
response <- httr2::request(
  "https://web-api.tp.entsoe.eu/api"
) |>
  httr2::req_url_query(
    securityToken = api_key,
    documentType = "A11",
    processType = "A16",
    in_Domain = "10Y1001A1001A82H",
    out_Domain = "10YNO-2--------T",
    periodStart = "202008010000",
    periodEnd = "202009010000"
  ) |>
  httr2::req_perform()

response


hent_stromflyt <- function(
    start,
    end,
    fra_zone,
    til_zone,
    fra_eic,
    til_eic,
    api_key
) {
  
  start <- as.Date(start)
  end <- as.Date(end)
  
  # Lag en liste med måneder
  maaneder <- seq(
    from = as.Date(format(start, "%Y-%m-01")),
    to = as.Date(format(end, "%Y-%m-01")),
    by = "month"
  )
  
  alle_data <- lapply(maaneder, function(maaned) {
    
    maaned_start <- max(
      maaned,
      start
    )
    
    maaned_slutt <- min(
      seq(maaned, by = "month", length.out = 2)[2] - 1,
      end
    )
    
    message(
      "Henter strømflyt ",
      fra_zone, " -> ", til_zone, ": ",
      maaned_start, " -> ", maaned_slutt
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
        documentType = "A11",
        processType = "A16",
        in_Domain = til_eic,
        out_Domain = fra_eic,
        periodStart = period_start,
        periodEnd = period_end
      ) |>
      httr2::req_perform()
    
    xml <- httr2::resp_body_xml(response)
    
    time_series <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    if (length(time_series) == 0) {
      return(tibble::tibble())
    }
    
    resultat <- lapply(time_series, function(ts) {
      
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
      
      if (is.na(minutes)) {
        stop("Ukjent oppløsning: ", resolution)
      }
      
      points <- xml2::xml_find_all(
        ts,
        ".//*[local-name()='Point']"
      )
      
      if (length(points) == 0) {
        return(NULL)
      }
      
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
      
      datetime <- start_datetime +
        minutes * 60 * (position - 1)
      
      tibble::tibble( #bygger opp en tibble
        datetime = datetime,
        from_zone = fra_zone,
        to_zone = til_zone,
        flow_MW = quantity,
        position = position,
        resolution = resolution
      )
    })
    
    dplyr::bind_rows(resultat)
  })
  
  resultat <- dplyr::bind_rows(alle_data)
  
  if (nrow(resultat) == 0) {
    warning(
      "Ingen strømflytdata ble funnet for ",
      fra_zone, " -> ", til_zone
    )
    
    return(tibble::tibble(
      datetime = as.POSIXct(character()),
      from_zone = character(),
      to_zone = character(),
      flow_MW = numeric(),
      position = integer(),
      resolution = character()
    ))
  }
  
  resultat |>
    dplyr::distinct(
      datetime,
      from_zone,
      to_zone,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(datetime)
}


flyt_no2_delu <- hent_stromflyt(
  start = "2020-08-01",
  end = "2020-08-31",
  fra_zone = "NO2",
  til_zone = "NO1",
  fra_eic = "10YNO-2--------T",
  til_eic = "10YNO-1--------2",
  api_key = api_key
)

flyt_no2_delu
