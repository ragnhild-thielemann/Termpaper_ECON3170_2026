


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



if (api_key == "") {
  stop("Fant ikke ENTSOE_TOKEN. Sjekk .Renviron.")
}


hent_priser_norge <- function(
    start,
    end,
    prisomrade,
    api_key
) {
  if (word(prisomrade,1)== "Norway"){ #tester for prisområde i Norge
    eic_koder <- c(
      NO1 = "10YNO-1--------2",
      NO2 = "10YNO-2--------T",
      NO3 = "10YNO-3--------J",
      NO4 = "10YNO-4--------9",
      NO5 = "10Y1001A1001A48H"
    )
    
    eic_code <- eic_koder[word(prisomrade,-1)]
  }
  
  
  
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
      "Henter Norge ", prisomrade, ": ",
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
        documentType = "A44",
        in_Domain = eic_code,
        out_Domain = eic_code,
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
      
      price <- as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            ".//*[local-name()='price.amount']"
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
      
      tibble::tibble(
        datetime = datetime,
        country = "Norway",
        zone = prisomrade,
        price = price,
        position = position,
        resolution = resolution
      )
    })
    
    dplyr::bind_rows(resultat)
  })
  
  dplyr::bind_rows(alle_data) |>
    dplyr::distinct(
      datetime,
      country,
      zone,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(datetime)
}

#kan nå hente for hvert prisområde vi ønsker å ha ut. Funksjonen bør lages generell, slik at


#Struktur = 
# Hvis i norge, hent inn tilleggsinformasjonen om prisområde. Du kaller funksjonen med (Norway, prisomrade)
priser_no2 <- hent_priser_norge(
  start = "2020-08-01",
  end = "2020-08-31",
  prisomrade = "Norway NO2",
  api_key = api_key
)

View(priser_no2)
saveRDS(
  priser_no2,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/priser_no2.rds")