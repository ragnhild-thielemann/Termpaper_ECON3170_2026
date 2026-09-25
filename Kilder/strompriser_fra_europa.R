# ============================================================
# HENT EUROPEISKE STRØMPRISER FRA ENTSO-E
# Fra 2020-01-01 til dagens dato
#
# Data:
#   Day-ahead electricity prices
#   Enhet: EUR/MWh
#
# API:
#   ENTSO-E Transparency Platform
# ============================================================


library(httr2)
library(xml2)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(tibble)
library(readr)


# ------------------------------------------------------------
# 2. API-nøkkel
# ------------------------------------------------------------
#henter inn filen med API-nøkkelen
readRenviron("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/.Renviron")

api_key <- Sys.getenv("europa") #henter ut nøkkelen

if (api_key == "") { #gir oss feilmelding dersom R ikke finner en nøkkel, med instruks om at åpning av strømdata krever lisens
  stop(
    "Fant ingen ENTSOE_TOKEN.\n",
    "Legg API-nøkkelen i .Renviron som:\n",
    'ENTSOE_TOKEN="din_nøkkel"'
  )
}


message("API-nøkkel er funnet, vi skal nå hente ut dataene")

# ------------------------------------------------------------
# 2. Angi testperiode og område
# ------------------------------------------------------------

start <- as.Date("2024-08-01")
end   <- as.Date("2024-08-01")

country <- "Austria"
zone <- "AT"

# EIC-kode for Østerrike
eic_code <- "10YAT-APG------L"


# ------------------------------------------------------------
# 3. Lag tidsformatet ENTSO-E krever
# ------------------------------------------------------------

period_start <- paste0(
  format(start, "%Y%m%d"),
  "0000"
)

# Vi setter sluttidspunktet til starten av neste dag.
# Da får vi med hele 1. august.
period_end <- paste0(
  format(end + 1, "%Y%m%d"),
  "0000"
)

message(
  "Henter ", country, " ", zone, ": ",
  start, " -> ", end
)

message("periodStart = ", period_start)
message("periodEnd   = ", period_end)


# ------------------------------------------------------------
# 4. Send forespørselen til ENTSO-E
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# 5. Sjekk at API-et svarte
# ------------------------------------------------------------

message("HTTP-status: ", resp_status(response))

if (resp_status(response) != 200) {
  stop("API-kallet feilet.")
}


# ------------------------------------------------------------
# 6. Les XML-responsen
# ------------------------------------------------------------

xml <- resp_body_xml(response)


# ------------------------------------------------------------
# 7. Finn TimeSeries
# ------------------------------------------------------------

time_series <- xml_find_all(
  xml,
  ".//*[local-name()='TimeSeries']"
)

message(
  "Antall tidsserier funnet: ",
  length(time_series)
)

if (length(time_series) == 0) {
  stop("Fant ingen TimeSeries i API-responsen.")
}


# ------------------------------------------------------------
# 8. Finn tidsintervallet
# ------------------------------------------------------------

period_start_xml <- xml_text(
  xml_find_first(
    time_series[[1]],
    ".//*[local-name()='period.timeInterval']/*[local-name()='start']"
  )
)

period_end_xml <- xml_text(
  xml_find_first(
    time_series[[1]],
    ".//*[local-name()='period.timeInterval']/*[local-name()='end']"
  )
)

resolution <- xml_text(
  xml_find_first(
    time_series[[1]],
    ".//*[local-name()='resolution']"
  )
)

message("XML start: ", period_start_xml)
message("XML slutt: ", period_end_xml)
message("Oppløsning: ", resolution)


# ------------------------------------------------------------
# 9. Finn alle prisobservasjonene
# ------------------------------------------------------------

points <- xml_find_all(
  time_series[[1]],
  ".//*[local-name()='Point']"
)

message(
  "Antall prisobservasjoner: ",
  length(points)
)


# ------------------------------------------------------------
# 10. Hent position og price.amount
# ------------------------------------------------------------

position <- as.integer(
  xml_text(
    xml_find_all(
      points,
      ".//*[local-name()='position']"
    )
  )
)

price <- as.numeric(
  xml_text(
    xml_find_all(
      points,
      ".//*[local-name()='price.amount']"
    )
  )
)


# ------------------------------------------------------------
# 11. Lag dataframe
# ------------------------------------------------------------

priser <- tibble(
  position = position,
  price = price,
  country = country,
  zone = zone
)


# ------------------------------------------------------------
# 12. Se resultatet
# ------------------------------------------------------------

print(priser)

message("Antall observasjoner: ", nrow(priser))

summary(priser$price)