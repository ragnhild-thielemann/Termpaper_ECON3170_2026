# ============================================================
# ENTSO-E strømpriser
# Test: Østerrike, 1. august 2024
# ============================================================

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


# ------------------------------------------------------------
# 2. Innstillinger
# ------------------------------------------------------------

start <- as.Date("2024-08-01")
end   <- as.Date("2024-08-01")

country <- "Austria"
zone <- "AT"

# EIC-kode for Østerrike
eic_code <- "10YAT-APG------L"


# ------------------------------------------------------------
# 3. Lag tidspunkt for API-kallet
# ------------------------------------------------------------

# ENTSO-E bruker formatet YYYYMMDDHHMM.
#
# Vi vil ha hele 1. august:
#
# start = 2024-08-01 00:00
# end   = 2024-08-02 00:00

period_start <- paste0(
  format(start, "%Y%m%d"),
  "0000"
)

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
# 4. Send forespørsel til ENTSO-E
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
# 5. Kontroller at API-kallet fungerte
# ------------------------------------------------------------

if (resp_status(response) != 200) {
  stop(
    "API-kallet feilet. HTTP-status: ",
    resp_status(response)
  )
}

message("API-kall OK")


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

if (length(time_series) == 0) {
  stop("Fant ingen TimeSeries i API-responsen.")
}

message(
  "Antall TimeSeries: ",
  length(time_series)
)


# Vi bruker den første TimeSeries i denne testen
ts <- time_series[[1]]


# ------------------------------------------------------------
# 8. Hent tidsintervallet
# ------------------------------------------------------------

# Viktig:
# start og end ligger direkte under TimeSeries-strukturen,
# så vi bruker local-name()='start' og local-name()='end'.

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


# ------------------------------------------------------------
# 9. Hent oppløsningen
# ------------------------------------------------------------

resolution <- xml_text(
  xml_find_first(
    ts,
    ".//*[local-name()='resolution']"
  )
)


message("Start:      ", interval_start)
message("Slutt:      ", interval_end)
message("Oppløsning: ", resolution)


# ------------------------------------------------------------
# 10. Gjør oppløsningen om til minutter
# ------------------------------------------------------------

minutes <- case_when(
  resolution == "PT15M" ~ 15,
  resolution == "PT30M" ~ 30,
  resolution == "PT60M" ~ 60,
  resolution == "PT1H"  ~ 60,
  TRUE ~ NA_real_
)

if (is.na(minutes)) {
  stop(
    "Ukjent oppløsning fra ENTSO-E: ",
    resolution
  )
}

message(
  "Antall minutter per observasjon: ",
  minutes
)


# ------------------------------------------------------------
# 11. Finn alle prisobservasjonene
# ------------------------------------------------------------

points <- xml_find_all(
  ts,
  ".//*[local-name()='Point']"
)

message(
  "Antall prisobservasjoner: ",
  length(points)
)


# ------------------------------------------------------------
# 12. Hent position
# ------------------------------------------------------------

position <- as.integer(
  xml_text(
    xml_find_all(
      points,
      ".//*[local-name()='position']"
    )
  )
)


# ------------------------------------------------------------
# 13. Hent pris
# ------------------------------------------------------------

price <- as.numeric(
  xml_text(
    xml_find_all(
      points,
      ".//*[local-name()='price.amount']"
    )
  )
)


# ------------------------------------------------------------
# 14. Kontroller at position og pris har samme lengde
# ------------------------------------------------------------

if (length(position) != length(price)) {
  stop(
    "Antall positioner (",
    length(position),
    ") er forskjellig fra antall priser (",
    length(price),
    ")."
  )
}


# ------------------------------------------------------------
# 15. Gjør starttidspunktet om til POSIXct
# ------------------------------------------------------------

start_datetime <- as.POSIXct(
  interval_start,
  format = "%Y-%m-%dT%H:%MZ",
  tz = "UTC"
)

if (is.na(start_datetime)) {
  stop(
    "Klarte ikke å tolke starttidspunktet: ",
    interval_start
  )
}


# ------------------------------------------------------------
# 16. Lag tidspunkt for hver observasjon
# ------------------------------------------------------------

datetime <- start_datetime +
  minutes * 60 * (position - 1)


# ------------------------------------------------------------
# 17. Lag datasett
# ------------------------------------------------------------

priser <- tibble(
  datetime = datetime,
  country = country,
  zone = zone,
  price = price,
  position = position
)


# ------------------------------------------------------------
# 18. Vis de første observasjonene
# ------------------------------------------------------------

print(
  priser,
  n = 20
)


# ------------------------------------------------------------
# 19. Oppsummer datasettet
# ------------------------------------------------------------

message("")
message("========== OPPSUMMERING ==========")

message(
  "Antall observasjoner: ",
  nrow(priser)
)

message(
  "Første tidspunkt (UTC): ",
  min(priser$datetime, na.rm = TRUE)
)

message(
  "Siste tidspunkt (UTC):  ",
  max(priser$datetime, na.rm = TRUE)
)

message(
  "Laveste pris: ",
  min(priser$price, na.rm = TRUE)
)

message(
  "Høyeste pris: ",
  max(priser$price, na.rm = TRUE)
)


# ------------------------------------------------------------
# 20. Kontroller positionene
# ------------------------------------------------------------

message("")
message("========== POSITION ==========")

message(
  "Laveste position: ",
  min(position)
)

message(
  "Høyeste position: ",
  max(position)
)

message(
  "Antall positioner: ",
  length(position)
)

message(
  "Manglende positioner: ",
  sum(is.na(position))
)

message(
  "Manglende priser: ",
  sum(is.na(price))
)