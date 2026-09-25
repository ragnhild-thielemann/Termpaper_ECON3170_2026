



source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_hentedata_ENTSO-E.R")


# Henter ut strømprisene i Norge fra 2020 til 2026

prissoner <- c("NO1","NO2","NO3","NO4","NO5")
strompris_norge <- tibble()


for (sone in prissoner){
  
  priser_sone <- hent_markedspriser(
    start_dato = "2026-08-01",
    slutt_dato = Sys.Date(),
    prisomrade= paste("Norway",sone),
    variabel = "A44",
    api_key = api_key
  )|>
    dplyr::select(datetime, price)|>
    rename(!!paste0("pris_", sone) := price)
  
  View(priser_sone)
  if (nrow(pris_total) == 0) {
    pris_total <- priser_sone
  } else {
    pris_total <- pris_total |>
      left_join(
        priser_sone,
        by = "datetime"
      )
  }}
  
View(strom_norge)


strompris_norge <- pris_total
#lagrer hele datafilen som en RDS-fil på datamaskinen
saveRDS(
  pris_total,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_norge.rds")





kraftflyt_no2_no3 <- hent_markedspriser(
  start_dato = "2026-08-01",
  slutt_dato = Sys.Date(),
  prisomrade_in = "Norway NO2",
  prisomrade_out = "Norway NO1",
  variabel = "A11",
  api_key = api_key
)

View(kraftflyt_no2_no3)

time_series <- xml2::xml_find_all(
  xml,
  ".//*[local-name()='TimeSeries']"
)

library(httr2)

response_test <- httr2::request(
  "https://web-api.tp.entsoe.eu/api"
) |>
  httr2::req_url_query(
    securityToken = api_key,
    documentType = "A11",
    in_Domain = "10YNO-2--------T",
    out_Domain = "10YNO-3--------J",
    periodStart = "202608010000",
    periodEnd = "202609010000"
  ) |>
  httr2::req_perform()

httr2::resp_status(response_test)
httr2::resp_body_string(response_test)