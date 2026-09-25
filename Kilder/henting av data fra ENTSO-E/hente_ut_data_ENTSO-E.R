



source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_hente_data_ENTSO-E.R")
ls()
priser_no2 <- hent_markedspriser(
  start_dato = "2020-08-01",
  slutt_dato = "2020-08-31",
  prisomrade = "Norway NO1",
  variabel = "A44",
  api_key = api_key
)
print("hei")
priser_no2


prissoner <- c("NO1","NO2","NO3","NO4","NO5")
pris_total <- tibble()


for (sone in prissoner){
  
  priser_sone <- hent_markedspriser(
    start_dato = "2020-08-01",
    slutt_dato = Sys.Date(),
    prisomrade = paste("Norway",sone),
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
  
glimpse(pris_total)


#lagrer hele datafilen som en RDS-fil på datamaskinen
saveRDS(
  pris_total,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/pris_total.rds")