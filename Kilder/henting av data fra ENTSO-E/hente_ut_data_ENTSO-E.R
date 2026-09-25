#Struktur = 
# Hvis i norge, hent inn tilleggsinformasjonen om prisområde. Du kaller funksjonen med (Norway, prisomrade)
priser_no2 <- hent_markedspriser(
  start_dato = "2020-08-01",
  slutt_dato = "2020-08-31",
  prisomrade = "Norway NO2",
  variabel = "A44",
  api_key = api_key
)


View(priser_no2)
saveRDS(
  priser_no2,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/priser_no2.rds")