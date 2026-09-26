
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteforventing_produksjo_ENTSO-E.R")

No1 <- hent_forbruk_ENTSOE(
  start_dato = "2026-09-01",
  slutt_dato = Sys.Date(),
  prisomrade = "Norway NO1",
  api_key = api_key ) 


View(No1)


