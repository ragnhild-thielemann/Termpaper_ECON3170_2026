library(dtplyr)
library(dplyr)

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteforventing_produksjo_ENTSO-E.R")

prissoner <- c("NO1", "NO2", "NO3", "NO4", "NO5")

forbruk_norge_2020 <- tibble()

for (sone in prissoner) {
  
  forbruk_sone <- hent_forbruk_ENTSOE(
    start_dato = "2020-01-01",
    slutt_dato = Sys.Date(),
    prisomrade = paste("Norway", sone),
    variabel = "A16",
    api_key = api_key
  ) |>
    mutate(prisomrade = sone) 
  
  if (nrow(forbruk_norge_2020) == 0) {
    forbruk_norge_2020 <- forbruk_norge_2020
  } else {
    forbruk_norge_2020 <- forbruk_norge_2020 |>
      bind_rows(forbruk_norge_2020)
  }
}

forbruk_norge_2020 <- forbruk_norge |>
  lazy_dt() |>
  arrange(datetime) |>
  as_tibble()

saveRDS(
  forbruk_norge_2020,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/forventing_mot_forbruk_norge_2020.rds"
)