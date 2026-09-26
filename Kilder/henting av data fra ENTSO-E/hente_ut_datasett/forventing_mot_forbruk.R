
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteforventing_produksjo_ENTSO-E.R")


prissoner <- c("NO1","NO2","NO3","NO4","NO5")
forbruk_norge <- tibble()


for (sone in prissoner){
  
  forbruk_sone <- hent_forbruk_ENTSOE(
    start_dato = "2026-07-1",
    slutt_dato = Sys.Date(),
    prisomrade= paste("Norway",sone),
    variabel = "A16",
    api_key = api_key
  )|>
    mutate(prisomrade = sone) #legger ved en kolonne som forklarer prisområdet vi oppererer i
  if(nrow(forbruk_norge) == 0){
    forbruk_norge <- forbruk_sone
  } else { #legger ved nye observasjoner under datasettet
    forbruk_norge <- forbruk_norge |>
      bind_rows(forbruk_sone)
  }
  forbruk_norge <- forbruk_norge |>
    arrange(datetime) #gir dem i fallende rekkefølge
  }


saveRDS(
  forbruk_norge,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/forventing_mot_forbruk_norge.rds")

View(forbruk_norge)
