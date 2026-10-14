
library(dtplyr)
library(dplyr)

source("Kilder/henting av data fra ENTSO-E/funksjon_henteforventing_produksjo_ENTSO-E.R")


# ==========================================================
# 1. Prisområder
# ==========================================================

prissoner <- c("NO1", "NO2", "NO3", "NO4", "NO5")


# ==========================================================
# 2. Hent data for alle prisområder
# ==========================================================

forbruk_norge_2020 <- lapply(
  prissoner,
  function(sone) {
    
    hent_forbruk_ENTSOE(
      start_dato = "2020-01-01",
      slutt_dato = Sys.Date(),
      prisomrade = paste("Norway", sone),
      variabel = "A16",
      api_key = api_key
    ) |>
      mutate(
        prisomrade = sone
      )
  }
) |>
  bind_rows()


# ==========================================================
# 3. Behandle data med dtplyr
# ==========================================================

forbruk_norge_2020 <- forbruk_norge_2020 |>
  lazy_dt() |>
  
  # Sorter etter prisområde og tidspunkt
  arrange(prisomrade, datetime) |>
  
  # Fjern eventuelle duplikater
  distinct(
    prisomrade,
    datetime,
    .keep_all = TRUE
  ) |>
  
  # Materialiser resultatet som tibble
  as_tibble()


# ==========================================================
# 4. Vis resultatet
# ==========================================================

View(forbruk_norge_2020)


forventing_mot_forbruk_norge_2020_time <-
  forbruk_norge_2020 |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    faktisk_forbruk = mean(faktisk_forbruk_MW), prognose_forbruk = mean(prognose_forbruk_MW),
    .by = c(datetime, prisomrade)
  ) |>
  drop_na()|>
  as_tibble()
# ==========================================================
# 5. Lagre datasettet
# ==========================================================

saveRDS(
  forventing_mot_forbruk_norge_2020_time,
  "Datasett/forventing_mot_forbruk_time.rds"
)


