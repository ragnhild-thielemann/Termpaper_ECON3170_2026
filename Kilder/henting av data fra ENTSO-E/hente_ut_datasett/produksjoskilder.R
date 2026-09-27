
library(tidyverse)
library(tibble)

source(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_produksjonstype.R"
)


# Oversikt mellom PSR-kode og produksjonstype
psr_oversikt <- tibble(
  kode = c(
    "B04", "B11", "B12",
    "B15", "B16", "B17", "B18"
  ),
  navn = c(
    "Gass",
    "Elvekraft",
    "Vannkraft med magasin",
    "Annen fornybar",
    "Solkraft",
    "Vindkraft til havs",
    "Vindkraft pa land"
  )
)


prisomrader <- c(
  "NO1", "NO2", "NO3", "NO4", "NO5"
)


#Vektorisert kode er mer effektivt enn å oppdatere tibbelen konstant. 

resultater <- vector("list", length(prisomrader))

for (i in seq_along(prisomrader)) {
  
  sone <- prisomrader[i]
  
  resultater[[i]] <- hent_produksjon_ENTSOE(
    start_dato = "2026-08-20",
    slutt_dato = Sys.Date(),
    prisomrade = paste("Norway", sone),
    api_key = api_key,
    psr_type = NULL
  )
}

total_produksjon <- bind_rows(resultater) |>
  drop_na() |>
  mutate(
    datetime = as.Date(datetime)
  ) |>
  left_join(
    psr_oversikt,
    by = c("psr_type" = "kode")
  ) |>
  rename(
    produksjonskilde = navn
  )

saveRDS(
  total_produksjon,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/total_produksjon.rds"
)
