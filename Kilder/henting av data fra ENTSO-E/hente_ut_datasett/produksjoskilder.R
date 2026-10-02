library(docstring)
library(tidyverse)
library(dtplyr)
library(data.table)

source("Kilder/henting av data fra ENTSO-E/funksjon_produksjonstype.R")


#Lager en oversikt over de ulike produksjonskildene for kraft
psr_oversikt <- tibble(
  psr_type = c("B01","B04","B10", "B11", "B12",
     "B15","B16", "B17","B18", "B19","B20"),
  produksjonskilde = c(
    "Biomasse",
    "Gass",
    "Pumpekraft",
    "Elvekraft",
    "Vannkraft med magasin",
    "Annen fornybar",
    "Solkraft",
    "Avfall",
    "Vindkraft til havs",
    "Vindkraft pa land",
    "Annet"
    )
)


View(psr_oversikt)
prisomrader <- c(
  "NO1", "NO2", "NO3", "NO4", "NO5"
)


# ------------------------------------------------------------
# Henter ut dataene
# ------------------------------------------------------------

resultater <- purrr::map(
  prisomrader,
  \(sone) {
    
    hent_produksjon_ENTSOE(
      start_dato = "2020-01-01",
      slutt_dato = Sys.Date(),
      prisomrade = paste("Norway", sone),
      api_key = api_key,
      psr_type = NULL #går over alle prisområdene som er gitt i funksjonen. 
    )
  }
)

#'Da den samme operasjon skal utføres for hvert prisområde, 
#'bruker vi purrr::map() til å gjennomføre funksjonen for 
#'hvert element i vektoren.


total_produksjon <- bind_rows(resultater) |>
  #gjør om til data.frame, slik at vi kan bruke dtplyr
  lazy_dt() |>
  
  # Fjern rader uten produksjonsdata
  filter(!is.na(production_MW)) |>

  # Koble PSR-kode til produksjonskilde
  left_join(
    lazy_dt(psr_oversikt),
    by = "psr_type") |>
  
  # Sorter datasettet
  arrange(datetime, prisomrade, psr_type) |> 
  
  # Gjør tilbake til tibble
  as_tibble()



#lagrer datasettet
saveRDS(
  total_produksjon,
  "Datasett/total_produksjon.rds")

View(total_produksjon)
