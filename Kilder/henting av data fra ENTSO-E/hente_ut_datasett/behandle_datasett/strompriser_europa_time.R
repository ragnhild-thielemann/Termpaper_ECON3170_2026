
library(tidyverse)
library(dtplyr)
library(data.table)

#' En nærmere titt på strømdataen fra Europa, viser at den går fra å være ha tids
#' intervaller på 15-minutter, til å være aggregert på timesnivå. Dette gjør dataettet
#' klønete å jobbe med. Derfor bør vi aggregere hele datasettet på timesnivå, slik at
#' alt er på samme format. Dette er noe ugunstig, men det beste alternativet vi har. 
#' 



strompris_europa_long <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_long_europa.rds"
)


strompris_europa_time <- strompris_europa_long|>
  lazy_dt()|>
  mutate(datetime = floor_date(datetime,"hour"))|>
  summarise(pris = mean(pris, na.rm =TRUE), .by = c(datetime, prisomrade)
            )|>
  as_tibble()


strompris_europa_long |> #finner manglende observasjoner
  group_by(prisomrade) |>
  summarise(
    antall = n(),
    manglende_priser = sum(is.na(pris)),
    andel_manglende = mean(is.na(pris)),
    .groups = "drop"
  )


strompris_europa_time |> #finner manglende observasjoner
  group_by(prisomrade) |>
  summarise(
    antall = n(),
    manglende_priser = sum(is.na(pris)),
    andel_manglende = mean(is.na(pris)),
    .groups = "drop"
  )

#' Ved å aggregere strømprisen til timespriser, gjør det at antall manglende obsrevasjoner faller dramatisk. 

saveRDS(strompris_europa_time,
        "Datasett/strompris_long_europa_time.rds")
