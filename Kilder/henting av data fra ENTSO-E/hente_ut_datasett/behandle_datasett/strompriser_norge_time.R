
library(tidyverse)
library(dtplyr)
strompris_norge_long_fra_2020 <- readRDS(
  "Datasett/strompris_norge_long_fra_2020.rds"
)
# Gjør strømprisen om til timesdata
strompris_norge_long_fra_2020_time <-
  strompris_norge_long_fra_2020 |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    times_pris = mean(pris_modellert),
    .by = c(datetime, prisomrade)
  ) |>
  drop_na()|>
  as_tibble()

saveRDS(
  strompris_norge_long_fra_2020_time,
  "Datasett/strompris_norge_long_time.rds")