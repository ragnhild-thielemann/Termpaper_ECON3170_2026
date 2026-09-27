

library(tidyverse)
library(lubridate)

source(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting_av_data_NVE/funksjon_henta_data.R"
)

vannreservoar <- hent_vannreservoar(start_date = "2020-09-02")

saveRDS(
  vannreservoar,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/vannreservoar.rds"
)
