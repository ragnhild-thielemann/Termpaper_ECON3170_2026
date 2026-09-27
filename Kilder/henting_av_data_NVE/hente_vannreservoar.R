

library(tidyverse)
library(lubridate)

source(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting_av_data_NVE/funksjon_henta_data.R"
)

vannreservoar <- hent_vannreservoar(start_date = "2020-09-02")|>
  mutate(datetime = as.Date(datetime)) |> #noen uker har to observasjoner. Jeg finner gjennomsnittet av disse observasjonene, for å gjøre datasette enklere å jobbe med vidre
  summarise(fyllingsgrad = mean(fyllingsgrad, na.rm = TRUE),
            fyllingsgrad_forrige_uke = mean(fyllingsgrad_forrige_uke, na.rm = TRUE),
            endring = mean(endring , na.rm = TRUE), .by = c(datetime, prisomrade)
            )

saveRDS(
  vannreservoar,
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/vannreservoar.rds"
)
