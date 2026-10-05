# Importerer bibliotekene
library(httr)
library(jsonlite)
library(lubridate)
library(zoo)
library(rugarch)
library(plm)
library(vars)
library(tidyverse)
library(data.table)
library(dtplyr)
library(scales)


# Leser inn datasett
vannreservoar <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/vannreservoar.rds"
)
total_produksjon <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/total_produksjon.rds"
)

strompris_norge_long_fra_2020 <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/strompris_norge_long_fra_2020.rds"
)

forventing_mot_forbruk_norge_2020 <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/forventing_mot_forbruk_norge_2020.rds"
)

nedbor_total <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/nedbor_total.rds"
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


# Gjør produksjonen om til timesdata
total_produksjon_time <-
  total_produksjon |>
  lazy_dt() |>
  mutate(
    datetime = floor_date(datetime, "hour")
  ) |>
  summarise(
    timeproduksjon = mean(production_MW, na.rm = TRUE),
    .by = c(datetime, produksjonskilde, prisomrade)
  ) |>
  mutate(total_produksjon = sum(timeproduksjon, na.rm = TRUE),.by = c(datetime,prisomrade))|>
  filter(produksjonskilde == "Vannkraft med magasin") |>
  rename(vannkraft = timeproduksjon   )|>
  dplyr::select(-produksjonskilde)|>
  #vi trenger ikke lengre produksjonskilden, fordi den eneste produksjonskilden er spesifikt vannraft
  as_tibble()

View(total_produksjon_time)
# Gjør forbruket om til timesdata

forventing_mot_forbruk_norge_2020_time <-
  forventing_mot_forbruk_norge_2020 |>
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

#lager en stor tibble, der jeg lagrer alle variablene i en tibbel. Dette gjør analysen senere murlig
total_tibble <- total_produksjon_time |>
  lazy_dt() |>
  left_join(
    strompris_norge_long_fra_2020_time,
    by = c("datetime", "prisomrade")
  ) |>
  left_join(
    forventing_mot_forbruk_norge_2020_time,
    by = c("datetime", "prisomrade")
  ) |>
  as_tibble()




vannreservoar <- vannreservoar |>
  mutate(datetime = as.Date(datetime))

colnames(total_produksjon_time)
vannresevorar_med_produksjon <- total_produksjon_time |>
  lazy_dt()|>
  
  #' Gjør produksjonen om til dagsproduksjon, da dataen fra NVE har så lav oppløsning
  mutate(datetime = as.Date(floor_date(datetime,"day")))|>
  summarise(vannkraft = mean(vannkraft, na.rm = TRUE), total_produksjon = mean(total_produksjon, na.rm = TRUE), .by = c(datetime, prisomrade))|>
  left_join(vannreservoar, by = c("datetime", "prisomrade"))|>
  
  #'da det bare er ukesobservasjoner for fyllingsgraden i magasinene, må vi 
  #'fylle ut kolnnene nedover
  fill(fyllingsgrad,fyllingsgrad_forrige_uke,endring,.direction  = "down")|>
  drop_na()|>
  as_tibble()






p4 <- vannresevorar_med_produksjon |>
  mutate(andel = vannkraft/total_produksjon) |>
  ggplot(aes(x = datetime, y = andel, color = prisomrade)) + 
  geom_point()

p4

# vi ser at andelen har falt dramatisk etter 2020. 
#andelen vannkraft i prisområde 1 har falt dramatisk

colnames(total_produksjon)
p5 <- total_produksjon |>
  lazy_dt()|>
  filter(prisomrade == "NO1")|>
  mutate(datetime = floor_date(datetime,"day"))|>
  summarise(produksjon_dag = mean(production_MW), 
            .by = c(datetime, produksjonskilde))|>

  as_tibble()|>
  ggplot(aes(x = datetime, y = produksjon_dag, fill =produksjonskilde ))+
  geom_area()

(p5)


### Vi vil finne om elvekraft i større grad er korrelert med nedbør

colnames(total_produksjon)
colnames(nedbor_total)
colnames(vannreservoar)
strompris_dag <- strompris_norge_long_fra_2020 |>
  mutate(datetime = as.Date(datetime))|>
  summarise(pris = mean(pris_modellert, na.rm = TRUE), .by = c(datetime, prisomrade))

summary(vannreservoar)
library(janitor)



### Redigerer total_produksjon, slik at den er bedre å jobbe med. 
unique(total_produksjon$produksjonskilde)
total_produksjon_jobbing <- total_produksjon|>
  lazy_dt()|>
  mutate(produksjonskilde = case_when(produksjonskilde == "Vannkraft med magasin" ~ "Vannkraft med magasin",
                                      produksjonskilde== "Elvekraft"~"Elvekraft",
                                      produksjonskilde == "Vindkraft pa land" ~ "Vindkraft pa land",
                                      .default = "Andre produksjonskilder"))|>
  summarise(produksjon = mean(production_MW, na.rm = TRUE), .by = c(datetime, prisomrade,produksjonskilde))|>
  as_tibble()



