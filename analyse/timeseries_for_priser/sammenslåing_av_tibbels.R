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
  
  #vi trenger ikke lengre produksjonskilden, fordi den eneste produksjonskilden er spesifikt vannraft
  select(-produksjonskilde)|>
  as_tibble()

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



p1 <- total_tibble |>
  filter(prisomrade == "NO2")|>
  ggplot(aes(x = times_pris, y = total_produksjon)) +
  geom_point()


p1

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

p3 <- vannresevorar_med_produksjon |>
  lazy_dt()|>
  mutate(andel = vannkraft/total_produksjon) |>
  filter(prisomrade == "NO2")|>
  pivot_longer(cols = c(andel,fyllingsgrad),
               names_to = "variabel",
               values_to = "verdi")|>
  as_tibble()|>
  ggplot(aes(x = datetime, y = verdi, color = variabel)) + 
  geom_smooth()



p3

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

strompris_dag <- strompris_norge_long_fra_2020 |>
  mutate(datetime = as.Date(datetime))|>
  summarise(pris = mean(pris_modellert, na.rm = TRUE), .by = c(datetime, prisomrade))

regn_produksjon_pris <- total_produksjon|>
  
  #må gjøre til dagsintervaller, for at det skal passe med datasettet for nedbør

  mutate(datetime = as.Date(datetime))|>
  summarise(produksjon = mean(production_MW), .by = c(datetime, prisomrade,produksjonskilde))|>
  left_join(nedbor_total, by = join_by(datetime == dato, prisomrade) )|>
  left_join(strompris_dag, by = join_by(datetime, prisomrade))|>
  mutate(produksjonskilde = replace_na(produksjonskilde,"ukjent"))



View(regn_produksjon)


## Setter opp en linjær maskinlæringsmodell, for å bruke en paramter til estimeringen. finner da en korrelasjonskoefesient

set.seed(67)
library(workflows)
library(tidymodels)
library(tidyverse)

#deler i test og treningssample
delt_elv = initial_split(regn_produksjon_pris|>
                           filter(produksjonskilde == "Elvekraft")|>
                           select(-produksjonskilde),
                          0.8, strata = prisomrade )
trening_elv = training(delt_elv)
test_elv= testing(delt_elv)

#deler i test og treningssample
delt_magasin= initial_split(regn_produksjon_pris|>
                           filter(produksjonskilde == "Vannkraft med magasin")|>
                           select(-produksjonskilde),
                         0.8, strata = prisomrade )
trening_magasin = training(delt_magasin)
test_magasin= testing(delt_magasin)


linear_model <- linear_reg()|>
  set_engine("lm")



rec_regn_magasin <- recipe(produksjon ~ nedbor, trening_magasin)
rec_regn_elv <- recipe(produksjon ~ nedbor, trening_elv)



wf_magasin <- workflow()|>
  add_recipe(rec_regn_magasin)|>
  add_model(linear_model)

wf_elv <- workflow()|>
  add_recipe(rec_regn_elv)|>
  add_model(linear_model)

fit_elv <- fit(wf_elv,test_elv)

summary(
  extract_fit_engine(fit_elv)
)

fit_magasin <- fit(wf_magasin,test_magasin)

summary(
  extract_fit_engine(fit_magasin)
)


## lager modell med akkumulert nedbør (da r-verdien er svært lav)

