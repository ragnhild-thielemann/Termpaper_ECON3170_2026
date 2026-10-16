
library(tidyverse)
library(lubridate)
library(dtplyr)

# --------------------------------------------------
# 1. Les inn data og behandle den
# --------------------------------------------------

vannreservoar <- readRDS("Datasett/vannreservoar.rds")

#Da det er flere produksjonsmåter med totalt lavt samlet produksjon, ser vi disse under ett. 
total_produksjon <- readRDS("Datasett/total_produksjon.rds")|>
  lazy_dt()|>
  mutate(produksjonskilde = replace_na(produksjonskilde,"Annet"))|>
  mutate(produksjonskilde = case_when(!produksjonskilde %in% c("Vannkraft med magasin","Elvekraft" ,"Vindkraft pa land" ) ~ "Annet",
                       produksjonskilde %in% c("Vannkraft med magasin","Elvekraft" ,"Vindkraft pa land" )~ produksjonskilde))|>
  summarise(production_MW = mean(production_MW, na.rm = TRUE), .by = c(prisomrade,produksjonskilde,datetime))|>
  arrange(datetime,prisomrade)|>
  as_tibble()

strompris_norge_time <- readRDS(
  "Datasett/strompris_norge_long_time.rds"
)

forventing_mot_forbruk <- readRDS(
  "Datasett/forventing_mot_forbruk_norge_2020.rds"
)

tilsig <- readRDS(
  "Datasett/tilsig_total_denviskalbruke.rds"
)

strompris_europa_time <- readRDS(
  "Datasett/strompris_long_europa_time.rds"
)|>
  summarise(times_pris = mean(pris, na.rm = TRUE),.by = datetime)

# --------------------------------------------------
# 2. Norske timespriser: utgangspunkt for tidsaksen
# --------------------------------------------------

pris_norge <- strompris_norge_time |>
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    dato = as.Date(datetime)
  ) |>
  rename(pris_norge = times_pris) |>
  distinct(datetime, prisomrade, .keep_all = TRUE)


# --------------------------------------------------
# 3. Forbruk: én rad per time og prisområde
# --------------------------------------------------

forbruk_time <- forventing_mot_forbruk |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) |>
  group_by(datetime, prisomrade) |>
  summarise(
    forbruk = mean(faktisk_forbruk_MW, na.rm = TRUE),
    .groups = "drop"
  )


# --------------------------------------------------
# 4. Produksjon: timesoppløst
# --------------------------------------------------
colnames(total_produksjon)
produksjon_time <- total_produksjon |>
  lazy_dt()|>
  mutate(produksjon = production_MW)|>

  mutate(produksjonskilde = if_else(produksjonskilde=="Vannkraft med magasin","Magasinkraftverk",produksjonskilde))|>
  
  pivot_wider(
    names_from = produksjonskilde,
    values_from = produksjon,
    names_prefix = "produksjon_"
  )|>
  as_tibble()

# --------------------------------------------------
# 5. Tilsig: dagsoppløst
# --------------------------------------------------

# Tilsig er dagsoppløst
tilsig_dag <- tilsig 


# --------------------------------------------------
# 6. Magasin: ukesoppløst
# Utvider til dagsnivå og videre til timer
# --------------------------------------------------
magasin_observert <- vannreservoar |>
  mutate(dato = as.Date(datetime)) |>
  group_by(dato, prisomrade) |>
  summarise(
    fyllingsgrad = mean(fyllingsgrad, na.rm = TRUE),
    .groups = "drop"
  )

magasin_dag <- crossing(
  prisomrade = unique(pris_norge$prisomrade),
  dato = seq(
    min(pris_norge$dato, na.rm = TRUE),
    max(pris_norge$dato, na.rm = TRUE),
    by = "day"
  )
) |>
  left_join(
    magasin_observert,
    by = c("dato", "prisomrade")
  ) |>
  arrange(prisomrade, dato) |>
  group_by(prisomrade) |>
  fill(fyllingsgrad, .direction = "down") |>
  ungroup()



# --------------------------------------------------
# 7. Europeiske priser: behold timesnivå
# Lager én kolonne per europeisk prisområde
# --------------------------------------------------

pris_europa <- strompris_europa_time |>
  mutate(datetime = as.POSIXct(datetime, tz = "UTC")) 

colnames(magasin_dag)
# --------------------------------------------------
# 8. Koble sammen til timesformat
# --------------------------------------------------
total_tibble <- pris_norge |>
  left_join(
    forbruk_time,
    by = c("datetime", "prisomrade")
  ) |>
  left_join(
    produksjon_time,
    by = c("datetime", "prisomrade")
  ) |>
  mutate(dato = as.Date(datetime))|>
  left_join(
    tilsig_dag,
    by = c("dato", "prisomrade")
  ) |>
  left_join(
    magasin_dag,
    by = c("dato", "prisomrade")
  ) 
  arrange(prisomrade, datetime)

# --------------------------------------------------
# 9. Kontroller resultatet
# --------------------------------------------------

glimpse(total_tibble)

nrow(total_tibble)

summary(total_tibble)

# --------------------------------------------------
# 10. Lagre resultatet
# --------------------------------------------------

saveRDS(total_tibble,
        "Datasett/total_tibble3.rds")