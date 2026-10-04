library(tidyverse)
library(dtplyr)
library(tidymodels)

# ---------------------------------------------------------
# 1. Del data i trenings- og testsett
# ---------------------------------------------------------
set.seed(67)
delt_elv <- initial_split(
  regn_produksjon_pris |>
    filter(produksjonskilde == "Elvekraft") |>
    select(-produksjonskilde),
  prop = 0.8,
  strata = prisomrade
)

trening_elv <- training(delt_elv)
test_elv <- testing(delt_elv)


delt_magasin <- initial_split(
  regn_produksjon_pris |>
    filter(produksjonskilde == "Vannkraft med magasin") |>
    select(-produksjonskilde),
  prop = 0.8,
  strata = prisomrade
)

trening_magasin <- training(delt_magasin)
test_magasin <- testing(delt_magasin)


# ---------------------------------------------------------
# 2. Lag lineær modell
# ---------------------------------------------------------

linear_model <- linear_reg() |>
  set_engine("lm")


# ---------------------------------------------------------
# 3. Funksjon for å lage lagget og akkumulert nedbør
# ---------------------------------------------------------

lagg_nedbor <- function(data) {
  
  data |>
    arrange(prisomrade, datetime) |>
    lazy_dt() |>
    group_by(prisomrade) |>
    mutate(
      
      # Nedbør 1, 7, 14 og 30 dager tidligere
      nedbor_lag1 = shift(nedbor, 1),
      nedbor_lag7 = shift(nedbor, 7),
      nedbor_lag14 = shift(nedbor, 14),
      nedbor_lag30 = shift(nedbor, 30),
      
      # Akkumulert nedbør
      nedbor_7d = frollsum(nedbor, 7, align = "right"),
      nedbor_14d = frollsum(nedbor, 14, align = "right"),
      nedbor_30d = frollsum(nedbor, 30, align = "right")
    ) |>
    collect()
}


# ---------------------------------------------------------
# 4. Lag variablene
# ---------------------------------------------------------

trening_elv <- lagg_nedbor(trening_elv)
test_elv <- lagg_nedbor(test_elv)

trening_magasin <- lagg_nedbor(trening_magasin)
test_magasin <- lagg_nedbor(test_magasin)


# ---------------------------------------------------------
# 5. Modell med nedbør samme dag
# ---------------------------------------------------------

rec_elv <- recipe(
  produksjon ~ nedbor + pris,
  data = trening_elv
)

rec_magasin <- recipe(
  produksjon ~ nedbor + pris,
  data = trening_magasin
)


wf_elv <- workflow() |>
  add_recipe(rec_elv) |>
  add_model(linear_model)

wf_magasin <- workflow() |>
  add_recipe(rec_magasin) |>
  add_model(linear_model)


# Tilpass modellene på treningsdata
fit_elv <- fit(wf_elv, trening_elv)

fit_magasin <- fit(wf_magasin, trening_magasin)


# ---------------------------------------------------------
# 6. Se summary
# ---------------------------------------------------------

summary(
  extract_fit_engine(fit_elv)
)

summary(
  extract_fit_engine(fit_magasin)
)


# ---------------------------------------------------------
# 7. Modell med 7 dagers akkumulert nedbør
# ---------------------------------------------------------

rec_elv_7d <- recipe(
  produksjon ~ nedbor_7d + pris,
  data = trening_elv
)

rec_magasin_7d <- recipe(
  produksjon ~ nedbor_7d + pris,
  data = trening_magasin
)


wf_elv_7d <- workflow() |>
  add_recipe(rec_elv_7d) |>
  add_model(linear_model)

wf_magasin_7d <- workflow() |>
  add_recipe(rec_magasin_7d) |>
  add_model(linear_model)


fit_elv_7d <- fit(wf_elv_7d, trening_elv)

fit_magasin_7d <- fit(wf_magasin_7d, trening_magasin)


# Summary
summary(
  extract_fit_engine(fit_elv_7d)
)

summary(
  extract_fit_engine(fit_magasin_7d)
)


# ---------------------------------------------------------
# 8. Modell med 14 dagers akkumulert nedbør
# ---------------------------------------------------------

rec_elv_14d <- recipe(
  produksjon ~ nedbor_14d + pris,
  data = trening_elv
)

rec_magasin_14d <- recipe(
  produksjon ~ nedbor_14d + pris,
  data = trening_magasin
)


wf_elv_14d <- workflow() |>
  add_recipe(rec_elv_14d) |>
  add_model(linear_model)

wf_magasin_14d <- workflow() |>
  add_recipe(rec_magasin_14d) |>
  add_model(linear_model)


fit_elv_14d <- fit(wf_elv_14d, trening_elv)

fit_magasin_14d <- fit(wf_magasin_14d, trening_magasin)


summary(
  extract_fit_engine(fit_elv_14d)
)

summary(
  extract_fit_engine(fit_magasin_14d)
)


# ---------------------------------------------------------
# 9. Modell med 30 dagers akkumulert nedbør
# ---------------------------------------------------------

rec_elv_30d <- recipe(
  produksjon ~ nedbor_30d + pris,
  data = trening_elv
)

rec_magasin_30d <- recipe(
  produksjon ~ nedbor_30d + pris,
  data = trening_magasin
)


wf_elv_30d <- workflow() |>
  add_recipe(rec_elv_30d) |>
  add_model(linear_model)

wf_magasin_30d <- workflow() |>
  add_recipe(rec_magasin_30d) |>
  add_model(linear_model)


fit_elv_30d <- fit(wf_elv_30d, trening_elv)

fit_magasin_30d <- fit(wf_magasin_30d, trening_magasin)


summary(
  extract_fit_engine(fit_elv_30d)
)


# For vannmagasinet har man en r**2 på 75%, som betyr at store deler av variasjonen faktisk fanges opp
summary(
  extract_fit_engine(fit_magasin_30d)
)