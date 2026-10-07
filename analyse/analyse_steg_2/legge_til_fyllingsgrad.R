library(tidyverse)
library(dtplyr)
library(tidymodels)
library(docstring)
library(scales)

# ---------------------------------------------------------
# 1. Henter inn datasettene jeg trenger, samt relevante biblioteker
# ---------------------------------------------------------

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/analyse/analyse_steg_2/tibble_for_lm_modell.R")
set.seed(67)
library(tidyverse)
library(dtplyr)
library(tidymodels)
library(docstring)

# ---------------------------------------------------------
# 2. Funksjon for å lage lagget og akkumulert nedbør
# ---------------------------------------------------------

lagg_nedbor <- function(data) {
  
  data |>
    arrange(prisomrade, datetime) |>
    
    #jobber med det som en data.frame
    lazy_dt() |>
    group_by(prisomrade) |>
    mutate(
      
      # Nedbør 1, 7, 14 og 30 dager tidligere
      nedbor_lag1 = shift(nedbor, 1),
      nedbor_lag7 = shift(nedbor, 7),
      nedbor_lag14 = shift(nedbor, 14),
      nedbor_lag30 = shift(nedbor, 30),
      
      # Akkumulert nedbør i løpet av  henholdsvis 7, 14 og 30 dager
      nedbor_0d = frollsum(nedbor, 1, align = "right"),
      nedbor_7d = frollsum(nedbor, 7, align = "right"),
      nedbor_14d = frollsum(nedbor, 14, align = "right"),
      nedbor_30d = frollsum(nedbor, 30, align = "right")
    ) |>
    collect()
}

total_tibble <- lagg_nedbor(total_tibble)



#henter inn tibbelsene fra sammenslåingen

#' Når vi deler dataen, ønsker vi å ha en lik andel av prisområdene i henholdsvis test-settet og trenignssettet
delt_elv <- initial_split(
  total_tibble |>
    filter(produksjonskilde == "Elvekraft") |>
    dplyr::select(-produksjonskilde),
  prop = 0.8,
  strata = prisomrade
)

trening_elv <- training(delt_elv)
test_elv <- testing(delt_elv)


delt_magasin <- initial_split(
  total_tibble |>
    filter(produksjonskilde == "Vannkraft med magasin") |>
    dplyr::select(-produksjonskilde),
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
# 5. Setter opp lineære modeller med pris og akkumulert
#    nedbør som forklaringsvariabler. Det opprettes egne
#    workflows for elvekraft og vannkraft med magasin,
#    og for hver av de ulike periodene med akkumulert nedbør. 

#     Dette gir mange modeller, men vi må undersøke korrelasjonskoeffisientene,
#     signifikansen deres og R² for hver modell. Dette er nødvendig for å vurdere
#     om pris og nedbør er signifikante forklaringsvariabler for produksjonen. 
#     Derfor kjører vi dette som en løkke, før vi plotter resulatene

# ---------------------------------------------------------
antall_dager <- c(
  "nedbor_0d",
  "nedbor_7d",
  "nedbor_14d",
  "nedbor_30d"
)

resultater <- tibble()
r2_resultater <- tibble()

for (dager in antall_dager) {
  
  # -------------------------------------------------------
  # Setter opp recipies slik at vi kan begynne å jobbe med dem
  # -------------------------------------------------------
  rec_elv <- recipe(
    reformulate(
      c(dager, "forbruk", "pris"),
      response = "produksjon"
    ),
    data = trening_elv
  ) |>
    step_normalize(all_numeric_predictors()) |>
    step_normalize(all_outcomes())
  
  
  rec_magasin <- recipe(
    reformulate(
      c(dager, "forbruk", "pris", "fyllingsgrad"),
      response = "produksjon"
    ),
    data = trening_magasin
  ) |>
    step_normalize(all_numeric_predictors()) |>
    step_normalize(all_outcomes())
  
  # -------------------------------------------------------
  # Workflows
  # -------------------------------------------------------
  
  wf_elv <- workflow() |>
    add_recipe(rec_elv) |>
    add_model(linear_model)
  
  wf_magasin<- workflow() |>
    add_recipe(rec_magasin) |>
    add_model(linear_model)
 
  
  
  # -------------------------------------------------------
  # Tilpass modellene
  # -------------------------------------------------------
  
  fit_elv <- fit(wf_elv, trening_elv)
  fit_magasin <- fit(wf_magasin, trening_magasin)
 
  
  
  # -------------------------------------------------------
  # R²
  # -------------------------------------------------------
  
  r2_elv <- glance(
    extract_fit_engine(fit_elv)
  ) |>
    dplyr::select(r.squared) |>
    mutate(
      dager = dager,
      produksjonstype = "Elvekraft",
      fyllingsgrad_modell = "Uten fyllingsgrad"
    )
  
  r2_magasin <- glance(
    extract_fit_engine(fit_magasin)
  ) |>
    dplyr::select(r.squared) |>
    mutate(
      dager = dager,
      produksjonstype = "Magasin",
      fyllingsgrad_modell = "Med fyllingsgrad"
    )
 
  
  r2_resultater <- bind_rows(
    r2_resultater,
    r2_elv,
    r2_magasin
  )
  
  
  # -------------------------------------------------------
  # Koeffisienter
  # -------------------------------------------------------
  
  resultat_elv <- tidy(
    extract_fit_engine(fit_elv)
  ) |>
    filter(term %in% c(dager, "pris", "forbruk")) |>
    mutate(
      dager = dager,
      produksjonstype = "Elvekraft"
    )
  
  
  
  
  
  resultat_magasin <- tidy(
    extract_fit_engine(fit_magasin)
  ) |>
    filter(term %in% c(
      dager,
      "pris",
      "forbruk",
      "fyllingsgrad"
    )) |>
    mutate(
      dager = dager,
      produksjonstype = "Magasin"
    )
  
  
  resultater <- bind_rows(
    resultater,
    resultat_elv,
    resultat_magasin
  )
}

r2_resultater <- r2_resultater |>
  mutate(
    dager = case_when(
      dager == "nedbor_0d"  ~ 0,
      dager == "nedbor_7d"  ~ 7,
      dager == "nedbor_14d" ~ 14,
      dager == "nedbor_30d" ~ 30
    )
  )

resultater <- resultater |>
  rename(
    mu = estimate,
    sigma = std.error
  ) |>
  mutate(
    variabel = case_when(
      str_starts(term, "nedbor") ~ "Nedbor",
      term == "pris" ~ "Pris",
      term == "forbruk" ~ "Forbruk",
      term == "fyllingsgrad" ~ "Fyllingsgrad"
    ),
    
    dager = case_when(
      dager == "nedbor_0d"  ~ 0,
      dager == "nedbor_7d"  ~ 7,
      dager == "nedbor_14d" ~ 14,
      dager == "nedbor_30d" ~ 30
    )
  ) |>
  dplyr::select(
    dager,
    mu,
    sigma,
    variabel,
    produksjonstype
  )

r2_resultater
r2_model <- ggplot(
  r2_resultater,
  aes(
    x = dager,
    y = r.squared,
    colour = produksjonstype
  )
) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    x = "Akkumulert nedbor (dager)",
    y = expression(R^2),
    colour = "Produksjonstype",
    linetype = "Modell",
    title = "Modellenes forklarte varians"
  ) +
  theme_bw() 

r2_model


lm_model <- resultater|>
  
  ggplot(
  aes(
    x = dager,
    y = mu,
    colour = produksjonstype,
    shape = variabel
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      ymin = mu - 1.96 * sigma,
      ymax = mu + 1.96 * sigma
    ),
    width = 0.5
  ) +
  scale_shape_manual(
    values = c(
      "Forbruk" = 18,
      "Fyllingsgrad" = 15,
      "Nedbor" = 10,
      "Pris" = 16
    )
  )+ 
  geom_point(size = 3) +
  labs(
    x = "Akkumulert nedbor (dager)",
    y = "Standardisert regresjonskoeffisient",
    colour = "Produksjonstype",
    shape = "Forklaringsvariabel",
    title = "Estimerte korrelasjonskoefesienter"
  ) +
  theme_bw()

lm_model #sier ingenting om kausale effekter, men kan gi oss en korrelasjon

# av modellen kan vi si at alt er positivt korrelert, men at i modellen for vannkraft er det svært lav R**2. Derfor er det lite vi kan fastslå utifra modellen. 

# Det er ikke pris som avgjør hvor mye vi bruker, men tilgjenlig strøm i markedet 