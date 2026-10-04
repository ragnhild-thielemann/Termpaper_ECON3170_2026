library(tidyverse)
library(dtplyr)
library(tidymodels)
library(docstring)

# ---------------------------------------------------------
# 1. Del data i trenings- og testsett
# ---------------------------------------------------------


set.seed(67)

#' Når vi deler dataen, ønsker vi å ha en lik andel av prisområdene i henholdsvis test-settet og trenignssettet
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


# ---------------------------------------------------------
# 4. Setter henholdsvis trening-settet og test-settet som 
#   variabler i lagg_nedbør(), slik at vi får retunert tibbles
#   med akkumulert nedbør som variabler
# ---------------------------------------------------------

trening_elv <- lagg_nedbor(trening_elv)
test_elv <- lagg_nedbor(test_elv)

trening_magasin <- lagg_nedbor(trening_magasin)
test_magasin <- lagg_nedbor(test_magasin)


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


antall_dager <- c("nedbor_0d", "nedbor_7d", "nedbor_14d", "nedbor_30d")

resultater <- tibble()

for (dager in antall_dager) {
  
  rec_elv <- recipe(
    
    #gjør antall dager som forkaringsvariablen tilhørende akkumulert nedbør
    reformulate(c(dager, "pris"), response = "produksjon"),
    data = trening_elv
  )
  
  rec_magasin <- recipe(
    reformulate(c(dager, "pris"), response = "produksjon"),
    data = trening_magasin
  )
  
  # Setter opp workflows
  wf_elv <- workflow() |>
    add_recipe(rec_elv) |>
    add_model(linear_model)
  
  wf_magasin <- workflow() |>
    add_recipe(rec_magasin) |>
    add_model(linear_model)
  
  # Tilpass modellene på treningsdata
  fit_elv <- fit(wf_elv, trening_elv)
  fit_magasin <- fit(wf_magasin, trening_magasin)
  
  # Hent koeffisienter for elvekraft
  resultat_elv <- tidy(extract_fit_engine(fit_elv)) |>
    filter(term %in% c(dager, "pris")) |>
    mutate(
      dager = dager,
      produksjonstype = "Elv"
    )
  
  # Hent koeffisienter for magasinkraft
  resultat_magasin <- tidy(extract_fit_engine(fit_magasin)) |>
    filter(term %in% c(dager, "pris")) |>
    mutate(
      dager = dager,
      produksjonstype = "Magasin"
    )
  
  # Legg resultatene til som rader under den allerde eksistende tibbelen
  resultater <- bind_rows(
    resultater,
    resultat_elv,
    resultat_magasin
  )
  
}

resultater <- resultater |>
  
  #Endrer til kjent notasjon for forventing og standardavvik
  rename(
    mu = estimate,
    sigma = std.error
  ) |>
  
  #Gjør variablen konstatnt som pris eller nedbør
  mutate(
    variabel = if_else(
      str_starts(term, "nedbor"),
      "Nedbor",
      "Pris"
    ),
    
    #Gjør dager om til numeriske variabler, slik at dette kan brukes som
    # x-aksen i plottet
    dager = case_when(
      dager == "nedbor_0d"  ~ 0,
      dager == "nedbor_7d"  ~ 7,
      dager == "nedbor_14d" ~ 14,
      dager == "nedbor_30d" ~ 30
    )
  ) |>
  
  #filtrerer ut de relevante variablene for plottet
  select(
    dager,
    mu,
    sigma,
    variabel,
    produksjonstype
  )


# Plotter resulatet

ggplot(
  resultater,
  aes(
    x = dager,
    y = mu,
    colour = produksjonstype,
    shape = variabel
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  #plotter et 95% koefedisiensintervall
  geom_errorbar(
    aes(
      ymin = mu - 1.96 * sigma,
      ymax = mu + 1.96 * sigma
    ),
    width = 0.5
  ) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(
      "Elv" = "hotpink",
      "Magasin" = "blue"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Pris" = 15,
      "Nedbor" = 16
    )
  ) +
  labs(
    x = "Akkumulert nedbør (dager)",
    y = "Estimert koeffisient",
    colour = "Produksjonstype",
    shape = "Forklaringsvariabel",
    title = "Utviklingen til forklaringsvariablene"
  ) +
  
  theme_minimal()
