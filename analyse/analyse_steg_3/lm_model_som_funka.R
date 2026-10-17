
library(tidyverse)
library(tidymodels)
library(broom)

set.seed(67)

# ---------------------------------------------------------
# 1. Les inn datasettet
# ---------------------------------------------------------

total_tibble <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/tibble5.rds"
)

# ---------------------------------------------------------
# 2. Klargjor datasettet
# ---------------------------------------------------------

data_modell <- total_tibble |>
  transmute(
    datetime,
    prisomrade,
    pris_norge,
    europeisk_pris,
    forbruk,
    fyllingsgrad,
    produksjon_Magasinkraftverk
  ) |>
  arrange(prisomrade, datetime)

# ---------------------------------------------------------
# 3. Definer modell og resultatlister
# ---------------------------------------------------------

modell <- linear_reg() |>
  set_engine("lm")

resultater <- list()
test_resultater <- list()
prediksjoner_liste <- list()

# ---------------------------------------------------------
# 4. Tilpass modeller for hvert prisomrade
# ---------------------------------------------------------

for (omrade in unique(data_modell$prisomrade)) {
  
  data_omrade <- data_modell |>
    filter(prisomrade == omrade) |>
    arrange(datetime)
  
  # Europeisk pris brukes bare for NO2
  forklaringsvariabler <- if (omrade %in% c("NO1", "NO2")) {
    c("europeisk_pris", "forbruk", "fyllingsgrad",
      "produksjon_Magasinkraftverk")
  } else {
    c("forbruk", "fyllingsgrad", "produksjon_Magasinkraftverk")
  }
  
  # Fjern manglende verdier i variablene som brukes
  data_omrade <- data_omrade |>
    filter(
      if_all(
        all_of(c("pris_norge", forklaringsvariabler)),
        ~ !is.na(.x)
      )
    )
  
  # Kronologisk deling: 80 % trening, 20 % test
  delt <- initial_time_split(
    data_omrade,
    prop = 0.8
  )
  
  trening <- training(delt)
  test <- testing(delt)
  
  # -------------------------------------------------------
  # 5. Standardiser ut fra treningsdataene
  # -------------------------------------------------------
  
  variabler <- c("pris_norge", forklaringsvariabler)
  
  gjennomsnitt <- trening |>
    summarise(
      across(
        all_of(variabler),
        ~ mean(.x, na.rm = TRUE)
      )
    )
  
  standardavvik <- trening |>
    summarise(
      across(
        all_of(variabler),
        ~ sd(.x, na.rm = TRUE)
      )
    )
  
  # Kontroller at ingen variabler har standardavvik lik null
  if (any(unlist(standardavvik) == 0)) {
    stop(paste("En variabel har standardavvik lik null i", omrade))
  }
  
  # Standardiser trening og test med samme parametre
  standardiser <- function(data) {
    data |>
      mutate(
        across(
          all_of(variabler),
          ~ (.x - gjennomsnitt[[cur_column()]]) /
            standardavvik[[cur_column()]]
        )
      )
  }
  
  trening_std <- standardiser(trening)
  test_std <- standardiser(test)
  
  # -------------------------------------------------------
  # 6. Tilpass regresjonsmodellen
  # -------------------------------------------------------
  
  formel <- reformulate(
    forklaringsvariabler,
    response = "pris_norge"
  )
  
  arbeidsflyt <- workflow() |>
    add_model(modell) |>
    add_formula(formel)
  
  tilpasset <- fit(
    arbeidsflyt,
    data = trening_std
  )
  
  # Standardiserte regresjonskoeffisienter
  resultater[[omrade]] <- tidy(
    extract_fit_engine(tilpasset)
  ) |>
    mutate(prisomrade = omrade)
  
  # -------------------------------------------------------
  # 7. Prediksjoner pa testdata
  # -------------------------------------------------------
  
  pred_std <- predict(
    tilpasset,
    new_data = test_std
  )$.pred
  
  # Konverter prediksjonene tilbake til opprinnelig prisskala
  pred <- tibble(
    datetime = test$datetime,
    prisomrade = omrade,
    observert_pris = test$pris_norge,
    predikert_pris =
      pred_std * standardavvik$pris_norge +
      gjennomsnitt$pris_norge
  )
  
  prediksjoner_liste[[omrade]] <- pred
  
  # Evaluer prediksjonene i opprinnelig enhet
  test_resultater[[omrade]] <- tibble(
    prisomrade = omrade,
    antall_testobservasjoner = nrow(test),
    RMSE = rmse_vec(
      truth = pred$observert_pris,
      estimate = pred$predikert_pris
    ),
    R2 = rsq_vec(
      truth = pred$observert_pris,
      estimate = pred$predikert_pris
    )
  )
}

# ---------------------------------------------------------
# 8. Samle resultatene
# ---------------------------------------------------------

koeffisienter <- bind_rows(resultater)
test_resultater <- bind_rows(test_resultater)
prediksjoner <- bind_rows(prediksjoner_liste)

# Vis standardiserte koeffisienter
koeffisienter

# Vis prediksjonsevne pa testdata
test_resultater

# ---------------------------------------------------------
# 9. Plott standardiserte regresjonskoeffisienter
# ---------------------------------------------------------

koeffisient_plot <- koeffisienter |>
  filter(term != "(Intercept)") |>
  mutate(
    variabel = recode(
      term,
      europeisk_pris = "Europeisk pris",
      forbruk = "Forbruk",
      fyllingsgrad = "Magasinfylling",
      produksjon_Magasinkraftverk =
        "Magasinkraftproduksjon"
    )
  ) |>
  ggplot(
    aes(
      x = variabel,
      y = estimate,
      colour = prisomrade
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2.5
  ) +
  labs(
    x = "Forklaringsvariabel",
    y = "Standardisert regresjonskoeffisient",
    colour = "Prisomrade",
    title = "Standardiserte regresjonskoeffisienter",
    subtitle = "Punktestimater med tilnærmede 95 % konfidensintervaller"
  ) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  theme_bw() 
  

koeffisient_plot
