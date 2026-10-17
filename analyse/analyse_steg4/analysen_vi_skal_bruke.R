
library(tidyverse)
library(tidymodels)
library(broom)

# =========================================================
# 1. Les inn datasettet
# =========================================================

total_tibble <- readRDS(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Datasett/tibble7.rds"
)

glimpse(total_tibble)


# =========================================================
# 2. Klargjor datasettet
# =========================================================

# Britisk strompris utelates fordi data bare finnes for 2020.
# Tilsig utelates ogsa fra den endelige modellen.

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

print(unique(data_modell$prisomrade))


# =========================================================
# 3. Undersok manglende observasjoner
# =========================================================

# Oversikt over mangler i variablene som brukes i modellen

mangeloversikt <- data_modell |>
  group_by(prisomrade) |>
  summarise(
    antall_opprinnelig = n(),
    mangler_pris = sum(!is.finite(pris_norge)),
    mangler_europeisk_pris = sum(!is.finite(europeisk_pris)),
    mangler_forbruk = sum(!is.finite(forbruk)),
    mangler_fyllingsgrad = sum(!is.finite(fyllingsgrad)),
    mangler_magasinkraft = sum(
      !is.finite(produksjon_Magasinkraftverk)
    ),
    .groups = "drop"
  )

print(mangeloversikt)


# =========================================================
# 4. Definer modell og resultatlister etter regresjonen
# =========================================================

modell <- linear_reg() |>
  set_engine("lm")

resultater <- list()
test_resultater_liste <- list()
prediksjoner_liste <- list()
observasjoner_liste <- list()


# =========================================================
# 5. Estimer en modell for hvert prisomrade
# =========================================================

for (omrade in c("NO1", "NO2", "NO3", "NO4", "NO5")) {
  
  message("Tilpasser modell for ", omrade)
  
  # Filtrer og sorter kronologisk
  data_omrade <- data_modell |>
    filter(prisomrade == omrade) |>
    arrange(datetime)
  
  # -------------------------------------------------------
  # Velg forklaringsvariabler
  # -------------------------------------------------------
  
  if (omrade %in% c("NO1", "NO2")) {
    
    # NO1 og NO2 bruker europeisk strompris
    forklaringsvariabler <- c(
      "europeisk_pris",
      "forbruk",
      "fyllingsgrad",
      "produksjon_Magasinkraftverk"
    )
    
  } else {
    
    # NO3, NO4 og NO5 bruker ikke utenlandsk strompris
    forklaringsvariabler <- c(
      "forbruk",
      "fyllingsgrad",
      "produksjon_Magasinkraftverk"
    )
  }
  
  nodvendige_variabler <- c(
    "pris_norge",
    forklaringsvariabler
  )
  
  
  # -------------------------------------------------------
  # 6. Fjern manglende og ugyldige observasjoner
  # -------------------------------------------------------
  
  antall_for_rensing <- nrow(data_omrade)
  
  data_omrade <- data_omrade |>
    filter(
      if_all(
        all_of(nodvendige_variabler),
        ~ is.finite(.x)
      )
    ) |>
    arrange(datetime)
  
  antall_beholdt <- nrow(data_omrade)
  
  # Lagre resultatene for prisomradet
  observasjoner_liste[[omrade]] <- tibble(
    prisomrade = omrade,
    antall_for_rensing = antall_for_rensing,
    antall_beholdt = antall_beholdt,
    antall_fjernet = antall_for_rensing - antall_beholdt,
    andel_beholdt_prosent = 100 * antall_beholdt / antall_for_rensing,
    andel_fjernet_prosent = 100 *
      (antall_for_rensing - antall_beholdt) / antall_for_rensing
  )
  
  message(
    omrade, ": beholdt ", antall_beholdt,
    " av ", antall_for_rensing, " observasjoner."
  )
  
  if (antall_beholdt < 10) {
    warning("For fa observasjoner for ", omrade)
    next
  }
  
  
  # -------------------------------------------------------
  # 7. Del kronologisk: 80 % trening og 20 % test
  # -------------------------------------------------------
  
  delt <- initial_time_split(
    data_omrade,
    prop = 0.8
  )
  
  trening <- training(delt)
  test <- testing(delt)
  
  if (nrow(test) < 2) {
    warning("For fa testobservasjoner for ", omrade)
    next
  }
  
  # Lagre antall observasjoner
  observasjoner_liste[[omrade]] <- tibble(
    prisomrade = omrade,
    antall_for_rensing = antall_for_rensing,
    antall_beholdt = antall_beholdt,
    antall_trening = nrow(trening),
    antall_test = nrow(test)
  )
  
  
  # -------------------------------------------------------
  # 8. Standardiser med treningsdata
  # -------------------------------------------------------
  
  # Gjennomsnitt og standardavvik beregnes bare fra trening.
  # De samme verdiene brukes pa testsettet.
  
  variabler <- c(
    "pris_norge",
    forklaringsvariabler
  )
  
  gjennomsnitt <- trening |>
    summarise(
      across(all_of(variabler), mean)
    )
  
  standardavvik <- trening |>
    summarise(
      across(all_of(variabler), sd)
    )
  
  sd_verdier <- unlist(standardavvik)
  
  if (
    any(!is.finite(sd_verdier)) ||
    any(sd_verdier == 0)
  ) {
    warning("Ugyldig standardavvik for ", omrade)
    next
  }
  
  standardiser <- function(data) {
    data |>
      mutate(
        across(
          all_of(variabler),
          ~ (
            .x - gjennomsnitt[[cur_column()]]
          ) / standardavvik[[cur_column()]]
        )
      )
  }
  
  trening_std <- standardiser(trening)
  test_std <- standardiser(test)
  
  
  # -------------------------------------------------------
  # 9. Tilpass lineaer regresjon
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
  
  # Lagre standardiserte regresjonskoeffisienter
  resultater[[omrade]] <- tidy(
    extract_fit_engine(tilpasset)
  ) |>
    mutate(prisomrade = omrade)
  
  
  # -------------------------------------------------------
  # 10. Prediker pa testsettet
  # -------------------------------------------------------
  
  pred_std <- predict(
    tilpasset,
    new_data = test_std
  )$.pred
  
  # Konverter prediksjonene tilbake til opprinnelig skala
  pred <- tibble(
    datetime = test$datetime,
    prisomrade = omrade,
    observert_pris = test$pris_norge,
    predikert_pris =
      pred_std * standardavvik$pris_norge +
      gjennomsnitt$pris_norge
  )
  
  prediksjoner_liste[[omrade]] <- pred
  
  
  # -------------------------------------------------------
  # 11. Evaluer prediksjonene pa testsettet
  # -------------------------------------------------------
  
  SSE <- sum(
    (pred$observert_pris - pred$predikert_pris)^2
  )
  
  SST <- sum(
    (pred$observert_pris - mean(pred$observert_pris))^2
  )
  
  R2_test <- if (SST > 0) {
    1 - SSE / SST
  } else {
    NA_real_
  }
  
  test_resultater_liste[[omrade]] <- tibble(
    prisomrade = omrade,
    antall_trening = nrow(trening),
    antall_testobservasjoner = nrow(test),
    RMSE = sqrt(mean(
      (pred$observert_pris - pred$predikert_pris)^2
    )),
    MAE = mean(
      abs(pred$observert_pris - pred$predikert_pris)
    ),
    R2 = R2_test
  )
}


# =========================================================
# 12. Samle resultatene
# =========================================================

koeffisienter <- bind_rows(resultater)

test_resultater <- bind_rows(test_resultater_liste)

prediksjoner <- bind_rows(prediksjoner_liste)

observasjoner <- bind_rows(observasjoner_liste)

# Resultater fra testsettet
print(test_resultater)

# Standardiserte koeffisienter
print(koeffisienter)

# Antall observasjoner i hvert prisomrade
print(observasjoner)


# =========================================================
# 13. Plott standardiserte regresjonskoeffisienter
# =========================================================

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
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5
  ) +
  labs(
    title = "Standardiserte regresjonskoeffisienter",
    subtitle = paste(
      "Punktestimater med tilnaermede",
      "95 % konfidensintervaller"
    ),
    x = "Forklaringsvariabel",
    y = "Standardisert regresjonskoeffisient",
    colour = "Prisomrade"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(koeffisient_plot)


# =========================================================
# 14. Plott observert og predikert pris for NO3
# =========================================================

plot_NO3 <- prediksjoner |>
  filter(prisomrade == "NO3") |>
  ggplot(aes(x = datetime)) +
  geom_line(
    aes(y = observert_pris, colour = "Observert"),
    linewidth = 0.5
  ) +
  geom_line(
    aes(y = predikert_pris, colour = "Predikert"),
    linewidth = 0.5
  ) +
  scale_colour_manual(
    values = c(
      "Observert" = "black",
      "Predikert" = "red"
    )
  ) +
  labs(
    title = "Observert og predikert strompris i NO3",
    subtitle = "Prediksjoner for de siste 20 % av tidsperioden",
    x = "Tid",
    y = "Strompris",
    colour = "Serie"
  ) +
  theme_bw()

print(plot_NO3)


# =========================================================
# 15. Plott observert og predikert pris for alle omrader
# =========================================================

plot_alle <- prediksjoner |>
  pivot_longer(
    cols = c(observert_pris, predikert_pris),
    names_to = "serie",
    values_to = "pris"
  ) |>
  mutate(
    serie = recode(
      serie,
      observert_pris = "Observert",
      predikert_pris = "Predikert"
    )
  ) |>
  ggplot(aes(x = datetime, y = pris, colour = serie)) +
  geom_line(linewidth = 0.45) +
  facet_wrap(~ prisomrade, scales = "free_y") +
  labs(
    title = "Observert og predikert norsk strompris",
    subtitle = "Testsett for hvert prisomrade",
    x = "Tid",
    y = "Strompris",
    colour = "Serie"
  ) +
  theme_bw()

print(plot_alle)


# =========================================================
# 16. Lagre resultater
# =========================================================

saveRDS(
  koeffisienter,
  "Datasett/koeffisienter_lm.rds"
)

saveRDS(
  test_resultater,
  "Datasett/test_resultater_lm.rds"
)

saveRDS(
  prediksjoner,
  "Datasett/prediksjoner_lm.rds"
)

saveRDS(
  observasjoner,
  "Datasett/observasjoner_lm.rds"
)

observasjoner_tabell <- bind_rows(observasjoner_liste)

print(observasjoner)



