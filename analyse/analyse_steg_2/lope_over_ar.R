library(tidyverse)
library(dtplyr)
library(tidymodels)
library(docstring)
library(scales)

# ---------------------------------------------------------
# 1. Henter inn datasettet
# ---------------------------------------------------------

source(
  "C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/analyse/analyse_steg_2/tibble_for_lm_modell.R"
)

set.seed(67)


# ---------------------------------------------------------
# 2. Legger til ar
# ---------------------------------------------------------

total_tibble <- total_tibble |>
  mutate(
    ar = year(datetime)
  )


# ---------------------------------------------------------
# 3. Deler data i produksjonstyper
# ---------------------------------------------------------

elv <- total_tibble |>
  filter(
    produksjonskilde == "Elvekraft"
  )

magasin <- total_tibble |>
  filter(
    produksjonskilde == "Vannkraft med magasin"
  )


# ---------------------------------------------------------
# 4. Finner hvilke ar som finnes i datasettet
# ---------------------------------------------------------

aar <- sort(unique(total_tibble$ar))


# ---------------------------------------------------------
# 5. Tom tibble for resultater
# ---------------------------------------------------------

resultater <- tibble()


# ---------------------------------------------------------
# 6. Beregner korrelasjoner for hvert ar
# ---------------------------------------------------------

for (a in aar) {
  
  # -------------------------------------------------------
  # Elvekraft
  # -------------------------------------------------------
  
  data_elv <- elv |>
    filter(ar == a)
  
  
  korrelasjon_elv <- tibble(
    variabel = c(
      "Nedbor",
      "Pris",
      "Forbruk"
    ),
    
    korrelasjon = c(
      cor(
        data_elv$produksjon,
        data_elv$nedbor,
        use = "complete.obs"
      ),
      
      cor(
        data_elv$produksjon,
        data_elv$pris,
        use = "complete.obs"
      ),
      
      cor(
        data_elv$produksjon,
        data_elv$forbruk,
        use = "complete.obs"
      )
    ),
    
    ar = a,
    produksjonstype = "Elvekraft"
  )
  
  
  # -------------------------------------------------------
  # Vannkraft med magasin
  # -------------------------------------------------------
  
  data_magasin <- magasin |>
    filter(ar == a)
  
  
  korrelasjon_magasin <- tibble(
    variabel = c(
      "Nedbor",
      "Pris",
      "Forbruk",
      "Fyllingsgrad"
    ),
    
    korrelasjon = c(
      cor(
        data_magasin$produksjon,
        data_magasin$nedbor,
        use = "complete.obs"
      ),
      
      cor(
        data_magasin$produksjon,
        data_magasin$pris,
        use = "complete.obs"
      ),
      
      cor(
        data_magasin$produksjon,
        data_magasin$forbruk,
        use = "complete.obs"
      ),
      
      cor(
        data_magasin$produksjon,
        data_magasin$fyllingsgrad,
        use = "complete.obs"
      )
    ),
    
    ar = a,
    produksjonstype = "Magasin"
  )
  
  
  # -------------------------------------------------------
  # Slår sammen resultatene
  # -------------------------------------------------------
  
  resultater <- bind_rows(
    resultater,
    korrelasjon_elv,
    korrelasjon_magasin
  )
}


# ---------------------------------------------------------
# 7. Velger variablene vi trenger
# ---------------------------------------------------------

resultater <- resultater |>
  dplyr::select(
    ar,
    korrelasjon,
    variabel,
    produksjonstype
  )


resultater

korrelasjon_model <- ggplot(
  resultater,
  aes(
    x = ar,
    y = korrelasjon,
    colour = produksjonstype,
    shape = variabel
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 3
  ) +
   
  scale_shape_manual(
    values = c(
      "Forbruk" = 18,
      "Fyllingsgrad" = 15,
      "Nedbor" = 10,
      "Pris" = 16
    )
  ) +
  labs(
    x = "Ar",
    y = "Korrelasjonskoeffisient",
    colour = "Produksjonstype",
    shape = "Forklaringsvariabel",
    title = "Utvikling i korrelasjon mellom produksjon og forklaringsvariabler"
  ) +
  theme_bw()

korrelasjon_model

#forbruk og produksjon er svært postivt korrelert
# Når vannkraft produserer mye, så er det pga mangel. 
# Når elv produserer mye, gir det overskudd i markedet. 