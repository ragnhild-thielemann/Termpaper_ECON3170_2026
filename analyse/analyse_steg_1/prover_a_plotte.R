library(tidyverse)
plott <- ggplot(
  resultater,
  aes(
    x = dager,
    y = mu,
    colour = produksjonstype,
    shape = variabel
  )
) +
  geom_plot(size = 3)

plott
  geom_hline(yintercept = 0, linetype = "dashed") +
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
      "Nedbor" = 16,
      "Forbruk" = 17
    )
  ) +
  labs(
    x = "Akkumulert nedbør (dager)",
    y = "Estimert koeffisient",
    colour = "Produksjonstype",
    shape = "Forklaringsvariabel",
    title = "Estimerte koeffisienter for pris, forbruk og nedbør"
  ) +
  theme_minimal()