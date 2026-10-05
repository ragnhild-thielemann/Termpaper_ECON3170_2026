
#henter inn de datasettene jeg trenger

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/analyse/analyse_steg_1/sammenslaing_av_tibbels.R")

View(total_produksjon_jobbing)
soylediagram <- total_produksjon_jobbing|>
  ggplot(aes(x = produksjonskilde, y = produksjon, fill = prisomrade)) + 
  geom_col(position = "dodge") + 
  labs(x = "Produksjonskilde", y = "Gjennomsnittlig produksjon per time (MW)", title = "Fordeling av ulike produksjonkilder (2020-2026)", fill = "Prisomrader") +
  theme_bw()


soylediagram

#lagrer plottet som et gg-objekt
ggsave(
       "Plott/soylediagram.png", soylediagram)

