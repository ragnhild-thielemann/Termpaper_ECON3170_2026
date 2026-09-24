

library(arrow)
library(httr)
library(dplyr)
library(purrr)
library(arrow)



library(arrow)
library(httr)
library(dplyr)
library(purrr)

hent_strompris <- function(
    start_date = "2020-01-01",
    end_date = "2024-12-31",
    prisomrade = "NO2"
) {
  
  url <- "https://allemannsdata.com/wiki/api/v1/kilder/strompris/get_prices"
  
  # Alle datoene vi ønsker
  datoer <- seq(
    as.Date(start_date),
    as.Date(end_date),
    by = "day"
  )
  
  resultat <- purrr::map_dfr(
    datoer,
    function(dato) {
      
      cat("Henter", dato, "\n")
      
      response <- GET(
        url,
        query = list(
          date = format(dato, "%Y-%m-%d"),
          area = prisomrade
        )
      )
      
      stop_for_status(response)
      
      json <- content(
        response,
        as = "parsed"
      )
      
      # Hent prisene
      data <- tibble::as_tibble(json$data$prices)
      
      data
    }
  )
  
  return(resultat)
}

strompris_NO2 <- hent_strompris(
  start_date = "2020-01-01",
  end_date = "2024-12-31",
  prisomrade = "NO2"
)


library(httr)

library(httr)

response <- GET(
  "https://allemannsdata.com/wiki/api/v1/kilder/strompris/get_prices",
  query = list(
    date = "2020-12-01",
    area = "NO2",
    limit = 100,
    offset = 0
  )
)

status_code(response)
content(response, as = "text")
