library(httr2)
library(xml2)
library(dplyr)
library(tibble)
library(docstring)
library(lubridate)




readRenviron("Kilder/.Renviron")

api_key <- Sys.getenv("europa") #henter ut nøkkelen



if (api_key == "") {
  stop("Fant ikke ENTSOE_TOKEN. Sjekk .Renviron.")
}
