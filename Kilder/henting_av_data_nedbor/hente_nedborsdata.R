

source("Kilder/henting_av_data_nedbor/funksjon_hente_nedborsdata.R")



readRenviron("Kilder/.Renviron")

api_key <- Sys.getenv("nedbor") #henter ut API-nøkkelen

print(api_key)
if (api_key == "") {
  stop("Fant ikke ENTSOE_TOKEN. Sjekk .Renviron.")
}


#henter ut nedbør for en målestasjon i prisområde 2, slik at vi senere kan regne akkumulert nedbør
nedbor_tonstad <- hent_nedbor( api_key, longitude = 6.716, latitude = 58.662 )

saveRDS(nedbor_tonstad,
        "Datasett/nedbor_tonstad.rds")


View(nedbor_tonstad)