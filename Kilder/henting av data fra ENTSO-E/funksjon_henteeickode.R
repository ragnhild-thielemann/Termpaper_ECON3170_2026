
source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")

hent_eic_kode <- function(land,omrade, api_key) {
  #' En funksjon som henter EIC-koden til landet/prisområdet.
  #' Både Norge og Sverige har ulike prisområder, som tas hensyn til. 
  
  
  eic_code <- dplyr::case_when(
    
    # Norge
    land == "Norway" & omrade == "NO1" ~ "10YNO-1--------2",
    land == "Norway" & omrade == "NO2" ~ "10YNO-2--------T",
    land == "Norway" & omrade == "NO3" ~ "10YNO-3--------J",
    land == "Norway" & omrade == "NO4" ~ "10YNO-4--------9",
    land == "Norway" & omrade == "NO5" ~ "10Y1001A1001A48H",
    
    # Sverige
    land == "Sweden" & omrade == "SE1" ~ "10Y1001A1001A44P",
    land == "Sweden" & omrade == "SE2" ~ "10Y1001A1001A45N",
    land == "Sweden" & omrade == "SE3" ~ "10Y1001A1001A46L",
    land == "Sweden" & omrade == "SE4" ~ "10Y1001A1001A47J",
    
    # Danmark
    land == "Denmark" & omrade == "DK1" ~ "10YDK-1--------W",
    land == "Denmark" & omrade == "DK2" ~ "10YDK-2--------M",
    
    # Finland
    land == "Finland" & omrade == "FI" ~ "10YFI-1--------U",
    
    # Tyskland
    land == "Germany" & omrade == "DE" ~ "10Y1001A1001A83F",
    
    # Frankrike
    land == "France" & omrade == "FR" ~ "10YFR-RTE------C",
    
    # Nederland
    land == "Netherlands" & omrade == "NL" ~ "10YNL----------L",
    
    # Belgia
    land == "Belgium" & omrade == "BE" ~ "10YBE----------2",
    
    # Østerrike
    land == "Austria" & omrade == "AT" ~ "10YAT-APG------L",
    
    # Polen
    land == "Poland" & omrade == "PL" ~ "10YPL-AREA-----S",
    
    # Tsjekkia
    land == "Czechia" & omrade == "CZ" ~ "10YCZ-CEPS-----N",
    
    # Slovakia
    land == "Slovakia" & omrade == "SK" ~ "10YSK-SEPS-----K",
    
    # Ungarn
    land == "Hungary" & omrade == "HU" ~ "10YHU-MAVIR----U",
    
    # Spania
    land == "Spain" & omrade == "ES" ~ "10YES-REE------0",
    
    # Portugal
    land == "Portugal" & omrade == "PT" ~ "10YPT-REN------W",
    
    # Hvis ingen av alternativene passer
    TRUE ~ NA_character_
  )
  
  if (is.na(eic_code)) {
    stop("Fant ikke EIC-kode for: ", land, " ", omrade)
  }
  
  return(eic_code)
}