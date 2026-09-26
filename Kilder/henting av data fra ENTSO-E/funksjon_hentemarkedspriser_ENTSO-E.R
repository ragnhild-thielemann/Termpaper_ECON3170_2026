


source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteeickode.R")

source("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")



# ------------------------------------------------------------
# Lager en funksjon som henter ut prisene
# ------------------------------------------------------------



hent_markedspriser <- function(start_dato, slutt_dato, prisomrade , variabel = "A44",  api_key= api_key) {
  #' Vi har startidspunkt, slutttidspunkt, prisområde, variabel (hva slags data vi vil hente ut) og API-nøkkel som funksjonsverdier
  land <- stringr::word(prisomrade, 1) #vi deler opp strengen med Land og prissone, slik at disse kan behandles separat. 
  omrade <- stringr::word(prisomrade, -1)
  
  eic_code = hent_eic_kode(land, omrade,api_key)
  #' Henter ut eic-koden ved hjelp av funkjsjonen
  
  #Konverterer datoene til klassen for datoer, slik at R håndterere datapunktene 
  start_dato <- as.Date(start_dato)
  slutt_dato <- as.Date(slutt_dato)
  
  #'Vi kan ikke hente ut hele datasettet på en gang, da dette blir for mye å håndtere. 
  #'Derfor oppretter vi en liste med månedsvise bolker, som vi henter  ut sekvensielt. 
  #'De månedsvise datasettenede bindes sammen med bind_cols
  
  maaneder <- seq(
    from = as.Date(format(start_dato, "%Y-%m-01")),
    to = as.Date(format(slutt_dato, "%Y-%m-01")),
    by = "month"
  )
  
  alle_data <- lapply(maaneder, function(maaned) { #løper gjennom alle månedene for å hente data
    #' Vi har delt opp tidsintervallet vårt i stykkevise bolker, der vi tar en måned om gangen. 
    maaned_start <- max(
      maaned,
      start_dato
    )
    
    maaned_slutt <- min(
      seq(maaned, by = "month", length.out = 2)[2] - 1,
      slutt_dato
    )
    
    message(
      #'Printer ut landet og prisområdet som hentes ut fra datasettet, samt datoen vi henter ut for
      
      "Henter ",land, prisomrade, ": ",
      maaned_start, " -> ", maaned_slutt
    )
    
    period_start <- paste0(
      format(maaned_start, "%Y%m%d"),
      "0000"
    )
    
    period_end <- paste0(
      format(maaned_slutt + 1, "%Y%m%d"),
      "0000"
    )
    
    
    response <- httr2::request( #Sender en request til API-et, der vi henter ut dataen for det gitte tidspunktet i det aktuelle prisomdådet
      "https://web-api.tp.entsoe.eu/api"
    ) |>
      httr2::req_url_query(
        securityToken = api_key,
        documentType = variabel,
        in_Domain = eic_code,
        out_Domain = eic_code,
        periodStart = period_start,
        periodEnd = period_end
      ) |>
      httr2::req_perform()
    
    #API-forespørselen retuneres som i et XML-format, som håndteres til tibbleler for R
    xml <- httr2::resp_body_xml(response)
    
    time_series <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    if (length(time_series) == 0) {
      return(tibble::tibble())
    }
    
    resultat <- lapply(time_series, function(ts) {
      
      interval_start <- xml2::xml_text(
        xml2::xml_find_first(
          ts,
          ".//*[local-name()='start']"
        )
      )
      
      resolution <- xml2::xml_text(
        xml2::xml_find_first(
          ts,
          ".//*[local-name()='resolution']"
        )
      )
      
      minutes <- dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT30M" ~ 30,
        resolution == "PT60M" ~ 60,
        resolution == "PT1H"  ~ 60,
        TRUE ~ NA_real_
      )
      
      if (is.na(minutes)) {
        stop("Ukjent oppløsning: ", resolution)
      }
      
      points <- xml2::xml_find_all(
        ts,
        ".//*[local-name()='Point']"
      )
      
      if (length(points) == 0) {
        return(NULL)
      }
      
      position <- as.integer(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            ".//*[local-name()='position']"
          )
        )
      )
      
      price <- as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            ".//*[local-name()='price.amount']"
          )
        )
      )
      
      start_datetime <- as.POSIXct(
        interval_start,
        format = "%Y-%m-%dT%H:%MZ",
        tz = "UTC"
      )
      
      datetime <- start_datetime +
        minutes * 60 * (position - 1)
      #Skriver dataen inn som en tibble
      tibble::tibble(
        datetime = datetime,
        country = "Norway",
        zone = prisomrade,
        price = price,
        position = position,
        resolution = resolution
      )
    })
    
    dplyr::bind_rows(resultat)
  })
  #Setter sammen dataen fra de ulike månedene til ett datasett
  dplyr::bind_rows(alle_data) |>
    dplyr::distinct(
      datetime,
      country,
      zone,
      .keep_all = TRUE
    ) |>
    #Sorterer dateen etter dato
    dplyr::arrange(datetime) 
}

#kan nå hente for hvert prisområde vi ønsker å ha ut. Funksjonen bør lages generell, slik at