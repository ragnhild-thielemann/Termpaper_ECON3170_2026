
source("Kilder/henting av data fra ENTSO-E/funksjon_henteeickode.R")
source("Kilder/henting av data fra ENTSO-E/funksjon_henteAPInokkel.R")


# ------------------------------------------------------------
# Lager en funksjon som henter ut prisene
# ------------------------------------------------------------

hent_markedspriser <- function(start_dato,
                               slutt_dato,
                               prisomrade,
                               variabel = "A44",
                               api_key = api_key) {
  
  # Vi har starttidspunkt, sluttidspunkt, prisomrade,
  # variabel (hva slags data vi vil hente ut) og API-nokkel
  # som funksjonsverdier.
  
  land <- stringr::word(prisomrade, 1)
  
  # Vi deler opp strengen med land og prissone, slik at
  # disse kan behandles separat.
  omrade <- stringr::word(prisomrade, -1)
  
  # Henter EIC-koden ved hjelp av funksjonen.
  eic_code <- hent_eic_kode(land, omrade, api_key)
  
  
  # ----------------------------------------------------------
  # Behandler datoene
  # ----------------------------------------------------------
  
  # Konverterer datoene til Date-klassen, slik at R kan
  # handtere datoene korrekt.
  start_dato <- as.Date(start_dato)
  slutt_dato <- as.Date(slutt_dato)
  
  if (start_dato > slutt_dato) {
    stop("start_dato kan ikke vaere etter slutt_dato.")
  }
  
  
  # ----------------------------------------------------------
  # Deler opp tidsperioden i manedsvise bolker
  # ----------------------------------------------------------
  
  # Vi kan ikke hente ut hele datasettet pa en gang, da dette
  # kan bli mye data a handtere. Derfor oppretter vi en liste
  # med manedsvise bolker som vi henter ut sekvensielt.
  # Datasettene bindes sammen etter at alle manedene er hentet.
  
  maaneder <- seq(
    from = as.Date(format(start_dato, "%Y-%m-01")),
    to = as.Date(format(slutt_dato, "%Y-%m-01")),
    by = "month"
  )
  
  
  # ----------------------------------------------------------
  # Henter data for hver maned
  # ----------------------------------------------------------
  
  alle_data <- lapply(maaneder, function(maaned) {
    
    # Finner forste og siste dato i den aktuelle maneden.
    maaned_start <- max(
      maaned,
      start_dato
    )
    
    maaned_slutt <- min(
      seq(maaned, by = "month", length.out = 2)[2] - 1,
      slutt_dato
    )
    
    # Printer ut landet, prisomradet og perioden som hentes.
    message(
      "Henter ", prisomrade, ": ",
      maaned_start, " -> ", maaned_slutt
    )
    
    # ENTSO-E forventer tidspunkt pa formatet YYYYMMDDHHMM.
    # Tidspunktene angis i UTC.
    period_start <- paste0(
      format(maaned_start, "%Y%m%d"),
      "0000"
    )
    
    # Sluttidspunktet er starten pa dagen etter siste dato.
    period_end <- paste0(
      format(maaned_slutt + 1, "%Y%m%d"),
      "0000"
    )
    
    
    # --------------------------------------------------------
    # Sender en foresporsel til ENTSO-E sitt API
    # --------------------------------------------------------
    
    response <- httr2::request(
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
      httr2::req_timeout(120) |>
      httr2::req_perform()
    
    
    # --------------------------------------------------------
    # Behandler API-svaret
    # --------------------------------------------------------
    
    # API-foresporselen returneres i XML-format.
    # Vi leser XML-filen og finner alle TimeSeries-elementene.
    xml <- httr2::resp_body_xml(response)
    
    time_series <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='TimeSeries']"
    )
    
    # Dersom ENTSO-E ikke returnerer noen tidsserier for
    # maneden, returnerer vi en tom tibble med riktige kolonner.
    # Dette hindrer at distinct() feiler senere.
    if (length(time_series) == 0) {
      
      warning(
        "Ingen tidsserier for ", prisomrade,
        " i perioden ", maaned_start,
        " til ", maaned_slutt
      )
      
      return(tibble::tibble(
        datetime = as.POSIXct(character(), tz = "UTC"),
        country = character(),
        zone = character(),
        price = double(),
        position = integer(),
        resolution = character()
      ))
    }
    
    
    # --------------------------------------------------------
    # Behandler hver tidsserie
    # --------------------------------------------------------
    
    resultat <- lapply(time_series, function(ts) {
      
      # Finner Period-elementet som inneholder starttidspunkt,
      # opplosning og prisobservasjoner.
      period <- xml2::xml_find_first(
        ts,
        ".//*[local-name()='Period']"
      )
      
      # Henter starttidspunktet for perioden.
      interval_start <- xml2::xml_text(
        xml2::xml_find_first(
          period,
          "./*[local-name()='timeInterval']/*[local-name()='start']"
        )
      )
      
      # Henter tidsopplosningen, for eksempel PT60M eller PT15M.
      resolution <- xml2::xml_text(
        xml2::xml_find_first(
          period,
          "./*[local-name()='resolution']"
        )
      )
      
      # Oversetter tidsopplosningen til antall minutter.
      minutes <- dplyr::case_when(
        resolution == "PT15M" ~ 15,
        resolution == "PT30M" ~ 30,
        resolution %in% c("PT60M", "PT1H") ~ 60,
        TRUE ~ NA_real_
      )
      
      if (is.na(minutes)) {
        stop("Ukjent opplosning: ", resolution)
      }
      
      # Finner alle prisobservasjonene i perioden.
      points <- xml2::xml_find_all(
        period,
        "./*[local-name()='Point']"
      )
      
      if (length(points) == 0) {
        return(NULL)
      }
      
      # Henter posisjonen til hvert datapunkt i perioden.
      position <- as.integer(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            "./*[local-name()='position']"
          )
        )
      )
      
      # Henter den tilhorende markedsprisen.
      price <- as.numeric(
        xml2::xml_text(
          xml2::xml_find_all(
            points,
            "./*[local-name()='price.amount']"
          )
        )
      )
      
      # Konverterer starttidspunktet til UTC.
      # ENTSO-E oppgir tidspunktene i UTC i dette formatet.
      start_datetime <- as.POSIXct(
        interval_start,
        format = "%Y-%m-%dT%H:%MZ",
        tz = "UTC"
      )
      
      # Kontrollerer at tidspunktet ble lest riktig.
      if (is.na(start_datetime)) {
        stop(
          "Kunne ikke lese starttidspunktet: ",
          interval_start
        )
      }
      
      # Beregner tidspunktet for hvert datapunkt ved hjelp av
      # starttidspunkt, tidsopplosning og posisjon.
      datetime <- start_datetime +
        minutes * 60 * (position - 1)
      
      # Skriver dataene inn som en tibble.
      # Land og prisomrade hentes fra argumentet til funksjonen.
      tibble::tibble(
        datetime = datetime,
        country = land,
        zone = omrade,
        price = price,
        position = position,
        resolution = resolution
      )
    })
    
    
    # Setter sammen tidsseriene for den aktuelle maneden.
    dplyr::bind_rows(resultat)
  })
  
  
  # ------------------------------------------------------------
  # Setter sammen dataene fra alle manedene
  # ------------------------------------------------------------
  
  # Binder sammen manedsdatasettene, fjerner eventuelle
  # duplikater og sorterer observasjonene etter tidspunkt.
  dplyr::bind_rows(alle_data) |>
    dplyr::distinct(
      datetime,
      country,
      zone,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(datetime)
}
