readRenviron("C:/Users/ragnh/OneDrive/Dokumenter/Termpaper_ECON3170_2026/Kilder/.Renviron")

api_key <- Sys.getenv("europa") #henter ut nøkkelen


finn_forste_aar <- function(
    prisomrade,
    api_key,
    startaar = 2010,
    sluttar = 2026
) {
  
  eic_koder <- c(
    NO1 = "10YNO-1--------2",
    NO2 = "10YNO-2--------T",
    NO3 = "10YNO-3--------J",
    NO4 = "10YNO-4--------9",
    NO5 = "10Y1001A1001A48H"
  )
  
  eic_code <- eic_koder[prisomrade]
  
  for (aar in startaar:sluttar) {
    
    
    dato <- as.Date(paste0(aar, "-01-01"))
    
    period_start <- paste0(format(dato, "%Y%m%d"), "0000")
    period_end <- paste0(format(dato + 1, "%Y%m%d"), "0000")
    
    response <- tryCatch(
      {
        httr2::request("https://web-api.tp.entsoe.eu/api") |>
          httr2::req_url_query(
            securityToken = api_key,
            documentType = "A44",
            in_Domain = eic_code,
            out_Domain = eic_code,
            periodStart = period_start,
            periodEnd = period_end
          ) |>
          httr2::req_perform()
      },
      error = function(e) NULL
    )
    
    if (is.null(response)) {
      next
    }
    
    xml <- tryCatch(
      httr2::resp_body_xml(response),
      error = function(e) NULL
    )
    
    if (is.null(xml)) {
      next
    }
    
    points <- xml2::xml_find_all(
      xml,
      ".//*[local-name()='Point']"
    )
    
    # Hvis vi finner data: stopp!
    if (length(points) > 0) {
      
      message(
        "Fant data for ", prisomrade,
        " i ", aar, "!"
      )
      
      return(aar)
    }
  }
  
  message(
    "Fant ingen data for ", prisomrade,
    " i perioden."
  )
  
  return(NULL)
}


for (omrade in c("NO1","NO2","NO3","NO4","NO5")){
finn_forste_aar(
  prisomrade = omrade,
  api_key = api_key,
  startaar = 2010,
  sluttar = 2026
)
}

#Vi får at det er observert data fra og med 2015
