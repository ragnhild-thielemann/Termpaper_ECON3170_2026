---
title: "README"
output: html_document
---

Datasettet over strømprisene er enormt, og krever derfor god kode for å få ut på en fornuftig måte. Når vi skal hente ut datasettet, oppretter vi en sjult fil som inneholder tilgangsnøkkelen til API-et. Deretter henter vi denne frem, og lager en testfunksjon der vi henter ut dataen fra Østerrike, for å undersøke strukturen til API-et. Datasettet er en XML-fil, som vi må håndtere, før vi får dataen på r-format. 


Jeg jobber objektorientert, og skriver de ulike funksjonene i hvert sitt dokument. Dette er for å unngå at filene blir for store, og dermed vannkelig å navigere i.


API-nøkkel → ENTSO-E API → XML → prisdata → tidspunkt → R-datasett


position <- et tall mellom 1 og 96, da vi får en ny observasjon hvert 15 minutt



Forklaring av datasettet

|Variabel|Forklaring|
|------|--------|
|position | Tidsintervall på 15 min. Begynner ved 1 ved midnatt|
|eic_code| Landkoden brukt for lagringen lokalt|
|price| Markedsprisen på strøm målt i EUR/MWh. Dette er ikke det samme som prisen vi faktisk betaler|


Pris konsumentene betaler = markedspris + nettleie + avgifter + mva

4. Day-ahead-forbruk vs. faktisk forbruk ⭐⭐⭐

Dette synes jeg er en veldig fin analyse for ECON3170.

ENTSO-E har både:

day-ahead forecast
week-ahead forecast
month-ahead forecast
year-ahead forecast
faktisk forbruk

Da kan du beregne:

$$ \text{Forecast error} = \text{Actual load} - \text{Forecast load} $$

For eksempel:

forecast = 8 000 MW
actual   = 8 800 MW

forecast error = +800 MW

Så kan du undersøke om store prognosefeil henger sammen med store prisendringer.

Det er en ganske fin data science-problemstilling.

5. Planlagte og uplanlagte utfall ⭐⭐⭐

Dette er kanskje enda mer spennende for kraftpris.

ENTSO-E publiserer data om planned unavailability og endringer i faktisk tilgjengelighet for blant annet forbindelser og transmisjonsnett.

Du kan da undersøke:

Hva skjer med strømprisen når en viktig overføringsforbindelse blir utilgjengelig?

For eksempel:

Linje ute
   ↓
lavere overføringskapasitet
   ↓
flaskehals
   ↓
prisforskjell mellom NO-områder

Dette kan kobles direkte til datasettet ditt med NO1_NO2, NO3_NO5 osv.

6. Day-ahead-priser

ENTSO-E har også day-ahead prices (A44).

Men her ville jeg vært litt forsiktig hvis du allerede bruker Nord Pool/Hvakosterstrømmen.

Det interessante er heller å bruke dem til å kontrollere at prisdatasettet ditt er riktig, eller analysere prisforskjeller mellom land.

For eksempel:

$$ P_{NO2}-P_{DE} $$

og

$$ P_{NO2}-P_{DK1} $$
7. Kommersiell kraftutveksling vs. fysisk kraftflyt ⭐⭐⭐

Dette synes jeg er spesielt interessant.

ENTSO-E skiller mellom:

scheduled/commercial exchanges
physical flows

Det betyr at du kan undersøke forskjellen mellom:

hva markedet har planlagt å utveksle

og

hva som faktisk fysisk flyter i nettet.

Dette er et veldig interessant tema fordi strøm ikke nødvendigvis følger den kommersielle kontraktsveien fysisk.

8. Import og eksport

For Norge kan du undersøke:

Norsk produksjon
       +
Import
       -
Eksport
       =
Tilgjengelig kraft

Og koble dette til:

vannmagasiner
temperatur
vind
strømpris
prisforskjeller mellom land

Da kan du for eksempel se perioder hvor Norge går fra netto eksportør til netto importør.

Hvis dette er til ECON3170-prosjektet ditt

Jeg ville faktisk bygget datasettet omtrent slik:

datetime
       │
       ├── strømpris NO1
       ├── strømpris NO2
       ├── strømpris NO3
       ├── strømpris NO4
       ├── strømpris NO5
       │
       ├── faktisk forbruk
       │
       ├── vannkraftproduksjon
       ├── vindkraftproduksjon
       ├── solkraftproduksjon
       │
       ├── NO1-NO2 fysisk flyt
       ├── NO1-NO3 fysisk flyt
       ├── NO1-NO5 fysisk flyt
       ├── NO2-NO5 fysisk flyt
       ├── NO3-NO4 fysisk flyt
       ├── NO3-NO5 fysisk flyt
       │
       └── tilgjengelighet/utfall

Da kan du begynne å undersøke hva som faktisk forklarer prisforskjellene mellom norske prisområder.

For eksempel:

$$ P_{NO1,t}-P_{NO2,t} = \beta_0 +\beta_1 Flow_{NO1-NO2,t} +\beta_2 Load_t +\beta_3 Hydro_t +\beta_4 Wind_t +\epsilon_t $$

Det blir en naturlig kobling mellom økonomi + statistikk + programmering + kraftsystemet, og du får brukt både tidyverse, visualisering og regresjon/ML.

Hvis du vil ha mest mulig interessante ENTSO-E-data å hente først, ville jeg prioritert:

Actual Total Load
Generation per Production Type
Physical Flows
Unavailability
Day-ahead prices
Scheduled commercial exchanges

Det fine er at ENTSO-E har API-støtte for strukturerte data tilbake til 2015, så dette kan også bli et ganske stort datasett hvis du vil analysere flere år.