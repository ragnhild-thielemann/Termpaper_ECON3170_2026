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