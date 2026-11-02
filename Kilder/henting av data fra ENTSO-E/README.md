---
title: "README"
output: html_document
---
Mappen `henting av data fra ENTSO-E` inneholder skript med funksjonsdefinisjoner for uthenting av ulike datatyper fra ENTSO-E sitt API. Funksjonene er fordelt på separate skript etter hva slags data de henter eller behandler:

- `funksjon_henteAPInokkel.R`: håndterer uthenting av API-nøkkel.
- `funksjon_henteeickode.R`: inneholder funksjoner for å finne EIC-koder til aktuelle prisområder og land.
- `funksjon_hentemarkedspriser_ENTSO-E.R`: henter markedspriser.
- `funksjon_henteforventing_produksjo_ENTSO-E.R`: henter data om forventet produksjon.
- `funksjon_produksjonstype.R`: henter produksjonsdata fordelt på produksjonstype.
- `henting_stromflyt.R`: inneholder funksjonen for å hente strømflyt mellom prisområder.

Selve datauthentingen er skilt fra funksjonsdefinisjonene og ligger i undermappen `hente_ut_datasett`. Her kalles de relevante funksjonene med angitte tidsperioder, prisområder og andre nødvendige argumenter. Resultatet er en modulær struktur der funksjonene kan gjenbrukes på tvers av datautak, samtidig som det er tydelig hvilke skript som definerer funksjonaliteten, og hvilke som gjennomfører datauthentingen.