---
title: "README"
output: html_document
---

Datasettet over strømprisene er enormt, og krever derfor god kode for å få ut på en fornuftig måte. Når vi skal hente ut datasettet, oppretter vi en sjult fil som inneholder tilgangsnøkkelen til API-et. Deretter henter vi denne frem, og lager en testfunksjon der vi henter ut dataen fra Østerrike, for å undersøke strukturen til API-et. Datasettet er en XML-fil, som vi må håndtere, før vi får dataen på r-format. 





API-nøkkel → ENTSO-E API → XML → prisdata → tidspunkt → R-datasett