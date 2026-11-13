# Datainnhenting fra eksterne kilder

Denne mappen inneholder R-skriptene som er benyttet til å hente inn data fra de eksterne datakildene NVE og ENTSO-E. Koden er organisert i følgende undermapper:

- `henting_av_data_NVE`: Skript for innhenting av hydrologiske data fra NVE, blant annet magasinfylling og vannføring.
- `henting_av_data_nedbor`: Skript for innhenting av nedbørsdata.
- `henting_av_data_ENTSO-E`: Skript for innhenting av strømpriser, kraftforbruk og kraftflyt fra ENTSO-E.

Nedbørsdataene er ikke benyttet i den endelige analysen, men skriptene er inkludert for å dokumentere arbeidet med datainnhentingen og hvilke datakilder som ble undersøkt.

## Bruk av generativ KI i datainnhentingen

ChatGPT har blitt benyttet som et støtteverktøy for å identifisere relevante datasett, tolke API-dokumentasjonen og utforme kode for datainnhenting. Promptene tok utgangspunkt i hvilke sammenhenger som skulle undersøkes i analysen, og hvilke variabler som var nødvendige for å belyse disse.

For ENTSO-E omfattet arbeidet blant annet identifisering av relevante prisområder og omkringliggende europeiske markeder, aktuelle datatyper, API-parametere og tidsavgrensninger (2020–2026). For NVE ble ChatGPT brukt til å finne relevante hydrologiske datasett, undersøke tilgjengelige endepunkter og forstå hvordan målinger fra ulike målestasjoner kunne hentes ut og sammenstilles.

Arbeidet foregikk iterativt. Jeg beskrev ønsket datainnhold, fikk forslag til API-forespørsler og R-kode, og videreutviklet løsningene gjennom testing og feilsøking. ChatGPT ble også brukt til å tolke feilmeldinger, identifisere feil i eksisterende kode og foreslå forbedringer. Jeg har selv vurdert og tilpasset kodeforslagene og gjennomført samtlige API-kall.

## API-nøkler og reproduksjon av datainnhentingen

Prosjektet benytter en `.Renviron`-fil til å lagre API-nøkler for tilgang til ENTSO-E og NVE. Filen inneholder private tilgangsnøkler og er derfor ikke inkludert i innleveringen.

Dersom en annen bruker ønsker å gjennomføre datainnhentingen på nytt, må vedkommende opprette egne API-nøkler hos de respektive dataleverandørene og lagre disse i en lokal `.Renviron`-fil. Variabelnavnene må samsvare med dem som er benyttet i R-skriptene.
