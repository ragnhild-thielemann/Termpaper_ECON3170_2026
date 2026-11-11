
# Bruk av AI i kildeuthentingen

I denne mappen ligger skriptene som er brukt til å hente inn data fra de ulike datakildene. Koden er strukturert i fire mapper: `henting_av_data_NVE`, `henting_av_data_nedbor`, `henting_av_data_ENTSO-E`.

Nedbørsdataene er ikke benyttet i det endelige prosjektet, men skriptene er likevel inkludert i innleveringen for å dokumentere arbeidet med datainnhentingen. 

Bruk av AI i kildehentingen
Jeg har benyttet ChatGPT som støtte til å identifisere relevante datasett og forstå dokumentasjonen til ENTSO-E og NVE. Promptene har i hovedsak tatt utgangspunkt i hvilke sammenhenger jeg ønsket å undersøke i analysen, og hvilke variabler jeg trengte for å belyse disse. Jeg har blant annet bedt ChatGPT om hjelp til å identifisere relevante API-endepunkter, parametere og dataforespørsler for å hente ut strømpriser, kraftforbruk, kraftflyt, magasinfylling, vannføring og nedbørsdata.

For ENTSO-E spesifiserte jeg hvilke prisområder og omkringliggende europeiske markeder jeg ønsket data fra, hvilke typer observasjoner jeg trengte, og hvilken tidsperiode dataene skulle dekke (2020–2026). For NVE ba jeg om hjelp til å finne relevante hydrologiske datasett, blant annet magasinfylling, tilsig og nedbør, samt hvordan målinger fra ulike målestasjoner kunne hentes ut og sammenstilles.

Arbeidet foregikk iterativt: Jeg beskrev ønsket datainnhold, fikk forslag til API-kall og kode, og justerte forespørslene underveis basert på dokumentasjonen og resultatene jeg fikk. Jeg brukte også ChatGPT til å forstå feilmeldinger og rette opp syntaks og parametere. Selve API-kallene ble gjennomført av meg ved hjelp av egne API-nøkler.
