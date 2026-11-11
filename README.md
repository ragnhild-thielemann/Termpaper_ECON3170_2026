# Semesteroppgave ECON3170 – Analyse av kraftmarkedet

## Om prosjektet

Dette prosjektet undersøker sammenhenger mellom norske strømpriser og utvalgte faktorer i kraftmarkedet. Analysen ser blant annet på strømforbruk, kraftproduksjon, vannmagasiner og strømpriser i europeiske markeder.

Datagrunnlaget hentes hovedsakelig fra ENTSO-E Transparency Platform og Norges vassdrags- og energidirektorat (NVE). Dataene samles inn og bearbeides i separate R-skript før de kombineres til datasett som brukes i den videre analysen.

Prosjektet har en utforskende og prediktiv tilnærming. Målet er å undersøke statistiske sammenhenger i dataene og vurdere hvor godt utvalgte variabler kan forklare eller predikere norske strømpriser over tid. Analysen er ikke ment å identifisere kausale sammenhenger.

## Mappestruktur og arbeidsflyt

Prosjektet er strukturert med separate skript for datainnsamling, databehandling og analyse. Denne inndelingen gir en ryddig arbeidsflyt, gjør koden enklere å vedlikeholde og gir bedre oversikt over datakildene og de ulike stegene i arbeidet. Den reduserer også behovet for å gjenta API-kall og beregninger. Prosjektmappene er organisert slik:

- Datasett inneholder data som er hentet inn og bearbeidet gjennom prosjektet. Her lagres både mellomliggende datasett fra de ulike stegene i databehandlingen og datasett som benyttes i den endelige analysen. Dette gjør det mulig å gjenbruke data uten å måtte gjennomføre API-kall og beregninger på nytt.

- Analyse inneholder skriptene for den endelige analysen, samt prosjektbeskrivelsen som redegjør for prosjektets formål, datagrunnlag og metodiske tilnærming.

- Kilder inneholder skriptene for innhenting av data fra de ulike datakildene, hovedsakelig ENTSO-E og NVE. Skriptene omfatter funksjoner for datauthenting, tilhørende API-kall og nødvendig bearbeiding av dataene i forbindelse med innhentingen.
# Bruk av AI i kildeuthentingen

I denne mappen ligger skriptene som er brukt til å hente inn data fra de ulike datakildene. Koden er strukturert i fire mapper: `henting_av_data_NVE`, `henting_av_data_nedbor`, `henting_av_data_ENTSO-E`.

Nedbørsdataene er ikke benyttet i det endelige prosjektet, men skriptene er likevel inkludert i innleveringen for å dokumentere arbeidet med datainnhentingen. 

Bruk av AI i kildehentingen
Jeg har benyttet ChatGPT som støtte til å identifisere relevante datasett og forstå dokumentasjonen til ENTSO-E og NVE. Promptene har i hovedsak tatt utgangspunkt i hvilke sammenhenger jeg ønsket å undersøke i analysen, og hvilke variabler jeg trengte for å belyse disse. Jeg har blant annet bedt ChatGPT om hjelp til å identifisere relevante API-endepunkter, parametere og dataforespørsler for å hente ut strømpriser, kraftforbruk, kraftflyt, magasinfylling, vannføring og nedbørsdata.

For ENTSO-E spesifiserte jeg hvilke prisområder og omkringliggende europeiske markeder jeg ønsket data fra, hvilke typer observasjoner jeg trengte, og hvilken tidsperiode dataene skulle dekke (2020–2026). For NVE ba jeg om hjelp til å finne relevante hydrologiske datasett, blant annet magasinfylling, tilsig og nedbør, samt hvordan målinger fra ulike målestasjoner kunne hentes ut og sammenstilles.

Arbeidet foregikk iterativt: Jeg beskrev ønsket datainnhold, fikk forslag til API-kall og kode, og justerte forespørslene underveis basert på dokumentasjonen og resultatene jeg fikk. Jeg brukte også ChatGPT til å forstå feilmeldinger og rette opp syntaks og parametere. Selve API-kallene ble gjennomført av meg ved hjelp av egne API-nøkler.
