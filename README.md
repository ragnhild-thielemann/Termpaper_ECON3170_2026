# Semesteroppgave ECON3170 – Analyse av kraftmarkedet

## Om prosjektet

Dette prosjektet undersøker sammenhenger mellom norske strømpriser og utvalgte faktorer i kraftmarkedet. Analysen ser blant annet på strømforbruk, kraftproduksjon, vannmagasiner og strømpriser i europeiske markeder.

Datagrunnlaget hentes hovedsakelig fra ENTSO-E Transparency Platform og Norges vassdrags- og energidirektorat (NVE). Dataene samles inn og bearbeides i separate R-skript før de kombineres til datasett som brukes i den videre analysen.

Prosjektet har en utforskende og prediktiv tilnærming. Målet er å undersøke statistiske sammenhenger i dataene og vurdere hvor godt utvalgte variabler kan forklare eller predikere norske strømpriser over tid. Analysen er ikke ment å identifisere kausale sammenhenger.

## Mappestruktur og arbeidsflyt

Prosjektet er organisert slik at datainnsamling, databehandling og analyse gjennomføres i separate skript. Dette gjør det enklere å holde oversikt over datakildene, vedlikeholde koden og unngå unødvendig gjentakelse av beregninger og API-kall.

- Mappen `Datasett` inneholder datasett som er hentet inn og bearbeidet underveis i prosjektet. Her lagres både mellomliggende datasett fra de ulike stegene i databehandlingen og datasett som benyttes i den endelige analysen. Dette gjør det mulig å gjenbruke allerede innhentede og bearbeidede data, uten å måtte gjennomføre alle API-kall og beregninger på nytt.
- Mappen `Analyse` inneholder den ferdige analysen, samt en prosjektbeskrivelse som redegjør for prosjektets formål, datagrunnlag og metodiske tilnærming.
- Mappen `Kilder` inneholder skriptene som brukes til å hente inn data fra de ulike datakildene, hovedsakelig ENTSO-E og NVE. Skriptene inneholder funksjoner for datauthenting og tilhørende API-kall.