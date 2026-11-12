# Semesteroppgave ECON3170 – Analyse av kraftmarkedet

## Om prosjektet

Dette prosjektet undersøker sammenhenger mellom norske strømpriser og utvalgte faktorer i kraftmarkedet. Analysen ser blant annet på strømforbruk, kraftproduksjon, vannmagasiner og strømpriser i europeiske markeder.

Datagrunnlaget hentes hovedsakelig fra ENTSO-E Transparency Platform og Norges vassdrags- og energidirektorat (NVE). Dataene samles inn og bearbeides i separate R-skript før de kombineres til datasett som brukes i den videre analysen.

Prosjektet har en utforskende og prediktiv tilnærming. Målet er å undersøke statistiske sammenhenger i dataene og vurdere hvor godt utvalgte variabler kan forklare eller predikere norske strømpriser over tid. Analysen er ikke ment å identifisere kausale sammenhenger.

## Mappestruktur og arbeidsflyt

Prosjektet er strukturert med separate skript for datainnsamling, databehandling og analyse. Denne inndelingen gir en ryddig arbeidsflyt, gjør koden enklere å vedlikeholde og gir bedre oversikt over datakildene og de ulike stegene i arbeidet. Den reduserer også behovet for å gjenta API-kall og beregninger. Prosjektmappene er organisert slik:

- `Datasett` inneholder datasett som er hentet inn og bearbeidet gjennom prosjektet. Mappen omfatter både mellomliggende datasett fra de ulike stegene i databehandlingen og datasett som benyttes i den endelige analysen. Lagringen gjør det mulig å gjenbruke data uten å måtte gjennomføre API-kall og beregninger på nytt.

- `analyse` inneholder skriptene for gjennomføring av den endelige analysen, samt prosjektbeskrivelsen. Prosjektbeskrivelsen redegjør for prosjektets formål, datagrunnlag og metodiske tilnærming.

- `Kilder` inneholder skriptene for innhenting av data fra de ulike datakildene, hovedsakelig ENTSO-E og NVE. Mappen omfatter funksjoner for datauthenting, tilhørende API-kall og den innledende bearbeidingen av dataene.