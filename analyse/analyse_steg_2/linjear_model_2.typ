// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#show: doc => article(
  title: [linjear\_model\_2],
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Kraftproduksjonen i Norge #footnote[Se analyse/analyse\_steg\_1/produksjonskilder\_i\_ulikeprisomrader.R]
<kraftproduksjonen-i-norge>
For å danne et overordnet bilde av den norske kraftmiksen viser figuren gjennomsnittlig strømproduksjon per time (MW) fordelt på produksjonskilder og prisområder i perioden 2020--2026. Figuren viser tydelig at vannkraft med magasin utgjør en stor del av den norske strømproduksjonen i alle prisområder. Det er imidlertid geografiske forskjeller i sammensetningen. Mens prisområde NO1 kjennetegnes av en betydelig andel uregulert elvekraft, har NO2 og NO3 en større andel vindkraft på land. Samlet sett viser figuren at den norske kraftproduksjonen i stor grad er basert på regulerbare vannkraftressurser.

#box(image("../../Plott/soylediagram.png"))

Basert på dette ønsker vi å gå videre med en formell økonometrisk analyse for å predikere produsert strømmengde. Vi konstruerer derfor en lineær regresjonsmodell med normalfordelte feilledd på formen

$ P_(t \, j) = hat(b)_(0 \, j) + hat(b)_(1 \, j) x_(1 \, t) + hat(b)_(2 \, j) x_(2 \, t) + dots.h.c + hat(b)_(k \, j) x_(k \, t) + epsilon_(t \, j) \, $

der $P_(t \, j)$ betegner produsert strømmengde for kraftverkstype $j$ på tidspunkt $t$. Her angir $j = 1 \, 2$ de to kraftverkstypene som analyseres (elvekraft og vannkraft med magasin), mens $t$ angir de observerte tidspunktene. $x_(1 \, t) \, dots.h x_(k \, t)$ er de ulike forklaringsvariablene, og $hat(b)_(0 \, j) \, dots.h \, hat(b)_(k \, j)$ er de estimerte regresjonskoeffisientene. Feilleddet (\_{t,j}) representerer variasjon i produksjonen som ikke forklares av modellen.

I modellen er produsert strøm, målt i MW, responsvariabel. Kovariatene vi ønsker å undersøke er:

- #strong[Spotpris:] Markedsprisen på det gitte tidspunktet. Ettersom spotprisen fastsettes dagen i forveien på kraftbørsen Nord Pool, er den kjent for produsentene når produksjonen for den påfølgende dagen planlegges.
- #strong[Forbruk:] Det samlede strømforbruket i det aktuelle prisområdet. Forbruket fungerer som et mål på etterspørselen etter elektrisitet og kan dermed bidra til å forklare variasjoner i produksjonen.
- #strong[Akkumulert nedbør:] Fanger opp de hydrologiske forholdene og tilgangen på vannressurser over tid.
- #strong[Nedbør:] Måler nedbørsmengden på det aktuelle tidspunktet, og kan særlig ha betydning for uregulerbar elvekraft.

== Rammeverk for modellen #footnote[Se fullstendig kode i vedlegget]
<rammeverk-for-modellen>
Vi setter opp modellen i et standard rammeverk for maskinlæring, der datasettet deles i et treningssett og et testsett med henholdsvis 80 og 20 prosent av observasjonene. Datasettet deles med hensyn på prisområde, slik at hvert prisområde er representert i både trenings- og testsettet. Deretter definerer vi en `recipe` der responsvariabelen og de ulike forklaringsvariablene konstrueres og klargjøres for modellering. Modellen trenes deretter på treningsdataene og evalueres på testdataene.

Fra modellens #emph[summary] får vi blant annet estimerte regresjonskoeffisienter for de ulike forklaringsvariablene, samt deres standardavvik. Vi får også en $R^2$-verdi, som angir hvor stor andel av variasjonen i den observerte produksjonen som modellen klarer å forklare.

Akkumulert nedbør er én av flere forklaringsvariabler i modellen. For å undersøke betydningen av nedbør over ulike tidsperioder estimerer vi modellen med akkumulert nedbør beregnet over henholdsvis 1, 7, 14 og 30 dager. På denne måten kan vi undersøke hvordan estimatet for nedbørkoeffisienten endres når nedbør akkumuleres over lengre perioder.

I plottet vises alle de estimerte parameterne i modellen for de ulike tidsperiodene, med et 95 prosent konfidensintervall rundt hvert estimat. Dette gjør det mulig å sammenligne både størrelsen og usikkerheten til de estimerte koeffisientene, samtidig som vi kan se hvordan effekten av akkumulert nedbør utvikler seg fra 1 til 30 dager.

I plottet vises de estimerte parameterne i modellene for de ulike tidsperiodene, med et 95 prosent konfidensintervall rundt hvert estimat. Dette gjør det mulig å sammenligne både størrelsen og usikkerheten til koeffisientene, samtidig som vi kan se hvordan effekten av akkumulert nedbør utvikler seg fra 0 til 30 dager.

For modellen for vannkraft med magasin er $R^2$ mellom 0,02 og 0,05. Modellen forklarer dermed bare mellom 2 og 5 prosent av variasjonen i produksjonen, og forklaringskraften til modellen er svært begrenset.

Modellen for elvekraft har derimot en $R^2$ på om lag 16 prosent. Selv om også denne forklaringskraften er begrenset, forklarer modellen en større andel av variasjonen i produksjonen og kan derfor brukes til å undersøke sammenhengene mellom variablene. Av plottet ser vi at nedbør samme dag har en positiv regresjonskoeffisient på omtrent 0,5 for elvekraft. Pris og forbruk fremstår også som signifikante forklaringsvariabler. Når nedbøren akkumuleres over lengre perioder, blir koeffisienten for nedbør mindre tydelig og konfidensintervallene overlapper i større grad med null. Dette tyder på at den direkte sammenhengen mellom nedbør og elvekraftproduksjon er sterkest for nedbør samme dag, og at effekten blir svakere når vi ser på lengre akkumulerte perioder.

#box(image("../../Plott/lm_model_verson_2.png"))

#block[
#box(image("../../Plott/R_2_verson2.png"))

]
== Refereanser
<refereanser>
\[^Se fullstendig kode i vedlegget\] : Se #emph[analyse/analyse\_steg\_1/linear\_model.R] for fullstendig kode
