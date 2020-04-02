Bernoulli Verdeling
==========================

Expositie van de Bernoulli verdeling.

## Toon Verdeling

Geeft de theoretische Bernoulli verdeling weer, met opgegeven parameterwaarden.

### Parameter
- p: Kans op "succes".

### Weergeven
- Toelichtende tekst: Toont toelichtende tekst.
- Parameters, drager en momenten: Geeft de definitie van de parameters, de drager van de stochastische variabele en de momenten van de theoretische verdeling weer.
- Kansmassfunctie: Geeft de histogram van de kansmassfunctie weer.
- Cumulatieve verdelingsfunctie: Geeft de cumulatieve verdelingsgrafiek weer.

## Genereer en Toon Gegevens
- Naam variabele: Specificeer hier de kolom naam. Uit de gespecifieërde distributie wordt een steekproef getrokken en in de dataset opgeslagen met de gegeven naam.
- Steekproefgrootte: Specificeer hier de steekproefgrootte.
- Trek steekproef: Trekt een steekproef uit de theoretische verdeling.

- Selecteer variabele uit dataset: Selecteer een variabele om weer te geven en te passen bij de theoretische verdeling.

### Statistieken
- Samenvatting: Geeft een tabel met de kengetallen van de geselecteerde variabele weer.

### Grafieken
- Staafdiagram: Toont een staafdiagram van de geselecteerde variabele.

## Schat Parameters
- Meest aannemelijke schatter: Schat de parameters met de waarden in het domein waar de aannemelijksheidsfunctie maximaal is. De aannemelijksheidsfunctie zet het gegevensargument (op basis van de geselecteerde variabele) in de theoretische kansmassafunctie vast, en beschouwt deze als een functie van de parameters. De optimalisatieprocedure wordt geïnitialiseerd met de waarden voor de parameters die zijn ingevoerd onder "Toon Verdeling".

### Uitvoer
- Schattingen: Geeft een tabel weer met de parameterschattingen. De getoonde parameters passen zich aan aan de gekozen parametrisatie.
- Std. error: Geeft de standaardfout van de parameterschattingen weer.
- Betrouwbaarheidsinterval: Geeft het betrouwbaarheidsinterval van de parameters weer op het opgegeven betrouwbaarheidsniveau.


## Evalueer Passing

### Grafieken
- Histogram vs. theoretische pmf (kansmassfunctie): Geeft een histogram weer van de geselecteerde variabele en ook de kansmassafunctie van de gepaste distributie.
- Empirische vs. theoretische cdf (cumulatieve verdelingsfunctie): Geeft een empirische cumulatieve verdelingsgrafiek weer, en ook de cumulatieve verdelingsfunctie van de gepaste distributie.
- P-P-plot: Geeft de kansverdelings-kansverdelingsgrafiek weer. De *x*-as toont de theoretische waarde van de gepaste cumulatieve verdelingsfunctie, de *y*-as toont de empirische percentielen van de geselecteerde variabele.
