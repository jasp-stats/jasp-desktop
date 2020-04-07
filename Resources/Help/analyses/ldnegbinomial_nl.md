Negatief-Binomiale Verdeling
==========================

Expositie van de negatief-binomiale verdeling.

## Toon verdeling
Geeft de theoretische negatief-binominale verdeling weer, met opgegeven parameterwaarden.

### Parameters
- &phi;, p: De negatief-binominale verdeling geparametriseerd op basis van het aantal successen en de kans op een success parameters.
- &phi;, &mu;: De negatief-binominale verdeling geparametriseerd met behulp van de spreiding en gemiddelde parameters.

### Weergeven
- Toelichtende tekst: Toont toelichtende tekst.
- Parameters, drager en momenten: Geeft de definitie van de parameters, de drager van de stochastische variabele en de momenten van de theoretische verdeling weer.
- Kansmassafunctie: Geeft de histogram van de kansmassafunctie weer.
- Cumulatieve verdelingsfunctie: Geeft de cumulatieve verdelingsgrafiek weer.

### Opties
- Bereik van x vanaf ... naar ...: Definieert de limieten van de *x*-as van de kansmassagrafiek en de cumulatieve verdelingsgrafiek.
- Markeer massa: Markeert de kansmassa op de kansmassagrafiek en de cumulatieve verdelingsgrafiek bij gespecificeerde waarden van x.
- Markeer cumulatieve verdeling: Markeert de kansmassa tussen de gespecificeerde waarden van x in de verdelingsgrafiek (gebied onder de curve), en markeert de cumulatieve kansen van de gespecifieerde waarden in de cumulatieve verdelingsgrafiek.
- Interval: Selecteer de grenzen van de weer te geven geordende verzameling: Kansmassa is gemarkeerd op de onder- en bovengrenzen, de verdeling wordt weergegeven voor het opgegeven interval.

## Gegevens genereren en weergeven
- Naam variabele: Specificeer hier de kolomnaam. Uit de gespecifieërde distributie wordt een steekproef getrokken en in de dataset opgeslagen met de gegeven naam.
- Steekproefgrootte: Specificeer hier de steekproefgrootte.
- Trek steekproef: Trekt een steekproef uit de theoretische verdeling.

- Selecteer variabele uit dataset: Selecteer een variabele om weer te geven en te passen bij de theoretische verdeling.

### Statistieken
- Samenvatting: Geeft een tabel met de kengetallen van de geselecteerde variabele weer.
- Eerste ... waargenomen momenten: Geeft een tabel weer met de empirische gewone en centrale momenten van de geselecteerde variabele. Staat standaard op de eerste 2 momenten.

### Grafieken
- Staafdiagram: Toont een staafdiagram van de geselecteerde variabele.
- Empirische cumulatieve verdeling: Geeft een empirische cumulatieve verdelingsgrafiek van de geselecteerde variabele weer.

## Schat Parameters
- Meest aannemelijke schatter: Schat de parameters met de waarden in het domein waar de aannemelijksheidsfunctie maximaal is. De aannemelijksheidsfunctie zet het gegevensargument (op basis van de geselecteerde variabele) in de theoretische kansmassafunctie vast, en beschouwt deze als een functie van de parameters. De optimalisatieprocedure wordt geïnitialiseerd met de waarden voor de parameters die zijn ingevoerd onder "Toon Verdeling".

### Uitvoer
- Schattingen: Geeft een tabel weer met de parameterschattingen. De getoonde parameters passen zich aan aan de gekozen parametrisatie.
- Std. error: Geeft de standaardfout van de parameterschattingen weer.
- Betrouwbaarheidsinterval: Geeft het betrouwbaarheidsinterval van de parameters weer op het opgegeven betrouwbaarheidsniveau.

## Evalueer Passing

### Grafieken
- Histogram vs. theoretische pmf: Geeft een histogram weer van de geselecteerde variabele en ook de kansmassafunctie van de gepaste distributie.
- Empirische vs. theoretische cdf: Geeft een empirische cumulatieve verdelingsgrafiek weer, en ook de cumulatieve verdelingsfunctie van de gepaste distributie.
- Q-Q plot: Geeft de kwantiel-kwantielgrafiek weer. De *x*-as toont de theoretische kwantielen van de gepaste verdeling, de *y*-as toont de empirische kwantielen van de geselecteerde variabele.
- P-P plot: Geeft de kansverdelings-kansverdelingsgrafiek weer. De *x*-as toont de theoretische waarde van de gepaste cumulatieve verdelingsfunctie, de *y*-as toont de empirische percentielen van de geselecteerde variabele.

### Statistieken
- Chi-kwadraat: Toont de chi-kwadraat pasmaat.
