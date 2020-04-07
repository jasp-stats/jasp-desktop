F-Verdeling
==========================

Expositie van de F-verdeling.

## Toon Verdeling
Geeft de theoretische F-verdeling weer, met opgegeven parameterwaarden.

### Parameters
- vg 1: De eerste vrijheidsgraadparameter bepaalt het aantal vrije parameters in een gerestricteerde (Gaussische) model.
- vg 2: De tweede vrijheidsgraadparameter bepaalt het aantal vrije parameters in de overkoepelende (Gaussische) model.
- ncp: De niet-centraliteitsparameter representeert de hypothetische afwijking van de som van het kwadratisch verschil.

### Weergeven
- Toelichtende tekst: Toont toelichtende tekst.
- Parameters, drager en momenten: Geeft de definitie van de parameters, de drager van de stochastische variabele en de momenten van de theoretische verdeling weer.
- Kansdichtheidsfunctie: Geeft de kansdichtheidsgrafiek weer.
- Cumulatieve verdelingsfunctie: Geeft de cumulatieve verdelingsgrafiek weer.
- Kwantielfunctie: Geeft de kwantielgrafiek weer.

### Opties
- Bereik van x vanaf ... naar ...: Definieert de limieten van de *x*-as van de kansdichtheidsgrafiek en de cumulatieve verdelingsgrafiek, en de limieten van de *y*-as van de kwantielgrafiek.
- Markeer dichtheid: Markeert de kansdichtheid op de kansdichtheidsgrafiek en de cumulatieve verdelingsgrafiek bij gespecificeerde waarden van x.
- Markeer cumulatieve verdeling: Markeert de kansdichtheid tussen de gespecificeerde waarden van x in de verdelingsgrafiek (gebied onder de curve), en markeert de cumulatieve kansen van de gespecifieerde waarden in de cumulatieve verdelingsgrafiek.
- Interval: Selecteer de grenzen van het weer te geven interval: Dichtheid is gemarkeerd op de onder- en bovengrenzen, de verdeling wordt weergegeven voor het opgegeven interval.

## Gegevens genereren en weergeven
- Naam variabele: Specificeer hier de kolomnaam. Uit de gespecifieërde distributie wordt een steekproef getrokken en in de dataset opgeslagen met de gegeven naam.
- Steekproefgrootte: Specificeer hier de steekproefgrootte.
- Trek steekproef: Trekt een steekproef uit de theoretische verdeling.

- Selecteer variabele uit dataset: Selecteer een variabele om weer te geven en te passen bij de theoretische verdeling.

### Statistieken
- Samenvatting: Geeft een tabel met de kengetallen van de geselecteerde variabele weer.
- Eerste ... waargenomen momenten: Geeft een tabel weer met de empirische gewone en centrale momenten van de geselecteerde variabele. Staat standaard op de eerste 2 momenten.

### Grafieken
- Histogram met ... klassen: Toon een histogram van de geselecteerde variabele met het aantal gespecificeerde klassen.
- Empirische cumulatieve verdeling: Geeft een empirische cumulatieve verdelingsgrafiek van de geselecteerde variabele weer.

## Schat Parameters
- Meest aannemelijke schatter: Schat de parameters met de waarden in het domein waar de aannemelijksheidsfunctie maximaal is. De aannemelijksheidsfunctie zet het gegevensargument (op basis van de geselecteerde variabele) in de theoretische kansdichtsheidsfunctie vast, en beschouwt deze als een functie van de parameters. De optimalisatieprocedure wordt geïnitialiseerd met de waarden voor de parameters die zijn ingevoerd onder "Toon Verdeling".

### Uitvoer
- Schattingen: Geeft een tabel weer met de parameterschattingen. De getoonde parameters passen zich aan aan de gekozen parametrisatie.
- Std. error: Geeft de standaardfout van de parameterschattingen weer.
- Betrouwbaarheidsinterval: Geeft het betrouwbaarheidsinterval van de parameters weer op het opgegeven betrouwbaarheidsniveau.

## Evalueer Passing

### Grafieken
- Histogram vs. theoretische pdf: Geeft een histogram weer van de geselecteerde variabele en ook de kansdichtheidsfunctie van de gepaste distributie.
- Empirische vs. theoretische cdf: Geeft een empirische cumulatieve verdelingsgrafiek weer, en ook de cumulatieve verdelingsfunctie van de gepaste distributie.
- Q-Q plot: Geeft de kwantiel-kwantielgrafiek weer. De *x*-as toont de theoretische kwantielen van de gepaste verdeling, de *y*-as toont de empirische kwantielen van de geselecteerde variabele.
- P-P plot: Geeft de kansverdelings-kansverdelingsgrafiek weer. De *x*-as toont de theoretische waarde van de gepaste cumulatieve verdelingsfunctie, de *y*-as toont de empirische percentielen van de geselecteerde variabele.

### Statistieken
- Kolmogorov-Smirnov: Geeft de pasmaat en p-waarde van de Kolmogorov-Smirnov toets weer.
- Cramér-von Mises: Geeft de pasmaat en p-waarde van de Cramér-von Mises toets weer.
- Anderson-Darling: Geeft de pasmaat en p-waarde van de Anderson-Darling toets weer.
