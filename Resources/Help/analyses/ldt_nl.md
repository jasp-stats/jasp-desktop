Student's t-verdeling
==========================

Demonstratie van de t-verdeling.

## Toon distributie

Geeft de theoretische student's t-verdeling weer, gegeven de opgegeven parameterwaarden.

### Parameters

- df: vrijheidsgraden
- ncp: niet-centraliteitsparameter

### Scherm

- Verklarende tekst: Toont verklarende tekst
- Parameters, ondersteuning en momenten: Geeft de definitie van parameters, de ondersteuning van de willekeurige variabele en de momenten van de distributie weer
- Kansdichtheidsfunctie: Geeft de kansdichtheidsgrafiek weer
- Cumulatieve verdelingsfunctie: Geeft de cumulatieve verdelingsgrafiek weer
- Kwantielfunctie: Geeft de kwantielgrafiek weer

### Opties

- Bereik van x van ... tot ...: definieert de limieten van de x-as van de kansdichtheidsgrafiek en de cumulatieve verdelingsgrafiek, en de limieten van de y-as van de kwantielgrafiek.
- Highlight density: Markeert de kansdichtheid op de kansdichtheidsgrafiek en de cumulatieve verdelingsgrafiek bij gespecificeerde waarden van x
- Waarschijnlijkheid markeren: markeert de waarschijnlijkheid tussen de gespecificeerde waarden van x in de dichtheidsplot (gebied onder de curve) en markeert de cumulatieve waarschijnlijkheid bij de gespecificeerde waarden in de cumulatieve verdelingsgrafiek
- Interval: selecteer de grenzen van het weer te geven interval: Dichtheid is gemarkeerd op de onder- en bovengrenzen, de waarschijnlijkheid wordt weergegeven voor het opgegeven interval.

## Gegevens genereren en weergeven
- Variabelenaam: geef de naam van de variabele op. Eenmaal gevuld, maakt u een lege kolom in de huidige dataset.
- Aantal monsters: specificeer het aantal monsters.
- Trek monsters: steekproef uit de theoretische verdeling.

- Haal variabele uit dataset: Selecteer een variabele om weer te geven en te passen bij de theoretische verdeling.

### Statistieken
- Beschrijvingen: Geeft een beschrijvende tabel van de geselecteerde variabele weer.
- Eerste ... momenten: Geeft een tabel weer met de onbewerkte en centrale voorbeeldmomenten van de geselecteerde variabele. Staat standaard op de eerste 2 momenten.

### Plots
- Histogram met ... bakken: Toon een histogram van de geselecteerde variabele met het aantal gespecificeerde bakken.
- Empirische cumulatieve verdeling: geeft een empirische cumulatieve verdelingsgrafiek van de geselecteerde variabele weer.

## Parameters schatten
- Maximale waarschijnlijkheid: pas de theoretische verdeling aan de geselecteerde variabele aan door de waarschijnlijkheidsfunctie te maximaliseren. De parameterwaarden die zijn gespecificeerd in het gedeelte Verdeling tonen dienen als startpunt voor de procedure.

### Uitvoer
- Schattingen: geeft een tabel weer met de parameterschattingen. Het wijzigen van parametrisering verandert welke parameters worden weergegeven.
- Soa. error: Geeft de standaardfout van de parameterschattingen weer
- Betrouwbaarheidsinterval: geeft het betrouwbaarheidsinterval van de parameters weer op het opgegeven betrouwbaarheidsniveau.


## Fit beoordelen

### Plots
- Histogram vs. theoretische pdf: geeft een histogram weer van de geselecteerde variabele overlay met de kansdichtheidsfunctie van de gepaste distributie
- Empirische vs. theoretische cdf: Geeft een empirische cumulatieve verdelingsgrafiek weer, overlay met de cumulatieve verdelingsfunctie van de gepaste distributie
- Q-Q-plot: geeft de kwantiel-kwantielgrafiek weer. De * x * -as toont de theoretische kwantielen van de gegevenspunten onder de gepaste verdeling, de * y * -as toont de empirische kwantielen van de geselecteerde variabele.
- P-P-plot: geeft de kans-waarschijnlijkheidsgrafiek weer. De * x * -as toont de theoretische waarde van de cumulatieve dichtheidsfunctie van de gegevenspunten onder de gepaste verdeling, de * y * -as toont de empirische percentielen van de geselecteerde variabele.

### Statistieken
- Kolmogorov-Smirnov: Geeft de Kolmogorov-Smirnov-test weer
- Cramer von Misses: Geeft de Cramer von Misses-test weer
- Anderson-Darling: Geeft de Anderson-Darling-test weer
