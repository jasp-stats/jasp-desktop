Bernoulli Verdeling
==========================

Demonstratie van de Bernoulli verdeling.

## Toon distributie

Geeft de theoretische Bernoulli verdeling weer, met opgegeven parameterwaarden.

### Parameter
- p: Kans op "succes"

### Scherm

- Verklarende tekst: Toont verklarende tekst
- Parameters, ondersteuning en momenten: Geeft de definitie van parameters, de ondersteuning van de willekeurige variabele en de momenten van de theoretische verdeling weer
- Waarschijnlijkheidsmassafunctie: Geeft de waarschijnlijkheidsmassaplot weer
- Cumulatieve verdelingsfunctie: Geeft de cumulatieve verdelingsgrafiek weer


## Gegevens genereren en weergeven
- Variabelenaam: geef de naam van de variabele op. Eenmaal gevuld, maakt u een lege kolom in de huidige dataset.
- Aantal monsters: specificeer het aantal monsters.
- Trek monsters: steekproef uit de theoretische verdeling.

- Haal variabele uit dataset: Selecteer een variabele om weer te geven en te passen bij de theoretische verdeling.

### Statistieken
- Beschrijvingen: Geeft een beschrijvende tabel van de geselecteerde variabele weer.
- Eerste ... momenten: Geeft een tabel weer met de onbewerkte en centrale voorbeeldmomenten van de geselecteerde variabele. Staat standaard op de eerste 2 momenten.

### Plots
- Barplot: toon een barplot van de geselecteerde variabele.
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
- Chi-kwadraat: Toont de chi-kwadraat goedheidstest
