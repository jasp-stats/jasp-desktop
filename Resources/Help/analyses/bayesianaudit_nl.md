Bayesiaanse Audit Werkflow
==========================

De taak van een Auditor is om een oordeel te maken over de eerlijkheid van gepresenteerde transacties in een populatie, en te bepalen of er fouten in de populatie zitten die materieel zijn. Als de auditor toegang heeft tot de populatiedata kan kan hij/zij gebruik maken van de *Bayesiaanse audit werkflow* om te berekenen hoeveel steekproeven er moeten worden geëvalueerd om tot een bepaald niveau van zekerheid in het oordeel te komen. Er worden dan steekproeven uit de data getrokken, deze worden geïnspecteerd en er wordt een stelling gemaakt over het aantal fouten in de populatie. De workflow stelt de auditor in staat de correcte beslissingen te maken in dit proces. De *Bayesiaanse audit werkflow* kan de risicoschattingen van het *audit risico model* gebruiken om deze informatie in een prior verdeling op te nemen, welke wordt geüpdatet met informatie van de data om tot een posterior verdeling te komen. Inferenties over de populatie fouten worden gemaakt op basis van de posterior.

----

Werkflow
-----------
De Bayesiaanse audit werkflow bestaat uit vier aparte stadia, elk met een eigen doel:
- Planning: Bereken de steekproefgrootte die nodig is voor het gewenste oordeel over de populatie. 
- Selectie: Selecteer de benodigde observaties uit je populatie.
- Uitvoering: Annoteer je data met je oordeel over de eerlijkheid van de geselecteerde observaties.
- Evaluatie: Maak een oordeel over de populatie op basis van je geannoteerde selectie.

----

Standaard opties
-------
### Materialiteit van de populatie:
- Absoluut: Voer de materialiteit van je populatie in als geldwaarde.
- Relatief: Voer de materialiteit van je populatie in als percentage van de totaalwaarde.

### Audit Risico
- Zekerheid: Het niveau van zekerheid van de analyse. Het zekerheidsniveau is het audit risico van de audit.

### Hoe wil je je variabelen evalueren? 
- Audit waarden: Als je deze optie selecteert moet je de selectie annoteren met de echte waarden van de observaties. Wanneer deze correct is, vul dan precies dezelfde waarde als de boekwaarde van de transactie in.
- Correct / Incorrect: Als je deze optie selecteert moet je de selectie annoteren met een indicator over of observaties correct (0) of incorrect (1) zijn.

----

Geavanceerde opties.
-------
### Inherent risico en controle risico
- Hoog: 100%
- Gemiddeld: 60%
- Laaf: 50%

Als beide risicobeoordelingen op Hoog (100%) worden gezet, wordt het audit risico model niet gebruikt om het detectierisico bij te stellen. 

### Verwachte fouten:
- Absoluut: Voer je verwachte fouten in als geldwaarde (bijv., $1000 in een totale balans van $1000000).
- Relatief: Voel je verwachte fouten in als percentage van de totale grootte van de selectie.

### Toelichtende tekst:
- Schakelt toelichtende tekst in door de werkflow om je te helpen de statistische resultaten en procedure te interpreteren. 

### Planning verdeling:
- Beta: De beta verdeling voor broken taints (de Swart, Wille & Majoor, 2013).
- Beta-binomiaal: De eindige populatie beta-binomiaal verdeling voor complete taints (Dyer & Pierce, 1993).

### Type selectie:
- Geldeenheid steekproeven: Voert selectie uit op het niveau van individuele steekproefeenheden.
- Record steekproeven: Voert selectie uit op het niveau van individuele records.

### Selectiemethode:
- Aselecte steekproeven: Voert aselecte selectie uit. 
- Celsteekproeven: Voert interval selectie uit met willekeurigheid. Elke observatie die meer dan twee keer groter is dan het interval wordt meerdere keren geselecteerd.
- Systematische steekproeven: Voert interval selectie uit. Elke observatie die groter is dan het interval wordt meerdere keren geselecteerd. 

### Seed:
- Seed voor de willekeurige nummer generator om resultaten reproduceerbaar te maken. Dit beïnvloedt welke steekproeven er uit de populatie worden genomen.

### Schattingsmethode:
- Cox en Snell: De Cox en Snell grens (Cox & Snell, 1979)

----

Standaard uitvoer
-------

### Samenvatting van de planning:
- Materialiteit: De populatie materialiteit.
- Inherent risico: Risico oordeel voor het inherente risico.
- Controle risico: Risico oordeel voor het controle risico. 
- Verwachte fouten: Het aantal verwachte fouten in de selectie.
- Benodigde steekproef grootte: De steekproefgrootte die nodig is voor het oordeel over de populatie.

### Samenvatting van de selectie:
- Steekproefgrootte: De grootte van de geselecteerde subset. 
- % van totale observaties: De relatieve grootte van de subset. 
- % van totale waarde: De relatieve waarde van de subset.
- Interval: De grootte van het interval die wordt gebruikt in de selectiemethode.

### Samenvatting van de evaluatie:
- Materialiteit: De populatie materialiteit.
- Steekproefgrootte: De grootte van de geselecteerde subset.
- Fouten: Het aantal foute elementen in de selectie.
- Totale Taining: De som van proportionele fouten. 
- x-% geloofwaardigheid interval: De schatting van de maximale misstatement in percentages. 
- Maximale misstatement: Schatting van de geprojecteerde maximale misstatement.

----

Tabellen en grafieken
-------

### Beschrijvende statistieken boekwaarden
- Produceert een tabel met verschillende statistieken over de boekwaarden zoals de populatiegrootte, totale waarde, gemiddelde, standaardafwijking en kwantielen.

### Verdeling boekwaarden
- Produceert een histogram van de verdeling van boekwaarden in de populatie. Belangrijke statistieken zoals het gemiddelde, de standaardafwijking en kwantielen worden in kleur aangegeven. 

### Geïmpliceerde prior van risico oordelen
- Produceert een grafiek die de prior laat zien die wordt bepaald door het inherente risico, het controle risico en de verwachtte fouten. 

### Verwachte posterior
- Voegt de verwachte posterior toe aan de prior grafiek. De verwachte posterior heeft zijn bovengrens van de geloofwaardigheid net onder de materialiteit. 

### verwachte Bayes factor
- Laat de verwachte toename in bewijs zien wanneer de geformuleerde planning wordt gevolgd. 

### Impliciete steekproef
- Produceert een tabel die de impliciete steekproef laat zien waar de prior verdeling op is gebaseerd. 

### Beslissing analyse
- Produceert een grafiek die alle planning verdelingen en hun steekproefgroottes vergelijkt. 

### Geef geselecteerde observaties weer
- Produceert een tabel met de geselecteerde observaties samen met aanvullende observaties ingevoegd in het bijbehorende veld. 

### Beschrijvende statistieken selectie 
- Produceert een tabel met beschrijvende informatie over numerieke variabelen in de selectie. 

### Meest waarschijnlijke fout (MLE) 
- Voegt een vel toe aan de evaluatie samenvatting tabel met een schatting van de fouten in de totale populatie.

### Bayes factor
- Berekent de Bayes factor, die de relatieve toename in bewijs geeft voor de hypothese dat het aantal fouten in de populatie lager is dan de materialiteit.

### Evaluatie informatie 
- produceert een staafgrafiek die de materialiteit, de maximale misstatement en de meest waarschijnlijke fout (MLE) weergeeft. 

### Prior en posterior
- Produceert een figuur met de prior verdeling in relatie tot de posterior verdeling. 

### Correlatie grafiek
- Produceert een scatterplot die de boekwaarden van de selectie afzet tegen hun audit waarden. Observaties die fout zijn worden rood gekleurd. 

----

R Packages
-------
- base R

----

Referenties
-------

Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. <i>Biometrika</i>, 66(1), 125-132.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.