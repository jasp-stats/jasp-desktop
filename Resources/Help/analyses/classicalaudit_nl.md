Audit Werkflow
==========================

De taak van een Auditor is om tot oordeel te komen over de eerlijkheid van gepresenteerde transacties in een populatie, en te bepalen of er fouten in de populatie zitten die materieel zijn. Als de auditor toegang heeft tot de populatiedata kan kan hij/zij gebruik maken van de *audit werkflow* om te berekenen hoeveel steekproeven er moeten worden geëvalueerd om tot een bepaald niveau van zekerheid in het oordeel te komen. Er worden dan steekproeven uit de data getrokken, deze worden geïnspecteerd en er wordt een oordeel gemaakt over het aantal fouten in de populatie. De workflow leidt de auditor om de correcte beslissingen te maken in dit proces. De frequentistische *audit werkflow* kan de risico oordelen van het *audit risico model* gebruiken om de benodigde riscos op het vinden van materiele fouten bij te stellen.

----

Werkflow
-----------
De audit werkflow bestaat uit vier aparte stadia, elk met een eigen doel:
- Planning: Bereken de steekproefgrootte die nodig is voor het gewenste oordeel over de populatie. 
- Selectie: Selecteer de benodigde observaties uit de populatie.
- Uitvoering: Annoteer de data met een oordeel over de eerlijkheid van de geselecteerde observaties.
- Evaluatie: Maak een oordeel over de populatie op basis van de geannoteerde selectie.

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
- Laag: 50%

Als beide risico beoordelingen op Hoog (100%) worden gezet wordt het audit risico model niet gebruikt om het detectierisico bij te stellen. 

### Verwachte fouten:
- Absoluut: Voer je verwachte fouten in als geldwaarde (bijv., $1000 in een totale balans van $1000000).
- Relatief: Voer je verwachte fouten in als percentage van de totale grootte van de selectie.

### Toelichtende tekst:
- Schakelt de toelichtende tekst in tijdens het gebruik van de werkflow om je te helpen de statistische resultaten en procedure te interpreteren. 

### Planning verdeling:
- Possion: de poisson verdeling voor broken taints (AICPA, 2017).
- Binomiaal: De oneindige populatie binomiaal verdeling coor complete taints. 
- Hypergeometrisch: De eindige populatie hypergeometrische verdeling voor complete taints. 

### Type selectie:
- Geldeenheid steekproeven: Voert selectie uit op het niveau van individuele steekproefeenheden.
- Record steekproeven: Voert selectie uit op het niveau van individuele records.

### Selectie methode:
- Aselecte steekproeven: Voert aselecte selectie uit. 
- Celsteekproeven: Voert interval selectie uit met willekeurigheid. Elke observatie die meer dan twee keer groter is dan het interval wordt meerdere keren geselecteerd.
- Systematische steekproeven: Voert interval selectie uit. Elke observatie die groter is dan het interval wordt meerdere keren geselecteerd. 

### Seed:
- Seed voor de willekeurige nummer generator om resultaten reproduceerbaar te maken. Dit beïnvloedt welke steekproeven er uit de populatie worden genomen.

### Schattings methode:
- Stringer: De Stringer grens (Stringer, 1963).
	- LTA bijstelling: de LTA bijstelling voor de stringer grens op understatements mee te nemen (Leslie, Teitlebaum, & Anderson, 1979).

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
- x-% zekerheids grens: De schatting van de maximale misstatement in percentages. 
- Maximale misstatement: Schatting van de geprojecteerde maximale misstatement.

----

Tabellen en grafieken
-------

### Beschrijvende statistieken boekwaarden
- Produceert een tabel met verschillende statistieken over de boekwaarden zoals de populatiegrootte, totale waarde, gemiddelde, standaardafwijking en kwantielen.

### Verdeling boekwaarden
- Produceert een histogram van de verdeling van boekwaarden in de populatie. Belangrijke statistieken zoals het gemiddelde, de standaardafwijking en kwantielen worden in kleur aangegeven. 

### Beslissingsanalyse
- Produceert een grafiek die alle planning verdeling en hun steekproefgroottes vergelijkt. 

### Geef geselecteerde observaties weer
- Produceert een tabel met de geselecteerde observaties samen met aanvullende observaties ingevoegd in het bijbehorende veld. 

### Beschrijvende statistieken selectie 
- Produceert een tabel met beschrijvende informatie over numerieke variabelen in de selectie. 

### Meest waarschijnlijke fout (MLE) 
- Voegt een vel toe aan de evaluatiesamenvattingstabel met een schatting van de fouten in de totale populatie.

### Evaluatie informatie 
- Produceert een staafgrafiek die de materialiteit, de maximale misstatement en de meest waarschijnlijke fout (MLE) weergeeft. 

### Correlatie grafiek
- Produceert een spreidingsdiagram die de boekwaarden van de selectie afzet tegen hun audit waarden. Observaties die fout zijn worden rood gekleurd. 

----

R Packages
-------
- base R

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Leslie, D. A., Teitlebaum, A. D., Anderson, R. J. (1979). <i>Dollar-unit Sampling: A Practical Guide for Auditors</i>. Toronto: Copp Clark Pitman.

Stringer, K.W. (1963) Practical aspects of statistical sampling in auditing. <i>Proceedings of Business and Economic Statistics Section</i>, American Statistical Association.