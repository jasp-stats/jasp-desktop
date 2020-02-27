Planning
==========================

De taak van een auditor is om te beoordelen hoe eerlijk de gepresenteerde transacties in een populatie zijn, en inschatten of de populatie fouten bevat die materieel zijn (lager dan materialiteit). Soms moet een auditor van tevoren de benodigde steekproefgrootte berekenen zonder dat hij toegang heeft tot de ruwe populatiedata. In dit geval kan de auditor gebruik maken van de *planning* analyse, samen met de populatie's samengevatte statistieken (totale grootte en waarde), om te berekenen hoeveel steekproeven moeten worden geëvalueerd om een bepaalde zekerheid in de beoordeling te behalen. De frequentistische *planning* analyse kan gebruik maken van risico schattingen van de *audit risico model* om de benodigde risico's op het vinden van materialiteitsfouten aan te passen.

*Let op:* Wanneer de gebruiker wel toegang heeft tot de ruwe populatie data, is het aan te raden gebruik te maken van de *audit werkflow*, een analyse die je door het audit proces heen leidt. 

----

Standaardopties
-------
### Materialiteit van de populatie:
- Absoluut: Voer de materialiteit van de populatie in als een monetaire waarde. 
- Relatief: Voer de materialiteit van de populatie in als een percentage relatief aan de totale waarde. 

### Populatie
- Grootte: Het totale aantal observaties in de totale populatie.
- Waarde: De totale waarde van de populatie in monetaire eenheden. 

### Audit Risico
- Zekerheid: Het zekerheidsniveau van de analyse. Het zekerheidsniveau is gelijk aan het audit risico van de audit. 

----

Geavanceerde Opties
-------

### Inherent risico en controle risico:
- Hoog: 100%
- Gemiddeld: 60%
- Laag: 50%

Wanneer beide risico beoordelingen op Hoog (100%) gezet zijn, wordt het audit risico model niet gebruikt om het detectie risico aan te passen. 

### Verwachte fouten:
- Absoluut: Voer de verwachte fout in als monetaire waarde (bijv., $1.000 op een totaal balans van $1.000.000). 
- Relatief: Voer de verwachte fout in als een percentage relatief aan de totale grootte van de selectie.

### Toelichtende tekst:
- Activeert de toelichtende tekst in het gebruik van de workflow om te helpen met het interpreteren van de statistische resultaten en procedure. 

### Planning verdeling:
- Poisson: De poisson verdeling voor broken taints (AICPA, 2017). 
- Binomiaal: De oneindige populatie binomiale verdeling voor complete taints. 
- Hypergeometrisch: De eindige populatie hypergeometrische verdeling voor complete taints. 

----

Standaard Uitvoer
-------

### Planning samenvatting
- Materialiteit: De materialiteit van de populatie. 
- Inherent risico: Risicoschatting  voor het inherente risico. 
- Controle risico: Risicoschatting voor het controle risico.
- Verwachte fouten: Het aantal verwachte fouten in de selectie. 
- Benodigde steekproefgrootte: De steekproefgrootte die nodig is voor de populatie uitspraak. 

----

Tabellen en Grafieken
-------

### Beslissingsanalyse
- Produceert een grafiek die alle planningsverdelingen en de bijbehorende steekproefgroottes vergelijkt. 

----

R Packages
-------
- base R

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.