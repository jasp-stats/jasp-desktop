Bayesiaanse Planning
==========================

De taak van een Auditor is om een oordeel te maken over de eerlijkheid van gepresenteerde transacties in een populatie, en te bepalen of er fouten in de populatie zitten die materieel zijn. Soms moet een auditor de benodigde steekproefgrootte van te voren berekenen zonder toegang tot de populatie data te hebben. In dit geval kan de auditor de *Bayesiaanse planning* analyse gebruiken in combinatie met samenvattende statistieken over de populatie (de totale grootte en waarde) om te berekenen hoeveel steekproeven moeten worden geëvalueerd om tot een bepaald niveau van zekerheid in hun oordeel te komen. De *Bayesiaanse planning* analyse kan risicoschattingen van het *audit risico model* gebruiken om deze informatie in een prior kansverdeling te integreren. Deze wordt geüpdatet met informatie om tot een posterior kansverdeling te komen. Inferenties over de fouten in de populatie worden op basis van de posterior verdeling gemaakt. 

*Let op:* Wanneer u toegang heeft tot de populatie data kan de *Bayesiaanse audit werkflow* gebruikt worden, een analyse die u door het audit proces leidt. 

----

Standaard opties
-------
### Materialiteit van de populatie:
- Absoluut: Voer de materialiteit van uw populatie in als geldwaarde.
- Relatief: Voer de materialiteit van uw populatie in als percentage van de totaalwaarde.

### Populatie
- Grootte: Het totale aantal observaties in de populatie. 
- Waarde: De totale waarde van de populatie in geldeenheiden. 

### Audit Risico
- Zekerheid: Het zekerheidsniveau van de analyse. Het zekerheidsniveau is gelijk aan het audit risico van de audit. 

----

Geavanceerde opties.
-------
### Inherent risico en controle risico
- Hoog: 100%
- Gemiddeld: 60%
- Laag: 50%

Als beide risico beoordelingen op Hoog (100%) worden gezet, wordt het audit risico model niet gebruikt om het detectie risico bij te stellen. 

### Verwachte fouten:
- Absoluut: Voer uw verwachte fouten in als geldwaarde (bijv., $1000 in een totale balans van $1000000).
- Relatief: Voel uw verwachte fouten in als percentage van de totale grootte van de selectie.

### Toelichtende tekst:
- Schakelt de toelichtende tekst in tijdens het gebruik van de werkflow om u te helpen de statistische resultaten en procedure te interpreteren. 

### Planning verdeling:
- Beta: De beta verdeling voor broken taints (de Swart, Wille & Majoor, 2013).
- Beta-binomiaal: De eindige populatie beta-binomiaal verdeling voor complete taints (Dyer & Pierce, 1993).

----

Standaard uitvoer
-------

### Samenvatting van de planning:
- Materialiteit: De populatie materialiteit.
- Inherent risico: Risico oordeel voor het inherente risico.
- Controle risico: Risico oordeel voor het controle risico. 
- Verwachte fouten: Het aantal verwachtte fouten in de selectie.
- Benodigde steekproef grootte: De steekproef grootte die nodig is voor het oordeel over de populatie.

----

Tabellen en grafieken
-------

### Geïmpliceerde prior van risico oordelen
- Produceert een grafiek die de prior laat zien die wordt bepaald door het inherente risico, het controle risico en de verwachtte fouten. 

### Verwachte posterior
- Voegt de verwachte posterior toe aan de prior grafiek. De verwachte posterior heeft zijn bovengrens van de geloofwaardigheid net onder de materialiteit. 

### verwachte Bayes factor
- Laat de verwachtte toename in bewijs zien wanneer de geformuleerde planning wordt gevolgd. 

### Impliciete steekproef
- Produceert een tabel die de impliciete steekproef laat zien waar de prior verdeling op is gebaseerd. 

### Beslissing analyse
- Produceert een grafiek die alle planning verdeling en hun steekproef groottes vergelijkt. 

----

R Packages
-------
- base R

----

Referenties
-------

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.