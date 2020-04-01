Bayesiaanse Planning
==========================

De taak van een auditor is om een oordeel te vellen over de eerlijkheid van de gepresenteerde transacties in een populatie, en te bepalen of de populatie als geheel materiële fouten bevat (lager dan de vastgestelde materialiteit). Soms moet een auditor vooraf de vereiste steekproefomvang berekenen, zonder toegang te hebben tot de onbewerkte populatiegegevens. In dit geval kan de auditor de *Bayesiaanse planning* analyse samen met de samenvattende statistieken van de populatie (totale omvang en waarde) gebruiken om te berekenen hoeveel steekproefelementen moeten worden geëvalueerd om een zekere betrouwbaarheid in hun oordeel te verkrijgen. De *Bayesiaanse planning* analyse kan de risicobeoordelingen van het *audit risico model* gebruiken om deze informatie op te nemen in een prior kansverdeling, welke wordt bijgewerkt met behulp van informatie uit de steekproef om een posterior kansverdeling te vormen. Inferenties over de totale fout in de populatie worden gemaakt met behulp van deze posterior verdeling.

*Opmerking:* Als u toegang heeft tot de onbewerkte populatiegegevens wordt het aangeraden om de *Bayesiaanse audit workflow* gebruiken, een analyse die u door het audit proces leidt.

----

Standaard invoeropties
-------

#### Populatiematerialiteit
De populatiematerialiteit is de maximaal toelaatbare fout in de populatie. Dit kan een absolute waarde zijn of een waarde die de materialiteit aangeeft als een percentage van de totale waarde van de populatie.

- Absoluut: Voer uw populatiematerialiteit in als geldwaarde.
- Relatief: Voer uw populatiematerialiteit in als een percentage van de totale waarde.

#### Populatie
Hier kunt u de samenvattende statistieken over de populatie verstrekken.

- Grootte: Het totale aantal waarnemingen in de populatie.
- Waarde: De totale waarde van de populatie in geldeenheden.

#### Auditrisico
Het auditrisico bepaalt het risico dat de auditor bereid is te nemen om een onjuist oordeel te geven over de eerlijkheid van de transacties in de populatie. Het auditrisico is het omgekeerde van de betrouwbaarheid van de analyse (auditrisico = 1 - betrouwbaarheid).

- betrouwbaarheid: Het betrouwbaarheidsniveau van uw vereiste statistische verklaring.

----

Geavanceerde invoeropties
-------

#### Inherent risico and controle risico
De beoordelingen van het inherente risico en het controlerisico (audit risico model) kunnen hier worden verstrekt om het vereiste bewijs uit de steekproef te verminderen. Ze worden toegewezen aan kansen volgens standaarden, maar kunnen ook worden toegewezen op basis van aangepaste voorkeuren.

- Hoog: 100%
- Middel: 60%
- Laag: 50%
- Aangepast

Wanneer beide risicobeoordelingen zijn ingesteld op Hoog (100%), wordt het audit risico model niet gebruikt om het detectierisico aan te passen.

#### Verwachte fouten
De verwachte fouten zijn de tolereerbare fouten die in de steekproef kunnen worden gevonden. Een steekproefomvang wordt berekend zodat, wanneer het aantal verwachte fouten in de steekproef wordt gevonden, de gewenste betrouwbaarheid behouden blijft.

- Absoluut: Voer uw verwachte fouten in als geldwaarde (bijvoorbeeld € 1.000 op een totaal saldo van € 1.000.000).
- Relatief: Voer uw verwachte fouten in als percentage ten opzichte van de totale grootte van de selectie.

#### Toelichtende tekst
Deze optie maakt verklarende tekst door de hele workflow mogelijk om u te helpen de statistische resultaten en procedure te interpreteren.

#### Planning verdeling
De statistische verdeling die wordt gebruikt voor het berekenen van de vereiste steekproefomvang.

- Beta: De beta kansverdeling voor gebroken fouten (de Swart, Wille & Majoor, 2013).
- Gamma: De gamma kansverdeling voor gebroken fouten.
- Beta-binomiaal: De beta-binomiaal kansverdeling voor hele fouten (Dyer & Pierce, 2993).

----

Standaard resultaten
-------

#### Planning samenvatting
Deze tabel is het standaard resultaat voor de planningsfase.

- Materialiteit: De maximaal toelaatbare fout in de populatie.
- Inherent risico: Risicobeoordeling voor het inherente risico.
- Controlerisico: Risicobeoordeling voor het controlerisico.
- Verwachte fouten: Het aantal verwachte fouten in de selectie.
- Vereiste steekproefgrootte: De vereiste steekproefomvang.

----

Geavanceerde resultaten (statistieken)
-------

#### Verwachte evidentieratio
Toont de verwachte posterior kansen (geïnduceerd door de planning) ten gunste van de nulhypothese van een aanvaardbare afwijking.

#### Verwachte Bayes factor
Toont de verwachte hoeveelheid bewijs (geïnduceerd door de planning) ten gunste van de nulhypothese van een aanvaardbare afwijking.

----

Geavanceerde resultaten (tabellen)
-------

#### Impliciete steekproef
Produceert een tabel met de impliciete steekproef waarop de prior kansverdeling is gebaseerd.

#### Prior en verwachte posterior beschrijvingen
Produceert een tabel waarin de prior en verwachte posterior kansverdelingen worden samengevat door middel van verschillende statistieken, zoals hun functionele vorm, hun prior en verwachte posterior kansen en kansen, en de verschuiving daartussen.

----

Geavanceerde resultaten (grafieken)
-------

#### Steekproefgrootte vergelijking
Produceert een plot die alle plannings verdelingen en hun bijbehorende steekproefomvang vergelijkt.

#### Geimpliceerde prior door risicobeoordelingen
Produceert een grafiek die de prior kansverdeling laat zien op basis van het inherente risico, het controlerisico en de verwachte fouten.

- x-as limiet: Wijzig de limiet voor de x-as in de grafiek.
- Verwachte posterior: Voegt het verwachte posterior kansverdeling toe aan de grafiek. De verwachte posterior kansverdeling heeft zijn bovenste geloofwaardigheidsgrens onder de materialiteit.
- Aanvullende info: Voegt een rode stip toe in de grafiek voor de materialiteit en een grijze stip voor de verwachte fouten.
- Markeer: Selecteer welk gebied onder de prior kansverdeling wordt weergegeven in kleur.

----

R Pakketten
-------

- jfa

----

Referenties
-------

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.
