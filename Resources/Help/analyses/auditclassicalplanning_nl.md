Planning
==========================

De taak van een auditor is om een oordeel te vellen over de eerlijkheid van de gepresenteerde transacties in een populatie, en te bepalen of de populatie als geheel materiële fouten bevat (lager dan de vastgestelde materialiteit). Soms moet een auditor vooraf de vereiste steekproefomvang berekenen, zonder toegang te hebben tot de onbewerkte populatiegegevens. In dit geval kan de auditor de *planning* analyse samen met de samenvattende statistieken van de populatie (totale omvang en waarde) gebruiken om te berekenen hoeveel steekproefelementen moeten worden geëvalueerd om een zekere betrouwbaarheid in hun oordeel te verkrijgen. De frequentistische *planning* analyse kan de risicobeoordelingen van het *audit risico model* gebruiken om het vereiste risico op het vinden van materiële fouten aan te passen.

*Opmerking:* Als u toegang heeft tot de onbewerkte populatiegegevens wordt het aangeraden om de *audit workflow* gebruiken, een analyse die u door het audit proces leidt.

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

- Binomiaal: de binominale verdeling.
- Poisson: de Poisson verdeling voor gebroken fouten (AICPA, 2017).
- Hypergeometrisch: de hypergeometrische verdeling (alleen juiste / onjuiste evaluatie).

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

Geavanceerde resultaten (grafieken)
-------

#### Impliciete steekproefverdeling
Produceert een plot die de steekproefverdeling weergeeft die wordt geïmpliceerd door het planningsproces.

#### Steekproefgrootte vergelijking
Produceert een plot die alle plannings verdelingen en hun bijbehorende steekproefomvang vergelijkt.

----

R Pakketten
-------

- jfa

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.