Audit Workflow
==========================

De taak van een auditor is om een oordeel te vellen over de eerlijkheid van de gepresenteerde transacties in een populatie, en te bepalen of de populatie als geheel materiële fouten bevat (lager dan de vastgestelde materialiteit). Wanneer de auditor toegang heeft tot de onbewerkte populatiegegevens, dan kan deze de *audit workflow* gebruiken om te berekenen hoeveel steekproefelementen moeten worden geaudit om een bepaalde betrouwbaarheid in het oordeel te verkrijgen. Vervolgens kan ze het berekende aantal transacties uit de populatie selecteren, deze transacties inspecteren en een oordeel vellen over de totale fout in de populatie. De workflow leidt de auditor door het audit proces en maakt onderweg de juiste keuzes in berekeningen. De frequentistische *audit workflow* kan de risicobeoordelingen van het *audit risico model* gebruiken om het vereiste risico op het vinden van materiële fouten aan te passen.

----

Workflow
-----------

De audit workflow bestaat uit vier afzonderlijke fasen, elk met hun eigen doel voor de analyse:
- Planning: Bereken de vereiste steekproefomvang voor uw gewenste materialiteit en betrouwbaarheid.
- Selectie: Selecteer de vereiste transacties uit uw populatie.
- Uitvoering: Annoteer uw selectie met uw beoordeling van de eerlijkheid van de geselecteerde transacties.
- Evaluatie: Kom tot een oordeel over de populatie op basis van uw geannoteerde selectie.

----

Standaard invoeropties
-------

#### Populatiematerialiteit
De populatiematerialiteit is de maximaal toelaatbare fout in de populatie. Dit kan een absolute waarde zijn of een waarde die de materialiteit aangeeft als een percentage van de totale waarde van de populatie.

- Absoluut: Voer uw populatiematerialiteit in als geldwaarde.
- Relatief: Voer uw populatiematerialiteit in als een percentage van de totale waarde.

#### Auditrisico
Het auditrisico bepaalt het risico dat de auditor bereid is te nemen om een onjuist oordeel te geven over de eerlijkheid van de transacties in de populatie. Het auditrisico is het omgekeerde van de betrouwbaarheid van de analyse (auditrisico = 1 - betrouwbaarheid).

- betrouwbaarheid: Het betrouwbaarheidsniveau van uw vereiste statistische verklaring.

#### Hoe wil u uw observaties evalueren?
In de uitvoeringsfase evalueert de auditor de geselecteerde transacties. Dit kan op twee manieren: evaluatie met werkelijke waardes of door te bepalen of de transacties correct of incorrect zijn.

- Auditwaarden: indien geselecteerd kunt u de selectie annoteren met de werkelijke waardes van de waarnemingen. Indien een transactie volledig correct is, vul dan exact dezelfde waarde in als vermeld is in de boekwaarde van de transactie. Deze benadering wordt aanbevolen wanneer de transacties een geldwaarde hebben.
- Correct / Incorrect: indien geselecteerd kunt u de selectie annoteren met een indicator of de waarnemingen correct (0) of incorrect (1) zijn. Deze aanpak wordt aanbevolen wanneer uw transacties geen geldwaarde hebben.

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

#### Steekproefeenheden
Bij statistische steekproeven krijgt elke steekproefeenheid een kans om in de selectie te worden opgenomen. De steekproefeenheden bepalen welke eenheden (individuele monetaire eenheden versus individuele transacties) een kans krijgen.

- Monetaire eenheid steekproeven: Wijst selectiekansen toe op het niveau van individuele steekproefeenheden. Deze methode heeft de voorkeur wanneer u alleen overwaarderingen onderzoekt.
- Record steekproeven: Wijst selectiekansen toe op het niveau van individuele transacties. Deze methode heeft de voorkeur wanneer u naar overwaarderingen en onderwaarderingen kijkt, of wanneer u geen geldeenheden in uw populatie heeft.

#### Selectiemethode
De selectiemethode bepaalt hoe transacties worden geselecteerd uit de populatie. Verschillende selectiemethoden hebben verschillende eigenschappen en kunnen resulteren in verschillende transacties in uw selectie.

- Aslecte steekproeven: Voert een willekeurige selectie uit waarbij elke steekproefeenheid een gelijke kans krijgt.
- Cel steekproeven: Voert intervalselectie uit met een willekeurig startpunt in elke cel. Elke transactie die groter is dan tweemaal het interval, wordt meerdere keren geselecteerd.
- Vast interval steekproeven: Voert intervalselectie uit waarbij de eerste waarneming van elk interval wordt geselecteerd. Elke waarneming die groter is dan het interval, wordt meerdere keren geselecteerd.

#### Toevalsgenerator
Willekeurige nummergenerator om resultaten reproduceerbaar te maken. Dit beïnvloedt welke steekproeven uit de populatie worden getrokken in aselecte steekproeven en cel steekproeven. Bij steekproefselectie met een vast interval is de toevalsgenerator uitgeschakeld omdat de eerste eenheid uit elk interval wordt geselecteerd.

#### Schattingsmethode
De schattingsmethode bepaalt hoe de resultaten van uw steekproef worden geëxtrapoleerd naar de populatie.

- Binomiaal: Gebruikt de binomiale kansverdeling om de steekproef te evalueren.
- Poisson: Gebruikt de Poisson kansverdeling om de steekproef te evalueren.
- Hypergeometrisch: gebruikt de hypergeometrische kansverdeling om de steekproef te evalueren.
- Stringer: Gebruikt de Stringer-bound om de steekproef te evalueren (Stringer, 1963).
     - LTA-aanpassing: LTA-aanpassing voor de Stringer bound die onderwaarderingen meeneemt (Leslie, Teitlebaum, & Anderson, 1979).

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

#### Selectie samenvatting
Deze tabel is het standaard resultaat voor de selectiefase.

- Selectie grootte: Het aantal geselecteerde transacties.
- % van totale waarnemingen: De relatieve grootte van de geselecteerde transacties.
- % van de populatiewaarde: De relatieve waarde van de geselecteerde transacties.
- Interval: De grootte van het interval dat gebruikt is in de selectie methode.

#### Evaluatie samenvatting
Deze tabel is het standaard resultaat voor de evaluatiefase.

- Materialiteit: De materialiteit van de populatie.
- Steekproefgrootte: Het aantal geevalueerde transacties.
- Fouten: Het aantal foutieve elementen in de selectie.
- Total taining: De som van de proportionele fouten.
- x-% betrouwbaarheidsgrens: De schatting van de maximale afwijking in percentages.
- Maximale afwijking: De schatting van de verwachte maximale afwijking in geldwaarden.

----

Geavanceerde resultaten (statistieken)
-------

#### Meest waarschijnlijke fout (MLE)
Voegt een cel toe aan de samenvattingstabel met een schatting van de werkelijke fout in de populatie.

----

Geavanceerde resultaten (tabellen)
-------

#### Beschrijvende statistieken boekwaarden
Produceert een tabel met verschillende statistieken over de boekwaarden, waaronder de populatiegrootte, totale waarde, gemiddelde, standaarddeviatie en kwartielen.

#### Geef geselecteerde observaties weer
Produceert een tabel met de geselecteerde waarnemingen samen met eventuele aanvullende waarnemingen die in het overeenkomstige veld zijn opgegeven.

#### Selectie samenvatting
Produceert een tabel met beschrijvende informatie over numerieke variabelen in de steekproef.

----

Geavanceerde resultaten (grafieken)
-------

#### Verdeling van boekwaarden
Produceert een histogram van de verdeling van boekwaarden in de populatie. Belangrijke statistieken zoals het gemiddelde, standaarddeviatie en kwartielen worden aangegeven met kleuren.

#### Impliciete steekproefverdeling
Produceert een plot die de steekproefverdeling weergeeft die wordt geïmpliceerd door het planningsproces.

#### Steekproefgrootte vergelijking
Produceert een plot die alle plannings verdelingen en hun bijbehorende steekproefomvang vergelijkt.

#### Evaluatie informatie
Produceert een staafdiagram waarin de materialiteit, maximale afwijking en meest waarschijnlijke fout (MLE) worden vergeleken.

#### Correlatiediagram
Produceert een spreidingsplot waarbij boekwaarden van de selectie worden vergeleken met hun werkelijke waarden. Waarnemingen die niet overeen komen, worden rood weergegeven.

----

R Paketten
-------

- jfa

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Leslie, D. A., Teitlebaum, A. D., Anderson, R. J. (1979). <i>Dollar-unit Sampling: A Practical Guide for Auditors</i>. Toronto: Copp Clark Pitman.

Stringer, K.W. (1963) Practical aspects of statistical sampling in auditing. <i>Proceedings of Business and Economic Statistics Section</i>, American Statistical Association.