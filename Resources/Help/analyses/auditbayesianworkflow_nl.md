Bayesian Audit Workflow
==========================

De taak van een auditor is om een oordeel te vellen over de eerlijkheid van de gepresenteerde transacties in een populatie, en te bepalen of de populatie als geheel materiële fouten bevat (lager dan de vastgestelde materialiteit). Wanneer de auditor toegang heeft tot de onbewerkte populatiegegevens, dan kan deze de *Bayesiaanse audit workflow* gebruiken om te berekenen hoeveel steekproefelementen moeten worden geaudit om een bepaalde betrouwbaarheid in het oordeel te verkrijgen. Vervolgens kan ze het berekende aantal transacties uit de populatie selecteren, deze transacties inspecteren en een oordeel vellen over de totale fout in de populatie. De workflow leidt de auditor door het audit proces en maakt onderweg de juiste keuzes in berekeningen. De *Bayesiaanse audit workflow* kan de risicobeoordelingen van het *audit risico model* gebruiken om deze informatie op te nemen in een prior kansverdeling, welke wordt bijgewerkt met behulp van informatie uit de steekproef om een posterior kansverdeling te vormen. Inferenties over de totale fout in de populatie worden gemaakt met behulp van deze posterior verdeling.

----

Workflow
-----------

De Bayesiaanse audit workflow bestaat uit vier afzonderlijke fasen, elk met hun eigen doel voor de analyse:
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

- Beta: De beta kansverdeling voor gebroken fouten (de Swart, Wille & Majoor, 2013).
- Gamma: De gamma kansverdeling voor gebroken fouten.
- Beta-binomiaal: De beta-binomiaal kansverdeling voor hele fouten (Dyer & Pierce, 2993).

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

- Beta: Gebruikt de beta posterior kansverdeling om de steekproef te evalueren.
- Gamma: Gebruikt de gamma posterior kansverdeling om de steekproef te evalueren.
- Beta-binomial: Gebruikt de beta-binomiaal posterior kansverdeling om de steekproef te evalueren.
- Cox and Snell: Gebruikt de Cox en Snell posterior kansverdeling om de steekproef te evalueren. (Cox & Snell, 1979).

#### Gebied onder posterior
- Geloofwaardigheidsgrens: Geeft een (bovenste) schatting van de maximale fout in de populatie.
- Geloofwaardigheidsinterval: Geeft een (bovenste en onderste) schatting van de maximale fout in de populatie.

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
- x-% geloofwaardigheidsgrens: De schatting van de maximale afwijking in percentages.
- Maximale afwijking: De schatting van de verwachte maximale afwijking in geldwaarden.

----

Geavanceerde resultaten (statistieken)
-------

#### Verwachte evidentieratio
Toont de verwachte posterior kansen (geïnduceerd door de planning) ten gunste van de nulhypothese van een aanvaardbare afwijking.

#### Verwachte Bayes factor
Toont de verwachte hoeveelheid bewijs (geïnduceerd door de planning) ten gunste van de nulhypothese van een aanvaardbare afwijking.

#### Meest waarschijnlijke fout (MLE)
Voegt een cel toe aan de samenvattingstabel met een schatting van de werkelijke fout in de populatie.

#### Evidentieratio
Toont de posterior kansen (na het zien van een steekproef) ten gunste van de nulhypothese van een aanvaardbare afwijking.

#### Bayes factor
Toont de hoeveelheid bewijs (na het zien van een steekproef) ten gunste van de nulhypothese van een aanvaardbare afwijking.

----

Geavanceerde resultaten (tabellen)
-------

#### Beschrijvende statistieken boekwaarden
Produceert een tabel met verschillende statistieken over de boekwaarden, waaronder de populatiegrootte, totale waarde, gemiddelde, standaarddeviatie en kwartielen.

#### Impliciete steekproef
Produceert een tabel met de impliciete steekproef waarop de prior kansverdeling is gebaseerd.

#### Prior en verwachte posterior beschrijvingen
Produceert een tabel waarin de prior en verwachte posterior kansverdelingen worden samengevat door middel van verschillende statistieken, zoals hun functionele vorm, hun prior en verwachte posterior kansen en kansen, en de verschuiving daartussen.

#### Geef geselecteerde observaties weer
Produceert een tabel met de geselecteerde waarnemingen samen met eventuele aanvullende waarnemingen die in het overeenkomstige veld zijn opgegeven.

#### Selectie samenvatting
Produceert een tabel met beschrijvende informatie over numerieke variabelen in de steekproef.

#### Prior en posterior beschrijvingen
Produceert een tabel waarin de prior en posterior kansverdelingen worden samengevat door middel van verschillende statistieken, zoals hun functionele vorm, hun prior en posterior kansen en kansen, en de verschuiving daartussen.

----

Geavanceerde resultaten (grafieken)
-------

#### Verdeling van boekwaarden
Produceert een histogram van de verdeling van boekwaarden in de populatie. Belangrijke statistieken zoals het gemiddelde, standaarddeviatie en kwartielen worden aangegeven met kleuren.

#### Steekproefgrootte vergelijking
Produceert een plot die alle plannings verdelingen en hun bijbehorende steekproefomvang vergelijkt.

#### Geimpliceerde prior door risicobeoordelingen
Produceert een grafiek die de prior kansverdeling laat zien op basis van het inherente risico, het controlerisico en de verwachte fouten.

- x-as limiet: Wijzig de limiet voor de x-as in de grafiek.
- Verwachte posterior: Voegt het verwachte posterior kansverdeling toe aan de grafiek. De verwachte posterior kansverdeling heeft zijn bovenste geloofwaardigheidsgrens onder de materialiteit.
- Aanvullende info: Voegt een rode stip toe in de grafiek voor de materialiteit en een grijze stip voor de verwachte fouten.
- Markeer: Selecteer welk gebied onder de prior kansverdeling wordt weergegeven in kleur.

#### Evaluatie informatie
Produceert een staafdiagram waarin de materialiteit, maximale afwijking en meest waarschijnlijke fout (MLE) worden vergeleken.

#### Correlatiediagram
Produceert een spreidingsplot waarbij boekwaarden van de selectie worden vergeleken met hun werkelijke waarden. Waarnemingen die niet overeen komen, worden rood weergegeven.

#### Prior en posterior
Produceert een plot die de prior kansverdeling distributie toont naast de posterior kansverdeling op basis van de steekproef.

 x-as limiet: Wijzig de limiet voor de x-as in de grafiek.
- Verwachte posterior: Voegt het verwachte posterior kansverdeling toe aan de grafiek. De verwachte posterior kansverdeling heeft zijn bovenste geloofwaardigheidsgrens onder de materialiteit.
- Aanvullende info: Voegt een rode stip toe in de grafiek voor de materialiteit en een grijze stip voor de verwachte fouten.
- Markeer: Selecteer welk gebied onder de posterior kansverdeling wordt weergegeven in kleur.

----

R Paketten
-------

- jfa

----

Referenties
-------

Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. <i>Biometrika</i>, 66(1), 125-132.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.
