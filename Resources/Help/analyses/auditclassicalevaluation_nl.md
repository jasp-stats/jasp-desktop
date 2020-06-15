Evaluatie
==========================

De evaluatie analyse stelt de auditor in staat een steekproef te evalueren en een statistische conclusie te trekken over de totale fout in de populatie. 

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

#### Annotatie methode
Voor de evaluatie fase heeft de auditor zijn of haar steekproef geannoteerd. Dit kan op twee manieren: evaluatie met werkelijke waardes of door te bepalen of de transacties correct of incorrect zijn.

- Auditwaarden: indien geselecteerd kunt u de selectie annoteren met de werkelijke waardes van de waarnemingen. Indien een transactie volledig correct is, vul dan exact dezelfde waarde in als vermeld is in de boekwaarde van de transactie. Deze benadering wordt aanbevolen wanneer de transacties een geldwaarde hebben.
- Correct / Incorrect: indien geselecteerd kunt u de selectie annoteren met een indicator of de waarnemingen correct (0) of incorrect (1) zijn. Deze aanpak wordt aanbevolen wanneer uw transacties geen geldwaarde hebben.
  - Gebruik samenvattende statistieken: Indien geselecteerd kan de auditor alleen de steekproefomvang en de gevonden fouten gebruiken om een conclusie te trekken.

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

#### Toelichtende tekst
Deze optie maakt verklarende tekst door de hele workflow mogelijk om u te helpen de statistische resultaten en procedure te interpreteren.

#### Planning verdeling
De statistische verdeling die wordt gebruikt voor het berekenen van de vereiste steekproefomvang.

- Binomiaal: de binominale verdeling.
- Poisson: de Poisson verdeling voor gebroken fouten (AICPA, 2017).
- Hypergeometrisch: de hypergeometrische verdeling (alleen juiste / onjuiste evaluatie).

#### Schattingsmethode
De schattingsmethode bepaalt hoe de resultaten van uw steekproef worden geëxtrapoleerd naar de populatie.

- Binomiaal: Gebruikt de binomiale kansverdeling om de steekproef te evalueren.
- Poisson: Gebruikt de Poisson kansverdeling om de steekproef te evalueren.
- Hypergeometrisch: gebruikt de hypergeometrische kansverdeling om de steekproef te evalueren.
- Stringer: Gebruikt de Stringer-bound om de steekproef te evalueren (Stringer, 1963).
     - LTA-aanpassing: LTA-aanpassing voor de Stringer bound die onderwaarderingen meeneemt (Leslie, Teitlebaum, & Anderson, 1979).
- Directe schatter: deze methode gebruikt alleen de audit waardes om de totale fout te schatten (Touw en Hoogduin, 2011).
- Verschil schatter: deze methode gebruikt het verschil tussen de boekwaardes en de audit waardes om de totale fout te schatten (Touw en Hoogduin, 2011).
- Ratio schatter: Deze methode gebruikt de verhouding van correctheid tussen de boekwaardes en de audit waardes om de totale fout te schatten (Touw en Hoogduin, 2011).
- Regressie: deze methode gebruikt de lineaire relatie tussen de boekwaardes en de audit waardes om de totale fout te schatten (Touw en Hoogduin, 2011).

----

Standaard resultaten
-------

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

Geavanceerde resultaten (grafieken)
-------

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

Touw, P., & Hoogduin, L. (2011). Statistiek voor audit en controlling.