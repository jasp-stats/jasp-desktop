Schatting
==========================

*Opmerking*: Deze analyse zal aanzienlijk veranderen in de volgende versie van JASP. Het zal worden omgevormd tot een volwaardige evaluatieanalyse, die meer mogelijk maakt dan alleen schatting op basis van betrouwbaarheidsintervallen.

Schatting wordt toegepast wanneer de auditor een steekproef van de populatie heeft verzameld en de totale fout de populatie wil schatten op basis van deze steekproef.

----

Standaard invoeropties
-------

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

#### Schatter
Kies welke schatter u wilt gebruiken voor de berekening van de resultaten.

- Directe schatter
- Verschilschatter
- Ratio schatter
- Regressieschatter (standaard)

#### Toelichtende tekst
Deze optie maakt verklarende tekst door de hele workflow mogelijk om u te helpen de statistische resultaten en procedure te interpreteren.

----

Standaard resultaten
-------

#### Schattingstabel
Deze tabel is het standaard resultaat voor de schattingsanalyse.

- Schatting: De puntschatting van de totale fout in de populatie.
- Onzekerheid: De onzekerheid van de schatting.
- Betrouwbaarheidsinterval: De boven- en ondergrens van het betrouwbaarheidsinterval dat bij de schatting hoort.

----

Geavanceerde resultaten (tabellen)
-------

#### Benodigde steekproefgrootte
Produceert een tabel die voor een gegeven onzekerheid (in geldeenheden) de vereiste steekproefomvang laat zien.

----

Geavanceerde resultaten (grafieken)
-------

#### Correlatiediagram
Produceert een spreidingsplot waarbij boekwaarden van de selectie worden vergeleken met hun werkelijke waarden. Waarnemingen die niet overeen komen, worden rood weergegeven.

----

R Paketten
-------

- jfa

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied 
Public Accountants.
