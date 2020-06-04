Selectie
==========================

Met de selectieanalyse kan de auditor transacties uit een populatie selecteren met een combinatie van selectie technieken (record sampling versus monetary unit sampling (MUS)) en selectie methoden (willekeurige selectie, cel selectie, vast intereval selectie).

----

Standaard invoeropties
-------

#### Steekproefgrootte
Het benodigde aantal steekproefelementen dat geselecteerd wordt uit de populatie.

#### Voeg selectie teller toe aan data
Wanneer ingeschakeld voegt de analyse het resultaat van de selectieanalyse in een nieuwe kolom toe aan de data. De nieuwe kolom geeft weer hoe vaak elke transactie in de steekproef is opgenomen.

----

Geavanceerde invoeropties
-------

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

----

Standaard resultaten
-------

#### Selectie samenvatting
Deze tabel is het standaard resultaat voor de selectiefase.

- Selectie grootte: Het aantal geselecteerde transacties.
- % van totale waarnemingen: De relatieve grootte van de geselecteerde transacties.
- % van de populatiewaarde: De relatieve waarde van de geselecteerde transacties.
- Interval: De grootte van het interval dat gebruikt is in de selectie methode.

----

Geavanceerde resultaten (tabellen)
-------

#### Geef geselecteerde observaties weer
Produceert een tabel met de geselecteerde waarnemingen samen met eventuele aanvullende waarnemingen die in het overeenkomstige veld zijn opgegeven.

#### Selectie samenvatting
Produceert een tabel met beschrijvende informatie over numerieke variabelen in de steekproef.

----

Geavanceerde resultaten (plots)
-------

#### Populatie en steekproef histogrammen
Produceert een overlappend histogram dat de verdeling van waarden van elke variabele in de populatie vergelijkt met de waarden in de steekproef.

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.