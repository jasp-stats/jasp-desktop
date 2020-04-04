Wet van Benford
==========================

De wet van Benford stelt dat de verdeling van eerste getallen in een populatie van nature een bepaalde kansverdeling heeft. Het controleren of de verdeling van de eerste getallen in de populatie voldoet aan de wet van Benford kan aanvullend bewijs dat de gegevens in de populatie verdere inspectie vereisen.

* Opmerking:* Het niet naleven van de wet van Benford duidt niet noodzakelijkerwijs op fraude. Een analyse van de wet van Benford kan daarom alleen worden gebruikt om inzicht in te krijgen in of een populatie nadere inspectie nodig heeft.

----

Standaard invoeropties
-------

#### Variabele
Hier kunt u de variabele opgeven die de leidende getallen bevat waarop de wet van Benford moet worden getest.

#### Controleer getallen
Met deze optie kunt u aangeven of u de wet van Benford wilt testen op de eerste cijfers 1 - 9 of op de eerste en tweede cijfers 10 - 99.

----

Geavanceerde invoeropties
-------

#### Auditrisico
Het auditrisico bepaalt het risico dat de auditor bereid is te nemen om een onjuist oordeel te geven over de eerlijkheid van de transacties in de populatie. Het auditrisico is het omgekeerde van de betrouwbaarheid van de analyse (auditrisico = 1 - betrouwbaarheid).

#### Toelichtende tekst
Deze optie maakt verklarende tekst door de hele workflow mogelijk om u te helpen de statistische resultaten en procedure te interpreteren.

----

Standaard resultaten
-------

#### Goodness-of-fit Toets
Deze tabel is het standaard resultaat voor de analyse van de wet van Benford.

- Statistiek: Notatie van de teststatistiek van de chi-kwadraat toets.
- Waarde: De waarde van de chi-kwadraat teststatistiek.
- df: Vrijheidsgraden geassocieerd met de chi-kwadraat toets.
- *p* waarde: De *p* waarde die is geassocieerd met de chi-kwadraat toets.

----

Geavanceerde resultaten (tabellen)
-------

#### Beschrijvende statistieken
Produceert een tabel met voor elk leidend cijfer het waargenomen aantal, het waargenomen percentage, en het verwachte percentage onder de wet van Benford.

#### Vergelijk met de wet van Benford
Produceert een grafiek met de waargenomen verdeling van leidende cijfers in de populatie, vergeleken met de verwachte verdeling onder de wet van Benford.

----

R Pakketten
-------

- base R

----

Referenties
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied 
Public Accountants.
