Mediatie Analyse 
============

Mediatie analyse met mogelijk meerdere predictoren, meerdere mediatoren, meerdere uitkomsten, en een optionele correctie voor meerdere achtergrond verstoringsvariabelen. Mediatie analyse in JASP is gebaseerd op de uitstekende `lavaan` software (Rosseel, 2012). Er is meer informatie te vinden over `lavaan` op [lavaan.org](http://lavaan.org).

Mediatie analyse in JASP is mogelijk voor continue en ordinale variabelen en voor binaire en continue exogene variabelen. Voor binaire endogene variabelen kunt u uw variabelen hercoderen in een "dummy" continue variabele met 0 als de eerste categorie en 1 voor de tweede. De lineariteitsassumptie geldt nog steeds, maar SEM doet bijvoorbeeld geen logistische regressies.


Voor meer informatie over de toegestane datatypen, zie [de lavaan website](http://lavaan.ugent.be/tutorial/cat.html).

### Details over het toetsen van indirecte effecten
Toetsen of een indirect effect bestaat kan het beste worden gedaan met de "Bootstrap" optie onder `Opties > betrouwbaarheidsintervallen > Methoden > Bootstrap`.  De betrouwbaarheidsintervallen worden dan berekend met de voor bias gecorrigeerde percentielmethode zoals besproken in Biesanz, Falk, and Savalei (2010).


## In
#### Predictoren 
Een of meerdere predictor variabele(n) die de mediator(en) en de uitkomst variabele(n) voorspellen.

#### Mediatoren
De variabele waardoor het indirecte effect van de predictor(en) op de uitkomst variabele(n) wordt verondersteld te stromen.

#### Uitkomst
De variabele(n) die wordt(worden) voorspeld door de predictor(en) en de mediator(en).

#### Achtergrond verstoringsvariabelen
Variabelen die variantie in de predictor(en), mediator(en) en uitkomst variabele(n) verklaren: de directe, indirecte en totale effecten worden geconditioneerd op deze variabelen.

#### Gestandaardiseerde schattingen
Vink dit aan om alle variabelen te standaardiseren voor de schatting (gemiddelde = 0, sd = 1).

#### Toon lavaan syntax
Laat de syntax zien die nodig is om dit model te schatten met `lavaan` in `R` of met de `SEM` module in `JASP`.

#### Laat R-Kwadraat zien 
Een tabel met de proportie van de variantie die wordt verklaard voor elk van de endogene variabelen in het mediatie model.

#### Parameterschattingen
Onder deze optie kunt u verschillende parameterschattings-tabellen aanvinken om weer te geven in de uitvoer van de analyse.

#### Betrouwbaarheidsintervallen 
Hier kunt u verschillende manieren om de onzekerheid rond de parameterschattingen schatten. Een voetnoot onder de hoofdtabellen zal de methode weergeven waarmee de standaardafwijking en betrouwbaarheidsintervallen zijn berekend. Zie ook de __details over het testen van indirecte effecten__

#### Grafieken
Deze optie laat gebruikers het padmodel dat is geschat door de mediatie analyse grafisch weergeven. De parameters kunnen worden weergegeven in deze grafiek. Als de labels overlappen kan de grafiek worden opgeslagen als een EPS en worden bewerkt in een vector bewerkingsprogramma.

#### Emulatie
Emuleer de uitvoer van verschillende SEM programma's. Geen standaardoptie.

#### Omgaan met ontbrekende waarden
Hoe met ontbrekende waarden wordt omgegaan. De standaardinstelling is volle informatie maximum likelihood (FIML), welke automatisch de schattingen berekent met alle beschikbare informatie -- dit neemt aan dat er wordt gemist bij aselecte (MAR) missingness patterns. Een suboptimaal alternatief is beschikbaar in lijstgewijze verwijdering.

#### Schatter
Verschillende keuzes voor schatters zijn beschikbaar. Wij raden aan om dit te laten staan op `auto` voor de meeste (zo niet alle) doeleinden.

Referenties 
==========

- Jeremy C. Biesanz, Carl F. Falk & Victoria Savalei (2010) Assessing Mediational Models: Testing and Interval Estimation for Indirect Effects, Multivariate Behavioral Research, 45:4, 661-701, DOI: 10.1080/00273171.2010.498292
- Rosseel, Y. (2012). Lavaan: An R package for structural equation modeling and more. Version 0.5–12 (BETA). Journal of Statistical Software, 48(2), 1-36.
