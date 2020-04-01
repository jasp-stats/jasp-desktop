Confirmatieve Factoranalyse
==========================

Confirmatieve Factoranalyse (CFA) modelleert geobserveerde variabelen (indicatoren) als rumoerige manifestaties van onderliggende latente variabelen (factoren). JASP's CFA is gemaakt met `lavaan` (lavaan.org; Rosseel, 2012), een `R` package voor structural equation modeling. Zie Brown (2014) of Kline (2015) voor boeken over het onderwerp, CFA.

Invoerveld
-------
In het invoerveld kunt u continue variabelen van uw dataset toewijzen aan verschillende factoren. Er is een minimum van één factor, en elke factor heeft minstens twee indicatoren. U kunt factoren toevoegen door te drukken op de (+) knop en verwijderen door te drukken op de (-) knop. U kunt de factoren hernoemen door de namen boven de invoervelden te wijzigen.

Tweede-orde factor
-------
JASP staat toe dat factoren op hun beurt indicatoren worden van een tweede-orde factor. Dit kan gespecificeerd worden door de factornamen naar het tweede orde invoerveld te slepen. Alle factoren die geen indicatoren van de tweede-orde factor zijn, kunnen covariëren met elkaar en met de tweede-orde factor, maar niet met de indicatoren van de tweede-orde factor.

Model opties
-------
- Voeg gemiddelde structuur toe: Toon gemiddeldes van de indicatoren en, in the geval van multi-groep CFA, de gemiddeldes van de latente variabelen.
- Neem ongecorreleerde factoren aan: Zet de correlatie tussen verschillende latente variabelen op 0.
- Factor schalen: Factoren kunnen op drie manier geschaald zijn:
  - Factor varianties (standaardoptie): De factor heeft een vaste variantie van 1.
  - Marker variabele: De factor heeft dezelfde schaal als zijn eerste indicator aangezien zijn factorlading 1 is.
  - Effecten coderen: Het gemiddelde van de factorlading is vastgezet op 1. Voor meer informatie over de interpretatie van effecten coderen, zie Veen (2018).
- Residu covarianties: Om ook covariantie toe te staan tussen indicatoren die niet verklaard worden door hun respectieve factor, bijvoorbeeld omdat vragen in een vergelijkbare manier verwoord zijn, sleep twee variabelen naar het rechter invoerveld. 

Aanvullende uitvoer
-------
- Extra pasmaten: Kies deze om de waarde van verschillende model pasmaten te tonen in de resultaten.
- Geïmpliceerde covariantiematrix: Toon de covariantiematrix die het model impliceert.
- Residu covariantiematrix: Toon de covarianties tussen indicatoren die behouden blijft met het model. Een perfect model toont enkel 0-en hier. 
- Modificatie indices: Toont MIs met een minimum grens. Een MI toont hoeveel de chi-square waarde van de passing zou veranderen als de gespecificeerde parameter vrij zou zijn. EPC toont de verwachten verandering van de parameter zelf.
- Toon lavaan syntax: Toon de lavaan modeleer syntax die nodig zou zijn om het model in R weer te geven.

Multigroep CFA
------
- Groepen: Selecteer hier een categorische variabele om CFA modellen voor iedere groep te creëren. 
- Invariantie testen: Selecteer een niveau van beperkende parameters over de verschillende groepen. 
  - Configureel: De verschilllende groepen hebben dezelfde CFA structuur.
  - Metrisch: De factorladingen van de groepen zijn gelijk.
  - Scalar: De factorladingen en gemiddeldes van de indicatoren van de groepen zijn gelijk.
  - Strikt: De factorladingen, gemiddeldes van de indicatoren, residu varianties, en residu covarianties van de groepen zijn gelijk.
  
Grafieken
-------
- Misfit grafiek: Visualisatie van de residu correlaties (gestandaardiseerde residu covariantiematrix) van de indicatoren.
- Model grafiek: Visualisatie van de geschatte modelstructuur.

Geavanceerd
-------
- Emulatie: Emuleer resultaten van verschillende software.
- Foutberekening: Wijzigt de manier waarop de standaardfout wordt berekend.
- Estimator: Wijzigt de estimator voor de CFA.
- Standaardisatie: Toon gestandardiseerde parameters voor verschillende standaardisatieschema's.

Referenties
-------
- Brown, T. A. (2014). Confirmatory factor analysis for applied research. _Guilford Publications_.
- Kline, R. B. (2015). Principles and practice of structural equation modeling. _Guilford publications_.
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. _Journal of Statistical Software, 48_(2), 1-36. URL www.jstatsoft.org/v48/i02/
- Veen, D., Little, T.D. & van de Schoot, R. (2018). Effects Coding as Unbiased Alternative to Scale Scores. (manuscript under review).

### R Packages
---
- ggplot2
- lavaan
- reshape2
- semPlot
- stats
