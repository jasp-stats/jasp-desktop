Binomiaal Toets
====
De binomiaal toets laat de gebruiker testen of een proportie van een dichotome variabele gelijk is aan een toetswaarde (= aangenomen populatiewaarde).

### Assumpties
- De variabele is op dichotome schaal gemeten.
- De observaties zijn onafhankelijk.

### Invoer 
----
- Toetswaarde: De proportie van de variabele onder de nulhypothese.

#### Alt. hypothese
- *&ne; Toetswaarde*: Tweezijdige alternatieve hypothese dat de proportie niet gelijk is aan de toetswaarde.
- *&gt; Toetswaarde*: Eénzijdige alternatieve hypothese dat de proportie groter is dan de toetswaarde.
- *&lt; Toetswaarde*: Eénzijdige alternatieve hypothese dat de proportie kleiner is dan de toetswaarde.

#### Aanvullende Statistieken
- Betrouwbaarheidsinterval: Dekking van de betrouwbaarheidsinterval in percentages. de standaardwaarde is 95.
- Vovk-Sellke Maximum *p*-Ratio: de grens 1/(-e *p* log(*p*)) is afgeleid van de vorm van de *p*-waarde-verdeling. Onder de nulhypothese (H<sub>0</sub>) is het uniform(0,1), en onder de alternatieve (H<sub>1</sub>) is het dalend in *p*, e.g., een beta(&#945;, 1)verdeling, met 0 < &#945; < 1. De Vovk-Sellke MPR is verkregen door de vorm &#945; te kiezen van de verdeling onder H<sub>1</sub> zodat de behaalde *p*-waarde *maximaal diagnostisch* is. De waarde is dan de ratio van de verdelingen op punt *p* onder H<sub>0</sub> en H<sub>1</sub>.
Bijvoorbeeld, als de tweezijdige *p*-waarde gelijk is aan .05, dan is de Vovk-Sellke MPR gelijk aan 2.46, wat betekent dat deze *p*-waarde hoogstens 2.46 keer meer waarschijnlijk is onder H<sub>1</sub> dan onder H<sub>0</sub>.

#### Plots
- Beschrijvende plots: de proportie en betrouwbaarheidsinterval van de twee verschillende waarden van uw dichotome variabele.
- Betrouwbaarheidsinterval: Dekking van de betrouwbaarheidsinterval in percentages. De standaardwaarde is 95.
 
### Uitvoer
-----------
#### Binomiaal Toets
- Niveau: De twee opties van de dichotome variabele
- Aantallen: Het aantal waarnemingen op een bepaald level van de dichotome variabele. Het aantal van alle mogelijke levels geeft het totaal.
- Totaal: Het totaal aantal observaties.
- Proportie: Berekend met aantallen/totaal.
- p: P-waarde.
- VS-MPR: Vovk-Sellke maximum p-ratio.

#### Beschrijvende Grafieken
- Vertoon de proportie van de gekozen level (zwarte punt) en de breedte van de 95% geloofwaardigheidsinterval. De horizontale stippellijn laat zien wanneer de proporties van de levels gelijk zijn (= 0.5).

### Referenties
---
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p values for testing precise null hypotheses. *de American Statistician, 55*(1), 62-71.
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.

### R Packages
---
- ggplot2
- stats
