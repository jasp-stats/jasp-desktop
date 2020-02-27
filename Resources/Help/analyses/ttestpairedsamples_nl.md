Gepaarde T-Toets
==========================

Met de gepaarde t-toets kan de gebruiker de effectgrootte schatten en de nulhypothese testen dat het populatiegemiddelde van het verschil tussen gepaarde observaties gelijk is aan 0.

### Assumpties
- De verschilscore is continu. 
- De verschilscores komen uit een aselecte steekproef uit de populatie. 
- De verschilscore is normaal verdeeld in de populatie.  

### Invoer
-------
#### Invoerveld
- Variabelen: In deze box zijn de variabelen geselecteerd waarvoor het verschil is berekend. Meerdere verschillen kunnen tegelijkertijd worden geanalyseerd door het specificeren van verschillende rijen met twee variabelen waarvoor het verschil is berekend. Met andere woorden, iedere rij geeft andere verschilscores weer.  

#### Toetsen  
- Student: De student's t-toets. Dit is de standaardoptie. 
- Wilcoxon signed-rank: Wilcoxon signed-rank test.

#### Alt. Hypothese 
- Maat 1 &ne; Maat 2: Tweezijdige alternatieve hypothese dat het populatiegemiddelde van het verschil niet gelijk is aan 0. Dit is de standaardoptie. 
- Maat 1 &ne; Maat 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van het verschil groter is dan 0. 
- Maat 1 &ne; Maat 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde kleiner is dan 0. 

#### Assumptie Checks 
- Normaliteit testen: Shapiro-Wilk toets voor normaliteit. 

#### Aanvullende Statistieken
- Plaatsparameter: Voor de Student's t-toets wordt de plaatsparameter gegeven met het gemiddelde verschil; voor de Wilcoxon signed-rank test wordt de plaatsparameter gegeven met de Hodges-Lehmann schatting. 
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de plaatsparameter. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Effectgrootte: Bij de Student t-toets wordt de effectgrootte gegeven met Cohen's d; voor de Wilcoxon test wordt de effectgrootte gegeven met de rank biserial correlatie. 
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de effectgrootte gebaseerd op een niet-centrale t-verdeling voor Cohen's d, Glass' delta en Hedges' g, en de normaal benadering van de Fisher getransformeerde rank biseriële correlatie. 
- Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaarddeviatie, standaard fout van het gemiddelde voor iedere maat.
- Beschrijvende grafieken: Geeft het steekproefgemiddelde en de betrouwbaarheidsinterval weer voor iedere maat (zie Morey [2008] voor de berekening van de standaardfout van het gemiddelde in gepaarde designs). 
  - Betrouwbaarheidsinterval: De betrouwbaarheidsintervallen worden weergegeven in percentages. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Vovk-Sellke Maximum *p*-Ratio: De grens 1/(-e *p* log(*p*)) wordt afgeleid van de vorm van de verdeling van de *p*-waarde. Onder de nulhypothese (H<sub>0</sub>) is het uniform(0,1), en onder de alternatieve (H<sub>1</sub>) neemt hij af in *p*, bijv., een beta(&#945;, 1) vergelijking, waarin 0 < &#945; < 1. The Vovk-Sellke MPR wordt verkregen door de vorm van &#945; van de verdeling onder H<sub>1</sub> zodat de verkregen *p*-waarde *maximaal diagnostisch* is. De waarde is dan de ratio van de dichtheid op punt *p* onder H<sub>0</sub> en H<sub>1</sub>. Bijvoorbeeld, als de tweezijdige *p*-waarde gelijk is aan .05, dan is de Vovk-Sellke MPR gelijk aan 2.46, wat aangeeft dat deze *p*-waarde op zijn hoogst 2.46 keer meer kans heeft om voor te komen onder H<sub>1</sub> dan onder H<sub>0</sub>.

#### Ontbrekende Waarden
 - Het uitsluiten van gevallen, analyse bij analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle gevallen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets. Dit is de standaardoptie. 
 - Het uitsluiten van gevallen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de gevallen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen. 

### Uitvoer 
--- 

#### Gepaarde T-Toets
- De eerste kolom bevat de twee variabelen waarvoor het verschil is berekend.
- Toets: Het type toets dat is geselecteerd. Als er maar een toets is geselecteerd wordt deze kolom niet weergegeven. In dit geval geeft de tabel alleen de resultaten van de geselecteerde toets weer. 
- Statistiek: De toets statistiek. Voor de Student's t-toets is dit de waarde van de t-statistiek. Voor de Wilcoxon signed-rank test is dit de waarde van de W-statistiek. 
- df: Vrijheidsgraden.
- p: De p-waarde.
- Gemiddelde verschil: Het gemiddelde van de verschilscores. Deze kolom heet alleen 'Gemiddelde verschil' wanneer de toets `Student` is geselecteerd. Wanneer de toets `Wilcoxon signed-rank` is geselecteerd, heet deze kolom 'Plaatsparameter'. 
- Plaatsparameter: Voor de Student's t-toets, de plaatsparameter is gegeven in verschil in gemiddelde; voor de Wilcoxon signed-rank test wordt de plaatsparameter gegeven met de Hodges-Lehmann schatting. Deze kolom heet alleen 'Plaatsparameter' wanneer de `Wilcoxon signed-rank` toets en/of de Z toets is geselecteerd. Deze kolom heet in alle andere gevallen 'Gemiddelde verschil'. 
- SE Verschil: De standaardfout van het gemiddelde van de verschilscores.  
- % BI voor gemiddelde verschil/plaatsparameter: Het betrouwbaarheidsinterval voor het gemiddelde verschil/de plaatsparameter van de verschilscores. De standaardoptie is 95%. 
  - Linker: De linkergrens van het betrouwbaarheidsinterval. 
  - Rechter: De rechtergrens van het betrouwbaarheidsinterval.  
- Effectgrootte: Voor de Student t-toets wordt de effectgrootte gegeven met Cohen's d; Voor de Wilcoxon test wordt de effectgrootte gegeven met de gematchte rank biseriële correlatie. 
- % BI voor effectgrootte: Het betrouwbaarheidsinterval voor de effectgrootte. De standaardoptie is 95%. 
  - Linker: De linkergrens van het betrouwbaarheidsinterval. 
  - Rechter: De rechtergrens van het betrouwbaarheidsinterval.

#### Assumptie Checks 
Normaliteit toets (Shapiro-Wilk)
- De eerste kolom bevat de twee variabelen waarvan het verschil is berekend.
- W: De waarde van de W toets statistiek. 
- p: De p-waarde.

#### Beschrijvende Statistiek
- De eerste kolom bevat de variabele. 
- N: De steekproefgrootte per variabele. 
- Gemiddelde: Het gemiddelde van de variabele.
- SD: Standaarddeviatie van het gemiddelde. 
- SE: Standaardfout van het gemiddelde. 

#### Beschrijvende Grafieken 
- Geeft het steekproefgemiddelde weer (black bullet), het % betrouwbaarheidsinterval (whiskers) voor iedere maat.  

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). *Tutorials in Quantitative Methods for Psychology, 4*, 61-64.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.


### R-packages 
---
- stats 

### Voorbeeld
--- 
- Voor een voorbeeld ga naar `Open` --> `Bibliotheek` --> `T-Tests` --> `Moon and Agression` 
