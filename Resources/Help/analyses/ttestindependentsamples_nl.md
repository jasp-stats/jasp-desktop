T-Toets voor Onafhankelijke Steekproeven
==========================

Met de t-toets voor onafhankelijke steekproeven kan de gebruiker de effectgrootte schatten en de nulhypothese dat steekproefgemiddelden van twee onafhankelijke steekproeven gelijk zijn toetsen. 

### Assumpties
- De afhankelijke variabele is continu. 
- De data van beide groepen komen uit een aselecte steekproef uit de populatie. 
- De afhankelijke variabele is normaal verdeeld in beide populaties. 
- De populatie varianties in beide groepen zijn homogeen.

### Invoer
-------

#### Invoerveld
- Variabelen: In deze box wordt de afhankelijke variabele geselecteerd.  
- Groeperende Variabele: In deze box wordt de variabele die de groepen definieert geselecteerd. 

#### Toetsen
- Student: De student t-toets. Dit is de standaardoptie. 
- Welch: Welch's t-toets. 
- Mann-Whitney: Mann-Whitney test. 

#### Alt. Hypothese 
- Groep 1 &ne; Groep 2: Tweezijdige alternatieve hypothese dat de populatiegemiddelden gelijk zijn. Dit is de standaardoptie. 
- Groep 1 &ne; Groep 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van groep 1 groter is dan het populatiegemiddelde van groep 2. 
- Groep 1 &ne; Groep 2: Eenzijdige alternatieve hypothese dat het populatiegemiddelde van groep 1 kleiner is dan het populatiegemiddelde van groep 2. 

#### Verificatie van aannames
- Normaliteit: Shapiro-Wilk toets voor normaliteit. 
- Gelijkheid van varianties: Levene's test voor homogeniteit van varianties. 

#### Aanvullende Statistieken
- Locatieparameter: Voor de Student's t-toets en Welch's T-toets wordt de locatieparameter gegeven met het gemiddelde verschil; voor de Mann-Whitney test wordt de locatieparameter gegeven met de Hodges-Lehmann schatting. 
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de locatieparameter. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Effectgrootte: Bij de Student t-toets en de Welch t-toets kan de effectgrootte hier beneden worden geselecteerd. Voor de Mann-Whitney wordt de effectgrootte gegeven met de rank biserial correlatie. 
  - Cohen's d: Bij de Student t-toets gebruikt deze de samengenomen standaarddeviatie om het gemiddelde verschil de standaardiseren. Voor de Welch's t-toets wordt de vierkantswortel van de gemiddelde variantie gebruikt om het gemiddelde verschil te standaardiseren.
  - Glass's delta: Gebruikt de standaarddeviatie van groep 2 om het gemiddelde verschil te standaardiseren. Om aan te passen welke groep wordt gebruikt als groep 2, kunt u de volgorde van de niveaus aanpassen door op de naam van de groeperende variabele te klikken in het data scherm. Klik op een van de niveaus en klik dan op de pijltjes om de volgorde aan te passen.
  - Hedges' g: Past een corrigerende factor toe op Cohen's d tegen bias. 
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de effectgrootte gebaseerd op een niet-centrale t-verdeling voor Cohen's d, Glass' delta en Hedges' g, en de normaal benadering van de Fisher getransformeerde rank biseriële correlatie. 
- Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaarddeviatie, standaardfout van het gemiddelde voor iedere groep.
- Beschrijvende grafieken: Geeft het steekproefgemiddelde en de betrouwbaarheidsinterval weer voor iedere groep. 
  - Betrouwbaarheidsinterval: De betrouwbaarheidsintervallen worden weergegeven in percentages. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Vovk-Sellke Maximum *p*-Ratio: De grens 1/(-e *p* log(*p*)) wordt afgeleid van de vorm van de verdeling van de *p*-waarde. Onder de nulhypothese (H<sub>0</sub>) is het uniform(0,1), en onder de alternatieve (H<sub>1</sub>) neemt hij af in *p*, bijv., een beta(&#945;, 1) vergelijking, waarin 0 < &#945; < 1. The Vovk-Sellke MPR wordt verkregen door de vorm van &#945; van de verdeling onder H<sub>1</sub> zodat de verkregen *p*-waarde *maximaal diagnostisch* is. De waarde is dan de ratio van de dichtheid op punt *p* onder H<sub>0</sub> en H<sub>1</sub>. Bijvoorbeeld, als de tweezijdige *p*-waarde gelijk is aan .05, dan is de Vovk-Sellke MPR gelijk aan 2.46, wat aangeeft dat deze *p*-waarde op zijn hoogst 2.46 keer meer kans heeft om voor te komen onder H<sub>1</sub> dan onder H<sub>0</sub>.

#### Ontbrekende Waarden
 - Het uitsluiten van waarnemingen, analyse voor analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets. Dit is de standaardoptie. 
 - Het lijstgewijze uitsluiten van waarnemingen: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen. 

### Uitvoer
-------

#### T-Toets voor Onafhankelijke Steekproeven 
- De eerste kolom bevat de variabelen waarvoor de analyse is uitgevoerd.
- Toets: Het type toets dat is geselecteerd. Als er maar een toets is geselecteerd wordt deze kolom niet weergegeven. In dit geval geeft de tabel alleen de resultaten van de geselecteerde toets weer. 
- t: De waarde van de t-waarde. 
- W: De toets statistiek van de Wilcoxon toets. Deze wordt berekend door de rangordes van de eerste groep op te tellen (dezelfde procedure als door R wordt gebruikt). 
- vg: Vrijheidsgraden.
- p: De p-waarde.
- Gemiddelde verschil: Gemiddeld verschil tussen de steekproefgemiddelden. Deze kolom heet alleen 'Gemiddelde verschil' wanneer de toets `Student` of `Welch` is geselecteerd. Wanneer de toets `Mann-Whitney` is geselecteerd, heet deze kolom 'Locatieparameter'. 
- Locatieparameter: Voor de Student's t-toets en Welch's t-toets is de locatieparameter gegeven in verschil in gemiddelde; voor de Mann-Whitney toets wordt de locatieparameter gegeven met de Hodges-Lehmann schatting. Deze kolom heet alleen 'Locatieparameter' wanneer de `Mann-Whitney` t-toets is geselecteerd. Deze kolom heet in alle andere gevallen 'Gemiddelde verschil'. 
- Std. Fout Verschil: De standaardfout van het gemiddelde van de verschilscores. Dit wordt alleen weergegeven voor Student's t-toets en Welch's t-toets.
- % BI voor gemiddeld verschil/locatieparameter: Het betrouwbaarheidsinterval voor het gemiddeld verschil/de locatieparameter van de verschilscores. De standaardoptie is 95%. 
  - Onder: De ondergrens van het betrouwbaarheidsinterval. 
  - Boven: De bovengrens van het betrouwbaarheidsinterval.  
- Effectgrootte: Voor de Student t-toets en de Welch t-toets wordt de effectgrootte gegeven met Cohen's d/Glass' delta/Hedges' g; Voor de Mann-Whitney test wordt de effectgrootte gegeven met de gematchte rank biseriële correlatie. 
- % BI voor effectgrootte: Het betrouwbaarheidsinterval voor de effectgrootte. De standaardoptie is 95%. 
  - Onder: De ondergrens van het betrouwbaarheidsinterval. 
  - Boven: De bovengrens van het betrouwbaarheidsinterval.

#### Verificatie van aannames
Toets voor normaliteit (Shapiro-Wilk)
- De eerste kolom bevat de afhankelijke variabele.
- De tweede kolom bevat alle niveaus van de groeperende variabele.
- W: De waarde van de W toets statistiek. 
- p: De p-waarde.
Variantiegelijkheid toets (Levene's):
- De eerste kolom bevat de afhankelijke variabele. 
- F: De waarde van de F-statistiek. 
- vg: De vrijheidsgraden. 
- p: De p-waarde. 

#### Beschrijvende Statistiek
- De eerste kolom bevat de afhankelijke variabele. 
- Groep: De niveaus van de groeperende variabele.
- N: De steekproefgrootte per groep. 
- Gemiddelde: Het gemiddelde van de afhankelijke variabele per groep. 
- SD: Standaarddeviatie van het gemiddelde. 
- Std. Fout: Standaardfout van het gemiddelde. 

#### Beschrijvende Grafieken 
- Geeft het steekproefgemiddelde weer (black bullet), de % betrouwbaarheidsinterval (whiskers) voor iedere groep, de x-as geeft de groeperende variabele weer, en de y-as de afhankelijke variabele.  

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R-packages
---
- stats 
- car 
- MBESS

### Voorbeeld
--- 
- Voor een voorbeeld, ga naar `Open`--> `Data Library` --> `T-Tests` --> `Directed Reading Activities`. 


