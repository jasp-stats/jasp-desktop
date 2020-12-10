T-Toets voor Eén Gemiddelde
==========================

Met de t-toets voor één gemiddelde kan de gebruiker de effectgrootte schatten en de nulhypothese testen dat het populatiegemiddelde gelijk is aan een specifieke constante, bijvoorbeeld de toetswaarde.

### Assumpties
- De afhankelijke variabele is continu.
- De data komen uit een aselecte steekproef uit de populatie.
- De afhankelijke variabele is normaal verdeeld in de populatie.

### Invoer
-------
#### Invoerveld
- Variabelen: De variabelen in dit veld geselecteerde variabelen worden meegenomen in de analyse.

#### Toetsen  
- Student: De student's t-toets. Dit is de standaardoptie.
- Wilcoxon rangtekentoets: Wilcoxon rangtekentoets.
- Z-toets: De Z-toets.
  - Toetswaarde: De toetswaarde bij de Z-toets. Deze staat standaard op 0.
  - Std.afwijking: De standaardafwijking die is toegepast in de Z-toets. Deze staat standaard op 1.

#### Hypothese
- &ne; Toetswaarde: Tweezijdige alternatieve hypothese dat het populatiegemiddelde niet gelijk is aan de toetswaarde. Dit is de standaardoptie.
- &gt; Toetswaarde: Eénzijdige alternatieve hypothese dat het populatiegemiddelde groter is dan de toetswaarde.
- &lt; Toetswaarde: Eénzijdige alternatieve hypothese dat het populatiegemiddelde kleiner is dan de toetswaarde.

#### Verificatie van aannames
- Normaliteit: Shapiro-Wilk toets voor normaliteit.

#### Aanvullende Statistieken
- Locatieschatting: Voor de Student's t-toets en de Z-toets is de locatieschatting de steekproefgemiddelde; voor de Wilcoxon rangtekentoets is de locatieschatting berekend met behulp van de Hodges-Lehmann schatter.
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de locatieparameter. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Effectgrootte: Bij de Student t-toets wordt de effectgrootte gegeven met Cohen's d; bij de Wilcoxon toets wordt de effectgrootte gegeven met de gematchte rank biseriële correlatie; voor de Z-toets wordt de effectgrootte gegeven met Cohen's d (gebaseerd op de gegeven standaardafwijking van de populatie).
  - Betrouwbaarheidsinterval: Betrouwbaarheidsinterval voor de effectgrootte.
- Beschrijvend: Steekproefgrootte, steekproefgemiddelde, steekproefstandaarddeviatie, standaardfout van het gemiddelde voor elke maat.
- Beschrijvende grafieken: Geeft het steekproefgemiddelde en de betrouwbaarheidsinterval weer.
  - Betrouwbaarheidsinterval: De betrouwbaarheidsintervallen worden weergegeven in percentages. De standaardoptie is 95%. Dit kan naar het gewenste percentage worden aangepast.
- Vovk-Sellke Maximum *p*-Ratio: De grens 1/(-e *p* log(*p*)) wordt afgeleid van de vorm van de verdeling van de *p*-waarde. Onder de nulhypothese (H<sub>0</sub>) is het uniform (0,1), en onder de alternatieve (H<sub>1</sub>) neemt hij af in *p*, bijv., een beta(&#945;, 1) vergelijking, waarin 0 < &#945; < 1. De Vovk-Sellke MPR wordt verkregen door de vorm van &#945; van de verdeling onder H<sub>1</sub> zodat de verkregen *p*-waarde *maximaal diagnostisch* is. De waarde is dan de ratio van de dichtheid op punt *p* onder H<sub>0</sub> en H<sub>1</sub>. Bijvoorbeeld, als de tweezijdige *p*-waarde gelijk is aan .05, dan is de Vovk-Sellke MPR gelijk aan 2.46, wat aangeeft dat deze *p*-waarde hoogstens 2.46 keer meer kans heeft om voor te komen onder H<sub>1</sub> dan onder H<sub>0</sub>.

#### Ontbrekende Waarden
 - Het uitsluiten van waarnemingen, analyse bij analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets. Dit is de standaardoptie.
 - Het uitsluiten van waarnemingen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde over alle toetsen.

### Uitvoer
-------

#### T-Toets voor één gemiddelde
- De eerste kolom bevat de variabelen waarvoor de analyse is uitgevoerd.
- Toets: Het type toets dat is geselecteerd. Als er maar een toets is geselecteerd, wordt deze kolom niet weergegeven. In dit geval geeft de tabel alleen de resultaten van de geselecteerde toets weer.
- Statistiek: De statistische toets. Voor de Student's t-toets is dit de waarde van de t-statistiek. Voor de Wilcoxon rangtekentoets is dit de waarde van de W-statistiek. Voor de Z-toets is dit de waarde van de Z-statistiek.
- vg: Vrijheidsgraden.
- p: De p-waarde.
- Steekproefgemiddelde: Deze kolom heet alleen 'Steekproefgemiddelde' wanneer de toets `Student` is geselecteerd. Wanneer de toets `Wilcoxon rangtekentoets` en/of de Z-toets is geselecteerd, heet deze kolom 'Locatieschatting'.
- Locatieschatting: Voor de Student's t-toets en de Z toets is de locatieschatting de steekproefgemiddelde; voor de Wilcoxon rangtekentoets toets is de locatieschatting de Hodges-Lehmann schatting. Deze kolom heet alleen 'Locatieschatting' wanneer de `Wilcoxon rangtekentoets` toets en/of de Z-toets is geselecteerd. Deze kolom heet in alle andere gevallen 'Steekproefgemiddelde'.
- Std. Fout: De standaardfout van het gemiddelde van de scores.  
- % BI voor gemiddelde/locatieparameter: Het betrouwbaarheidsinterval voor het gemiddelde/de locatieparameter van de scores. De standaardoptie is 95%.
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.  
- Effectgrootte: Voor de Student t-toets wordt de effectgrootte gegeven met Cohen's d; voor de Wilcoxon toets wordt de effectgrootte gegeven met de gematchte rank biseriële correlatie; voor de Z-toets wordt de effectgrootte gegeven met Cohen's d (gebaseerd op de gegeven standaardafwijking van de populatie).
- % BI voor effectgrootte: Het betrouwbaarheidsinterval voor de effectgrootte. De standaardoptie is 95%.
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.

#### Verificatie van aannames
Toets voor normaliteit (Shapiro-Wilk)
- De eerste kolom bevat de variabelen waarvoor de analyse is uitgevoerd.
- W: De waarde van de W-toetsstatistiek.
- p: De p-waarde.

#### Beschrijvende Statistiek
- De eerste kolom bevat de variabele.
- N: De steekproefgrootte per variabele.
- Gemiddelde: Het gemiddelde van de variabele.
- SD: Standaarddeviatie van het gemiddelde.
- Std. Fout: Standaardfout van het gemiddelde.

#### Beschrijvende Grafieken
- Geeft het steekproefgemiddelde weer (black bullet), de % betrouwbaarheidsinterval (whiskers), en de waarde van de teststatistiek (onderbroken lijn).

### Referenties
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R-packages
---
- stats
- BSDA

### Voorbeeld
---
- Voor een voorbeeld, ga naar `Open` --> `Bibliotheek` --> `T-Toetsen` --> `Gewichtstoename`.  
