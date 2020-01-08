Bayesiaanse Multinomiale Toets
===

Met de Bayesiaanse multinomiale toets kan de gebruiker toetsen of een geobserveerde verdeling van celtellingen overeenkomt met een verwachte verdeling. 

### Assumpties
- De gebruikte variabele moet categorisch zijn. 

### Invoer 
---

#### Invoer veld
- Factor: De categorische variabele waarin we geinteresseerd zijn. 
- Tellingen (optioneel): De variabele die de tellingdata bevat. 
- Verwachte Tellingen (optioneel): Wanneer de data set een variabele bevat die de verwachtingen van celtellingen reflecteerd, kan die kolom hier ingevoerd worden. De waarden in deze variabele zullen worden geïnterpreteerd als de nulhypothese.  

#### Toetswaarden
- Gelijke proporties: Met de Bayesiaanse multinomiale toets, toetsen we de nulhypothese dat de celkansen uniform verdeeld zijn. De nulhypothese wordt getoetst tegenover de alternatieve hypothese die inhoudt dat alle categorie proporties vrij afwijken. 
- Verwachte proporties: Hier specificeren we de nulhypothese dat de celkansen gelijk zijn aan een specifieke verwachte verdeling. De eerste hypothese is standaard de nulhypothese van de Bayesiaanse multinomiale toets, maar de verwachte tellingen kunnen naar wens worden aangepast. De nulhypothese wordt getoetst tegenover de alternatieve hypothese, deze houdt in dat alle categorie proporties vrij afwijken. Het specificeren van meerdere hypotheses is mogelijk. 

#### Bayes Factor
- *BF10*: De Bayes factor die bewijs geeft voor de alternatieve hypothese ten opzichte van de nulhypothese. 
- *BF01*: De Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de alternatieve hypothese. 
- *Log(BF10)*: Natuurlijk logaritme van *BF10*.

#### Aanvullende Statistieken
- Beschrijvende statistieken: De optie om beschrijvende statistieken van de data weer te geven; de geobserveerde tellingen, de verwachte tellingen en de geloofwaardigheidsintervallen van de geobserveerde waarden.
- Geloofwaardigheidsinterval: Standaard 95%. Let op dat de geloofwaardigheidsintervallen zijn gebaseerd op onafhankelijke binomiale verdelingen met 'flat priors'. De berekening van de geloofwaardigheidsintervallen zijn gebaseerd op de procedure van Clopper en Pearson (1934). Deze procedure gaat uit van een Beta(0,1) prior verdeling wanneer de linkergrens van het geloofwaardigheidsinterval wordt berekend. Wanneer de rechtergrens wordt berekend, gebruiken ze een Beta(1,0) verdeling. 

#### Weergave
- Tellingen: Met deze optie worden de beschrijvende statistieken weergegeven als absolute tellingen. 
- Proporties: Met deze optie worden de beschrijvende statistieken weergegeven als een proportie van het totale aantal tellignen.
- Grafieken:
  - Beschrijvende grafieken: Geeft de frequenties en geloofwaardigheidsintervallen van de geobserveerde tellingen weer. 

### Prior
De optie om de prior verdeling aan te passen voor de vector van celkansen. 
- *Dirichlet*: Voor *K* categorien is de standaard prior Dirichlet(alpha_*1*, alpha_*2*, ..., alpha_*K*), met alle alfa parameters op 1. Let op dat de parameters van de *Dirichlet verdeling* prior tellingen weergeven. Dit wijst erop dat alle waardes niet-negatief moeten zijn.

### Uitvoer
---

#### Bayesiaanse Multinomiale Toets
- Hypotheses: Geeft alle gespecificeerde hypotheses weer.
- Niveaus: Geeft het aantal categorien weer van de gebruikte variabele. 
- *BF10*, *BF01*, or *Log(BF10)*: Geeft de Bayes factor weer, berekend met de door de gebruiker gedefinieerde prior. 

#### Beschrijvende statistieken
De beschrijvende tabel geeft de gebruikte categorien, de geobserveerde waarden, en de verwachte waarden onder de gespecificeerde hypothese. De beschrijvende statistieken worden weergegeven in tellingen of proporties, afhankelijk van wat de gebruiker heeft geselecteerd bij de optie 'Weergave'. Wanneer zo gespecificeerd, geeft de tabel ook het door de gebruiker gedefinieerde geloofwaardigheidsinterval.  

#### Beschrijvende grafiek
De beschrijvende grafiek geeft de frequentie van gerapporteerde tellingen en de bijbehorende geloofwaardigheidsintervallen weer, voor ieder niveau van de variabele die is gebruikt. 

### Referenties
---
- Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.
- Good, I. J. (1967). A Bayesian significance test for multinomial distributions. *Journal of the Royal Statistical Society: Series B (Methodological), 29*, 399-418.

### R Packages
---
- ggplot2
- stats
