Multinomiale Toets
===
Met de multinomiale toets kan de gebruiker toetsen of een geobserveerde verdeling van celtellingen overeenkomt met een verwachte verdeling. 

### Assumpties
- De gebruikte variabele moet categorisch zijn. 

### Invoer
---

#### Invoer veld
- Factor: De categorische variabele waarin we geinteresseerd zijn. 
- Tellingen (optioneel): De variabele die de tellingdata bevat. 
- Verwachte Tellingen (optioneel): Wanneer de data set een variabele bevat die de verwachtingen van celtellingen reflecteerd, kan die kolom hier ingevoerd worden. De waarden in deze variabele zullen worden geinterpreteerd als de nulhypothese.  

#### Alt. Hypothese
- Multinomiale toets: De multinomiale toets controleert of de geobserveerde celtellingen uniform verdeeld zijn. De nulhypothese wordt getoetst met de Pearson's chi-kwadraat toets statistieken. Deze meet de afwijking van het geobserveerde van de verwachte celtellingen onder de nulhypothese. De nulhypothese wordt getoetst tegenover de alternatieve hypothese dat de proporties van de categorie verschillen van de uniforme verdeling.
    - $$\chi^2$$ toets: De chi-kwaadraat goodness-of-fit toets bepaalt of de geobserveerde celtellingen afwijken van een specifieke verwachte verdeling. In de chi-kwadraat goodness-of-fit toets is de verwachte verdeling niet beperkt tot een uniforme verdeling. De eerste hypothese is standaard dat de celtellingen uniform verdeeld zijn, maar de verwachte tellingen kunnen door de gebruiker worden aangepast. Het is ook mogelijk om meerdere hypotheses the specificeren. De gespecificeerde nulhypotheses worden tegenover de alternatieve hypotheses getoetst, die inhoudt dat de proporties van de categorie afwijken van de uniforme verdeling. 

#### Aanvullende statistieken
- Beschrijvende statistieken: De optie om beschrijvende statistieken van de data weer te geven; de geobserveerde tellingen, de verwachte tellingen en de betrouwbaarheidsintervallen van de geobserveerde waarden.
  - Betrouwbaarheidsinterval: Het betrouwbaarheidsinterval in percentages. Deze staat standaard op 95%. De betrouwbaarheidsintervallen zijn gebaseerd op een procedure ontwikkeld door Clopper en Pearson (1934). Deze gaat uit van onafhankelijke binomiale verdelingen voor ieder factor niveau.
  - Vovk-Selke maximum *p*-ratio: De grens 1/(-e *p* log(*p*)) wordt afgeleid van de vorm van de verdeling van de *p*-waardes. Onder de nulhypothese (H<sub>0</sub>) is het uniform (0,1) en onder de alternatieve hypothese (H<sub>1</sub>) neemt hij af in *p*, bijv. een beta (&#945;, 1) verdeling waarin 0 < &#945; < 1. De Vovk-Selke MPR wordt verkregen door de vorm van &#945; onder H<sub>1</sub> te kiezen zodat de *p*-waarde *maximaal diagnostisch* is. De waarde is dan de ratio van de dichtheid op punt *p* onder H<sub>0</sub> en H<sub>1</sub>. Als de tweezijdige p-waarde bijvoorbeeld .05 is, is de Vovk-Sellke MPR 2.46. Dit geeft aan dat deze *p*-waarde maximaal 2.46 zo waarschijnlijk is onder H1 dan onder H<sub>0</sub>. 

#### Weergave
  - Tellingen: Met deze optie worden de beschrijvende statistieken weergegeven als absolute tellingen. 
  - Proporties: Met deze optie worden de beschrijvende statistieken weergegeven als een proportie van het totale aantal tellingen.

#### Grafieken
  - Beschrijvende Grafieken: Geeft de frequenties en betrouwbaarheidsintervallen van de geobserveerde tellingen weer.
      - Betrouwbaarheidsinterval

### Uitvoer
---
#### Multinomiale Toets
- Multinomiale Toets:
  - Hypothese: Wanneer de $$\chi^2$$ is geselecteerd en er zijn meerdere hypotheses, worden deze hier weergegeven.
  - $$\chi^2$$: De chi-kwadraat goodness-of-fit waarde. 
  - p: De p-waarde van de multinomiale toets, of de $$\chi^2$$ goodness-of-fit toets.
  - VS-MPR: Vovk-Sellke maximum p-ratio.
- Beschrijvende Statistieken:
  - De beschrijvende tabel bevat de categorien, de geobserveerde waarden, de verwachte waarden onder de gespecificeerde hypotheses, en de betrouwbaarheidsintervallen gebaseerd op onafhankelijke binomiale verdelingen. De beschrijvende statistieken worden weergegeven in tellingen of proporties. 

#### Beschrijvende Grafiek
De beschrijvende grafiek geeft de frequentie van gerapporteerde tellingen en de bijbehorende betrouwbaarheidsintervallen weer, voor ieder niveau van de variabele die is gebruikt. 

### Referenties
---
- Haberman, S. J. (1978). *Analysis of qualitative data: Introductory topics (Vol 1)*. Academic Press.
-  Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.

### R Packages
---
- ggplot2
- stats
