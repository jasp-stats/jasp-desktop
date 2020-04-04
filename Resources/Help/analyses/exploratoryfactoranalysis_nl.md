Exploratieve Factoranalyse 
=== 

Met Exploratieve Factoranalyse kunt u één of meer onderliggende factoren van de data identificeren. De factoren zijn dusdanig gekozen dat zij gemeenschappelijke variantie dekken. 

### Assumpties (Yong & Pearce, 2013)
- De variabelen in de analyse zijn continu. 
- De data heeft een multivariate normale verdeling. 
- Er is een lineaire relatie tussen de variabelen en de factoren. 
- Er is geen multicollineariteit en singulariteit in de data. 

### Invoer 
---
#### Invoerveld 
- Ingevoegde Variabelen: In dit veld selecteert u de variabelen waarmee u de exploratieve factoranalyse uitvoert. 

#### Aantal Factoren
- Hier specificeert u het aantal factoren waar de rotatie op wordt toegepast. Diverse methodes bepalen hoe dit aantal gekozen wordt:   
  - Parallel Analyse: Factoren worden gekozen op basis van parallel analyse. Met deze methode worden factoren geselecteerd met een eigenwaarde die hoger is dan de parallel gemiddelde willekeurige eigenwaarde. Dit is de standaardoptie. 
  - Eigenwaardes: Factoren worden gekozen indien zij een bepaalde eigenwaarde hebben. Als standaardoptie worden factoren met een eigenwaarde van 1 of hoger gekozen. Dit wordt het Kaiser criterium genoemd. 
  - Handmatig: Het aantal factor kan handmatig gespecificeerd worden. De standaardoptie is 1. 

#### Rotatie 
- De rotatiemethode die wordt toegepast op de factoren, kan hier gespecificeerd worden.
  - Orthogonaal: Deze methode produceert ongecorreleerde factoren. Diverse mogelijkheden zijn beschikbaar: 
      - Geen: Geen rotatiemethode wordt geselecteerd. 
      - varimax: Orthogonale rotatiemethode varimax. Deze rotatie is gebaseerd op het maximaliseren van de variantie van de ladingen. 
      - quartimax: Orthogonale rotatiemethode quartimax. Voor deze methode worden het aantal factoren die nodig zijn om iedere variabele te verklaren geminimaliseerd.
      - bentlerT: Orthogonale rotatiemethode bentlerT. 
      - equamax: Orthogonale rotatiemethode equamax. Dit is een combinatie van varimax en quartimax. 
      - varimin: Orthogonale rotatiemethode varimin. 
  - Oblique: Deze methode produceert factorendat correlaties tussen factoren toestaat. Dit is de standaardoptie. Diverse mogelijkheden zijn beschikbaar: 
      - promax: Oblique rotatiemethode promax. Dit is de standaardoptie. 
      - oblimin: Oblique rotatiemethode oblimin. 
      - simplimax: Oblique rotatiemethode simplimax. 
      - bentlerQ: Oblique rotatiemethode bentlerQ. 
      - biquartimin: Oblique rotatiemethode biquartimin. 
      - cluster: Oblique rotatiemethode cluster. 

### Uitvoeropties 
- Markeer: Deze optie zet de waarde vanaf waar de paden schalen in breedte. Paden met absolute gewichten hoger dan deze waarde zullen steeds breder worden terwijl waardes eronder een vaste dunne breedte hebben. Alle paden krijgen een sterkere of zwakkere kleurintensiteit naarmate ze een sterker gewicht hebben. Als de waarde op 0 gezet wordt zullen alle paden een verschillende breedte krijgen.
- Voeg Tabellen Toe: 
    - Factorcorrelaties: Bij het selecteren van deze optie, wordt een tabel met de correlaties tussen factoren weergegeven. 
    - Aanvullende fit indices: Deze optie toont de wortel van de gemiddelde kwadraatsom fout (RMSEA) met 90% betrouwbaarheidsinterval, de Tucker Lewis Index (TLI), en de Bayesian Information Criterion (BIC) om de model fit te testen. 
    - Paddiagram: Bij het selecteren van deze optie wordt een visuele representatie van de richting en de sterkte van de relatie tussen de variabele en de factor weergegeven. 
    - Screeplot: Bij het selecteren van deze optie, wordt een screeplot getoond. De screeplot geeft informatie over hoeveel variantie in de data, aangegeven door de eigenwaarde, wordt verklaard door elke factor. Een screeplot kan gebruikt worden om te beslissen over de hoeveelheid van de factoren. 
- Ontbrekende waarden: 
    - Sluit paarsgewijs uit: Indien 1 observatie van een variabele mist, worden de observaties van de andere variabelen nog wel gebruikt voor de analyse. In dit scenario is het niet nodig om voor elke variabele een observatie te hebben. Dit is de standaardoptie. 
    - Sluit lijstgewijs uit: Indien 1 observatie van een variabele mist, wordt de gehele casus (dus alle andere variabelen van dezelfde casus) uitgesloten voor analyse. In dit scenario zijn is voor elke variabele een observatie nodig. 

### Uitvoer 
--- 
#### Exploratieve Factoranalyse
Factorladingen:  
- Variabelen: De eerste kolom toont alle variabelen die zijn meegenomen in de analyse. 
- PC (1, 2, 3, etc.): Deze kolom toont de factorladingen op de variabele. 
- Uniciteit: Het percentage van de variantie van elke variabele dat niet verklaard wordt door de factor.

Factor Correlaties:  
- De correlaties tussen de factoren. 

Chi-squared Toets: 
De fit van het model wordt getoetst. Als de toets significant is, dan wordt het model verworpen. Onthoud dat een chi-kwadraat schatting onbetrouwbaar kan zijn voor kleine steekproeven, en bij hele grote steekproeven kan de chi-kwadraattoets het model te snel verwerpen. Aanvullende informatie over de fit van het model kan verkregen worden door de optie `Aanvullende pas indexen` onder `Uitvoeropties` te selecteren. Voor een verdere discussie over fit indices kan bijvoorbeeld Saris, Satorra, & van der Veld (2009) geraadpleegd worden. 
- Model: Het verkregen model van de exploratieve factoranalyse. 
- Value: De chi-squared toetsstatistiek.  
- vg: Vrijheidsgraden. 
- p: P-waarde. 

Aanvullende Fit Indices: 
Deze fit indices geven informatie over de fit van het model. 
- Model: Het verkregen model van de exploratieve factoranalyse. 
- RMSEA: De wortel van de gemiddelde kwadraatsom fout van de schatting (RMSEA). Corrigeert voor spaarzaamheid. Wanneer een model hetzelfde presteert, maar het model 1 vrijheidsgraad dan model 2, wordt model 1 aangeraden. Browne and Cudeck (1993) benoemt een waarde kleiner dan 0.08 als acceptabele model fit, kleiner dan 0.05 een goede model fit, en adviseert om modellen met een waarde van 0.1 of hoger te verwerpen. Er is echter geen overeenstemming over deze grens. 
- RMSEA 90% betrouwbaarheidsinterval: Het 90% betrouwbaarheidsinterval van de wortel van de gemiddelde kwadraatsom fout van de schatting. 
- TLI: Tucker-Lewis Index. Evalueert de fit vergeleken met een striktere, genestelde baseline model. Hopwood and Donnallan (2010) suggereerde dat een waarde hoger dan .9 een goede fit aangeeft. Er is echter geen consensus over deze grens. 
- BIC: Bayesian Information Criterion. Deze maat is nuttig voor het vergelijken van de prestatie van verschillende modellen op dezelfde data. Een lage waarde impliceert een betere fit. 

#### Paddiagram 
- F(1,2,3,...): De factoren in het model zijn weergegeven als cirkels.  
- Variabelen: De variabelen zijn weergegeven als rechthoeken. 
- Pijlen: Gaan van de factoren naar de variabelen, toont de lading van de factor op de variabele. Rood betekent een negatieve lading, groen een positieve lading. Hoe wijder de pijlen, hoe hoger de lading. Deze markering kan aangepast worden bij `Markeren` in de `Uitvoeropties`. 

#### Screeplot 
De screeplot geeft informatie over hoeveel variantie in de data, aangegeven door de eigenwaarde, wordt verklaard door elke factor. Een screeplot kan gebruikt worden om te beslissen over de hoeveelheid van de factoren. 
- Factors: Op de x-as, alle mogelijke factoren. 
- Eigenvalue: Op de y-as, de eigenwaarde die de verklaarde variantie van elke factor aangeeft. 
- Data: De gestippelde lijn staat voor de data. 
- Gesimuleerd: De driehoekslijn staat voor de gesimuleerde data. Deze lijn is indicatief voor de parallel analyse. Als de punten van de gestippelde lijn (werkelijke data) boven deze lijn zijn, worden deze factoren meegenomen in het model door parallel analyse. 
- Kaiser criterium: De horizontale lijn op de eigenwaarde van 1 staat voor het Kaiser criterium. Volgens dit criterium dienen enkel factoren met waarden boven deze lijn (eigenwaarde van 1) mee te worden genomen in het model. 

### Referenties 
---
- Brown, T. A. (2014). *Confirmatory factor analysis for applied research*.     
    Guilford Publications. 
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention     
    decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational research methods, 7*(2), 191-205.
- Hopwood, C. J., & Donnellan, M. B. (2010). How should the internal structure 
    of personality inventories be evaluated? *Personality and Social Psychology Review, 14*, 332–346. 
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in 
    exploratory factor analysis. *Best practices in quantitative methods*, 86-99.
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. Structural Equation Modeling, 16(4), 561-582.
- Yong, A. G., & Pearce, S. (2013). A beginner’s guide to factor analysis: Focusing on exploratory factor analysis. *Tutorials in quantitative methods for psychology, 9*(2), 79-94.

### R Packages 
--- 
- ggplot2
- psych
- qgraph
- stats

### Voorbeeld 
---
- Voor een voorbeeld ga naar `File`-->`Data library`-->`Factor`-->`G Factor`. 
- Voor meer details over Exploratieve Factoranalyse in JASP, zie <a href="https://www.youtube.com/watch?v=dUPzMBqcMjo&feature=youtu.be">video</a>. 
