Principale Componentenanalyse 
===

Principale componentenanalyse wordt gebruikt om data weer te geven in minder componenten dan waaruit de dataset oorspronkelijk bestaat. De componenten worden zo gekozen dat ze zoveel mogelijk variantie in de originele dataset verklaren.

### Assumpties
- De variabelen die worden meegenomen in de analyse zijn gecorreleerd (Shlens, 2014).
- De variabelen die worden meegenomen in de analyse zijn lineair gerelateerd (Shlens, 2014).

### Invoer
---
#### Invoerveld
- Meegenomen variabelen: In dit veld kunnen de variabelen worden ingevuld om een principale componentenanalyse op uit te voeren. 

#### Aantal Componenten 
- Hier wordt het aantal componenten gespecificeerd waarop de rotatie wordt toegepast. Er kunnen verschillende modellen worden gekozen om dit kunnen te bepalen:
  - Parallelle Analyse: Componenten worden geselecteerd op basis van parallelle analyse. Met deze methode worden componenten geselecteerd wanneer hun eigenwaarde groter is dan de parallelle gemiddelde willekeurige eigenwaarde. Dit is de standaardoptie.
  - Eigenwaardes: Componenten worden geselecteerd wanneer ze een bepaalde eigenwaarde hebben. De standaardoptie is een eigenwaarde boven 1. 
  - Handmatig: Het aantal componenten kan handmatig worden ingesteld. De standaardoptie is 1. 

#### Rotatie
- Hier wordt de rotatiemethode gespecificeerd. Rotatie zorgt ervoor dat de structuur van de data makkelijker te interpreteren is.   
  - Orthogonaal: deze methode produceert componenten die niet gecorreleerd zijn. Voor deze methode kunnen verschillende opties worden geselecteerd:
	  - Geen: Er wordt geen rotatiemethode geselecteerd.
      - Varimax: De varimax orthagonale rotatiemethode. Rotatie gebaseerd op het maximaliseren van de variantie van factorladingen. 
      - Quartimax: De quartimax orthagonale rotatiemethode. In deze methode wordt het aantal componenten die nodig zijn om elke variabele te verklaren geminimaliseerd. 
      - BetlerT: De betlerT orthagonale rotatiemethode. 
      - Equamax: De exuamax orthagonale rotatiemethode. Dit is een combinatie van varimax en quartimax. 
      - Varimin: De varimin orthagonale rotatiemethode
  - Oblique: Deze methode produceert componenten waartussen wel een correlatie is toegestaan. Dit is de standaardoptie. Verschillende opties zijn beschikbaar:
	  - Promax: De promax oblique rotatiemethode. Dit is de standaardoptie.
      - Oblimin: De oblimin oblique rotatiemethode.
      - Simplimax. De simplimax oblique rotatiemethode.
      - BentlerQ: De betlerQ oblique rotatiemethode.
      - Biquartimin: De biquartimin oblique rotatiemethode.
      - Cluster: De cluster oblique rotatiemethode.

### Uitvoeropties 
- Markeer: Deze optie zet de waarde vanaf waar de paden schalen in breedte. Paden met absolute gewichten hoger dan deze waarde zullen steeds breder worden terwijl waardes eronder een vaste dunne breedte hebben. Alle paden krijgen een sterkere of zwakkere kleurintensiteit naarmate ze een sterker gewicht hebben. Als de waarde op 0 gezet wordt zullen alle paden een verschillende breedte krijgen.
- Neem tabellen mee:
	- Component correlaties: Als u deze optie selecteert wordt er een tabel met de correlates tussen componenten weergegeven.
	- Pad diagram: Als u deze optie selecteert wordt er een visuele representatie van de richting en sterkte van de relatie tussen variabelen en componenten weergegeven. 
    - Screeplot: Als u deze optie selecteert wordt er een screeplot weergegeven. Deze grafiek geeft informatie over de variantie in de data die wordt verklaard door elke component, door middel van de eigenwaarde. Een scree-grafiek kunt u gebruiken om het aantal componenten te selecteren.
- Ontbrekende waarden:
	- Sluit waarnemingen paarwijs uit: Als een observatie van een variabele mist worden de andere observaties op andere variabelen van hetzelfde geval nog steeds gebruikt voor de analyse. In dit scenario is het niet nodig om een observatie voor alle variabelen te hebben om een geval mee te nemen in de analyse. Dit is de standaardoptie.
	- Sluit waarnemingen op lijstwijze uit: Als een observatie van een variabele mist wordt het hele geval, dus alle verbonden observaties op andere variabelen, uitgesloten van de analyse. In dit scenario moet een geval observaties op elke variabele hebben om meegenomen te worden in de analyse.

### Uitvoer
--- 
#### Principale Componentenanalyse
Chi-kwadraat toets: 
De passing van het model wordt getoetst. Wanneer de toets significant is wordt het model verworpen. Houd in gedachten dat een chi-kwadraat benadering onbetrouwbaar kan zijn voor kleine steekproeven, en bij hele grote steekproeven kan de chi-kwadraattoets het model te snel verwerpen. Voor een verdere discussie over pas indices kan bijvoorbeeld Saris, Satorra, & van der Veld (2009) geraadpleegd worden. 
- Model: Het model dat uit de principale componentenanalyse is gekomen.
- Waarde: De chi-kwadraat toetsstatistiek.
- vg: Vrijheidsgraden.
- P: De p-waarde.

Componentladingen:
- Variabelen: De eerste kolom geeft alle variabelen die zijn meegenomen.
- PC (1, 2, 3, ...): Deze kolom geeft de ladingen van de variabelen op de componenten.
- Uniciteit: Het percentage van de variantie van iedere variabele die wordt verklaard door de component.

Component karakteristieken:
- Eigenwaardes: De eigenwaarde van elk geselecteerd component.
- Proportie var.: De proportie van variatie in de dataset die door iedere component wordt verklaard. 
- Cumulatief: De proportie van variantie in de dataset die wordt verklaard door de componenten tot en met deze component.

Component correlaties:
- De correlatie tussen de principale componenten.

#### Pad Diagram
- PC: De principale componenten worden weergegeven in de cirkels. 
- Variabelen: De variabelen worden weergegeven als rechthoeken.
- Pijlen: Gaan van de variabelen naar de componenten en representeren de lading van een variabele op een component. Rood is een negatieve lading, groen een positieve. Hoe breder de strepen, hoe sterker de lading. Deze markering kan worden aangepast  bij `markeren` in de `uitvoer opties`.

#### Screeplot
De screeplot geeft informatie over hoeveel variantie in de data wordt verklaard door elke component, door middel van de eigenwaarde. De screeplot kan worden gebruikt om over het aantal componenten in het model te beslissen.
- Componenten: De componenten staan op de x-as.
- Eigenwaarde: De eigenwaarden staan op de y-as, en geven aan hoeveel variantie door elke component wordt verklaard. 
- Data: De stippellijn representeert de data.
- Gesimuleerd: De driehoekslijn representeert de gesimuleerde data. Deze lijn is indicatief voor de parallelle analyse. Wanneer de punten van de stippellijn (echte data) boven deze lijn liggen worden deze componenten meegenomen in het model door parallelle analyse. 
- Kaiser criterium: De horizontale lijn bij een eigenwaarde van 1 representeert het Kaiser criterium. Volgens dit criterium moeten componenten met een eigenwaarde boven de 1 worden meegenomen.

### Referenties  
--- 
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational research methods, 7*(2), 191-205.
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An introduction to statistical learning* (Vol. 112, p. 18). New York: springer.
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in exploratory factor analysis. *Best practices in quantitative methods*, 86-99.
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. Structural Equation Modeling, 16(4), 561-582.
- Shlens, J. (2014). A tutorial on principal component analysis. *arXiv preprint arXiv:1404.1100*.

### R Packages 
--- 
- psych 
- qgraph 

