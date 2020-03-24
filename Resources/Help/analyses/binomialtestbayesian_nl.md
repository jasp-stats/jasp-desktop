Bayesiaanse binomiaaltoets
===

Met de bayesiaanse binomiaaltoets kan worden getoetst of de proportie van een dichotome variabele gelijk is aan een toetswaarde (aangenomen populatie waarde).

### Assumpties
- De variabele is dichotoom.
- Observaties moeten onafhankelijk zijn.

### Invoer
---------

- Toetswaarde: De proportie van de variabele onder de nulhypothese. De standaardwaarde is 0.5. 

#### Hypothesen
- *&ne; Toets waarde*: Tweezijdige alternatieve hypothese dat de proportie niet gelijk is aan de toetswaarde. 
- *&gt; Toets waarde*: Eenzijdige alternatieve hypothese dat de proportie hoger is dan de toetswaarde.
- *&lt; Toets waarde*: Eenzijdige alternatieve hypothese dat de proportie lager is dan de toetswaarde.

#### Bayes factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nul hypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nul hypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Als u deze optie selecteert wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusie</sub>, BF<sub>10, U</sub> weergegeven in de output .

#### Grafieken
- Prior en posterior: Geeft de dichtheid van de prior en posterior voor de proportie onder de alternatieve hypothese weer. 
	- Aanvullende info: Voegt de Bayes factor toe die is berekend met de door de gebruiker geselecteerde prior; voegt een kanswiel toe die de odds van de data onder de nul- vs. de alternatieve hypothese weergeeft; voegt mediaan en het 95% geloofwaardigheidsinterval toe van de dichtheid van de posterior van de effectgrootte.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomt. 


#### Prior
**Beta prior** parameters *a* en *b* staan standaard beide op '1'. Dit correspondeert met een uniforme prior.

### Uitvoer
---

#### Bayesiaanse Binomiaal Toets
- Niveau: de twee waarden van de dichotome variabele
- Aantallen: Het aantal keer dan een bepaald niveau van de variabele voorkomt. 
- Totaal: Het totale aantal observaties.
- Proportie: berekend door aantallen/totaal 
- BF10 (of BF01): Bayes factor. Als een eenzijdige toets word gedaan: 
  - BF+0: Bayes factor die bewijs voor de eenzijdige alternatieve hypothese geeft dat het populatiegemiddelde hoger is dan de toetswaarde, ten opzichte van de nulhypothese.
  - BF-0: Bayes factor die bewijs voor de eenzijdige alternatieve hypothese geeft dat het populatiegemiddelde lager is dan de toetswaarde, ten opzichte van de nulhypothese.
  - BF0+: Bayes factor die bewijs voor de nulhypothese geeft ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde. 
  - BF0-: Bayes factor die bewijs voor de nulhypothese geeft ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde. 

#### Grafieken
- Prior en Posterior: 
  - Geeft de dichtheid van de prior (onderbroken lijn) en de posterior (lijn) voor de populatieproportie onder de alternatieve hypothese weer; de grijze cirkels representeren de hoogte van de prior en posterior bij de toetswaarde. De horizontale lijn representeert de breedte van een 95% geloofwaardigheidsinterval van de posterior.
  - Aanvullende info: Geeft de Bayes factor weer gebaseerd op de gedefinieerde prior; geeft kanswiel weer die de odds van de data onder de nul- vs de alternatieve hypothese weer; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer. 
- Sequentiële analyse: 
  - Geeft de ontwikkeling van de Bayes factor weer als een functie van het aantal observaties (n) op basis van de gedefinieerde prior; geeft een kanswiel weer die de kans op de data onder de nul- vs de alternatieve hypothese weer; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer; geeft de interpretatie van het bewijs in termen van Jeffreys' (1961) bewijscategorieën.


### Referenties
---
- Jeffreys, H. (1961). *Theory of Probability*. Oxford, Oxford University Press.
- O’Hagan, A., & Forster, J. (2004). *Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.)*. London: Arnold.
- Haldane, J. B. S. (1932). A note on inverse probability. *Mathematical Proceedings of the Cambridge Philosophical Society, 28*, 55-61.

### R Packages
---
- ggplot2
- plotrix
- stats
