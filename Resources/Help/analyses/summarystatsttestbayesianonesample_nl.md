Samenvattende Statistieken Bayesiaanse T-Toets voor Eén Gemiddelde
==================

Met deze functie kan men de Bayes factor berekenen die correspondeert met een t-toets voor één steekproef door middel van de *t* statistiek. De nulhypothese is dat het populatiegemiddelde gelijk is aan een bepaalde constante (i.e., de toetswaarde). Deze Bayesiaanse analyse kan zonder de pure data worden uitgevoerd.

### Invoer
---

#### Invoerveld
- *t*: t-Statistiek
- *Groepsgrootte*

#### Alt. Hypothesis
- *&ne; Toetswaarde*: Tweezijdige hypothese dat het populatiegemiddelde niet gelijk is aan de toetswaarde.
- *&gt; Toetswaarde*: Eenzijdige hypothese dat het populatiegemiddelde groter is dan de toetswaarde.
- *&lt; Toetswaarde*: Eenzijdige hypothese dat het populatiegemiddelde kleiner is dan de toetswaarde.

#### Bayes Factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nul hypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nul hypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Natuurlijk logaritme van BF10 

#### Grafieken
- *Prior en posterior*: Geeft de dichtheid van de prior (stippellijn) en de posterior (lijn) van de effectgrootte onder de alternatieve hypothese; de grijze cirkels representeren de hoogte van de prior en de posterior bij een effectgrootte delta = 0. De horizontale lijn representeert de breedte van een 95% geloofwaardigheidsinterval van de posterior.
  - Aanvullende info: Geeft de Bayes factor weer; geeft een kanswiel weer met de kans kl de data onder de nul- en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer.
- *Bayes factor robuustheidsgrafiek*: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5 (tussen 0 en 2 als de prior van de gebruiker groter is dan 1.5), op zo'n manier dat de priors steeds minder informatief worden.

### Prior
- **Gestandaardiseerde effectgrootte**
  - Standaard 
    - *Cauchy* : Schaal van de Cauchy prior voor de effectgrootte onder de alternatieve hypothese; de standaardoptie is .707.
  - Geinformeerd
	- *Cauchy*: Schaal en locatie
	- *Normaal*: Gemiddelde en standaardafwijking
	- *Studenten t*: Schaal, locatie en vrijheidsgraden (vg) 


[comment]: # (- **Pure effectgrootte (Dienes)**)
[comment]: # (  - *Half-Normaal*: Standaardafwijking)
[comment]: # (  - *Normaal*: Gemiddelde en standaardafwijking)
[comment]: # (  - *Uniform*: Onder- en bovengrens)


### Uitvoer
---
- **Bayes factor**
  - BF+0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF-0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
  - BF0+: De Bayes factor de bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF0-: De Bayes factor de bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
  **fout &** De fout van de Gaussiaanse kwadratuur intergratiemethode die wordt gebruikt op de Bayes factor te berekenen.
- **p**: De p-waarde van de t-statistiek.

### Referenties
---
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review, 16*, 225-237.
- Morey, R. D. & Rouder, J. N. (2011). Bayes Factor Approaches for Testing Interval Null Hypotheses. *Psychological Methods, 16*, 406-419

### R Packages
---
- BayesFactor
- ggplot2
- logspline
- stats
