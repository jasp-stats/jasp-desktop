Samenvattende Statistiek Bayesiaanse Binomiaal Toets
===

Deze functie berekent de Bayes factor voor een observatie van een binomiaalverdeling. De bayesiaanse binomiaaltoets wordt beschreven in Jeffreys (1961, p 256). Deze toets vertelt u of de data een bepaalde waarde voor de parameter ondersteunt of juist niet.

### Invoer
---

#### Invoerveld
- Nul model: *p = p0*
- Alt  model: *p ~ Beta(a,b)*
*p0* is de voorgestelde waarde voor de "rate parameter" van de binomiaalverdeling onder de nulhypothese. Geobserveerde data: *s* successen,*f* mislukkingen en het totaal aantal trials *n = s + f*. In Theory on Probability neemt Jeffreys een uniforme priorverdeling voor de rate parameter aan onder de alternatieve hypothese. De Bayes Factor die hier wordt gebruikt is gebaseerd op een meer algemeen geval, namelijk een beta prior. *Let op* beta(1,1) is een uniforme prior.

#### Alt. Hypothese
- *&ne; Test value*: De tweezijdige alternative hypothese dat het populatiegemiddelde niet gelijk aan de toetswaarde is.
- *&gt; Test value*: De eenzijdige alternatieve hypothese dat het populatiegemiddelde hoger dan de toetswaarde is.
- *&lt; Test value*: De eenzijdige alternatieve hypothese dat het populatiegemiddelde lager dan de toetswaarde is.


#### Bayes Factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Natuurlijk logaritme van BF10.

#### Prior
**Beta prior** parameters *a* en *b* staan standaard beide op 1. Dit is een uniforme prior.

#### Grafieken
- *Prior en posterior*: Geeft de dichtheid van de prior (stippellijn) en de posterior (lijn) van de effectgrootte onder de alternatieve hypothese; de grijze cirkels representeren de hoogte van de prior en de posterior bij een effectgrootte delta = 0. De horizontale lijn representeert de breedte van een 95% geloofwaardigheidsinterval van de posterior.
  - Aanvullende info: Geeft de Bayes factor weer die berekend is met de prior gespecificeerd door de gebruiker; geeft een kanswiel weer met de kans op de data onder de nul- en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior weer.

### Uitvoer 
---
#### Bayesiaanse Binomiaal Toets 
- *Bayes factor*: Als een eenzijdige toets is geselecteerd.
  - BF+0: Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF-0: Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
  - BF0+: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF0-: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
- **p**: De p-waarde van de toetsstatistiek. 

### Referenties
---
- Jeffreys, H. (1961). *Theory of Probability*. Oxford, Oxford University Press.
- O’Hagan, A., & Forster, J. (2004). *Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.)*. London: Arnold.
- Haldane, J. B. S. (1932). A note on inverse probability. *Mathematical Proceedings of the Cambridge Philosophical Society, 28*, 55-61.


### R Packages
---
- stats
