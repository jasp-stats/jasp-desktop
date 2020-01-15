Bayesiaanse Correlatie Paren
===

De Bayesiaanse correlatie paren maakt het mogelijk de populatiecorrelatie te schatten en de nulhypothese dat de populatiecorrelatie tussen paren van variabelen gelijk is aan 0, te toetsen. Specifieke paren van variabelen worden geanalyseerd.

### Assumpties (Pearson's rho)
- Continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De paren van variabelen hebben een bivariate normale verdeling in de populatie.
- De relatie tussen de paren van variabelen is lineair.

### Assumpties (Kendall's tau)
- Ordinale of continue variabelen.
- De data zijn een aselecte steekproef van de populatie.
- De relatie tussen de paren van variabelen is monotoon.

### Invoer
---
#### Correlatiecoëfficient
- Pearson's rho: Pearson's productmoment correlatiecoëfficient. 
- Kendall's tau-b: Kendall's tau-b rank order correlatiecoëfficient om de monotone associatie tussen twee variabelen te kwantificeren.

#### Alt. Hypotheses
- Correlatie: Tweezijdige alternatieve hypothese dat de populatiecorrelatie niet gelijk is aan 0.
- Positieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0.
- Negatieve correlatie: Eénzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0.

#### Bayes Factor  
- BF<sub>10</sub>: Als je deze optie selecteert, geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als je deze optie selecteert, geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Als je deze optie selecteert, wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> weergegeven in de uitvoer.

#### Prior
- Gespreide beta prior breedte: Breedte van de geschaalde beta verdeling op de correlatie onder de alternatieve hypothese; de standaardwaarde is 1. Hoe lager deze waarde, hoe meer de prior verdeling is geconcentreerd rond 0. De waarde moet tussen de 0 en de 2 liggen. 

#### Aanvullende Opties
- Geloofwaardigheidsintervallen: Geeft geloofwaardigheidsintervallen weer voor de correlatiecoëfficient.

#### Grafieken
- Spreidingsdiagram: Geeft spreidingsdiagrammen weer voor ieder gespecificeerd paar variabelen. 
- Prior en posterior: Geeft de prior en posterior verdeling van de correlatie onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes factor toe, berekend met de door de gebruiker gedefinieerde prior; voegt een kanswiel toe die de kans van de data onder de nul vs. alternatieve hypothese weergeeft; voegt de mediaan en de 95% geloofwaardigheidsinterval van de posterior verdeling van de effectgrootte toe. 
- Bayes factor robuustheidscheck: Geeft de Bayes factor als een functie van de breedte van de gespreide beta prior op de correlatie. De breedte van de beta prior varieert tussen de 0 en 2. 
  - Aanvullende informatie: Voegt de Bayes factor toe, met de door de gebruiker gedefinieerde prior en de maximaal haalbare Bayes factor. 
- Sequentiele analyse: Geeft de ontwikkeling van de Bayes factor naar hoe de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior. 

#### Missende Waardes
  - Sluit paarsgewijs gevallen uit: In het geval van meerdere correlatietoetsen in een enkele analyse zal elke toets worden uitgevoerd met alle gevallen die valide data bevatten voor de variabele van de toets. Steekproefgroottes kunnen dus verschillen over de correlatietoetsen. 
  - Sluit lijstgewijs gevallen uit: In het geval van meerdere correlatietoetsen in een enkele analyse zal elke toets worden uitgevoerd met alleen gevallen die valide data bevatten voor alle variabelen. De steekproefgrootte is dus constant over alle correlatietoetsen. 

### Uitvoer 
---
#### Bayesiaanse Pearson Correlaties
- Pearson r: Pearson's product-moment correlatiecoëfficient.
- Kendall tau:  Kendall's tau b rank correlatiecoëfficient.
- BF10 (of BF01): Bayes factor. Als een eenzijdige toets wordt opgevraagd:
  - BF+0: Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0. 
  - BF-0: Bayes factor die bewijs geeft voor de eenzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0. 
  - BF0+: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige alternatieve hypothese dat de populatiecorrelatie hoger is dan 0. 
  - BF0-: Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige alternatieve hypothese dat de populatiecorrelatie lager is dan 0. 
- Geloofwaardigheidsinterval: Centrale geloofwaardigheidsinterval voor de correlatiecoefficent. 

#### Grafieken
Spreidingsdiagram: 
- Geeft de spreidingsdiagram van de correlatieparen. 

Prior en posterior:
- Geeft de prior (onderbroken lijn) en posterior (lijn) verdeling van de correlatie onder de alternatieve hypothese; de grijze cirkels geven de hoogte van de prior en de posterior verdeling bij een correlatie van 0. De horizontale lijn geeft de breedte van de 95% geloofwaardigheidsinterval van de posterior verdeling. 
  - Aanvullende informatie: Geeft de Bayes factor weer, berekend met de door de gebruiker gedefinieerde prior; geeft een kanswiel weer die de kans toont op de data onder de nul vs. de alternatieve hypothese; geeft de mediaan en de 95% geloofwaardigheidsinterval weer voor de posterior verdeling. 

Bayes factor robuustheidscheck:
- Geeft de Bayes factor weer als een functie van de breedte van de beta prior op de correlatie. 
  - Aanvullende informatie: De rode cirkel toont de maximaal behaalbare Bayes factor; de grijze cirkel toont de Bayes factor berekend met de door de gebruiker gedefinieerde prior. 

Sequentiële analyse:
- Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal observaties (n), met de door de gebruiker gedefinieerde prior; geeft de Bayes factor weer, berekend met de door de gebruiker gedefinieerde prior; geeft een kanswiel weer die de kans van de data onder de nul vs. alternatieve hypothese toont; toont de besluitvaardigheid van het bewijs zoals beschreven door Jeffreys' (1961) bewijscategorieën. 

### Referenties
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. *The American Statistician*,  72, 303-308.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.

### R Packages
---
- hypergeo
- ggplot2
- stats
