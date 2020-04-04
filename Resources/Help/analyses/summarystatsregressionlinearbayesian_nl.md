Samenvattende Statistiek Bayesiaanse Lineaire Regressie
==================

Met Bayesiaanse Lineaire Regressie kan men een lineaire relatie tussen een of meer verklarende variabele(n) (predictoren) en een continue afhankelijke (respons) variabele modelleren. Met deze analyse, gebaseerd op de klassieke (niet-bijgestelde) R^2 statistiek, kan met de Bayes factor de relatie toetsen. De Bayes factor wordt uitgerekend door middel van Gaussiaanse kwadratuur.

### Invoeropties en restricties
- Als er alleen alternatieve model waarden worden gespecificeerd gebruikt de analyse het standaard nulmodel om de Bayes factor te berekenen.
- Model vergelijking: Beide modellen zijn gespecificeerd.
	- Als het aantal covariaten voor het alternatieve model hoger is dan het aantal covariaten voor het nulmodel moet de R^2 waarde voor het nulmodel lager zijn dan die van het alternatieve model. 
- Als alleen waarden voor het nulmodel worden gespecificeerd wordt de analyse niet uitgevoerd.

### Invoer
---
#### Invoerveld
- Steekproefgrootte.
- Nulmodel.
	- *Covariaten*: Het aantal predictoren in het model (zonder het intercept).
	- *Onaangepaste R-kwadraat*: Proportie van de variantie die wordt verklaard door de predictoren.
- Alternatief model 
    - *Covariaten*
    - *R-kwadraat*

#### Bayes Factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Natuurlijk logaritme van BF10.

#### Grafieken.
- Bayes factor robuustheidsgrafiek: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5 (tussen 0 en 2 als de prior van de gebruiker groter is dan 1.5), op zo'n manier dat de priors steeds minder informatief worden.

### Geavanceerde opties
- Prior
  - *Cauchy prior breedte*: Schaal van de dichtheid van de Cauchy prior voor effectgrootte onder de alternatieve hypothese. De standaardoptie is 0.5. 

### Uitvoer
--- 
#### Model vergelijking
- Als het nulmodel niet is gespecificeerd (standaardoptie).
	- **n**: steekproefgrootte.
	- **aantal covariaten**
	- **onbijgestelde R^2**
	- **BF10**: Bayes factor voor het model ten opzichte van het nulmodel (i.e., alleen het intercept).
    - **fout %**: Proportie fout in de berekening van de Bayes factor.
- Als het nulmodel is gespecificeerd wordt de bovenstaande uitvoer voor alternatieve- en nulmodellen gegeven.

### Referenties
---
- Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J.O. (2008). Mixtures of g-priors for Bayesian variable selection. *Journal of the American Statistical Association, 103*, 410-423.
- Rouder, J. N., & Morey, R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Zellner, A., & Siow, A. (1980) Posterior odds ratios for selected regression hypotheses. In J. M. Bernardo, D. V. Lindley, & A. F. M. Smith (Eds), *Bayesian statistics: Proceedings of the first international meeting held in Valencia (Spain)* (pp. 585-603). University of Valencia.
- Perception and Cognition Lab (University of Missouri): Bayes factor calculators. http://pcl.missouri.edu/bayesfactor

### R Packages
---
- BayesFactor
- stats
