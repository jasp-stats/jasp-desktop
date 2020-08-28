Bayesiaanse lineaire regressie
===

Met de bayesiaanse Lineaire regressie kan men een lineaire relatie tussen een of meer verklarende variabele(n) en een continue afhankelijke variabele modelleren. Deze analyse gebruikt het BAS package, die Bayesiaans model middelen en model selectie implementeert met prior verdelingen voor lineaire en gegeneraliseerde lineaire modellen. 

### Assumpties
- Continue response variabele.
- Lineariteit en additiviteit: De respons variabele is lineair gerelateerd aan alle predictoren en de effecten van de predictoren zijn additief.
- Onafhankelijkheid van de residuen: De residuen zijn niet gecorreleerd met elkaar.
- Homoskedastischiteit: De fout variantie van elke predictor is constant over alle waarden van de predictor. 
- Normaliteit van residuen: De residuen zijn normaal verdeeld met een gemiddelde van 0.

### Invoer
---

#### Bayes Factor
- BF<sub>10</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nul hypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert geeft de Bayes factor bewijs voor de nul hypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Natuurlijk logaritme van BF10.

#### Volgorde
Vergelijkt elk model met het geselecteerde model.
  - Vergelijk met het beste model.
  - Vergelijk met het nulmodel. 

#### Uitvoer
  - Samenvatting van de posterior: Uitvoer tabel met de samenvattingen van de marginale posterior coëfficiënten. Er zijn opties beschikbaar om tegen het beste model, het mediaan model, meest complexe model en model gemiddelde model te vergelijken.
  - Beschrijvende statistieken: Uitvoertabel met beschrijvende statistieken over de geselecteerde variabelen (gemiddelde, standaardafwijking en steekproefgrootte).


#### Maximale aantal modellen die worden weergegeven
- Aantal modellen dat wordt weergegeven. De standaardoptie is alle modellen. Er is een optie om de beste *n* modellen weer te geven.

### Model
- Componenten en model termen:
	- Componenten: Alle onafhankelijke variabelen die in het model worden meegenomen.
	- Model termen: De onafhankelijke variabelen in het model. De standaardoptie is om de hoofdeffecten van de gespecificeerde onafhankelijke variabelen in het model te betrekken. Vink meerdere variabelen aan om interacties mee te nemen (bijv., door de ctrl/cmd knop op uw toetsenbord ingedrukt te houden terwijl u klikt, en sleep de variabelen naar het `Model Termen` veld.
	
### Grafieken
- Coëfficiënten
  - Inclusiekansen: Geeft een histogram van de posterior inclusiekansen. De stippellijn geeft de prior inclusiekansen weer.
  - Gemiddelde posterior verdelingen: Geeft t-benaderingen van de posterior verdelingen gemiddeld over alle modellen.
- Residuen 
  - Residuen vs gefit: Geeft een grafiek van de model gemiddelde predicties (BMA) tegen de residuen.
- Modellen 
  - Posterior log odds: Geeft een heatmap van de log posterior odds tegen de model rang.
  - Log(P(data|M)) vs model grootte: Geeft de relatie tussen de passing van een model en zijn complexiteit.
  - Model kansen: Geeft de cumulatieve verdeling functie van elk model.


### Geavanceerde opties
- Prior: Prior verdelingen voor regressiecoëfficiënten. Er zijn verschillende opties beschikbaar:
  - AIC: Vergelijk modellen met het Akaike Informatie Criterium.
  - BIC: Vergelijk modellen met het Bayesiaanse Informatie Criterium.
  - EB-globaal: Globale empirische Bayesiaanse schattingen van g in de Zellner-Siow g-prior en model kansen. Gebruikt een EM algoritme om een gemeenschappelijke of globale schatting van g te vinden, gemiddeld over alle modellen. Als het niet mogelijk is om alle modellen op te sommen worden alleen de modellen die onder de EB-lokaal zijn gesampled gebruikt.
  - EB-lokaal: Gebruikt MLE van g van de marginale likelihood in elk model.
  - g-prior: Zellner's g-prior.
  - Hyper-g: Een mix van g-priors waar de prior voor g/(1+g) een Beta(1,alpha/2) is, zoals in Liang et al (2008). Dit gebruikt een Cephes library voor de evaluatie van de marginale likelihoods en kan numeriek instabiel zijn voor een grote n of een R2 die dichtbij 1 zijn. De standaardoptie voor alpha is 3.
  - Hyper-g-Laplace: Zelfde als *hyper-g* maar dan met een Laplace schatting om te integreren over de prior voor g. 
  - Hyper-g-n: Een mix van g-priors waar u = g/n en u  Beta(1, alpha/2) om consistentie de krijgen wanneer het nulmodel waar is. 
  - JZS: Jeffreys-Zellner-Siow prior welke de Jeffreys prior voor sigma en de Zellner-Siow Cauchy prior voor de coëfficiënten gebruikt. De optionele parameter kan gebruikt worden om de gekwadrateerde schaal van de prior te controleren (standaard r-schaal - 0.354).
- Model prior: Prior verdelingen voor de modellen.
  - Uniform
  - Wilson: Standaard lambda = 1. Gelijk aan een Beta binomiaal met a = 1 en b = lambda  *p*, waar p staat voor het aantal predictoren.
  - Castillo: Standaard u = 1. Gelijk aan een beta binomiaal met a = 1 en b = p^u, waar p staat voor het aantal predictoren. 
  - Beta binomiaal: Standaard Beta(a=1, b-1).
  - Bernoulli: Standaard p = 0.5.
- Steekproef methode: Geeft aan welke steekproef methode wordt gebruikt. Het wordt aangeraden om *BAS* te gebruiken wanneer de model ruimte kan worden opgeteld.
  - BAS: Gebruikt Bayesiaanse adaptieve steekproeven (zonder vervanging). Deze kan worden geüpdatet gebaseerd op schattingen van de marginale inclusie. *N. modellen* geeft het aantal modellen aan om steekproeven van te nemen zonder vervanging. Als u de waarde op 0 zet betekent dit dat de analyse zal proberen alle modellen op te tellen.
  - MCMC: Steekproeven met vervanging door middel van een MCMC algoritme dat een birth/death random walk combineert met een random swap move om variabiliteit in het model uit te wisselen. *n. samples* geeft het aantal MCMC steekproeven aan. Als u de waarde op 0 zet is het aantal interacties 10 keer het aantal modellen. Het nemen van steekproeven stopt wanneer min(aantal modellen, MCMC iteraties) wordt bereikt. 
- Reproduceerbaarheid:
  - Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

### Uitvoer
---

#### Model Vergelijking
  - Modellen: Predictoren in het model.
  - P(M): Prior model kansen.
  - P(M|data): Posterior kansen van de modellen.
  - BFM: De update factor waarmee de prior model odds veranderen in de posterior model odds (bv. wanneer BFM = 2, dan is de posterior model odds twee keer zo groot als de prior model odds).
  - BF10 (of BF01): Bayes factor die een model vergelijkt met ofwel het nulmodel, ofwel het model met de hoogste posterior model kans.
  - R2: Verklaarde variantie.

#### Samenvatting van de posterior
Posterior samenvattingen van de coëfficiënten.
  - Coëfficiënt: Naam van de predictoren.
  - Gemiddelde: Gemiddelde van de model gemiddelde posterior.
  - P(incl | data): Posterior inclusie kans. 

### Referenties
---
- Castillo, I., Schmidt-Hieber, J., & Van der Vaart, A. (2015). Bayesian linear regression with sparse priors. *The Annals of Statistics, 43*(5), 1986-2018.
- Clyde, M. A. (2018). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software].
- Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. *Journal of Computational and Graphical Statistics, 20*, 80-101.
- Consonni, G., Fouskakis, D., Liseo, B., & Ntzoufras, I. (2018). Prior Distributions for Objective Bayesian Analysis. *Bayesian Analysis, 13*, 627-679.
- Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of *g* Priors for Bayesian Variable Selection. *Journal of the American Statistical Association, 103*, 410-423.
- Wilson, M. A., Iversen, E. S., Clyde, M. A., Schmidler, S. C., & Schildkraut, J. M. (2010). Bayesian model search and multilevel inference for SNP association studies. *The Annals of Applied Statistics, 4*(3), 1342.

### R Packages
---
- BAS
- ggplot2
- scales
- stats
