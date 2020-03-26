Netwerkanalyse
===

Met netwerkanalyse kan de gebruiker de netwerkstructuur van variabelen analyseren. Laad de data in het veld `Variabelen`. Om netwerken van verschillende groepen te vergelijken, kunt u een groepen toevoegen in `Opdelen`. In netwerken worden 'knopen' geobserveerde variabelen genoemd en worden geschatte relaties 'paden' genoemd.


### Assumpties

Assumpties kunnen variëren per netwerkanalyse methode. In het algemeen wordt aangenomen dat relaties tussen variabelen *lineair* zijn. 

Aanvullende assumpties voor sommige netwerken:
- Correlatie $ partiële correlatie netwerken: Als u de paden wilt interpreteren met enkel significante (partiële) correlaties, moet uw data normaal zijn verdeeld. Deze assumptie maakt alleen uit voor de significantie toets.
- EBICglasso: Uw data is normaal verdeeld.
- Gemixte grafische modellen: uw variabelen zijn normaal verdeeld, categorisch, of komen van een Poissonverdeling.


### Invoer
---

#### Invoerveld 
- Variabelen: In dit veld selecteert u alle afhankelijke variabelen.
- Opdelen: Deel de data op aan de hand van een categorische variabele, zoals een experimentele conditie. 

#### Schatter 
Deze analyse laat u niet één, maar meerdere type netwerken schatten. De ondersteunde netwerken zijn:
- Correlatie netwerken. 
- Partiële correlatie netwerken.
- EBICglasso Netwerken(Foygel & Drton, 2010; Friedman, Hastie, & Tibshirani, 2008; Friedman, Hastie, & Tibshirani, 2014).
- Veel-dimensionale ongerichte grafiek schatting (HUGE; Zhao et a., 2015).
- Ising netwerken (door middel van IsingFit, van Borkulo et al., 2014; or IsingSampler, Epskamp, 2014; Epskamp, Borsboom & Maris, in drukking).
- Adaptieve lasso (Schaeafer & Boulesteix, 2009).
- Gemixte Grafische Modellen (Haslbeck & Waldorp, 2015).

#### Grafieken
- Netwerkgrafiek: Een grafiek van het geschatte netwerk.
- Centraliteitsgrafiek: Een grafiek van de centraliteitsmaten van het geschatte netwerk.

#### Tabellen
- Centraliteitstabel: Een tabel met de waarden van de centraliteitsmaten. 
- Gewichtenmatrix: De geschatte parameters in de vorm van een matrix.
- Lay-out matrix: De lay-out voor elke knoop in de netwerkgrafiek. De optie "geef namen van variabelen weer" kan ook worden aangeklikt om de lay-out te voorzien van namen.



### Analyse opties 
Voor elke netwerkmethode kunnen er opties worden bijgesteld om het resultaat te beïnvloeden. Alleen opties voor een specifieke schattingsmethode zijn tegelijk beschikbaar.

#### Correlatiemethode
- `Auto`: Detecteert automatisch het type variabele en gebruikt de meest geschikte type correlatie. Dit detecteert continue, binaire en ordinale variabelen en gebruikt Pearson, tetrachorische of polychorische correlaties.
- `Cor`: Pearson correlatie. 
- `Cov`: Covarianties. 
- `Nonparanormaal`: Deze optie zal eerst een nonparanormale transformatie op de data toepassen om deze nomaal verdeeld te maken, en gebruikt daarna Pearson correlaties.


#### Regel
Welke regel moet er worden gebruikt om te bepalen of er een pad is tussen twee knopen?

- `EN`: Beide schatters, de pad van A naar B en van B naar A moeten niet 0 zijn. 
- `OF`: Een van de schatters moet niet 0 zijn.

#### Opdelen 
Wanneer u een Ising model schat (met de IsingFit of de IsingSampler), worden niet-binaire variabelen gebinariseerd door middel van: 
- `Mediaan`: De mediaan van de observaties. 
- `Gemiddelde`: Het gemiddelde van de observaties.

#### Type Variabele 
Wanneer u het gemixte grafische model gebruikt, kunt u hier een variabele invoeren om het type variabele te specificeren. Toegestane invoer bestaat uit:
- `g`: Voor normaal verdeelde / Gaussiaanse variabelen.
- `c`: Voor categorische variabelen.
- `p`: Voor Poisson variabelen.

De data moet van het formaat `variabeleNaam` = `groep` zijn. Als een variabele bijvoorbeeld "aantal voertuigen" heet en het een Poissonverdeling volgt, noemt u hem `aantal voertuigen = p`.

#### Ising Schatter 
Er bestaan vele methoden om Ising modellen te schatten. De ondersteunde methoden zijn:
- `Pseudo-likelihood`: Schat het Ising model door de pseudo-likelihood te maximaliseren (TODO: Besag, 1975).
- `Univariate regressies`: Berekenen univariate logistische regressies van elke knoop naar elke andere knoop. Dit geeft twee schattingen voor elke pad welke dan worden gecombineerd door de methode gespecificeerd onder *regel*.
- `Bivariate regressies`: Berekenen bivariate logistische regressies van elke knoop naar elke andere knoop. Dit geeft twee schattingen voor elke pad welke dan worden gecombineerd door de methode gespecificeerd onder *regel*.
- `Loglineair`: Schat het Ising model alsof het een loglineair model is dat hoogstens paarsgewijze interacties heeft.

#### Ontbrekende Waarden 
Hoe moet met ontbrekende waarden worden omgegaan? Sommige analyses staan paarsgewijze uitsluiting toe, maar niet allemaal.

#### Afstel Parameters
Deze parameter is de &gamma; hyperparameter van de EBIC schattingsprocedure. Het bepaalt de schaarsheid van het geschatte netwerk. Als u het op 0 zet, wordt de gewone BIC gebruikt. 

#### Kruisvalidatie
Hoeveel kruisvalidatie steekproeven moeten er genomen worden. Deze methode wordt alleen gebruikt door de adaptieve lasso en de gemixte grafische modellen (als het criterium op kruisvalidatie wordt gezet). 

#### Drempelwaarden 
Drempelwaarden die worden gebruikt in correlatie of partiële correlatie netwerken. Dit kan een nummer of methode zijn. Als het een nummer is, worden paden met een absolute waarde onder het nummer niet weergegeven. Het kan ook een methode zijn om te controleren voor family-wise foutmarges. Beschikbare methodes zijn:
- `Significant`: Laat de paden zijn die significant zijn bij het 0.05 niveau.
- `Bonferroni`: Net als `Significant`, maar dan met Bonferroni correctie.
- `Holm`: Net als `Significant`, maar dan met Holm correctie.
- `Hochberg`: Net als `Significant`, maar dan met Hochberg correctie. Neemt aan dat hypothesetoetsen onafhankelijk of niet-negatief geassocieerd zijn. 
- `Hommel`: Net als `Significant`, maar dan met Hommel correctie. Neemt aan dat hypothesetoetsen onafhankelijk of niet-negatief geassocieerd zijn. 
- `BH`: Net als `Significant`, maar controleert voor de valse ontdekking rate. 

In de meeste scenario's is de `Bonferroni` methode nogal beperkend en heeft de `Holm` methode de voorkeur.

#### Criterium
Welk criterium moet gebruikt worden om het netwerk te fitten? Beschikbare opties zijn:
- `EBIC`: Uitgebreid Bayesiaans Informatie Criterium.
- `RIC`:  Rotatie Informatie Criterium.
- `STARS`: Stabiliteits Benadering tot Regularisering Selectie.
- `CV`: Kruis-Validatie.

#### Steekproef grootte 

#### Netwerk 
Als u `gewogen` uitvinkt, bestaat het geschatte netwerk alleen uit positieve (1), negatieve (-1) en afwezige (0) paden. Als u `Gesigneerd` uitvinkt, bestaat het netwerk alleen uit positieve paden. Let op: de absolute waarde wordt genomen van de negatieve paden om ze positief te maken. Als u zowel `Gewogen` als `Gesigneerd` uitvinkt, zegt het netwerk of er een pad (1) is of niet (0).


#### Centraliteitsmaten 
Centraliteitsmaten van een netwerk kunnen moeilijk zijn om te vergelijken. Om dit de faciliteren kunt u `Genormaliseerd` selecteren om te zorgen dat alle centraliteitsmaten een gemiddelde van 0 en variantie van 1 hebben. U kunt ook `relatief` selecteren om elke centraliteitsmaat te delen door zijn maximaal geobserveerde waarde.


### Bootstrap opties
Om de stabiliteit van een geschat netwerk te onderzoeken, kunt u `Bootstrap Network` aanvinken onder `Schatter`. Zo worden de paden van het geschatte netwerk en hun centraliteit automatisch gebootstrapt. Aanvullende opties kunnen worden gespecificeerd, zoals het aantal bootstraps en het type bootstrap.


### Grafische opties 
Alle onderstaande opties passen de uitvoer van de netwerkgrafiek aan. Andere figuren en tabellen worden niet veranderd. Om netwerkgrafieken mooier te maken, bestaan vele opties. 

#### Lay-out
De lay-out van een netwerk bepaalt waar de knopen worden geplaatst. De standaardoptie is `spring`, wat betekent dat de lay-out wordt gegenereerd door het krachtgedreven Fruchterman-Reingold algoritme (TODO: ref). Dit algoritme kan worden aangepast met de `repulsie` parameter; een grotere repulsie verhoogt de afstand tussen nabije knopen. U kunt ook alle knopen weergeven in een cirkel door de `cirkel` lay-out te selecteren. Een derde optie is `data`. Hier kunnen coördinaten voor de netwerken worden gegeven. De data moet van het `variabeleNaam` = `groep` formaat zijn net als `type variabele` en `kleur knopen per` . Als een variabele bijvoorbeeld "A1" heet en moet worden geplot op x-coördinaat 1, wordt dit "A1 = 1".

#### Paden
- `Padgrootte`: Een vermenigvuldiger voor de grootte van paden (bijv., 2 is twee keer zo groot).
- `minimum`: De absolute minimale sterkte voor een pad om te worden weergegeven.
- `maximum`: De absolute maximale sterkte voor een pad om te worden weergegeven.
- `cut`: 
- `details`: Als deze optie wordt aangevinkt, worden `minimum`, `maximum`, en `cut` weergegeven op de netwerkgrafiek (als ze zijn aangepast).
- `kleuren pallet`: Welke kleuren moeten gebruikt worden voor positieve en negatieve paden?

#### Legenda
Er zijn drie opties:
- Laat geen legenda zien.
- Laat de legenda in alle netwerken zien.
- Laat de legenda in een specifieke grafiek zien.

- `Ratio legenda tot plot`: Specificeert de breedte van de legenda relatief aan het figuur.

#### Labels
- `Labelgrootte`: Een vermenigvuldiger voor de grootte van labels (bijv., 2 is twee keer zo groot).
- `Schaallabelgrootte`:
- `Kort labels af tot ... letters`: Als de labels te lang zijn, worden ze automatisch afgekort. 

#### Knopen 
- `Knoop grootte`: Een vermenigvuldiger voor de grootte van knopen (bijv., 2 is twee keer zo groot).
- `Kleur knopen op`: Een categorische variabele dit aangeeft tot welke groep iedere variabele behoort. De data moet van het `variabeleNaam` = `groep` formaat zijn net als `type variabele` en `kleur knopen per`. Als een variabele bijvoorbeeld "A1" heet en moet worden geplot op x-coördinaat 1, wordt dit "A1 = 1".
- `kleuren pallet`: welke kleuren moeten worden gebruikt voor het kleuren?

#### Geef namen van variabelen weer 
Een alternatief voor het verkorten van knoop labels is om ze te laten zien in de legenda. Dit kan door `In legenda` aan te klikken, of kan ongedaan worden door op `In knopen` te klikken.

#### Geef type variabele weer
Wanneer u een gemixt grafisch model schat, kan de aangenomen verdeling van een variabele worden weergegeven op verschillende manieren:
- `Laat niet zien`: Laat de aangenomen verdeling niet zien.
- `Met de knoop kleuren`: Pas de kleur van de knopen aan op basis van hun aangenomen verdeling. Wordt gegenereerd als `kleur knopen op` aanstaat.
- `Met de knoop vorm`: Verander de vorm van de knopen op basis van hun aangenomen verdeling. Gaussiaanse knopen zijn cirkels, categorische knopen zijn vierkanten en Poisson knopen zijn driehoeken.

### Uitvoer
-------
TBA.

### Referenties 
-------
Deze referentielijst bevat referenties voor alle netwerken. Een subset van referenties die relevant zijn voor een specifieke schatter, is te vinden door met de rechtermuisknop te klikken op een tabel en "kopieer citaties" te selecteren.

- van Borkulo, C. D., Borsboom, D., Epskamp, S., Blanken, T. F., Boschloo, L., Schoevers, R. A., & Waldorp, L. J. (2014). A new method for constructing networks from binary data. *Scientific reports, 4*(5918), 1-10.
- Epskamp, S., Borsboom, D., & Fried, E. I. (2016). Estimating psychological networks and their accuracy: a tutorial paper. arXiv preprint, arXiv:1604.08462.
- Epskamp, S., Cramer, A. O., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. *Journal of Statistical Software, 48*(4), 1-18. Chicago
- Epskamp, S., Maris, G., Waldorp, L., & Borsboom, D. (in press). Network psychometrics. In P. Irwing, D. Hughes, & T. Booth (Eds.), *Handbook of psychometrics.* New York, NY, USA: Wiley.
- Epskamp, S. (2014). IsingSampler: Sampling methods and distribution functions for the Ising model. Retrieved from github.com/SachaEpskamp/IsingSampler
- Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. *In Advances in neural information processing systems* (pp. 604-612).
- Friedman, J., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. *Biostatistics, 9*(3), 432-441.
- Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso
- Fruchterman, T. M., & Reingold, E. M. (1991). Graph drawing by force-directed placement. Software: Practice and experience, 21(11), 1129-1164.
- Haslbeck, J., & Waldorp, L. J. (2015). mgm: Structure Estimation for time-varying mixed graphical models in high-dimensional data. arXiv preprint arXiv:1510.06871.
- Kraeamer, N., Schaeafer, J., & Boulesteix, A.-L. (2009). Regularized estimation of large-scale gene association networks using graphical gaussian models. *BMC Bioinformatics, 10*(1), 1-24.
- Zhao, T., Li, X., Liu, H., Roeder, K., Lafferty, J., & Wasserman, L. (2015). huge: High-dimensional undirected graph estimation. Retrieved from https://CRAN.R-project.org/package=huge


### R Packages 
---
- bootnet
- glasso
- huge
- mgm
