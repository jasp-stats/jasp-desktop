Logistische Regressie
===
 
Met logistische regressie kan men een lineaire relatie tussen een of meer verklarende variabele(n) (predictoren) en een categorische afhankelijke (respons) variabele.
 
### Assumpties
- De afhankelijke variabele is categorisch.
- Lineariteit en additiviteit: De responsvariabele is lineair gerelateerd aan alle predictoren en de effecten van de predictoren zijn additief.
- Onafhankelijkheid van de residuen: De residuen zijn niet gecorreleerd met elkaar.
- Homoskedasticiteit: De fout variantie van elke predictor is constant over alle waarden van de predictor. 
- Normaliteit van residuen: De residuen zijn normaal verdeeld met een gemiddelde van 0.
- De covariaat en het experimentele effect zijn onafhankelijk. 
 
### Invoer
---
 
#### Invoerveld: 
- Afhankelijke variabele: De variabele waarin u geïnteresseerd bent. Dit wordt ook wel de uitkomstvariabele genoemd. Als er meerdere afhankelijke variabelen zijn specificeert u de volgorde waarin de predictoren in het model worden gevoegd. i.e., hiërarchische regressie analyse). Een blok van een of meer predictor(en) representeert een stap in de hiërarchie. 
    - Toevoegen: Alle predictoren worden tegelijk toegevoegd aan het model (forced entry).
	- Achterwaarts: Alle predictoren worden tegelijk toegevoegd, en daarna sequentieel verwijderd gebaseerd op het criterium in "criterium stapsgewijze methode".
	- Voorwaarts: Predictoren worden sequentieel toegevoegd op basis van het criterium gespecificeerd in "criterium stapsgewijze methode". 
	- Stapsgewijs: Predictoren worden sequentieel toegevoegd op basis van het criterium gespecificeerd in "criterium stapsgewijze methode"; na elke stap wordt de minst nuttige predictor verwijderd. 
- Covariaten: Predictor variabele(n).
- Factoren: De variabelen die worden gemanipuleerd/die de verschillende groepen definiëren. Deze worden ook wel de onafhankelijke variabelen genoemd. 

 
### Model
- Componenten en model termen:
	- Componenten: Alle onafhankelijke variabelen die in het model worden meegenomen.
	- Model termen: De onafhankelijke variabelen en covariaten in het model. De standaardoptie is om de hoofdeffecten en interactie-effecten van de geselecteerde onafhankelijke variabelen mee te nemen in het model, net zoals de covariaten.
	- Voeg toe aan nul model: De onafhankelijke variabelen in het model kunnen ook aan het nulmodel worden toegevoegd. 
- Neem intercept mee:
  - Neem het intercept mee in het model.

 
### Statistieken
- Beschrijvende statistieken: 
  - Factor: De niveaus van de afhankelijke variabele(n) en het aantal observaties per niveau. 
 
- Regressiecoëfficiënten:
  - Schattingen: Ongestandaardiseerde en gestandaardiseerde schattingen van de coëfficiënten, standaardafwijkingen, t-waarden en de corresponderende p-waarden. 
  - Van `...` bootstraps: Als u deze optie selecteert wordt ge-bootstrapte schatting toegepast. De standaardoptie voor het aantal replicaties is 1000. Dit kan naar behoeven worden aangepast.
  - Gestandaardiseerde coëfficiënten: Gestandaardiseerde schattingen zijn schattingen waar de predictoren zijn gestandaardiseerd (X-standaardisatie). 
  - Odds ratio: De odds ratio is een indicatie van de verandering in odds als gevolg van een verandering binnen een predictor (Field).
  - Betrouwbaarheid intervals: De standaardwaarde is 95%. 
	- Odds ratio schaal: Proportionele verandering in odds door het delen van de odds na een verandering in de predictor door de odds voor die verandering (Field).
  - Robuuste standaardfouten: Deze optie controleert voor fouten die niet onafhankelijk en gelijk zijn verdeeld. Het gebruik van robuuste standaardfouten zal de schattingen van coëfficiënten niet aantasten. Als deze optie niet wordt geselecteerd wordt de normale standaardfout berekend.
  - Vovk-Selke maximum p-ratio: De grens 1/(-e p log(p)) wordt afgeleid van de vorm van de verdeling van de p-waarden. Onder de nulhypothese (H<sub>0</sub>) is het uniform (0,1) en onder de alternatieve hypothese (H<sub>1</sub>) neemt hij af in p, bijv. een beta (α, 1) verdeling waar 0 < α < 1. De Vovk-Selke MPR wordt verkregen door het vorm van α onder de alternative hypothese te kiezen zodat de p-waarde maximaal diagnostisch is. De waarde is dan de ratio van de dichtheid op punt p onder H<sub>0</sub> en H<sub>1</sub>. Als de tweezijdige p-waarde bijvoorbeeld .05 is is de Vovk-Sellke MPR 2.46. Dit geeft aan dat deze p-waarde maximaal 2.46 zo waarschijnlijk is onder H1 dan onder H<sub>0</sub>. Meer informatie via href="https://jasp-stats.org/2017/06/12/mysterious-vs-mpr/">blogpost</a>.

- Residuen: 
  - Diagnostieken per observatie: Gevalsgewijze en samengevatte diagnostieken voor de residuen.
	- Gestandaardiseerd residu > 3: Uitschieters buiten x standaardafwijkingen: Geef diagnostieken weer voor waarnemingen waar de absolute waarde voor het gestandaardiseerde residu groter is dan x; de standaardoptie is x=3.
    - Cook's afstand > 1: Geef diagnostieken weer voor waarnemingen waar de waarde van Cook's afstand groter is dan x; de standaardoptie is x=1.
    - Alle waarnemingen: Geef diagnostieken voor alle waarnemingen weer.
 
- Prestatie statistieken:
- De confusion matrix geeft aan hoe goed het model de uitkomsten voorspelt. In de diagonaal staan de waarnemingen die het model correct identificeerde. Daarbuiten de waarnemingen waar het model een verkeerde uitkomst voorspelde.
 - Proporties: De tabel geeft de proporties voor de echte versus de voorspelde uitkomsten.
 
- Prestatiestatistieken:
  - AUC: Gebied onder de kromme. 
  - Sensitiviteit: Sensitiviteit beschrijft de proportie ware positieven ("true positives").
  - Specificiteit: Specificiteit beschrijft het aantal ware negatieven ("true negatives"). 
  - Precisie: Precisie beschrijft de proportie van ware positieve ten opzichte van alle positieven. Wordt ook wel de "positive prediction value" genoemd.
  - F-maat: Dit is gebaseerd op de hoeveelheid systematische variantie gedeeld door de hoeveelheid onsystematische variantie (i.e., de mean squares van het model / de mean squares van de residuen). 
  - Brier score: Nog een maat voor de precisie van de predicties. 
  - H-maat: Een andere maat voor de precisie van predicties. De standaard implementatie in de package hmeasure op CRAN. 
 
### Grafieken
- Inferentiële grafieken:
  - Geef grafieken van de conditionele schattingen: De grafieken zijn conditioneel in de zin dat u de kans op de categorische variabele geeft voor alle niveaus van de predictor variabele gegeven het referentieniveau van alle andere factoren.
	- Laat data punten zien.

- Residu grafieken: Als de assumpties van het lineaire regressiemodel houdbaar zijn moeten de residuen aselect rond een horizontale lijn liggen. Elk systematisch patroon of clustering van residuen wijst op een schending.
  - Voorspeld - residu grafiek: Spreidingsdiagram van de waarden van de residuen tegen de voorspelde waarden.
  - Predictor - residu grafiek: Spreidingsdiagram voor elke onafhankelijke variabele en covariaat van de residuen en de niveaus van de variabele waarin u geïnteresseerd bent.
  - Gekwadrateerde Pearson residuen: Met de Gekwadrateerde Pearson residuen grafiek kunt u voor overdispersie van het model checken. Overdispersie betekent dat de data grotere variabiliteit heeft dan het model voorspelt.

- Type Residu:
  - Afwijking: De gestandaardiseerde afwijkingsresiduen.
  - Pearson: De gestandaardiseerde Pearson residuen.

 
### Uitvoer
---

#### Logistische regressie
Samenvatting model: 
- Model: De verschillende hypothesen die worden vergeleken.
- afwijking: -2 x log-likelihood.
- AIC: Vergelijk modellen met het Akaike Informatie Criterium.
- BIC: vergelijk modellen met het Bayesiaanse Informatie Criterium.
- vg: Vrijheidsgraden.
- X2: Chi-kwadraat.
- p: De p-waarde.
- Determinatiecoëfficient waarde (de proportie van de variantie die wordt verklaard door het model). Er zijn drie pseudo R^2 waarden beschikbaar in JASP: 
  - McFadden: berekend als 1 min de ratio van de log-likelihoods van het gespecificeerde model en het nul-model. Als het gespecificeerde model beter op de data past dan het nul-model, dan is McFadden's R2 in de buurt van 1. Als het nul-model ongeveer evengoed op de data past als het gespecificeerde model, dan is McFadden's R2 in de buurt van 0.
  - Cox & Snell: berekend als 1 min de ratio van de log-likelihoods van het gespecificeerde model en het nul-model, waarbij de ratio verheven wordt tot de macht 2/n (steekproefgrootte). Hogere waarden geven aan dat het gespecificeerde model relatief beter op de data past dan het nul-model. De Cox & Snell index heeft echter als limiet 1 min de likelihood van het nul-model, verheven tot de macht 2/n, en kan zelfs onder ideale omstandigheden niet hoger zijn dan 0.75.
  - Nagelkerke: dit is een correctie op de methode van Cox & Snell, zodat het als limiet 1 heeft. Deze index wordt berekend als de Cox & Snell R2, gedeeld door 1 min de likelihood van het nul-model verheven tot de macht 2/n. Waarden in de buurt van 1 geven aan dat het gespecificeerde model beter op de data past dan het nul-model.
  - Tjur: berekend als de absolute waarde van het verschil tussen de gemiddelde voorspelde waarde voor alle gevallen waar de afhankelijke variabele gelijk is aan 0, en de gemiddelde voorspelde waarde voor alle gevallen waar de afhankelijke variabele gelijk is aan 1. In tegenstelling tot de andere pseudo R2 indices, is Tjur's R2 niet relatief aan het nul-model.
 
Coëfficiënten
- Schatting: Regressiecoëfficiënten.
- (Robuuste) standaardfout: Standaardfout van de regressiecoëfficiënten.
- Gestandaardiseerd: Gestandaardiseerde regressiecoëfficiënten.
- Odds ratio: De belangrijkste waarden in de coëfficiënten tabel. Door een continue predictor geeft een odds ratio > 1 een positieve relatie aan terwijl <1 een negatieve relatie aanduidt. 
- z: De z-waarde. 
- Wald toets: De Wald toets wordt gebruikt om de statistische significantie van elk coëfficiënt te evalueren.
  - Wald statistiek: z^2.
  - vg: Vrijheidsgraden.
  - p: De p-waarde.
- VS-MPR: Vovk-Sellke maximum p-ratio.
- % BI: Het betrouwbaarheidsinterval (odds-ratio schaal). Standaard is 95%. 
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.
   
Bootstrap Coëfficiënten.
- Schatting: ge-bootstrapte regressiecoëfficiënten.
- Bias: Schatting van de bias.
- Standaardfout: Standaardfout van de ge-bootstrapte regressiecoëfficiënten.

Stapsgewijze Diagnostieken: 
  - Gevalsnummer: Identificatie van het geval.
  - Geobserveerd: De geobserveerde waarde van de uitkomst.
  - Voorspeld: De voorspelde waarde. 
  - Voorspelde groep: De voorspelde afhankelijke waarde van de uitkomst. 
  - Residu: Het verschil tussen de geobserveerde en voorspelde waarde. 
  - Gestandaardiseerd residu. 
  - Cook's afstand: De waarde van Cook's afstand. 
  - % BI: Het ge-bootsttrapte betrouwbaarheid interval voor de ongestandaardiseerde coëfficiënten. Standaard is 95%. 
    - Onder: De ondergrens van het betrouwbaarheid interval.
    - Boven: De bovengrens van het betrouwbaarheid interval.

Factor beschrijvingende statistieken:
  - De eerste kolom geeft alle niveaus van de factor.
  - N: Het aantal observaties per niveau van de factor.

   
#### Prestatie statistieken
Confusion Matrix:
- De confusion matrix geeft aan hoe goed het model de uitkomsten voorspelt. In de diagonaal staan de waarnemingen die het model correct identificeerde. Daarbuiten de waarnemingen waar het model een verkeerde uitkomst voorspelde.
 
Prestatie Matrix:
- Alle geselecteerde prestatiestatistieken en hun waarden staan in deze tabel.
 
#### Geschatte grafieken
De conditionele schattings grafieken geven de kans weer op de afhankelijke variabele voor alle niveaus van de covariaat gegeven de referentie over alle andere factoren. Als een continue covariaat wordt toegevoegd wordt er een grijze waas rond de lijn aangebracht die een 95% betrouwbaarheid interval aangeeft.
 
#### Residu grafieken 
Voorspeld - residu grafiek.

Voorspeller - residu grafiek voor predictor.

Gekwadrateerde Pearson residuen grafiek.
- De verwachte waarde van de gekwadrateerde residuen in 1, aangegeven met een stippellijn. De rode lijn geeft de smoother aan door de residuen (= bewegend gemiddelde). Als de rode lijn vooral dichtbij de 1 is kan het worden geconcludeerd dat het model niet aan overdispersie leidt. Enige afwijking rond de staarten valt te verwachten.

### Referenties
-------
- Field, A.P., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. London: Sage.
- Moore, D.S., McCabe, G.P., & Craig, B.A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W.H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Stevens, J.P. (2009). *Applied multivariate statistics for the social sciences (5th ed.)*. New York, NY: Routledge.

### R Packages
-------
- boot
- ggplot2
- hmeasure
- MASS
- matrixStats
- mdscore
- stats
