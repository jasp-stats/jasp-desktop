Bayesiaanse ANCOVA
===
 
Met de Bayesiaanse ANCOVA kan men het verschil tussen meerdere groepsgemiddelden analyseren, terwijl er rekening wordt gehouden met het effect van variabelen die een invloed hebben op de afhankelijke variabele, maar geen deel uitmaken van de experimentele manipulatie (i.e., covariaten). 

### Assumpties
- De residuen zijn voor iedere groep normaal verdeeld.
- De onafhankelijke variabelen zijn categorisch, de afhankelijke variabele is continu. 
- De variantie in de afhankelijke variabele is hetzelfde voor iedere groep. Dit heet de homogeniteit van varianties. 
- De groepen zijn onafhankelijk. 
- Voor elke onafhankelijke variabele is de relatie tussen de afhankelijke variabele en de covariaat lineair. 
- Het effect van de covariaat op de afhankelijke variabele verschilt niet per groep. Dit heet de homogeniteit van de regressielijnen. 

### Invoer
---
#### Invoerveld
- Afhankelijke Variabele: De variabele waarin we het meest geïnteresseerd zijn. Deze wordt ook wel de uitkomstvariabele genoemd. 
- Vaste Factoren: De variabelen die zijn gemanipuleerd/die de verschillende groepen definiëren. Deze worden ook wel de onafhankelijke variabelen genoemd.  
- Aselecte Factoren: In dit veld kan de gebruiker de variabele selecteren die moet worden meegenomen in alle modellen, inclusief het nulmodel.
- Covariaten: In dit veld kan de variabele die de covariaat is worden geselecteerd. Covariaten zijn continue variabelen die een invloed uitoefenen op de afhankelijke variabele, maar geen deel uitmaken van de experimentele manipulatie.  

#### Bayes Factor  
- BF<sub>10</sub>: Als u deze optie selecteert, geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als u deze optie selecteert, geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : Als u deze optie selecteert, wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusie</sub>, BF<sub>10, U</sub> weergegeven in de uitvoer.

#### Uitvoer 
 - Effecten: Als deze optie wordt geselecteerd, wordt de kans op inclusie van elke component (i.e., modelterm) in de modellen berekend. 
	- Over alle modellen: Als deze optie wordt geselecteerd, zal elk model waar de component in is meegenomen, worden gebruikt om het effect te berekenen (i.e., inclusiekans) van de component. Wanneer de optie `Effecten` is geselecteerd, is dit de standaardoptie.
    - Over alle gematchte modellen: Wanneer deze optie is geselecteerd, worden alleen modellen met precies die component meegenomen in de analyse. Zodanig worden interacties met de component uitgesloten. Vergelijkt modellen die de component meenemen met modellen die deze niet meenemen. Hogere-orde interacties worden uitgesloten. Analyse voorgesteld door Sebastiaan Mathot.
      <details>
        <summary><b>GIF demonstratie: Selecteer effecten </b></summary>
        <img src="analyses/gif/inclusion_bayes_anova.gif"/>
      </details>

- Schattingen: Als deze optie is geselecteerd, wordt een tabel weergegeven met een samenvatting van de model gemiddelde posterior. Deze tabel bevat informatie over het model gemiddelde posterior, de standaardafwijking, en de geloofwaardigheidsinterval voor elk niveau van de vaste factoren, hun interacties en de covariaten.
- Beschrijvende statistieken: Als deze optie is geselecteerd, worden het gemiddelde, de standaardafwijking en de steekproefgrootte weergegeven voor elke combinatie van de onafhankelijke variabelen. 
  - Geloofwaardigheidsinterval: Het centrale geloofwaardigheidsinterval. De standaardoptie is 95%.
 
#### Volgorde
- Vergelijk met nulmodel: De modellen die gebruikt worden in de analyse onder de optie 'model', zullen worden vergeleken met het model dat het totale gemiddelde en de willekeurige factoren bevat (het nulmodel). Dit is de standaardoptie.
- Vergelijk met het beste model: De modellen die gebruikt worden in de analyse onder de optie 'model', zullen worden vergeleken met het beste model in de analyse.
  <details>
    <summary><b>GIF demonstratie: Selecteer volgorde </b></summary>
    <img src="analyses/gif/banova_simplebestcrop.gif"/>
  </details>
  
#### Grafieken
- Model gemiddelde posteriors: Als deze optie is geselecteerd, worden er grafieken weergegeven die de model gemiddelde posterior van elke vaste factor, interactie en covariaat illustreren. 
  - Groep niveaus in een enkele grafiek: Als deze optie is geselecteerd, wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
  - Een grafiek per niveau: Als deze optie is geselecteerd, wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek worden weergegeven.
- Q-Q grafiek van residuen: Controleert de validiteit van de verdelingsassumpties van de dataset. Om precies te zijn geeft de plot weer of de residuen normaal zijn verdeeld. 
- Posterior R<sup>2</sup>: Als deze optie is geselecteerd, wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).
 
### Model
- Componenten: Alle onafhankelijke variabelen, en covariaten die kunnen worden meegenomen in het model.
- Modeltermen: De onafhankelijke variabelen en covariaten die worden meegenomen in het model. Alle vaste factoren staan standaard in dit veld.
- Toevoegen aan nulmodel: De onafhankelijke variabelen en covariaten die worden meegenomen in het model kunnen worden geselecteerd om aan het nulmodel toe te voegen.

### Enkele Model Inferentie
- Hier kan een enkel model worden gespecificeerd om informatie van de posterior van dit model te verkrijgen, inclusief een tabel met een samenvatting van de posterior en grafieken van de marginale posterior. 
  - Toekennings veld: Hier wordt het enkele model gespecificeerd.
	- Componenten: Dit veld bevat alle factoren en covariaten die meegenomen zouden moeten worden in het model. 
    - Specifieke modeltermen: Selecteer de factoren en covariaten die meegenomen moeten worden in het model.
  - Tabellen:
	- Schatting: Een tabel zal worden weergegeven met de samenvatting van de posterior voor het enkele model dat is gespecificeerd in het invulveld. Deze tabel geeft informatie over het gemiddelde van de enkele model posterior, de standaardafwijking en het geloofwaardigheidsinterval van elk niveau van de vaste factoren, hun interacties en de covariaten die zijn meegenomen in het model. Dit is anders dan de `estimate` optie in de Uitvoer, omdat de `estimates` functie een samenvatting geeft van de posterior gemiddeld over alle modellen in de analyse, terwijl deze optie de posterior geeft voor het enkele gespecificeerde model. 
  - Grafieken:
	- Marginale posteriors: Als deze optie is geselecteerd, worden er grafieken gegenereerd die de posterior verdeling van elke vaste factor, interactie en covariaat meegenomen in het model weergeven. 
		- Group niveaus in een grafiek: Als deze optie is geselecteerd, wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
		- Een grafiek per niveau : Wanneer deze optie is geselecteerd, wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek weer worden gegeven.
    - Q-Q grafiek van residuen: Controleert de validiteit van de verdelingsassumpties van de dataset. Om precies te zijn geeft de plot weer of de residuen normaal zijn verdeeld. 
	- Posterior R<sup>2</sup>: Als deze optie is geselecteerd, wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).
  

### Post-Hoc Toetsen
- Sleep de naam van een factor naar de rechterkolom om een post-hoc toets uit te voeren. 
  <details>
    <summary><b>GIF demonstratie: Post-hoc toets </b></summary>
    <img src="analyses/gif/bayes_posthoc_anova.gif"/>
  </details>

  Dan is het mogelijk om het volgende te selecteren:
	- Correctie
	  - Nulcontrole: Wanneer deze optie is geselecteerd, worden de prior odds gecorrigeerd voor meervoudig toetsen. Dit is de standaardoptie. Momenteel wordt er geen uitvoer voor de post-hoc toetsen gegenereerd als u deze optie niet selecteert. 


### Beschrijvende grafieken
- Selecteer de onafhankelijke variabele op de horizontale as om een beschrijvende grafiek te maken. Als er meerdere onafhankelijke variabelen zijn kunnen de variabelen in een plot worden weergegeven door de andere variabele in het veld Aparte lijnen te zetten. De variabelen kunnen ook in aparte grafieken worden weergegeven door de andere variabele in het veld Aparte grafieken te zetten.
  - Factoren: De onafhankelijke variabelen die mee worden genomen in de analyse.
  - Horizontale as: Selecteer de onafhankelijke variabele die op de horizontale as moet worden weergegeven.
  - Aparte lijnen: Door een onafhankelijke variabele in dit veld te plaatsen, corresponderen verschillende lijnen met verschillende niveaus van de geselecteerde onafhankelijke variabele.
  - Aparte grafieken: Door een onafhankelijke variabele in dit veld te plaatsen, corresponderen verschillende grafieken met verschillende niveaus van de geselecteerde onafhankelijke variabele.
- Weergeven:
	  -  Geloofwaardigheidsinterval: Wanneer deze optie is geselecteerd, bevat de grafiek centrale geloofwaardigheidsintervallen. De standaardoptie is 95%. Dit kan worden aangepast tot het gewenste percentage. 

### Aanvullende opties 
- Prior: Hier kunnen de prior verdelingen worden ingesteld voor de willekeurige en vaste effectgroottes.
	- r schaal gefixeerde effecten: De vorm parameter van de prior verdeling voor gefixeerde effecten. De standaardwaarde is 0.5, maar dit kan naar wens worden aangepast.
	- r schaal willekeurige effecten: De vorm parameter van de prior verdeling voor de willekeurige effecten. De standaardwaarde is 1, maar dit kan naar wens worden aangepast. 
- Numerieke precisie: Het aantal stappen dat wordt genomen om de integraal voor de Bayes factor te berekenen.
  - Auto: Als deze optie is geselecteerd, worden er 10000 stappen gezet. Dit is de standaardoptie.
  - Handmatig: Als deze optie is geselecteerd, kan het aantal stappen handmatig worden ingesteld. De standaardwaarde is 10000.
- Posterior Steekproeven: Het is mogelijk om het aantal Markov Chain Monte Carlo steekproeven in te stellen. Dit wordt gebruikt om de posterior verdeling en het fout % te benaderen. 
	- Auto: Als deze optie is geselecteerd, worden er 10000 steekproeven gebruikt. Dit is de standaardoptie.
    - Handmatig: Als deze optie is geselecteerd, kan het aantal steekproeven handmatig worden ingesteld. Bij deze optie is de standaardwaarde 1000.
- Reproduceerbaarheid:
  - Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Dit zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

### Uitvoer
---
#### Bayesiaanse ANCOVA 
Model vergelijking - Afhankelijke variabele: 
- Modellen: De eerste kolom bevat alle modellen die worden meegenomen in de analyse.
	- Nulmodel: Dit model bevat het totale gemiddelde en de willekeurige factoren.
    - Onafhankelijke variabele model: Dit model voegt het effect van de onafhankelijke variabele toe.
- P(M): Deze kolom bevat de prior kans van het model. 
- P(M|data): Deze kolom bevat de bijgewerkte kans op het model gegeven de data. Dit heet de posterior model kans. 
- BF<sub>M</sub> : Deze kolom bevat de posterior model odds. Dit is de verandering van de prior odds naar de posterior odds van het model. 
- BF<sub>10</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese geeft. Echter, als de optie `vergelijk met het beste model` is geselecteerd, bevat de kolom de Bayes factor het bewijs voor dit model ten opzichte van het beste model geeft. 
- BF<sub>01</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de nulhypothese/het nulmodel ten opzichte van de alternatieve hypothese geeft. Echter, als de optie `vergelijk met het beste model` is geselecteerd, bevat de kolom de Bayes factor het bewijs voor het beste model ten opzichte van dit model geeft. 
- fout %: De fout van de Gaussiaanse kwadratuur integratie routine die het "Bayes Factor" package gebruikt voor het berekenen van de Bayes Factor.

Analyse van Effecten - Afhankelijke Variabele:
- Effecten: Deze kolom bevat de componenten meegenomen in de modellen, zoals de onafhankelijke variabelen en hun interacties.
- P(incl): Deze kolom bevat de prior inclusiekans. Dit is de opgetelde prior kans over alle modellen die de component bevatten.
- P(incl|data): Deze kolom bevat de posterior inclusiekans. Dit is de opgetelde posterior kans over alle modellen die de component bevatten.
- BF<sub>inclusie</sub>: Deze kolom bevat de verandering van prior inclusie odds tot posterior inclusie odds voor elke component gemiddeld over alle modellen die de component bevatten.

Samenvatting van Model Gemiddelde van de Posterior:
- Variabele: Deze kolom bevat alle vaste factoren, hun interacties, en covariaten die zijn meegenomen in de modellen. De eerste rij geeft informatie over de intercept. 
- Niveau: Elk niveau van de factor en combinatie van niveaus van de interacties die zijn meegenomen in het model.
- Gemiddelde: Het gemiddelde van het modelgemiddelde. Voor de factoren is dit de afwijking van de intercept voor elk niveau van de factor. De niveau gemiddelden voor een factoren tellen op tot 0. 
- SD: De standaardafwijking van het modelgemiddelde. 
- % Geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
  - Onder: De ondergrens van het geloofwaardigheidsinterval van het gemiddelde.
  - Boven: De bovengrens van het geloofwaardigheidsinterval van het gemiddelde.

#### Modelgemiddelde Posterior Verdelingen 
Voor elke factor, interactie, en covariaat worden de modelgemiddelde posterior verdelingen per niveau weergegeven, met de factor op de x-as en dichtheid op de y-as. De posterior verdeling van elk niveau kan in dezelfde grafiek of in een andere grafiek worden weergegeven. 

#### Modelgemiddelde Q-Q Grafiek
Met de Q-Q grafiek kan de normaliteit van residuen visueel worden bekeken. De theoretische kwantielen staan op de x-as en de gestandaardiseerde residuen op de y-as. Hoe dichter de punten bij de diagonaal liggen, hoe meer bewijs dat de residuen normaal zijn verdeeld. 

#### Modelgemiddelde Posterior R<sup>2</sup>
De modelgemiddelde dichtheid van de verklaarde variantie (R<sup>2</sup>).

#### Post-Hoc Toetsen
Post-hoc Vergelijkingen - Onafhankelijke Variabele:
  - De eerste kolommen bevatten de niveaus van de onafhankelijke variabelen die worden vergeleken. 
  - Prior Odds: Deze kolom bevat de prior odds. Deze zijn gecorrigeerd voor meervoudig toetsen (Westfall, Johnson, & Utts, 1997). 
  - Posterior Odds: deze kolom bevat de posterior odds. Dit zijn de prior odds vermenigvuldigd met de Bayes factor.
  - BF<sub>10, U</sub>: Deze kolom bevat de Bayes factor die bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese weergeeft. De Bayes factor is niet gecorrigeerd voor meervoudig toetsen.
  - BF<sub>01, U</sub>: Deze kolom bevat de Bayes factor die bewijs voor de nulhypothese/het nulmodel ten opzichte van de alternatieve hypothese weergeeft. De Bayes factor is niet gecorrigeerd voor meervoudig toetsen.
  - Fout %: De fout van de Gaussiaanse kwadratuur integratie routine gebruikt voor het berekenen van de Bayes Factor.

#### Enkel Model Inferentie
Enkel model Samenvatting van de Posterior:
  - Variabele: Deze kolom bevat alle vaste factoren, interacties en covariaten die zijn meegenomen in de modellen. De eerste rij bevat informatie over de intercept. 
  - Niveau: Elk niveau van de factor en combinatie van niveaus van de interacties die zijn meegenomen in het enkele model.
  - Gemiddelde: Het enkele model gemiddelde. Voor de factoren is dit de afwijking van de intercept voor elk niveau van de factor. De niveau gemiddelden voor een factor tellen op tot nul. 
  - SD: De standaardafwijking van van het enkele model gemiddelde. 
  - % Geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
    - Onder: De ondergrens van het geloofwaardigheidsinterval van het gemiddelde.
    - Boven: De bovengrens van het geloofwaardigheidsinterval van het gemiddelde. 


Posterior Verdelingen: 
- Voor elke factor, interactie en covariaat worden de posterior verdelingen van het enkele model per niveau weergegeven. De posterior verdeling voor elk niveau worden ofwel in dezelfde grafiek, ofwel in verschillende grafieken voor elk niveau worden weergegeven. 

Q-Q Grafiek: 
- Met de Q-Q grafiek kan de normaliteit van residuen visueel worden geïnspecteerd. De theoretische kwantielen staat op de x-as en de gestandaardiseerde residuen op de y-as. Hoe dichter de punten bij de diagonaal liggen, hoe meer bewijs dat de residuen normaal verdeeld zijn.

Posterior R<sup>2</sup>: 
- De dichtheid van R<sup>2</sup> (verklaarde variantie) voor een enkel model, met R<sup>2</sup> op de x-as en dichtheid op de y-as.

#### Beschrijvende Statistieken
Beschrijvende Statistieken - afhankelijke variabele:
- Onafhankelijke variabelen: De niveaus van de onafhankelijke variabele(n) die zijn meegenomen in de analyse. Als het er meer dan een zijn worden de beschrijvende statistieken weergegeven voor elke combinatie van niveaus van de onafhankelijke variabelen.
- Gemiddelde: Het gemiddelde per niveau, of, als er meer dan 1 onafhankelijke variabele is, het gemiddelde per combinatie van niveaus. 
- SD: De standaardafwijking.
- N: De steekproefgrootte.
  - % geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardwaarde is 95%.
    - Onder: De ondergrens van het geloofwaardigheidsinterval van het gemiddelde.
    - Boven: De bovengrens van het geloofwaardigheidsinterval van het gemiddelde.

Beschrijvende Grafiek: 
- Onafhankelijke variabele op de x-as en afhankelijke variabele op de y-as. Als andere onafhankelijke variabelen worden meegenomen, kunnen verschillende lijnen in dezelfde grafiek de andere onafhankelijke variabele weergeven, of kunnen er verschillende grafieken worden gemaakt voor verschillende onafhankelijke variabelen. 

### Referenties
---
- Rouder, J. N., Engelhardt C. R., McCabe S., & Morey R. D. (2016). Model comparison in ANOVA. *Psychonomic Bulletin and Review, 23*, 1779-1786.
- Rouder, J.N., Morey R.D., Speckman P.L., & Province J M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*, 356-374.
- Rouder, J. N., Morey, R. D., Verhagen, A. J., Swagman, A. R., & Wagenmakers, E.-J. (2017). Bayesian analysis of factorial designs. *Psychological Methods, 22*, 304-321.
- van den Bergh, D., Van Doorn, J., Marsman, M., Draws, T., Van Kesteren, E.J., ... & Wagenmakers, E.-J. (2019). A Tutorial on Conducting and Interpreting a Bayesian ANOVA in JASP. Manuscript submitted for publication.
- Wagenmakers, E. J., Love, J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., ... & Meerhoff, F. (2018). Bayesian inference for psychology. Part II: Example applications with JASP. *Psychonomic bulletin & review, 25*(1), 58-76.
- Westfall, P. H., Johnson, W. O., & Utts, J. M. (1997). A Bayesian perspective on the Bonferroni adjustment. *Biometrika, 84*, 419-427.
- Wetzels, R., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for ANOVA designs. *The American Statistician, 66*, 104-111.

### R Packages
---
- BayesFactor
- colorspace
- ggplot2
- KernSmooth
- matrixStats
- plyr
- stats
- stringi
- stringr
- utils

### Voorbeeld
---
- Voor een uitgebreid voorbeeld, zie van den Bergh et al. (2019).
