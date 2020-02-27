Bayesiaanse ANOVA
===
 
De bayesiaanse ANOVA laat de gebruiker verschillende tussen meerdere groepsgemiddelden berekenen.

### Assumpties 
- De residuen zijn voor iedere groep normaal verdeeld.
- De onafhankelijke variabelen zijn categorisch, de afhankelijke variabele is continu. 
- De variantie in de afhankelijke variabele is hetzelfde voor iedere groep. Dit heet de homogeniteit van variantie. 
- De groepen zijn onafhankelijk. 

### Invoer
---
#### Invoerveld
- Afhankelijke Variabele: De variabele waarin we het meest geïnteresseerd zijn. Deze wordt ook wel de uitkomst variabele genoemd. 
- Vaste Factoren: De variabelen die zijn gemanipuleerd/die de verschillende groepen definiëren. Deze worden ook wel de onafhankelijke variabelen genoemd.  
- Willekeurige Factoren: In dit veld kan je de variabele selecteren die in alle modellen moet worden meegenomen, inclusief het nulmodel.

#### Bayes Factor  
- BF<sub>10</sub>: Als je deze optie selecteert, geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als je deze optie selecteert, geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : als je deze optie selecteert, wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusie</sub>, BF<sub>10, U</sub> weergegeven in de uitvoer.

#### Uitvoer
- Effecten: Als je deze optie selecteert, wordt de kans op inclusie van elke component (i.e., model term) in de modellen berekend. 
	- Over alle modellen: Als je deze optie selecteert, wordt elk model waar de component in wordt meegenomen gebruikt om het effect te berekenen (i.e., inclusie kans) van de component. Wanneer de optie `Effecten` is geselecteerd is dit de standaardoptie.
    - Over alle gematchte modellen:  Als je deze optie selecteert, worden alleen modellen met precies die component meegenomen in de analyse. Zodanig worden interacties met de component uitgesloten. Vergelijkt modellen die de component meenemen met modellen die hem niet meenemen, hogere orde interacties worden uitgesloten. Analyse voorgesteld door Sebastiaan Mathot.
      <details>
        <summary><b>GIF demonstration: Geselecteerde effecten </b></summary>
        <img src="analyses/gif/inclusion_bayes_anova.gif"/>
      </details>

- Schattingen: Als je deze optie selecteert, wordt een tabel weergegeven met een samenvatting van de model gemiddelde posterior. Deze tabel bevat informatie over de model gemiddelde standaardafwijking, gemiddelde, en betrouwbaarheidsinterval voor elk niveau van de vaste factoren en hun interacties.
- Beschrijvende statistieken: Als je deze optie selecteert, worden het gemiddelde, de standaardafwijking en de steekproefgrootte weergegeven voor elke combinatie van de onafhankelijke variabele. 
  - Betrouwbaarheidsinterval: Het centrale betrouwbaarheidsinterval. De standaardoptie is 95%.
 
#### Volgorde
- Vergelijk met nulmodel: De modellen die gebruikt worden in de analyse onder de optie 'model', vergelijken met het model dat het totale gemiddelde en de willekeurige factoren bevat (het nulmodel). Dit is de standaardoptie.
- Vergelijk met het beste model: De modellen die gebruikt worden in de analyse onder de optie 'model', vergelijken met het beste model in de analyse.
  <details>
    <summary><b>GIF demonstratie: Selecteer volgorde </b></summary>
    <img src="analyses/gif/banova_simplebestcrop.gif"/>
  </details>

#### Grafieken
- Modelgemiddelde posteriors: Als je deze optie selecteert, worden er grafieken weergegeven die de model gemiddelde posterior van elke vaste factor en interactie illustreren. 
  - Groepsniveaus in één grafiek: Als je deze optie selecteert, wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
  - Een grafiek per niveau : Als je deze optie selecteert, wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek weer worden gegeven.
- Q-Q grafiek van residuen: Controleert de validiteit van de verdelingsassumpties van de dataset. Om precies te zijn geeft de grafiek weer of de residuen normaal zijn verdeeld. 
- Posterior R<sup>2</sup>: Als je deze optie selecteert, wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).

### Model
- Componenten: Alle onafhankelijke variabelen die worden meegenomen in het model 
- Modeltermen: De onafhankelijke variabelen die worden meegenomen in het model. Alle vaste factoren staan standaard in dit veld.
- Toevoegen aan nul model: De onafhankelijke variabelen die worden meegenomen in het model kunnen worden geselecteerd om aan het nulmodel toe te voegen.

### Enkel model inferentie
- Hier kan een enkel model worden gespecificeerd om informatie van de posterior van dit model te verkrijgen, inclusief een tabel met een samenvatting van de posterior en grafieken van de marginale posterior. 
  - Invoerveld Hier wordt het enkele model gespecificeerd.
	- Componenten: Dit veld bevat alle factoren in het model. 
    - Specifieke modeltermen: Selecteer de factoren die meegenomen moeten worden in het model.
  - Tabellen:
	- Schatting: Een tabel met de samenvatting van de posterior voor het model dat is gespecificeerd in het invulveld. Deze tabel geeft informatie over het gemiddelde, de standaardafwijking en het betrouwbaarheidsinterval van elk niveau van de vaste factoren van het model. Dit is anders dan de "schatten" optie in Uitvoer, omdat de "estimate" functie een samenvatting geeft van de posterior gemiddelde over alle modellen in de analyse, terwijl deze optie de posterior geeft voor het enkele model. 
  - Grafieken
	- Marginale posteriors: Als je deze optie selecteert, worden er grafieken gegenereerd die de posterior verdeling van elke vaste factor van het model en hun interactie weergeven. 
		- Groepniveaus in één grafiek: Als je deze optie selecteert, wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
		- Een grafiek per niveau : Als je deze optie selecteert, wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek weer worden gegeven.
    - Q-Q grafiek van residuen: Controleert de validiteit van de verdelingsassumpties van de dataset. Om precies te zijn geeft de grafiek weer of de residuen normaal zijn verdeeld. 
	- Posterior R<sup>2</sup>: Als je deze optie selecteert, wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).


### Post Hoc Toetsen
- Sleep de naam van een factor naar de rechterkolom om een post-hoc toets uit te voeren. 
  <details>
    <summary><b>GIF demonstratie: Post-hoc toets </b></summary>
    <img src="analyses/gif/bayes_posthoc_anova.gif"/>
  </details>

  Dan is het mogelijk om het volgende te selecteren:
	- Correctie:
	  - Nulcontrole: Als je deze optie selecteert, worden de prior odds gecorrigeerd voor meervoudig toetsen. Dit is de standaardoptie. Momenteel wordt er geen output voor de post-hoc toetsen gegenereerd, als je deze optie niet selecteert. 

### Beschrijvende grafieken
- Selecteer de onafhankelijke variabele op de horizontale as om een beschrijvende grafiek te maken. Als er meerdere onafhankelijke variabelen zijn, kunnen de variabelen in een grafiek worden weergegeven door de andere variabele in het veld Aparte lijnen te zetten. De variabelen kunnen ook in diverse grafieken worden weergegeven door de andere variabele in het veld Aparte grafieken te zetten.
  - Factoren: De onafhankelijke variabelen die mee worden genomen in de analyse.
  - Horizontale as: Selecteer de onafhankelijke variabele die op de horizontale as wordt weergegeven.
  - Aparte lijnen: Door een onafhankelijke variabele in dit veld te plaatsen, corresponderen verschillende lijnen met verschillende niveaus van de geselecteerde variabele.
  - Aparte grafieken: Door een onafhankelijke variabele in dit veld te plaatsen, corresponderen verschillende grafieken met verschillende niveaus van de geselecteerde variabele.
- Weergeven:
	  -  Geloofwaardigheidsinterval: Als je deze optie selecteert, bevat de grafiek de geloofwaardigheidsinterval. De standaardoptie is 95%. Dit kan worden aangepast tot het gewenste percentage. 

### Aanvullende opties 
- Prior: Hier kan je de prior verdelingen voor de willekeurige en vaste effectgroottes bepalen.
	- r schaal vaste effecten: De vorm parameter van de prior verdeling voor vaste effecten. De standaardwaarde is .5, maar je kan het naar behoeven aanpassen,
	- r schaal willekeurige effecten: De vorm parameter van de prior verdeling voor de willekeurige effecten. De standaardwaarde is 1, maar je kan het naar behoeven aanpassen. 
- Numerieke precisie: Het aantal stappen dat wordt genomen om de integraal voor de Bayes factor te berekenen.
  - Auto: Als je deze optie selecteert, worden er 10000 stappen gezet. Dit is de standaardoptie.
  - Handmatig: Als je deze optie selecteert, kan je handmatig het aantal stappen selecteren. De standaardwaarde is 10000.
- Posterior steekproeven: Het is mogelijk om het aantal Markov Chain Monte Carlo steekproeven in te stellen. Dit wordt gebruikt om de posterior verdeling en het fout % te benaderen. 
	- Auto: Als deze optie is geselecteerd, worden er 10000 steekproeven gebruikt. Dit is de standaardoptie.
    - Handmatig: Als je deze optie selecteert, kan je handmatig het aantal steekproeven instellen. Als je deze optie kiest, is de standaardwaarde 1000.
- Reproduceerbaarheid:
  - Gebruik seed: Geeft de mogelijkheid een seed te gebruiken voor je analyse. Een seed gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

### Output
---
#### Bayesiaanse ANOVA 
Model vergelijking - Afhankelijke variabele: 
- Modellen: De eerste kolom bevat alle modellen die worden meegenomen in de analyse.
	- Nulmodel: Dit model bevat het totale gemiddelde en de willekeurige factoren.
    - Onafhankelijke variabele model: Dit model voegt het effect van de onafhankelijke variabele toe.
- P(M): Deze kolom bevat de prior kans van het model. 
- P(M|data): Deze kolom bevat de geüpdatet kans op het model gegeven de data. Dit heet de posterior kans. 
- BF<sub>M</sub> : Deze kolom bevat de posterior model odds. Dit is de verandering van de prior odds naar de posterior odds van het model. 
- BF<sub>10</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese geeft. Echter, als de optie 'vergelijk met het beste model' is geselecteerd, bevat de kolom de Bayes factor het bewijs voor dit model ten opzichte van het beste model geeft. 
- BF<sub>01</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese geeft. Echter, als de optie 'vergelijk met het beste model' is geselecteerd, bevat de kolom de Bayes factor het bewijs voor het beste model ten opzichte van dit model geeft. 
- fout %: De fout van de Gaussiaanse kwadratuur integratie routine die het "Bayes Factor" package gebruikt voor het berekenen van de Bayes Factor.

Analyse van effecten - Afhankelijke variabele:
- Effecten: Deze kolom bevat de componenten meegenomen in de modellen, zoals de onafhankelijke variabelen en hun interacties.
- P(incl): Deze kolom bevat de prior inclusie kans. Dit is de opgetelde prior kans over alle modellen die de component bevatten.
- P(incl|data): Deze kolom bevat de posterior inclusiekant. Dit is de opgetelde posterior kans over alle modellen die de component bevatten.
- BF<sub>inclusie</sub> : Deze kolom bevat de verandering van prior inclusie odds tot posterior inclusie odds voor elke component gemiddeld over alle modellen die de component bevatten.

Model Gemiddelde samenvatting van de posterior:
- Variabele: Deze kolom bevat alle vaste factoren en hun interacties. De eerste rij geeft informatie over het intercept. 
- Niveau: Elk niveau van de factor en combinatie van niveaus van de interacties in het model.
- Gemiddelde: Het gemiddelde van het modelgemiddelde. Voor de factoren is dit de afwijking van het intercept voor elk niveau van de factor. De niveau gemiddelden voor een factoren tellen op tot 0. 
- SD: De standaardafwijking van het modelgemiddelde gemiddelde. 
- % geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
  - Linker: De linkergrens van het geloofwaardigheidsinterval van het gemiddelde.
  - rechter: De rechtergrens van het geloofwaardigheidsinterval van het gemiddelde.
  
#### Modelgemiddelde posterior verdeling 
Voor elke factor en interactie worden de modelgemiddelde posterior verdelingen per niveau weergegeven met de factor op de x-as en dichtheid op de y-as. De posterior verdeling van elk niveau kan in dezelfde grafiek of in een andere grafiek worden weergegeven. 

#### Modelgemiddelde Q-Q grafiek
Met de Q-Q grafiek kan de normaliteit van residuen visueel worden geïnspecteerd. De theoretische kwantielen staat op de x-as en de gestandaardiseerde residuen op de y-as. Hoe dichter de punten bij de diagonaal liggen, hoe meer bewijs dat de residuen normaal zijn verdeeld. 

#### Modelgemiddelde posterior R<sup>2</sup>
De modelgemiddelde dichtheid van de verklaarde variantie (R<sup>2</sup>).

#### Post Hoc Toetsen
Post hoc vergelijkingen - onafhankelijke variabele:
	- De eerste kolommen bevatten die niveaus van de onafhankelijke variabelen die worden vergeleken. 
	- Deze kolom bevat de prior odds. Deze zijn gecorrigeerd voor meervoudig toetsen (Westfall, Johnson, & Utts, 1997).
	- Posterior odds: Deze kolom bevat de posterior odds. Dit zijn de prior odds vermenigvuldigd met de Bayes factor.
	- BF<sub>10, U</sub>: Deze kolom bevat de Bayes factor die bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese weergeeft. De Bayes factor is niet gecorrigeerd voor meervoudig toetsen. 
	- BF<sub>01, U</sub> : Deze kolom bevat de Bayes factor die bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese weergeeft. De Bayes factor is niet gecorrigeerd voor meervoudig toetsen. 
	- Fout %: De fout van de Gaussiaanse kwadratuur integratie routine die het "Bayes Factor" package gebruikt voor het berekenen van de Bayes Factor.

#### Enkel model inferentie
Enkel model samenvatting van de posterior:
  - Variabele: Deze kolom bevat alle vaste factoren en hun interacties. De eerste rij geeft informatie over het intercept. 
  - Niveau: Elk niveau van de factor en combinatie van niveaus van de interacties in het model.
  - Gemiddelde: Het model gemiddelde gemiddelde. Voor de factoren is dit de afwijking van het intercept voor elk niveau van de factor. De niveau gemiddelden voor een factoren tellen op tot 0. 
  - SD: De standaardafwijking van het modelgemiddelde gemiddelde. 
  - % geloofwaardigheidsinterval: het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
    - Linker: De linkergrens van het geloofwaardigheidsinterval van het gemiddelde.
    - rechter: De rechtergrens van het geloofwaardigheidsinterval van het gemiddelde. 


Posterior Verdelingen: 
- Voor elke factor en interactie worden de posterior verdelingen van het enkele model weergegeven. De posterior verdeling voor elk niveau kan ofwel in een grafiek, ofwel in verschillende grafieken voor elk niveau worden weergegeven. 

Q-Q Grafiek: 
Met de Q-Q grafiek kan de normaliteit van residuen visueel worden geïnspecteerd. De theoretische kwantielen staan op de x-as en de gestandaardiseerde residuen op de y-as. Hoe dichter de punten bij de diagonaal liggen, hoe meer bewijs dat de residuen normaal zijn verdeeld. 

Posterior R<sup>2</sup>: 
- De dichtheid van R<sup>2</sup> (verklaarde variantie) voor een enkel model, met R<sup>2</sup> op de x-as en dichtheid op de y-as.

#### Beschrijvende statistieken
Beschrijvende statistieken - afhankelijke variabele:
- Onafhankelijke variabele: De niveaus van de onafhankelijke variabele(n) die worden meegenomen in de analyse. Als het er meer dan één zijn, worden de beschrijvende statistieken weergegeven voor elke combinatie van niveaus van de onafhankelijke variabelen.
- Gemiddelde: Het gemiddelde per niveau, of, als er meerdere onafhankelijke variabelen zijn, het gemiddelde per combinatie van niveaus. 
- SD: De standaardafwijking.
- N: De steekproefgrootte.
  - % geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. DE standaardwaarde is 95%
    - Linker: De linkergrens van het geloofwaardigheidsinterval van het gemiddelde.
    - rechter: De rechtergrens van het geloofwaardigheidsinterval van het gemiddelde.

Beschrijvende grafiek: 
- Onafhankelijke variabele op de x-as en afhankelijke variabele op de y-as. ALs andere onafhankelijke variabelen worden meegenomen, kunnen verschillende lijnen in dezelfde grafiek de andere onafhankelijke variabele weergeven, of kunnen er verschillende grafieken worden gemaakt voor verschillende onafhankelijke variabelen. 

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
- ply
- stats
- stringi
- stringr
- utils

### Voorbeeld
---
- Zie voor een gedetailleerd voorbeeld van den Bergh et al. (2019).
- Ga voor een voorbeeld van de Bayesiaanse One-Way ANOVA naar `Bestand`-->'Data bibliotheek'-->`ANOVA`-->`Pain Thresholds`. 
- Ga voor een voorbeeld van de Bayesiaanse two-Way ANOVA naar `Bestand`-->'Data bibliotheek'-->`ANOVA`-->`Singers`. 
- Zie voor meer informatie over de laatste twee voorbeelden Wagenmakers et al. (2018). 


