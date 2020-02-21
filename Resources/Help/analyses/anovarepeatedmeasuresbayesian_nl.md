Bayesiaanse Herhaalde Metingen ANOVA
===

Met de Bayesiaanse Herhaalde Metingen ANOVA kan men verschillen tussen gemiddelden analyseren waar observaties afhankelijk zijn.

### Assumpties 
- De afhankelijke variabele is normaal verdeeld voor elke groep.
- De covariaat en het experimentele effect zijn onafhankelijk.
- Er is aan de assumptie van sphericiteit voldaan. Dit houdt in dat varianties van de verschillen tussen de condities van de herhaalde metingen gelijk zijn.

### Invoer
---

#### Invoerveld
- Herhaaldelijke meting factoren: De binnen-subject variabele. Hier kan je de binnen-subject factoren en de verschillende niveaus die daartoe behoren labellen. 
- Herhaaldelijke meting cellen: De aparte kolommen in de data die de niveaus van de binnen-subect factor(en) weergeven. 
- Tussen subject factoren: Deze variabele kan worden geselecteerd als de participanten in twee of meer groepen zijn ingedeeld. 
- Covariaten: In dit veld kan je een covariaat selecteren. Covariaten zijn continue variabelen die een invloed op de afhankelijke variabele hebben maar geen deel zijn van de manipulatie.

#### Bayes Factor
- BF<sub>10</sub>: Als je deze optie selecteert geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nul hypothese. Dit is de standaardoptie. 
- BF<sub>01</sub>: Als je deze optie selecteert geeft de Bayes factor bewijs voor de nul hypothese ten opzichte van de alternatieve hypothese. Dit is gelijk aan 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>): Als je deze optie selecteert wordt het natuurlijke logaritme <sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> weergegeven in de output.

#### Volgorde:
- Vergelijk met het nulmodel: Do modellen gemaakt onder de optie `Model` worden vergeleken met het model dat het totale gemiddelde en de aselecte factoren bevatten, het nulmodel genoemd. Dit is de standaardoptie. 
- Vergelijk met het beste model: De modellen gemaakt onder de optie `Model`worden vergelen met het beste model in de analyse.

#### Tabellen
- Effecten: Als je deze optie selecteert wordt het effect van elk component van het model berekend.
	- Over alle modellen: Als je deze optie selecteert wordt elk model waar de component in zit gebruikt om het effect van de component te berekenen. Als de optie `Effecten` is geselecteerd is dit de standaardoptie.
    - Over alle gematchte modellen: Als je deze optie selecteert wordt elk model met precies die component meegenomen in de analyse. Interacties met de component worden dus uitgesloten.
- Schattingen: Als je deze optie selecteert wordt een tabel met een samenvatting van de model gemiddelde posterior weergegeven. Deze tabel bevat informatie over het model gemiddelde gemiddelde voor elk niveau van de factor en hun interacties, de standaardafwijking en geloofwaardigheidsinterval.    
- Beschrijvende statistieken: Als deze optie is geselecteerd worden het gemiddelde, de standaardafwijking en de steekproefgrootte weergegeven voor elke combinatie van niveaus van de onafhankelijke variabele.
  - Geloofwaardigheidsinterval: De standaardoptie is 95%.

#### Grafieken
- Model gemiddelde posteriors: Als je deze optie selecteert worden er grafieks weergegeven die de model gemiddelde posterior van elke vaste factor en interactie illustreren. 
  - Groep niveaus in één grafiek: Als je deze optie selecteert wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
  - Een grafiek per niveau : Als je deze optie selecteert wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek weer worden gegeven.
- Q-Q grafiek van residuen: Controleert de validiteit van de verdelingsassumpties van de dataset. Om precies te zijn geeft de grafiek weer of de residuen normaal zijn verdeelt. 
- Posterior R<sup>2</sup>: Als je deze optie selecteert wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).

### Model
- Componenten: Alle onafhankelijke variabelen die worden meegenomen in het model.
- Modeltermen: De onafhankelijke variabelen die worden meegenomen in het model. Alle vaste factoren staan standaard in dit veld.
- Toevoegen aan nulmodel: De onafhankelijke variabelen die worden meegenomen in het model kunnen worden geselecteerd om aan het nulmodel toe te voegen.

### Enkel Model Inferentie
- Hier kan een enkel model worden gespecificeerd om informatie van de posterior van dit model te verkrijgen, inclusief een tabel met een samenvatting van de posterior en grafieken van de marginale posterior.
- Tabellen:
  - Schatting: Een tabel met een samenvatting van de posterior voor het enkele model dat is gespecificeerd in het toekennignsveld. Deze tabel geeft informatie over het gemiddelde, de standaardafwijking en het betrouwbaarheidsinterval van elk niveau van de vaste factoren van het model. Dit is anders dan de "schatten" optie in Output, omdat de "estimate" functie een samenvatting geeft van de posterior gemiddeld over alle modellen in de analyse, terwijl deze optie de posterior geeft voor het enkele model. 
 
- Grafieken
  - Marginale posteriors: Als je deze optie selecteert worden er grafieken gegenereerd die de posterior verdeling van elke vaste factor van het model en hun interactie weergeven. 
	- Groep niveaus in één grafiek: Als je deze optie selecteert wordt er voor elke factor een grafiek weergegeven. De posterior verdelingen van elk niveau van de factor zullen dus in dezelfde grafiek worden weergegeven.
	- Een grafiek per niveau : Als je deze optie selecteert wordt een aparte grafiek voor elk niveau van de factor weergegeven. De posterior verdeling van elke factor zal dus in een eigen grafiek weer worden gegeven.
  - Q-Q grafiek van residuen: Controleert de validiteit van de verdelings assumpties van de dataset. Om precies te zijn geeft de grafiek weer of de residuen normaal zijn verdeeld. 
  - Posterior R<sup>2</sup>: Als je deze optie selecteert wordt een grafiek weergegeven van de posterior verdeling van R<sup>2</sup> (i.e., de verklaarde variantie).
- Toekennings veld: Hier wordt het enkele model gespecificeerd.
	- Componenten: Dit veld bevat alle factoren in het model. 
    - Specifieke model termen: Selecteer de factoren die meegenomen moeten worden in het model.

### Post-Hoc Toetsen
- Sleep de naam van een factor naar de rechter kolom om een post-hoc toets uit te voeren. Daar kan je selecteren:
- Correctie:
	- Nulcontrole: Als je deze optie selecteert worden de prior odds gecorrigeerd voor meervoudig toetsen. Dit is de standaardoptie. Momenteel wordt er geen uitvoer gegenereerd als deze optie niet geselecteerd is.


### Beschrijvende Grafieken
- Selecteer de onafhankelijke variabele op de horizontale as om een beschrijvende grafiek te maken. Als er meerdere onafhankelijke variabelen zijn kunnen de variabelen in een grafiek worden weergegeven door de andere variabele in het veld Aparte lijnen te zetten. De variabelen kunnen ook in aparte grafieken worden weergegeven door de andere variabele in het veld Aparte grafieken te zetten.
  - Factoren: De onafhankelijke variabelen die mee worden genomen in de analyse.
  - Horizontale as: Selecteer de onafhankelijke variabele die op de horizontale as wordt weergegeven.
  - Aparte lijnen: Door een onafhankelijke variabele in deze box te plaatsen corresponderen verschillende lijnen met verschillende niveaus van de geselecteerde variabele.
  - Aparte grafieken: Door een onafhankelijke variabele in deze box te plaatsen corresponderen verschillende grafieken met verschillende niveaus van de geselecteerde variabele.
  - Label de y-as: Het label van de y-as kan je handmatig aanpassen.
- Geef weer:
  - geloofwaardigheidsinterval: Als je deze optie selecteert bevat de grafiek geloofwaardigheidsintervallen. De standaardoptie is 95%. Dit kan tot behoeven worden aangepast.


### Aanvullende opties 
- Prior: Hier kan je de prior verdelingen voor de aselecte en vaste effectgroottes bepalen.
	- r schaal vaste effecten: De vorm parameter van de prior verdeling voor vaste effecten. De standaardwaarde is .5, maar je kan dit naar behoeven aanpassen.
	- r schaal aselecte effecten: De vorm parameter van de prior verdeling voor de aselecte effecten. De standaardwaarde is 1, maar je kan dit naar behoeven aanpassen. 
- Numerische precisie: Het aantal stappen dat wordt genomen om de integraal voor de Bayes factor te berekenen.
  - Auto: Als je deze optie selecteert worden er 10000 stappen gezet. Dit is de standaardoptie.
  - Handmatig: Als je deze optie selecteert kan je handmatig het aantal stappen selecteren. De standaardwaarde is 10000.
- Posterior steekproeven: Het is mogelijk om het aantal Markov Chain Monte Carlo steekproeven in te stellen. Dit wordt gebruikt om de posterior verdeling en het fout % te benaderen. 
	- Auto: Als deze optie is geselecteerd worden er 10000 steekproeven gebruikt. dit is de standaardoptie.
    - Handmatig: Als je deze optie selecteert kan je handmatig het aantal steekproeven instellen. Als je deze optie kiest is de standaardwaarde 1000.
- Reproduceerbaarheid:
  - Gebruik seed: Geeft de mogelijkheid een seed te gebruiken voor je analyse. Een seed gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

### Uitvoer
---
Model vergelijking - Afhankelijke variabele: 
- Modellen: De eerste kolom bevat alle modellen die worden meegenomen in de analyse.
	- Nulmodel: Dit model bevat het totale gemiddelde en de willekeurige factoren.
    - Onafhankelijke variabele model: Dit model voegt het effect van de onafhankelijke variabele toe.
- P(M): Deze kolom bevat de prior kans van het model. 
- P(M|data): Deze kolom bevat de geüpdate kans op het model gegeven de data. Dit heet de posterior kans. 
- BF<sub>M</sub> : Deze kolom bevat de posterior model odds. Dit is de verandering van de prior odds naar de posterior odds van het model. 
- BF<sub>10</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese geeft. Echter, als de optie "vergelijk met het beste model" is geselecteerd bevat de kolom de Bayes factor die bewijs voor dit model ten opzichte van het beste model geeft. 
- BF<sub>01</sub> : Deze kolom bevat de Bayes factor die het bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese geeft. Echter, als de optie "vergelijk met het beste model" is geselecteerd bevat de kolom de Bayes factor die bewijs voor het beste model ten opzichte van dit model geeft. 
- fout %: De fout van de Gaussiaanse kwadratuur integratie routine die het "Bayes Factor" package gebruikt voor het berekenen van de Bayes Factor.

Analyse van effecten - Afhankelijke variabele:
- Effecten: Deze kolom bevat de componenten meegenomen in de modellen, zoals de onafhankelijke variabelen en hun interacties.
- P(incl): Deze kolom bevat de prior inclusie kans. Dit is de opgetelde prior kans over alle modellen die de component bevatten.
- P(incl|data): Deze kolom bevat de posterior inclusiekant. Dit is de opgetelde posterior kans over alle modellen die de component bevatten.
- BF<sub>inclusion</sub> : Deze kolom bevat de verandering van prior inclusie odds tot posterior inclusie odds voor elke component gemiddeld over alle modellen die de component bevatten.

Model Gemiddelde samenvatting van de posterior:
- Variabele: Deze kolom bevat alle vaste factoren en hun interacties. De eerste rij geeft informatie over het intercept. 
- Niveau: Elk niveau van de factor en combinatie van niveaus van de interacties in het model.
- Gemiddelde: Het model gemiddelde gemiddelde. Voor de factoren is dit de afwijking van het intercept voor elk niveau van de factor. De niveaugemiddelden voor een factoren tellen op tot 0. 
- SD: De standaardafwijking van het modelgemiddelde gemiddelde. 
- % geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
  - Linker: De linkergrens van het geloofwaardigheidsinterval van het gemiddelde.
  - rechter: De rechtergrens van het geloofwaardigheidsinterval van het gemiddelde.

#### Model gemiddelde posterior verdelingen
Voor elke factor en interactie worden de modelgemiddelde posterior verdelingen per niveau weergegeven met de factor op de x-as en dichtheid op de y-as. De posterior verdeling val elk niveau ken in dezelfde grafiek of in een andere grafiek worden weergegeven. 

Model gemiddelde Q-Q grafiek:
- Met de Q-Q grafiek kan de normaliteit van de residuen visueel worden geïnspecteerd. De theoretische kwantielen staan op de x-as en de gestandaardiseerde residuen op de y-as. Hoe meer stippen op de diagonaal, hoe meer de residuen normaal verdeeld zijn.

Model gemiddelde Posterior R<sup>2</sup>:
- De model gemiddelde dichtheid van de R<sup>2</sup> (verklaarde variantie), met de R<sup>2</sup>: op de x-as en dichtheid op de y-as. 

#### Post-Hoc Toetsen:
Post-hoc vergelijkingen - onafhankelijke variabele:
	- De eerste kolommen bevatten die niveaus van de onafhankelijke variabelen die worden vergeleken. 
	- Deze kolom bevat de prior odds. Deze zijn gecorrigeerd voor meervoudig toetsen (Westfall, Johnson, & Utts, 1997)
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
  - % geloofwaardigheidsinterval: Het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%
    - Linker: De linker grens van het geloofwaardigheidsinterval van het gemiddelde.
    - rechter: De rechter grens van het geloofwaardigheidsinterval van het gemiddelde. 


Posterior Verdelingen: 
- Voor elke factor en interactie worden de posterior verdelingen van het enkele model weergegeven. De posterior verdeling voor elk niveau kan ofwel in een grafiek, ofwel in verschillende grafieken voor elk niveau worden weergegeven. 

Q-Q Grafiek: 
Met de Q-Q grafiek kan de normaliteit van residuen visueel worden geïnspecteerd. De theoretische kwantielen staat op de x-as en de gestandaardiseerde residuen op de y-as. Hoe dichter de punten bij de diagonaal liggen, hoe meer bewijs dat de residuen normaal zijn verdeeld. 

Posterior R<sup>2</sup>: 
- De dichtheid van R<sup>2</sup> (verklaarde variantie) voor een enkel model, met R<sup>2</sup> op de x-as en dichtheid op de y-as.

#### Beschrijvende statistieken: 
Afhankelijke variabele: 
  - Herhaaldelijk meten factor: De niveaus van het binnen-subect factor(en) van het model. Als het er meerdere zijn worden de beschijvende statistieken voor elke combinatie van niveaus van de factor weergegeven.
  - Gemiddelde: Het gemiddelde niveau, of, als er meerdere binnen-subject factoren zijn, het gemiddelde niveau per combinatie van niveaus.
  - SD: De standaardafwijking. 
  - N: De steekproefgrootte.
  - % geloofwaardigheidsinterval: het geloofwaardigheidsinterval van het gemiddelde. De standaardoptie is 95%.
    - Linker: De linkergrens van het geloofwaardigheidsinterval van het gemiddelde.
    - Rechter: De rechtergrens van het geloofwaardigheidsinterval van het gemiddelde. 

#### Beschijvende grafieken
Beschrijvende grafiek: 
- Onafhankelijke variabele op de x-as en afhankelijke variabele op de y-as. Als andere onafhankelijke variabelen worden meegenomen kunnen verschillende lijnen in dezelfde grafiek de andere onafhankelijke variabele weergeven, of kunnen er verschillende grafieken worden gemaakt voor verschillende onafhankelijke variabelen. 

### Referenties
---
- Rouder, J. N., Engelhardt C. R., McCabe S., & Morey R. D. (2016). Model comparison in ANOVA. *Psychonomic Bulletin and Review, 23*, 1779-1786.
- Rouder, J.N., Morey R.D., Speckman P.L., & Province J M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*, 356-374.
- Rouder, J. N., Morey, R. D., Verhagen, A. J., Swagman, A. R., & Wagenmakers, E.-J. (2017). Bayesian analysis of factorial designs. *Psychological Methods, 22*, 304-321.
- Van den Bergh, D., Van Doorn, J., Marsman, M., Draws, T., Van Kesteren, E.J., ... & Wagenmakers, E.-J. (2019) A Tutorial on Conducting and Interpreting a Bayesian ANOVA in JASP. Manuscript submitted for publication.
- Wagenmakers, E. J., Love, J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., ... & Meerhoff, F. (2018). Bayesian inference for psychology. Part II: Example applications with JASP. *Psychonomic bulletin & review, 25*(1), 58-76.
- Wetzels, R., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for ANOVA designs. *The American Statistician, 66*, 104-111.

### R Packages
---
- BayesFactor
- colorspace
- gggrafiek2
- KernSmooth
- matrixStats
- plyr
- stats
- stringi
- stringr
- utils
