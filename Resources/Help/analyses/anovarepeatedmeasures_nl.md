Herhaalde Metingen ANOVA
===

Met de herhaalde metingen ANOVA kan men verschillen tussen gemiddelden analyseren wanneer observaties afhankelijk zijn.

### Assumpties
- De afhankelijke variabele is normaal verdeeld voor elke groep.
- De covariaat en het experimentele effect zijn onafhankelijk.
- Er is aan de assumptie van sphericiteit voldaan. Dit houdt in dat varianties van de verschillen tussen de herhaalde meting condities gelijk zijn.

### Invoer
---

#### Invoerveld 
- binnen-proefpersoon factoren: De binnen-proefpersoon variabele. Hier kunt u de binnen-proefpersoon factoren en de verschillende niveaus die daartoe behoren labellen. 
- binnen-proefpersoon cellen: De aparte kolommen in de data die de niveaus van de binnen-proefpersoon factor(en) weergeven. 
- tussen-proefpersoon factoren: Deze variabele kan worden geselecteerd als de participanten in twee of meer groepen zijn ingedeeld. 
- Covariaten: In dit veld kunt u een covariaat selecteren. Covariaten zijn continue variabelen die een invloed op de afhankelijke variabele hebben maar geen deel zijn van de manipulatie.

#### Weergeven
- Beschrijvende statistieken: Als u deze optie selecteert, worden het gemiddelde, de standaardafwijking en de steekproefgrootte weergegeven voor iedere combinatie van niveaus van de onafhankelijke variabele.
- Schattingen van effectgrootte: Als u deze optie selecteert kunt u de specifieke type berekening van de effectgrootte selecteren. 
	  - &eta;<sup>2</sup> : Als u deze optie selecteert wordt de eta-kwadraat berekend als schatting van de effectgrootte. Deze methode overschat echter de populatie variantie, wat het moeilijk maakt om het effect van dezelfde variabele te vergelijken tussen verschillende onderzoeken (Goss-Sampson, 2018).       
	  - partial &eta;<sup>2</sup> : Als u deze optie selecteert wordt de gedeeltelijke eta-kwadraat berekend als schatting van de effectgrootte. Deze methode lost het probleem van overschatting van populatievariantie op, wat het makkelijker maakt om het effect van dezelfde variabele te vergelijken tussen verschillende onderzoeken (Goss-Sampson, 2018).
    - &omega;<sup>2</sup> : Als u deze optie selecteert wordt de omega-kwadraat uitgerekend als schatting van de effectgrootte. Dit wordt gezien als een goede schatter bij een kleine steekproefgrootte (Goss-Sampson, 2018).
- Vovk-Selke maximum p-ratio: De grens 1/(-e p log(p)) wordt afgeleid van de vorm van de verdeling van de p-waarden. Onder de nul hypotheses (H<sub>0</sub>)  is het uniform (0,1) en onder de alternatieve hypothese (H<sub>1</sub>) neemt hij af in p, bijv. een beta (α, 1) verdeling waar 0 < α < 1. De Vovk-Selke MPR wordt verkregen door het vorm van α onder de alternatieve hypothese te kiezen zodat de p-waarde maximaal diagnostisch is. De waarde is dat de ratio van de dichtheid op punt p onder H<sub>0</sub> en H<sub>1</sub>. Als de tweezijdige p-waarde bijvoorbeeld .05 is is de Vovk-Sellke MPR 2.46. Dit geeft aan dat deze p-waarde maximaal 2.46 zo waarschijnlijk is onder H1 dan onder H<sub>0</sub>. Meer informatie vind u in deze <a href="https://jasp-stats.org/2017/06/12/mysterious-vs-mpr/">blogpost</a>.


### Model
- Componenten en model termen:
  - binnen-proefpersoon componenten: Alle binnen-proefpersoon factoren en covariaten die kunne worden meegenomen in het model. 
	  - model termen: De binnen-proefpersoon factoren en covariaten die meegenomen kunnen worden.
  - tussen-proefpersoon componenten: Alle tussen-proefpersoon factoren die kunnen worden meegenomen in het model.
	  - model termen: De tussen-proefpersoon factoren die kunnen worden meegenomen in het model.

- Kwadratensom: Er zijn verschillende soorten de kwadratensom. De keuze van het type is belangrijk als er meerdere factoren zijn en de data ongebalanceerd is. In een ongebalanceerd design hebben de verschillende niveaus van de onafhankelijke variabele niet evenveel observaties (bijv. als een groep meer observaties heeft dan de ander). In dit scenario kan het type kwadratensom de resultaten beïnvloeden.   
    - Type I: Sequentiële kwadratensom. Het verminderen van fouten wanneer elke factor van het model wordt toegevoegd aan de factoren die al mee worden genomen, hiermee wordt de volgorde van factoren in het model behouden. Het resultaat hangt af van de volgorde waarin de factoren aan het model worden toegevoegd. Wanneer het model meer dan een factor bevat is het belangrijk hierover na te denken. 
    - Type II: Hiërarchisch/gedeeltelijke sequentiële kwadratensom. Het verminderen van fouten wanneer elke factor wordt toegevoegd aan het model dat alle andere factoren bevat, behalve de factoren waar de factor deel van uitmaakt, bijvoorbeeld interacties die deze factor bevatten. Langsrud (2003) adviseert dit type bij een ANOVA met on gebalanceerde data. 
    - Type III: Gedeeltelijke kwadratensom. Het verminderen van fouten wanneer elke factor wordt toegevoegd aan het model dat alle andere factoren bevat, inclusief interacties met deze factor. Dit type wordt vaak geselecteerd, omdat het rekening houdt met interacties (Langsrud, 2003). Dit is de standaardoptie. 
- Gebruik multivariaat model voor vervolgtoetsen: Als u deze optie selecteert wordt het multivariate lineaire model gebruikt in plaats van het aov model voor vervolgtoetsen (contrasten, post-hoc toetsen, marginale gemiddelden). Multivariate toetsen passen waarschijnlijk een betere correctie toe voor schendingen van de sphericiteit assumptie. 

### Verificatie van aannames
- Sphericiteit toetsen: Sphericiteit houdt in dat de varianties van de verschillen tussen condities van de herhaalde meting gelijk zijn.
- Sphericiteit correcties: Als er niet aan de assumptie van sphericiteit is voldaan is de kans op een type 1 fout hoger. Correcties voor sphericiteit zijn een manier om dit te verminderen.
  - Geen: Er wordt geen correctie uitgevoerd.
  - Greenhouse-Geisser: Deze correctie varieert tussen 1/(k − 1) en 1, waar k staat voor het aantal herhaalde metingen.
  - Huyn-Feldt: Een andere populaire correctie om de vrijheidsgraden te corrigeren.
  
- Homogeniteits toets: Als u deze optie selecteert zal worden geanalyseerd of de variantie van de afhankelijke variabele gelijk is tussen groepen door middel van Levene's toets. 

### Contrasten
Voor elke binnen-proefpersoon factor kan een specifiek contrast worden geselecteerd door op "geen" te klikken in de rechter kolom.
- Factoren: Dit zijn de binnen-proefpersoon factoren in de analyse. 
- Contrasten: Contrasten maken geplande vergelijkingen mogelijk. Er zijn verschillende contrasten die verschillende soorten vergelijkingen mogelijk maken.  
- Geen: Als u deze optie selecteert worden er geen contrasten uitgerekend. Dit is de standaardoptie. 
    - afwijking: Als u dit contrast selecteert wordt het gemiddelde van elk niveau van de onafhankelijke variabele vergeleken met het totale gemiddelde (het gemiddelde wanneer alle niveaus samen worden genomen).
    - enkelvoudig: Als u dit contrast selecteert wordt het gemiddelde van ieder niveau vergeleken met het gemiddelde van een gespecificeerd niveau, bijvoorbeeld met het gemiddelde van de controlegroep.
    - verschil: Dit contrast wordt ook wel "reverse Helmert" genoemd. Als u dit contrast selecteert wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van het vorige niveau.
    - Helmert: Als u dit contrast selecteert wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van het volgende niveau. Dit is het omgekeerde van het "verschil" contrast.
    - herhaald: Als u dit contrast selecteert wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van het volgende niveau.
    - polynoom: Dit contrast test polynome trends in de data. Welk specifieke polynoom wordt gebruikt is afhankelijk van het aantal niveaus van de onafhankelijke variabele. De graad van de trend die wordt gebruikt is het aantal niveaus min 1. Als de onafhankelijke variabele dus bestaat uit twee niveaus wordt een lineaire trend geanalyseerd. Als de onafhankelijke variabele bestaat uit drie niveaus worden een kwadratische trend en een lineaire trend geanalyseerd.
    - handmatig: Hier kunnen de contrast gewichten handmatig ingevoerd worden. Van de gewichten dient minstens 1 gewicht niet gelijk aan 0 te zijn.
   
   

### Post-hoc Toetsen
Sleep een of meer namen van factoren naar de rechter kolom om een post-hoc test uit te voeren. Er zijn verschillende opties: 
- effectgrootte: Als u deze optie selecteert wordt de effectgrootte weergegeven (i.e., de grootte van het geobserveerde effect). De gebruikte maat voor de effectgrootte is Cohen's d. De effectgrootte wordt alleen weergegeven voor het post-hoc type `Standaard`.
- betrouwbaarheidsintervallen: Als u deze optie selecteert wordt er een betrouwbaarheidsinterval voor het gemiddelde verschil berekend. Dit wordt voor welke post-hoc methode gedaan, behalve voor Dunn.  De standaardoptie is een interval van 95%. Dit kan tot het gewenste percentage worden aangepast.
- samengevoegde foutterm voor binnen-proefpersoon factoren: een samengevoegde foutterm die aanneemt dat de varianties van de contrast scores ongeveer gelijk zijn (i.e., assumptie van sphericiteit) zie Morey (2008) voor details. 
  - Correctie: Om te corrigeren voor meerdere vergelijkingen en type 1 fouten te voorkomen bestaan er verschillende methoden om de p waarde te corrigeren: 
	  - Tukey: Vergelijk alle mogelijke paren van groepsgemiddelden. Deze correctie kan worden gebruikt wanneer de niveaus op de onafhankelijke variabele een gelijke steekproefgrootte en variantie hebben. Deze methode wordt veel gebruikt en is de standaardoptie.
      - Scheffe: Het aanpassen van significatieniveaus in een lineare regressie om rekening te houden met meerdere vergelijkingen. Deze methode is vrij conservatief.
      - Bonferroni: Deze correctie wordt gezien als vrij conservatief. Het risico op een type 1 fout wordt verminderd, maar statistische kracht (power) wordt ook lager.
      - Holm: Deze methode wordt ook wel sequentiële Bonferroni genoemd, en wordt gezien als minder conservatief dan de Bonferronimethode.

### Beschrijvende grafieken 
- Selecteer de onafhankelijke variabele op de horizontale as om een beschrijvende grafiek te maken. Als er meerdere onafhankelijke variabelen zijn kunnen de variabelen in een plot worden weergegeven door de andere variabele in het veld Aparte lijnen te zetten. De variabelen kunnen ook in aparte grafieken worden weergegeven door de andere variabele in het veld Aparte grafieken te zetten.
  - Factoren: De onafhankelijke variabelen die mee worden genomen in de analyse
  - Horizontale as: Selecteer de onafhankelijke variabele die op de horizontale as wordt weergegeven.
  - Aparte lijnen: Door een onafhankelijke variabele in dit veld te plaatsen corresponderen verschillende lijnen met verschillende niveaus van de geselecteerde variabele.
  - Aparte grafieken: Door een onafhankelijke variabele in deze box te plaatsen corresponderen verschillende grafieken met verschillende niveaus van de geselecteerde variabele.
  - label y-as: Het label van de y-as kan handmatig worden bijgesteld.

- weergeven:
	- Geef foutmarges weer: Als u deze optie selecteert worden er foutmarges weergegeven in de grafiek. De foutmarges kunnen ofwel betrouwbaarheidsintervallen ofwel standaardfouten weergeven. Om precieze intervallen of standaardfouten te krijgen wordt de data genormaliseerd door de gemiddelde van elke observatie af te trekken en dan het totale gemiddelde aan elke observatie op te tellen. De varianties van de resulterende genormaliseerde waarden in elke conditie, en daardoor de grootte van de foutmarges, hangt niet langer af van de participant effecten en zijn zo een meer accurate representatie van de experimentele manipulatie. Zie Morw (2008) voor meer detail.
		- betrouwbaarheidsinterval: Dit is de standaardoptie. Met deze optie geven de foutmarges betrouwbaarheidsintervallen van het gemiddelde van elke combinatie van onafhankelijke variabelen weer. De standaardoptie is een interval van 95%. Dit kan tot het gewenste percentage worden aangepast.
        - Standaardfout: Als u deze optie selecteert geven de foutmarges de standaardfouten van de gemiddelden van elke combinatie van niveaus van de onafhankelijke variabele weer. 
    - Gemiddelde over ongebruikte binnen-proefpersoon factoren: Wanneer er meerdere binnen-proefpersoon factoren in het model zijn maar u slechts één van de factoren plot wordt het gemiddelde over de ongebruikte binnen-proefpersoon factoren genomen. Als er bijvoorbeeld twee binnen-proefpersoon factoren zijn met twee niveaus, A(1&2) en B (1&2), en alleen A geselecteerd wordt voor de grafiek wordt het gemiddelde van B over de niveaus genomen. Dit betekent dat wanneer het gemiddelde van A1 wordt geplot, het eigenlijk het gemiddelde is van A1B1 en A1B2. Deze procedure wordt beschreven in Loftus & Masson (1994). Wanneer dit veld niet is aangevinkt worden de gemiddelden niet genomen en worden de kolommen A1B1 en A1B2 simpelweg geconcateneerd.
  
### Marginal Gemiddelden
- Marginale gemiddelden: Als u deze optie selecteert wordt het gemiddelde van elk niveau van de onafhankelijke variabele gecorrigeerd voor alle andere variabelen in het model. 
- Van `...` bootstraps: Als u deze optie selecteert worden de ge-bootstrapte marginale gemiddelde berekend. De standaard hoeveelheid iteraties is 1000. U kunt dit aanpassen tot het gewenste aantal.
- Vergelijk marginale gemiddelden met 0: Als u deze optie selecteert worden de gecorrigeerde gemiddelden vergeleken met 0 en worden de betrouwbaarheidsintervallen voor de gecorrigeerde gemiddelden berekend.  
	  - betrouwbaarheidsinterval correctie: De betrouwbaarheidsintervallen kunnen op verschillende manieren worden aangepast.
		    - Geen: Als u deze optie selecteert wordt er geen correctie gedaan. 
        - Bonferroni: Bonferroni correctie voor de betrouwbaarheidsintervallen.
        - Sidak: Sidak correctie voor de betrouwbaarheidsintervallen.

### Hoofdeffecten:
De hoofdeffecten geven het effect van een binnen-proefpersoon factor voor elk niveau van de andere binnen-proefpersoon factor door een ANOVA uit te voeren voor elke subset van de data zoals gespecificeerd door de moderator variabele.
  - Factoren: Dit veld bevat alle binnen-proefpersoon variabelen die worden meegenomen in de analyse.
  - Simpel effect factor: In dit veld selecteert u de binnen-proefpersoon variabelen om het effect van deze variabele geconditioneerd op de niveaus van de moderator te berekenen. 
  - Moderator factor 1: In dit veld selecteert u de binnen-proefpersoon variabele die de verschillende niveaus weergeeft. 
  - Moderator factor 2: In dit veld kunt u optioneel een aanvullende binnen-proefpersoon variabele toevoegen.

- Samengevoegde error term: Een samengevoegde error term maakt de assumptie dat de varianties van de contrastscores ongeveer gelijk zijn (i.e., assumptie sphericiteit).

#### Verdelingsvrije toetsen
De Friedman toets is een niet parametrisch alternatief voor de herhaalde metingen ANOVA wanneer er een compleet blok design is. The Durbin toets wordt automatisch geselecteerd wanneer er een incompleet blok design is. 
- Factoren: Dit veld bevat alle binnen-proefpersoon factoren uit de analyse.
- BP factor: De binnen-proefpersoon factor waarin u geïnteresseerd bent.
- Tussen factor (optioneel): Het is mogelijk om hier een tussen-persoon factor toe te voegen.
- Conover's post-hoc toetsen: Conover's post-hoc toets voor paar wijze vergelijkingen, wanneer de verdelingsvrije toets statistiek significant is.


### Uitvoer
---
#### Herhaalde Metingen ANOVA
binnen-proefpersoon Effecten:
- Spericiteit correctie: De geselecteerde correcties wanneer de assumptie van sphericiteit is geschonden.
- kwadratensom: De opgetelde kwadraten van binnen groep-gemiddelden verschillen. 
- vg: vrijheidsgraden.
- Mean square: Schatting van de populatie variantie (de dum of squares gedeeld door de vrijheidsgraden).
- F: De waarde van de F statistiek.
- P: De p-waarde.

tussen-proefpersoon Effecten:
- kwadratensom: De opgetelde kwadraten van tussen groep-gemiddelden verschillen. 
- vg: Vrijheidsgraden.
- Mean Square: Schatting van de populatievariantie (de sum of squares gedeeld door de vrijheidsgraden).
- F: De waarde van de F statistiek.
- P: De p-waarde.

#### Verificatie van aannames
Toets van sphericiteit:
- Mauchly's W: Mauchly's W toets statistiek 
- p: p-waarde.
- Greenhouse-Geisser &epsilon;: De Greenhouse-Geisser correctie. Een waarde van 1 geeft aan dat er aan de assumptie sphericiteit is voldaan en een waarde < 1 geeft aan dat sphericiteit geschonden.
- Huynh-Feldt &epsilon;: De Huynh-Feldt correctie.

#### Contrasten
Afwijking/Enkelvoudig/Verschil/Helmert/Herhaald/Polynoom/Handmatig Contrast:
- Vergelijking: De niveaus van de onafhankelijke variabele die worden vergeleken. Voor de handmatige contrasten wordt het gewicht getoond voor elk niveau van de factor.
- Schatting: Het geschatte gemiddelde verschil tussen de vergeleken niveaus.
- Std. Fout: De standaardfout van het geschatte gemiddelde.
- vg: De vrijheidsgraden van het model.
- t: De waarde van de t-statistiek.
- p: De p-waarde.


#### Post-Hoc Toetsen
Post-Hoc Vergelijkingen:  
- De eerste twee kolommen geven de niveaus van de onafhankelijke variabele die met elkaar worden vergeleken.
- Gemiddelde verschil: Het gemiddelde verschil tussen de niveaus.
- % BI voor gemiddelde verschil: het betrouwbaarheidsinterval voor het gemiddelde verschil tussen de vergeleken niveaus. 95% is de standaardoptie.
	- Onder: De ondergrens van het betrouwbaarheidsinterval.
    - Boven: De bovengrens van het betrouwbaarheidsinterval.

- Std. Fout: De standaardfout van het gemiddelde verschil.	
- t: De waarde van de t statistiek.
- Cohen's d: De effectgrootte Cohen's d. Deze corrigeert niet voor meerdere vergelijkingen.
- p<sub>tukey</sub>: Tukey's gecorrigeerde p-waarde voor meerdere vergelijkingen.
- p<sub>scheffe</sub>:  Scheffe's gecorrigeerde p-waarde voor meerdere vergelijkingen.
- p<sub>bonf</sub>: Bonferroni's gecorrigeerde p-waarde voor meerdere vergelijkingen.
- p<sub>holm</sub>: Holm's gecorrigeerde p-waarde voor meerdere vergelijkingen. 

Hoofdeffecten:
- Niveau: De niveaus op de onafhankelijke variabele die worden vergeleken. 
- kwadratensom: De som van kwadraten tussen niveau-gemiddelden verschil.
- vg: De vrijheidsgraden van het model.
- Mean Square: Schatting van de populatievariantie (de sum of squares gedeeld door de vrijheidsgraden).
- F: De waarde van de F statistiek.
- P: De p-waarde.

#### Marginale gemiddelden
Marginale gemiddelden - onafhankelijke variabele: 
- De eerste kolom bevat de niveaus van de onafhankelijke variabele.
- Marginale gemiddelde: Het marginale gemiddelde voor elk niveau van de onafhankelijke variabele. Dit gemiddelde is gecorrigeerd voor alle andere variabelen in het model. 
- Std. Fout: De standaardfout van het marginale gemiddelde.
- Onder BI: De ondergrens van het betrouwbaarheidsinterval.
- Boven BI: De bovengrens van het betrouwbaarheidsinterval.
- t: De waarde van de t-statistiek.
- p: De p-waarde.


#### Marginale gemiddelde door middel van Bootstrapping
Ge-bootstrapte marginale gemiddelden - binnen-proefpersoon variabele: 
- binnen-proefpersoon variabele: Deze kolom bevat alle niveaus van de binnen-proefpersoon variabele.
- Marginale gemiddelde: Schatting van het marginale gemiddelde voor elk niveau van de binnen-proefpersoon variabele. Dit gemiddelde is gecorrigeerd voor alle andere variabelen in het model. De schatting is gebaseerd op de bootstrap verdeling.
- Bias: Bias: Het gemiddelde verschil tussen het ge-bootstrapte marginale gemiddelde en het geschatte marginale gemiddelde.
- Std. Fout: De standaardfout van de ge-Bootstrapte marginale gemiddelden. 
- 95% BI voor gemiddelde verschil: De voor bias-gecorrigeerde betrouwbaarheidsinterval voor het gemiddelde verschil tussen de vergeleken niveaus. De standaardoptie is 95%.
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.

#### Verdelingsvrije toetsen
Friedman Toets / Durbin Toets:
- Factor: The binnen-proefpersoon factor van de analyse.
- Chi-kwadraat: De chi-squared toets statistiek.
- vg: vrijheidsgraden.
- p: De p-waarde.
- Kendall's W: Kendall’s W Test kan worden gezien als normalisatie van de Friedman/Durbin statistiek.
- F: De waarde van de F statistiek.
- vg teller: Vrijheidsgraden voor het bepalen van p-waarden van de F-statistieken.
- vg noemer: Vrijheidsgraden voor het bepalen van p-waarden van de F-statistieken.
- p<sub>f</sub>: De p-waarde van de F-statistiek.

Conover's post-hoc Vergelijkingen:
- De eerste twee kolommen representeren de niveaus van de binnen-proefpersoon factor die worden vergeleken.
  - T-stat: De toets statistiek die de t-verdeling volgt.
- vg: Vrijheidsgraden.
- W<sub>i</sub>: Som van de opgetelde rangen van niveau 1.
- W<sub>j</sub>: Som van de opgetelde rangen van niveau 2.
- p: De p-waarde.
- p<sub>bonf</sub>: Bonferroni gecorrigeerde p-waarde voor meerdere vergelijkingen.
- p<sub>holm</sub>: Holm's Gecorrigeerde p-waarde voor meerdere vergelijkingen. 

#### Beschrijvende grafieken 
De onafhankelijke variabele op de x-as en afhankelijke variabele op de y-as. Als er meerdere binnen-proefpersoon factoren worden meegenomen in de analyse kunnen deze ofwel met verschillende lijnen, ofwel in verschillende grafieken worden weergegeven.

### Referenties
---
- Conover,W. J. (1999). *Practical nonparametric Statistics, 3rd. Edition*, Wiley.
- Morey, R. D. (2008) Confidence intervallen from Normalized Data: A correction to Cousineau (2005). Tutorial in *Quantatitative Methods for Psychology, 4*(2), 61-64.
- Loftus, G. R., & Masson, M. E. J. (1994). Using confidence intervallen in within-subject designs. *Psychonomic Bulletin and Review, 1*, 476–490.

### R-packages
---
- afex
- boot
- emmeans
- ggplot2
- plyr
- stats
