ANOVA 
=== 

Met een ANOVA berekent men het verschil tussen meerdere groepsgemiddelden. 

### Assumpties
- De residuen zijn voor iedere groep normaal verdeeld.
- De onafhankelijke variabelen zijn categorisch, de afhankelijke variabele is continue. 
- De variantie van de afhankelijke variabele is hetzelfde voor iedere groep. Dit heet de homogeniteit van variantie. 
- De groepen zijn onafhankelijk. 

### Invoer
--- 

#### Invoerveld 
- Afhankelijke Variabele: De variabele waarin we het meest geïnteresseerd zijn, is de uitkomstvariabele. 
- Vaste Factoren: De variabelen die zijn gemanipuleerd/die de verschillende groepen definiëren, worden ook wel de onafhankelijke variabelen genoemd.  
- WLS gewichten: Gewogen kleinste kwadraten, hier kunnen de variabelen worden geselecteerd die meer gewicht hebben en daarom als informatiever worden gezien. Voor deze laatste optie is het belangrijk om de gewichten a priori te weten. Deze optie wordt vooral gebruikt als de meetfouten heteroskedastisch zijn.



#### Weergave
- Beschrijvende statistieken: Als u deze optie selecteert, worden het gemiddelde, de standaardafwijking en de steekproefgrootte weergegeven voor iedere combinatie van niveaus van de onafhankelijke variabele.
- Schattingen van de effectgrootte: Als u deze optie selecteert, kunt u de specifieke type berekening van de effectgrootte selecteren. 
		- &eta;<sup>2</sup> : Als u deze optie selecteert, wordt het eta-kwadraat berekend als schatting van de effectgrootte. Deze methode overschat echter de populatievariantie, wat het moeilijk maakt om het effect van dezelfde variabele te vergelijken tussen verschillende onderzoeken (Goss-Sampson, 2018).       
		- partial &eta;<sup>2</sup> : Als u deze optie selecteert, wordt het gedeeltelijke eta-kwadraat berekend als schatting van de effectgrootte. Deze methode lost het probleem van overschatting van populatievariantie op, wat het makkelijker maakt om het effect van dezelfde variabele te vergelijken tussen verschillende onderzoeken (Goss-Sampson, 2018).
    - &omega;<sup>2</sup> : Als u deze optie selecteert, wordt het omega-kwadraat uitgerekend als schatting van de effectgrootte. Dit wordt gezien als een goede schatter bij een kleine steekproefgrootte (Goss-Sampson, 2018).
- Vovk-Selke maximum p-ratio: De grens 1/(-e p log(p)) wordt afgeleid van de vorm van de verdeling van de p-waarden. Onder de nulhypothese (H<sub>0</sub>) is het uniform (0,1) en onder de alternatieve hypothese (H<sub>1</sub>) neemt hij af in p, bijv. een beta (α, 1) verdeling waarin 0 < α < 1. De Vovk-Selke MPR wordt verkregen door de vorm van α onder de alternatieve hypothese te kiezen zodat de p-waarde maximaal diagnostisch is. De waarde is dan de ratio van de dichtheid op punt p onder H<sub>0</sub> en H<sub>1</sub>. Als de tweezijdige p-waarde bijvoorbeeld .05 is, is de Vovk-Sellke MPR 2.46. Dit geeft aan dat deze p-waarde maximaal 2.46 keer zo waarschijnlijk is onder H1 dan onder H<sub>0</sub>. Meer informatie vind u in deze <a href="https://jasp-stats.org/2017/06/12/mysterious-vs-mpr/">blogpost</a>.


### Model 
- Componenten en model termen: 
    - Componenten:  Alle onafhankelijke variabelen die in het model kunnen worden meegenomen. 
    - Modeltermen: De onafhankelijke variabelen in het model. Standaard worden alle hoofd- en interactie-effecten van de gekozen onafhankelijke variabele in het model meegenomen. 
  <details>
    <summary><b>GIF demonstratie: Toevoegen interactie </b></summary>
    <img src="analyses/gif/interaction_effect_anova.gif"/>
  </details>
                   
- Kwadratensom: Er zijn verschillende soorten van de kwadratensom. De keuze van het type is belangrijk als er meerdere factoren zijn en de data ongebalanceerd is. In een ongebalanceerd design hebben de verschillende niveaus van de onafhankelijke variabele niet evenveel observaties (bijv. als een groep meer observaties heeft dan de ander). In dit scenario kan het type kwadratensom de resultaten beïnvloeden.   
    - Type I: Sequentiële kwadratensom. Het verminderen van fouten wanneer elke factor van het model wordt toegevoegd aan de factoren die al mee worden genomen, hiermee wordt de volgorde van factoren in het model behouden. Het resultaat hangt af van de volgorde waarin de factoren aan het model worden toegevoegd. Wanneer het model meer dan een factor bevat is het belangrijk hierover na te denken. 
    - Type II: Som van hiërarchisch/gedeeltelijk sequentiële kwadraten. Het verminderen van fouten wanneer elke factor wordt toegevoegd aan het model dat alle andere factoren bevat, behalve de factoren waar de factor deel van uitmaakt, bijvoorbeeld interacties die deze factor bevatten. Langsrud (2003) adviseert dit type bij een ANOVA met ongebalanceerde data. 
    - Type III: Partiële kwadratensom. Het verminderen van fouten wanneer elke factor wordt toegevoegd aan het model dat alle andere factoren bevat, inclusief interacties met deze factor. Dit type wordt vaak geselecteerd, omdat het rekening houdt met interacties (Langsrud, 2003). Dit is de standaardoptie. 

### Verificatie van aannames
- Toets voor homogeniteit: Bij het selecteren van deze optie, wordt er gecontroleerd of de variantie van de afhankelijke variabele gelijk is tussen de groepen met een Levene's toets.
- Homogeniteitscorrecties: Als de assumptie van homogeniteit is geschonden, kunnen er correcties worden geselecteerd. 
	- Geen: Geen homogeniteitscorrectie. 
    - Brown-Forsythe: Als de assumptie van homogeniteit is geschonden, kan deze correctie worden gebruikt. Deze correctie is alleen beschikbaar voor een one-way ANOVA. 
	- Welch: Als de assumptie van homogeniteit is geschonden, kan deze correctie worden gebruikt. Deze correctie is alleen beschikbaar voor een one-way ANOVA.
- Q-Q grafiek van residuen: Controleert de validiteit van assumpties over de verdeling van de dataset. Om precies te zijn, geeft de grafiek aan of residuen normaal verdeeld zijn. 


### Contrasten 
- Voor elke onafhankelijke variabele kan een specifiek contrast worden geselecteerd door op `geen` te klikken in de rechterkolom.
  <details>
    <summary><b>GIF demonstratie: Selecteer contrast </b></summary>
    <img src="analyses/gif/contrasts_anova.gif"/>
  </details>

  - Factoren: Dit zijn de onafhankelijke variabelen die worden meegenomen in de analyse (i.e., de variabelen die zijn geselecteerd in het `vaste factoren` veld).
  - Contrasten: Contrasten stellen u in staat om geplande vergelijkingen te maken. Er zijn verschillende contrasten die verschillende soorten vergelijkingen mogelijk maken:
	  - geen: Als u deze optie selecteert, worden er geen contrasten uitgerekend. Dit is de standaardoptie. 
      - afwijking: Als u dit contrast selecteert, wordt het gemiddelde van elk niveau van de onafhankelijke variabele vergeleken met het totale gemiddelde (het gemiddelde wanneer alle niveaus samen worden genomen). 
      - enkelvoudig: Als u dit contrast selecteert, wordt het gemiddelde van ieder niveau vergeleken met het gemiddelde van een gespecificeerd niveau, bijvoorbeeld met het gemiddelde van de controlegroep. 
      - verschil: Dit contrast wordt ook wel "reverse Helmert" genoemd. Als u dit contrast selecteert, wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van het vorige niveau.
      - Helmert: Als u dit contrast selecteert, wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van de volgende niveaus. Dit is het omgekeerde van het "verschil" contrast.
      - herhaald: Als u dit contrast selecteert, wordt het gemiddelde van elk niveau vergeleken met het gemiddelde van het volgende niveau. 
      - polynoom: Dit contrast test polynome trends in de data. Welke specifieke polynoom wordt gebruikt, is afhankelijk van het aantal niveaus van de onafhankelijke variabele. De graad van de trend die wordt gebruikt, is het aantal niveaus min 1. Als de onafhankelijke variabele dus bestaat uit twee niveaus, wordt een lineaire trend geanalyseerd. Als de onafhankelijke variabele bestaat uit drie niveaus, wordt een kwadratische trend en een lineaire trend geanalyseerd.
      - handmatig: Hier kunnen de contrast gewichten handmatig ingevoerd worden. De gewichten hebben 3 vereisten: (1) minstens 1 gewicht dient niet gelijk aan 0 te zijn (2) Het maximum aantal contrasten is gelijk aan het aantal niveaus van de factor - 1 (3) voor factoren met meer dan 2 niveaus, dienen er minstens 2 contrasten ingevoerd te worden.
- Betrouwbaarheidsintervallen: Als u deze optie selecteert, worden er betrouwbaarheidsintervallen voor het geschatte verschil in gemiddelden gemaakt. De standaardoptie is een interval van 95%. Dit kan tot het gewenste percentage worden aangepast.

### Post Hoc Tests 
- Sleep een of meer namen van factoren naar de rechterkolom om een post-hoc toets uit te voeren. Er zijn verschillende opties: 
  - Effectgrootte: Als u deze optie selecteert, wordt de effectgrootte weergegeven (i.e., de grootte van het geobserveerde effect). De gebruikte maat voor de effectgrootte is Cohen's d. De effectgrootte wordt alleen weergegeven voor het post hoc type `Standaard`.
  - Betrouwbaarheidsintervallen: Als u deze optie selecteert, wordt er een betrouwbaarheidsinterval voor het gemiddelde verschil berekend. Dit wordt voor elke post hoc methode gedaan, behalve voor Dunn. De standaardoptie is een interval van 95%. Dit kan tot het gewenste percentage worden aangepast.
  - Van `...` bootstraps: Als u deze optie selecteert, wordt de gebootstrapte post-hoc toets uitgevoerd. Het standaard aantal iteraties is 1000. Dit kan tot het gewenste aantal worden aangepast. 
  - Correctie: Om te corrigeren voor meerdere vergelijkingen en type 1 fouten te voorkomen, bestaan er verschillende methoden om de p-waarde te corrigeren: 
	  - Tukey: Vergelijk alle mogelijke paren van groepsgemiddelden. Deze correctie kan worden gebruikt wanneer de groepen op de onafhankelijke variabele een gelijke steekproefgrootte en variantie hebben. Deze methode wordt veel gebruikt en is de standaardoptie.
      - Scheffe: Het aanpassen van significatieniveaus in een lineaire regressie om rekening te houden met meerdere vergelijkingen. Deze methode is vrij conservatief.
      - Bonferroni: Deze correctie wordt gezien als vrij conservatief. Het risico op een type 1 fout wordt verminderd, maar statistische kracht (power) wordt ook lager.
      - Holm: Deze methode wordt ook wel sequentiële Bonferroni genoemd, en wordt gezien als minder conservatief dan de Bonferroni methode.
  - Type: Er kunnen verschillende typen post-hoc toets worden geselecteerd. 
	  -  Standaard: Paarsgewijze t-toetsen worden uitgevoerd. Alle correcties kunnen op deze methode worden toegepast. Dit is de standaardoptie.
      -  Games-Howell: Deze methode kan worden gebruikt wanneer groeps-/niveauvarianties niet gelijk zijn. De p-waarden worden gecorrigeerd met de Tukey methode.
      -  Dunett: Als u deze methode selecteert, worden alle niveaus vergeleken met een specifiek niveau, bijvoorbeeld met de controlegroep. Momenteel is het nog niet mogelijk om handmatig te specificeren met welk niveau de andere niveaus worden vergeleken, maar dit is gebaseerd op de volgorde van de niveaus. Om de volgorde aan te passen, kunnen de niveaulabels worden veranderd.
        <details>
	        <summary><b>GIF demonstratie: Aanpassen niveaulabels </b></summary>
	        <img src="analyses/gif/labelediting.gif"/>
      </details> 
    
	  -  Dunn: Dit is een niet-parametrische toets die kan worden gebruikt om kleine subsets van paren te testen. Deze post-hoc toets is een opvolger voor de Kruskal-Wallis toets. De p-waarden worden gecorrigeerd met de Bonferroni en de Holm methode.

### Beschrijvende grafieken
- Selecteer De onafhankelijke variabele op de horizontale as om een beschrijvende grafiek te maken. Als er meerdere onafhankelijke variabelen zijn, kunnen de variabelen in één grafiek worden weergegeven door de andere variabele in het veld Aparte Lijnen te zetten. De variabelen kunnen ook in aparte grafieken worden weergegeven door de andere variabele in het veld Aparte Grafieken te zetten.
  - Factoren: De onafhankelijke variabelen die mee worden genomen in de analyse.
  - Horizontale as: Selecteer de onafhankelijke variabele die op de horizontale as wordt weergegeven.
  - Aparte lijnen: Door een onafhankelijke variabele in deze box te plaatsen, corresponderen verschillende lijnen met verschillende niveaus van de geselecteerde variabele.
  - Aparte grafieken: Door een onafhankelijke variabele in deze box te plaatsen, corresponderen verschillende grafieken met verschillende niveaus van de geselecteerde variabele.
- Weergeven: 
	- Geef foutmarges weer: Als u deze optie selecteert, worden er foutmarges (error bars) weergegeven in de grafiek. De foutmarges kunnen ofwel betrouwbaarheidsintervallen ofwel standaardfouten weergeven. 
		- Betrouwbaarheidsinterval: Dit is de standaardoptie. Met deze optie geven de foutmarges betrouwbaarheidsintervallen van het gemiddelde van elke combinatie van onafhankelijke variabelen weer. De standaardoptie is een interval van 95%. Dit kan tot het gewenste percentage worden aangepast.
        - Stanaardfout: Als u deze optie selecteert, geven de foutmarges de standaardfouten van de gemiddelden van elke combinatie van niveaus van de onafhankelijke variabele weer. 

### Marginale Gemiddelden 
- Marginale gemiddelden: Als u deze optie selecteert, wordt het gemiddelde van elk niveau van de onafhankelijke variabele gecorrigeerd voor alle andere variabelen in het model. 
- Vergelijk marginale gemiddelden met 0: Als u deze optie selecteert, worden de gecorrigeerde gemiddelden vergeleken met 0 en worden de betrouwbaarheidsintervallen voor de gecorrigeerde gemiddelden berekend.  
	- Betrouwbaarheids interval correctie: De betrouwbaarheidsinterval kunnen op verschillende manieren worden aangepast.
		- Geen: Als u deze optie selecteert, wordt er geen correctie gedaan. 
        - Bonferroni: Bonferroni correctie voor de betrouwbaarheidsintervallen. 
        - Sidak: Sidak correctie voor de betrouwbaarheidsintervallen.
- Van `...` bootstraps: Als u deze optie selecteert, worden de gebootstrapte marginale gemiddelde berekend. De standaardhoeveelheid iteraties is 1000. U kunt dit aanpassen tot het gewenste aantal. 

### Hoofdeffecten
- Het hoofdeffect geeft het effect van een onafhankelijke variabele voor elk niveau van de andere onafhankelijke variabele door een ANOVA te doen voor elke subset van de data zoals die wordt gespecificeerd door de moderator variabelen. 
  <details>
	  <summary><b>GIF demonstratie: Hoofdeffecten</b></summary>
	  <img src="analyses/gif/simplemaineffects_anova.gif"/>
  </details>

  - Factoren: Dit veld bevat alle onafhankelijke variabelen die worden meegenomen in de analyse.
  - Simpel effect factor: In dit veld selecteert u de onafhankelijke variabele om het effect van deze variabele geconditioneerd op de niveaus van de moderator te berekenen. 
  - Moderator factor 1: In dit veld selecteert u de onafhankelijke variabele die de verschillende niveaus weergeeft. 
  - Moderator factor 2: In dit veld kunt u optioneel een aanvullende onafhankelijke variabele toevoegen.

### Verdelingsvrije toetsen
- Kruskal-Wallis toets: De Kruskal-Wallis toets is een verdelingsvrije ANOVA en kan gebruikt worden om twee of meer groepen te vergelijken. Deze toets is een op rang-gebaseerde one-way ANOVA. De Kruskal-Wallis toets kan worden uitgevoerd wanneer een van de volgende assumpties is geschonden: normaliteit van de afhankelijke variabele, geen uitschieters, homogeniteit van varianties tussen groepen. Sleep onafhankelijke variabelen van de linker naar de rechter kolom om deze test uit te voeren.

### Uitvoer 
---
#### ANOVA

ANOVA - afhankelijke variabele: 
- Cases: Deze kolom bevat de onafhankelijke variabelen, hun interacties en het residu. 
- kwadratensom: De som van de gekwadrateerde verschillen met het groepsgemiddelde.
- vg: Vrijheidsgraden van het model. 
- Gemiddelde kwadratensom: Schatting van de populatievariantie (de kwadratensom gedeeld door de vrijheidsgraden).
- F: De waarde van de F-statistiek.
- P: De p-waarde. 
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.   
- &eta;<sup>2</sup> : Geschatte effectgrootte eta-kwadraat.      
- &eta;<sup>2</sup><sub>p</sub> : Geschatte effectgrootte gedeeltelijke eta-kwadraat.  
- &omega;<sup>2</sup> : Geschatte effectgrootte omega-kwadraat. 

#### Verificatie van aannames
Test voor gelijkheid van varianties (Levene's):
- F: F-statistiek voor Levene's toets.
- vg 1: Vrijheidsgraden berekend door k-1, waar k staat voor het aantal groepen in de analyse. 
- vg 2: Vrijheidsgraden berekend door N-k-1, waar N staat voor de totale steekproefgrootte en k staat voor het aantal groepen in de analyse.
- p: De p-waarde. Als de p-waarde significant is, betekent dit dat de groepsvarianties van de afhankelijke variabele niet gelijk zijn (i.e., de assumptie van homogeniteit is geschonden). 
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.  

Q-Q Grafiek: 
- Met een Q-Q grafiek kan de normaliteit van de residuen visueel worden geïnspecteerd. De theoretische kwantielen staan op de x-as en de gestandaardiseerde residuen op de y-as. Hoe meer de punten op de diagonale lijn staan, hoe meer de data normaal verdeeld is. 

#### Contrasten
Afwijking/Enkelvoudig/Verschil/Helmert/Herhaald/Polynoom/Handmatig Contrast:
- Vergelijking: De niveaus van de onafhankelijke variabele die worden vergeleken. Voor de handmatige contrasten wordt het gewicht getoond voor elk niveau van de factor.
- Schatting: Het geschatte gemiddelde verschil tussen de vergelen niveaus.
- Std. Fout: De standaardfout van het geschatte gemiddelde.
- vg: De vrijheidsgraden van het model.
- t: De waarde van de t-statistiek.
- p: De p-waarde.
- % BI voor gemiddelde verschil: % betrouwbaarheidsinterval voor het gemiddelde verschil. 95% is de standaardoptie.
	- Onder: De ondergrens van het betrouwbaarheidsinterval. 
    - Boven: De bovengrens van het betrouwbaarheidsinterval. 


#### Post Hoc Tests 
Post Hoc Vergelijkingen (standaard) - onafhankelijke variabele: 
- De eerste twee kolommen geven de niveaus/groepen van de onafhankelijke variabele die met elkaar worden vergeleken.
- Gemiddelde verschil: Het gemiddelde verschil tussen de niveaus.
- % BI voor gemiddelde verschil: Het betrouwbaarheidsinterval voor het gemiddelde verschil tussen de vergeleken niveaus. 95% is de standaardoptie.
	- Onder: De ondergrens van het betrouwbaarheidsinterval.
    - Boven: De bovengrens van het betrouwbaarheidsinterval.

- Std. Fout: De standaardfout van het gemiddelde verschil .	
- t: De waarde van de t-statistiek. 
- Cohen's d: de effectgrootte Cohen's d. Deze corrigeert niet voor meerdere vergelijkingen.
- p<sub>tukey</sub>: Tukey's gecorrigeerde p-waarde voor meerdere vergelijkingen. 
- p<sub>scheffe</sub>:  Scheffe's gecorrigeerde p-waarde voor meerdere vergelijkingen. 
- p<sub>bonf</sub>: Bonferroni's gecorrigeerde p-waarde voor meerdere vergelijkingen.  
- p<sub>holm</sub>: Holm's gecorrigeerde p-waarde voor meerdere vergelijkingen. 

Games-Howell Post Hoc Vergelijkingen - onafhankelijke variabele:  
- De eerste twee kolommen geven de niveaus/groepen van de onafhankelijke variabele die met elkaar worden vergeleken.
- Gemiddelde verschil: Het gemiddelde verschil tussen de niveaus.
- % BI voor gemiddelde verschil: het betrouwbaarheidsinterval voor het gemiddelde verschil tussen de vergeleken niveaus. 95% is de standaardoptie.
	- Onder: De ondergrens van het betrouwbaarheidsinterval.
    - Boven: De bovengrens van het betrouwbaarheidsinterval.
- Std. Fout: De standaardfout van het geschatte gemiddelde.
- t: De waarde van de t-statistiek.
- p<sub>tukey</sub>: Tukey's gecorrigeerde p-waarde voor meerdere vergelijkingen.
    
Dunnett Post Hoc Vergelijkingen:  
- De eerste twee kolommen geven de niveaus/groepen van de onafhankelijke variabele die met elkaar worden vergeleken.
- Gemiddelde verschil: Het gemiddelde verschil tussen de niveaus.
- % BI voor gemiddelde verschil: Het betrouwbaarheidsinterval voor het gemiddelde verschil tussen de vergeleken niveaus. 95% is de standaardoptie.
	- Onder: De ondergrens van het betrouwbaarheidsinterval.
    - Boven: De bovengrens van het betrouwbaarheidsinterval. 
- Std. Fout: De standaardfout van het geschatte gemiddelde.
- t: De waarde van de t-statistiek.
- p<sub>dunnett</sub>: Dunnett's p-waarde. 

Dunn's Post Hoc vergelijkingen:  
- De eerste twee kolommen geven de niveaus/groepen van de onafhankelijke variabele die met elkaar worden vergeleken.
- z: De waarde van de z-statistiek. 
- W<sub>i</sub>: De gemiddelde rang van het eerste niveau/groep van de vergelijking.
- W<sub>j</sub>: De gemiddelde rang van het tweede niveau/groep van de vergelijking.
- p: De p-waarde.
- p<sub>bonf</sub>: Bonferroni's gecorrigeerde p-waarde voor meerdere vergelijkingen.
- p<sub>holm</sub>: Holm's gecorrigeerde p-waarde voor meerdere vergelijkingen.

#### Post Hoc Tests door middel van Bootstrapping 
Ge-bootstrapte Post Hoc Vergelijkingen - Onafhankelijke variabele: 
- De eerste twee kolommen geven de niveaus/groepen van de onafhankelijke variabele die met elkaar worden vergeleken.
- Gemiddelde verschil: het gemiddelde verschil tussen de niveaus. Deze schatting wordt gebaseerd op de mediaan van de bootstrap verdeling. 
- Bias: Het gemiddelde verschil tussen de gebootstrapte gemiddelde verschillen en het geschatte gemiddelde verschil. 
- Std. Fout: De standaardfout van de gebootstrapte geschatte gemiddelden.
- 95% betrouwbaarheidsinterval voor het gemiddelde verschil: Het voor bias-gecorrigeerde betrouwbaarheidsinterval van het gemiddelde verschil tussen vergeleken niveaus. De standaardoptie is 95%.
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.
  
#### Marginale Gemiddelden
Marginale gemiddelden - onafhankelijke variabele: 
- De eerste kolom bevat de niveaus van de onafhankelijke variabele.
- Marginale gemiddelde: Het marginale gemiddelde voor elk niveau van de onafhankelijke variabele. Dit gemiddelde is gecorrigeerd voor alle andere variabelen in het model. 
- Std. Fout: De standaardfout van het marginale gemiddelde.
- Onder BI: De ondergrens van het betrouwbaarheidsinterval.
- Boven BI: De bovengrens van het betrouwbaarheidsinterval.
- t: De waarde van de t-statistiek.
- p: De p-waarde.

#### Marginale Gemiddelden Door Middel van Bootstrapping
Ge-bootstrapte marginale gemiddelden - Onafhankelijke variabele: 
- Onafhankelijke variabele: Deze kolom bevat alle niveaus van de onafhankelijke variabele.
- Marginale gemiddelde: Schatting van het marginale gemiddelde voor elk niveau van de onafhankelijke variabele.  Dit gemiddelde is gecorrigeerd voor alle andere variabelen in het model. De schatting is gebaseerd op de bootstrap verdeling.
- Bias: Bias: Het gemiddelde verschil tussen het gebootstrapte marginale gemiddelde en het geschatte marginale gemiddelde.
- Std. Fout: De standaardfout van de gebootstrapte marginale gemiddelden. 
- 95% BI voor gemiddelde verschil: de voor bias-gecorrigeerde betrouwbaarheidsinterval van het gemiddelde verschil tussen de vergeken niveaus. De standaardoptie is 95%.
  - Onder: De ondergrens van het betrouwbaarheidsinterval.
  - Boven: De bovengrens van het betrouwbaarheidsinterval.

Hoofdeffecten - onafhankelijke variabele: 
- De eerste kolom bevat de niveaus van de andere onafhankelijke variabele (als die er zijn).
- Kwadratensom: De som van de gekwadrateerde verschillen met het groepsgemiddelde.
- vg: De vrijheidsgraden.
- Mean Square: Schatting van de populatievariantie (de kwadratensom gedeeld door de vrijheidsgraden).
- F: De waarde van de F-statistiek.
- p: De p-waarde.


Kruskal-Wallis Toets: 
- Factor: Deze kolom bevat de onafhankelijke variabele die is meegenomen in de analyse.
- Statistiek: De waarde van de teststatistiek.
- vg: De vrijheidsgraden.
- P: De p-waarde.


#### Beschrijvende Statistieken
Beschrijvende statisteken - afhankelijke variabele:
- Onafhankelijke variabele: De niveaus van de onafhankelijke variabele(n) die worden meegenomen in de analyse. Als het er meer dan één zijn, worden de beschrijvende statistieken weergegeven voor elke combinatie van niveaus van de onafhankelijke variabelen.
- Gemiddelde: Het gemiddelde per niveau of als er meerdere onafhankelijke variabelen zijn, het gemiddelde per combinatie van niveaus. 
- SD: De standaardafwijking.
- N: De steekproefgrootte.

#### Beschrijvende Statistieken Grafiek 
Onafhankelijke variabele op de x-as en afhankelijke variabele op de y-as. Als er meerdere onafhankelijke variabelen mee worden genomen, worden deze weergegeven door verschillende lijnen of verschillende grafieken.


### Referenties 
--- 
-	Field, A. (2009). *Discovering Statistics using SPSS (3rd ed.)*. Sage Publishing.
-	Field, A., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. Sage Publishing.
-	Goss-Sampson, M. A. (2018). *Statistical Analysis in JASP: A Guide for Students*. Version 2, October 2018. 
-	Langsrud, Ø. (2003). ANOVA for unbalanced data: Use Type II instead of Type III sums of squares. *Statistics and Computing, 13*(2), 163-167.
-	Moore, D.S., McCabe, G.P., & Craig, B.A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W.H. Freeman and Company.
-	Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
-	Whitlock, M.C. & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.


### R Packages
---
- afex
- boot
- car
- emmeans
- ggplot2
- lmtest
- MASS
- multcomp
- onewaytests
- sandwich
- stats

### Voorbeeld
--- 
- Ga voor een voorbeeld naar `file`-->`data library`-->`ANOVA`-->`Erotic Pictures and Love`. 
- Kijk deze <a href="https://youtube.com/watch?v=nlAhWQmG5Iw&feature=youtu.be">video</a>. voor meer informatie over de anova in jasp. 
