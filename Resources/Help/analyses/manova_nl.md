MANOVA 
==========================

Met een MANOVA kan men het verschil tussen twee of meer groepsgemiddelden berekenen.

### Assumpties 
- De residuen zijn voor iedere groep normaal verdeeld.
- De onafhankelijke variabelen zijn categorisch, de afhankelijke variabele is continue. 
- De populatie covariantie matrices van elke groep zijn gelijk 
- De groepen zijn onafhankelijk. 

### Input 
--- 
#### Invoerveld
- Afhankelijke Variabele: De variabele waarin we het meest geïnteresseerd zijn. Deze wordt ook wel de uitkomst variabele genoemd. 
- Vaste Factoren: De variabelen die zijn gemanipuleerd/die de verschillende groepen definiëren. Deze worden ook wel de onafhankelijke variabelen genoemd.   
   

#### Model
- Componenten en model termen: 
    - Componenten: Alle onafhankelijke variabelen die in het model kunnen worden meegenomen. 
    - Model termen: De onafhankelijke variabelen in het model. Standaard worden alle hoofd- en interactie-effecten van de gekozen onafhankelijke variabele in het model meegenomen. 

- Neem intercept mee: Geef het intercept weer in de MANOVA en ANOVA tabellen.


#### Additionele Opties
- Toets: selecteer de statistische test om uit te voeren voor de MANOVA, en hoe de F-ratio wordt benaderd.
    - Pillai: Pillai's trace.
    - Wilks: Wilks' lambda. Dit kan worden geïnterpreteerd als de proportie van de variantie in uitkomsten die niet wordt verklaard door een effect. 
    - Hotelling-Lawley: Hotelling-Lawley's trace.
    - Roy: Roy's grootste wortel.

- Weergeven: 
  - ANOVA tabellen: Geeft individuele ANOVA tabellen per afhankelijke variabele.
  - Vovk-Selke maximum p-ratio: De grens 1/(-e p log(p)) wordt afgeleid van de vorm van de verdeling van de p-waardes. Onder de nul hypotheses (H<sub>0</sub>) is het uniform (0,1) en onder de alternatieve hypothese (H<sub>1</sub>) neemt hij af in p, bijv. een beta (α, 1) verdeling waar 0 < α < 1. De Vovk-Selke MPR wordt verkregen door het vorm van α onder de alternatieve hypothese te kiezen zodat de p-waarde maximaal diagnostisch is. De waarde is dat de ratio van de dichtheid op punt p onder H<sub>0</sub> en H<sub>1</sub>. Als de tweezijdige p-waarde bijvoorbeeld .05 is is de Vovk-Sellke MPR 2.46. Dit geeft aan dat deze p-waarde maximaal 2.46 zo waarschijnlijk is onder H1 dan onder H<sub>0</sub>. Meer informatie vind je in deze a href="https://jasp-stats.org/2017/06/12/mysterious-vs-mpr/">blogpost</a>. 

### Uitvoer
---

#### MANOVA 
MANOVA - toets: 
- Cases: Deze kolom bevat de onafhankelijke variabelen, hun interacties en het residu. 
- df: Vrijheidsgraden van het model.
- Benad. F: Benadering van de F-ratio. De verschillende toetsen kunnen verschillende benaderingen opleveren.
- Test statistiek: De waarde van de test statistiek, afhankelijk van welke test is geselecteerd. 
- Num df: Vrijheidsgraden voor het bepalen van p-waardes van de F-statistieken.
- Den df: Vrijheidsgraden voor het bepalen van p-waardes van de F-statistieken.
- p: De p-waarde van de benaderde F-ratio.
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.   


#### ANOVA 
ANOVA - afhankelijke variabele: 
- Cases: Deze kolom bevat de onafhankelijke variabelen, hun interacties en het residu. 
- Sum of squares: De som van de gekwadrateerde verschillen met het groepsgemiddelde.
- df: Vrijheidsgraden van het model. 
- Mean square: Schatting van de populatie variantie (de sum of squares gedeeld door de vrijheidsgraden).
- F: De waarde van de F statistiek.
- P: De p-waarde.
- VS-MPR: Vovk-Sellke Maximum *p*-ratio. 


### Referenties 
--- 
-	Field, A. (2009). *Discovering Statistics using SPSS (3rd ed.)*. Sage Publishing.
-	Field, A., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. Sage Publishing.

### R Packages
---
- stats


### Voorbeeld 
--- 
TBA