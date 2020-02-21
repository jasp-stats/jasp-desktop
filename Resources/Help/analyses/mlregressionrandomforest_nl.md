Random Forest Regressie
==========================

Random Forest is een regressiemethode die een set beslisbomen maakt, bestaande uit een groot aantal individuele bomen die functioneren als ensemble. 

### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vul je de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vul je de variabelen in die informatie geven over de target. 

#### Tabellen  
- Evaluatiemetrieken: Toon regelmatig gebruikte classificatie evaluatiemetrieken zoals kwadratisch gemiddelde fout (MSE), wortel kwadratisch gemiddelde fout (RMSE) en R<sup>2</sup>.
- Variabele van belang: Toont de gemiddelde daling van de Precisie en de totale stijging van de node purity voor alle voorspellervariabelen. Dit zijn indicatoren voor het belang van de voorspellers.

#### Plots
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Out-of-bag verbetering: Plot het aantal beslisbomen tegen de out-of-bag classificatie Precisiesverbetering van het model. Precisie is bepaald voor de trainingsset.
- Voorspellingsvermogen: Laat de observaties van de geselecteerde testset tegen de voorspelde waardes zien.
- Gemiddelde daling van de Precisie: Toont de variabele gemiddelde daling van de Precisie van het model.
- Totale stijging van de node purity: Toont de variabele totale stijging van node purity van het model.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van je data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan je dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in je data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens je indicator.

#### Trainings- and Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).

### Parameters Trainen
#### Algorithme Instellingen
- Trainingsdata per boom: Selecteer het percentage traingsdata dat wordt gebruikt om elke individuele boom te trainen.
- Voorspellers per splitsing: Geef het aantal voorspellervariabelen die gebruikt worden binnen elke splitsing in de beslisboom.   
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waardes van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik seed: Geeft de mogelijkheid een seed te gebruiken voor je analyse. Een seed gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een seed maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

##### Aantal Beslisbomen
- Vast: Laat je een eigen gespecificeerd aantal beslisbomen gebruiken. 
- Optimalisatie: Laat je de voorspellingsfout te optimaliseren van een validatie dataset met betrekking tot het aantal beslisbomen. 
- Max. aantal beslisbomen: Het maximum aantal mogelijke beslisbomen. Als standaardoptie staat dit op 100.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in je dataset met de klasselabels van je classificatie resultaat. Dit geeft je de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Random Forest Regression Model Table
- De eerste kolom laat het aantal beslisbomen zien.
- Voorspellers per splitsing: Geef het aantal voorspellervariabelen die gebruikt worden binnen elke splitsing in de beslisboom. 
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (mogelijk wanneer model is geoptimaliseerd).
- n(Test): Het aantal observaties in de testset.
- Validation MSE: De MSE van de validatieset (mogelijk wanneer model is geoptimaliseerd).
- Testset MSE: De MSE van de testset.
- OOB fout: De out-of-bag MSE van de testset.

#### Evaluatiemetrieken
- MSE: De kwadratisch gemiddelde fout van het model.
- RMSE: De wortel van de kwadratische gemiddelde fout van het model.
- MAE: De gemiddelde absolute fout van het model.
- MAPE: De gemiddelde absolute percentagefout van het model.
- R<sup>2</sup>: De proportie variabelen van een afhankelijke variabele die is uitgelegd door de onafhankelijke variabele(n).

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Breiman, Leo. (2001). Random forests. Machine Learning. Springer, 45(1), 5-32

### R-packages 
--- 
- randomForest

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  


