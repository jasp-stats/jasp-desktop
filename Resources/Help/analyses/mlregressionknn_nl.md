K-Naaste Buren Regressie
==========================

K-naaste buren is een regressiemethode die lijkt op het *k* aantal voorspellende observaties die het meest lijken op de nieuwe observatie om een voorspelling te maken over de waarde. Het aantal naaste buren is intrinsiek gebonden aan de complexiteit van het model, aangezien kleine getallen de flexibiliteit van het model verhogen.

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

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Kwadratisch gemiddelde fout: Plot het aantal naaste buren tegen de MSE van het model. Precisie is bepaald voor de trainingsset (en validatieset).
- Voorspellingsvermogen: Laat de observaties van de geselecteerde testset tegen de voorspelde waardes zien.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van je data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele die aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan je dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in je data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens je indicator.

#### Trainings- and Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).
- K-fold met *k* folds: Deel de overgebleven data op in *k* delen.
- Laat-één-weg: Deel de overgebleven data op in *n* delen.

### Parameters Trainen
#### Algoritme Instellingen
- Gewichten: Stel het gewichtsschema voor de naaste buren in. De standaardoptie, rectangular,  geeft de standaard knn, terwijl de andere opties het algoritme uitbreiden door de naaste buren af te wegen. Zie ook het kknn package.
- Afstand: De afstandsmetriek om de gelijkheid tussen naaste buren te bepalen. Kan met de Euclidean- of Manhattan afstand.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waardes van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Set seed: Geeft de mogelijkheid een seed te gebruiken voor je analyse. Een seed gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een seed maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Aantal Naaste Buren
- Vast: Laat je een eigen gespecificeerd aantal naaste buren gebruiken. 
- Optimalisatie: Laat je de voorspellingsfout te optimaliseren van een validatie dataset met betrekking tot het aantal naaste buren. 
- Max. aantal naaste buren: Het maximum aantal mogelijke naaste buren. Als standaardoptie staat dit op 10.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in je dataset met de klasselabels van je classificatie resultaat. Dit geeft je de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### K-Naaste Buren Regressie Model Tabel
- De eerste kolom toont het aantal naaste buren.
- Gewichten: Het gewichtenschema.
- Distance: De gebruikte afstand.
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (mogelijk wanneer model is geoptimaliseerd).
- n(Test): Het aantal observaties in de testset.
- Validation MSE: De MSE van de validatieset (mogelijk wanneer model is geoptimaliseerd).
- Testset MSE: De MSE van de testset.

#### Evaluationmetrieken
- MSE: De kwadratisch gemiddelde fout van het model.
- RMSE: De wortel van de kwadratische gemiddelde fout van het model.
- MAE: De gemiddelde absolute fout van het model.
- MAPE: De gemiddelde absolute percentagefout van het model.
- R<sup>2</sup>: De proportie variabelen van een afhankelijke variabele die is uitgelegd door de onafhankelijke variabele(n).

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- kknn

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  

