Boosting Classificatie
==========================

Boosting werkt door opeenvolgende voorspellers toe te voegen aan een beslisboom ensemble, waarin elke voorspeller de voorgaande corrigeert. Echter, in plaats van  de gewichten voor elke incorrecte geclassificeerde observatie bij elke iteratie te vveranderen, probeert de Boosting methode de nieuwe voorspeller aan te passen aan de residuen gemaakt door de voorgaande voorspeller.


### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vult u de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vult u de variabelen in die informatie geven over de target. 

#### Tabellen  
- Confusion matrix: Toont een tabel die de geobserveerde klassen uitgezet tegen de voorspelde klassen laat zien. Wordt gebruikt om de Precisie van het model te bepalen.
- Klasse proporties: Toont een tabel die de proporties van elke klasse laat zien in de data-, trainings- (en validatie-), en testset.
- Evaluatiemetrieken: Toont regelmatig gebruikte classificatie evaluatiemetrieken zoals precision, recall, de F1-score, support en AUC (gebied onder de ROC kromme).
- Relatieve invloed: Laat de relatieve invloed van de voorspellers zien.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- Out-of-bag verbetering: Plot het aantal bomen tegen de out-of-bag classificatie Precisieverbetering van het model. Precisie is bepaald voor de trainingsset.
- ROC kromme: Toont een ROC kromme voor elke klasse voorspelt tegenover alle andere klassen.
- Andrews kromme: Een manier om de structuur in hoger dimensionele data te visualiseren. Lijnen die clusteren, zijn observaties die meer overeenkomen. 
- Afwijking: Laat de voorspellingsfout zien geplot tegen het aantal bomen.
- Relatieve invloed: Laat de relatieve invloed van de voorspellers zien.
- Beslissingsseparatrix: Maakt een *n* x *n* grafiek die visualiseert hoe elke observatie zou zijn geclassificeerd zoals het huidige model voorspelt. Grenzen tussen klassen zijn gevisualiseerd. Kan enkel gemaakt worden voor numerieke voorspellers.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van uw data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan uw dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in uw data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens uw indicator.

#### Training- and Validatiedata
- Steekproef *x*% van validatiedata: Selecteer aselect een percentage als steekproef van de overgebleven data (na het selecteren van de testset).
- K-fold met *k* folds: Deel de overgebleven data op in *k* delen.

### Parameters Trainen
#### Algoritme Instellingen
- Shrinkage: Een shrinkage parameter toegepast op elke beslisboom van de uitbreiding. Ookwel bekend als de learning rate of de step-size reductie 0.001 tot 0.1 werkt gewoonlijk, maar een kleinere learning rate heeft gebruikelijk meer bomen nodig.
- Interactie diepte: Geheel getal dat de maximale diepte van elke beslisboom specificeert (i.e., het hoogste niveau van toegestane variabele interacties. Een waarde van 1 impliceert een additieve model, een waarde van 2 impliceert een model met hoogstens 2-weg interacties, etc. Standaardoptie is 1.
- Min. observatie in knoop: Geheel getal dat het minimum aantal observaties in de eindknopen van de bomen specificeert. Let op dat dit het daadwerkelijk aantal observaties is, niet het totale gewicht.
- Gebruikte trainingdata per beslisboom: Selecteert het percentage trainingsdata dat is gebruikt om elk individuele beslisboom te trainen.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waarden van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Aantal Beslisbomen
- Vast: Stelt u in staat om een eigen gespecificeerd aantal bomen te gebruiken. 
- Optimalisatie: Stelt u in staat om de voorspellingsfout te optimaliseren van een validatie dataset met betrekking tot het aantal bomen. 
- Max. aantal bomen: Het maximum aantal mogelijke bomen. Als standaardoptie staat dit op 100.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Boosting Classificatie Model Tabel
- De eerste kolom laat het aantal bomen zien.
- Shrinkage: De shrinkage parameter.
- n(Train): Het aantal observaties in de trainingsset.
- n(Validatie): Het aantal observaties in de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- n(Test): Het aantal observaties in de testset.
- Validatie Precisie: De classificatie Precisie voor de validatieset (zichtbaar wanneer model geoptimaliseerd is).
- Testset Precisie: De classificatie Precisie voor de testset.

#### Evaluatiemetrieken
- Precisie: Ratio van correcte positieve voorspellingen en het totaal aantal positieve voorspellingen.
- Recall: Ratio van correcte positieve voorspellingen en het totale aantal positieve observaties.
- F1 Score: Het harmonische gemiddelde van de precision- en recallscores.
- Support: Het aantal observaties van een klasse in de testset.
- AUC: Gebied onder de ROC kromme. Elke klasse is voorspeld tegen alle andere klassen. Zie ook ROC kromme.

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- gbm
- ROCR

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Telco's Overstappende Klanten`.  

