Linear Discriminant Classificatie
==========================

Linear Discriminant Analysis (LDA) is een classificatiemethode die doelt op het vinden van *p - 1* componenten die het beste discrimineren tussen de klassen van the target variabele. LDA is een lineaire classifier, wat betekent dat de decision boundaries tussen klassen lineair zijn.

### Assumpties
- De target variabele is een nominale of ordinale variabele.
- De voorspellende variabelen bestaan uit continue, nominale, of ordinale variabelen.
- Gelijkheid van de klassegemiddeldes: De klassegemiddeldes dienen gelijk te zijn, dit kan gecontroleerd worden met de corresponderende tabel.
- Gelijkheid van de covariantiematrices: De covariantiematrices dienen gelijk te zijn, dit kan gecontroleerd worden met de corresponderende tabel.
- Multicollineariteit: De klassen dienen niet met elkeaar te correleren, dit kan gecontroleerd worden met de corresponderende tabel.

### Invoer 
-------
#### Invoerveld 
- Target: In dit veld vul je de variabele in die voorspeld wordt. 
- Voorspellers: In dit veld vul je de variabelen in die informatie geven over de target. 

#### Tabellen  
- Confusion matrix: Toont een tabel die de geobserveerde klassen uitgezet tegen de voorspelde klassen laat zien. Wordt gebruikt om de Precisie van het model te bepalen.
- Klasse proporties: Toont een tabel die de proporties van elke klasse laat zien in de data-, trainings- (en validatie-), en testset.
- Evaluatiemetrieken: Toont regelmatig gebruikte classificatie evaluatiemetrieken zoals precision, recall, de F1-score, support en AUC (gebied onder de ROC curve).
- Coëfficiënten: Laat de coëfficiënten voor de lineaire discriminanten zien. 
- Prior en posterior kansen: Laat de prior en posterior groepskansen zien. Prior kansen zijn de proporties in de trainingsset.
- Klassegemiddelds trainingsdata: Laat de gemiddeldes van elke variabele in elke klasse in de trainingsdata zien.

#### Grafieken
- Datasplit: Laat zien hoe de data is gesplitst in trainings- (en validatie-) en testset.
- ROC curves: Toont ROC curves voor elke klasse voorspelt tegenover alle andere klassen.
- Andrews curves: Een manier om de structuur in hoger dimensionele data te visualizeren. Lijnen die clusteren, zijn observaties die meer overeenkomen. 
- Lineaire discriminant matrix: Maakt een matrixgrafiek die de verdelingen op de lineaire discriminanten visualiseert samen met een spreidingsdiagram van variabelen op deze discriminanten.
- Decision boundary matrix: Maakt een *n* x *n* grafiek die visualiseert hoe elke observatie zou zijn geclassificeerd zoals het huidige model voorspelt. Grenzen tussen klassen zijn gevisualiseert. Kan enkel gemaakt worden voor numerieke voorspellers.

### Datasplit Voorkeuren
#### Holdout Testdata
- Steekproef *x*% van alle data: Kies een percentage om aselect een steekproef van je data te nemen zodat de voorspellingsfout berekend kan worden. Genereert een interne indicator-variabele dat aangeeft of de observatie is meegenomen (1) of uitgesloten (0) van de testset.
- Voegt gegenereerde indicator toe aan de data: Voeg de gegenereerde testset indicator van bovenstaande optie toe aan je dataset. Vereist een kolomnaam.
- Testset indicator: Gebruik een indicator-variabele om data te selecteren voor de testset. Deze indicator dient een kolom in je data te zijn die enkel bestaat uit 0 (uitgesloten van de testset) en 1 (meegenomen in de testset). De data zal dan gesplitst worden in een trainingsset (en validatie- indien aangevraagd)(0), en een testset (1) volgens je indicator.

### Parameters Trainen 
#### Algoritme instellingen
- Schattingsmethode: Zet de estimator voor het algoritme. Is één van de moments voor standaard estimators van het gemiddelde en de variantie: "mle", "mve", of "t" voor robuuste schattingen gebaseerd op een t-distributie. Zie ook de MASS package.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waardes van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik seed: Geeft de mogelijkheid een seed te gebruiken voor je analyse. Een seed gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een seed maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in je dataset met de klasselabels van je classificatie resultaat. Dit geeft je de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Linear Discirminant Classificatie Model Tabel
- De eerste kolom laat het aantal lineaire discriminanten.
- Methode: De schattingsmethode.
- n(Train): Het aantal observaties in de trainingsset.
- n(Test): Het aantal observaties in de testset.
- Testset Precisie: De classificatie Precisie voor de testset.


#### Evaluatiemetrieken
- Precision: Ratio van correcte positieve voorspellingen en het totaal aantal positieve voorspellingen.
- Recall: Ratio van correcte positieve voorspellingen en het totale aantal positieve observaties.
- F1 Score: Het harmonische gemiddelde van de precision- en recallscores.
- Support: Het aantal observaties van een klasse in de testset.
- AUC: Gebied onder de ROC curve. Elke klasse is voorspeld tegen alle andere klassen. Zie ook ROC curves.

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- MASS
- ROCR

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Data Library` --> `Machine Learning` --> `Wine Types`.  


