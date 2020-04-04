Hiërarchische Clustering
==========================

Hiërarchische clustering is een hard clusteringsalgoritme dat data opdeelt in meerdere clusters, waar elke observatie tot één groep behoort. De data is dusdanig verdeeld dat de mate van gelijkheid tussen twee observaties maximaal is als ze bij dezelfde groep behoren en minimaal als die niet zo is.

### Assumpties
- De data bestaat uit continue variabelen.
- (Normaal verdeelde data helpt het clusteringsproces).

### Invoer 
-------
#### Invoerveld 
- Variabelen: In dit veld vult u de variabelen in die meegenomen dienen te worden in het clusteringsalgoritme. 

#### Tabellen  
- Clusterinformatie: Toont de grootte van elke cluster. Dit is de standaardoptie. 
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elke cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.

#### Grafieken
- Elleboogmethode: Genereert een grafiek met de totale binnen kwadratensom op de y-as en het aantal clusters op de x-as. De grafiek kan gebruikt worden om het optimale aantal clusters te bepalen. De grafiek laat 3 lijnen zien door AIC, BIC, en 'elleboogmethode' te optimaliseren.
- Dendrogram: Genereert een dendrogram van de clustering output. Een dendrogram kan worden geïnterpreteerd als een boom op zijn kop. Onderop zitten de bladeren, en hoe verder u omhoog gaat hoe meer de bladeren zich samenvoegen in takken (=groepen observaties). De hoogte waarom een blad samenvoegt geeft de (on)gelijkheid van observaties of groepen observaties weer. Het dendogram wordt gebruikt om clusters te maken door een snede te maken binnen het dendogram. Een dendogram kan worden gebruikt om de clusteringsstructuur te inventariseren. 
- t-SNE clustergrafiek: Genereert een t-SNE grafiek van de clustering output. t-SNE grafieken worden gebruikt voor het visualiseren van hoog-dimensionale data in een laag-dimensionale ruimte van twee dimensies om de relatieve afstand tussen observaties te laten zien. De t-SNE twee-dimensionale ruimte maakt de assen oninterpreteerbaar. Een t-SNE grafiek geeft een indruk van de relatieve afstanden tussen observaties en clusters. Om dezelfde t-SNE grafiek nog een keer te maken voor meerdere clusteringanalyses, kunt u een toevalsgenerator beginwaarde gebruiken, aangezien het t-SNE algoritme willekeurige startpunten gebruikt.
- Legenda: Geeft een legenda met het clusternummer voor elke observatie. Dit is de standaardoptie.
- Labels: Geeft de clusteringlabels van de verschillende observaties.


#### Trainings parameters
#### Algoritme Instellingen
- Afstand: Specificeer de ongelijkheidsmeting. De Euclidische afstand gebruikt de geometrische afstand tussen twee punten en is volledig gebaseerd op de magnitude van de afstand. De Pearsoncorrelatie is een daarentegen een correlatie-gebaseerde ongelijkheidsmeting, welke kijkt naar de lineaire associatie van observaties en als deze correlaties hoog zijn, deze observaties worden als gelijk gezien. Euclidische afstand is de standaardoptie.
- Binding: Specificeer de gebruikte bindingsmeting. Enkele binding gebruikt de kleinste afstand tussen clusters. Complete binding gebruikt de verste afstand tussen clusters. Centroïde binding gebruikt de centroïden van de clusters. Gemiddelde binding berekent de afstand tussen alle clusters. Gemiddelde binding is de standaardoptie. 
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waarden van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een toevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Clusteringsparameters 
- vast: Stelt u in staat om een eigen gespecificeerd aantal clusters te gebruiken. Hiermee kunt u handmatig optimaliseren.
- Optimalisatie: Stelt u in staat om een optimalisatiemethode kiezen. De opties zijn AIC, BIC, en silhouette. De AIC gebruikt de binnen kwadratensom (within-cluster variatie), het aantal gegenereerde clusters en het aantal dimensies voor het optimaliseren van de clustering output. De BIC gebruikt de binnen kwadratensom (within-cluster variatie), het aantal gegenereerde clusters, het aantal dimensies, en de steekproefgrootte voor het optimaliseren van de clustering output. De silhouette waarde gebruikt de gelijkheid van de observaties binnen een cluster en de ongelijkheid aan andere clusters voor het optimaliseren van de clustering output. BIC optimalisatie is de standaardoptie.
- Max. clusters: Bepaalt het maximum aantal mogelijke clusters om te genereren. De standaardoptie is 10.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in uw dataset met de klasselabels van uw classificatie resultaat. Dit geeft u de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Hiërarchisch Clustering Model Tabel
- De eerste kolom geeft het aantal gegenereerde clusters.
- N: De steekproefgrootte.
- R<sup>2</sup>: Geeft de hoeveelheid verklaarde variantie door het model.
- AIC: De AIC waarde van het model. Lage waarden betekenen beter clusterende output.
- BIC: De BIC waarde van het model. Lage waarden betekenen beter clusterende output.
- Silhouette: De Silhouette waarde van het model. De Silhouette waarde spreiden van -1 tot 1, waar 1 een perfecte score is.

#### Hiërarchisch Cluster Informatie
- Grootte: De grootte van elk cluster.
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elke cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.

#### Evaluatiemetrieken Tabel
- Maximum diameter: De maximale cluster diameter in *euclidische* afstand.
- Minimum scheiding: De minimale cluster scheiding in *euclidische* afstand.
- Pearson's \u03B3: Correlatie tussen afstanden en een 0-1-vector waar 0 betekent dezelfde cluster, 1 betekent andere clusters. 
- Dunn index: Minimum scheiding / maximum diameter. 
- Entropie: Entropie van de distributie van clusterlidmaatschappen.
- Calinski-Harabasz index: De variantie ratio criterium van de clusterlidmaatschappen.

### Referenties
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.
- Akaike, H. (1987). Factor analysis and AIC. Psychometrika, 52(3), 317–332.
- Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of clusters in a data set via the gap statistic. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 63(2), 411–423.
- Matthiesen, R. (Ed.). (2010). Bioinformatics methods in clinical research. Humana Press.
- Schwarz, G., et al. (1978). Estimating the dimension of a model. The annals of statistics, 6(2), 461–464.

### R-packages 
--- 
- cluster
- stats
- ggdendro
- Rtsne

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Iris Bloemen`.  

