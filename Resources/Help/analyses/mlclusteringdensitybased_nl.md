Density Based Clustering
==========================
Density-based clustering is een soft clusteringsmethode waarbij clusters worden gemaakt als maximale sets van punten die verbonden zijn aan punten waarvan de dichtheid een drempelwaarde overtreft. De dichtheid komt voort uit het concept dat voor elke punt in een cluster, de neighborhood binnen een gegeven radius minstens een bepaalde hoeveelheid punten bevat, dat resulteert in de dichtheid van die neighborhood om een bepaalde drempelwaarde te overtreffen. Een density-based cluster is te herkennen aan punten met een hogere dichtheid dan punten buiten dat cluster. De set van alle hoge-dichtheid punten wordt het dichtheidsniveau genoemd. De punten die een dichtheidsniveau niet overschrijden, worden gezien als uitschieters. Het dichtheidsniveau beïnvloedt het aantal gegenereerde clusters. 

### Assumpties
- De data bestaat uit continue variabelen.
- (Normaal verdeelde data helpt het clusteringsproces).

### Invoer 
-------
#### Invoerveld 
- Variabelen: In dit veld vul je de variabelen in die meegenomen dienen te worden in het clusteringsalgoritme. 

#### Tabellen  
- Clusterinformatie: Toont de grootte van elk cluster. Dit is de standaardoptie. 
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elk cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.

#### Grafieken
- K-afstandsgrafiek: Genereer een grafiek met de naaste buren afstand (het aantal naaste buren is bepaald met de Min. kernpunten parameter) op de y-as en de punten gesorteerd op afstand op de x-as. Deze plot kan gebruikt worden om de optimale Epsilon waarde te bepalen. De waarde waar de grafiek een kink heeft, is de optimale Epsilon waarde (de waarde op de y-as).
- t-SNE clustergrafiek: Genereert een t-SNE grafiek van de clustering output. t-SNE grafieken worden gebruikt voor het visualiseren van hoog-dimensionale data in een laag-dimensionale ruimte van twee dimensies om de relatieve afstand tussen observaties te laten zien. De t-SNE twee-dimensionale ruimte maakt de assen oninterpreteerbaar. Een t-SNE grafiek geeft een indruk van de relatieve afstanden tussen observaties en clusters. Om dezelfde t-SNE grafiek nog een keer te maken voor meerdere clusteringanalyses, kan je een toevalsgenerator beginwaarde gebruiken, aangezien het t-SNE algoritme willekeurige startpunten gebruikt.
- Legenda: Geeft een legenda met het clusternummer voor elke observatie. Dit is de standaardoptie.
- Labels: Geeft de clusteringlabels van de verschillende observaties.

#### Parameters Trainen 
#### Algoritme Instellingen
- Epsilon buurtomvang: Dit is de grootte van de radius waarin een minimum hoeveelheid kernpunten is die resulteren in de dichtheid van die neighborhood om een drempelwaarde te overtreffen. Door de drempelwaarde te overtreffen, kunnen punten (i.e., observaties) een cluster genereren.
- Min. kernpunten: Reflecteert de minimale hoeveelheid punten die nodig zijn in de Epsilon neighborhood om de punten een cluster te laten vormen. De Eps en MinPts parameters bepalen het dichtsheidsniveau en reguleren hoeveel punten nodig zijn in een gegeven radius om een bepaalde drempelwaarde te overtreffen voor het vormen van een cluster.
- Afstand: Specificeer de ongelijkheidsmeting. Normale dichtheid gebruikt de geometrische afstand tussen twee punten en is volledig gebaseerd op de magnitude van de afstand. Gecorreleerde dichtheid is daarentegen een correlatie-gebaseerde ongelijkheidsmeting, welke kijkt naar de lineaire associatie van observaties en als deze correlaties hoog zijn, deze observaties worden als gelijk gezien. Normale dichtheid is de standaardoptie.
- Schaal variabelen: Schaalt de continue variabelen. Standaardiseren zorgt dat waardes van variabelen met verschillende schalen, worden geschaald in een specifieke gelijke schaal. Hierdoor geeft standaardiseren numerieke stabiliteit, wat de uitvoer van het clusteren verbetert. JASP gebruikt de Z-score standaardisatie met een gemiddelde, 0, en een standaardafwijking van 1. Dit is de standaardoptie.
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor je analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse. Bijvoorbeeld, een oevalsgenerator beginwaarde maakt het mogelijk de analyse opnieuw te doen met dezelfde gesplitste data.

#### Voeg Voorspelde Klassen toe aan Data
Genereert een nieuwe kolom in je dataset met de klasselabels van je classificatie resultaat. Dit geeft je de mogelijkheid de gegenereerde klasselabels te inspecteren, classificeren, of voorspellen.

### Uitvoer
-------

#### Density-based Clustering Model Tabel
- De eerste kolom geeft het aantal gegenereerde clusters.
- N: De steekproefgrootte.
- R<sup>2</sup>: Geeft de hoeveelheid verklaarde variantie door het model.
- AIC: De AIC waarde van het model. Lage waardes betekenen beter clusterende output.
- BIC: De BIC waarde van het model. Lage waardes betekenen beter clusterende output.
- Silhouette: De Silhouette waarde van het model. De Silhouette waarde spreiden van -1 tot 1, waar 1 een perfecte score is.

#### Density-based Cluster Informatie
- Grootte: De grootte van elk cluster.
- Binnen kwadratensom: Toont de binnen kwadratensom van elk cluster. Dit is de standaardoptie.
- Silhouette score: Toont de silhouettescore van elke cluster.
- Tussen kwadratensom: Geeft de tussen kwadratensom van het clustermodel onder de clusterinformatie tabel.
- Totale kwadratensom: Geeft de totale kwadratensom van het clustermodel onder de clusterinformatie tabel.

#### Evaluatiemetrieken Tabel
- Maximum diameter: De maximum cluster diameter in *euclidische* afstand.
- Minimum scheiding: De minimum cluster scheiding in *euclidische* afstand.
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
- Kriegel, H.-P., Kröger, P., Sander, J., & Zimek, A. (2011). Density-based clustering. Wiley Inter- disciplinary Reviews: Data Mining and Knowledge Discovery, 1(3), 231–240.

### R-packages 
--- 
- cluster
- dbscan
- Rtsne

### Voorbeeld 
--- 
- Voor een dataset als voorbeeld ga naar `Open` --> `Bibliotheek` --> `Machine Learning` --> `Iris`.  

