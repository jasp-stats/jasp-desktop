Density Based Clustering
==========================
Density-based clustering is a soft clustering method where clusters are constructed as maximal sets of points that are connected to points whose density exceeds some threshold. The density is produced by the concept that for each point within a cluster, the neighborhood within a given radius has to contain at least a minimum amount of points, that results in the density of that neighborhood to exceed a certain threshold. A density-based cluster is recognized by points having a higher density than points outside of the cluster. The set of all high-density points is called the density level. The points that do not exceed a density level are identified as outliers. The density level influences the amount of generated clusters. 

### Assumptions
- The data consists of continuous variables.
- (Normally distributed data aids the clustering process).

### Input 
-------
#### Assignment Box 
- Variables: In this box, the variables are need to be considered in the clustering algorithm should be entered. 

#### Tables  
- Cluster information: Displays the size of each cluster. This option is selected by default. 
- Within sum of squares: Displays the within sum of squares of each cluster. This option is selected by default.
- Silhouette score: Displays the silhouette score of each cluster.
- Between sum of squares: Notes the between sum of squares of the cluster model beneath the cluster information table.
- Total sum of squares: Notes the total sum of squares of the cluster model beneath the cluster information table.

#### Plots
- K-distance plot: Generates a plot with the nearest neighbors distance (the amount of nearest neighbors is determined by the Min. core points parameter) on the y-axis and the points sorted by distance on the x-axis. This plot can be used for determining the optimal Epsilon value. The value where the graph shows a kink represents the optimal Epsilon value (the y-axis value).
- T-sne cluster plot: Generates a T-sne plot of the clustering output. T-sne plots are used for visualizing high-dimensional data in a low-dimensional space of two dimensions aiming to illustrate the relative distances between data observations. The T-sne two-dimensional space makes the axes uninterpretable. A T-sne plot seeks to give an impression of the relative distances between observations and clusters
- Legend: Sets a legend showing the cluster number for each observation. This option is set by default.
- Labels: Shows the clustering labels of the different observations.

#### Training Parameters 
#### Algorithmic Settings
- Epsilon neighborhood size: Reflects the size of the radius wherein there must be a minimal amount of core points that results in the density of that neighborhood to exceed a certain threshold. By exceeding this threshold, point (i.e., observations) can generate a cluster.
- Min. core points: Reflects the minimal amount of points that need to be in the Epsilon neighborhood to let points form a cluster. The Eps and MinPts parameters determine the density level and regulate how many points need to be in a given radius to exceed a certain threshold for forming a cluster.
- Distance: Specify the used dissimilarity measurement. Normal density uses the geometric distance between two points and is entirely based on the magnitude of the distance. In contrast, correlated density is a correlation-based dissimilarity measurement, which examines the linear association observations and if these correlations are high, these observations are considered to be similar. Normal density is set as default.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same outcomes.

#### Add Predicted Clusters to Data
Generates a new column in your dataset with the cluster labels of your cluster result. This gives you the option to inspect, classify, or predict the generated cluster labels.

### Output
-------

#### Density-based Clustering Model Table
- The first column shows the number of generated clusters.
- N: The sample size.
- R<sup>2</sup>: Indicates the amount of variance explained by the model.
- AIC: The AIC value of the model. Lower values represent better clustering outputs.
- BIC: The BIC value of the model. Lower values represent better clustering outputs.
- Silhouette: The Silhouette value of the model. The silhouette value ranges from -1 to 1, where 1 represents a perfect score.

#### Density-based Cluster Information
- Size: The size of each cluster.
- Within sum of squares: The within sum of squares of each cluster.
- Silhouette: The Silhouette value of each cluster.
- Between sum of squares: The between sum of squares of the model noted underneath the table.
- Total sum of squares: The total sum of squares of the model noted underneath the table.

### References
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
- colorspace
- dbscan
- e1017
- ggdendro
- Rtsne

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Iris`.  

