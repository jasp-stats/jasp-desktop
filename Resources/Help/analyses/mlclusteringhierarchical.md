Hierarchical Clustering
==========================

Hierarchical clustering is a hard partitioning algorithm which aims to partition data into several clusters, where each observation belongs to only one group. The data is divided in such a way that the degree of similarity between two data observations is maximal if they belong to the same group and minimal if not.

### Assumptions
- The data consists of continuous variables.
- (Normally distributed data aids the clustering process).

### Input 
-------
#### Assignment Box 
- Variables: In this box, the variables are selected that are included in the analysis. 

#### Training Parameters 
#### Algorithmic Settings
- Distance: specify the used dissimilarity measurement. The Euclidean distance uses the geometric distance between two points and is entirely based on the magnitude of the distance. In contrast, the Pearson correlation metric is a correlation-based dissimilarity measurement, which examines the linear association observations and if these correlations are high, these observations are considered to be similar. The Euclidean distance is set as default.
- Linkage: specify the used linkage measure. Single linkage uses the smallest distance between cases of clusters. Complete linkage uses the furthest distance between cases of clusters. Centroid linkage uses the centroids of clusters. Average linkage computes the distances between all cases between clusters. Average linkage is set as default. 
- Scale variables: scales the variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

#### Cluster Determination
- Fixed: enables you to generate a fixed amount of clusters. This allows you to generate your own specified number of clusters, and thus, optimize manually.
- Optimized according to: enables you to choose an optimization method. The options are AIC, BIC, and silhouette. The AIC uses the within sum of squares (within-cluster variation), the number of generated clusters and the number of dimensions for optimizing the clustering output. The BIC uses the within sum of squares (within-cluster variation), the number of generated clusters, the number of dimensions, and the sample size for optimizing the clustering output. The silhouette value uses the similarity of observations within a cluster and their dissimilarity to other clusters for optimizing the clustering output. BIC optimization is set as default.
- Max. clusters: sets the maximum number of possible clusters to be generated. At default, this is set to 10.

#### Tables  
- Cluster information: displays the size of each cluster. This option is selected by default. 
- Within sum of squares: displays the within sum of squares of each cluster. This option is selected by default.
- Silhouette score: displays the silhouette score of each cluster.
- Between sum of squares: notes the between sum of squares of the cluster model beneath the cluster information table.
- Total sum of squares: notes the total sum of squares of the cluster model beneath the cluster information table.

#### Plots
- Elbow method: generates a plot with the total within sum of squares on the y-axis and the number of clusters on the x-axis. This plot can be used for determining the optimal number of clusters. The plot shows three curves using AIC, BIC, and 'elbow method' optimization.
- Dendrogram: generates a dendrogram of the clustering output. A dendrogram can be interpreted as a tree upside down. At the bottom, there are the leaves, and as one goes up the tree, leaves merge into branches (i.e., groups of observations). The leaves correspond to an observation. Also, the height at which leaves merge together indicates the (dis)similarity between observations or groups of observations. The dendrogram is used to create clusters by creating a cut within the dendrogram. A dendrogram can be used for inventorying the clustering structure.
- T-sne cluster plot: generates a T-sne plot of the clustering output. T-sne plots are used for visualizing high-dimensional data in a low-dimensional space of two dimensions aiming to illustrate the relative distances between data observations. The T-sne two-dimensional space makes the axes uninterpretable. A T-sne plot seeks to give an impression of the relative distances between observations and clusters
- Legend: sets a legend showing the cluster number for each observation. This option is set by default.
- Labels: shows the clustering labels of the different observations.

#### Add Predicted Clusters to Data
Generates a new column in your dataset with the cluster labels of your cluster result. This gives you the option to inspect, classify, or predict the generated cluster labels.

### Output
-------

#### Hierarchical Clustering Model Table
- The first column shows the number of generated clusters.
- N: The sample size.
- R^2: indicates the amount of variance explained by the model.
- AIC: The AIC value of the model. Lower values represent better clustering outputs.
- BIC: The BIC value of the model. Lower values represent better clustering outputs.
- Silhouette: The Silhouette value of the model. The silhouette value ranges from -1 to 1, where 1 represents a perfect score.

#### Hierarchical Cluster Information
- Size: The size of each cluster.
- Within sum of squares:The within sum of squares of each cluster.
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
- For an example go to `Open` --> `Data Library` --> `Machine Learning` --> `Iris`.  

