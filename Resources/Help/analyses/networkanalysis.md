#Network Analysis

Network Analysis allows you to analyze the network structure of your variables. Load your data of interest into the box `Variables`. If you'd like to compare networks for different groups, you can add a grouping variable to `Split by`. In networks, observed variables are referred to as nodes and estimated relations are called edges.

##Estimators

This analysis allows you to estimate not one type of network, but multiple. Supported networks are:

- Correlation Networks
- Partial correlation Networks
- EBICglasso Networks
- Huge
- Ising Networks
- Adaptive Lasso
- Mixed Graphical Models

An estimator can be selected by clicking on `Estimator`.

##Assumptions

Assumption vary per network analysis method. In general, relations among variables are assumed to be *linear*. 

Additional assumptions are required for each network:

- Correlations & Partial correlation networks: If you want to interpret edges with only significant (partial) correlation, your data must be normally distributed.
- EBICglasso: your data are normally distributed.
- Mixed Graphical Models: Your variables are either normally distributed, catgorical, or Poisson (counts). 


##Basic output


There are five options:

- Network plot: a plot of the estimated network.
- Centrality plot: a plot of centrality measures of the estimated network.
- Centrality table: a table containing the values of the centrality measures.
- Weights matrix: the estimates parameters, in the form of a matrix.
- Layout matrix: the layout for each node in the network plot.

Analysis options
-----------

Bootstrap options
-----------

To investigate the stability of estimated networks, check `Bootstrap Network` underneath `Estimator`. Doing so will automatically bootstrap the edges of estimated network and their centrality. Additional options can be specified, such as the number of bootstraps and the type of bootstrap.


##Graphical options

[//]: # (Goes from left column top to down, then right column top to down.)


To make Networks plots aesthetically pleasing, many options exist. 

####Layout
The layout of a network determines where the nodes are placed. By default the layout is set to `spring`, which implies the layout will be generated via the force-driven Fruchterman-Reingold algorithm (TODO: ref). Alternatively all nodes can be displayed in a circle by selecting the `circle` layout. A third option is called `data`, where you can specify a column with x-coordinates and a column with y-coordinates which will be used as positions for each node. 

#### Edges
`size`: A multiplier on edge size (i.e. 2 is twice as big).
`minimum`: The (absolute) minimum edge strength to be displayed.
`maximum`: The (absolute) minimum edge strength to be displayed.
`cut`: 
`show details`: If checked, `minimum`, `maximum`, and `cut` will be displayed on the network plot (if they were modified).
`color scheme`: What colors should be used for positive and negative edges?

#### Labels

#### Nodes
`size`: A multiplier on node size  (i.e. 2 is twice as big).
`color nodes by`: A categorical variable that indicates the group to which each variable belongs.
`color scheme`: What colors should be used for the coloring?

#### Legend

There are three options:
- Don't show the legend.
- Show the legend in all networks.
- Show the legend in a specified plot.




