Principal Component Analysis 
===

Principcal Component Analysis is used to represent the data in smaller components than the dataset originally consists of. The components are chosen such that they explain most of the variance in the original dataset. 

### Assumptions 
- The variables included in the analysis are correlated (Shlens, 2014). 
- The variables included in the analysis are linearly related (Shlens, 2014). 

### Input 
---
#### Asssignment Box 
- Included Variables: In this box, the variables to perform the principal component analysis on are selected. 

#### Number of Components  
- Here, the number of components that the rotation is applied to is specified. Several methods to determine this number can be chosen from:   
  - Parallell Analysis: Components are selected on the basis of parallell analysis. With this method, components are selected when their eigenvalue is bigger than the parallel average random eigenvalue. This method is selected by default. 
  - Eigenvalues: Components are selected when they have a certain eigenvalue. By default components are selected that have an eigenvalue above 1. 
  - Manual: The number of components can be specified manually. By default this is set to 1. 

#### Rotation  
- Here, the rotation method to apply to the components can be specified. Rotation ensures a simpler understanding of the data structure.
  - Orthogonal: This method produces components that are uncorrelated. For this method, there are several possibilities that can be selected: 
      - None: No rotation method is selected. 
      - varimax: Orthogonal rotation method varimax. Rotation based on the maximizing the variance of the loadings. 
      - quartimax: Orthogonal rotation method quartimax. In this rotation method, the number of components that is  necessary to explain each variable is minimized. 
      - bentlerT: Orthogonal rotation method bentlerT. 
      - equamax: Orthogonal rotation method equamax. This is a combination of varimax and quartimax. 
      - varimin: Orthogonal rotation method varimin. 
  - Oblique: This method produces components that allow for correlation between the components. This method is selected by default. Several possibilities are available:  
      - promax: Oblique rotation method promax. This method is selected by default. 
      - oblimin: Oblique rotation method oblimin. 
      - simplimax: Oblique rotation method simplimax. 
      - bentlerQ: Oblique rotation method bentlerQ. 
      - biquartimin: Oblique rotation method biquartimin. 
      - cluster: Oblique rotation method cluster. 

### Output Options 
- Highlight: This option cuts the scaling of paths in width and color saturation. Paths with absolute weights over this value will have the strongest color intensity and become wider the stronger they are, and paths with absolute weights under this value will have the smallest width and become vaguer the weaker the weight. If set to 0, no cutoff is used and all paths vary in width and color.
- Include Tables: 
    - Component correlations: When selecting this option, a table with the correlations between the components will be displayed. 
    - Path diagram: By selecting this option, a visual representation of the direction and strength of the relation between the variable and component will be displayed. 
    - Scree plot: When selecting this option, a scree plot will be displayed. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each component. A scree plot can be used to decide how many components should be selected.  
- Missing values: 
    - Exclude cases pairwise: If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have a observation for all the variables to include the case in the analysis. This option is selected by default. 
    - Exclude cases listwise: If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. 

### Output 
--- 
#### Principal Component Analysis 
Chi-squared Test: 
The fit of the model is tested. When the test is significant, the model is rejected. Bear in mind that a chi-squared approximation may be unreliable for small sample sizes, and the chi-squared test may too readily reject the model with very large sample sizes. See, for example, Saris, Satorra, & van der Veld (2009) for more discussions on overall fit metrics.
- Model: The model obtained from the principal component analysis. 
- Value: The chi-squared test statistic.  
- df: Degrees of freedom. 
- p: P-value. 

Component Loadings:  
- Variables: The first column shows all the variables included in the analysis. 
- PC (1, 2, 3, ...): This column shows the variable loadings on the components. 
- Uniqueness: The percentage of the variance of each variable that is not explained by the component. 

Component Characteristics:
- Eigenvalues: The eigenvalue for each selected component
- Proportion var.: The proportion of variance in the dataset explained by each component
- Cumulative: The proportion of variance in the dataset explained by the components up to and including the current component.

Component Correlations: 
- The correlation between the principal components. 

#### Path Diagram 
- PC: The principal components are represented by the circles. 
- Variables: The variables loadings on the components are represented by the boxes. 
- Arrows: Going from the variables to the principal components, representing the loading from the variable on the component. Red indicates a negative loading, green a positive loading. The wider the arrows, the higher the loading. This highlight can be adjusted at  `highlight` in the `Output Options`. 

#### Screeplot 
The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each component. The scree plot can be used to decide how many components should be selected in the model. 
- Components: On the x-axis, the components. 
- Eigenvalue: On the y-axis, the eigenvalue that indicates the variance explained by each component. 
- Data: The dotted line represents the data. 
- Simulated: The triangle line represents the simulated data. This line is indicative for the parallel analysis. When the points from the dotted line (real data) are above this line, these components will be included in the model by parallel analysis. 
- Kaiser criterion: The horizontal line at the eigenvalue of 1 represents the Kaiser criterion. According to this criterion, only components with values above this line (at an eigenvalue of 1) should be included in the model. 

### References 
--- 
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational research methods, 7*(2), 191-205.
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). *An introduction to statistical learning* (Vol. 112, p. 18). New York: springer.
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in exploratory factor analysis. *Best practices in quantitative methods*, 86-99.
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. Structural Equation Modeling, 16(4), 561-582.
- Shlens, J. (2014). A tutorial on principal component analysis. *arXiv preprint arXiv:1404.1100*.

### R Packages 
--- 
- psych 
- qgraph 

