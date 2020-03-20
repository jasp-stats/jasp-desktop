Exploratory Factor Analysis 
=== 

With Exploratory Factor Analysis it is possible to identify one or more factors underlying the data. The factors are chosen such that they capture the common variance in the data. 

### Assumptions (Yong & Pearce, 2013)
- The variables included in the analysis are continuous. 
- The data follow a multivariate normal distribution. 
- There is a linear relation between the variables and the factors. 
- There is no multicollinearity and singularity in the data. 

### Input 
---
#### Asssignment Box 
- Included Variables: In this box, the variables to perform the exploratory factor analysis on are selected. 

#### Number of Factors 
- Here, the number of factors that the rotation is applied to is specified. Several methods to determine this number can be chosen from:   
  - Parallell Analysis: Factors are selected on the basis of parallell analysis. With this method, factors are selected when their eigenvalue is bigger than the parallel average random eigenvalue. This method is selected by default. 
  - Eigenvalues: Factors are selected when they have a certain eigenvalue. By default factors are selected that have an eigenvalue of 1 or higher. This is called the Kaiser criterion. 
  - Manual: The number of factors can be specified manually. By default this is set to 1. 

#### Rotation 
- Here, the rotation method to apply to the factors can be specified.
  - Orthogonal: This method produces factors that are uncorrelated. For this method, there are several possibilities that can be selected: 
      - None: No rotation method is selected. 
      - varimax: Orthogonal rotation method varimax. This rotation is based on maximizing the variance of the loadings. 
      - quartimax: Orthogonal rotation method quartimax. For this method, the number of factors that is necessary to explain each variable is minimized. 
      - bentlerT: Orthogonal rotation method bentlerT. 
      - equamax: Orthogonal rotation method equamax. This is a combination of varimax and quartimax. 
      - varimin: Orthogonal rotation method varimin. 
  - Oblique: This method produces factors that allow for correlation between the factors. This method is selected by default. Several possibilities are available: 
      - promax: Oblique rotation method promax. This method is selected by default. 
      - oblimin: Oblique rotation method oblimin. 
      - simplimax: Oblique rotation method simplimax. 
      - bentlerQ: Oblique rotation method bentlerQ. 
      - biquartimin: Oblique rotation method biquartimin. 
      - cluster: Oblique rotation method cluster. 

### Output Options 
- Highlight: This option cuts the scaling of paths in width and color saturation. Paths with absolute weights over this value will have the strongest color intensity and become wider the stronger they are, and paths with absolute weights under this value will have the smallest width and become vaguer the weaker the weight. If set to 0, no cutoff is used and all paths vary in width and color.
- Include Tables: 
    - Factor correlations: When selecting this option, a table with the correlations between the factors will be displayed. 
    - Additional fit indices: This option displays the Root Mean Squared Error of Approximation (RMSEA) with 90% confidence interval, the Tucker Lewis Index (TLI), and the Bayesian Information Criterion (BIC) to test the fit of the model. 
    - Path diagram: By selecting this option, a visual representation of the direction and strength of the relation between the variable and factor will be displayed. 
    - Scree plot: When selecting this option, a scree plot will be displayed. The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. A scree plot can be used to decide how many factors should be selected. 
- Missing values: 
    - Exclude cases pairwise: If one observation from a variable is missing, all the other variable observations from the same case will still be used for the analysis. In this scenario, it is not necessary to have a observation for all the variables to include the case in the analysis. This option is selected by default. 
    - Exclude cases listwise: If one observation from a variable is missing, the whole case, so all the other connected variable observations, will be dismissed from the analysis. In this scenario, observations for every variable are needed to include the case in the analysis. 

### Output 
--- 
#### Exploratory Factor Analysis 
Factor Loadings:  
- Variables: The first column shows all the variables included in the analysis. 
- PC (1, 2, 3, etc.): This column shows the factor loadings on the variable. 
- Uniqueness: The percentage of the variance of each variable that is not explained by the factor. 

Factor Correlations:  
- The correlations between the factors. 

Chi-squared Test: 
The fit of the model is tested. When the test is significant, the model is rejected. Bear in mind that a chi-squared approximation may be unreliable for small sample sizes, and the chi-squared test may too readily reject the model with very large sample sizes. Additional information about the fit of the model can be obtained by selecting the option `Additional fit indices` in the `Output options`. See, for example, Saris, Satorra, & van der Veld (2009) for more discussions on overall fit metrics.
- Model: The model obtained from the exploratory factor analysis. 
- Value: The chi-squared test statistic.  
- df: Degrees of freedom. 
- p: P-value. 

Additional Fit Indices: 
These fit indices provide information about the fit of the model. 
- Model: The model obtained from the exploratory factor analysis. 
- RMSEA: Root Mean Square Error of Approximation. Corrects for parsimony. When models peform the same, but model 1 has more degrees of freedom than model 2, model 1 will be recommended. Browne and Cudeck (1993) advise a value less than 0.08 for an acceptable model fit, less than 0.05 a good model fit, and advice to reject models with values of 0.1 or higher. However, there is absolute agreement on these cutoffs. 
- RMSEA 90% confidence interval: The 90% confidence interval of the Root Mean Square Error of Approximation. 
- TLI: Tucker-Lewis Index. Evaluates the fit compared to a more resticted, nested baseline model. Hopwood and Donnallan (2010) suggested that a value higher than .9 indicates a good fit. However, there is no consensus about this cutoff. 
- BIC: Bayesian Information Criterion. This measure is useful for comparing the performances of different models on the same data, where a lower value indicates a better fitting model. 

#### Path Diagram 
- F(1,2,3,...): The factors in the model are represented by the circles.  
- Variables: The variables are represented by the boxes. 
- Arrows: Going from the factors to the variables, representing the loading from the factor on the variable. Red indicates a negative loading, green a positive loading. The wider the arrows, the higher the loading. This highlight can be adjusted at `Highlight` in the `Output Options`. 

#### Screeplot 
The scree plot provides information on how much variance in the data, indicated by the eigenvalue, is explained by each factor. The scree plot can be used to decide how many factors should be selected in the model. 
- Factors: On the x-axis, all possible factors. 
- Eigenvalue: On the y-axis, the eigenvalue that indicates the variance explained by each factor. 
- Data: The dotted line represents the data. 
- Simulated: The triangle line represents the simulated data. This line is indicative for the parallel analysis. When the points from the dotted line (real data) are above this line, these factors will be included in the model by parallel analysis. 
- Kaiser criterion: The horizontal line at the eigenvalue of 1 represents the Kaiser criterion. According to this criterion, only factors with values above this line (at an eigenvalue of 1) should be included in the model. 

### References 
---
- Brown, T. A. (2014). *Confirmatory factor analysis for applied research*.     
    Guilford Publications. 
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention     
    decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational research methods, 7*(2), 191-205.
- Hopwood, C. J., & Donnellan, M. B. (2010). How should the internal structure 
    of personality inventories be evaluated? *Personality and Social Psychology Review, 14*, 332–346. 
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in 
    exploratory factor analysis. *Best practices in quantitative methods*, 86-99.
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. Structural Equation Modeling, 16(4), 561-582.
- Yong, A. G., & Pearce, S. (2013). A beginner’s guide to factor analysis: Focusing on exploratory factor analysis. *Tutorials in quantitative methods for psychology, 9*(2), 79-94.

### R Packages 
--- 
- ggplot2
- psych
- qgraph
- stats

### Example 
---
- For an example go to `File`-->`Data library`-->`Factor`-->`G Factor`. 
- For more details about Exploratory Factor Analysis in JASP, watch this <a href="https://www.youtube.com/watch?v=dUPzMBqcMjo&feature=youtu.be">video</a>. 
