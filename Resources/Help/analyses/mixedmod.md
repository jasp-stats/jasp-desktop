Mixed Modeling 
=== 

Mixed Modeling allows users to both statistically and visually model relationships between variables, while also allowing the user to perform diagnostics of the statistical model. This module allows users to model both fixed and random effects. 

### Input 
--- 
#### Assignment Box 
- Dependent Variable: The variable whose scores one is interested in modeling. Also called the outcome or response variable. Only numeric variables are allowed. 
- Independent Variable(s): The variable(s) for which we wish to assess relationships with the DV. Also called the predictors variable(s). This can be either a numeric variable or a grouping variable (e.g., a factor in an ANOVA). In other words, this module makes no distinction between categorical/manipulated variables and numeric variables. 
- Random: The variable (e.g., ID or school) that indicates which observervations belong to the same cluster. 

### Model 
- Components and model terms: 
    - Components: All the predictors that can be included in the model. 
    - Model terms: The independent variables and covariates included in the model. By default, all the main effects are included in the model.
    - Add as a random effect: Users can check on particular variables to specify whether to model a random slope for that particular variable. 

### Results Displays
- Plots:
    - Model plot: Should the results display a visualization of the statistical model? Defaults to yes. These plots are created automatically in the background by flexplot. The different clusters are presented either in separate panels or as separate lines/colors/symbols. 
    - Univariate: Checking this box will show univariate distributions of all variables, both predictor and outcome. All numeric variables will be displayed as histograms, while all categorical variables as barcharts. 
    - Diagnostics: Checking this box will produce residual plots for diagnostics, including a histogram of the residuals to assess normality, a residual dependence plot (fitted versus observed) to assess linearity, and a scale-location (SL) plot to assess homoscedasticity. 
- Estimation:
    - Report fixed effects: Checking this box will report the fixed effects (slopes and intercept) for each effect. 
    - Report random effects: Checking this box will display the variance of the random effects. 
      
  
### Plot controls
  - Point controls: 
    - Point transparency: The degree of transparency of the dots in the graphics
    - Jitter in X: The maximal amount of "jittering" used on the X axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap. 
    - Jitter in Y: The maximal amount of "jittering" used on the Y axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap. 
  - Other parameters: 
    - GGplot theme: the type of GGplot theme to use when displaying the data. Can be one of the following:
      - JASP
      - Black and white
      - Minimal
      - Classic
      - Dark
    - Number of clusters: Changing this value will alter the number of clusters to visually display. Defaults to 3. 




### References 
--- 
-	Fife, D.A., (in press). The Eight Steps of Data Analysis: A Graphical Framework to Promote Sound Statistical Analysis. *Perspectives on Psychological Science.* doi: 10.31234/osf.io/r8g7c
- Fife, D.A., (2020). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3


### R Packages
---
- ggplot2
- flexplot
- cowplot
- ggplot2 
- MASS 
- tibble
- withr 
- dplyr 
- magrittr
- forcats 
- purrr
- plyr
- R6
- lme4

### Example 
--- 
- For more details about flexplot in JASP, watch this <a href="https://www.youtube.com/watch?v=N2vM74rw6-Q&list=PL8F480DgtpW8pF6MmNaEUR95n1RmIgasP&feature=youtu.be">video</a>.
