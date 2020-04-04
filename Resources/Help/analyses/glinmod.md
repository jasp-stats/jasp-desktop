Generalized Linear Modeling 
=== 

Generalized linear models (GLMs) relax the standard assumptions of normality, linearity, homoscedasticity, etc. In this model, users can model and visually display logistic regression models, poisson models, negative binomial, etc. 

### Input 
--- 
#### Assignment Box 
- Dependent Variable: The variable whose scores one is interested in modeling. Also called the outcome or response variable. Only numeric variables are allowed. 
- Independent Variable(s): The variable(s) for which we wish to assess relationships with the DV. Also called the predictors variable(s). This can be either a numeric variable or a grouping variable (e.g., a factor in an ANOVA). In other words, this module makes no distinction between categorical/manipulated variables and numeric variables. 
- Distribution family: The family of distribution the residuals are expected to take. Can be one of the following: 
  - Normal: Residuals are modeled to be gaussian (normal). The link function is the identity.
  - Logistic: Residuals are modeled to be binomial with a logit link function. 
  - Poisson: Residuals are modeled as count data using a poisson distribution, or integers with strictly positive values. The default link is the log. This distribution assumes the mean and variance are the same. 
  - Negative binomial: Residuals are modeled as count data using a negative binomial distribution, which relaxes the assumption that the mean and variance are the same. 
  - Gamma: Residuals are modeled under the assumption that the underlying distribution is a Gamma, which is a strictly positive continuous distribution, though it need not be integer. Often used for skewed continuous data. The default link is the inverse function. 

### Model 
- Components and model terms: 
    - Components: All the predictors that can be included in the model. 
    - Model terms: The independent variables and covariates included in the model. By default, all the main effects are included in the model.

### Results Displays
- Plots:
    - Model plot: Should the results display a visualization of the statistical model? Defaults to yes. These plots are created automatically in the background by flexplot. The different clusters are presented either in separate panels or as separate lines/colors/symbols. 
    - Univariate: Checking this box will show univariate distributions of all variables, both predictor and outcome. All numeric variables will be displayed as histograms, while all categorical variables as barcharts. 
- Estimation:
    - Show parameter estimates: Checking this box will report the parameter estimates from the model. It is important to note, however, these estimates will be reported for the linked data (e.g., for a poisson, the parameter estimates will be based on logged data). 
      
  
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
