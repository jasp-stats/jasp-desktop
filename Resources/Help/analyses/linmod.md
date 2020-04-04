Linear Modeling 
=== 

Linear Modeling allows users to both statistically and visually model relationships between variables, while also allowing the user to perform diagnostics of the statistical model

### Input 
--- 
#### Assignment Box 
- Dependent Variable: The variable whose scores one is interested in modeling. Also called the outcome or response variable. Only numeric variables are allowed. 
- Independent Variable(s): The variable(s) for which we wish to assess relationships with the DV. Also called the predictors variable(s). This can be either a numeric variable or a grouping variable (e.g., a factor in an ANOVA). In other words, this module makes no distinction between categorical/manipulated variables and numeric variables. 


### Model 
- Components and model terms: 
    - Components: All the predictors that can be included in the model. 
    - Model terms: The independent variables and covariates included in the model. By default, all the main effects are included in the model.
    - Add as a polynomial: Users can check on particular variables to specify that its relationship with the outcome requires a quadratic/cubic term. (See visual fitting section for how to fit one versus the other). 

  
### Visual Fitting 
- Fitted line: 
    - Regression: The displayed line is a regression line (e.g., y = b0 + b1x)
    - Quadratic: The displayed line adds a squared term to the regression line (e.g., y = b0 + b1x + b2x1^2). Only those terms selected in the model section will have a squared term. 
    - Cubic: The displayed line adds a squared and cubed term to the regression line (e.g., y = b0 + b1x + b2x1^2 + b3x1^3). Only those terms selected in the model section will have a cubed/squared term. 

### Results Displays
- Plots:
    - Model plot: Should the results display a visualization of the statistical model? Defaults to yes. These plots are created automatically in the background by flexplot. For more control of how the visuals are displayed, use the flexplot module. 
    - Univariate: Checking this box will show univariate distributions of all variables, both predictor and outcome. All numeric variables will be displayed as histograms, while all categorical variables as barcharts. 
    - Diagnostics: Checking this box will produce residual plots for diagnostics, including a histogram of the residuals to assess normality, a residual dependence plot (fitted versus observed) to assess linearity, and a scale-location (SL) plot to assess homoscedasticity. 
    - Added variable plot: Checking this box will display the relationship between the *last* variable entered the the outcome, after residualizing the effects of the other predictor variables entered. Note that the mean of the outcome variable will be added back to the residuals to retain the original scale of the data. 
- Estimation:
    - Show model comparisons: Checking this box will show nested model comparison metrics for each of the predictor variables. 
    - Report means: Checking this box will display the means of the outcome variable for each level of the grouping variables.
    - Show mean differences: Checking this box will display the mean differences between levels the grouping variables. Cohen's d will also be reported. 
    - Show slopes/intercepts: Checking this box will report the slope for each numeric variable as well as the intercept. 
    - Show 95% intervals: Checking this box will report the 95% confidence interval for various estimates. 
      


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

### Example 
--- 
- For more details about flexplot in JASP, watch this <a href="https://www.youtube.com/watch?v=N2vM74rw6-Q&list=PL8F480DgtpW8pF6MmNaEUR95n1RmIgasP&feature=youtu.be">video</a>.
