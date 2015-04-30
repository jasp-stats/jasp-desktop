
Linear Regression
==========================

The linear regression allows you to model a linear relation between one or more explanatory variable(s) and a continuous dependent variable.

Assumptions
-----------
- Linearity and additivity: The dependent variable is linearly related to any explanatory variable and the effects of the explanatory variables are additive.
- Independence of errors: The errors are uncorrelated with each other.
- Homoscedasticity: The error variance of every explanatory variable is constant across all values of that explanatory variable.
- Normality of errors: The errors are normally distributed with mean zero.

Assignment box
-------
- Dependent: Dependent variable.
- Blocks: Allows to decide in which order the explanatory variables are entered into the model (i.e., hierarchical regression analysis). A block of one or more explanatory variable(s) represents one step in the hierarchy. 
	- Enter: All explanatory variables are entered into the model (forced entry).

Options
-------
### Statistics:
- Regression coefficients:
	- Estimates: Unstardardized and standardized coefficient estimates, standard errors, t-values and their corresponding p-values.
	
  - Confidence intervals: Lower and upper bound of confidence intervals for the coefficient estimates.
  	- Interval: Desired confidence interval %.
  	
  - Model fit: ANOVA output for the different steps in the hierarchical regression analysis.
  
  - R squared change: Change in R squared between the different steps in the hierarchical regression analysis with corresponding significance test (i.e., F change value, df1, df2, p-value).
 
Output
-------

### Model Summary
- Model: Each model corresponds to a step in the hierarchical regression analysis. In each step, variables of a block are entered into the model according to a certain criterion, e.g., "enter" (forced-entry).
- R: Multiple correlation coefficient R. 
- R squared: R squared value (proportion of the total variance that is explained by the regression model).
- Adjusted R squared: Adjusted R squared.
- RMSE:	Root-mean-square error.
- R squared change: Change in R squared.
- F change: F change value.
- df1: Numerator degrees of freedom of F change.
- df2: Denominator degrees of freedom of F change.
- p: p-value for F change.

### ANOVA
- Model: Each model corresponds to a step in the hierarchical regression analysis. In each step, variables of a block are entered into the model according to a certain criterion, e.g., "enter" (forced-entry).
- Sum of Squares: Sum of Squares for the regression model, residual, and total Sum of Squares.
- df: Degrees of freedom for the regression model, residual, and total degrees of freedom.
- Mean Square: Mean Squares for the regression model and residual Mean Squares.
- F: F value.
- p: p-value.

### Coefficients
- Model: Each model corresponds to a step in the hierarchical regression analysis. In each step, variables of a block are entered into the model according to a certain criterion, e.g., "enter" (forced-entry).
- Unstardardized: Unstandardized regression coefficients.
- Standard Error: Standard errors of the regression coefficients.
- Standardized: Standardized regression coefficients.
- t-value: t-values for test whether regression coefficients differ significantly from zero.
- p: p-values.
- 2.5%: Lower bound of confidence intervals for the regression coefficients (note: the width of the confidence intervals can be specified by the user via "Interval").
- 97.5%: Upper bound of confidence intervals for the regression coefficients (note: the width of the confidence intervals can be specified by the user via "Interval").

Example
-------

### Data set: 
- Physical Activity and BMI

### Input: 
- The regression analysis investigates whether physical activity is a significant predictor of body mass index (BMI).

### Output: 
- Physical activity is a significant predictor of body mass index (BMI): b=-0.655 (t=-4.135, p<.001).

References
-------

- Field, A.P., Miles, J., & Field, Z. (2012). Discovering statistics using R. London: Sage.
- Moore, McCabe, Craig (2012). Introduction to the Practice of Statistics (7th ed.). New York, NY: W.H. Freeman and Company, Chapter 10, p. 536; from Mestek, M.L, Plaisance E., & Grandjean, P. (2008). The relationship between pedometer-determined and self-reported physical activity and body composition variables in college-aged men and women, Journal of American Collage Health, 57, 39-44.
- Stevens, J.P. (2009). Applied multivariate statistics for the social sciences (5th ed.). New York, NY: Routledge. 
