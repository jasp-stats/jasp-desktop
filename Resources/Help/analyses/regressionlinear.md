Linear Regression
==========================

Linear regression allows you to model a linear relationship between one or more explanatory variable(s) (predictors) and a continuous dependent (response) variable.

Assumptions
-----------
- Continuous response variable
- Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.
- Independence of errors: The errors are uncorrelated with each other.
- Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.
- Normality of errors: The errors are normally distributed with mean zero.

Assignment Box
-------
- Dependent: Dependent (response) variable
- Blocks: Specify the order in which the predictors are entered into the model (i.e., hierarchical regression analysis). A block of one or more predictor(s) represents one step in the hierarchy.
        Note that the present release does not allow for more than one block.
	- Enter: All predictors are entered into the model simultaneously (forced entry).
	- Backward: All predictors are entered simultaneously, and then removed sequentially based on the criterion specified in "Stepping method criteria".
	- Forward: Predictors are entered sequentially based on the criterion specified in "Stepping method criteria".
	- Stepwise: Predictors are entered sequentially based on the criterion specified in "Stepping method criteria"; after each step, the least useful predictor is removed.
- WLS Weights: The weights used for weighted least square regression.
        Note that the present release does not support weighted least square regression.

Default Options
-------
### Statistics:
- Regression coefficients:
	- Estimates: Unstandardized and standardized coefficient estimates, standard errors, t-values, and their corresponding p-values
  - Model fit: Separate ANOVA table for each model (i.e., each step in Backward, Forward, and Stepwise regression)

### Options:
- Stepping Method Criteria:
  - Use p value: Use p-value as criterion for adding and removing predictors in Backward, Forward, and Stepwise regression.
    - Entry: Add predictor if p-value of regression coefficient < x; default is x=0.05.
    - Removal: Remove predictor if p-value of regression coefficient > x; default is x=0.1.
  - Use F value: Use F-value as criterion for adding and removing predictors.
    - Entry: Add predictor if F-value (t^2) of regression coefficient is > x; default is x=3.84.
    - Removal: Remove predictor if F-value (t^2) of regression coefficient is < x; default is x=2.71.

- Include constant in equation:
  - Include the intercept in the regression model.

- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.


Default Output
-------
### Model Summary:
- Model: Regression model (one for each step in Backward, Forward, and Stepwise regression)
- R: Multiple correlation coefficient R
- R squared: R squared value, i.e., proportion of the total variance that is explained by the regression model
- Adjusted R squared: Adjusted R squared value
- RMSE:	Root-mean-square error

### ANOVA:
- Model: Regression model (one for each step in Backward, Forward, and Stepwise regression)
- Sum of Squares: Sum of squares for the regression model (Regression) and the residual (Residual), and total sum of squares (Total)
- df: Degrees of freedom for the regression model (Regression) and the residual (Residual), and total degrees of freedom (Total)
- Mean Square: Mean squares for the regression model (Regression) and the residual (Residual)
- F: F-value
- p: p-value

### Coefficients:
- Model: Regression model (one for each step in Backward, Forward, and Stepwise regression)
- Unstandardized: Unstandardized regression coefficients
- Standard Error: Standard error of the regression coefficients
- Standardized: Standardized regression coefficients
- t-value: t-value for testing the null hypothesis that the population regression coefficient equals 0
- p: p-value

Example
-------
### Data set:
- Physical Activity and BMI

### Input:
- The null hypothesis that Physical Activity is unrelated to Body Mass Index (BMI) in college-aged females is tested against
the two-sided alternative that Physical Activity predicts BMI.

### Output:
- The null hypothesis that Physical Activity is unrelated to Body Mass Index (BMI) in college-aged females is rejected in
favor of two-sided alternative that Physical Activity predicts of BMI: unstandardized b=-0.655, t(98)=-4.135, p<.001.

Additional Options
-------
### Statistics:
- Regression coefficients:
  - Confidence intervals: Confidence intervals for the coefficients
  	- Interval: The coverage of the confidence interval in percentages; the default is 95%
  - Covariance matrix: Covariance matrix of the coefficients
  - R squared change: Change in R squared between the different steps in Backward, Forward, and Stepwise regression,  with corresponding significance test (i.e., F change value, df1, df2, p-value)
  - Descriptives: Samples size, sample mean, sample standard deviation, and standard error of the mean
  - Part and partial correlations: Semipartial and partial correlations
  - Collinearity diagnostics: Collinearity statistics, eigenvalues, condition indices, and variance proportions
  - Residuals:
    - Durbin-Watson: Durbin-Watson statistic to test the autocorrelation of the residuals
    - Casewise diagnostic: Casewise and summarized diagnostics for the residuals
      - Outliers outside x standard deviations: Shows diagnostics for cases where the absolute value of the standardized residual is larger than x; default is x=3
      - All cases: Show diagnostics for all cases

Additional Output
-------
### Descriptives:
- N: Sample size
- Mean: Sample mean
- SD: Sample standard deviation
- SE: Standard error of the mean

### Part And Partial Correlations: TBA

### Model Summary:
- R squared change: Change in R squared value
- F change: Change in F-value
- df1: Numerator degrees of freedom of F change
- df2: Denominator degrees of freedom of F change
- p: p-value for F change
- Durbin-Watson: Durbin-Watson statistic

### Coefficients:
- [lower]%: Lower bound of the user-defined x% confidence intervals for the regression coefficients
- [upper]%: Upper bound of the user-defined x% confidence intervals for the regression coefficients
- Collinearity Statistics:
  - Tolerance: Inverse of the Variance Inflation Factor (VIF)
  - VIF: Variance Inflation Factor; large values indicate multicollinearity

### Coefficients Covariance Matrix:
- Displays the covariance matrix of the coefficients of the predictors for each regression model considered (Model)

### Collinearity Diagnostics:
  - Displays for each regression model considered (Model) and for each element of the scaled uncentered cross-product matrix (Dimension):
    - Eigenvalue
    - Condition Index
    - Variance Proportions for each term in the regression equation

### Casewise Diagnostics:
 - For each flagged case (Case Number) displays:
    - Standardized (Std.) residual
    - The value on the dependent variable
    - Predicted Value
    - Residual

### Residual Statistics:
- Displays the minimum (Minimum), maximum (Maximum), mean (Mean), standard deviation (SD), and the sample size (N) for:
 - Predicted Value
 - Residual
 - Standardized (Std.) Predicted Value
 - Standardized (Std.) Residual

References
-------
- Field, A.P., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. London: Sage.
- Moore, D.S., McCabe, G.P., & Craig, B.A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W.H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Stevens, J.P. (2009). *Applied multivariate statistics for the social sciences (5th ed.)*. New York, NY: Routledge.
