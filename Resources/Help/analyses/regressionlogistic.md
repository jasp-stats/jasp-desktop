Logistic Regression
===
 
Logistic regression allows the user to model a linear relationship between one or more explanatory variable(s) (predictors) and a categorical dependent (response) variable.
 
### Assumptions
- The dependent variables are categorical.
- The dependent variable is linearly related to all predictors and the effects of the predictors are additive.
- The assumption of homoscedasticity is met. Homoscedasticity entails that the error variance of each predictor is constant across all values of that predictor.
- The residuals are uncorrelated with each other.
- The residuals are normally distributed with a mean zero.
- The covariate and the experiment effect are independent. 
 
### Input
---
 
#### Assignment box
- Dependent Variable: The variable of interest. This is also called the outcome variable. In case of multiple dependent variables, specify the order in which the predictors are entered into the model (i.e., hierarchical regression analysis). A block of one or more predictor(s) represents one step in the hierarchy.
  - Enter: All predictors are entered into the model simultaneously (forced entry).
  - Backward: All predictors are entered simultaneously, and then removed sequentially based on the criterion specified in "Stepping method criteria".
  - Forward: Predictors are entered sequentially based on the criterion specified in "Stepping method criteria".
  - Stepwise: Predictors are entered sequentially based on the criterion specified in "Stepping method criteria"; after each step, the least useful predictor is removed.
- Covariates: In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation. 
- Factors: The variables that are manipulated/define the different groups. These are also called the independent variables.
 
### Model
- Components and model terms:
    - Components: All the independent variables and covariates that can be included in the model.
    - Model terms: The independent variables and covariates included in the model. By default, all the main effects and interaction effects of the specified independent variables, and the covariates are included in the model. 
    - Add to null model: The independent variables included in the model can be selected to add to the null model. 
- Include intercept: Ticking this box will add a coefficient estimate of the intercept as well. This corresponds to the first (=reference) level for the independent variable.

 
### Statistics
- Descriptives:
  - Factor descriptives: The levels of the dependent variable(s) and the number of observations per level.
 
- Regression Coefficients:
  - Estimates: Coefficient estimates, standard errors, z-values, and their corresponding p-values.
     - From `...` bootstraps: By selecting this option, bootstrapped estimation is applied. By default, the number of replications is set to 1000. This can be changed into the desired number. 
  - Standardized coefficients: Standardized estimates represent estimates were the predictors are standardized (X-standardization).
  - Odds ratio: Odds ratio is an indicator of the change in odds resulting from a unit change in the predictor. (Field)
  - Confidence intervals: Coverage of the confidence intervals in percentages. The default value is 95.
    - Odds ratio scale: proportionate change in odds by dividing the odds after a unit change in the predictor by the odds before that change (Field)
  - Robust Standard errors: this option controls for errors that are not independent and identically distributed. The use of robust standard errors will not change the coefficient estimates. If this option is not selected the normal standard error will be computed. 
  - Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.
 
- Residuals:
  - Casewise diagnostics: Casewise and summarized diagnostics for the residuals.
    - Standard residual > 3: Outliers outside x standard deviations: Shows diagnostics for cases where the absolute value of the standardized residual is larger than x; default is x=3.
    - Cook's distance > 1 : Shows diagnostics for cases where the value of Cook’s distance is larger than x; default is x = 1. 
    - All: Show diagnostics for all cases. 
 
- Performance Diagnostics:
  - Confusion matrix: The confusion matrix indicates how well the model predicts the outcomes. The table is showing actual versus predicted outcomes and can be used to determine the accuracy of the model.
    - Proportions: The table is showing the proportions of actual versus predicted outcomes
 
- Performance metrics:
  - AUC: Area Under the Curve.
  - Sensitivity / Recall: Sensitivity describes the proportion of true positive.
  - Specificity: Specificity describes the proportion of true negatives.
  - Precision: Precision describes the proportion of true positives of all positives. Also called the positive predictive value.
  - F-measure: This is based on the amount of systematic variance divided by the amount of unsystematic variance (i.e., mean squares for the model / the residual mean squares)
  - Brier score: Another measure of the accuracy of predictions.
  - H-measure: Another measure of the accuracy of predictions. The default implementation in the package hmeasure on CRAN.
 
### Plots
- Interferential plots:
  - Display conditional estimates plots: The plots are conditional in the sense that they display the probability of the categorical dependent (response) variable for all levels of the predictor variables given the reference level of all other factors.
    - Show data points.
 
- Residual plots: If the assumptions of the linear regression model are tenable, then these residuals should scatter randomly about a horizontal line. Any systematic pattern or clustering of the residuals suggests a model violation(s).
  - Predicted - residuals plot: Scatterplot of the values of the residuals against the predicted values. 
  - Predictor - residual plot: Scatterplot for every independent variable and covariate of the residuals and the levels of the variable of interest. 
  - Squared Pearson residuals: With the Squared Pearson residuals plot one can check for overdispersion of the model. Overdispersion indicates that the actual data show greater variability than the model has predicted.  
 
- Residual type:
  - Deviance: The standardized deviance residuals.
  - Pearson: The standardized Pearson residuals.
 
### Output
---

#### Logistic regression
Model summary:
- Model: The different hypotheses that are compared.
- Deviance: -2 x log-likelihood.
- AIC (Akaike Information Criteria): Compare models using the Akaike Information Criterion.
- BIC (Bayesian Information Criteria): Compare models using the Bayesian Information Criterion.
- df: Degrees of freedom.
- X2: chi-squared.
- p: The p-value.
- R squared value, i.e., the proportion of the total variance that is explained by the regression model. There are three pseudo R2 values calculated in JASP.
  - McFadden: calculated as one minus the ratio of the log-likelihood of the specified model to the log-likelihood of the null model. If the specified model fits the data relatively better than the null model, McFadden's R2 is close to 1. If the null model fits the data about the same as the specified model, McFadden's R2 is close to 0.
  - Cox & Snell: calculated as one minus the ratio of the likelihood of the null model to the likelihood of the specified model, with the ratio raised to the power of 2/n (sample size). Higher values indicate that the specified model fits the data relatively better than the null model. However, this index is bounded at one minus the likelihood of the null model raised to the power of 2/n, and under ideal circumstances can be only as high as 0.75.
  - Nagelkerke: provides a correction to the Cox & Snell R2 so that it is bounded at 1. Specifically, it is calculated as the Cox & Snell R2 divided by one minus the likelihood of the null model raised to the power of 2/n. Values closer to one indicate that the specified model outperforms the null model.
  - Tjur: calculated as the absolute value of the difference between the mean average predicted value for all cases with zero and the mean average predicted value for all cases with one. Values close to one indicate clear separation between the predicted values for cases with zeros and cases with ones. Unlike the other pseudo R2 indices, Tjur's R2 is not relative to the null model.
 
Coefficients:
- Estimate: regression coefficients.
- (Robust) Standard Error: Standard error of the regression coefficients.
- Standardized: Standardized regression coefficients.
- Odds Ratio: The most important values in the coefficients table are the odds ratios. For the continuous predictor, an odds ratio of greater than 1 suggests a positive relationship while < 1 implies a negative relationship.
- z: The z-value.
- Wald Test: The wald test is used to evaluate the statistical significance of each coefficient in the model.
  - Wald statistics: z^2.
  - df: Degrees of freedom.
  - p: The p-value.
- VS-MPR: Vovk-Sellke maximum p-ratio.
- 95% Confidence Interval (odds ratio scale)
- [lower]%: Lower bound of the user-defined x% confidence intervals for the regression coefficients.
- [upper]%: Upper bound of the user-defined x% confidence intervals for the regression coefficients.
   
Bootstrap Coefficients:
- Estimate: bootstrapped regression coefficients.
- Bias: Estimation of the bias.
- Standard Error: Standard error of the bootstrapped regression coefficients.

Casewise Diagnostics:
- Case number: Identification of the case being inspected.  
- Observed: The observed value of the outcome.
- Predicted: The predicted value of the outcome.
- Predicted Group: The predicted dependent value of the outcome.
- Residual: The difference between the observed and the predicted value.
- Standardized Residual: The standardized difference between the observed and the predicted value.
- Cook’s Distance: The value of Cook’s distance. 
- 95% Confidence Interval
  - [lower]%: Lower bound of the user-defined x% confidence intervals for the bootstrapped regression coefficients.
  - [upper]%: Upper bound of the user-defined x% confidence intervals for the bootstrapped regression coefficients.

Factor Descriptives:
  - The first column displays all levels of the factor.
  - N: The amount of observations per level of the factor.
   
#### Performance Diagnostics
Confusion Matrix: 
- The confusion matrix indicates how well the model predicts the outcomes. In the diagonal the cases that the model correctly identified are shown. The off-diagonal displays cases where the model predicted an incorrect outcome. 
 
Performance metrics: 
- All selected performance metrics and their values are displayed in this table. 
 
#### Estimates Plots
The conditional estimates plots display the probability of the dependent variable for all levels of the covariate given the reference of all other factors. If a (continues) covariate is added the grey shade around the line represents the 95% confidence intervals.
 
#### Residual Plots
Predicted - residuals plot.

Predictor - residuals plot for predictor. 

Squared Pearson residuals plot:
- The expected value of the squared residuals is 1 displayed by the dotted gray line. The red line displays the smoother through the residuals (= moving average). If the red line lies mostly near 1, it can be concluded that the model does not suffer much from overdispersion. Some deviation around the tails is to be expected.

### References
-------
- Field, A.P., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. London: Sage.
- Moore, D.S., McCabe, G.P., & Craig, B.A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W.H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Stevens, J.P. (2009). *Applied multivariate statistics for the social sciences (5th ed.)*. New York, NY: Routledge.

### R Packages
-------
- boot
- ggplot2
- hmeasure
- MASS
- matrixStats
- mdscore
- stats
