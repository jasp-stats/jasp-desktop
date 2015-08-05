Bayesian Linear Regression
==========================

Bayesian Linear regression allows you to model a linear relationship between one or more explanatory variable(s) (predictors) and a continuous dependent (response) variable.

Assumptions
-----------
- Continuous response variable 
- Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.
- Independence of errors: The errors are uncorrelated with each other.
- Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.
- Normality of errors: The errors are normally distributed with mean zero.

Assignment Box
-------
- Dependent Variable: Dependent (response) variable
- Covariates: Predictors

Default Options
-------
### Bayes Factor:
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis
- Log(BF10): Natural logarithm of BF10

### Prior:
- Cauchy prior width: Scale of the Cauchy prior density on effect size under the alternative hypothesis; the default is 0.5

Default Output
-------

### Model Comparison
- Models: The regression models considered; Contains the null model (intercept only, if not some covariates are declared as nuisance, see "Additional Options"), and all possible additive models.
- P(M): Prior probability of the model; by default, all models have equal prior probability.
- P(M|data): Probability of the model given the data; i.e., Posterior probability of the model
- BFM: Bayes factor in favor of the model relative to the remaining models
- BF10: Bayes factor in favor of the model compared to the null model (i.e., intercept only, if not some covariates are declared as nuisance, see "Additional Options").
- % error: Proportional error of the computation of the Bayes factor.

Example
-------
### Data set: 
- Physical Activity and BMI

### Input: 
- The null hypothesis that Physical Activity is unrelated to Body Mass Index (BMI) in college-aged females is tested against
the two-sided alternative that Physical Activity predicts BMI.

### Output: 
- The Bayes factor in favor of including physical activity as a predictor in the regression model as opposed to not including physical activity as a predictor is 284.3 with error % 0.002. The data are 284.3 times more likely when including physical activity as a predictor in the regression model.



### Output
- Effects: Displays the analysis of effects.
    - Stepwise: Displays the Bayes factors and % errors for a forward and backward stepwise analysis of effects.
 
 
 
### Analysis of Effects
- Effects: The different covariates for which the effect analysis is displayed.
- P(incl): Prior probability of including the covariate in the regression model; is by default 0.5 (i.e., equally likely to include and not to include).
- P(incl|data): Posterior probability of including the covariate in the regression model (i.e., probability of inclusion given the data).
- BFInclusion: Bayes factor in favor of including the covariate.
- BFBackward: Bayes factor in favor of including the covariate when using the backward stepwise procedure.
- % errorB: Proportional error of the computation of BFBackward.
- BFForward: Bayes factor in favor of including the covariate when using the forward stepwise procedure.
- % errorF: Proportional error of the computation of BFForward.

Additional Options
-------
### Model:
- Factors: The covariates. Can be manually moved to model terms if they are not already inlcuded in the model.
- Model Terms: The model terms. Can be manually removed.
- Is Nuisance: Allows to declare a covariate as nuisance. The covariate is then included in all models and is not displayed in the output tables (a footnote is displayed which tells that the covariate is included in all models).

References
-------
- Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J.O. (2008). Mixtures of g-priors for Bayesian variable selection. *Journal of the American Statistical Association, 103*, 410-423.
- Rouder, J. N., & Morey, R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Zellner, A., & Siow, A. (1980) Posterior odds ratios for selected regression hypotheses. In J. M. Bernardo, D. V. Lindley, & A. F. M. Smith (Eds), *Bayesian statistics: Proceedings of the first international meeting held in Valencia (Spain)* (pp. 585-603). University of Valencia.