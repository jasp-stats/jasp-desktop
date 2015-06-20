
Bayesian Linear Regression
==========================

The Bayesian linear regression allows you to model a linear relation between one or more explanatory variable(s) and a continuous dependent variable.

Assumptions
-----------
- Linearity and additivity: The dependent variable is linearly related to any explanatory variable and the effects of the explanatory variables are additive.
- Independence of errors: The errors are uncorrelated with each other.
- Homoscedasticity: The error variance of every explanatory variable is constant across all values of that explanatory variable.
- Normality of errors: The errors are normally distributed with mean zero.

Assignment Box
-------
- Dependent Variable: Dependent variable.
- Covariates: Explanatory variables.

Options
-------
### Bayes Factor
- BF10: Bayes factor to quantify evidence in favor of the alternative hypothesis.
- BF01: Bayes factor to quantify evidence in favor of the null hypothesis.
- Log(BF10): Natural logarithm of BF10.

### Output
- Effects: Displays the analysis of effects.
    - Stepwise: Displays the Bayes factors and % errors for a forward and backward stepwise analysis of effects.
 
Output
-------

### Model Comparison
- Models: The different regression models. Contains the null model (intercept only, if not some covariates are declared as nuisance, see "Additional Options"), and all possible additive models.
- P(M): Prior probability of the models. By default, all models have equal prior probability.
- P(M|data): Posterior probability of the models (i.e., probability of the models given the data). 
- BFM: Bayes factor in favor of each model compared to the remaining models.
- BF10: Bayes factor in favor of each model compared to the null model (i.e., intercept only, if not some covariates are declared as nuisance, see "Additional Options").
- % error: Proportional error of the computation of the Bayes factor.

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
- Liang, F. and Paulo, R. and Molina, G. and Clyde, M. A. and Berger, J. O. (2008). Mixtures of g-priors for Bayesian Variable Selection. Journal of the American Statistical Association, 103, pp. 410-423.

- Rouder, J. N., & Morey, R. D. (2012). Default Bayes factors for model selection in regression. Multivariate Behavioral Research, 47(6), 877-903.

- Zellner, A. and Siow, A., (1980) Posterior Odds Ratios for Selected Regression Hypotheses. In Bayesian Statistics: Proceedings of the First Interanational Meeting held in Valencia (Spain). Bernardo, J. M., Lindley, D. V., and Smith A. F. M. (eds), pp. 585-603. University of Valencia.

