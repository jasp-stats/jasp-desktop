Summary Statistics Bayesian Linear Regression
==================

Bayesian Linear regression allows you to model a linear relationship between one or more explanatory variable(s) (predictors) and a continuous dependent (response) variable. This analysis, based on the classical (unadjusted) R^2 statistic, allows you to compute the corresponding Bayes factor test. The Bayes factor is computed using Gaussian quadrature.

### Input options and restrictions
- If only alternative model values are specified, then the analysis uses the default null model to calculate the Bayes factor
- Model comparison - both models are specified.
    - If number of covariates for alternative model is higher than the number of covariates for the null model, then R^2 value for null model should be less than that of alternative model.
- If only null model values are specified, the analysis is not carried out.    

### Input
---
#### Assignment Box
- Sample size
- Null model
    - *Covariates*: Number of predictors in model (excluding intercept).
    - *Unadjusted R-squared*: Proportion of variance accounted by the predictors.
- Alternative model
    - *Covariates*
    - *R-squared*

#### Bayes Factor
- *BF10*: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- *BF01*: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- *Log(BF10)*: Natural logarithm of BF10.

#### Plots
- Bayes factor robustness plot: Displays the Bayes factor as a function of the width of the Cauchy prior on effect size. The scale of the Cauchy prior is varied between 0 and 1.5 (between 0 and 2 if user prior width is greater than 1.5), creating progressively more uninformative priors.

### Advanced Options
- Prior
  - *Cauchy prior width*: Scale of the Cauchy prior density on effect size under the alternative hypothesis; the default is 0.5.

### Output
--- 
#### Model Comparison
- If Null model not specified (default option)
    - **n**: sample size
    - **Number of covariates**
    - **unadjusted R^2**
    - **BF10**: Bayes factor in favor of the model compared to the null model (i.e., intercept only).
    - **% error**: Proportional error of the computation of the Bayes factor.
- If Null model is specified, above output is shown for both the null and alternative models.

### References
---
- Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J.O. (2008). Mixtures of g-priors for Bayesian variable selection. *Journal of the American Statistical Association, 103*, 410-423.
- Rouder, J. N., & Morey, R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Zellner, A., & Siow, A. (1980) Posterior odds ratios for selected regression hypotheses. In J. M. Bernardo, D. V. Lindley, & A. F. M. Smith (Eds), *Bayesian statistics: Proceedings of the first international meeting held in Valencia (Spain)* (pp. 585-603). University of Valencia.
- Perception and Cognition Lab (University of Missouri): Bayes factor calculators. http://pcl.missouri.edu/bayesfactor

### R Packages
---
- BayesFactor
- stats
