Confirmatory Factor Analysis
==========================

Confirmatory factor analysis (CFA) models observed variables (indicators) as noisy manifestations of underlying latent variables (factors). JASP's CFA is built on `lavaan` (lavaan.org; Rosseel, 2012), an `R` package for performing structural equation modeling. See Brown (2014) or Kline (2015) for books on the topic of CFA.

Assignment Box
-------
In the assignment box, continuous variables in your dataset can be assigned to different factors. There is a minimum of one factor, and each factor needs to have at least two indicators. You can add factors by pressing the (+) button and remove factors by pressing the (-) button. You may rename factors by changing the name above the assignment boxes.

Second-order factor
-------
JASP allows the factors in turn to be indicators of a second-order factor. This can be specified by dragging the factor names to the second-order assignment box.

Model options
-------
- Include mean structure: display means for the indicators and, in the case of multi-group CFA, the means of the latent variables.
- Assume factors uncorrelated: set the correlation between the different latent variables to 0.
- Factor scaling: factors can be given a scale in one of three ways:
  - Factor variances (default): the factor has a fixed variance of 1
  - Marker variable: the factor has the same scale as its first indicator as its factor loading is fixed to 1
  - Effects coding: the mean of the factor loadings is fixed to 1. For more information on the interpretation of effects coding, see Veen (2018)
- Residual covariances: to allow for covariance between indicators not explained by their respective factor, for example because questions were phrased in a similar way, drag two variables to the right-side assignment box.

Additional output
-------
- Additional fit measures: select these to display the value of the various fit indices in the output pane
- Implied covariance matrix: show the covariance matrix implied by the model
- Residual covariance matrix: show the covariances between the indicators that remains after taking into consideration the model. A perfect model shows all 0s here. 
- Modification indices: Display MIs with a minimum cutoff. A MI shows how much the chi-square value of overall fit would change if the parameter in question is freed up. EPC shows the expected change of the parameter itself.
- Show lavaan syntax: Display the lavaan modeling syntax that would be needed to run the model in R

Multigroup CFA
------
- Grouping variable: Select a categorical variable here to create CFA models for each group 
- Invariance testing: Select a level of constraining parameters over the different groups.
  - configural: the different groups have the same CFA structure
  - metric: the factor loadings are constrained to be equal across groups
  - scalar: the factor loadings and means of the indicators are constrained to be equal across groups
  - strict: the factor loadings, means of the indicators, residual variances, and residual covariances are constrained to be equal across groups

Plots
-------
- Misfit plot: visually show the residual correlations (standardised residual covariance matrix) of the indicators
- Model plot: graphically show the estimated model structure

Advanced
-------
- Emulation: Emulate results from different software
- Error calculation: Change how standard errors for the parameters are computed
- Estimator: change the estimator for the CFA
- Standardization: display standardized parameters for different standardization schemes

References
-------
- Brown, T. A. (2014). Confirmatory factor analysis for applied research. _Guilford Publications_.
- Kline, R. B. (2015). Principles and practice of structural equation modeling. _Guilford publications_.
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. _Journal of Statistical Software, 48_(2), 1-36. URL www.jstatsoft.org/v48/i02/
- Veen, D., Little, T.D. & van de Schoot, R. (2018). Effects Coding as Unbiased Alternative to Scale Scores. (manuscript under review).

### R Packages
---
- ggplot2
- lavaan
- reshape2
- semPlot
- stats
