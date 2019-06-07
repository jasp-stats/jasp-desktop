Bayesian Correlation Pairs
===

The Bayesian Correlation Pairs allows estimation of the population correlation, as well as testing the null hypothesis that the population correlation between pairs of variables equals 0. Specified pairs of variables are analyzed.

### Assumptions (Pearson's rho)
- Continuous variables
- The data are a random sample from the population
- The pairs of variables follow a bivariate normal distribution in the population
- The relationship between the pairs of variables is linear

### Assumptions (Kendall's tau)
- Ordinal or continuous variables
- The data are a random sample from the population
- The relationship between the pairs of variables is monotonic

### Input
---

##### Correlation Coefficient
- Pearson's rho: Pearson's product moment correlation coefficient. 
- Kendall's tau-b: Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables.

##### Alt. Hypothesis
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0.
- Correlated positively: One-sided alternative hypothesis that the population correlation is greater than 0.
- Correlated negatively: One-sided alternative hypothesis that the population correlation is lower than 0.

#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will show evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub> : By selecting this option, the Bayes factor will show evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.

#### Prior
- Stretched beta prior width: Width of the scaled beta distribution on the correlation under the alterative hypothesis; default is 1. The lower this value, the more concentrated the prior density is around 0. Value must be between 0 and 2.

#### Additional Options
- Credible intervals: Display central 95% credible intervals for the correlation coefficient.

#### Plots
- Scatterplot: Display scatterplots for each specified pair of variables.
- Prior and posterior: Displays the prior and posterior distribution of the correlation under the alternative hypothesis.
  - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.
- Bayes factor robustness check: Displays the Bayes factor as a function of the width of the stretched beta prior on the correlation. The width of the beta prior is varied between 0 and 2.
  - Additional info: Adds the Bayes factor computed with the user-defined prior and the maximum obtainable Bayes factor. 
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior.

#### Missing Values
 - Exclude cases analysis by analysis: In case of multiple correlations tests within a single analysis, each test will be conducted using all cases with valid data for the variables for the particular test. Sample sizes may therefore vary across the multiple correlation tests.
 - Exclude cases listwise: In case of multiple correlation tests within a single analysis, each test will be conducted using only cases with valid data for all variables. Sample size is therefore constant across the multiple correlation tests.

### Output
---
#### Bayesian Pearson Correlations
- Pearson's r: Pearson product-moment correlation coefficient.
- Kendall's tau: Kendall's rank correlation coefficient.
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0.
- Credible interval: Central credible intervals for the correlation coefficient.

#### Plots
Scatterplot:
- Displays the scatterplot of the correlation pairs.

Prior and posterior: 
- Displays the prior (dashed line) and posterior (solid line) distribution of the correlation under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at correlation = 0. The horizontal solid line represents the width of the 95% credible interval of the posterior distribution.
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior distribution.

Bayes factor robustness check: 
- Displays the Bayes factor as a function of the width of the beta prior on the correlation. 
  - Additional info: The red circle represents the maximum obtainable Bayes factor; the gray circle represents the Bayes factor computed with the user-defined prior.

Sequential analysis: 
- Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.

### References
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. *The American Statistician*,  72, 303-308.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.

### R Packages
---
- hypergeo
- ggplot2
- stats
