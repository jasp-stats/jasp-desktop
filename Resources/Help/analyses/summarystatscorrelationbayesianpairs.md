Summary Statistics Bayesian Correlation
==========

The Bayesian Correlation analysis allows you to test the null hypothesis that the population (Pearson product-moment correlation) between two variables equals 0.

### Input
---

#### Assignment Box
- *n*: Sample size (minimum: 2)

#### Sample Correlation coefficient
  - *Pearson's r*: The observed Pearson product-moment correlation coefficient
  - *Kendall's tau-b*: The observed Kendall's tau-b rank-order correlation coefficient.

#### Alt. Hypothesis
- *Correlated*: Two-sided alternative hypothesis that the population correlation does not equal 0.
- *Correlated positively*: One-sided alternative hypothesis that the population correlation is higher than 0.
- *Correlated negatively*: One-sided alternative hypothesis that the population correlation is lower than 0.

#### Bayes Factor
- *BF10*: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- *BF01*: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- *Log(BF10)*: Natural logarithm of BF10.

#### Plots
- *Prior and posterior*: Displays the prior (dashed line) and posterior (solid line) density of the correlation under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at 0. The horizontal solid line represents the width of the 95% credible interval of the posterior
  - Additional info: Displays the Bayes factor; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density
- *Bayes factor robustness plot*: Displays the Bayes factor as a function of the width of the Cauchy prior on effect size. The scale of the Cauchy prior is varied between 0 and 1.5 (between 0 and 2 if user prior width is greater than 1.5), creating progressively more uninformative priors.

#### Prior
*Stretched Beta prior width*: Default is 1

### Output
---
#### Bayesian Pearson Correlations
- **Bayes factor**: If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0
- **p**: p-value corresponding to t-statistic.

### References
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2017). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (in press). Bayesian Inference for Kendall's Rank Correlation Coefficient. *The American Statistician*.

### R Packages
---
- hypergeo
- ggplot2
- stats
