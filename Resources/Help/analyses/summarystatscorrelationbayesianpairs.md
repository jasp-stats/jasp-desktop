Summary stats - Bayesian Correlation Pairs
==========================

The Bayesian Correlation Pairs allows you to test the null hypothesis that the population (Pearson product-moment correlation) between pairs of variables equals 0.

Input Box
------
- Sample size, n (minimum sample size is 2)
- Pearson's r

Default Options
-------
### Hypothesis:
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0
- Correlated positively: One-sided alternative hypothesis that the population correlation is higher than 0
- Correlated negatively: One-sided alternative hypothesis that the population correlation is lower than 0

### Bayes Factor:
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis
- Log(BF10): Natural logarithm of BF10

Default Output
-------
### Bayesian Pearson Correlations:
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0

Additional Options
-------
### Plots:
- Prior and posterior: Displays the prior and posterior density of the correlation under the alternative hypothesis
  - Additional info: Adds the Bayes factor; adds a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior density of the correlation

### Additional Output

- Prior and posterior: Displays the prior (dashed line) and posterior (solid line) density of the correlation under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at 0. The horizontal solid line represents the width of the 95% credible interval of the posterior
 - Additional info: Displays the Bayes factor; displays a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density

References
-------
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2017). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.
