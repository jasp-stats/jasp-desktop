Bayesian Correlation Pairs
==========================

The Bayesian Correlation Pairs allows you to test the null hypothesis that the population (Pearson product-moment correlation) between pairs of variables equals 0.

Assumptions
-----------
- Continuous variables
- The data are a random sample from the population
- The pairs of variables follow a bivariate normal distribution in the population

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
- Pearson's r: Pearson product-moment correlation coefficient
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0

### Missing Values:
 - Exclude cases analysis by analysis: In case of multiple correlations tests within a single analysis, each test will be conducted using all cases with valid data for the variables for the particular test.
 Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple correlation tests within a single analysis, each test will be conducted using only cases with valid data for all variables. Sample size is therefore constant across the tests.

### Output:
- The Pearson correlation between "Value" and "Revenue" equals -0.323; the data are 1.039501 times more likely under the null hypothesis that the population correlation equals 0
than under the two-sided alternative hypothesis that the population correlation does not equal 0, BF10 = 0.962.
- The Pearson correlation between "Value" and "Debt" equals 0.986; the data are 3.772e+19 times more likely under the two-sided alternative hypothesis that the population correlation does
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 3.772e+19.
- The Pearson correlation between "Value" and "Income" equals 0.718; the data are 2995.516 times more likely under the two-sided alternative hypothesis that the population correlation does
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 2995.516.
- The Pearson correlation between "Revenue" and "Debt" equals -0.339; the data are 1.126 times more likely under the two-sided alternative hypothesis that the population correlation does
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 1.126.
- The Pearson correlation between "Revenue" and "Income" equals -0.301; the data are 1.267427 times more likely under the null hypothesis that the population correlation equals 0
than under the two-sided alternative hypothesis that the population correlation does not equal 0, BF10 = 0.789.
- The Pearson correlation between "Debt" and "Income" equals 0.713; the data are 2494.484 times more likely under the two-sided alternative hypothesis that the population correlation does
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 2494.484.

Additional Options
-------
### Plots:
- Prior and posterior: Displays the prior and posterior density of the correlation under the alternative hypothesis
  - Additional info: Adds the Bayes factor; adds a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior density of the correlation

Additional Output
-------
- Prior and posterior: Displays the prior (dashed line) and posterior (solid line) density of the correlation under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at 0. The horizontal solid line represents the width of the 95% credible interval of the posterior
 - Additional info: Displays the Bayes factor; displays a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density

References
-------
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.
