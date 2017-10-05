Bayesian Correlation Matrix
==========================

The Bayesian Correlation Matrix allows you to test the null hypothesis that the population (Pearson product-moment correlation) equals 0.

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

### Report Bayes factors: Report Bayes factor for each test

### Beta* prior width: Width of the scaled beta density on the correlation under the alterative hypothesis; default is 1

Default Output
-------
### Bayesian Pearson Correlations:
- Pearson's r: Pearson product-moment correlation coefficient
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0

Additional Options
-------
- Flag supported correlations: Mark correlations in the "Bayesian Pearson Correlations" table that are supported by the Bayes factor

### Plots:
  - Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables
    - Densities for variables: Add a histogram and the corresponding density plot for each variable to the Correlation Matrix plot
    - Posteriors under H1: Adds the density plot of the posterior distribution of the correlation under the alternative hypothesis to the Correlation Matrix plot

Additional Output
-------
### Bayesian Pearson Correlations:
- Flag supported correlations: Correlations that are supported to the Bayes factor are marked with (see Jeffreys [1961] for evidence categories)
  - *BF > 10 if the data are at least 10 times more likely under the chosen hypothesis (see Hypothesis above)
  - **BF > 30 if the data are at least 30 times more likely under the chosen hypothesis (see Hypothesis above)
  - ***BF > 100 if the data are at least 100 times more likely under the chosen hypothesis (see Hypothesis above)

### Plots:
- Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables (in the upper off-diagonal entries of the matrix). The black line represents the least-square regression line
    - Densities for variables: Displays a histogram and the corresponding density plot for each variable in the diagonal entries of the matrix
    - Posteriors under H1: Displays density plot(s) of the posterior of the correlation(s) [rho] under the alternative hypothesis in the lower off-diagonal entries of the
    matrix

References
-------
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.
