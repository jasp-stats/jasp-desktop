Bayesian Correlation
===

The Bayesian Correlation analysis allows for the estimation of the population correlation, as well as the comparison of hypotheses. The three comparisons are (i) between the null hypothesis (H0) that the correlation between pairs of variables equal 0, and the alternative hypothesis (H1) that the population correlation takes its value between -1 and 1; (ii) between H0 and the alternative hypothesis (H+) that the population correlation is positive; and (iii) between H0 and the alternative hypothesis (H-) that the population correlation is negative. All possible pairs of the specified variables are analyzed.

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

#### Population Correlation Coefficient
- Pearson's rho: Pearson's product moment correlation coefficient.
- Kendall's tau-b: Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables.

#### Alt. Hypothesis
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0.
- Correlated positively: One-sided alternative hypothesis that the population correlation is greater than 0.
- Correlated negatively: One-sided alternative hypothesis that the population correlation is lower than 0.

#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will show evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub> : By selecting this option, the Bayes factor will show evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.

#### Additional Options
- Display pairwise table: Display a table where one row corresponds to one pair of the specified variables. If unticked, the results are presented in matrix format, with variable names in the columns and rows.
- Report Bayes factors: Report Bayes factor for each test.
- Flag supported correlations: Indicate which correlation coefficients yield Bayes factors greater than 10, 30, and 100.
- Sample size: Report the sample size for each test.
- Credible intervals: Display central 95% credible intervals for the correlation coefficient.

#### Plots
- Correlation matrix: Display a grid of scatterplots for each possible combination of the selected variables. These are placed above the diagonal.
  - Densities for variables: Display histogram and the corresponding density plot for each variable. These are placed on the diagonal.
  - Posteriors under H<sub>1</sub>: Display posterior distribution of the correlation coefficient for each possible combination of the selected variables. These are placed below the diagonal.

#### Prior
- Stretched beta prior width: Width of the scaled beta distribution on the correlation under the alterative hypothesis; default is 1. The lower this value, the more concentrated the prior density is around 0. Value must be between 0 and 2.

### Plot Individual Pairs
- Correlation coefficient to plot:
  - Pearson's rho: Pearson's product moment correlation coefficient.
  - Kendall's tau-b: Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables.

- Scatterplot: Displays scatterplots for each specified pair of variables.

- Prior and posterior: Displays the prior and posterior distribution of the correlation under the alternative hypothesis for each specified pair of variables.
  - Estimation info: Adds the median and the 95% credible interval of the posterior distribution of the effect size.
  - Testing info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis (under the assumption that the null and alternative hypotheses were equal probable a priori, i.e., P(H0)=P(H1)=1/2); adds indicators representing the prior and posterior at the test point (e.g., rho=0), the ratio between these two values equals the Bayes factor.

- Bayes factor robustness check: Displays the Bayes factor as a function of the width of the stretched beta prior on the correlation for each specified pair of variables. The width of the kappa prior is varied between 0 and 2.
  - Additional info: Adds the Bayes factor computed with the user-defined prior and the maximum obtainable Bayes factor.

- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior for each specified pair of variables.
  - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.

### Options
- Missing values:
  - Exclude cases pairwise
  - Exclude cases listwise
- Repeatability:
  - Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.


### Output
---

#### Bayesian Correlation Table
- n: sample size.
- Pearson's r: Pearson product-moment correlation coefficient.
- Kendall's tau: Kendall's rank correlation coefficient.
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is higher than 0.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is lower than 0.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is higher than 0.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is lower than 0.
- Credible interval: Central credible intervals for the correlation coefficient.

- Flag supported correlations: Correlations that are supported to the Bayes factor are marked with (see Jeffreys (1961) for evidence categories).
  - *BF > 10 if the data are at least 10 times more likely under the chosen hypothesis (see Hypothesis above).
  - **BF > 30 if the data are at least 30 times more likely under the chosen hypothesis (see Hypothesis above).
  - ***BF > 100 if the data are at least 100 times more likely under the chosen hypothesis (see Hypothesis above).

#### Bayesian Correlation Matrix Plot
- Displays a (matrix of) scatterplot(s) between the variables (in the upper off-diagonal entries of the matrix). The black line represents the least-square regression line.
    - Densities for variables: Displays a histogram and the corresponding density plot for each variable in the diagonal entries of the matrix.
    - Posteriors under H1: Displays density plot(s) of the posterior of the correlation(s) under the alternative hypothesis in the lower off-diagonal entries of the matrix.

#### Bayesian Correlation Pairwise Plots
- Scatterplot:
  - Displays the scatterplot of the correlation pairs.

- Prior and Posterior:
  - Displays the prior (dashed line) and posterior (solid line) distribution of the correlation under the alternative hypothesis; The horizontal solid line represents the width of the 95% credible interval of the posterior distribution.
    - Estimation info: Displays the median and the 95% credible interval of the posterior distribution of the effect size.
    - Testing info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis (under the assumption that the null and alternative hypotheses were equal probable a priori, i.e., P(H0)=P(H1)=1/2); displays indicators (gray circles) representing the prior and posterior at the test point (e.g., rho=0), the ratio between these two values equals the Bayes factor.

- Bayes factor robustness check:
  - Displays the Bayes factor as a function of the width of the beta prior on the correlation.
    - Additional info: The red circle represents the maximum obtainable Bayes factor; the gray circle represents the Bayes factor computed with the user-defined prior.

- Sequential analysis:
  - Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior.
    - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.

### References
---
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Ly, A., Verhagen, A. J., & Wagenmakers, E.-J. (2016). Harold Jeffreys's default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. *Journal of Mathematical Psychology, 72*, 19-31.
- Ly, A., Marsman, M., & Wagenmakers, E.-J. (2018).  Analytic Posteriors for Pearson’s Correlation Coefficient. *Statistica Neerlandica, 72*(1), 4-13
- Rouder, J. N., & Morey R. D. (2012). Default Bayes factors for model selection in regression. *Multivariate Behavioral Research, 47*, 877-903.
- van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (2018). Bayesian Inference for Kendall’s Rank Correlation Coefficient. *The American Statistician*,  72, 303-308.
- Wagenmakers, E.-J., Lodewyckx, T., Kuriyal, H., and Grasman, R. (2010). Bayesian hypothesis testing for psychologists: A tutorial on the Savage-Dickey method. *Cognitive Psychology, 60*, 158-189.
- Wetzels, R., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. *Psychonomic Bulletin & Review, 19*, 1057-1064.

### R Packages
---
- hypergeo
- ggplot2
- stats
