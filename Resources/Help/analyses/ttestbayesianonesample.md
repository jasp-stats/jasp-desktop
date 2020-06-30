Bayesian One Sample T-Test
===
The one sample t-test allows the user to estimate the effect size and test the null hypothesis that the population mean equals a specific constant, i.e., the test value.

### Assumptions
- Continuous dependent variable.
- The data are a random sample from the population.
- The dependent variable is normally distributed in the population.

### Input
---

#### Assignment Box 
- Variables: In this box the dependent variable is selected.  

#### Test value
Test value specified in the null hypothesis.

#### Hypothesis
- &ne; Test value: Two-sided alternative hypothesis that the population mean is not equal to the test value.
- &gt; Test value: One-sided alternative hypothesis that the population mean is larger than the test value.
- &lt; Test value: One sided alternative hypothesis that the population mean is smaller than the test value.

#### Bayes Factor
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF10): Natural logarithm of BF10.

#### Tests 
- Student: Student's t-test. This option is selected by default. 
- Wilcoxon signed-rank: Wilcoxon signed-rank test. 
  - No. samples: The number of MCMC samples to use.

#### Prior
- Standardized effect size
  - Default
    - Cauchy: Scale of the Cauchy prior distribution on effect size under the alternative hypothesis; the default is 0.707.
  - Informed
    - Cauchy: Scale and location.
    - Normal: Mean and standard deviation.
    - Student's t: Scale, location and degrees of freedom (df).

[comment]: # (- Raw effect size (Dienes))
[comment]: # (  - Half-Normal: Standard deviation.)
[comment]: # (  - Normal: Mean and standard deviation.)
[comment]: # (  - Uniform: Lower and upper bounds.)

#### Additional Statistics
- Descriptives: Sample size, sample mean, sample standard deviation, and standard error of the mean.

#### Repeatability
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

#### Plots
- Prior and posterior: Displays the prior and posterior distribution of the effect size under the alternative hypothesis.
  - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size
- Bayes factor robustness check: Displays the Bayes factor as a function of the width of the Cauchy prior on effect size. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior.
  - Robustness check: Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2)).
- Descriptives plots
  - Credible interval: Default is 95%.

#### Missing Values
 - Exclude cases per dependent variable: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test.
 Sample sizes may therefore vary across the multiple t-tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the multiple t-tests.

### Output
---

#### Bayesian One Sample T-Test
- Bayes factor: If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is larger than the test value.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value.
- error %: The error of the Gaussian quadrature integration routine used for the computation of the Bayes factor.
- W: The test statistic of the signed rank test.
- Rhat: A measure of MCMC convergence for the signed rank test. The ratio of within chain variance and total variance of the MCMC chains for the delta parameter. Values equal to 1 indicate convergence.

#### Descriptives
- N: Sample size
- Mean: Sample mean
- SD: Sample standard deviation
- SE: Standard error of the mean

#### Plots
- Prior and posterior: Displays the prior (dashed line) and posterior (solid line) distribution of the effect size under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at effect size delta = 0. The horizontal solid line represents the width of the 95% credible interval of the posterior distribution.
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior distribution.
- Bayes factor robustness check: Displays the Bayes factor as a function of the width of the Cauchy prior on effect size. The black circle represents the Bayes factor computed with a wide prior; the white circle represents the Bayes factor computed with an ultrawide prior; the gray circle represents the Bayes factor computed with the user-defined prior distribution.
- Sequential analysis: Displays the development of the Bayes factor as a function of the number of data points (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior distribution; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.
  - Robustness check: Displays the development of the Bayes factor as a function of the number of data points (n) using the wide and ultrawide prior distribution. The black circle represents the Bayes factor computed with a wide prior distribution; the white circle represents the Bayes factor computed with an ultrawide prior distribution; the gray circle represents the Bayes factor computed with the user-defined prior distribution.
- Descriptives plots
  - Credible interval: Default is 95%.

### References
---
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961). *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Morey, R. D., Rouder, J. N., Pratte, M. S., & Speckman, P. L. (2011). Using MCMC chain outputs to efficiently estimate Bayes factors. *Journal of Mathematical Psychology, 55*, 368-378.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. *Psychonomic Bulletin & Review, 16*, 225-237.
- van Doorn, J, Ly, A, Marsman, M, & Wagenmakers, E.-J. (2020). Bayesian rank-based hypothesis testing for the rank sum test, the signed rank test, and Spearman's rho. *Journal of Applied Statistics*.

### R-packages 
---
- BayesFactor
- ggplot2
- logspline
- stats

