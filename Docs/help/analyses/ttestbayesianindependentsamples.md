
Bayesian Independent Samples T-Test
==========================

The independent samples t-test allows you to test the null hypothesis that the population means of two independent groups are equal.

Assumptions
-----------
- Numeric dependent variable.
- The observations from both groups are a random sample from the population.
- The dependent variable is normally distributed in both populations.
- The population variances in the two groups are homogeneous.

Options
-------
### Hypothesis:
- Group one &ne; Group two: Two-sided alternative hypothesis that the population means are equal.
- Group one &gt Group two: One-sided alternative hypothesis that the population mean of Group one is larger than the population mean of Group two.
- Group one &lt; Group two: One-sided alternative hypothesis that the population mean of Group one is smaller than the population mean of Group two. 

### Bayes Factor
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF10): Natural logarithm of BF10.

### Prior
- Cauchy prior width: Scale of the Cauchy prior density on effect size under the alternative hypothesis; the default is 0.707.

### Missing Values
 - Exclude cases analysis by analysis: TBA
 - Exclude cases listwise: TBA

Output
-------

### Bayesian Independent Samples T-Test
- BF10 (or BF01): Bayes factor. If one-sided test is requested: 
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that Group one > Group two.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that Group one < Group two.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that Group one > Group two.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that Group one < Group two.
- error %: The error of a Gaussian quadrature integration routine used for the computation of the Bayes factor.

Example
-------

### Data set: 
- Gender Differences in Talkativeness

### Input: 
- The null hypothesis that the average number of words spoken per day is the same for women and men is tested against the one-sided alternative hypothesis that the average 
number of words spoken per day is higher for women than for men.

### Output: 
- The data are 3.855 times more likely under the null hypothesis that the average number of words spoken per day is the same for women and men than
under the one-sided alternative hypothesis that the average number of words spoken per day is higher for women than for men, BF0- =3.857, with error %  ~ 1.979e-4.

Additional Options
-------
### Additional Statistics:
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean

### Plots
- Prior and posterior: Displays the prior and posterior density of the effect size under the alternative hypothesis.
  - Additional info: Adds Bayes factor computed with the user-defined prior; adds pizza plot depicting the odds of the data under the null vs. alternative hypothesis; adds the median and 95% credible interval of the posterior density.
- Bayes factor robustness check: Displays Bayes factor as a function of the width of the Cauchy prior on effect size. The scale of the Cauchy prior is varied between 0 and 1.5, creating progressively more uninformative priors.
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior. 
 - Robustness check: Additional to the analysis with the user-defined prior, displays the results of the sequential analysis using the wide (TBA) and ultrawide prior (TBA).
 
Additional Output
-------

### Descriptives:
- N: Sample size.
- Mean: Sample mean. 
- SD: Sample standard deviation.
- SE: Standard error of the mean.

### Plots:
- Prior and posterior: Displays the prior (dashed line) and posterior (solid line) density of the effect size under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at effect size delta = 0. The horizontal solid line represents the width of the 95% credible interval of the posterior.
 - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density.
- Bayes factor robustness check: Displays Bayes factor as a function of the width of the Cauchy prior on effect size. The black circle represents the Bayes factor computed with a wide prior; the white circle represents the Bayes factor computed with an ultrawide prior; the gray circle represents the Bayes factor computed with the user-defined prior.
- Sequential analysis: Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior.
 - Robustness check: Displays the development of the Bayes factor as a function of the number of observations (n) using the wide and ultrawide prior. The black circle represents the Bayes factor computed with a wide prior; the white circle represents the Bayes factor computed with an ultrawide prior; the gray circle represents the Bayes factor computed with the user-defined prior.
 
References
-------
- Morey, R.D., Rouder, J.N., Pratte, M.S., & Speckman, P.L. (2011). Using MCMC chain outputs to efficiently estimate Bayes factors. Journal of Mathematical Psychology, 55, 368-378.
- Rouder, J.N., Speckman, P.L., Sun, D., Morey, R.D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225-237.
- Perception and Cognition Lab (University of Missouri): Bayes factor calculators. http://pcl.missouri.edu/bayesfactor 