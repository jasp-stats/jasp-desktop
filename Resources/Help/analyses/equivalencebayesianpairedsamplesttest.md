Equivalence Bayesian Paired Samples T-Test
==========================

The equivalence paired samples t-test allows the user to test the null hypothesis that the effect size of the population mean of the difference between paired observations falls inside a by the user-defined interval, i.e., the equivalence region.

### Assumptions
- Continuous difference score.
- The difference scores are a random sample from the population.
- The difference scores are normally distributed in the population.

### Input
---

#### Assignment Box
- Variables: In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents other difference scores.

#### Equivalence region
- from ... to ... : Defines the equivalence region by specifying the lower bound and the upper bound.

#### Plots
- Prior and posterior: Displays the prior and posterior distribution of the effect size under the alternative hypothesis.
  - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior distribution of the effect size.
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior.
  - Robustness check: Adds the results of the sequential analysis using the wide (scale=1) and ultrawide prior (scale=sqrt(2)).

#### Additional Statistics
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each measure.
- Prior and posterior mass: Displays the prior and posterior mass in and outside the set equivalence region.

### Missing Values
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests. This options is selected by default.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.

### Prior
- Default:
  - Cauchy: Scale of the Cauchy prior distribution on the effect size under the alternative hypothesis; the default is 0.707.
- Informed:
  - Cauchy: Scale and location.
  - Normal: Mean and standard deviation.
  - Student's t: Scale, Location and degrees of freedom (df).

### Output
---

#### Equivalence Bayesian Paired Samples T-Test
- The first column contains the two variables for which the difference is calculated.
- Model Comparison:
  - &delta; &in; I vs. H<sub>1</sub>: Bayes factor to quantify evidence for the interval-null hypothesis to the unconstrained alternative hypothesis.
  - &delta; &notin; I vs. H<sub>1</sub>: Bayes factor to quantify evidence for the hypothesis that the effect size is outside the interval-null to the unconstrained alternative hypothesis.
  - &delta; &in; I vs. &delta; &notin; I: Bayes factor to quantify evidence for the interval-null hypothesis to the hypothesis that the effect size falls outside the interval-null.
  - &delta; &notin; I vs. &delta; &in; I: Bayes factor to quantify evidence for the hypothesis that the effect size falls outside the interval-null to the interval-null hypothesis.
- BF: The Bayes factor.
- error %: The error of the numerical integration used for the computation of the Bayes factor.

#### Descriptives
- The first column contains the dependent variable.
- N: The sample size.
- Mean: The mean of the dependent variable.
- SD: Standard deviation of the mean.
- SE: Standard error of the mean.
- Credible interval: Central credible interval. Default is 95%.

#### Prior and Posterior Mass Table
- The first column contains the dependent variable.
- Section: The section under which the mass is calculated.
  - p(&delta; &in; I | H<sub>1</sub>): Section inside the equivalence region of the prior distribution.
  - p(&delta; &in; I | H<sub>1</sub>, data): Section inside the equivalence region of the posterior distribution.
  - p(&delta; &notin; I | H<sub>1</sub>): Section outside the equivalence region of the prior distribution.
  - p(&delta; &notin; I | H<sub>1</sub>, data): Section outside the equivalence region of the posterior distribution.
- Mass: Displays the mass under the specified section.

#### Plots
- Prior and posterior: Displays the prior (dashed line) and posterior (solid line) distribution of the effect size under the alternative hypothesis. The grey areas represents the equivalence region of the prior and the posterior density. The horizontal solid line represents the width of the 95% credible interval of the posterior distribution.
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the equivalence vs. non-equivalence hypothesis; displays the median and 95% credible interval of the posterior distribution.
- Sequential analysis: Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the equivalence vs. non-equivalence hypothesis; displays the median and 95% credible interval of the posterior distribution; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.

### References
---
- Morey, R. D., & Rouder, J. N. (2011). Bayes factor approaches for testing interval null hypotheses. *Psychological methods*, 16(4), 406. <a href="https://psycnet.apa.org/buy/2011-15467-001">https://psycnet.apa.org/buy/2011-15467-001</a>
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (in press). Informed Bayesian t-tests. *The American Statistician*. <a href="https://arxiv.org/abs/1704.02479">https://arxiv.org/abs/1704.02479</a>
- Jeffreys, H. (1961).  *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.

### R-packages
---
- stats
- metaBMA
