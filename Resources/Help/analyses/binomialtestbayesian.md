Bayesian Binomial Test
===
The Bayesian binomial test allows you to test whether a proportion of a dichotomous variable is equal to a test value (presumed population value).

### Assumptions
- The variable should be a dichotomous scale.
- Observations should be independent.

### Input
---------

- Test value: The proportion of the variable under the null hypothesis. By default, this is set to 0.5.

#### Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the proportion is not equal to test value.
- *&gt; Test value*: One-sided alternative hypothesis that the proportion is larger than the test value.
- *&lt; Test value*: One-sided alternative hypothesis that the proportion is smaller than the test value.

#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will show evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub> : By selecting this option, the Bayes factor will show evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.

#### Plots
- Prior and posterior: Displays the prior and posterior density of the population proportion under the alternative hypothesis.
    - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior density of the effect size.
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior.

#### Prior
**Beta prior** parameters *a* and *b* are set to '1' each. This corresponds to a uniform prior.

### Output
---

#### Bayesian Binomial Test
- Level: The two options of the dichotomous variable.
- Counts: the count of the instances at the certain level of the dichotomous variable.
- Total: the total number of observations.
- Proportion: calculated by counts/total.
- BF10 (or BF01): Bayes factor. If a one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis  that the population mean is larger than the test value, relative to the null hypothesis.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value, relative to the null hypothesis.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis, relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis, relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value.

#### Plots
- Prior and posterior: 
  - Displays the prior (dashed line) and posterior (solid line) density of the population proportion under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at the test value. The horizontal solid line represents the width of the 95% credible interval of the posterior.
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density.
- Sequential analysis: 
  - Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.


### References
---
- Jeffreys, H. (1961). *Theory of Probability*. Oxford, Oxford University Press.
- O’Hagan, A., & Forster, J. (2004). *Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.)*. London: Arnold.
- Haldane, J. B. S. (1932). A note on inverse probability. *Mathematical Proceedings of the Cambridge Philosophical Society, 28*, 55-61.

### R Packages
---
- ggplot2
- plotrix
- stats
