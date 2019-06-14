Summary Statistics Bayesian Binomial Test
===

This function computes the Bayes factor for a binomially distributed observation. The Bayesian binomial test is described in Jeffreys (1961, p. 256). This test informs us whether the data support or contradict a value suggested for the parameter (chance) in question.

### Input 
---

#### Assignment box
- Null model: *p = p0*
- Alt  model: *p ~ Beta(a,b)*
*p0* is the suggested value of the rate parameter of Binomial under null hypothesis. Data observed: *s* successes and *f* failures, total number of trials, *n = s + f*. In Theory of Probability, Jeffreys assumes a uniform prior on the rate parameter under alternative hypothesis. The Bayes factor used here is a more general case, assuming a beta prior on the rate parameter. *Note*: beta(1,1) corresponds to a uniform prior.

#### Alt. Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the population mean is not equal to the test value.
- *&gt; Test value*: One-sided alternative hypothesis that the population mean is larger than the test value.
- *&lt; Test value*: One sided alternative hypothesis that the population mean is smaller than the test value.

#### Bayes Factor
- *BF10*: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- *BF01*: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- *Log(BF10)*: Natural logarithm of BF10.

#### Prior
**Beta prior** parameters *a* and *b* are set to '1' each. This corresponds to a uniform prior.

#### Plots
- *Prior and posterior*: Displays the prior (dashed line) and posterior (solid line) density of the effect size under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at effect size delta = 0. The horizontal solid line represents the width of the 95% credible interval of the posterior
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density

### Output
---
#### Binomial Bayesian Test
- *Bayes factor*: If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is larger than the test value
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value
- **p**: p-value corresponding to t-statistic.

### References
---
- Jeffreys, H. (1961). *Theory of Probability*. Oxford, Oxford University Press.
- O’Hagan, A., & Forster, J. (2004). *Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.)*. London: Arnold.
- Haldane, J. B. S. (1932). A note on inverse probability. *Mathematical Proceedings of the Cambridge Philosophical Society, 28*, 55-61.


### R Packages
---
- stats
