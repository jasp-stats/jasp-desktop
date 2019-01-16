Bayesian Binomial Test
==========================
The Bayesian binomial test allows you to test whether a proportion of a dichotomous variable is equal to a test value (presumed population value).

Assumptions
----------
- The test variable should be a dichotomous scale.
- Observations should be independent

Options
---------
Test value: The proportion of the test variable under the null hypothesis.

### Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the proportion is not equal to test value
- *&gt; Test value*: One-sided alternative hypothesis that the proportion is larger than the test value
- *&lt; Test value*: One-sided alternative hypothesis that the proportion is smaller than the test value

### Bayes Factor
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis
- Log(BF10): Natural logarithm of BF10

### Plots
- Prior and posterior: Displays the prior and posterior density of the population proportion under the alternative hypothesis
    - Additional info: Adds the Bayes factor computed with the user-defined prior; adds a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; adds the median and the 95% credible interval of the posterior density of the effect size
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior

### Prior
Relevant background knowledge can be incorporated in the prior. The prior is set by the parameter a and b. These parameters can be seen as sort of pseudo-observations: parameter a would be the number of counts for one level and parameter b would be the number of counts for the other level. When the prior is uniform (a = b), the posterior has the same shape as the likelihood. Since a uniform prior gives no preferences to certain values of population value, the posterior is determined by the data. If we have a strong prior belief in a model, we set high values to the a and b parameters. Then it takes more evidence for the model to overcome our prior beliefs. So data overwhelm the prior more easily for lower values of a and b.

Output
-----------
### Bayesian Binomial Test
- Level: The two options of the dichotomous variable
- Counts: the count of the instances at the certain level of the dichotomous variable.
- Total: the total amount of observations
- Proportion: calculated by counts/total
- BF10 (or BF01): Bayes factor. If one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is larger than the test value
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value

### Plots
Prior and posterior: Displays the prior (dashed line) and posterior (solid line) density of the population proportion under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at the test value. The horizontal solid line represents the width of the 95% credible interval of the posterior
Additional info: Displays the Bayes factor computed with the user-defined prior; displays a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density

### Sequential Analysis
Sequential analysis: Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a pizza plot depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories

References
-------
- Jeffreys, H. (1961). Theory of Probability. Oxford, Oxford University Press.
