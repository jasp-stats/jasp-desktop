Binomial Test
====
The binomial test allows the user to test whether a proportion of a dichotomous variable is equal to a test value (=presumed population value).

### Assumptions
- The variable should be a dichotomous scale.
- Observations should be independent.

### Input 
----
- Test value: The proportion of the variable under the null hypothesis.

#### Alt. Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the proportion is not equal to test value.
- *&gt; Test value*: One-sided alternative hypothesis that the proportion is larger than the test value.
- *&lt; Test value*: One-sided alternative hypothesis that the proportion is smaller than the test value.

#### Additional Statistics
- Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

#### Plots
- Descriptive plots: The proportion and confidence interval of the two different values of your dichotomous variable
- Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.

### Output
-----------
#### Binomial Test
- Level: The two options of the dichotomous variable
- Counts: the count of the instances at the certain level of the dichotomous variable. The counts of all possible levels will result in the total.
- Total: the total number of observations.
- Proportion: calculated by counts/total.
- p: p-value.
- VS-MPR: Vovk-Sellke maximum p-ratio.

#### Descriptive Plots
- Displays the proportion of the level of interest (black dot) and the width of the 95% credible interval. The horizontal dotted line represents the when the proportion of both levels would be equal (= 0.5).

### References
---
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.

### R Packages
---
- ggplot2
- stats
