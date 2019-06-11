Multinomial Test
===

Multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.

### Assumptions
- The variable of interest should be categorical.

### Input
---

#### Assignment Box
- Factor: The categorical variable we are interested in.
- Counts (optional): The variable that contains the count data.
- Expected Counts (optional): When the data set contains a variable which reflects the expectations of cell counts, that column can be entered here. The values in this variable will be interpreted as the null hypothesis.

#### Alt. Hypothesis
- Multinomial test: The multinomial test checks whether the observed cell counts are uniformly distributed.The null hypothesis is tested by means of the Pearson's chi-squared test statistics, which measures the deviation of the observed from the expected cell counts under the null hypothesis. The null hypothesis is tested against the alternative hypothesis that states that the category proportions differ from the uniform distribution.
    - $$\chi^2$$ test: The chi-squared goodness-of-fit test determines whether the observed cell counts deviate from a particular expected distribution. In the chi-square goodness-of-fit test, the expected distribution is not restricted to a uniform distribution. By default the first hypothesis postulates that the cell counts are uniformly distributed, but the expected counts can be changed manually and the specification of multiple hypotheses is possible. The specified null hypotheses are tested against the alternative hypothesis which states that the category proportions differ from the uniform distribution.

#### Additional statistics
- Descriptives: Option to display the descriptives of the data; the observed and e\chipected counts as well as the confidence intervals of the observed values.
  - Confidence interval: Coverage of the confidence intervals in percentages. By default the confidence level is set to 95%. The confidence intervals are based on a procedure first given by Clopper and Pearson (1934) and assumes independent binomial distributions for each factor level.
  - Vovk-Dellke maximum *p*-ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

#### Display
  - Counts: With this option the descriptives are displayed as absolute counts.
  - Proportions: With this option the descriptives are displayed as a proportion of the total number of counts.

#### Plots
  - Descriptive plot: Plots the frequencies and the confidence intervals of the observed counts.
      - Confidence interval

### Output
---
#### Multinomial Test
- Multinomial Test:
  - Hypothesis: When the $$\chi^2$$ test is selected and there are multiple hypotheses entered these will all be displayed here.
  - $$\chi^2$$: The chi-square goodness-of-fit value.
  - p: the p-value of the multinomial test, or the $$\chi^2$$ goodness-of-fit test.
  - VS-MPR: Vovk-Sellke maximum p-ratio.
- Descriptives:
  - The descriptives table includes the categories of interest, the observed values, the expected values under the specified hypotheses, and confidence intervals based on independent binomial distributions. The descriptives are displayed either in counts or in proportions.

#### Descriptives plot
The descriptive plot displays the frequency of the reported counts and the corresponding confidence intervals for every level of the variable of interest.

### References
---
- Haberman, S. J. (1978). *Analysis of qualitative data: Introductory topics (Vol 1)*. Academic Press.
-  Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.

### R Packages
---
- ggplot2
- stats
