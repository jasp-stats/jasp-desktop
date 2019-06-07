Bayesian Multinomial Test
===
  
The Bayesian multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.

### Assumptions
- The variable of interest should be categorical.

### Input
---

#### Assignment Box
- Factor: The categorical variable we are interested in.
- Counts (optional): The variable that contains the count data.
- Expected Counts (optional): When the data set contains a variable which reflects the expectations of cell counts, that column can be entered here. The values in this variable will be interpreted as the null hypothesis.


#### Test Values
- Equal proportions: In the Bayesian multinomial test we test the null hypothesis that the cell probabilities are uniformly distributed. The null hypothesis is tested against the alternative hypothesis which states that all category proportions are free to vary.
- Expected proportions: Here, we specify the null hypothesis that the cell probabilites are equal to a particular expected distribution. By default the first hypothesis constitutes the null hypothesis from the Bayesian multinomial test, but the expected counts can be changed manually. The null hypothesis is tested against the alternative hypothesis which states that all category proportions are free to vary. The specification of multiple hypotheses is possible.

#### Bayes Factor
- *BF10*: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- *BF01*: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- *Log(BF10)*: Natural logarithm of *BF10*.

#### Additional statistics
- Descriptives: Option to display the descriptives of the data; the observed and expected counts as well as the credible intervals of the observed values.
- Credible interval: Default is 95%. Note that the credible intervals are based on independent binomial distributions with flat priors. The computation of the credible intervals are based on a procedure first given by Clopper and Pearson (1934). This procedure assumes a Beta(0,1) prior distribution when computing the lower bound of the credible interval, and a Beta(1, 0) distribution when computing the upper bound for the credible interval.

#### Display
- Counts: With this option the descriptives are displayed as absolute counts.
- Proportions: With this option the descriptives are displayed as a proportion of the total number of counts.
- Plots:
  - Descriptive plot: Plots the frequencies and the credible intervals of the observed counts.

### Prior
Option to adjust the prior distribution for the vector of cell probabilities. 
- *Dirichlet*: For *K* categories, the default prior is Dirichlet(alpha_*1*, alpha_*2*, ..., alpha_*K*) with all alpha parameters set to 1. Note that the parameters of the *Dirichlet distribution* reflect prior counts, which implies that all values must be non-negative.

### Output
---

#### Bayesian Multinomial Test
- Hypothesis: Displays all specified hypotheses.
- Levels: Displays the number of categories of the variable of interest.
- *BF10*, *BF01*, or *Log(BF10)*: Displays the Bayes factor computed with the user-defined prior.

#### Descriptives
The descriptives table includes the categories of interest, the observed values, the expected values under the specified hypotheses. The descriptives are displayed either in counts or in proportions, dependent on what was selected under the option 'Display'. When specified, that table also displays the user-defined credible interval.

#### Descriptives plot
The descriptive plot displays the frequency of the reported counts and the corresponding credible intervals for every level of the variable of interest.

### References
---
- Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.
- Good, I. J. (1967). A Bayesian significance test for multinomial distributions. *Journal of the Royal Statistical Society: Series B (Methodological), 29*, 399-418.

### R Packages
---
- ggplot2
- stats
