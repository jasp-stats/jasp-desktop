Equivalence Paired Samples T-Test
==========================

The equivalence paired samples t-test allows the user to test the null hypothesis that the effect size of the population mean of the difference between paired observations falls inside an interval (i.e., equivalence region). The procedure follows the two-one-sided test (TOST).

### Assumptions
- The difference score is continuous.
- The difference scores are a random sample from the population.
- The difference scores are normally distributed in the population.

### Input
-------
#### Assignment Box
- Variables: In this box the variables are selected for which the difference is computed. Multiple differences can be analysed at the same time by specifying different rows with two variables for which the difference is computed. In other words, each row represents other difference scores.

#### Equivalence region
- Upper bound: The upper bound of the equivalence region.
- Lower bound: The lower bound of the equivalence region.
- Bound specification:  The equivalence bounds can be set in effect size units or in raw units.
  - Raw: Raw differences.
  - Cohen's d: For the Student's t-test, uses the pooled standard deviation to standardize the mean difference. For the Welch's t-test, uses the square-root of the average variance to standardize the mean difference. difference. For the Welch's t-test, uses the square-root of the average variance to standardize the mean difference.
- Alpha level: The default value is 0.05.

#### Additional Statistics
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each group.
- Equivalence bounds plot: Displays if the 100-(2*alpha)% confidence interval falls within the set equivalence bounds.

### Missing Values
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests. This options is selected by default.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.

### Output
-------

#### Equivalence Independent Samples T-Test
- The first column contains the two variables for which the difference is calculated.
- Statistic: Only when both the upper bound and the lower bound statistic are rejected, we reject our initial non-equivalence hypothesis.
  - T-Test: The standard t-test statistic. Two-sided alternative hypothesis that the population mean of the difference is not equal to 0.
  - Upper bound: Test the null-hypothesis whether the effect is bigger than or equal to the upper bound compared to the alternative hypothesis that the effect is smaller than the upper bound.
  - Lower bound: Test the null-hypothesis whether the effect is smaller than or equal to the lower bound compared to the alternative hypothesis that the effect is larger than the lower bound.
- t: The value of the t-value.
- df: Degrees of freedom.
- p: The p-value.

#### Equivalence Bounds
- The first column contains the two variables for which the difference is calculated.
- The second column contains the information if the equivalence bounds are expressed in standardized mean differences (Cohen's d) or in raw units.
- Low: low equivalence bound expressed in standardized mean difference for Cohen's d and in raw units for Raw
- High: high equivalence bound equivalence bound expressed in standardized mean difference for Cohen's d and in raw units for Raw
- 90% Confidence Interval for Mean difference/location parameter: The confidence interval for the mean difference/location parameter of the difference scores.
  - Lower: The lower bound of the confidence interval.
  - Upper: The upper bound of the confidence interval.

#### Descriptives
- The first column contains the dependent variable.
- Group: The levels of the grouping variable.
- N: The sample size per group.
- Mean: The mean of the dependent variable per group.
- SD: Standard deviation of the mean.
- SE: Standard error of the mean.

#### Equivalence Bounds Plots
Displays the sample mean difference and the 90% confidence interval. The grey area is the set equivalence region.

### References
-------
- Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: A tutorial. *Advances in Methods and Practices in Psychological Science*, 1(2), 259-269. <a href="https://journals.sagepub.com/doi/abs/10.1177/2515245918770963">https://journals.sagepub.com/doi/abs/10.1177/2515245918770963</a>

### R-packages
---
- TOSTER
