Paired Samples T-Test
==========================

The paired samples t-test allows you to test the null hypothesis that the population mean of the difference between paired observations equals 0.

Assumptions
-----------
- Numeric difference score.
- The difference scores are a random sample from the population.
- The difference scores are normally distributed in the population.

Options
-------
### Hypothesis:
- Group 1 &ne; Group 2: Two-sided alternative hypothesis that the population mean of the difference does not equal 0.
- Group 1 &gt Group 2: One-sided alternative hypothesis that the population mean of the difference is larger than 0.
- Group 1 &lt; Group 2: One sided alternative hypothesis that the population mean of the difference is smaller than 0.

### Missing Values
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the difference score for the
  particular t-test. Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all difference scores. 
 Sample size is therefore constant across the tests. 
 
Output
-------

### Paired Samples T-Test:
- t: t-value.
- df: Degrees of freedom.
- p: p-value.

Example
-------

### Data set: 
- Moon and Aggression

### Input: 
- The null hypothesis that the average difference between the number of disruptive behaviors on full moon days ("Moon") and not full 
moon days ("Other") equals 0 is tested against the two sided alternative hypothesis that the average difference in disruptive behaviors differs from 0.

### Output: 
- The null hypothesis that the average difference between the number of disruptive behaviors on full moon days ("Moon") and not full moon days ("Other") equals 0 is rejected in favor of 
the two sided alternative hypothesis that the average difference in disruptive behaviors differs from 0, t(14)=6.452, p < .001.

Additional Options
-------
### Additional Statistics:
- Mean difference: Mean of the differences scores.
- Effect Size: Cohen's d measure of effect size.
- Confidence interval: Confidence interval for the population mean of the difference score.
  - Interval: Coverage of the confidence interval in percentages.
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean.

Additional Output
-------

### Paired Samples T-test:
- Mean Difference: Mean of the differences scores.
- SE Difference: Standard error of the difference scores.
- Cohen's d: Cohen's d measure of effect size.
- x% Confidence Interval: Lower and upper bound of the x% confidence interval for the population mean of the difference score.

### Descriptives:
- N: Sample size.
- Mean: Sample mean.
- SD: Sample standard deviation.
- SE: Standard error of the mean.

References
-------
 - Moore, D.S., McCabe, G.P., Craig, B.A. (2012). Introduction to the Practice of Statistics (7th ed.). New York, NY: W.H. Freeman and Company.
 - Whitlock, M.C. & Schluter, D. (2015). The Analysis of Biological Data (2nd ed.). Greenwood Village, Colorado: Roberts and Company Publishers.
