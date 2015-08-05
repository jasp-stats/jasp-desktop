Independent Samples T-Test
==========================

The independent samples t-test allows you to test the null hypothesis that the population means of two independent groups are equal

Assumptions
-----------
- Continuous dependent variable
- The observations in both groups are a random sample from the population
- The dependent variable is normally distributed in both populations
- The population variances in the two groups are homogeneous

Default Options
-------
### Hypothesis:
- Group 1 &ne; Group 2: Two-sided alternative hypothesis that the population means are equal
- Group 1 &gt; Group 2: One-sided alternative hypothesis that the population mean of Group 1 is larger than the population mean of Group 2
- Group 1 &lt; Group 2: One-sided alternative hypothesis that the population mean of Group 1 is smaller than the population mean of Group 2

### Equality of Variances:
- Assume equal: Assume that the variances of the two groups are equal. The pooled variance is used as an estimate of the population variance.
- No assumption: Do not assume that the variances of the two groups are equal. The degrees of freedom will be computed using the Welch approximation.
- Report both: Report results assuming and not assuming the equality of variances.

### Missing Values:
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. 
 Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests. 
 
Default Output
-------

### Independent Samples T-Test:
- t: t-value
- df: Degrees of freedom
- p: p-value

Additional Options
-------

### Test inequality of variances:
- Levene's test for homogeneity of variances

### Additional Statistics:
- Mean difference: Difference in sample means and the standard error of the difference in means
- Effect Size: Cohen's d measure of effect size
- Confidence interval: Confidence interval for the difference in population means
  - Interval: Coverage of the confidence interval in percentages
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each group
- Normality tests: Shapiro-Wilk test of normality
- Descriptive plots: Displays the sample means and the confidence intervals for each group
  - Confidence interval: Coverage of the confidence intervals in percentages

Additional Output
-------

### Independent Samples T-test:
- Mean difference: Difference in sample means
- SE Difference: Standard error of the difference in means
- Cohen's d: Cohen's d measure of effect size
- x% Confidence Interval: Lower and Upper bound of the x% confidence interval for the difference in population means

### Group Descriptives:
- N: Sample size
- Mean: Sample mean
- SD: Sample standard deviation
- SE: Standard error of the mean

### Test of Normality (Shapiro-Wilk)
- W: W test statistic
- p: p-value

### Descriptive Plots: 
- Displays the sample means (black bullet), the x% confidence intervals (whiskers) for each group

References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.