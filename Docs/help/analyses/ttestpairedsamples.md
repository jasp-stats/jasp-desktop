Paired Samples T-Test
==========================

The paired samples t-test allows you to test the null hypothesis that the population mean of the difference between paired observations equals 0

Assumptions
-----------
- Continuous difference score
- The difference scores are a random sample from the population
- The difference scores are normally distributed in the population

Default Options
-------
### Hypothesis:
- Measure 1 &ne; Measure 2: Two-sided alternative hypothesis that the population mean of the difference is not equal to 0
- Measure 1 &gt; Measure 2: One-sided alternative hypothesis that the population mean of the difference is larger than 0
- Measure 1 &lt; Measure 2: One sided alternative hypothesis that the population mean of the difference is smaller than 0

### Missing Values:
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the difference score for the
  particular t-test. Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all difference scores. 
 Sample size is therefore constant across the tests. 
 
Default Output
-------

### Paired Samples T-Test:
- t: t-value
- df: Degrees of freedom
- p: p-value

Additional Options
-------
### Additional Statistics:
- Mean difference: Mean of the differences scores
- Effect size: Cohen's d measure of effect size
- Confidence interval: Confidence interval for the population mean of the difference score
  - Interval: Coverage of the confidence interval in percentages
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each measure
- Normality tests: Shapiro-Wilk test of normality
- Descriptive plots: Displays the sample means and the confidence intervals for each measure (see Morey [2008] for the computation of the standard error of the mean in paired designs)
  - Confidence interval: Coverage of the confidence intervals in percentages
  
Additional Output
-------

### Paired Samples T-test:
- Mean Difference: Mean of the differences scores
- SE Difference: Standard error of the mean of the difference scores
- Cohen's d: Cohen's d measure of effect size
- x% Confidence Interval: Lower and Upper bound of the x% confidence interval for the population mean of the difference score

### Descriptives:
- N: Sample size
- Mean: Sample mean
- SD: Sample standard deviation
- SE: Standard error of the mean

### Test of Normality (Shapiro-Wilk)
- W: W test statistic
- p: p-value

### Descriptive Plots: 
- Displays the sample means (black bullet), the x% confidence intervals (whiskers) for each measure


References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Morey, R. D. (2008). Confidence intervals from normalized data: A correction to Cousineau (2005). *Tutorials in Quantitative Methods for Psychology, 4*, 61-64.