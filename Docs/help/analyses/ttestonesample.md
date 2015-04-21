One Sample T-Test
==========================

The one sample t-test allows you to test the null hypothesis that the population mean is equal to a specific constant, i.e., the test value.

Assumptions
-----------
- Numeric dependent variable.
- The data are a random sample from the population.
- The dependent variable is normally distributed in the population.

Options
-------
### Test value: 
-Test value specified in the null hypothesis.

### Hypothesis:
- &ne; Test value: Two-sided alternative hypothesis that the population mean is not equal to the test value. 
- &gt; Test value: One-sided alternative hypothesis that the population mean is larger than the test value.
- &lt; Test value: One sided alternative hypothesis that the population mean is smaller than the test value. 

### Missing Values:
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data on the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests. 
 
Output
-------
### One Sample T-Test:
- t: t-value.
- df: Degrees of freedom.
- p: p-value.

Example
-------

### Data set: 
- Weight Gain

### Input: 
- The null hypothesis that the mean "Difference" in weight between the two time points in the population equals 16 lb is tested against the two sided alternative hypothesis that the mean "Difference" does not equal 16 lb.

### Output: 
- The null hypothesis that the mean "Difference" in weight between the two time points in the population equals 16 lb is rejected in favor of the two sided alternative hypothesis that the mean "Difference" does not equal 16 lb, t(15)=-5.823, p < .001.

Additional Options
-------
### Additional Statistics:
- Mean difference: Mean of the differences between the data points and the test value.
- Effect size: Cohen's d measure of effect size.
- Confidence interval: Confidence interval for the population mean.
  - Interval: Coverage of the confidence interval in percentages.
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean.

Additional Output
-------
### One-Sample T-test:
- Mean Difference: Mean of the differences between the data points and the test value.
- Cohen's d: Cohen's d measure of effect size.
- x% Confidence Interval: Lower and upper bound of the x% confidence interval for the population mean.

### Descriptives:
- N: Sample size.
- Mean: Sample mean. 
- SD: Sample standard deviation.
- SE: Standard error of the mean.

References
-------
 - Moore, D.S., McCabe, G.P., Craig, B.A. (2012). Introduction to the Practice of Statistics (7th ed.). New York, NY: W.H. Freeman and Company.
 - Whitlock, M.C. & Schluter, D. (2015). The Analysis of Biological Data (2nd ed.). Greenwood Village, Colorado: Roberts and Company Publishers.
