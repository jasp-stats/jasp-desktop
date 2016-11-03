One Sample T-Test
==========================

The one sample t-test allows you to test the null hypothesis that the population mean equals specific constant, i.e., the test value

Assumptions
-----------
- Continuous dependent variable
- The data are a random sample from the population
- The dependent variable is normally distributed in the population

Default Options
-------
### Test value:
-Test value specified in the null hypothesis

### Hypothesis:
- &ne; Test value: Two-sided alternative hypothesis that the population mean is not equal to the test value
- &gt; Test value: One-sided alternative hypothesis that the population mean is larger than the test value
- &lt; Test value: One sided alternative hypothesis that the population mean is smaller than the test value

### Missing Values:
 - Exclude cases analysis by analysis: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test.
 Sample sizes may therefore vary across the tests.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.

Default Output
-------
### One Sample T-Test:
- t: t-value
- df: Degrees of freedom
- p: p-value

Additional Options
-------
### Additional Statistics:
- Mean difference: Average difference between the data points and the test value
- Effect size: Cohen's d measure of effect size
- Confidence interval: Confidence interval for the population mean
  - Interval: Coverage of the confidence interval in percentages
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean
- Normality tests: Shapiro-Wilk test of normality
- Descriptive plots: Displays the sample mean and the confidence interval
  - Confidence interval: Coverage of the confidence interval in percentages
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.  
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

Additional Output
-------
### One-Sample T-test:
- Mean Difference: Average difference between the data points and the test value
- Cohen's d: Cohen's d measure of effect size
- x% Confidence Interval: Lower and Upper bound of the x% confidence interval for the population mean

### Descriptives:
- N: Sample size
- Mean: Sample mean
- SD: Sample standard deviation
- SE: Standard error of the mean

### Test of Normality (Shapiro-Wilk):
- W: W test statistic
- p: p-value

### Descriptive Plots:
- Displays the sample mean (black bullet), the x% confidence interval (whiskers), and the value of the test statistic (dashed line)

References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
