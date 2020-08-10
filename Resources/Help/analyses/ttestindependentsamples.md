Independent Samples T-Test
==========================

The independent samples t-test allows the user to estimate the effect size and test the null hypothesis that the population means of two independent groups are equal. 

### Assumptions
- The dependent variable is continuous. 
- The observations in both groups are a random sample from the population. 
- The dependent variable is normally distributed in both populations. 
- The population variances in the two groups are homogeneous. 

### Input
-------

#### Assignment Box 
- Variables: In this box the dependent variable is selected.  
- Grouping Variable: In this box the variable defining the groups is selected. 

#### Tests 
- Student: Student's t-test. This option is selected by default. 
- Welch: Welch's t-test. 
- Mann-Whitney: Mann-Whitney test. 

#### Alt. Hypothesis 
- Group 1 &ne; Group 2: Two-sided alternative hypothesis that the population means are equal. This options is selected by default. 
- Group 1 &gt; Group 2: One-sided alternative hypothesis that the population mean of Group 1 is larger than the population mean of Group 2. 
- Group 1 &lt; Group 2: One-sided alternative hypothesis that the population mean of Group 1 is smaller than the population mean of Group 2. 

#### Assumption Checks 
- Normality: Shapiro-Wilk test of normality. 
- Equality of variances: Levene's test for homogeneity of variances. 

#### Additional Statistics
- Location parameter: For the Student's t-test and Welch's t-test, the location parameter is given by mean difference; for the Mann-Whitney test, the location parameter is given by the Hodges-Lehmann estimate.
  - Confidence interval: Confidence interval for the location parameter. By default, the confidence interval is set to 95%. This can be changed into the desired percentage.
- Effect Size: For the Student t-test and Welch t-test, the effect size can be selected below; for the Mann-Whitney test, the effect size is given by the rank biserial correlation. 
  - Cohen's d: For the Student's t-test, uses the pooled standard deviation to standardize the mean difference. For the Welch's t-test, uses the square-root of the average variance to standardize the mean difference.
  - Glass' delta: Uses the standard deviation of group 2 to standardize the mean difference. In order to change which group is used as group 2, you can change the order of the levels by clicking on the name of the grouping variable in the data window, click on one of the levels and then click the arrow buttons to switch the order.
  - Hedges' g: Applies a correction factor to Cohen's d to make it unbiased.
  - Confidence interval: Confidence interval for the effect size based on the non-central t-distribution for Cohen's d, Glass' delta and Hedges' g, and normal approximation of the Fisher transformed rank biserial correlation. 
- Descriptives: Sample size, sample mean, sample standard deviation, standard error of the mean for each group. 
- Descriptive plots: Displays the sample means and the confidence intervals for each group. 
  - Confidence interval: Coverage of the confidence intervals in percentages. By default, the confidence interval is set to 95%. This can be changeed into the desired percentage.
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

### Missing Values
 - Exclude cases per dependent variable: In case of multiple t-tests within a single analysis, each test will be conducted using all cases with valid data for the dependent variable for the particular t-test. Sample sizes may therefore vary across the tests. This options is selected by default.
 - Exclude cases listwise: In case of multiple t-tests within a single analysis, each t-test will be conducted using only cases with valid data for all dependent variables. Sample size is therefore constant across the tests.

### Output
-------

#### Independent Samples T-Test 
- The first column contains the dependent variable. 
- Test: The type of t-test that is selected. If only one test is selected, this column will not be displayed. In this scenario, the table only displays the results for the selected test. 
- t: The value of the t-value. 
- W: The test statistic of the Wilcoxon test. The two most common definitions correspond to the sum of the ranks of the first sample with the minimum value subtracted or not. JASP uses the definition with the subtraction (also used by R).
- df: Degrees of freedom. 
- p: The p-value. 
- Mean difference: Difference in sample means. This column is only named 'Mean difference' when the tests `Student` or `Welch` are selected. When the test `Mann-Whitney` is selected, this column is called 'Location parameter'. 
- Location parameter: For the Student's t-test and Welch's t-test, the location parameter is given by mean difference; for the Mann-Whitney test, the location parameter is given by the Hodges-Lehmann estimate. This column is only called 'Location parameter' when `Mann-Whitney` t-test is selected, otherwise this column is called 'Mean difference'. 
- SE Difference: Standard error of the difference in means. This is only displayed for the Student's t-test and Welch's t-test. 
- % CI for Mean difference/location parameter: The confidence interval for the mean difference/location parameter. By default this is set to 95%. 
  - Lower: The lower bound of the confidence interval. 
  - Upper: The upper bound of the confidence interval. 
- Effect Size: For the Student t-test and Welch t-test, the effect size is given by Cohen's d/Glass' delta/Hedges' g; for the Mann-Whitney test, the effect size is given by the rank biserial correlation. 
- % CI for Effect Size: The confidence interval for the effect size. By default this is set to 95%. 
    - Lower: The lower bound of the confidence interval. 
    - Upper: The upper bound of the confidence interval. 

#### Assumption Checks 
Test of Normality (Shapiro-Wilk):
- The first column contains the dependent variable. 
- The second column contains each level of the grouping variable. 
- W: The value of the W test statistic. 
- p: The p-value. 

Test of Equality of Variances (Levene's):
- The first column contains the dependent variable. 
- F: The value of the F-statistic. 
- df: The degrees of freedom. 
- p: The p-value. 

#### Descriptives 
- The first column contains the dependent variable. 
- Group: The levels of the grouping variable. 
- N: The sample size per group. 
- Mean: The mean of the dependent variable per group. 
- SD: Standard deviation of the mean. 
- SE: Standard error of the mean. 

##### Descriptive Plots 
- Displays the sample means (black bullet), the x% confidence intervals (whiskers) for each group. The x-axis represents the grouping variable, and the y-axis the dependent variable. 

### References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

### R-packages
---
- stats 
- car 
- MBESS

### Example 
--- 
- For an example go to `Open`--> `Data Library` --> `T-Tests` --> `Directed Reading Activities`. 


