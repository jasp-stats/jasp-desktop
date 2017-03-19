Correlation Matrix
==========================

Correlation Matrix allows you to test the null hypothesis that the population correlation equals 0.

Assumptions for null hypothesis significance testing using the Pearson product-moment correlation coefficient
-----------
- Continuous variables
- The data are a random sample from the population
- The pairs of variables follow a bivariate normal distribution in the population

Assumptions for null hypothesis significance testing using the Spearman's rank-order correlation coefficient and the Kendall's tau B rank-order correlation coefficient
-----------
- Ordinal or continuous variables
- The data are a random sample from the population
- The relationship between the pairs of variables is monotonic

Default Options
-------
### Correlation Coefficients:
- Pearson: Pearson product-moment correlation coefficient to quantify the linear association between two variables

### Hypothesis:
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0
- Correlated positively: One-sided alternative hypothesis that the population correlation is higher than 0
- Correlated negatively: One-sided alternative hypothesis that the population correlation is lower than 0

### Report significance: Report p-value for each significance test

Default Output
-------
### Pearson Correlations (Note that this table is called "Correlation Table" if Spearman's rank-order correlation and/or the Kendall's tau B rank-order correlation is requested):
- Pearson's r: Pearson product-moment correlation coefficient
- p-value: p-value

Additional Options
-------
### Correlation Coefficients:
- Kendall's tau-b: Kendall's tau B rank-order correlation coefficient to quantify the monotonic association between two variables
- Spearman: Spearman's rank-order correlation coefficient to quantify the monotonic association between two variables

### Flag significant correlations: Marks statistically significant correlations

### Confidence Intervals: Confidence intervals for the population correlation (only available for the Pearson correlation)
  - Interval: Coverage of the confidence interval in percentages

### Plots
  - Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables
    - Densities for variables: Adds a histogram and the corresponding density plot for each variable to the Correlation Matrix plot
    - Statistics: Adds the correlation coefficient(s) to the Correlation Matrix plot. If only the Pearson correlation is requested, this option also adds the x%
      confidence interval(s) as specified in the "Confidence Intervals" option

Additional Output
-------
### Correlation Table:
- Spearman's rho: Spearman's rank-order correlation coefficient
- Kendall's tau B: Kendall's tau B rank-order correlation coefficient
- Significant correlations are marked with:
  - *p < .05 if the correlation is significant at alpha=.05 level
  - **p < .01 if the correlation is significant at alpha=.01 level
  - ***p < .001 if the correlation is significant at alpha=.001 level
- Upper x% CI: Upper bound of the x% confidence interval for the population correlation
- Lower x% CI: Lower bound of the x% confidence interval for the population correlation
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>.
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.


### Plots:
- Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables (in the upper off-diagonal entries of the matrix). The black line represents the least-square regression line
    - Densities for variables: Displays a histogram and the corresponding density plot for each variable in the diagonal entries of the matrix
    - Statistics: Displays the correlation coefficient(s) and, in case only the Pearson's correlation is requested, also the corresponding x% confidence interval(s) in the lower off-diagonal entries of the
    matrix
      - r: Pearson product-moment correlation coefficient
      - rho: Spearman's rank-order correlation coefficient
      - tau: Kendall's tau B rank-order correlation coefficient

References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
