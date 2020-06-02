Correlation
===

The Correlation analysis allows estimation of the population correlation, as well as testing the null hypothesis that the population correlation between pairs of variables equals 0. All possible pairs of the specified variables are analyzed.


### Assumptions (Pearson's rho)
- Continuous variables.
- The data are a random sample from the population.
- The pairs of variables follow a bivariate normal distribution in the population.
- The relationship between the pairs of variables is linear.


### Assumptions (Spearman's rho & Kendall's tau)
- Ordinal or continuous variables.
- The data are a random sample from the population.
- The relationship between the pairs of variables is monotonic.

### Input
---

#### Sample Correlation Coefficient
- Pearson's r: Pearson's product moment correlation coefficient.
- Spearman: Spearman's rank-order correlation coefficient to quantify the monotonic association between two variables.
- Kendall's tau-b: Kendall's tau-b rank-order correlation coefficient to quantify the monotonic association between two variables.

#### Alt. Hypothesis
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0.
- Correlated positively: One-sided alternative hypothesis that the population correlation is greater than 0.
- Correlated negatively: One-sided alternative hypothesis that the population correlation is lower than 0.

#### Additional Options
- Display pairwise: Display a table where one row corresponds to one pair of the specified variables, and the scatter plots are shown individually for each pair. If unticked, the results are presented in matrix format, with variable names in the columns and rows.
- Report significance: Display the p-value corresponding to the observed correlation.
- Flag significant correlations: Mark statistically significant correlations.
- Confidence Intervals: Confidence intervals for the population correlation (confidence intervals for nonparametric correlations follow the z-approximation of Hollander, Wolfe & Chicken (2013)).
  - Interval: Coverage of the confidence interval in percentages.
- Vovk-Selke maximum p-ratio: The bound 1/(-e p log(p)) is derived from the shape of the p-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform (0,1), and under the alternative (H<sub>1</sub>) it is decreasing in p, e.g., a beta (α, 1) distribution, where 0 < α < 1. The Vovk-Sellke MPR is obtained by choosing the shape α of the distribution under H1 such that the obtained p-value is maximally diagnostic. The value is then the ratio of the densities at point p under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H<sub>0</sub>.
- Sample size: The number of complete observations for a given pair of variables.

#### Plots
- Scatter plots: Display a scatter plots for each possible combination of the selected variables. In a matrix format, these are placed above the diagonal.
  - Densities for variables: Display histogram and the corresponding density plot for each variable. In a matrix format, these are placed on the diagonal.
  - Statistics: Display the correlation coefficient(s) in the plot. This option also adds the x% confidence interval(s) as specified in the "Confidence Intervals" option.
- Heatmap: Display a correlation heatmap for Pearson, Spearman, and Kendall's tau B coefficients separately.

#### Assumption checks

- Multivariate normality
  - Shapiro: Computes the Shapiro-Wilk statistic to test the null hypothesis that the selected variables have multivariate normal distribution.

- Pairwise normality
  - Shapiro: For each possible combination of the selected variables, computes the Shapiro-Wilk statistic to test the null hypothesis that the variable pair has a bivariate normal distribution.

#### Options

- Missing values
  - Exclude cases pairwise: Uses all complete observations for each individual pair of variables.
  - Exclude cases listwise: Uses only complete cases across all variables.

### Output
---
#### Correlation Table
- Pearson r: Pearson's product-moment correlation coefficient.
- Spearman rho: Spearman's rank correlation coefficient.
- Kendall tau:  Kendall's tau b rank correlation coefficient.
- p: The p-value.
  - Significant correlations are marked with:
    - *p < .05 if the correlation is significant at alpha=.05 level.
    - **p < .01 if the correlation is significant at alpha=.01 level.
    - ***p < .001 if the correlation is significant at alpha=.001 level.
- Vovk-Sellke Maximum *p*-Ratio: For an explanation, see Vovk-Sellke under `Options`.
- Upper x% CI: Upper bound of the x% confidence interval for the population correlation.
- Lower x% CI: Lower bound of the x% confidence interval for the population correlation.
- n: Sample size.

#### Assumption checks

- Shapiro-Wilk: The Shapiro-Wilk statistic
- p: The p-value of the assumption check

#### Correlation Plot
- Scatter plots: Displays a (matrix of) scatter plot(s) between the variables (in the upper off-diagonal entries of the matrix). The black line represents the least-square regression line
    - Densities for variables: Displays a histogram and the corresponding density plot for each variable.
    - Statistics: Displays the correlation coefficient(s) and, if requested, also the corresponding x% confidence interval(s)
      - r: Pearson's product-moment correlation coefficient
      - rho: Spearman's order correlation coefficient
      - tau: Kendall's tau b order correlation coefficient

#### Heatmap
- Displays a correlation heatmap for Pearson, Spearman, and Kendall's tau separately. The heatmap is symmetric along the diagonal. Blue colors correspond to positive correlation coefficients, red colors correspond to negative correlation coefficients. The saturation of colors reflects the absolute value of the correlation coefficient. If "Flag significant correlations" is selected, the significant correlations are marked with:
  - *p < 0.05 if the correlation is significant at alpha=.05 level.
  - **p < .01 if the correlation is significant at alpha=.01 level.
  - ***p < .001 if the correlation is significant at alpha=.001 level.

### References
-------
- Hollander, M., Wolfe, D. A., & Chicken, E. (2013). *Nonparametric statistical methods (3rd ed.)*. John Wiley & Sons.
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.

### R Packages
---
- ggplot2
- grid
- stats
- ppcor
