Repeated Measures ANOVA
===

The repeated Measures ANOVA allows the user to analyze the differences between means, when observations are dependent.

### Assumptions
- The dependent variable is normally distributed for every group.
- The covariate and the experiment effect are independent.
- The assumption of sphericity is met. Sphericity entails that the variances of the differences of the repeated measures conditions all have the same variance.

### Input
---

#### Assignment Box
- Repeated Measures Factors: The within-subjects (repeated measures) variable. Here the repeated measures factors of interest and the different levels that belong to the factor can be labelled.
- Repeated Measures Cells: The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s).
- Between Subject Factors: When the subjects have been assigned into two or more separate groups this variable can be selected.
- Covariates: In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.

### Model
- Components and model terms:
  - Repeated Measures Components: All the repeated measures factors and covariates that can be included in the model.
      - Model terms: The repeated measures factors and covariates that can be included in the model.
  - Between Subjects Components: All the between subjects factors and covariates that can be included in the model.
      - Model terms: The repeated measures factors and covariates that can be included in the model.

- Sum of Squares: There are different types of the sum of squares. The choice of the type is important when there are multiple factors and when the data are unbalanced. In an unbalanced design, the different levels of the repeated measures factor do not contain an equal number of observations (e.g., one group contains more observations than another group). In this scenario, the sum of squares type can influence the results.   
    - Type I: Sequential sum of squares. It is the reduction of error when each factor of the model is added to the factors already included, preserving the order of factors in the model. The results depend on the order in which the factors are added to the model. This is important to consider when the model contains more than one factor.
    - Type II: Hierarchical/partially sequential sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, except the factors where the added factor is a part of, such as interactions containing that factor. Langsrud (2003) advises to apply this type for an ANOVA with unbalanced data.
    - Type III: Partial sum of squares. It is the reduction of error when each factor is added to the model that includes all the other factors, including interactions with this factor. This type is often selected, because it takes interactions into account (Langsrud, 2003). This type is selected by default.

### Assumption checks
- Sphericity tests: Sphericity entails that the variances of the differences of the repeated measures conditions all have the same variance.
- Sphericity corrections: If sphericity is not met, then the test will have increased false positive rate. One approach to correcting this is to reduce the degrees of freedom by sphericity corrections.
  - None: No correction is performed.
  - Greenhouse-Geisser: This correction varies between 1/(k − 1), where k is the number of repeated-measures conditions, and 1.
  - Huyn-Feldt: Another common method to correct the degrees of freedom is Huynh-Feldt correction.
- Homogeneity tests: By selecting this option, it will be checked whether the variance of the dependent variable is equal between the groups by performing Levene's test of equal variances.  

### Contrasts
For each repeated measures factor, a specific contrast can be selected by clicking on `none` in the right column.
- Factors: These are the repeated measures factors included in the analysis.  
- Contrasts: Contrasts enable the analysis of planned comparisons. There are different contrasts that enable different types of comparisons:
    - none: By selecting this option, no contrasts are calculated. This option is selected by default.
    - deviation: By selecting this contrast, the mean of each level of the repeated measures factor is compared to the overall mean (the mean when all the levels are taken together).
    - simple: When this contrast is selected, the mean of each level is compared to the mean of a specified level, for example with the mean of the control group.
    - difference: This contrast is also called reverse Helmert. By selecting this contrast, the mean of each level is compared to the mean of the previous levels.
    - Helmert: When this contrast is selected, the mean of each level is compared to the mean of the subsequent levels. This is the reverse of the difference contrast.
    - repeated: By selecting this contrast, the mean of each level is compared to the mean of the following level.
    - polynomial: This contrast tests polynomial trends in the data. The specific polynomial that will be used for the analysis depends on the number of levels of the repeated measures factor. The degree of the trend used for the analysis is the number of levels minus 1. Therefore, if the repeated measures factor consist of 2 levels, a linear trend is analysed. If the repeated measures factor consists of three levels, a quadratic trend is analysed in addition to the linear trend.

### Post Hoc Tests
To perform a post hoc test, drag one or more factor names to the right column. Several options are available:    
- Effect size: By selecting this option, the effect size (i.e., the magnitude of the observed effect) will be displayed. The used measure for the effect size is Cohen's d. The effect size will only be displayed for the post hoc type `Standard`.
- Confidence intervals: When this option is selected, the confidence interval for the mean difference is calculated. This is done for every post hoc method except for Dunn. By default this is set to 95% but this can be adjusted into the desired percentage.
- Pool error term for RM factors: A pooled error term assumes that the variances of the contrast scores are approximately equal (i.e., sphericity assumption) See Morey (2008) for more details.
- Correction: To correct for multiple comparison testing and avoid Type I errors, different methods for correcting the p-value are available:  
    - Tukey: Compare all possible pairs of group means. This correction can be used when the groups of the repeated measures have an equal sample size and variance. This method is commonly used and is selected by default.
    - Scheffe: Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be quite conservative.
    - Bonferroni: This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.
    - Holm: This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method.

### Descriptives Plots
To create a descriptive plot, select the repeated measures factor to be placed on the horizontal axis. If there are more than one repeated measures factor, the variables can be displayed in one plot by putting the other variable in the box Separate lines, or the variables can be displayed in separate plots by selecting the other variable in the box Separate plots.
- Factors: The repeated measures factor included in the analysis.
- Horizontal axis: Select the repeated measures factor that should be displayed on the horizontal axis of the plot.
- Separate lines: By placing a repeated measures factor in this box, different lines corresponding to the different levels of the repeated measures factor will be displayed.
- Separate plots: By placing a repeated measures factor in this box, different plots corresponding to the different levels of the repeated measures factor will be displayed.
- Label y-axis: The label of the y-axis can be changed manually.
- Display:
    - Display error bars: By selecting this option, error bars will be displayed in the plot. The error bars can either represent confidence intervals or standard errors. In order to get accurate confidence intervals and standard errors, the data are normalized by subtracting the appropriate participantʹs mean performance from each observation, and then adding the grand mean score to every observation. The variances of the resulting normalized values in each condition, and thus the size of the bars, no longer depend on the participant effects and are therefore a more accurate representation of the experimental manipulation. See Morey (2008) for a thorough discussion of this procedure.
        - Confidence interval: This option is selected by default. With this option, the error bars will represent confidence intervals of the mean of each level combination of the repeated measures factors. By default the confidence interval is set to 95%, but this can be changed  into the desired percentage.
        - Standard error: By selecting this option, the error bars will represent standard errors of the mean of each level combination of the repeated measures factor.
    - Average across unused RM factors: When there are multiple RM factors in the model, but only plotting a subset of these factors, the mean is taken across the unused RM factors. For instance, when there are two RM factors with two levels in the model, A (1&2) and B (1&2), and only A is selected to be plotted, the average is taken of B across its levels. This means that when the mean of A1 is plotted, it is actually the average of A1B1 and A1B2). This procedure is discussed by Loftus & Masson (1994). When the box is not ticked, the averages are not taken, and the columns A1B1 and A1B2 are simply concatenated.


### Additional Options:
- Marginal means: When this option is selected, the mean for each level of the repeated measures factor, adjusted for all the other variables in the model, is calculated.
- Compare marginal means to 0: By selecting this option, the adjusted means are compared to 0 and the confidence intervals of the adjusted means are calculated.
    - Confidence interval adjustment: The confidence intervals can be adjusted in several ways.
        - None: When this option is selected, no adjustment will be applied.
        - Bonferroni: Bonferroni correction of the confidence intervals.
        - Sidak: Sidak correction of the confidence intervals.
- From `...` bootstraps: When this option is selected, the bootstrapped marginal means are calculated. By default, the number of replications is set to 1000. This can be changed into the desired number.
- Display:
    - Descriptive statistics: When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the repeated measures factors.
    - Estimates of effect size: By selecting this option, the specific types of calculations to estimate the effect size can be specified.
        - &eta;<sup>2</sup> : When this option is selected, the eta-squared is calculated as an estimate of the effect size. However, this method is considered to overestimate the population variance, making it hard to compare the effect of the same variable across different studies (Goss-Sampson, 2018).
        - partial &eta;<sup>2</sup> : When this option is selected, the Partial eta-squared is calculated as an estimate of the effect size. This method is considered to solve the problem of overestimation of the population variance, which makes it less difficult to compare the effect of the same variable from different studies (Goss-Sampson, 2018).
        - &omega;<sup>2</sup> : When this option is selected, the Omega squared is calculated as an estimate of the effect size. This is considered a good estimate when the sample size is small (Goss-Sampson, 2018).
    - Vovk-Selke maximum p-ratio: The bound 1/(-e p log(p)) is derived from the shape of the p-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform (0,1), and under the alternative (H<sub>1</sub>) it is decreasing in p, e.g., a beta (α, 1) distribution, where 0 < α < 1. The Vovk-Sellke MPR is obtained by choosing the shape α of the distribution under H1 such that the obtained p-value is maximally diagnostic. The value is then the ratio of the densities at point p under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H<sub>0</sub>.

### Simple Main Effects:
The simple main effects represent the effect of one repeated measure factor for each level of the other repeated measures factor, by conducting an ANOVA for each subset of the data as specified by the moderator variables.
- Factors: This box contains all the repeated measures factors included in the analysis.
- Simple effect factor: In this box, select the repeated measures factor to determine the effect of this variable, conditional on the levels of the moderator factor(s).
- Moderator factor 1: In this box, select the repeated measures factor that will represent the different levels.
- Moderator factor 2: In this box, selector an optional, additional repeated measures factor.
- Pool error terms: A pooled error term assumes that the variances of the contrast scores are approximately equal (i.e., sphericity assumption).

#### Nonparametrics
The Friedman test is a non-parametric alternative to the Repeated-Measures ANOVA when there is a complete block design. The Durbin test will automatically be selected when there is an incomplete block design.
- Factors: This box contains all the repeated measures factors included in the analysis.
- RM factor: The repeated measures factor(s) of interest.
- Optional Grouping Factor: Possible to select the between subjects factor here.
- Conover's post hoc tests: Conover's post-hoc test for pairwise comparisons, if the non-parametric test indicates significance.

### Output
---
#### Repeated Measures ANOVA
Within Subject Effects:
- Sphericity Correction: The selected corrections when the assumption of sphericity is not met.
- Sum of Squares: The summed squared within group-mean differences.
- df: Degrees of freedom
- Mean Square: The estimate of population variance (the sum of squares divided by df's).
- F: The value of the F-statistic.
- p: p-value

Between Subjects Effects:
- Sum of squares: The summed squared between group-mean differences.
- df: Degrees of freedom
- Mean Square: The estimate of population variance (the sum of squares divided by df's).
- F: The value of the F-statistic.
- p: The p-value.

#### Assumption Checks
Test of Sphericity:
- Mauchly's W: Mauchly's test statistic.
- p: p-value.
- Greenhouse-Geisser &epsilon;: The Greennhouse-Geisser correction. A value of 1 indicates that sphericity holds and any value < 1 indicates sphericity is violated.
- Huynh-Feldt &epsilon;: The Huynh-Feldt correction.

#### Contrasts
Deviation/Simple/Difference/Helmert/Repeated/Polynomial Contrast:
- Comparison: The levels of the repeated measures factor that are compared.
- Estimate: The estimated mean difference between the compared levels.
- SE: The standard error of the estimated mean.
- df: The degrees of freedom of the model.
- t: The value of the t-statistic.
- p: The p-value.

#### Post-Hoc Tests
Post Hoc Comparisons:  
 - The first two columns represent the levels of the repeated measures factor that are compared with each other.
- Mean Difference: The mean difference between the levels.
- % CI for Mean Difference: The confidence interval of the mean difference between the compared levels. By default this is set to 95%.
    - Lower: The lower bound of the confidence interval.
    - Upper: The upper bound of the confidence interval.
- SE: The standard error of the mean difference.
- t: The value of the t-statistic.
- Cohen's D: The effect size Cohen's d. Cohen's d does not correct for multiple comparisons.
- p<sub>tukey</sub>: Tukey's corrected p-value for multiple comparisons.
- p<sub>scheffe</sub>: Scheffe's corrected p-value for multiple comparisons.
- p<sub>bonf</sub>: Bonferroni's corrected p-value for multiple comparisons.  
- p<sub>holm</sub>: Holm's corrected p-value for multiple comparisons.

Simple Main Effects:
- Level: The levels/groups of the repeated measures factor that are compared with each other.
- Sum of Squares: The summed squared between levels-mean differences.
- df: The degrees of freedom of the model.
- Mean Square: The estimate of population variance (the sum of squares divided by df's).
- F: The value of the F-statistic.
- p: The p-value.

#### Marginal Means
Marginal Means - Repeated measures factor:
- The first column contains the levels of the repeated measures factor.
- Marginal Mean: The marginal mean for each level of the repeated measures factor. This mean is adjusted for all the other variables in the model.
- SE: The standard error of the marginal mean.
- Lower CI: The lower bound of the confidence interval.
- Upper CI: The upper bound of the confidence interval.
- t: The value for the t-statistic.
- p: The p-value.

#### Marginal Means via Bootstrapping
Bootstrapped Marginal Means - Repeated measures factor:
- Repeated measures factor: This column contains all the levels of the repeated measures factor.
- Marginal Mean: The estimate of the marginal mean for each level of the repeated measures factor. This mean is adjusted for all the other variables in the model. The estimate is based on the median of the bootstrap distribution.
- Bias: The average difference between the bootstrapped marginal mean and the estimated marginal mean.
- SE: The standard error of the bootstrapped marginal means.  
- 95% bca CI for Mean Difference: The bias corrected accelerated confidence interval of the mean difference between the compared levels. By default this is set to 95%.
  - Lower: The lower bound of the confidence interval.
  - Upper: The upper bound of the confidence interval.

#### Nonparametrics
Friedman Test / Durbin Test:
- Factor: The repeated measures factor included in the analysis.
- Chi-square: The chi-squared test statistic.
- df: Degrees of Freedom.
- p: The p-value.
- Kendall's W: Kendall’s W Test is referred to the normalization of the Friedman/Durbin statistic.
- F: The value of the F-statistic.
- df num: Degrees of freedom used in determining the p-values of the F statistics.
- df den: Degrees of freedom used in determining the p-values of the F statistics.
- p<sub>f</sub>: The p-value of the F-statistic.

Conover's Post Hoc Comparisons:
- The first two columns represent the levels/groups of the repeated measures factor that are compared to each other.
  - T-Stat: The test statistic that follows the t-distribution.
- df: Degrees of Freedom.
- W<sub>i</sub>: Sum of the aggregated ranks of level 1
- W<sub>j</sub>: Sum of the aggregated ranks of level 2
- p: The p-value.
- p<sub>bonf</sub>: Bonferroni's corrected p-value for multiple comparisons.  
- p<sub>holm</sub>: Holm's corrected p-value for multiple comparisons.

#### Descriptive plots
The independent variable / repeated measures factor on the x-axis and dependent variable on the y-axis. If other repeated measures factors are included, either different lines representing different values of the other repeated measures factor are displayed in the same plot, or different plots representing different values of the other repeated measures factor are displayed.

### References
---
- Conover,W. J. (1999). *Practical nonparametric Statistics, 3rd. Edition*, Wiley.
- Morey, R. D. (2008) Confidence Intervals from Normalized Data: A correction to Cousineau (2005). Tutorial in *Quantatitative Methods for Psychology, 4*(2), 61-64.
- Loftus, G. R., & Masson, M. E. J. (1994). Using confidence intervals in within-subject designs. *Psychonomic Bulletin and Review, 1*, 476–490.

### R-packages
---
- afex
- boot
- emmeans
- ggplot2
- plyr
- stats
