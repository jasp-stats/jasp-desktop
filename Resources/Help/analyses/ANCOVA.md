# ANCOVA 
---
ANCOVA allows the user to analyze the difference among three, or more, group means, when partialling out the effect of variables that have an influence on the dependent variable but are not part of the experimental manipulation (covariates). 

### Assumptions 
- The dependent variable should be normally distributed for every group.
- The independent variables are categorical, the dependent variable should be continuous or ordinal. 
- The variance of the dependent variable should be the same for every group. This is called homogeneity. 
- The groups are independent. 
- The covariate and the experiment effect have to be independent. 
- Homogeneity of the regression slopes, meaning that the effect of the covariate on the dependent variable is not allowed to differ between groups.  

## Input 
--- 
### Assignment Box 
- Dependent Variable: The variable you are interested in. Also called the outcome variable. 
- Fixed Factors: The variables that you manipulated/define the different groups. Also called the independent variables.  
- Covariates: In this box you can place the variable that is the covariate. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.  
- WLS Weights: Weighted Least Squares, here you can place the variable specifying which points have more weight and are therefore considered more informative. For this last option it is important to know the weights a priori. This option is mainly used when the errors are heteroskedastic.    

#### Model: 
- Components and model terms 
    - Components: All the independent variables you can put in the model. 
    - Model terms: The independent variables and covariates being used in the model. By default all the independent variables and covariates included in the model are placed in this box, plus the interaction between the independent variables.  
- Sum of Squares 
    - Type I: Sequential sum of squares. It is the reduction of error when each factor of the model is added to the factors already included, containing the order in the model. 
    - Type II: Hierarchical/Partially sequential sum of squares. It is the reduction of error when a factor is added to the model that includes all the other factors, except the factors where the added factor is part of. Therefore, it does not include interactions. 
    - Type III: Partial sum of squares. It is the reduction of error when a factors is added at last to the model, a model that includes all the other factors, including interactions with this factor. This type is selected by default. 

#### Assumption Checks: 
- Homogeneity tests: By selecting this option it will be checked if the variance of the dependent variable is equal between the groups by using Levene's test.  
- Q-Q plot of residuals: Checks the validity of the distributional assumption of the data set. More specific, from the plot it could be retrieved if the data is normally distributed and linear. 

#### Contrasts: 
For every independent variable you can select a specific contrast by clicking on the `none` in the right column.  
- Factors: These are the independent variables included in the analysis, so the variables placed in the `Fixed Factors` box.  
- Contrasts: Contrast allow you to conduct planned comparisons, for example to compare the different levels of the independent variable with each other. There are different contrasts that allow for different types of comparisons.  
    - none: By selecting this option no contrasts are calculated. This option is selected by default. 
    - deviation: By selecting this contrast the mean of each level of the independent variable is compared with the overall mean (the mean when all the levels are taken together). 
    - simple: When this contrast is selected the mean of each level is compared to the mean of a specified level, for example with the mean the control group. 
    - difference: This contrast is also called reverse Helmert. By selecting this contrast the mean of each level is compared to the mean of the previous levels. 
    - Helmert: When this contrast is selected the mean of each level is compared to the mean of the following levels. This is the reverse of the difference contrast. 
    - repeated: By selecting this contrast the mean of each level is compared to the mean of the following level. 
    - polynomial: This contrast test polynomial trends in the data. The specific polynomial that will be used for the analysis depends on the amount of levels of the independent variable. The degree of the trend used for the analysis is the amount of levels - 1. Therefore, if the independent variable consist of 2 levels, a linear trend is analysed. If the independent variable consists of three levels, a quadratic trend is analysed besides to linear. 
- Assume equal variances: You can select this option when you assume that the variances of the levels of the independent variable are similar. This option is selected by default. 
- Confidence Intervals: By selecting this option, confidence intervals for the estimated mean difference will be given. By default this is set to 95%. You can change this into the desired percentage. 

#### Post Hoc Tests: 
If there is a significant effect of the independent variable when is accounted for the covariate, you can follow up the analysis by performing a post hoc test to see which levels of a independent variable exactly differ. To perform a post hoc test drop the factor to perform the post hoc test on in the right column. Then you can select:    
- Effect size: By selecting this option the effect size, referred as the magnitude of the observed effect, will be presented. The used measure for the effect size is Cohen's d. The effect size will only be presented for the post hoc type `Standard`. 
- Confidence intervals: When this option is selected, the confidence interval for the mean difference is calculated. This is done for every post hoc method except for Dunn. By default this is set to 95% but you can adjust this into the desired percentage.  
- Correction: To correct for multiple comparison testing and avoid Type I errors, you can select different correction methods. 
    - Tukey: Compare all possible pairs of group means. This correction can be used when the groups of the independent variable have the same sample size and variance. It is a considered a common used correction and is selected by default. 
    - Scheffe: Adjusting significance levels in a linear regression, to account for multiple comparisons. This method is considered to be very strict on Type I errors. 
    - Bonferroni: This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well. 
    - Holm: This method is also called sequential Bonferroni, and considered less conservative than the Bonferroni method. 
- Type: You can select different types of post hoc tests. 
    -  Standard: Multiple t-tests are performed. All the corrections can be applied to this method. This option is selected by default. 
    -  Games-Howell: This method can be used when you do not assume equal group/level variances. The Tukey correction is given for this method.  
    -  Dunnett: When selecting this method, all the levels are compared to one specific level, for example to the control group. 
    -  Dunn: This is a non-parametric test that can be used for testing small subsets of pairs. This post hoc test is a follow up for the Kruskall-Wallis test. The Bonferroni and Holm correction can be applied to this method. 

#### Descriptive Plots:  
If you want a descriptive plot you can place the independent variable on the horizontal axis. If you have more than one independent variable, you can decide to display the variables in one plot by putting the other variable in the box `Separate lines`, or decide to show them in separate plots by dragging the other variable in the box `Separate plots`.
- Factors: The independent variables included in the analysis. 
- Horizontal axis: Here you can place the independent variable that you would like to be displayed on the horizontal axis in the plot.  
- Separate lines: By placing a independent variable in this box, different lines representing the different levels of the independent variable will be created. 
- Separate plots: By placing a independent variable in this box, different plots for the different levels of the independent variable will be created. 
- Display: 
    - Display error bars: By selecting this option error bars will be displayed in the plot. The error bars can either represent confidence intervals or standard errors. 
        - Confidence interval: This option is selected by default. With this option, the error bars will represent confidence intervals of the mean of each level combination of the independent variables. By default the confidence interval is set to 95%, but you can change this into the desired percentage.  
        - Standard error: By selecting this option, the error bars will represent standard errors of the mean of each level combination of the independent variables. 

#### Additional Options: 
- Marginal means: When this option is selected, the mean for each level of the independent variable, adjusted for all the other variables in the model, is calculated. 
- Compare marginal means to 0: By selecting this option the adjusted means are compared to 0. With this option the confidence intervals are calculated as well. 
    - Confidence interval adjustment: You can adjust the confidence intervals in several ways. 
        - None: When this option is selected, no adjustment will be applied. 
        - Bonferroni: Bonferroni correction of the confidence intervals. 
        - Sidak: Sidak correction of the confidence intervals. 
- Display: 
    - Descriptive statistics: When this option is selected the mean, standard deviation, and the sample size will be presented for each level combination of the independent variables. 
    - Estimates of effect size: By selecting this option you can choose specific types of calculations to estimate the effect size. 
        - &eta;$$^2$$ : When this option is selected, the Eta-squared is calculated as estimate for the effect size. However, this method is considered to overestimate the population variance, making it hard to compare the effect of the same variable from different studies.     
        - partial &eta;$$^2$$ : When this option is selected, the Partial eta-squared is calculated as estimate for the effect size. This method is considered to solve the problem of overestimation of the population variance, making it less difficult to compare the effect of the same variable from different studies. 
        - &omega;$$^2$$ : When this option is selected, the Omega squared is calculated as estimate for the effect size. This is considered a good estimate when the sample size is small. 
    - Vovk-Selke maximum p-ratio: The bound 1/(-e p log(p)) is derived from the shape of the p-value distribution. Under the null hypothesis (H $$_0$$ ) it is uniform (0,1), and under the alternative (H1) it is decreasing in p, e.g., a beta (α, 1) distribution, where 0 < α < 1. The Vovk-Sellke MPR is obtained by choosing the shape α of the distribution under H1 such that the obtained p-value is maximally diagnostic. The value is then the ratio of the densities at point p under H0 and H1. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H0.

#### Simple Main Effects: 
When your results contain a significant interaction, the main effects can be misleading. To solve this, you can calculate the simple main effects. The simple main effects represent the effect of one independent variable for each level of the other independent variable. 
- Factors: This box contains all the independent variables included in the analysis. 
- Simple effect factor: In this box you place the independent variable you would like to obtain the effect of. 
- Moderator factor 1: In this box you place the independent variable that will represent the different levels. 
- Moderator factor 2: In this box you place another independent variable, if included in the analysis, that will add other levels to distinguish the effects from. 

#### Nonparametrics: 
- Kruskal-Wallis test: The Kruskal-Wallis test is a non-parametric ANOVA and can be used to compare two or more groups. This test is rank-based one-way ANOVA. You can perform the Kruskal-Wallis test when one of the following assumptions is not met: normality of the dependent variable, no outliers, homogeneity of the variance between the groups. To perform the test, move the independent variables from the left column to the right column. 

### Output 
---
#### ANCOVA
ANCOVA - dependent variable: 
- Cases: This column contains the independent variables, their interaction, the covariates, and the residual. 
- Sum of Squares: The summed squared group-mean differences.  
- df: Degrees of freedom of the model.  
- Mean Square: The estimate of population variance (the sum of squares divided by df's). 
- F: The value for the F-statistic.  
- p: The p-value.  
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.   
- &eta;$$^2$$ : Estimate effect size eta-squared.      
- &eta;$$^2_p$$ : Estimate effect size partial eta-squared.  
- &omega;$$^2$$ : Estimate effect size omega-squared. 

#### Assumptions Checks: 
Test for Equality of Variances (Levene's) 
- F: F-statistic of Levene's test.  
- df1: Degrees of freedom calculated by k-1, where k represents the amount of groups in the analysis. 
- df2: Degrees of freedom calculated by N-k-1, where N represents the total sample size, and k amount of groups in the analysis. 
- p: The p-value. If the p-value is significant, this means that the group variances of the dependent variable are not equal. In this case, the assumption of homogeneity is not met. 
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.  

##### Q-Q Plot: 
With Q-Q plot we inspect the normality of the residuals. The Theoretical Quantiles are presented on the x-axis and Standardized Residuals on y-axis. If the dots are located on the diagonal line, this indicates normality. 

#### Contrasts: 
Deviation/Simple/Difference/Helmert/Repeated/Polynomial Contrast: 
- Comparison: The levels of the independent variable that are compared. 
- Estimate: The estimated mean difference between the compared levels. 
- SE: The standard error of the estimated. 
- df: The degrees of freedom of the model. 
- t: The value for the t-statistic. 
- p: The p-value. 
- % CI for Mean Difference: % confidence interval of the mean difference. This is 95% by default.   
    - Lower: This is the lower bound of the confidence interval. 
    - Upper: This is the upper bound of the confidence interval. 

#### Post Hoc Tests: 
Post Hoc Comparisons (Standard)- independent variable:  
 - The first two columns represent the levels/groups of the independent variable that are compared with each other. 
- Mean Difference: The mean difference between the levels. 
- % CI for Mean Difference: The confidence interval of the mean difference between the compared levels. By default this is set to 95%. 
    - Lower: The lower bound of the confidence interval. 
    - Upper: The upper bound of the confidence interval. 
- SE: The standard error of the mean difference. 
- t: The value for the t-statistic. 
- Cohen's D: The effect size Cohen's d. Cohen's d does not correct for multiple comparisons. 
- $$p_{tukey}$$: Tukey's corrected p-value for multiple comparisons. 
- $$p_{scheffe}$$: Scheffe's corrected p-value for multiple comparisons. 
- $$p_{bonf}$$: Bonferroni's corrected p-value for multiple comparisons.  
- $$p_{holm}$$: Holm's corrected p-value for multiple comparisons. 

Games-Howell Post Hoc Comparisons - independent variable:  
- The first two columns represent the levels/groups of the independent variable that are compared with each other. 
- Mean Difference: The mean difference between the levels. 
- % CI for Mean Difference: The confidence interval of the mean difference between the compared levels. By default this is set to 95%. 
    - Lower: The lower bound of the confidence interval. 
    - Upper: The upper bound of the confidence interval. 
- SE: The standard error of the mean difference. 
- t: The value for the t-statistic. 
- Cohen's D: The effect size Cohen's d. Cohen's d does not correct for multiple comparisons. 
- $$p_{tukey}$$: Tukey's corrected p-value for multiple comparisons. 
    
Dunnett Post Hoc Comparisons 
- The first two columns represent the levels/groups of the independent variable that are compared with each other. 
- Mean Difference: The mean difference between the levels. 
- % CI for Mean Difference: The confidence interval of the mean difference between the compared levels. By default this is set to 95%. 
    - Lower: The lower bound of the confidence interval. 
    - Upper: The upper bound of the confidence interval. 
- SE: The standard error of the mean difference. 
- t: The value for the t-statistic. 
- $$p_{dunnett}$$: Dunnett's p-value. 

Dunn's Post Hoc Comparisons 
- The first two columns represent the levels/groups of the independent variable that are compared with each other. 
- z: The value for the z-statistic. 
- $$W_i$$: The mean ranking of the first level/group of the comparison. 
- $$W_j$$: The mean ranking of the second level/group of the comparison.  
- p: The p-value.  
- $$p_{bonf}$$: Bonferroni's corrected p-value for multiple comparisons.  
- $$p_{holm}$$: Holm's corrected p-value for multiple comparisons. 

#### Marginal Means: 
Marginal Means - Independent Variable: 
- The first column contains the levels of the independent variable. 
- Marginal Mean: The marginal mean for each level of the independent variable. This mean is adjusted for all the other variables in the model. 
- SE: The standard error of the marginal mean. 
- Lower CI: The lower bound of the confidence interval. 
- Upper CI: The upper bound of the confidence interval. 
- t: The value for the t-statistic. 
- p: The p-value. 

Simple Main Effects - Independent Variable: 
- The first column contains the levels of the other independent variable included in the analysis (if present).  
- Sum of Squares: The summed squared group-mean differences. 
- df: The degrees of freedom. 
- Mean Square: The estimate of population variance (the sum of squares divided by df's)
- F: The value for the F-statistic. 
- p: The p-value. 

Kruskall-Wallis Test: 
- Factor: This column contains the independent variable included in the analysis. 
- Statistic: The value for the test statistic. 
- df: The degrees of freedom. 
- p: The p-value. 

#### Descriptives: 
Descriptives - dependent variable:
- Independent variables: The levels of the independent variable(s) included in the analysis. If more than 1, the descriptives will be displayed for each combination of levels from the independent variables. 
- Mean: The mean per level, or if more than 1 independent variable the mean per combination of levels. 
- SD: The standard deviation.  
- N: The sample size. 

##### Descriptives Plot: 
Independent variable on x-axis and dependent variable on y-axis. If other independent variables included, you can either have different lines representing different values of the other independent variable in the same plot, or you can have different plots representing different values of the other independent variable. 

### References 
--- 
-	Field, A. (2009). Discovering Statistics using SPSS (3rd ed.). Sage Publishing.
-	Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. Sage Publishing.
-	Goss-Sampson, M. A. (2018). Statistical Analysis in JASP: A Guide for Students. Version 2, October 2018. 
-	Langsrud, Ø. (2003). ANOVA for unbalanced data: Use Type II instead of Type III sums of squares. Statistics and Computing, 13(2), 163-167.
-	Moore, D.S., McCabe, G.P., & Craig, B.A. (2012). Introduction to the practice of statistics (7th ed.). New York, NY: W.H. Freeman and Company.
-	Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p values for testing precise null hypotheses. The American Statistician, 55(1), 62-71.
-	Whitlock, M.C. & Schluter, D. (2015). The analysis of biological data (2nd ed.). Greenwood Village, Colorado: Roberts and Company Publishers.

### Example 
--- 
For an example go to `file`-->`data library`-->`ANOVA`-->`Viagra`. 
