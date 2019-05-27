MANOVA 
==========================

MANOVA allows the user to analyze the difference among three, or more, group means.

### Assumptions 
- The dependent variables are normally distributed for every group.
- The independent variables are categorical, the dependent variables are continuous. 
- The population covariance matrices of each group are equal.
- The groups are independent. 

### Input 
--- 
#### Assignment Box 
- Dependent Variable: The variable(s) of interest. Also called the outcome variable(s). 
- Fixed Factors: The variables that are manipulated/define the different groups. Also called the independent variables.   
   

#### Model
- Components and model terms 
    - Components: All the independent variables that can be included in the model. 
    - Model terms: The independent variables included in the model. By default all the main effects and interaction effects of the specified independent variables are included in the model. 

- Include intercept:  display the intercept term in the MANOVA and ANOVA tables.


#### Additional Options 
- Test: Select the statistical test to be performed for the MANOVA and how the F-ratio is approximated. 
    - Pillai: Pillai's trace.
    - Wilks: Wilks' lambda. This can be interpreted as the proportion of the variance in the outcomes that is not explained by an effect. 
    - Hotelling-Lawley: Hotelling-Lawley's trace.
    - Roy: Roy's largest root.

- Display:
  - ANOVA tables: Outputs individual ANOVA tables per dependent variable. 
  - Vovk-Sellke maximum p-ratio: The bound 1/(-e p log(p)) is derived from the shape of the p-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform (0,1), and under the alternative (H<sub>1</sub>) it is decreasing in p, e.g., a beta (α, 1) distribution, where 0 < α < 1. The Vovk-Sellke MPR is obtained by choosing the shape α of the distribution under H1 such that the obtained p-value is maximally diagnostic. The value is then the ratio of the densities at point p under H<sub>0</sub> and H<sub>1</sub>. For example, if the two-sided p-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this p-value is at most 2.46 times more likely to occur under H1 than under H<sub>0</sub>. More information can be found in this [blogpost](https://jasp-stats.org/2017/06/12/mysterious-vs-mpr/). 

### Output 
---

#### MANOVA 
MANOVA - test: 
- Cases: This column contains the independent variables, their interaction, and the residual. 
- df: degrees of freedom of the design.
- Approx. F: Approximation of the F-ratio. The different tests can provide different approximations.
- Test statistic: The test statistic, dependent on which tests are selected.
- Num df: Degrees of freedom used in determining the p-values of the F statistics.
- Den df: Degrees of freedom used in determining the p-values of the F statistics.
- p: The p-value of the approximated F-ratio.  
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.   


#### ANOVA 
ANOVA - dependent variable: 
- Cases: This column contains the independent variables, their interaction, and the residual. 
- Sum of Squares: The summed squared group-mean differences.  
- df: Degrees of freedom of the model.  
- Mean Square: The estimate of population variance (the sum of squares divided by df's). 
- F: The value of the F-statistic.  
- p: The p-value.  
- VS-MPR: Vovk-Sellke Maximum *p*-ratio.   


### References 
--- 
-	Field, A. (2009). *Discovering Statistics using SPSS (3rd ed.)*. Sage Publishing.
-	Field, A., Miles, J., & Field, Z. (2012). *Discovering statistics using R*. Sage Publishing.

### R Packages
---
- stats


### Example 
--- 
TBA