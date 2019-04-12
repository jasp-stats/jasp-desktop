Bayesian ANOVA
==========================

The Bayesian ANOVA allows the user to analyze the difference among three, or more, group means.

### Assumptions 
- The dependent variable should be normally distributed for every group.
- The independent variables are categorical, the dependent variable should be continuous. 
- The variance of the dependent variable should be the same for every group. This is called homogeneity. 
- The groups are independent. 

### Input 
---
#### Assignment Box 
- Dependent Variable: The variable of interest. Also called the outcome variable.
- Fixed Factors: The variables that are manipulated/define the different groups. Also called the independent variables.
- Random Factors: In this box the variable that should be inlcuded in all models, including the null model, can be placed. 

#### Bayes Factor:  
- BF<sub>10</sub>: By selecting this option the Bayes factor will quantify evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default. 
- BF<sub>01</sub> : By selecting this option the Bayes factor will quantify evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>. 
- Log(BF<sub>10</sub>) : By selecting this option the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output. 

#### Output: 
- Effects: <img align="right" src="https://jasp-stats.org/wp-content/uploads/2018/01/inclusionBayesANOVA.gif" width="60%" height="60%" /> By selecting this option, the effect of each component in the models will be calculated. 
    - Across all models: When this option is selected, each model where the component is included will be used. When the option `Effects` is selected, this method is used by default. 
    - Across matched models: When this option is selected, each model with exactly that component will be included in the analysis. Therefore, interactions with the component are excluded.  
- Descriptives: When this option is selected, the mean, standard deviation, and the sample size will be presented for each level combination of the independent variables.

#### Order:  
- Compare to null model: <img align="right" src="https://jasp-stats.org/wp-content/uploads/2017/10/anovaSimpleBestCrop.gif" width="60%" height="60%" /> The models used for the analysis constructed under the option `Model` will be compared to the model containing the grand mean and the random factors, called the null model. This option is selected by default. 
- Compare to best model: The models used for the analysis constructed under the option `Model` will be compared to the best model included in the analysis. 

#### Model 
- Components: All the independent variables that can be included in the model. 
- Model terms: The independent variables included in the model. By default all the fixed factors are located in this box.  
- Add to null model: The independent variables included in the model can be selected to add to the null model.  

#### Post Hoc Tests 
If <img align="right" src="https://jasp-stats.org/wp-content/uploads/2018/01/bayesPostHocANOVA.gif" width="60%" height="60%" /> there is a significant effect of the independent variable, the analysis can be followed up by performing a post hoc test to see which specific levels of an independent variable differ from the other levels. To perform a post hoc test, drag the factor name to perform the post hoc test on to the right column. Then it is possible to select:
- Correction 
    - Null control: When selecting this option, the prior odds will be corrected for multiple testing. This option is selected by default. At the moment, no output will be generated for the post hoc test when this option is not selected. 

#### Descriptives Plots 
To create a descriptive plot, place the independent variable on the horizontal axis. If there is more than one independent variable, the variables can be displayed in one plot by putting the other variable in the box Separate lines, or the variables can be displayed in separate plots by dragging the other variable in the box Separate plots.
- Factors: The independent variables included in the analysis.
- Horizontal axis: Place here the independent variable that should be displayed on the horizontal axis of the plot.
- Separate lines: By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
- Separate plots: By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
- Display:
    - Credible interval: When this option is selected, the plot will contain credible intervals. By default this is set to 95%. This can be changed into the desired percentage. 

#### Advanced Options: 
- Prior: Here it is possible to set the prior distributions for the fixed and random effect sizes. 
    - r scale fixed effects: The shape parameter of the prior distribution for the fixed effects. This is set to 0.5 by default, but this can be changed into the desired value.  
    - r scale random effects: The shape parameter of the prior distribution for the random effects. This is set to 1 by default, but this can be changed into the desired value. 
- Samples: Here, it is possible to set the sample size used to calculate the posterior and error %.  
    - Auto: If this option is selected, a sample size of 10000 will be used. This option is selected by default. 
    - Manual: If this option is selected, the sample size can be manually specified. When selecting this option a sample size of 10000 is used by default. 

### Output 
--- 
#### Model Comparison - Dependent Variable: 
- Models: The first column contains all the models included in the analysis. 
    - Null model: This model contains the grand mean and the random factors. 
    - Independent Variable model: This model adds the effect of the independent variable. 
- P(M): This column contains the prior belief of the plausibility of the model, called the prior model probability. 
- P(M|data): This column contains the updated probability of the model given the data. This is called the posterior model probability. 
- BF<sub>M</sub> : This column contains the posterior model odds. This is the change from the prior odds to the posterior odds for the model. 
- BF<sub>10</sub> : This column contains the Bayes factor that quantifies evidence for the alternative hypothesis relative to the null hypothesis/null model. However, when the option `Compare to best model` is selected, the column will contain the Bayes factor that quantifies evidence for this model relative to the best model. 
- BF<sub>01</sub> : This column contains the Bayes factor that quantifies evidence for the null hypothesis/null model relative to the alternative hypothesis. However, when the option `Compare to best model` is selected, the column will contain the Bayes factor that quantifies evidence for the best model relative to this model.
- error % : The error of the Gaussian quadrature integration routine used for the computation of the Bayes factor.

#### Analysis of Effects - Dependent Variable: 
- Effects: This column contains the components included in the models. So these are the independent variables and their interaction for example. 
- P(incl): This column contains the prior inclusion probability. This is the summed prior probability, calculated by adding the prior probability of each model where the component is in. 
- P(incl|data): This column contains the posterior inclusion probability. This is the summed posterior probability, calculated by adding the posterior probability of each model where the component is in. 
- BF<sub>inclusion</sub> : This column contains the change from prior inclusion odds to posterior inclusion odds for each component averaged by all the models the component is in. 

#### Post Hoc Tests: 
- Post Hoc Comparisons - Independent Variable: 
    - The first columns contain the levels of the independent variable that are compared with each other. 
    - Prior Odds: This column contains the prior odds. The prior odds are corrected for multiple testing. 
    - Posterior Odds: This column contains the posterior odds. The posterior odds are the prior odds * the Bayes factor. 
    - BF<sub>10, U</sub> : This column contains the Bayes factor quantifying evidence for the alternative hypothesis relative to the null hypothesis/null model. The Bayes factor is uncorrected for multiple testing. 
    - BF<sub>01, U</sub> : This column contains the Bayes factor quantifying evidence for the null hypothesis/null model relative to the alternative hypothesis, while uncorrected for multiple testing. 
    - error % : The error of the Gaussian quadrature integration routine used for the computation of the Bayes factor. 

#### Descriptives: 
- Descriptives - dependent variable:
    - Independent variables: The levels of the independent variable(s) included in the analysis. If more than 1, the descriptives will be displayed for each combination of levels of the independent variables.
    - Mean: The mean per level or, if more than 1 independent variable, the mean per combination of levels.
    - SD: The standard deviation.
    - N: The sample size.

#### Descriptives Plot: 
Independent variable on the x-axis and dependent variable on the y-axis. If other independent variables are included, either different lines representing different values of the other independent variable are displayed in the same plot, or different plots representing different values of the other independent variable are displayed.

### References 
--- 
- Rouder, J. N., Engelhardt C. R., McCabe S., & Morey R. D. (2016). Model comparison in ANOVA. Psychonomic Bulletin and Review, 23, 1779-1786.
- Rouder, J.N., Morey R.D., Speckman P.L., & Province J M. (2012). Default Bayes factors for ANOVA designs. Journal of Mathematical Psychology, 56, 356-374.
- Rouder, J. N., Morey, R. D., Verhagen, A. J., Swagman, A. R., & Wagenmakers, E.-J. (2017). Bayesian analysis of factorial designs. Psychological Methods, 22, 304-321.
- Van den Bergh, D., Van Doorn, J., Marsman, M., Draws, T., Van Kesteren, E.J., ... & Wagenmakers, E.-J. (2019) A Tutorial on Conducting and Interpreting a Bayesian ANOVA in JASP. Manuscript submitted for publication.
- Wagenmakers, E. J., Love, J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., ... & Meerhoff, F. (2018). Bayesian inference for psychology. Part II: Example applications with JASP. Psychonomic bulletin & review, 25(1), 58-76.
- Wetzels, R., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for ANOVA designs. The American Statistician, 66, 104-111.

### Example 
--- 
For a very detailed example see Van den Bergh et al. (2019).
For an example of a Bayesian One-Way ANOVA go to `File`-->`Data Library`-->`ANOVA`-->`Pain Thresholds`. 
For an example of a Bayesian Two-Way ANOVA go to `File`-->`Data Library`-->`ANOVA`-->`Singers`. 
For more details about these examples see Wagenmakers et al. (2018). 
