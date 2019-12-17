Bayesian Repeated Measures ANOVA
===

The Bayesian Repeated Measures ANOVA allows the user to analyze the differences between means, when observations are dependent.

### Assumptions
- The dependent variable is normally distributed for every group.
- The covariate and the experiment effect are independent.
- The assumption of sphericity is met. Sphericity entails that the variances of the differences of the repeated measures conditions all have the same variance.

### Input
---

#### Assignment Box
- Repeated Measures Factors: The within-subjects (repeated measures variable). Here the repeated measures factors of interest and the different levels that belong to the factor can be labelled.
- Repeated Measures Cells: The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s). 
- Between Subject Factors: Select when the subjects have been assigned into two or more separate groups
- Covariates: In this box the variable that is the covariate can be selected. Covariates are continuous variables that have an influence on the dependent variable but are not part of the experimental manipulation.

#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will quantify evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub> : By selecting this option, the Bayes factor will quantify evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.

#### Order
- Compare to null model: The models used for the analysis constructed under the option `Model` will be compared to the model containing the grand mean and the random factors, called the null model. This option is selected by default.
- Compare to best model: The models used for the analysis constructed under the option `Model` will be compared to the best model included in the analysis.

#### Tables
- Effects: By selecting this option, the effect of each component in the models will be calculated.
    - Across all models: When this option is selected, each model where the component is included will be used to estimate the effect of the component. When the option `Effects` is selected, this method is used by default.
    - Across matched models: When this option is selected, each model with exactly that component will be included in the analysis. Therefore, interactions with the component are excluded.     
- Estimates: By selecting this option, a table with the model averaged posterior summary will be displayed. This table includes information about the model averaged posterior mean of each level of the fixed factors and their interactions, the standard deviation, and the credible interval.
- Descriptives: When this option is selected, the mean, standard deviation, and the sample size will be displayed for each level combination of the independent variables.
  - Credible interval: By default this is set to 95%.

#### Plots
- Model averaged posteriors: By selecting this option, plots illustrating the model averaged posterior distribution of each fixed factor, and interaction will be displayed.
  - Group levels in single plot: When this option is selected, one plot for each factor will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in the same plot. 
  - Individual plot per level: When this option is selected, a plot for each level of the factors will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in a different plot.
- Q-Q plot of residuals: Checks the validity of the distributional assumption of the data set. Specifically, the plot illustrates whether the residuals are normally distributed and linear.
- Posterior R<sup>2</sup>: By selecting this option, a plot of the posterior distribution of the R<sup>2</sup> (i.e., explained variance) will be displayed.

### Model
- Components: All the independent variables that can be included in the model.
- Model terms: The independent variables included in the model. By default all the fixed factors are located in this box. 
- Add to null model: The independent variables included in the model can be selected to add to the null model. 

### Single Model Inference
Here, a single model can be specified to obtain information about the posterior of this specific model, including a table with the posterior summary and plots of the marginal posterior.
- Tables:
  - Estimate: A table with the posterior summary for the single model, specified in the assignment box, will be provided. This table provides information about the single model posterior mean of each level of the fixed factors and their interaction included in the model, the standard deviation, and the credible interval. This is different from the `estimate` option in Output, since the  `estimates` option provides the posterior summary averaged over all the models included in the analysis while this option gives the posterior summary for only the single specified model.
  
- Plots:
  - Marginal posteriors:  By selecting this option, plots illustrating the posterior distribution of each fixed factor, and interaction included in the single model will be generated.
    - Group levels in single plot: When this option is selected, one plot for each factor will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in the same plot.
    - Individual plot per level: When this option is selected, a plot for each level of the factors will be displayed. Therefore, the posterior distribution of each level of the factor will be shown in a different plot.
  - Q-Q plot of residuals: Checks the validity of the distributional assumption of the data set. Specifically, the plot illustrates whether the residuals are normally distributed and linear.
  - Posterior R<sup>2</sup>: By selecting this option, a plot of the posterior distribution of the R<sup>2</sup> (i.e., explained variance) for the specified model will be shown.
- Assignment Box: Here, the single model is specified.
  - Components: This box contains all the factors included in the model.
  - Specific model terms: Place the factors that should be included in the model in this box.

### Post Hoc Tests
To perform a post hoc test, drag the factor name to perform the post hoc test on to the right column. Then it is possible to select:
- Correction:
    - Null control: When selecting this option, the prior odds will be corrected for multiple testing. This option is selected by default. At the moment, no output will be generated for the post hoc test when this option is not selected.

### Descriptives Plots
To create a descriptive plot, place the independent variable on the horizontal axis. If there is more than one independent variable, the variables can be displayed in one plot by putting the other variable in the box `Separate lines`, or the variables can be displayed in separate plots by dragging the other variable in the box `Separate plots`.
- Factors: The independent variables included in the analysis.
- Horizontal axis: Place here the independent variable that should be displayed on the horizontal axis of the plot.
- Separate lines: By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
- Separate plots: By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
- Label y-axis: The label of the y-axis can be changed manually.
- Display:
    - Credible interval: When this option is selected, the plot will contain credible intervals. By default this is set to 95%. This can be changed into the desired percentage.

### Additional Options
- Prior: Here it is possible to set the prior distributions for the fixed and random effect sizes.
    - r scale fixed effects: The shape parameter of the prior distribution for the fixed effects. This is set to 0.5 by default, but this can be changed into the desired value. 
    - r scale random effects: The shape parameter of the prior distribution for the random effects. This is set to 1 by default, but this can be changed into the desired value.
- Numerical accuracy: The number of steps to approximate the integral for the Bayes factor.
  - Auto: If this option is selected, 10000 steps will be used. This option is selected by default.
  - Manual: If this option is selected, the number of steps can be manually specified. The number of steps is set to 10000 by default.
- Posterior Samples: It is possible to set the he number of Markov Chain Monte Carlo samples, used to calculate the posterior and error %. 
    - Auto: If this option is selected, 10000 samples will be used. This option is selected by default.
    - Manual: If this option is selected, the number of samples can be specified manually. When selecting this option a sample size of 1000 is used by default.

### Output
---

Model Comparison - Dependent Variable:
- Models: The first column contains all the models included in the analysis.
    - Null model: This model contains the grand mean and the random factors.
    - Independent Variable model: This model adds the effect of the independent variable.
- P(M): This column contains the prior belief of the plausibility of the model, called the prior model probability.
- P(M|data): This column contains the updated probability of the model given the data. This is called the posterior model probability.
- BF<sub>M</sub> : This column contains the posterior model odds. This is the change from the prior odds to the posterior odds for the model.
- BF<sub>10</sub> : This column contains the Bayes factor that quantifies evidence for the alternative hypothesis relative to the null hypothesis/null model. However, when the option `Compare to best model` is selected, the column will contain the Bayes factor that quantifies evidence for this model relative to the best model.
- BF<sub>01</sub> : This column contains the Bayes factor that quantifies evidence for the null hypothesis/null model relative to the alternative hypothesis. However, when the option `Compare to best model` is selected, the column will contain the Bayes factor that quantifies evidence for the best model relative to this model.
- error % : The error of the Gaussian quadrature integration routine used for the computation of the Bayes factor.

Analysis of Effects - Dependent Variable:
- Effects: This column contains the components included in the models. So these are the repeated measures factors and their interaction for example.
- P(incl): This column contains the prior inclusion probability. This is the summed prior probability, calculated by adding the prior probability of each model that includes the component.
- P(incl|data): This column contains the posterior inclusion probability. This is the summed posterior probability, calculated by adding the posterior probability of each model that includes the component.
- BF<sub>inclusion</sub> : This column contains the change from prior inclusion odds to posterior inclusion odds for each component averaged by all the models that includes the component.

Model Averaged Posterior Summary
- Variable: This column contains all the fixed factors, their interactions, and covariates included in the models. The first row contains information about the intercept.
- Level: Each level of the factor and combination of levels of the interactions that are included in the model.
- Mean: The model averaged mean. For the factors, this is the deviation from the intercept for each level of the factor. The level means for a factor sum to zero. 
- SD: The standard deviation of the model averaged mean.
- % Credible interval: The credible interval of the mean. By default, this is set to 95%.
  - Lower: The lower bound of the credible interval of the mean.
  - Upper: The upper bound of the credible interval of the mean.

#### Model Averaged Posterior Distributions
For each factor, interaction, and covariate, the model averaged posterior distributions per level are displayed, with on the x-axis the factor and on the y-axis the density. The posterior distribution for each level can either be displayed in the same plot, or by different plots for each level.

Model Averaged Q-Q plot:
- With Q-Q plot the normality of the residuals can be inspected visually. The theoretical quantiles are presented on the x-axis and standardized residuals on y-axis. The more dots are on the diagonal line, the more the residuals are normally distributed.

Model Averaged Posterior R<sup>2</sup>:
- The model averaged density of the R<sup>2</sup> (i.e., explained variance), with the R<sup>2</sup> on the x-axis and the density on the y-axis.

#### Post Hoc Tests:
Post Hoc Comparisons - Repeated Measures Factor:
    - The first columns contain the levels of the repeated measures factor that are compared with each other.
    - Prior Odds: This column contains the prior odds. The prior odds are corrected for multiple testing.
    - Posterior Odds: This column contains the posterior odds. The posterior odds are the prior odds multiplied by the Bayes factor.
    - BF<sub>10, U</sub> : This column contains the Bayes factor quantifying evidence for the alternative hypothesis relative to the null hypothesis/null model. The Bayes factor is uncorrected for multiple testing.
    - BF<sub>01, U</sub> : This column contains the Bayes factor quantifying evidence for the null hypothesis/null model relative to the alternative hypothesis. The Bayes factor is uncorrected for multiple testing. 
    - error % : The error of the Gaussian quadrature integration routine used for the computation of the Bayes factor.

#### Single Model Inference
Single Model Posterior Summary:
  - Variable: This column contains all the factors, interactions, and covariates included in the models. The first row contains information about the intercept.
  - Level: Each level of the factors and combination of levels of the interactions that are included in the single model.
  - Mean: The single model mean. For the factors, this is the deviation from the intercept for each level of the factor. The level means for a factor sum to zero. 
  - SD: The standard deviation of the single model mean.
  - % Credible Interval: The credible interval of the mean. By default, this is set to 95%.
    - Lower: The lower bound of the credible interval of the mean.
    - Upper: The upper bound of the credible interval of the mean. 

Posterior Distributions:
- For each factor, interaction, and covariate, the single model posterior distributions per level are displayed, with on the x-axis the factor and on the y-axis the density. The posterior distribution for each level can either be displayed in the same plot, or by different plots for each level.

Q-Q plot: 
- With Q-Q plot the normality of the residuals can be inspected visually. The theoretical quantiles are presented on the x-axis and standardized residuals on y-axis. The more dots are on the diagonal line, the more the data are normally distributed.

Posterior R<sup>2</sup>:
- The single model density of the R<sup>2</sup> (i.e., explained variance), with the R<sup>2</sup> on the x-axis and the density on the y-axis.

#### Descriptives
Descriptives - dependent variable:
  - Repeated Measures Factor: The levels of the repeated measures factor(s) included in the analysis. If more than 1, the descriptives will be displayed for each combination of levels of the repeated measures factors.
  - Mean: The mean per level or, if more than 1 repeated measures factors, the mean per combination of levels.
  - SD: The standard deviation.
  - N: The sample size.
  - % Credible interval: The credible interval of the mean. By default, this is set to 95%.
    - Lower: The lower bound of the credible interval of the mean.
    - Upper: The upper bound of the credible interval of the mean.

#### Descriptives Plots
- Descriptives plot: Repeated measures factor on the x-axis and dependent variable on the y-axis. If other repeated measures factors are included, either different lines representing different values of the other repeated measures factor are displayed in the same plot, or different plots representing different values of the other repeated measures factor are displayed.

### References
---
- Rouder, J. N., Engelhardt C. R., McCabe S., & Morey R. D. (2016). Model comparison in ANOVA. *Psychonomic Bulletin and Review, 23*, 1779-1786.
- Rouder, J.N., Morey R.D., Speckman P.L., & Province J M. (2012). Default Bayes factors for ANOVA designs. *Journal of Mathematical Psychology, 56*, 356-374.
- Rouder, J. N., Morey, R. D., Verhagen, A. J., Swagman, A. R., & Wagenmakers, E.-J. (2017). Bayesian analysis of factorial designs. *Psychological Methods, 22*, 304-321.
- Van den Bergh, D., Van Doorn, J., Marsman, M., Draws, T., Van Kesteren, E.J., ... & Wagenmakers, E.-J. (2019) A Tutorial on Conducting and Interpreting a Bayesian ANOVA in JASP. Manuscript submitted for publication.
- Wagenmakers, E. J., Love, J., Marsman, M., Jamil, T., Ly, A., Verhagen, J., ... & Meerhoff, F. (2018). Bayesian inference for psychology. Part II: Example applications with JASP. *Psychonomic bulletin & review, 25*(1), 58-76.
- Wetzels, R., Grasman, R. P. P. P., & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for ANOVA designs. *The American Statistician, 66*, 104-111.

### R Packages
---
- BayesFactor
- colorspace
- ggplot2
- KernSmooth
- matrixStats
- plyr
- stats
- stringi
- stringr
- utils
