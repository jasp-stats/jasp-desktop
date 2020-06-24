Bayesian Linear Mixed Models
===

Bayesian Linear Mixed Models allow you to model a linear relationship between one or more explanatory variable(s) and a continuous dependent variable in cases where the observations are not independent, but clustered given one or several random effects grouping factors (e.g., repeated measures across participants or items, children within schools). An introduction to this model class and the concepts introduced below is provided in <a href="http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf">Singmann and Kellen (2019)</a>.

### Assumptions
- Continuous response variable.
- Linearity and additivity: The response variable is linearly related to all predictors and the effects of the predictors are additive.
- Independence of errors: The errors are uncorrelated with each other after taking the model (i.e., fixed effects and random effects structure) into account.
- Homoscedasticity: The error variance of each predictor is constant across all values of that predictor.
- Normality of errors: The errors are normally distributed with mean zero.

### Input

#### Assignment Box
  - Dependent variable: Dependent (response) variable.
  - Fixed effects variables: Variables used as the fixed effects predictors (the model terms can be specified under `Model` section). These are usually the variables of primary scientific interest.
  - Random effects grouping factors: Categorical variable(s) specifying clusters of observations (i.e., several observations per level of a random effects grouping factor). These are typically variables, such as participant or item, one wants to generalize over. Factors with very few levels (i.e., less then five or six levels) should not be used as random effects grouping factors and the number of levels of the random effects grouping factors determines the power of the test of the fixed effects (Westfall, Kenny, & Judd, 2014). The random effect structure (i.e., random intercepts, random slopes, and correlations among random effects parameters) can be specified under `Model` - `Random effects`. The default random effects structure is the automatically determined "maximal random effects structure justified by the design" (Barr, Levy, Scheepers, & Tily, 2013).


#### Run Analysis 
Press the button to run the analysis. Model relevant changes in the settings will not be applied until the button is pressed.


### Output
- Estimated grand mean and estimated differences from the grand mean for all levels of each fixed effects model term. It can be changed to estimated marginal means in the `Options` section.
  - Level: Levels of the fixed effects model term.
  - Estimate: This column contains the independent variables or their interaction.
  - SE: Standard error of the estimate.
  - 95% CI: 95% credible interval.
    - Lower: Lower bound of the credible interval.
    - Upper: Upper bound of the credible interval.
  - R-hat: Convergence diagnostic comparing within and between chains estimates of model parameters. Values larger than 1.01 indicates possible convergence problems.
  - ESS (bulk): Estimated sample size in the middle of the distribution. Small values render the parameter estimates unprecise. 
  - ESS (tail): Estimated sample size in the tails of the distribution. Small values render the bounds of credible intervals unprecise. 


### Model
- Components and model terms: 
    - Model components: All the fixed effects variables that can be included in the model. 
    - Fixed effects: The independent variables in the model. By default, all the main effects of the specified independent variables and their interactions are included in the model. To include interactions, click multiple variables (e.g., by holding the ctrl/cmd button on your keyboard while clicking) and drag those into the `Fixed effects` box.
    - Random effects: The random effects organized by random effects grouping factors. By default, all of the random effects corresponding to the fixed effects are included and JASP internally checks and removes non-estimable random effects. That is, the default corresponds to the "maximal random effects structure justified by the design" (Barr, Levy, Scheepers, & Tily, 2013). Unticking the boxes on the left of the variable names removes the random effect from corresponding random effects grouping factor.
     - Correlations: Whether the correlations between the random effects parameters within each random effects grouping factor should be estimated. 


### Options
- Warmup: Number of iterations reserved for warm-up.
- Iterations: Total number of iterations.
- Chains: Number of chains.
- Adapt delta: Average targer proposal acceptance of each step. Increasing `Adapt delta` results in better-behaved chains, but also longer fitting times.
- Maximum treedepth: The cap for number of trees evaluated during each iteration. Prevents excessively long execution times.
- Show: What should be the default output.
  - Differences from intercept: A table for each fixed effects term will be created in the default output and it will show the differences from the grand mean for each of the terms' levels (or one standard deviation distance for continuous terms). This option is selected by default.
  - Marginal means: A table for each fixed effects term will be created in the default output and it will show the estimated marginal mean for each of the terms' levels (or one standard deviation distance for continuous terms). 
- Fixed effects estimates: Shows the estimated fixed effect coefficients.
- Variance/correlation estimates: Shows the estimated residual variances and variances/correlations of random effects coefficients.
- Confidence interval: Width of the confidence interval.
- Repeatability: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. Affects only parametric bootstrap and jitter in plots.


### MCMC diagnostics
- Model terms: Fixed effects model terms whose MCMC chains can be diagnosed.
- Selected term: Fixed effects model term which chain will be diagnosed.
- Selected term (2): Fixed effects model term which chain will be diagnosed. Only available if `Plot type` is Scatterplot (selected term is plotted on the y-axis).
- Plot type: Different types of MCMC diagnostics plots. The plotted values correspond to the fixed effect terms displayed in the default output. Those are the deviations from estimated grand mean by default but can be changed to estimated marginal means in the `Options` section.
  - Traceplot: Traceplot of the individual chains.
  - Scatterplot: Scatterplot of two model terms.
  - Histogram: Histogram of the posterior samples.
  - Density: Overlying densities of samples from each chain.
  - Autocorrelations: Average autocorrelations across all chains.


### Plots
- Model factors: Categorical or ordinal fixed effects variables that can be used for visualization.
  - Horizontal axis: Variables that will be plotted on the horizontal axis.
  - Separate lines: Variables that will be plotted "inside" the plot as different traces/lines.
  - Separate plots: Variables which levels will be split across different plots.
- Random effect grouping factors: Random effect grouping factors that can be used for data aggregation of data shown in the background.
  - Background data show: The level of aggregation for the response variable. I.e., if participants are selected, the individual data points in the background are their averages across the combinations of levels of fixed effect factors selected in the `Horizontal axis`, `Separate lines`, and `Separate plots`.
 - Confidence interval method: Type of standard error on which the error bars will be based. Default is "model", which plots model-based standard errors.
 - Confidence interval: The width of the confidence interval.
 - Background geom: Geom that will be used to display the aggregated response variable.
  - Transparency: Transparency of the geom.
  - Jitter width: Width of the jitter.
  - Jitter height: Height of the jitter.
  - Geom width: Width of the geoms.
  - Dodge: Spacing between the geoms.
- Distinguish factor levels: How the factor levels should be distinguished.
  - Color
  - Shape
  - Linetype
  - Fill
- Theme: A theme of the plots.
- Legend position: Whether and where should be the legend plotted.
- Color background data: Color of the aggregated response variable.
- Relative size text: Relative size of the plotted text.
- Relative size foreground data: Relative size of the foreground data (confidence interval bars, etc...).
- Estimates table: Display numerical summary of the plotted objects.


### Estimated marginal means
- Model variables: Fixed effects variables that can be used for computing estimated marginal means.
- Selected variables: Variables for which the estimated marginal means will be computed.
- Confidence interval: Width of the confidence interval.
- SD factor covariates: What should be the "levels" of continuous variables (expressed in standard deviations) for which are the estimated marginal means computed.
- Specify contrasts: Creates a table for specifying contrasts based on the estimated marginal means.
- Contrasts: A table created by checking `Specify contrasts` checkbox. 
  - The first column contains indices of rows corresponding to the estimated marginal means output table. 
  - Columns with variable names contain the combinations of variables level for each estimated marginal mean.
  - Columns named Contrast `x` are used for specifying the contrasts. To set a contrast between two marginal means, enter -1 and 1 to the corresponding rows. Interactions can be tested by specifying differences between the changes in marginal means of one variable across levels of another variable.


### Estimated trends/conditional slopes
- Continuous variables: Continuous fixed effects variables that can be used for estimating the conditional slopes.
- Trend variable: Variables for which the estimated conditional slopes will be computed.
- Model variables: Fixed effects variables over which the conditional slopes can be computed.
- Selected variables: Variables over which the the conditional slopes will be computed.
- Confidence interval: Width of the confidence interval.
- SD factor covariates: What should be the "levels" of continuous variables (expressed in standard deviations) over which the conditional slopes are computed.
- Specify contrasts: Creates a table for specifying contrasts based on the estimated conditional slopes.
- Contrasts: A table created by checking `Specify contrasts` checkbox. 
  - The first column contains indices of rows corresponding to the estimated conditional slopes output table. 
  - Columns with variable names contain the combinations of variables level for each estimated conditional slope.
  - Columns named Contrast `x` are used for specifying the contrasts. To set a contrast between two conditional slopes, enter -1 and 1 to the corresponding rows. Interactions can be tested by specifying differences between the changes in conditional slopes of one variable across levels of another variable.


### References
---
- Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects structure for confirmatory hypothesis testing: Keep it maximal. *Journal of Memory and Language*, 68(3), 255–278. https://doi.org/10.1016/j.jml.2012.11.001
- Singmann, H., & Kellen, D. (2019). An Introduction to Mixed Models for Experimental Psychology. In D. H. Spieler & E. Schumacher (Eds.), *New Methods in Cognitive Psychology* (pp. 4–31). Psychology Press. http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf
- Westfall, J., Kenny, D. A., & Judd, C. M. (2014). Statistical power and optimal design in experiments in which samples of participants respond to samples of stimuli. *Journal of Experimental Psychology: General*, 143(5), 2020–2045. https://doi.org/10.1037/xge0000014


### R Packages
---
- stanova
- rstan
- rstanarm
- emmeans
- ggplot2
- stats