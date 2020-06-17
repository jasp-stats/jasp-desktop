Robust Bayesian Meta-Analysis
===

The robust Bayesian meta-analysis allows the user to specify a wide range of meta-analytic models, combine their estimates using model averaging and quantify evidence for different hypotheses using Bayes factors. The analysis allows to specify various prior distributions for effect sizes and heterogeneity and incorporate models correcting for publication bias by estimating a weight function on p-values. 

### Input
---
#### Input type
- Cohen's d / t-statistics & (N / SE): Specifying input using either Cohen's d effect sizes and the sample size or standard errors, or reported t-statistics and sample sizes.
- Correlations & (N / SE): Specifying input using correlations and the sample size. Note that the effect size is internally transformed to Cohen's d scale for model estimation (see Advanced tab for additional options). The prior distributions are specified on the transformed scale, with the provided visualization transforming the priors back to the correlation scale. The output for the mean parameter is transformed back for easier interpretability, however, the heterogeneity parameter is always summarized on the transformed scale.
- Effect sizes & SE: Specifying input using any other types of effect sizes and standard errors. Note that effect sizes supplied in this way will result in approximating the corresponding test statistics distribution using a normal distribution.
- Fitted model: Specify a path to already fitted RoBMA model using R. The model must be saved as an RDS file.

#### Data
- Effect Size: Effect sizes of the studies. In case that the effect sizes are measured by Cohen's d t-statistics can be provided instead.
- t-statistic: t-statistics of the studies. Only possible if the studies effect sizes are measured by Cohen's d or correlations. Effect sizes can be provided instead. 
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive. A 95% CI for the effect sizes can be provided instead. In case that the effect sizes are measured by Cohen's d the sample sizes can be provided instead.
- 95% CI Lower and Upper Bound: 95% CI lower and upper bounds of the effect sizes per study. Must be separated into two columns. The standard errors of the effect sizes can be provided instead.
- N: Overall sample sizes of the studies. Only possible if the studies effect sizes are measured by Cohen's d or correlations. Effect Size Standard Errors can be provided instead. In the case of two-sample t-tests, sample sizes per group can be provided as well.
- N (group 1) / N (group 2): Sample sizes of each group of the studies. Only possible if the studies effect sizes are measured by Cohen's d and correspond to two-sample t-tests. Overall sample sizes or Effect Size Standard Errors can be provided instead.
- Study Labels: Optional argument that will add study labels to the output.

#### Input test type
Only for 'Cohen's d / t-statistics & (N / SE)' input type. 

#### Run Analysis
Press the button to run the analysis. Model relevant changes in the settings will not be applied until the button is pressed.


### Priors
---
The individual models that form up the robust Bayesian meta-analysis are defined by creating combinations of all specified priors for the effect size / heterogeneity / publication bias. The individual models' prior odds are obtained by multiplying the prior odds of prior distributions for each of the parameter that forms the model. Note that the prior distributions for the mean parameter are transformed into the correlation scale using a selected transformation if correlations are supplied as input.

#### Plot priors
Displays the specified prior density function(s).

#### Effect / Heterogeneity
Set a prior distribution(s) for the Effect size or heterogeneity.
- Distribution: Name and parametrization of the distribution.
  - Normal(μ,σ): Normal distribution parametrized by mean (μ) and standard deviation (σ).
  - Student's t(μ,σ,v): Generalized Student's t distribution parametrized by location (μ), scale (σ), and degrees of freedom (v).
  - Cauchy(x₀,θ): Cauchy distribution parametrized by location (μ) and scale (σ).
  - Gamma(α,β): Gamma distribution parametrized by shape (α) and rate (β).
  - Gamma(k,θ): Gamma distribution parametrized by shape (k) and scale (θ).
  - Inverse-Gamma(α,β): Inverse-Gamma distribution parametrized by shape (α) and scale (β).
  - Spike(x₀): Point density parametrized by location (x₀).
  - Uniform(a,b): Uniform distribution parametrized by lower bound (a) and upper bound (b).
- Parameters: Values for parameters of the selected distribution.
- Truncation: Lower and upper truncation of the distribution.
- Prior Odds: Prior odds of the distribution.

#### Publication bias
Set a prior distribution(s) for the parameters of the weight function modeling the publication bias. α α₁ α₂ p-values
- Distribution: Name and parametrization of the distribution.
    - Two-sided: Prior distribution for a two-sided weight function characterized by vector of cut points on p-values (p-values) and vector alpha (α). The vector alpha (α) determines an alpha parameter of Dirichlet distribution which cumulative sum is used for the weights omega. The first element of α corresponds to the weight for the highest p-value interval.
  - One-sided (mon.): Prior distribution for a monotonic one-sided weight function characterized by vector of cut points on p-values (p-values) and vector alpha (α). The vector alpha (α) determines the alpha parameter of Dirichlet distribution which cumulative sum is used for the weights omega. The first element of α corresponds to the weight for the highest p-value interval.
  - One-sided: Prior distribution for a non-monotonic one-sided weight function characterized by vector of cut points on p-values (p-values) and two vectors alpha (α₁ α₂). The vectors alpha (α₁ α₂) determine the alpha parameters of Dirichlet distribution which cumulative sum is used for the weights omega - α₁ determines the omegas for p-value intervals starting at = .50 and decreasing and α₂ determines the omegas for p-value intervals = .50 and increasing.
  - None: Prior distribution assuming complete lack of publication bias - the probability of publication is the same for all p-values.
- Parameters: Values for parameters of the selected distribution.
- Truncation: Lower and upper truncation of the distribution.
- Prior Odds: Prior odds of the distribution.

#### Set null priors
Allows to specify prior distributions for the null models.


### Inference
---
#### Conditional estimates
Displays estimates assuming that the alternative models are true.

#### Models overview
Display overview of the specified models.
- BF: Show different types of Bayes factors
  - Inclusion: Change from prior to posterior odds for each individual model.
  - vs Best: Bayes factor comparing to the best fitting model.
  - vs Previous: Bayes factor comparing to a better fitting model.
- Order: Order the overview of displayed models.
  - Priors: Based on the priors distribution specification.
  - Marginal likelihood: Based on the marginal likelihood of each model.
  - Posterior prob.: Based on the posterior probability of each model.

#### Individual models
Display a detailed overview of each specified model.
- Single model: Display the overview for only one of the specified models.

#### Bayes Factor
- BF<sub>01</sub> : Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF<sub>01</sub>: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF<sub>01</sub>) : Natural logarithm of BF<sub>01</sub>.

#### CI width
Width of the credible intervals.

#### Estimated studies' effects
Display the estimated effects of individual studies. These correspond to the 'random effect estimates' for models assuming a distribution over the heterogeneity parameter tau. They are substituted with the overall mean parameter mu for models assuming the heterogeneity parameter to be 0 when model averaging over all models.


### Plots
---
#### Pooled estimates
- Forest plot
  - Observed: Displays a forest plot with the observed effect sizes and the estimated overall effect size(s).
  - Estimated: Displays a forest plot with the estimated effect sizes and the estimated overall effect size(s).
  - Both: Displays a forest plot with both the observed and estimated effect sizes and the estimated overall effect size(s).
  - Order
    - Ascending: Displays the effect sizes in the forest plot in ascending order.
    - Descending: Displays the effect sizes in the forest plot in descending order.
    - Row order: Displays the effect sizes in the forest plot in the same order as in the provided data.
- Effect: Display a plot with the estimated pooled effect size.
- Heterogeneity: Display a plot with the estimated heterogeneity parameter tau.
- Weights: Display a plot with the estimated weights corresponding to the p-values cut-offs.
  - Weight function: Combine the weights into a weight function and display that instead.

#### Type
- Model averaged: Pooled estimates will contain model averaged estimates across all models.
- Conditional: Pooled estimates will contain model averaged estimates across models assuming that the alternative hypothesis is true models.

#### Show priors
Displays prior distribution density on top of the pooled estimates figures.

#### Individual models
Display estimates from each individual model included in the ensemble.
- Effect: Display a plot with the estimated pooled effect size for each individual model.
- Heterogeneity: Display a plot with the estimated heterogeneity parameter tau for each individual model.
- Weights: Display a plot with the estimated weights corresponding to the p-values cut-offs for each individual model.

#### Omit null models
The individual models will only display that assume non-zero Effect / Heterogeneity / Publication bias. The displayed posterior probabilities and the overall estimate corresponds to the Conditional model (assuming that the alternative hypothesis is true).

#### Order
Order the displayed individual models according to:
- Model number
- Posterior probability
- Marginal likelihood
in either Ascending or Descending manner.


### MCMC Diagnostics
---
#### Overview
Display overview of the individual model diagnostics. The table summarizes the minimal Estimated Sample Size and maximum R-hat per model. More details can be accessed by displaying individual model summary in the Inference tab.
- Include theta: Whether the random effect estimates theta should be included in the diagnostics overview.

#### Plot
Display chains summaries according to the selected type for each of the models for the selected parameters:
- Effect: The pooled effect size estimate.
- Heterogeneity: The estimated heterogeneity parameter tau.
- Weights: The estimated weights corresponding to the p-values cut-offs.
- Estimated studies' effects: The estimated effects of individual studies. These correspond to the 'random effect estimates' for models assuming a distribution over the heterogeneity parameter tau.

#### Type
Type of the chains summaries to be displayed.
- Trace: Displays the overlaying traces of each chain for the selected parameters. Different chains are visualized with a different color.
- Autocorrelation: Displays the average autocorrelations of the chains for the selected parameters.
- Posterior sample densities: Displays the overlaying densities of samples from each chain for the selected parameters. Different chains are visualized with a different color.

#### Single model
Display chains summaries for only a specific model.


### Advanced
---
#### Transform correlations
- Cohen's d: Supplied correlations will be transformed and modeled on Cohen's d scale.
- Fisher's z: Supplied correlations will be transformed and modeled on Fisher's z scale. We advise for using Cohen's d transformation since it preserves the proper distribution of test statistics important for the publication bias adjusted models.

#### Estimation settings (MCMC)
- Adaptation: Sets the number of iterations to be used for adapting the MCMC chains.
- Burnin: Sets the number of iterations to be used for burnin of the MCMC chains.
- Iterations: Sets the number of iterations to be used for sampling from the MCMC chains.
- Chains: Sets the number of chains for the MCMC estimation.
- Thin: Sets the thinning of the MCMC chains.

#### Bridge sampling iterations
A number of iterations for computing the marginal likelihood.

#### Autofit
When turned on, the MCMC estimation for each model continues until the maximum fitting time was reached or the target MCMC margin of error for parameters was obtained.
- Maximum fitting time: The maximum fitting time per model.
- Target margin of error: The target MCMC margin of error of parameter estimates for terminating the automatic estimation procedure.

#### Exclude models
Allows excluding individual models from the overall ensemble based on MCMC diagnostics.
- error %: All models whose parameters have MCMC error higher than the specified value will be excluded.
- R-hat: All models whose parameters have R-hat error higher than the specified value will be excluded.
- Estimated sample site %: All models whose parameters have smaller estimated sample sizes lower than the specified value will be excluded.
- Include theta: Whether random effect estimates theta should be included in the considered parameters.

#### Redistribute prior probability
Allows respecifying how should be the prior probabilities of individual models changed in case that some of them fail to converge.
- Conditional models: The prior model probability of a non-converged model will be transferred to a model with same the type of priors if possible. Meaning that if there are two models with non-null prior on effect size, heterogeneity, and publication bias and one of them fails to converge, the other one will gain the prior probability of the non-converged one. If there is no such model, the prior probability will be redistributed equally across all models.
- Model space: If there is a non-converged model, the prior probability will be redistributed equally across all models.

#### Control
Allows modifying the fitting and refitting behavior of the analysis when 'Run Analysis' is clicked.
- Clever refitting: The whole ensemble is refitted only if prior distributions change, otherwise, only the non-converged models are refitted.
- Always refit: Always refits the whole ensemble.
- Never refit: Never refits the ensemble or the individual models.

#### Seed
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.


### References
---


### R-packages
---
- RoBMA
