Bayesian Meta-Analysis
===

The Bayesian meta-analysis allows the user to estimate an overall effect size estimate from multiple studies and test whether the effect size estimate deviates from zero. 

### Input
---

#### Data
- Effect Size: Effect size of the studies. 
- Effect Size Standard Error: Standard errors of the effect sizes per study. Must always be positive. A 95% CI for the effect sizes can be provided instead. 
- 95% CI Lower and Upper Bound: 95% CI lower and upper bound of the effect sizes per study. Must be separated into two columns. The standard errors of the effect sizes can be provided instead.
- Study Labels: Optional. Will add study labels to 'Effect sizes per study' table and the (cumulative) forest plot.

#### Model
- Fixed effects: performs a fixed effects Bayesian meta-analysis.
- Random effects: performs a random effects Bayesian meta-analysis.
- Model averaged: performs a model averaged Bayesian meta-analysis. Averages over the fixed and random effects models. Fixed and random effects results are also given. 
- Constrained random effects
  - All positive: performs a constrained random effects Bayesian meta-analysis, where the effect size estimates are constrained to be positive. 
  - All negative: performs a constrained random effects Bayesian meta-analysis, where the effect size estimates are constrained to be negative. 

#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will show evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub>: By selecting this option, the Bayes factor will show evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>): By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.

### Table
- Model probabilities: Displays the prior and posterior model probabilities.
- Effect sizes per study: Displays the observed and estimated effect sizes per study. For the fixed effects model, there are no estimated effect sizes per study. 

### Prior
---

#### Effect size
- Cauchy: Scale of the Cauchy prior distribution on the effect size under the alternative hypothesis; the default is 0.707.
 - Normal: Mean and standard deviation.
- Student's t: Scale, Location and degrees of freedom (df).

#### Heterogeneity
- Inverse gamma: Shape and scale. 
- Student's half t: Scale and degrees of freedom (df). Location is always to zero.


#### Truncation
- Lower bound: adds lower truncation to the prior distribution.
- Upper bound: adds upper truncation to the prior distribution.

#### Plot prior(s)
Displays the prior density function(s). This option is also available before assigning the data. 

### Plots
---
#### Forest plot
- Observed: Displays a forest plot with the observed effect sizes and the estimated overall effect size(s).
- Estimated: Displays a forest plot with the estimated effect sizes and the estimated overall effect size(s).
- Both: Displays a forest plot with both the observed and estimated effect sizes and the estimated overall effect size(s).
- Order
  - Ascending: Displays the effect sizes in the forest plot in ascending order.
  - Descending: Displays the effect sizes in the forest plot in descending order.
  - Row order: Displays the effect sizes in the forest plot in the same order as in the provided data. 
  
  
#### Cumulative forest plot
Displays the cumulative forest plot. 

#### Prior and posterior
Displays the prior and posterior distribution of the effect size/heterogeneity under the alternative hypothesis. 

- Additional info:
  - Effect size: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under the null hypothesis vs. an alternative hypothesis; adds the mean and the 95% credible interval of the posterior distribution of the effect size.
  - Heterogeneity: Adds the Bayes factor computed with the user-defined prior; adds a probability wheel depicting the odds of the data under random effects H<sub>1</sub> (& H<sub>0</sub> for model averaging) vs. fixed effects H<sub>1</sub> (& H<sub>0</sub> for model averaging); adds the mean and the 95% credible interval of the posterior distribution of the effect size.
- Add fixed and random effects posterior: Adds the posterior density functions of the fixed and random effects models.
- Shade 95% CI: Colour the 95% CI in the posterior density function.

#### Sequential plot
- Bayes factors: Displays the development of the Bayes factor as the data come in using the user-defined prior.
- Posterior model probabilities: Displays the development of posterior model probabilities as the data come in using the user-defined prior model probabilities.
  
### Advanced
---

#### Prior model probability
- Fixed effects H<sub>0</sub>: Sets the prior model probability for the null hypothesis of the fixed effects model.
- Fixed effects H<sub>1</sub>: Sets the prior model probability for the alternative hypothesis of the fixed effects model.
- Random effects H<sub>0</sub>: Sets the prior model probability for the null hypothesis of the random effects model.
- Random effects H<sub>1</sub>: Sets the prior model probability for the alternative hypothesis of the random effects model.

#### Estimation settings (MCMC)
- Iterations: Sets the number of iterations for the MCMC estimation.
- Chains: Sets the number of chains for the MCMC estimation.

#### Bayes factor computation
- Integration: Use integration to compute the Bayes factor.
- Bridge sampling: Use bridge sampling to compute the Bayes factor. 

### Output
---

### Posterior Estimates per Model
- The first column contains the model(s) included in the analysis. 
- The second column indicates the parameter.
  - $\mu$: Group-level effect size.
  - $\tau$: Group-level standard deviation.
- Mean: This column contains the estimated overall effect size/heterogeneity. 
- SD: This column containts the estimated standard deviations for the overall effect size/heterogeneity.
- 95% Credible Interval: This columns contain the 95% credible interval lower and upper bounds for the overall effect size/heterogeneity.
- BF<sub>10</sub>: This column contains the Bayes factor that quantifies evidence for:
  - Effect size: The alternative hypothesis relative to the null hypothesis/null model. 
  - Heterogeneity: The random effects H<sub>1</sub> (& H<sub>0</sub> for model averaging) relative to the fixed effects H<sub>1</sub> (& H<sub>0</sub> for model averaging).
- BF<sub>01</sub>: This column contains the Bayes factor that quantifies evidence for:
  - Effect size: The null hypothesis/null model relative to the alternative hypothesis.
  - Heterogeneity: The fixed effects H<sub>1</sub> (& H<sub>0</sub> for model averaging) relative to the random effects H<sub>1</sub> (& H<sub>0</sub> for model averaging).

### Tables
- Model probabilities
  - Prior: This column contains the (user-defined) prior model probabilites (must sum to 1, if user-defined  probabilities do not sum to 1 the probabilities are scaled so they do sum to 1: $\frac{prior\,model\,probabilities}{\sum{prior\, model\,probabilities}}$).
  - Posterior: This column contains the posterior model probabilities. 
- Effect sizes per study
  - Observed: This column contains the observed effect sizes per study.
  - Estimated: This column contains the estimated effect sizes per study.


### Plots

#### Prior
Displays the prior density function of the effect size/heterogeneity under the alternative hypothesis.

#### Forest plot
Displays the observed/estimated effect sizes per study above the estimated overall effect size, with on the y-axis the studies and on the x-axis the effect size.

#### Cumulative forest plot
Displays the development of the overall effect size estimate as a function of the number of studies. 

#### Prior and posterior
Displays the prior (dashed line) and posterior (solid line) distribution of the overall effect size/heterogeneity under the alternative hypothesis.

- Additional info:
  - Effect size: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null hypothesis vs. the alternative hypothesis; displays the mean and 95% credible interval of the posterior distribution.
  - Heterogeneity: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under random effects H<sub>1</sub> (& H<sub>0</sub> for model averaging) vs. fixed effects H<sub>1</sub> (& H<sub>0</sub> for model averaging); displays the mean and 95% credible interval of the posterior distribution.
- Add fixed and random effects posterior: Displays the posterior density functions of the fixed and random effects models under the alternative hypothesis.
- Shade 95% CI: Displays the 95% CI as a shade in the posterior density function.

#### Sequential plot
- Bayes factors: Displays the development of the Bayes factor as a function of the number of studies using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays the mean and 95% credible interval of the posterior distribution; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.
  - Effect size: Displays a probability wheel depicting the odds of the data under the null hypothesis/hypotheses vs. the alternative hypothesis/hypotheses.
  - Heterogeneity: Displays a probability wheel depicting the odds of the data under random effects H<sub>1</sub> (& H<sub>0</sub> for model averaging) vs. fixed effects H<sub>1</sub> (& H<sub>0</sub> for model averaging).
- Posterior model probabilities: Displays the development of posterior model probabilities as a function of the number of studies using the user-defined prior model probabilities.

### References
---
- Heck, D. W., Gronau, Q. F., & Wagenmakers, E.-J. (2019). metaBMA: Bayesian model averaging for random and fixed effects meta-analysis. Retrieved from https://CRAN.R-project.org/package=metaBMA
- Gronau, Q. F., Van Erp, S., Heck, D. W., Cesario, J., Jonas, K. J., & Wagenmakers, E. J. (2017). A Bayesian model-averaged meta-analysis of the power pose effect with informed and default priors: The case of felt power. *Comprehensive Results in Social Psychology, 2*(1), 123-138.
- Rouder, J. N., Haaf, J. M., Davis-Stober, C. P., & Hilgard, J. (2019). Beyond overall effects: A Bayesian approach to finding constraints in meta-analysis. *Psychological methods.*

### R-packages
---
- metaBMA
