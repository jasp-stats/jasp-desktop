Summary Statistics Bayesian A/B test
====================================

The Bayesian A/B test allows one to monitor the evidence for the hypotheses that an intervention or treatment has either a positive effect, a negative effect or no effect.


### Input
---------

#### Data
The input data needs to contain the following elements:

- Number of successes in group 1 (control condition)
- Number of trials in group 1 (control condition)
- Number of successes in group 2 (experimental condition)
- Number of trials in group 2 (experimental condition)


#### Bayes Factor
- BF<sub>10</sub>: By selecting this option, the Bayes factor will show evidence for the alternative hypothesis relative to the null hypothesis. This option is selected by default.
- BF<sub>01</sub> : By selecting this option, the Bayes factor will show evidence for the null hypothesis relative to the alternative hypothesis. This is equal to 1/BF<sub>10</sub>.
- Log(BF<sub>10</sub>) : By selecting this option, the natural logarithm of BF<sub>10</sub>, BF<sub>m</sub>, BF<sub>Inclusion</sub>, BF<sub>10, U</sub> will be displayed in the output.


#### Plots
  - Prior and posterior: Displays the prior and posterior density for the quantity of interest. The following quantities can be displayed
    - Log odds Ratio
    - Odds Ratio
    - Relative Risk: the ratio of the latent "success" probabilities in the experimental and control condition
    - Absolute Risk: the difference of the "success" probability in the experimental and control condition
    - p1&p2: the marginal posteriors of the latent "success" probabilities in the experimental and control condition

    (In addition, posterior median and central credible interval are also displayed in the plot)
  - Sequential analysis: Displays the development of posterior probabilities as the data come in. The probability wheels visualize prior and posterior probabilities of the hypotheses.
  - Bayes factor robustness check: Displays the prior sensitivity analysis.
     - Bayes factor type: Specifies which Bayes factor is plotted. Options include "BF10", "BF+0" and "BF-0".
  - Prior: Plot parameter prior distributions. The available quantities are the same as the ones mentioned for the Prior and posterior plot with the additional possibility of displaying p1 and p2 separately.


#### Normal prior on Log Odds Ratio
Allows specification of mean and standard deviation for the normal prior on the test-relevant log odds ratio.


#### Descriptives
Display the descriptives table: counts and proportion of the two groups.


#### Order
Compares each model against the model selected.
  - Compare to best model.
  - Compare to null model.


### Advanced Options
--------------------

#### Prior Model probability
Specify the prior probabilities for the four hypotheses:
  - Log odds ratio = 0 (H0): specifies that the "success" probability is identical (there is no effect)
  - Log odds ratio > 0 (H+): specifies that the "success" probability in the experimental condition is higher than in the control condition
  - Log odds ratio < 0 (H+): specifies that the "success" probability in the experimental condition is lower than in the control condition
  - Log odds ratio ≠ 0 (H1): specifies that the "success" probability differs between the control and experimental condition, but does not specify which one is higher

#### Sampling
Determines the number of importance samples for obtaining log marginal likelihood for (H+) and (H-) and the number of posterior samples.

#### Repeatability
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

#### Robustness Plot, No. Steps
- mu: Specifies in how many discrete steps the mu step range is partitioned
- sigma: Specifies in how many discrete steps the sigma step range is partitioned

#### Robustness Plot, Step Range
- mu: Specifies the range of mu values to consider
- sigma: Specifies the range of sigma values to consider


### Output
----------

#### Model Comparison
  - Models: Hypotheses
  - P(M): Prior model probabilities
  - P(M | data): Posterior probabilities of the models considered.
  - BFM: Posterior model odds.
  - BF10 (or BF01): Bayes factor.

#### Descriptives
  - Groups
  - Counts: "Successes" in each group
  - Total: Sample size of the groups
  - Proportion


### References
--------------
  - Kass R. E. and Vaidyanathan S. K. (1992). *Approximate Bayes Factors and Orthogonal Parameters, with Application to Testing Equality of Two Binomial Proportions*. Journal of the Royal Statistical Society, Series B, 54, 129-144.
  - Gronau Q.F., Raj K.N. A., Wagenmakers E. J. (2019). *Informed Bayesian Inference for the A/B Test*. arXiv preprint arXiv:1905.02068.


### R packages
--------------
  - abtest
