Estimation
==========================

*Note*: This analysis will change significantly in the next version of JASP. It will be revamped into a full-fledged evaluation analysis, allowing for more than just estimation. 

Estimation is applied when the auditor has collected a sample of the population, and wants to estimate the total error is the population on the basis of their sample.

----

Default input options
-------

#### Population
Here you can provide the summary statistics about the population.

- Size: The total number of observations in the total population.
- Value: The total value of the population in monetary units.

#### Audit Risk
The audit risk determines the risk that the auditor is willing to take to give an incorrect judgment with regards to the fairness of the transactions in the population. The audit risk is the inverse of the confidence of the analysis (audit risk = 1 - confidence).

- Confidence: The confidence level for your required statistical statement.

----

Advanced input options
-------

#### Estimator
Choose which estimator to use for computation of the results.

- Direct estimator
- Difference estimator
- Ratio estimator
- Regression estimator (default)

#### Explanatory text
Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure.

----

Default output
-------

#### Estimation table
This table is the default output for the estimation analysis.

- Estimate: The point estimate of the total error in the population.
- Uncertainty: The uncertainty associated with the confidence interval.
- Confidence interval: The confidence interval associated with the estimate.

----

Advanced output (tables)
-------

#### Required sample size
Produces a table that shows, for a given uncertainty, the required sample size.

----

Advanced output (plots)
-------

#### Correlation plot
Produces a scatter plot comparing book values of the selection against their audit values. Observations that are in error are colored in red.

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied 
Public Accountants.
