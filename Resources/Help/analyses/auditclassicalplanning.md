Planning
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). Sometimes an auditor needs to calculate the required sample size beforehand, without having access to the raw population data. In this case, the auditor can use the *planning* analysis together with the population's summary statistics (total size and value) to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgement. The frequentist *planning* analysis may use the risk assessments from the *audit risk model* to adjust the required risks of finding material errors.

*Note:* When you have access to the raw population data you may want to use the *audit workflow*, an analysis that guides you through the audit process.

----

Default input options
-------

#### Population materiality
The population materiality is the maximum tolerable error in the total population. This can be either an absolute value, or a value that quantifies the materiality as a percentage relative to the total value of the population.

- Absolute: Enter your population materiality as a monetary value.
- Relative: Enter your population materiality as a percentage relative to the total value.

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

#### Inherent risk and control risk
The assessments of the inherent risk and control risk (Audit Risk Model) can be provided here to reduce the required evidence from the test. They are mapped to probabilities according to standards, but can also be mapped according to custom preferences.

- High: 100%
- Medium: 60%
- Low: 50%
- Custom

When both risk assessments are set to High (100%) the audit risk model is not used to adjust the detection risk.

#### Expected errors
The expected errors are the tolerable errors that can be found in the sample. A sample size is calculated so that, when the number of expected errors is found in the sample, the desired confidence is retained.

- Absolute: Enter your expected errors as a monetary value (e.g., $1.000 in a total balance of $1.000.000).
- Relative: Enter your expected errors as a percentage relative to the total size of the selection.

#### Explanatory text
Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure.

#### Planning distribution
The statistical distribution that is used for calculating the required sample size.

- Binomial: The infinite population binomial distribution for complete taints.
- Poisson: The poisson distribution for broken taints (AICPA, 2017).
- Hypergeometric: The finite population hypergeometric distribution (only correct/incorrect evaluation).

----

Default output
-------

#### Planning summary
This table is the default output for the planning stage.

- Materiality: The maximum tolerable error in the population.
- Inherent risk: Risk assessment for the inherent risk.
- Control risk: Risk assessment for the control risk.
- Expected errors: The number of expected errors in the selection.
- Required sample size: The sample size that is required for your population statement.

----

Advanced output (plots)
-------

#### Implied sampling distribution
Produces a plot that displays the sampling distribution implied by the planning process. 

#### Sample size comparison
Produces a plot that compares all planning distributions and their corresponding sample sizes.

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.