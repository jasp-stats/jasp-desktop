Bayesian Planning
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). Sometimes an auditor needs to calculate the required sample size beforehand, without having access to the raw population data. In this case, the auditor can use the *Bayesian planning* analysis together with the population's summary statistics (total size and value) to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgement. The *Bayesian planning* analysis may use the risk assessments from the *audit risk model* to incorporate this prior information into a prior probability distribution, which is updated using information from the data to form a posterior probability distribution. Inferences about the population error are made using the posterior distribution.

*Note:* When you have access to the raw population data you may want to use the *Bayesian audit workflow*, an analysis that guides you through the audit process.

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

- Beta: The beta distribution for broken taints (de Swart, Wille & Majoor, 2013).
- Gamma: The gamma distribution for broken taints.
- Beta-binomial: The finite population beta-binomial distribution for complete taints (Dyer & Pierce, 2993).

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

Advanced output (statistics)
----

#### Expected evidence ratio
Shows the expected posterior odds (induced by the planning) in favor of the null hypothesis of tolerable misstatement.

#### Expected Bayes factor
Shows the expected gain of evidence (induced by the planning) in favor of the null hypothesis of tolerable misstatement.

----

Advanced output (tables)
-------

#### Implicit sample
Produces a table that displays the implicit sample on which the prior distribution is based.

#### Prior and expected posterior descriptives
Produces a table in which the prior and expected posterior distribution are summarized through several statistics, such as their functional form, their prior and expected posterior probabilities and odds, and the shift between these.

----

Advanced output (plots)
-------

#### Sample size comparison
Produces a plot that compares all planning distributions and their corresponding sample sizes.

#### Implied prior from risk assessments
Produces a plot that shows the prior that is defined by the inherent risk, control risk, and the expected errors.

- x-axis limit: Change the limit for the x-axis in the plot.
- Expected posterior: Adds the expected posterior to the prior graph. The expected posterior has its upper credible bound just below the materiality.
- Additional info: Adds a red dot for the materiality, and a gray dot for the expected errors to the graph.
- Shade: Select which area under the prior distribution to shade. 

----

R Packages
-------

- jfa

----

References
-------

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.