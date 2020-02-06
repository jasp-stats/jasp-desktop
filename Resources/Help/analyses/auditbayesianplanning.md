Bayesian Planning
==========================

An auditor's job is to make a judgement regarding the fairness of the presented
transactions in a population, and judge if the population contains errors that 
are material (lower than materiality). Sometimes, an auditor needs to calculate 
the required sample size beforehand without having access to the raw population 
data. In this case, the auditor can use the *Bayesian planning* analysis 
together with the population's summary statistics (total size and value) to 
calculate how many samples need to be evaluated in order to meet a certain 
confidence in their judgement. The *Bayesian planning* analysis may use the 
risk assessments from the *audit risk model* to incorporate this prior 
information into a prior probability distribution, which is updated using 
information from the data to form a posterior probability distribution. 
Inferences about the population error are made using the posterior distribution.

*Note:* When you have access to the raw population data you may want to use the 
*Bayesian audit workflow*, an analysis that guides you through the audit 
process.

----

Default options
-------
### Population materiality:
- Absolute: Enter your population materiality as a monetary value.
- Relative: Enter your population materiality as a percentage relative to the 
total value.

### Population
- Size: The total number of observations in the total population.
- Value: The total value of the population in monetary units.

### Audit Risk
- Confidence: The confidence level of the analysis. The confidence level equals 
the audit risk of the audit.

----

Advanced options
-------
### Inherent risk and control risk:
- High: 100%
- Medium: 60%
- Low: 50%
- Custom

When both risk assessments are set to High (100%) the audit risk model is not 
used to adjust the detection risk.

### Expected errors:
- Absolute: Enter your expected errors as a monetary value (e.g., $1.000 in a 
total balance of $1.000.000).
- Relative: Enter your expected errors as a percentage relative to the total 
size of the selection.

### Explanatory text:
- Enables explanatory text throughout the workflow to help you interpret the 
statistical results and procedure.

### Planning distribution:
- Beta: The beta distribution for infinite population sampling 
(de Swart, Wille & Majoor, 2013).
- Gamma: The gamma distribution for infinite population sampling
(Stewart, Strijbosch, Moors & van Batenburg, 2007).
- Beta-binomial: The beta-binomial distribution for finite population sampling 
(Dyer & Pierce, 1993).

----

Default Output
-------

### Planning summary
- Materiality: The population materiality.
- Inherent risk: Risk assessment for the inherent risk.
- Control risk: Risk assessment for the control risk.
- Expected errors: The number of expected errors in the selection.
- Required sample size: The sample size that is required for your population 
statement.

----

Tables and plots
-------

### Expected Bayes factor
- Shows the expected gain of evidence when following the formulated planning.

### Implicit sample
- Produces a table that displays the implicit sample on which the prior 
distribution is based.

### Prior and posterior descriptives
- Produces a table that displays the functional form, model probabilities, odds 
and credible bounds of the prior and expected posterior distribution. The table 
also displays the shift in model probabilities and odds between the prior and 
the expected posterior distribution. This shift is due to the expected 
evidence provided by the sample if the audit goes as planned.

### Decision analysis
- Produces a plot that compares all planning distributions and their 
corresponding sample sizes.

### Implied prior from risk assessments
- Produces a plot that shows the prior that is defined by the inherent risk, 
control risk, and the expected errors.

### Additional info
- Adds a red point representing the materiality and a gray point representing
the expected errors on the prior distribution. In addition, shades the area 
under the prior distribution that represents *x*% of the prior probability, 
where *x* equals the detection risk. If the expected posterior 
distribution is shown, adds the *x*% expected posterior credible interval
above the posterior distribution.

### Expected posterior
- Adds the expected posterior to the prior distribution plot. The expected 
posterior has its upper credible bound just below the materiality.

----

R Packages
-------
- jfa

----

References
-------

Dyer, D., and Pierce, R. L. (1993). On the choice of the prior distribution in 
hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 
22(8), 2125-2146.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). 
<i>Handboek Auditing Rijksoverheid 2007</i>, established by the 
Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) 
on March 28, 2006, and May 29, 2007.

Stewart, T., Strijbosch, L. W. G., Moors, H., and van Batenburg, P. (2007). 
A simple approximation to the convolution of gamma distributions.

Swart de J., Wille J., and Majoor B. (2013). Het 'Push Left'-Principe als Motor 
van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a 
Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en 
Bedrijfseconomie</i>, 87, 425-432.
