Bayesian Planning
==========================

The Bayesian planning analysis allows you to plan an audit sample without a data file using Bayesian statistics.

----

Default options
-------
### Population materiality:
- Absolute: Enter your population materiality as a monetary value
- Relative: Enter your population materiality as a percentage relative to the total value

----

Advanced options
-------
### Inherent risk and control risk:
- High: 100%
- Medium: 60%
- Low: 50%

### Expected errors:
- Absolute: Enter your expected errors as a monetary value (e.g., $1.000 in a total balance of $1.000.000)
- Relative: Enter your expected errors as a percentage relative to the total size of the selection

### Explanatory text:
- Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure

### Planning distribution:
- Beta: The beta distribution for broken taints (de Swart, Wille & Majoor, 2013)
- Beta-binomial: The finite population beta-binomial distribution for complete taints (Dyer & Pierce, 2993)

----

Default Output
-------

### Planning summary
- Materiality: The population materiality
- Inherent risk: Risk assessment for the inherent risk
- Control risk: Risk assessment for the control risk
- Expected errors: The number of expected errors in the selection
- Required sample size: The sample size that is required for your population statement

----

Tables and plots
-------

### Implied prior from risk assessments
- Produces a plot that shows the prior that is defined by the inherent risk, control risk, and the expected errors.

### Expected posterior
- Adds the expected posterior to the prior graph. The expected posterior has its upper credible bound just below the materiality.

### Expected Bayes factor
- Shows the expected gain of evidence when following the formulated planning.

### Implicit sample
- Produces a table that displays the implicit sample on which the prior distribution is based.

### Decision analysis
- Produces a plot that compares all planning distributions and their corresponding sample sizes.

----

References
-------

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432