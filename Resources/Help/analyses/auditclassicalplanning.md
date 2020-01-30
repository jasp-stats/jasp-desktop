Planning
==========================

An auditor's job is to make a jugement regarding the fairness of the presented 
transactions in a population, and judge if the population contains errors that 
are material (lower than materiality). Sometimes, an auditor needs to calculate 
the required sample size beforehand without having access to the raw population 
data. In this case, the auditor can use the *planning* analysis together with 
the population's summary statistics (total size and value) to calculate how 
many samples need to be evaluated in order to meet a certain confidence in their 
judgement. The frequentist *planning* analysis may use the risk assessments from 
the *audit risk model* to adjust the required riks of finding material errors.

*Note:* When you have access to the raw population data you may want to use the 
*audit workflow*, an analysis that guides you through the audit process.

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
- Binomial: The binomial distribution for infinite population sampling.
- Poisson: The poisson distribution for infinite population sampling 
(AICPA, 2017).
- Hypergeometric: The hypergeometric distribution for finite population 
sampling.

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

### Decision analysis
- Produces a plot that compares all planning distributions and their 
corresponding sample sizes.

### Implied sampling distribution
- Produces a plot that shows the expected distribution of errors in the sample.

----

R Packages
-------
- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of 
Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). 
<i>Handboek Auditing Rijksoverheid 2007</i>, established by the 
Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) 
on March 28, 2006, and May 29, 2007.