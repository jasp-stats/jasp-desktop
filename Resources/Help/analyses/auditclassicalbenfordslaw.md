Benford's Law
==========================

Benford's law states that the distribution of leading digits in a population 
naturally follows a certain distribution. In auditing, assessing whether the 
distribution of leading digits in the population conforms to Benford's law may 
provide additional evidence that the records in the population might need 
further investigation.

*Note:* Non-conformity to Benford's law does not necessarily indicate fraud. A 
Benford's law analysis should therefore only be used to acquire evidence for 
further investigation of the population. 

----

Default options
-------

### Variable
- The variable that contains the leading digits that should be tested against 
Benford's law.

----

Advanced options
-------

### Confidence
- The confidence level of the analysis. The confidence level equals the audit 
risk of the audit.

### Explanatory text:
- Enables explanatory text throughout the workflow to help you interpret the 
statistical results and procedure.

### Report badges:
- When enabled, shows badges below the conclusion to faciliate easy 
interpretation of the report.

----

Default Output
-------

### Goodness-of-fit table
- Statistic: Notation for the test statistics of the chi-square test.
- Value: The value of the chi-square test statistic.
- df: Degrees of freedom associated with the chi-square test.
- *p* value: The *p* value associated with the chi-square test.

----

Tables and plots
-------

### Descriptive statistics
- Produces a table containing, for every leading digit, the count, observed 
percentage in the population, and expected percentage under Benford's law.

### Compare vs. Benford's Law
- Produces a plot that shows the observed distribution of leading digits in the 
population, compared with the expected distribution under Benford's law. 

----

R Packages
-------
- base R

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied 
Public Accountants.
