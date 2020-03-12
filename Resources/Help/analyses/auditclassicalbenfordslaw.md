Benford's Law
==========================

Benford's law states that the distribution of leading digits in a population 
naturally follows a certain distribution. In auditing, assessing whether the 
distribution of leading digits in the population conforms to Benford's law may 
provide additional evidence that the records in the population might need 
further investigation.

*Note:* Non-conformity to Benford's law does not necessarily indicate fraud. A 
Benford's law analysis should therefore only be used to acquire insight into 
whether a population might need further investigation. 

----

Default input options
-------

#### Variable
There you can provide the variable that contains the leading digits that should be tested against 
Benford's law.

#### Check numbers
This options allows you to specify whether you want to test Benford's law for the numbers 1 - 9 or the numbers 10 - 99.

----

Advanced input options
-------

#### Confidence
The audit risk determines the risk that the auditor is willing to take to give an incorrect judgment with regards to the fairness of the transactions in the population. The audit risk is the inverse of the confidence of the analysis (audit risk = 1 - confidence).

#### Explanatory text
Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure.

----

Default output
-------

#### Goodness-of-fit table
This table is the default output for the Benford's Law analysis.

- Statistic: Notation for the test statistics of the chi-square test.
- Value: The value of the chi-square test statistic.
- df: Degrees of freedom associated with the chi-square test.
- *p* value: The *p* value associated with the chi-square test.

----

Advanced output (tables)
-------

#### Descriptive statistics
Produces a table containing, for every leading digit, the count, observed 
percentage in the population, and expected percentage under Benford's law.

#### Compare vs. Benford's Law
Produces a plot that shows the observed distribution of leading digits in the 
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
