Benford's Law
==========================

Benford's law states that the distribution of leading digits in a population 
naturally follows a certain distribution. In auditing, assessing whether a 
distribution of digits in the population conforms to Benford's law may 
provide additional evidence that the transactions in the population might need 
further investigation.

*Note:* Non-conformity to Benford's law does not necessarily indicate fraud. A 
Benford's law analysis should therefore only be used to acquire insight into 
whether a population might need further investigation. 

----

Default input options
-------

#### Variable
Here you can provide the variable that contains the digits that should be tested against Benford's law or the uniform distribution.

#### Check digits
This option allows you to specify which digits you want to test against Benford's law or the uniform distribution. Possible options are:
- First (default): Checks only the first digit of the transactions against the specified distribution.
- First and second: Checks the first and second digit of the transactions against the specified distribution.
- Last: Checks only the last digit of the transactions against the specified distribution.

#### Compare to
This option allows you to specify which distribution you want to test the digits against. Possible options are:
- Benford's law (default): Test the digits against Benford's law.
- Uniform distribution: Test the digits against the uniform distribution.

----

Advanced input options
-------

#### Confidence
The confidence is related to the audit risk. The audit risk determines the risk that the auditor is willing to take to give an incorrect based on this analysis. More specifically, the audit risk is the inverse of the confidence of the analysis (audit risk = 1 - confidence).

#### Explanatory text
When checked, enables explanatory text in the analysis to help you interpret the procedure and the statistical results.

----

Default output
-------

#### Goodness-of-fit table
This table is the default output for the Benford's Law analysis.

- Statistic: Notation for the test statistics of the chi-square test.
- Value: The value of the chi-square test statistic.
- df: Degrees of freedom associated with the chi-square test.
- *p* value: The *p* value associated with the chi-square test.
- N: The total number of digit (combinations) that are tested against the distribution.

----

Advanced output (tables)
-------

#### Descriptive statistics
Produces a table containing, for every digit, the count, observed percentage in the population, and expected percentage under Benford's law or the uniform distribution.

#### Compare vs. Benford's Law
Produces a plot that shows the observed distribution of digits in the population, compared to the expected distribution under the Benford's law or the uniform distribution. 

----

R Packages
-------

- base R

----

References
-------
