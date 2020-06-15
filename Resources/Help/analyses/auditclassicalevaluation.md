Evaluation
==========================

The evaluation analysis allows the auditor to evaluate an audit sample and infer a statistical conclusion about the total misstatement in the population. 

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

#### Annotation method
Before the start of the evaluation stage the auditor has annotated their selected transactions. This can be done in two ways: evaluation using audit values, or evaluation by determining whether the transactions are correct or incorrect.

- Audit values: When selected, you will have to annotate the selection with the observations' true values. When correct, fill in the exact same value as is stated in the book value of the transaction. This approach is recommended when the transactions have a monetary value.
- Correct / Incorrect: When selected, you will have to annotate the selection with an indicator for whether the observations are correct (0) or incorrect (1). This approach is recommended when your transactions do not have a monetary value.
    - Use summary statistics: When selected, the auditor can use only the sample size and the found errors to make an inference.

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

#### Explanatory text
Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure.

#### Estimation method
The estimation method determines how the results from your sample are extrapolated to the population.

- Binomial: Uses the binomial likelhood to evaluate the sample.
- Poisson: Uses the Poisson likelhood to evaluate the sample.
- Hypergeometric: Uses the hypergeometric likelhood to evaluate the sample.
- Stringer: The Stringer bound to evaluate the sample (Stringer, 1963).
    - LTA adjustment: LTA adjustment for the stringer bound to incorporate understatements (Leslie, Teitlebaum, & Anderson, 1979).
- Direct: This method uses only the audit values to estimate the misstatement (Touw and Hoogduin, 2011).
- Difference: This method uses the difference between the book values and the audit values to estimate the misstatement (Touw and Hoogduin, 2011).
- Ratio: This method uses the ratio of correctness between the book values and the audit values to estimate the misstatement (Touw and Hoogduin, 2011).
- Regression: This method uses the linear relation between the book values and the audit values to estimate the misstatement (Touw and Hoogduin, 2011).

----

Default output
-------

#### Evaluation summary
This table is the default output for the evaluation stage.

- Materiality: The population materiality.
- Sample size: The size of the selected subset.
- Errors: The number of erroneous elements in the selection.
- Total taining: The sum of the proportional errors.
- x-% Confidence bound: The estimate of the maximum misstatement in percentages.
- Maximum misstatement: The estimate of the projected maximum misstatement.

----

Advanced output (statistics)
-------

#### Most likely error (MLE)
Adds a cell to the evaluation summary table containing an estimate of the errors in the total population.

----


Advanced output (plots)
-------

#### Evaluation information
Produces a bar chart comparing the materiality, maximum misstatement and most likely error (MLE).

#### Correlation plot
Produces a scatter plot comparing book values of the selection against their audit values. Observations that are in error are colored in red.

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

Leslie, D. A., Teitlebaum, A. D., Anderson, R. J. (1979). <i>Dollar-unit Sampling: A Practical Guide for Auditors</i>. Toronto: Copp Clark Pitman.

Stringer, K.W. (1963) Practical aspects of statistical sampling in auditing. <i>Proceedings of Business and Economic Statistics Section</i>, American Statistical Association.

Touw, P., & Hoogduin, L. (2011). Statistiek voor audit en controlling.