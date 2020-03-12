Audit Workflow
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). When the auditor has access to the raw population data, they can use the *audit workflow* to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgment. She can then sample these transactions from the population, inspect these observations, and produce a statement about the total error in the population. The workflow guides the auditor through the audit process, making the correct choices of calculations along the way. The frequentist *audit workflow* may use the risk assessments from the *audit risk model* to adjust the required risk of finding material errors.

----

Workflow
-----------

The audit workflow consists of four separate stages, each with their own purpose for the analysis:
- Planning: Calculate the required sample size for your desired materiality and confidence.
- Selection: Sample the required observations from your population.
- Execution: Annotate your selection with your assessment of the fairness of the selected observations.
- Evaluation: Make a population statement based on your annotated selection.

----

Default input options
-------

#### Population materiality
The population materiality is the maximum tolerable error in the total population. This can be either an absolute value, or a value that quantifies the materiality as a percentage relative to the total value of the population.

- Absolute: Enter your population materiality as a monetary value.
- Relative: Enter your population materiality as a percentage relative to the total value.

#### Audit Risk
The audit risk determines the risk that the auditor is willing to take to give an incorrect judgment with regards to the fairness of the transactions in the population. The audit risk is the inverse of the confidence of the analysis (audit risk = 1 - confidence).

- Confidence: The confidence level for your required statistical statement.

#### How would you like to evaluate your variables?
In the execution stage the auditor evaluates their selected transactions. This can be done in two ways: evaluation using audit values, or evaluation by determining whether the transactions are correct or incorrect.

- Audit values: When selected, you will have to annotate the selection with the observations' true values. When correct, fill in the exact same value as is stated in the book value of the transaction. This approach is recommended when the transactions have a monetary value.
- Correct / Incorrect: When selected, you will have to annotate the selection with an indicator for whether the observations are correct (0) or incorrect (1). This approach is recommended when your transactions do not have a monetary value.

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

#### Sampling units
In statistical sampling, each sampling unit receives a probability to be included in the selection. The sampling units determine which units (individual monetary units vs individual transactions) receive a probability.

- Monetary unit sampling: Assigns inclusion probabilities on the level of individual sampling units. This method is preferred when you are investigating overstatements only.
- Record sampling: Assigns inclusion probabilities on the level of individual transactions. This method is preferred when you are looking at overstatements and understatements, or when you do not have monetary units in your population.

#### Selection method
The selection method determines how transactions are selected from the population. Different selection methods have different properties and might result in different transations in your selection.

- Random sampling: Performs random selection in which each sampling unit receives an equal probability.
- Cell sampling: Performs interval selection with randomness. Any observation that is larger than twice the interval will be selected multiple times.
- Fixed interval sampling: Performs interval selection while selecting the first observation of each interval. Any observation that is larger than the interval will be selected multiple times.

#### Seed
Random number generator seed to make results reproducible. This influences which samples are drawn from the population in random sampling and cell sampling. In fixed interval sampling the seed is disabled because the first unit from each interval is selected.

#### Estimation method
The estimation method determines how the results from your sample are extrapolated to the population.

- Binomial: Uses the binomial likelhood to evaluate the sample.
- Poisson: Uses the Poisson likelhood to evaluate the sample.
- Hypergeometric: Uses the hypergeometric likelhood to evaluate the sample.
- Stringer: The Stringer bound to evaluate the sample (Stringer, 1963).
    - LTA adjustment: LTA adjustment for the stringer bound to incorporate understatements (Leslie, Teitlebaum, & Anderson, 1979).

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

#### Selection summary
This table is the default output for the selection stage.

- Sample size: The size of the selected subset.
- % of total observations: The relative size of the subset.
- % of total value: The relative value of the subset.
- Interval: The size of the interval used in the selection method.

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

Advanced output (tables)
-------

#### Book value descriptives
Produces a table containing several statistics about the book values including the population size, total value, mean, standard deviation and quartiles.

#### Display selected observations
Produces a table containing the selected observations along with any additional observations inserted in the corresponding field

#### Selection descriptives
Produces a table containing descriptive information about numerical variables in the selection

----

Advanced output (plots)
-------

#### Book value distribution
Produces a histogram of the distribution of book values in the population. Important statistics like the mean, standard deviation, and quartiles are indicated with colors.

#### Implied sampling distribution
Produces a plot that displays the sampling distribution implied by the planning process. 

#### Sample size comparison
Produces a plot that compares all planning distributions and their corresponding sample sizes.

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