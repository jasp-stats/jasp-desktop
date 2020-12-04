The Bayesian Audit Sampling Workflow
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). When the auditor has access to the raw population data, they can use the *Bayesian audit workflow* to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgment. They can then sample these transactions from the population, inspect these observations, and produce a statement about the totalerror in the population. The workflow guides the auditor through the audit process, making the correct choices of calculations along the way. The *Bayesian audit workflow* may use the risk assessments from the *audit risk model* to incorporate this prior information into a prior probability distribution, which is updated using information from the sample to form a posterior probability distribution. Inferences about the population error are made using the posterior distribution.

----

Workflow
-----------

The Bayesian audit workflow consists of four separate stages, each with their own purpose for the analysis:
- Planning: Calculate the required sample size for your desired materiality and confidence.
- Selection: Sample the required observations from your population.
- Execution: Annotate your selection with your assessment of the fairness of the selected observations.
- Evaluation: Make a population statement based on your annotated selection.

----

Default input options
-------

#### Sampling objectives
In order to start the analysis you must specify the objectives that have to be achieved with the sampling procedure. The sampling objectives influence the course of the procedure. They can currently be one of two objectives:
- **Test against a performance materiality:** Also called the upper error limit, the tolerable deviation rate, or the tolerable misstatement, the performance materiality is the amount established by the auditor below the normal materiality of the financial reports to decrease the probability that the aggregate of uncorrected and undetectable misstatements exceeds the materiality of financial reports as a whole. In the statistical analysis, the performance materiality represents the upper bound of tolerable misstatement in the population to be tested. By testing against a performance materiality, you are able to plan a sample in order to collect evidence for or against the statement that the population as a whole does not contain misstatements that are considered material (i.e., are greater than the upper bound of tolerable misstatement). You should enable this objective when you want to find out whether the population contains misstatements that are greater than a certain limit (the performance materiality) using a sample of the population. A lower performance materiality will result in a higher required sample size. Vice versa, a higher performance materiality will result in a lower required sample size.
- **Obtain a required minimum precision:** The precision is a measure of how much certainty there is in the estimate of the misstatement from testing a particular characteristic of a sample at a given level of sampling risk. In the statistical analysis, the precision is represented by the difference between the estimated most likely error and the upper bound on the misstatement. By enabling this sampling objective, you are be able to plan a sample so that the difference between the estimated most likely error and the upper bound on the misstatement is reduced to a minimum percentage. You should enable this objective if you are interested in making an estimate of the population misstatement with a certain accuracy. A lower minimum required precision will result in a higher required sample size. Vice versa, a higher minimum required precision will result in a lower required sample size.

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

- Beta: The beta distribution for broken taints (de Swart, Wille & Majoor, 2013).
- Gamma: The gamma distribution for broken taints.
- Beta-binomial: The finite population beta-binomial distribution for complete taints (Dyer & Pierce, 2993).

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

- Beta: Use the beta posterior distribution to evaluate the sample.
- Gamma: Use the gamma posterior distribution to evaluate the sample.
- Beta-binomial: Use the beta-binomial posterior distribution to evaluate the sample.
- Cox and Snell: The Cox and Snell posterior distribution to evaluate the sample (Cox & Snell, 1979).

#### Area under posterior
- Credible bound: Gives an (upper) estimate of the maximum error in the population.
- Credible interval: Gives an (upper and lower) estimate of maximum error in the population.

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
- x-% Credible bound: The estimate of the maximum misstatement in percentages.
- Maximum misstatement: The estimate of the projected maximum misstatement.

----

Advanced output (statistics)
----

#### Expected evidence ratio
Shows the expected posterior odds (induced by the planning) in favor of the null hypothesis of tolerable misstatement.

#### Expected Bayes factor
Shows the expected gain of evidence (induced by the planning) in favor of the null hypothesis of tolerable misstatement.

#### Most likely error (MLE)
Adds a cell to the evaluation summary table containing an estimate of the errors in the total population.

#### Evidence ratio
Shows the posterior odds (after seeing a sample) in favor of the null hypothesis of tolerable misstatement.

#### Bayes factor
Shows the gain of evidence (after seeing a sample) in favor of the null hypothesis of tolerable misstatement.

----

Advanced output (tables)
-------

#### Book value descriptives
Produces a table containing several statistics about the book values including the population size, total value, mean, standard deviation and quartiles.

#### Implicit sample
Produces a table that displays the implicit sample on which the prior distribution is based.

#### Prior and expected posterior descriptives
Produces a table in which the prior and expected posterior distribution are summarized through several statistics, such as their functional form, their prior and expected posterior probabilities and odds, and the shift between these.

#### Display selected observations
Produces a table containing the selected observations along with any additional observations inserted in the corresponding field.

#### Selection descriptives
Produces a table containing descriptive information about numerical variables in the selection.

#### Prior and posterior descriptives
Produces a table in which the prior and posterior distribution are summarized through several statistics, such as their functional form, their prior and posterior probabilities and odds, and the shift between these.

----

Advanced output (plots)
-------

#### Book value distribution
Produces a histogram of the distribution of book values in the population. Important statistics like the mean, standard deviation, and quartiles are indicated with colors.

#### Sample size comparison
Produces a plot that compares all planning distributions and their corresponding sample sizes.

#### Implied prior from risk assessments
Produces a plot that shows the prior that is defined by the inherent risk, control risk, and the expected errors.

- x-axis limit: Change the limit for the x-axis in the plot.
- Expected posterior: Adds the expected posterior to the prior graph. The expected posterior has its upper credible bound just below the materiality.
- Additional info: Adds a red dot for the materiality, and a gray dot for the expected errors to the graph.
- Shade: Select which area under the prior distribution to shade. 

#### Evaluation information
Produces a bar chart comparing the materiality, maximum misstatement and most likely error (MLE).

#### Correlation plot
Produces a scatter plot comparing book values of the selection against their audit values. Observations that are in error are colored in red.

#### Prior and posterior
Produces a plot that shows the prior distribution alongside the posterior distribution induced by the data.

- x-axis limit: Change the limit for the x-axis in the plot.
- Expected posterior: Adds the expected posterior to the prior graph. The expected posterior has its upper credible bound just below the materiality.
- Additional info: Adds a red dot for the materiality, and a gray dot for the expected errors to the graph.
- Shade: Select which area under the posterior distribution to shade. 

----

R Packages
-------

- jfa

----

References
-------

Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. <i>Biometrika</i>, 66(1), 125-132.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.