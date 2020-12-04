Bayesian Planning of an Audit Sample
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). Sometimes an auditor needs to calculate the required sample size beforehand, without having access to the raw population data. In this case, the auditor can use the *Bayesian planning* analysis together with the population's summary statistics (total size and value) to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgement. The *Bayesian planning* analysis may use the risk assessments from the *audit risk model* to incorporate this prior information into a prior probability distribution, which is updated using information from the data to form a posterior probability distribution. Inferences about the population error are made using the posterior distribution.

*Note:* When you have access to the raw population data you may want to use the *Bayesian audit workflow*, an analysis that guides you through the audit process.

----

Standard Options
-------

#### Sampling objectives
In order to start the analysis you must specify the objectives that have to be achieved with the sampling procedure. The sampling objectives influence the course of the procedure. They can currently be one of two objectives:

**Test against a performance materiality:** Also called the upper error limit, the tolerable deviation rate, or the tolerable misstatement, the performance materiality is the amount established by the auditor below the normal materiality of the financial reports to decrease the probability that the aggregate of uncorrected and undetectable misstatements exceeds the materiality of financial reports as a whole. In the statistical analysis, the performance materiality represents the upper bound of tolerable misstatement in the population to be tested. By testing against a performance materiality, you are able to plan a sample in order to collect evidence for or against the statement that the population as a whole does not contain misstatements that are considered material (i.e., are greater than the upper bound of tolerable misstatement). You should enable this objective when you want to find out whether the population contains misstatements that are greater than a certain limit (the performance materiality) using a sample of the population. A lower performance materiality will result in a higher required sample size. Vice versa, a higher performance materiality will result in a lower required sample size.

*Base effects on options:* Enables the incorporation of information from the Audit Risk Model, equal prior probabilities, custom prior probabilities, and the beta-binomial distribution for planning. Enables the compare required sample sizes plot option. 

**Obtain a required minimum precision:** The precision is a measure of how much certainty there is in the estimate of the misstatement from testing a particular characteristic of a sample at a given level of sampling risk. In the statistical analysis, the precision is represented by the difference between the estimated most likely error and the upper bound on the misstatement. By enabling this sampling objective, you are be able to plan a sample so that the difference between the estimated most likely error and the upper bound on the misstatement is reduced to a minimum percentage. You should enable this objective if you are interested in making an estimate of the population misstatement with a certain accuracy. A lower minimum required precision will result in a higher required sample size. Vice versa, a higher minimum required precision will result in a lower required sample size.

*Base effects on options*: None.

#### Population
Here you can provide the relevant statistics about the population. Some analyses require that you fill in both of these values, but others might require only one of these statistics.

**Size:** An integer specifying the total number of transactions in the population. The population size is always required.

*Base effects on options*: None.

**Value:** The total value of the transactions in the population (in monetary units). The population value is sometimes optional.

*Base effects on options*: Enables the currency option. Selects the total value as the population size and selects the Poisson probability distribution.

#### Audit Risk
The audit risk is the risk that the auditor is willing to take to give an incorrect judgment about the population. The audit risk is the complement of the confidence of the analysis (audit risk = 1 - confidence). For example, if you want to have an audit risk of 5%, this equals 95% confidence. 

**Confidence:** A percentage indicating the complement of the required audit risk.

*Base effects on options*: None.

----

A. Prior Information
-------

#### Prior information 
You can incorporate existing information from various sources into the statistical sampling procedure. Possible options are:

**None:** This option does not incorporate any information into the statistical analysis and therefore assumes a negligible and conservative prior distribution. 

**Audit Risk Model:** This option can only be selected when testing against a performance materiality. When selected, it incorporates the information from the assessed risks of material misstatement (inherent risk and control risk) from the audit risk model into the statistical analysis. 
They are mapped to probabilities according to standards, but can also be mapped according to custom preferences.

- High: 100%
- Medium: 60%
- Low: 36%
- Custom

When both risk assessments are set to High (100%) the audit risk model is not used to adjust the detection risk.

*Base effects on options*: None.

**Equal prior probabilities:** This option can only be selected when testing against a performance materiality. When selected, it incorporates the information that tolerable misstatement is equally likely to occur a priori as intolerable misstatement. The prior distribution resulting from this method has 50\% of its probability mass below the performance materiality and 50\% of its probability mass above the performance materiality.

*Base effects on options*: None.

**Custom prior probabilities:** This option can only be selected when testing against a performance materiality. When selected, you can use your own assessment of the a priori probability that the population contains tolerable misstatement. You may also deduct this value as one minus the probability of intolerable misstatement. 

*Base effects on options*: None.

**Earlier sample:** This option incorporates the information from an earlier sample in the statistical analysis. 

*Base effects on options*: None.

**Weighted earlier sample:** This option incorporates the information from an earlier sample in the statistical analysis, weighted by a factor *f*, which determines the weight that you assign to the results of the earlier sample.

*Base effects on options*: None.

#### Expected errors in sample
The expected errors are the tolerable errors that can be found in the sample while still achieving the specified sampling objectives. A sample size is calculated so that, when the number of expected errors is found in the sample, the desired confidence is retained.

*Base effects on options*: Disables the custom prior probabilities option.

**Absolute:** Enter your expected errors as a monetary value (e.g., $1.000 in a total balance of $1.000.000).

*Base effects on options*: None.

**Relative:** Enter your expected errors as a percentage relative to the total size of the selection.

*Base effects on options*: None.

----

B. Advanced Options
-------

#### Probability distribution
The statistical probability distribution that is used for calculating the required sample size.

**Beta:** The beta distribution accompanies the binomial likelihood. The binomial likelihood assumes an infinite population size and is therefore generally used when the population size is large. It is a likelihood that models the rate of misstatement (*\u03B8*) as a function of the observed number of errors (*k*) and the number of correct transactions (*n - k*). Because the binomial distribution strictly does not accommodate partial errors, it is generally used when you are not planning a monetary unit sample. However, the beta distribution does accommodate partial errors, and may also be used for monetary unit sampling (see for example de Swart, Wille & Majoor, 2013).

*Base effects on options*: None.

**Gamma:** The gamma distribution accompanies the Poisson likelihood. The Poisson likelihood assumes an infinite population size and is therefore generally used when the population size is large. It is a likelihood that models the rate of misstatement (*\u03B8*) as a function of the observed sample size (*n*) and the sum of the proportional errors found (*t*). Because the gamma distribution accommodates partial errors it is generally used when you are planning a monetary unit sample (see for example Stewart, 2013).

*Base effects on options*: None.

**Beta-binomial:** The beta-binomial distribution accompanies the hypergeometric likelihood (Dyer & Pierce, 1993). The hypergeometric likelihood assumes a finite population size and is therefore generally used when the population size is small. It is a likelihood that models the number of errors (*K*) in the population as a function of the population size (*N*), the number of observed found errors (*k*) and the number of correct transactions (*n*).

*Base effects on options*: None.

#### Calculation settings
The calculation settings allow you to control the additional rules by which the sample size is calculated.

**Step size:** The step size alows you to limit the possible sample sizes to a multiple of its value. For example, a step size of 5 allows only sample sizes of 5, 10, 15, 20, 25, etc.

*Base effects on options*: None.

#### Currency
Adjust the currency that is displayed in the output tables and plots depending on your preferences.

*Base effects on options*: None.

#### Explanatory text
Enables explanatory text throughout the workflow to help you interpret the statistical results and procedure.

*Base effects on options*: None.

----

C. Tables and Plots
----

#### Statistics
You can request various relevant statistics for the expected sample. 

**Expected posterior odds:** Shows the expected posterior odds (induced by the prior distribution) in favor of the null hypothesis of tolerable misstatement.

**Expected Bayes factor:** Shows the expected gain of evidence (induced by the prior distribution) in favor of the null hypothesis of tolerable misstatement.

#### Tables
You can request various relevant tables for the expected sample.

**Implicit sample:** Produces a table that displays the implicit sample on which the prior distribution is based.

**Prior and expected posterior descriptives:** Produces a table in which the prior and expected posterior distribution are summarized through several statistics, such as their functional form, their prior and expected posterior probabilities and odds, and the shift between these.

#### Plots
You can request various relevant plots for the expected sample.

**Compare required sample sizes:** Produces a plot that compares the sample size 1) across probability distributions, and 2) across the number of expected errors in the sample.

**Implied prior distribution:** Produces a plot that shows the prior distribution that is constructed by the incorporated prior information.

- x-axis limit: Change the limit for the x-axis in the plot of the prior distribution.
- Expected posterior distribution: Adds the expected distribution posterior to the plot of the prior distribution. 
- Additional info: Adds a red dot for the materiality (optional), and a gray dot for the expected errors to the plot of the prior distribution.
- Shade: Select which area under the prior distribution to highlight. You can choose to shade the are below the upper bound of the prior distribution, or the areas that correspond to the hypotheses (in case of testing against a performance materiality).  

----

R Packages
-------

- jfa

----

References
-------

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.3.1.

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146.

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007.

Stewart, T. R. (2013). A Bayesian audit assurance model with application to the component materiality problem in group audits (Doctoral dissertation).

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432.