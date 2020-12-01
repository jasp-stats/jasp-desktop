Planning of an Audit Sample
==========================

The task of an auditor is to make a judgment regarding the fairness of the presented transactions in a population, and give on opinion on whether the population as a whole contains errors that are material (lower than the set materiality). Sometimes an auditor needs to calculate the required sample size beforehand, without having access to the raw population data. In this case, the auditor can use the *planning* analysis together with the population's summary statistics (total size and value) to calculate how many samples need to be evaluated in order to meet a certain confidence in their judgement. The frequentist *planning* analysis may use the risk assessments from the *audit risk model* to adjust the required risks of finding material errors.

*Note:* When you have access to the raw population data you may want to use the *audit workflow*, an analysis that guides you through the audit process.

----

#### The Audit Risk Model

When the auditor has information that indicates a low-risk profile on the population, they can use this information to reduce their required sample size via the Audit Risk Model (ARM) provided that there are no errors in the population. According to the ARM, the audit risk (AR) is a function of the inherent risk (IR), the internal control risk (CR), and the detection risk (DR). 

*AR = IR x CR x DR*

The auditor assesses inherent risk and internal control risk generally on a 3-point scale to determine the appropriate detection risk. Using the ARM and zero errors the sample size depends on the risk factor *R*, which is a function of the detection risk. 

*R = -ln(DR)*

The following table presents values of *R* as a function of the detection risk, provided that there are zero errors (Touw and Hoogduin 2012).

| Detection risk (%) | 1 | 4 | 5 | 10 | 14 |
| :---: | :---: | :---: | :---: | :---: | :---: |
| R | 4.6 | 3.2 | 3 | 2.3 | 2 |

The risk factor *R* can be adjusted using the assessments of the inherent risk and the internal control risk. By default, the standard method of setting the probabilities of IR and CR is by following the table below for a detection risk of 5%:

|  | High | Medium | Low | 
| :---: | :---: | :---: |
| R | 3 | 2 | 1 |

These values of *R* are used to select default percentages for IR and CR. JASP for Audit handles the following default values for IR and CR:

- High: 100%
- Medium: 60%
- Low: 36%

You can manually adjust the value of IR and CR by selecting the Custom option under the corresponding risk assessment.

----

Standard Options
-------

#### Sampling objectives
In order to start the analysis you must specify the objectives that have to be achieved with the sampling procedure. The sampling objectives influence the course of the procedure. They can currently be one of two objectives:

**Test against a performance materiality:** Also called the upper error limit, the tolerable deviation rate, or the tolerable misstatement, the performance materiality is the amount established by the auditor below the normal materiality of the financial reports to decrease the probability that the aggregate of uncorrected and undetectable misstatements exceeds the materiality of financial reports as a whole. In the statistical analysis, the performance materiality represents the upper bound of tolerable misstatement in the population to be tested. By testing against a performance materiality, you are able to plan a sample in order to collect evidence for or against the statement that the population as a whole does not contain misstatements that are considered material (i.e., are greater than the upper bound of tolerable misstatement). You should enable this objective when you want to find out whether the population contains misstatements that are greater than a certain limit (the performance materiality) using a sample of the population. A lower performance materiality will result in a higher required sample size. Vice versa, a higher performance materiality will result in a lower required sample size.

*Base effects on options:* Enables the options inherent risk, control risk, and the hypergeometric likelihood for planning. Enables the compare required sample sizes plot. 

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

A. Risk Assessments
-------

#### Inherent risk and control risk
The assessments of the inherent risk and control risk (Audit Risk Model) can be provided here to reduce the required evidence from the test. They are mapped to probabilities according to standards, but can also be mapped according to custom preferences. When both risk assessments are set to High (100%) the audit risk model is not used to adjust the detection risk.

**Inherent risk:** The inherent risk is defined as the risk of material misstatement posed by an error or omission in a financial statement due to a factor other than a failure of internal control.

**Control risk:** The control risk is defined as the risk of a material misstatement in the financial statements arising due to absence or failure in the operation of relevant controls of the auditee.

#### Expected errors in sample
The expected errors are the tolerable errors that can be found in the sample while still achieving the specified sampling objectives. A sample size is calculated so that, when the number of expected errors is found in the sample, the desired confidence is retained.

*Base effects on options*: Disables the assessments of inherent and control risk. Selects high inherent risk and high control risk.

**Absolute:** Enter your expected errors as a monetary value (e.g., $1.000 in a total balance of $1.000.000).

*Base effects on options*: None.

**Relative:** Enter your expected errors as a percentage relative to the total size of the selection.

*Base effects on options*: None.

----

B. Advanced Options
-------

#### Probability distribution
The statistical probability distribution that is used for calculating the required sample size.

**Binomial:** The binomial distribution assumes an infinite population size and is therefore generally used when the population size is large. It is a probability distribution that models the rate of misstatement (*\u03B8*) as a function of the observed number of errors (*k*) and the number of correct transactions (*n - k*). Because the binomial distribution strictly does not accommodate partial errors, it is generally used when you are not planning a monetary unit sample. 

*Base effects on options*: None.

**Poisson:** The Poisson distribution assumes an infinite population size and is therefore generally used when the population size is large. It is a probability distribution that models the rate of misstatement (*\u03B8*) as a function of the observed sample size (*n*) and the sum of the proportional errors found (*t*). Because the Poisson distribution accommodates partial errors it is generally used when you are planning a monetary unit sample.

*Base effects on options*: None.

**Hypergeometric:** The hypergeometric distribution assumes a finite population size and is therefore generally used when the population size is small. It is a probability distribution that models the number of errors (*K*) in the population as a function of the population size (*N*), the number of observed found errors (*k*) and the number of correct transactions (*n*).

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

C. Plots
-------

#### Implied sampling distribution
Produces a plot that displays the probability distribution implied by the input options and the calculated sample size. 

#### Sample size comparison
Produces a plot that compares the sample size 1) across probability distributions, and 2) across the number of expected errors in the sample.

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certified Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.3.1.