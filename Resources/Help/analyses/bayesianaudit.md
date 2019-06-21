Bayesian Audit Workflow
==========================

The Bayesian audit workflow allows you to make a estimate of the population misstatement in an audit population using Bayesian statistics.

----

Workflow
-----------
The Bayesian audit workflow consists of four separate stages, each with their own purpose for the analysis:
- Planning: Compute the sample size that is required for your desired population statement
- Selection: Select the required observations from your population
- Execution: Annotate your data set with your assessment of the fairness of the selected observations
- Evaluation: Make a population statement based on your annotated selection

----

Default options
-------
### Population materiality:
- Absolute: Enter your population materiality as a monetary value
- Relative: Enter your population materiality as a percentage relative to the total value

### How would you like to evaluate your variables?
- Audit values: When selected, you will have to annotate the selection with the observations' true values. When correct, fill in the exact same value as is stated in the book value of the transaction.
- Correct / Incorrect: When selected, you will have to annotate the selection with an indicator for whether the observations are correct (0) or incorrect (1)

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

### Selection type:
- Monetary unit sampling: Performs selection on the level of individual sampling units
- Record sampling: Performs selection on the level of individual records

### Selection method:
- Random sampling: Performs random selection.
- Cell sampling: Performs interval selection with randomness. Any observation that is larger than twice the interval will be selected multiple times.
- Systematic sampling: Performs interval selection. Any observation that is larger than the interval will be selected multiple times.

### Seed:
- Random number generator seed to make results reproducible

### Estimation method:
- Cox and Snell: The Cox and Snell bound (Cox & Snell, 1979)

----

Default Output
-------

### Planning summary
- Materiality: The population materiality
- Inherent risk: Risk assessment for the inherent risk
- Control risk: Risk assessment for the control risk
- Expected errors: The number of expected errors in the selection
- Required sample size: The sample size that is required for your population statement

### Selection summary:
- Sample size: The size of the selected subset 
- % of total observations: The relative size of the subset
- % of total value: The relative value of the subset
- Interval: The size of the interval used in the selection method

### Evaluation summary:
- Materiality: The population materiality
- Sample size: The size of the selected subset
- Errors: The number of erroneous elements in the selection
- Total taining: The sum of the proportional errors
- x-% Credible bound: The estimate of the maximum misstatement in percentages
- Maximum misstatement: The estimate of the projected maximum misstatement

----

Tables and plots
-------

### Book value descriptives
- Produces a table containing several statistics about the book values including the population size, total value, mean, standard deviation and quartiles.

### Book value distribution
- Produces a histogram of the distribution of book values in the population. Important statistics like the mean, standard deviation, and quartiles are indicated with colors.

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

### Display selected observations
- Produces a table containing the selected observations along with any additional observations inserted in the corresponding field

### Selection descriptives
- Produces a table containing descriptive information about numerical variables in the selection

### Most likely error (MLE)
- Adds a cell to the evaluation summary table containing an estimate of the errors in the total population.

### Bayes factor
- Computes the Bayes factor, which quantifies the relative gain in evidence for the hypothesis that the population error is lower than materiality. 

### Evaluation information
- Produces a bar chart comparing the materiality, maximum misstatement and most likely error (MLE).

### Prior and posterior
- Produces a figure showing the prior distribution in its relation to the posterior distribution.

### Correlation plot
- Produces a scatter plot comparing book values of the selection against their audit values. Observations that are in error are colored in red.

----

References
-------

Cox, D. R., & Snell, E. J. (1979). On sampling and the estimation of rare errors. <i>Biometrika</i>, 66(1), 125-132

Dyer, D., & Pierce, R. L. (1993). On the choice of the prior distribution in hypergeometric sampling. <i>Communications in Statistics-Theory and Methods</i>, 22(8), 2125-2146

Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (2007). <i>Handboek Auditing Rijksoverheid 2007</i>, established by the Interdepartementaal Overlegorgaan Departementale Accountantsdiensten (IODAD) on March 28, 2006, and May 29, 2007

Swart de J, Wille J, Majoor B (2013). Het 'Push Left'-Principe als Motor van Data Analytics in de Accountantscontrole [The 'Push-Left'-Principle as a Driver of Data Analytics in Financial Audit]. <i>Maandblad voor Accountancy en Bedrijfseconomie</i>, 87, 425-432