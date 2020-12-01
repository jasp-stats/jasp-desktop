Selection of an Audit Sample
==========================

The selection analysis allows the auditor to select a number of required transactions from a population using a combination of sampling techniques (record sampling versus monetary unit sampling) and sampling methods (random sampling, cell sampling, fixed interval sampling).

----

Standard Options
-------

#### Variables

**Record ID's:** A unique non-missing identifyer for every transaction in the population. The row number of the transactions may suffice.

*Base effects on options*: Enables the option add selection counter to data.

**Ist position:** The variable that contains the ist values of the transactions in the population.

*Base effects on options*: Enables and selects the option monetary unit sampling.

**Ranking variable:** When provided, the population is first ranked in ascending order with respect to the values of this variable.

*Base effects on options*: None.

**Additional variables:** Any other variables that you want to include in the sample.

*Base effects on options*: None.

#### Number of sampling units to select
The required number of sampling units that should be selected from the population. Be aware that the sampling units are determined by the corresponding option in Section A (Selection Methodology). By default, when no Ist position is provided the sampling units are records. When an Ist position is provided, the sampling units switch to monetary units automatically.

*Base effects on options*: None.

#### Add selection counter to data
When checked, adds the result from the selection analysis in a new column to the data. The new column reflects how many times each transaction is included in the sample. This option is only available when record ID's are provided, which is also the requirement for selecting a sample.

*Base effects on options*: None.

----

A. Selection Methodology
-------

#### Randomly organize transactions before selection
Sometimes you want to perform the selection with a randomized population. When this option is enabled the population is randomly reordered before drawing the requested number of sampling units.

*Base effects on options*: None.

#### Sampling units
In statistical sampling, each sampling unit receives a probability to be included in the selection. The sampling units determine which units (individual monetary units vs individual transactions) receive a probability to be included. The two possible sampling units are:

**Monetary unit sampling:** Assigns inclusion probabilities on the level of individual sampling units. This method is preferred when you are investigating transactions, but are primarily interested in detecting overstatements.

*Base effects on options*: None.

**Record sampling:** Assigns inclusion probabilities on the level of individual transactions. This method is required when you do not have monetary units in your population, but is preferred when you are looking to detect understatements.

*Base effects on options*: None.

#### Selection method
The selection method determines how sampling units are selected from the population. Different selection methods have different properties and might result in a different sample. The three selection types are:

**Random sampling:** Performs random selection in which each sampling unit receives an equal probability.

*Base effects on options*: Changes the starting point / seed option to seed.

**Cell sampling:** Performs interval selection with an element of randomness. Any observation that is larger than twice the interval will be selected multiple times.

*Base effects on options*: Changes the starting point / seed option to seed.

**Fixed interval sampling:** Performs interval selection while selecting the first observation of each interval. Any observation that is larger than the determined interval will be selected multiple times.

*Base effects on options*: Changes the starting point / seed option to starting point.

#### Starting point / Seed
This option represents the starting point for the fixed interval selection, determining which sampling unit is selected from each nterval. However, this option changes to a random number generator when the random sampling or cell sampling method is selected. The random number generator seed is there to make results reproducible, since which samples are drawn from the population in random sampling and cell sampling is subject to randomness.

----

B. Advanced Options
-------

#### Currency
Adjust the currency that is displayed in the output tables and plots depending on your preferences.

*Base effects on options*: None.

----

C. Tables
-------

#### Display selected transactions
Produces a table containing the selected transactions along with any additional observations inserted in the additional variables field.

#### Selection descriptives
Produces a table containing descriptive information about numerical variables in the selection. Possible statistics to request are the mean (enabled by default), the median (enabled by default), the starndard deviation (enabled by default), the variance, the minimum, the maximum, and the range

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certified Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.3.1.