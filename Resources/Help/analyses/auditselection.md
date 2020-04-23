Selection
==========================

The selection analysis allows the auditor to sample their required transactions from a population using a combination of sampling techniques (record sampling versus monetary unit sampling (MUS)) and sampling methods (random sampling, cell sampling, fixed interval sampling).

----

Default input options
-------

#### Sample size
The required sample size that should be selected from the population. 

----

Advanced input options
-------

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

----

Default output
-------

#### Selection summary
This table is the default output for the selection stage.

- Sample size: The size of the selected subset. 
- % of total observations: The relative size of the subset.
- % of total value: The relative value of the subset.
- Interval: The size of the interval used in the selection method.

----

Advanced output (tables)
-------

#### Display selected observations
Produces a table containing the selected observations along with any additional observations inserted in the corresponding field.

#### Selection descriptives
Produces a table containing descriptive information about numerical variables in the selection.

----

R Packages
-------

- jfa

----

References
-------

AICPA (2017). <i>Audit Guide: Audit Sampling</i>. American Institute of Certied Public Accountants.

Derks, K (2020). jfa: Bayesian and Classical Audit Sampling. R package version 0.1.0.