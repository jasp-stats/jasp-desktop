Mediation analysis
============

Mediation analysis with potentially multiple predictors, multiple mediators, multiple outcomes, and optional correction for multiple background confounders. Mediation analysis in JASP is based on the excellent `lavaan` software (Rosseel, 2012). More information about `lavaan` can be found here: [lavaan.org](http://lavaan.org). 


Mediation analysis in JASP allows for continuous and ordinal endogenous variables and binary and continuous exogenous variables. For binary endogenous variables, recode your variable into a dummy continous variable with 0 for the first category and 1 for the second category. The linearity assumption still holds, however, i.e., SEM does not perform logistic regression.

For more information on allowed data types, see [the lavaan website](http://lavaan.ugent.be/tutorial/cat.html).

### Details about testing indirect effects
Testing whether an indirect effect exists is best done via the "Bootstrap" option under `Options > Confidence Intervals > Methods > Bootstrap`. The confidence intervals are then computed using the bias-corrected percentile method as suggested by Biesanz, Falk, and Savalei (2010).


## Input
#### Predictors
One or multiple predictor variable(s), predicting the mediator(s) and the outcome variable(s).

#### Mediators
The variable(s) through which the indirect effect of the predictor(s) on the outcome variable(s) is hypothesized to flow. 

#### Outcome
The variable(s) predicted by the predictor(s) and the mediator(s).

#### Background confounders
Variables explaining the predictor(s), mediator(s), and outcome variable(s): the direct, indirect, and total effects are estimated conditional on these variables.

#### Standardized estimates
Check this to standardize (mean = 0, sd = 1) all variables before estimation.

#### Show lavaan syntax
Show the syntax needed to estimate this model using `lavaan` in `R` or the `SEM` module in `JASP`.

#### Show R-Squared
A table with the proportion of variance explained for each of the endogenous variables in the mediation model.

#### Parameter estimates
Under this option, you can check and uncheck different parameter estimates tables to display in the main output of the mediation analysis.

#### Confidence intervals
Here, you can select different ways of estimating the uncertainty around the parameter estimates. A note under each of the main tables will display the methods by which standard errors and confidence intervals are computed. See also the __details about testing indirect effects__ section above. 

#### Plots
This option allows users to graphically display the path model being estimated by the mediation analysis. Optionally, the parameters can be shown in this plot. If the labels overlap, the plot can be saved as an EPS and edited in any vector editing program.

#### Emulation
Emulate the output from different SEM programs. Default none.

#### Missing value handling
How missing values are handled. By default, this is set to full information maximum likelihood (FIML), which automatically computes the estimates using all the available information -- assuming missing at random (MAR) missingness patterns. A suboptimal alternative is available in listwise deletion.

#### Estimator
Different choices for estimators can be found here. We suggest leaving this at `auto` for most -- if not all -- purposes.

References
==========

- Jeremy C. Biesanz, Carl F. Falk & Victoria Savalei (2010) Assessing Mediational Models: Testing and Interval Estimation for Indirect Effects, Multivariate Behavioral Research, 45:4, 661-701, DOI: 10.1080/00273171.2010.498292
- Rosseel, Y. (2012). Lavaan: An R package for structural equation modeling and more. Version 0.5–12 (BETA). Journal of Statistical Software, 48(2), 1-36.
