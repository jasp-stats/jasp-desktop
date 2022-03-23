# Guide for consistent option names

This guide is aimed to help us in using consistent option names across JASP. 

In the past, option names were used only internally and never shown to the user - and so while we generally followed some general principles, complete consistency was never enforced. However, now we are heading towards syntax mode in JASP, which will result in exposing these option names to the user as arguments to the analysis function in R. Hence, we not only need to stick to good habits in naming the options, we also need to use consistent naming conventions for inputs that are used across JASP in different analyses.

This guide first goes through the basic principles of how to write option names. Then, it lists common options and how to name them.

## Basic principles

### Use camelCase

That is, the name is written in lowercase letters except of the first letter in each word which is in UPPERCASE to distinguish between different words (except for the first word that is always lowercase). No underscores are allowed. 

#### Examples

- `modelTerms` (not `ModelTerms` or `model_terms`)
- `mean` (not `Mean`)
- `qqPlot` (not `QQplot` or `QQPlot`)
- `ppPlot` (not `Ppplot` or `PPPlot`)

### Correspondence between option name and label

It should be possible from the option name to deduce the option label, and vice versa. This also includes avoiding shortcuts or acronyms except for standardized option names (see below).

#### Examples

- if an option is labeled as `label: qsTr("Variables")`, the option name should be `name: "variables"`, and not `name: "dependent"` or `name: "variable"`.
- `kruskalWallisTerms` (not `kwTerms`)


### Correspondence between options

If several options functionally belong together (e.g., relate to the same output), if they are grouped together in a `Section{}`, or if one option is a "child" of another option, it is often beneficial to use common naming so that it is clear that the options belong together.

#### Examples

##### One option is a child of another option

```
CheckBox
{
	label:		qsTr("Display error bars")
	name:		"errorBars"
	RadioButtonGroup
	{
		name: "errorBarsType"
		RadioButton
		{
			value:					"ci"
			label:					qsTr("Confidence interval"); 
			checked: 				true
			childrenOnSameRow:		true
			CIField { name: "errorBarsCiLevel" }
		}
		RadioButton
		{ 
			value: "se";	
			label: qsTr("Standard error") }
		}	
	}
}
```

Here, the `CheckBox` `errorBars` is a parent to the the radio button control and the `CIField`. To make this clear, we use the `"errorBars"` as an indication that the rest of the options are related to the checkbox. The reason for this is that when the user calls the analysis from R, the relationship between the arguments is clear from the names:

```
analysis(errorBars = TRUE, errorBarsType = "ci", errorBarsCiLevel = 0.95, ...)
```

##### Options are grouped in a `Section{}`

```
Section
{
	title: qsTr("Marginal Means")
	columns: 1
		
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "marginalMeansTermsAvailable"; source: "modelTerms" }
		AssignedVariablesList {  name: "marginalMeansTerms" }
	}

	CheckBox
	{
		name: "marginalMeansBootstrap"; label: qsTr("From")
		childrenOnSameRow: true
		IntegerField
		{
			name: "marginalMeansBootstrapSamples"
			defaultValue: 1000
			fieldWidth: 50
			min: 100
			afterLabel: qsTr("bootstraps")
		}
	}
}
```

Here, we have a section for specifying marginal means. So every option starts with `marginalMeans`. Further, the number of bootstrapping replicates is a child of the checkbox that enables bootstrapping, and so it starts with `marginalMeansBootstrap`.


### Long option names are not evil

Combining the basic principles above may seem to be silly and annoying because they can lead to long option names - and a lot of typing. This is true, but given that most of the time, people use autocomplete features of text editors, it is a small price to pay for enhanced clarity - the best state of code documentation is when the user does not need additional documentation to understand what the code does. In the other extreme, the users would need to bury themselves in the documentation to understand what the arguments mean - which takes much more time than simply typing up a couple of extra letters.

## List of common option names

- `ci`: confidence (or credible) interval checkbox
- `ciLevel`: confidence (or credible) level of the confidence (credible) interval
- `se`: standard error
- `pi`: prediction interval
- `piLevel`: confidence level of a prediction interval
- `dependent`: for specifying the dependent variable
- `fixedFactors`: for specifying Fixed Factors variable list
- `randomFactors`: for specifying Random Factors variable list
- `covariates`: for specifying covariates
- `weights`: variable specifying weights
- `terms`: for specifying terms (allowing interactions). This can be combined with what the terms relate to, e.g., `modelTerms`, `marginalMeansTerms`, `plotTerms`, etc.
- `naAction`: name for an option that specifies the action to take with missing values. The option arguments can be for example, `pairwise` for pairwise deletion, `listwise` for listwise deletion, `perAnalysis` for deleting per analysis, `perDependent` for deleting per dependent variable.
- `alternative`: name for the option specifying the alternative hypothesis. This is to ensure consistency with base R.
- `testValue`: name for the option specifying the test value
- `vovkSellke`: name for the Vovk-Sellke maximum p-ratio checkbox
- `bootstrap`: checkbox for enabling bootstrapping
- `bootstrapSamples`: number of bootstrap samples
- `bootstrapType`: type of bootstrap
- `setSeed`: checkbox for setting seed for reproducibility
- `seed`: specifying the seed for reproducibility

### Specific to Bayesian analyses

- `sampling`: radiobutton for selecting type of MCMC sampling (e.g., `auto`, `manual`, `bas`, `mcmc`, etc.)
- `samples`: number of MCMC iterations for Bayesian analyses
- `bayesFactor`: Bayes factor
- `priorAndPosterior`: checkbox for prior and posterior
- `robustnessCheck`: checkbox for robustness check
- `cauchyPrior`: Cauchy prior checkbox/radiobutton
- `cauchyPriorLocation`: Location of the cauchy prior
- `cauchyPriorScale`: Scale of the cauchy prior
- `normalPrior`: Normal prior checkbox/radiobutton
- `normalPriorMean`: Mean of the normal prior
- `normalPriorSd`: SD of the normal prior
- `tPrior`: t-distribution prior checkbox/radiobutton
- `tPriorLocation`: location of the t prior
- `tPriorScale`: scale of the t prior
- `tPriorDf`: df of the t prior