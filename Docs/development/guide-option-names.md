# Option names style guide: Towards consistency in the syntax mode

This guide is aimed to help us in using consistent option names across JASP. 

In the past, option names were used only internally and never shown to the user - and so while we generally followed some general principles, complete consistency was never enforced. However, now we are heading towards syntax mode in JASP, which will result in exposing these option names to the user as arguments to the analysis function in R. Hence, we not only need to stick to good habits in naming the options, we also need to use consistent naming conventions for inputs that are used across JASP in different analyses.

This guide first goes through the basic principles of how to write option names. Then, it lists common options and how to name them.

## Basic principles

### Use camelCase

That is, the name is written in lowercase letters except of the first letter in each word which is in UPPERCASE to distinguish between different words (except for the first word that is always lowercase). No underscores are allowed. 

#### Examples

- `modelTerms` (not `ModelTerms` or `model_terms`)
- `qqPlot` (not `QQplot` or `QQPlot`)
- `ppPlot` (not `Ppplot` or `PPPlot`)

### Correspondence between option name and label

It should be possible from the option name (which is visible to the R users) to deduce the option label (which is visible to the JASP users), and vice versa. This also includes avoiding shortcuts or abbreviations except for standardized option names (see below).

#### Examples

- if an option is labeled as `label: qsTr("Variables")`, the option name should be `name: "variables"`, and not `name: "dependent"` or `name: "variable"`.
- `kruskalWallisTerms` (not `kwTerms`)

### Abbreviations are a single word

Abbreviations should not be used in general, except for common option names (see below). For these cases an abbreviation is treated as a single word - which is important to keep in mind with respect to the camelCase.

#### Examples

- `mcmcMethod`: `mcmc` is an abbreviation of markov chain monte carlo, and it is treated as a word. Do not do `MCMCMethod`
- `marginalMeansCi`: `Ci` is an abbreviation of confidence interval, and so is treated as a word. Do not do `marginalMeansCI`
- `contrastsSeMethod`: `Se` is an abbreviation of a standard error, and so is treated as a word. Do not do `contrastsSEMethod`


### Correspondence between options

If several options functionally belong together (e.g., relate to the same output), if they are grouped together in a `Section{}`, or if one option is a "child" of another option, it is often beneficial to use common naming so that it is clear that the options belong together.

#### Examples

##### One option is a child of another option

```qml
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

```r
analysis(errorBars = TRUE, errorBarsType = "ci", errorBarsCiLevel = 0.95, ...)
```

##### Options are grouped in a `Section{}`

```qml
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

### Reserved option names

`data` and `formula` are reserved option names: **Do not use them for naming options in the Qml file.**


### Long option names are not evil

Combining the basic principles above may seem to be silly and annoying because they can lead to long option names - and a lot of typing. This is true, but given that most of the time, people use autocomplete features of text editors, it is a small price to pay for enhanced clarity - the best state of code documentation is when the user does not need additional documentation to understand what the code does. In the other extreme, the users would need to bury themselves in the documentation to understand what the arguments mean - which takes much more time than simply typing up a couple of extra letters.

## List of common option names

- `ci`: confidence (or credible) interval checkbox
- `ciLevel`: confidence (or credible) level of the confidence (credible) interval
- `se`: standard error
- `sd`: standard deviation
- `predictionInterval`: prediction interval
- `predictionIntervalLevel`: confidence level of a prediction interval
- `dependent`: for specifying the dependent variable
- `fixedFactors`: for specifying Fixed Factors variable list
- `randomFactors`: for specifying Random Factors variable list
- `covariates`: for specifying covariates
- `weights`: variable specifying weights
- `terms`: for specifying terms (allowing interactions). This can be combined with what the terms relate to, e.g., `modelTerms`, `marginalMeansTerms`, `plotTerms`, etc.
- `naAction`: name for an option that specifies the action to take with missing values. The option values can be for example, `pairwise` for pairwise deletion, `listwise` for listwise deletion, `perAnalysis` for deleting per analysis, `perDependent` for deleting per dependent variable, so that the analysis can be called as `analysis(..., naAction = "pairwise")`, for example.
- `alternative`: name for the option specifying the alternative hypothesis. This is to ensure consistency with base R. Typical values of this option would be `greater`, `less`, `two.sided` for hypotheses such as "> Test Value" (consistency with base R functions). For hypotheses such as "Group one > Group two" use `greater`, for "Group one < Group two" use `less` and for "Group one != Group two" use `two.sided`. This needs to be explained further in the documentation (`info` fields).
- `testValue`: name for the option specifying the test value
- `vovkSellke`: name for the Vovk-Sellke maximum p-ratio checkbox
- `bootstrap`: checkbox for enabling bootstrapping
- `bootstrapSamples`: number of bootstrap samples
- `bootstrapType`: type of bootstrap
- `setSeed`: checkbox for setting seed for reproducibility
- `seed`: specifying the seed for reproducibility
- `xAxis`, `yAxis`: name of the variable on the x- and y-axes, respectivelly
- `xAxisLabel`, `yAxisLabel`: name of the text field specifying the axis labels
- `rSquared`: name for R^2
- `effectSize`: effect size
- `pValue`: p-value
- `max`: maximum
- `min`: minimum

### Specific to Bayesian analyses

- `sampling`: radiobutton for selecting type of MCMC sampling (e.g., `auto`, `manual`, `bas`, `mcmc`, etc.)
- `samples`: number of MCMC iterations for Bayesian analyses
- `burnin`: number of burnin samples
- `thinning`: thinning factor
- `chains`: number of chains
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

## Points of contention

### Rephrain from using verbs for options

Option names should preferably be nouns. 

#### Examples

- `histrogramPlot`, not `plotHistogram`
- `errorBars`, not `displayErrorBars`

- `descriptiveTableTransposed`, not `transposeDescriptiveTable`??
- `residualsExport`, not `exportResiduals`??


### Consistency with underlying analysis packages

In some cases, an option name/value is selected so that it is directly compatible with option names/values of the underlying package. For example, consider the following excerpt from a SEM analysis:

```qml
DropDown
{
	name:	"emulation"
	label:	qsTr("Emulation")
	values: [
		{ value: "lavaan",	label: qsTr("None") 	},
		{ value: "Mplus",	label: qsTr("Mplus") 	},
		{ value: "EQS",		label: qsTr("EQS") 		}
	] 
}
```

Here we see a schism between the value of the option (`lavaan`) and the label of the option ("None"). This is because the value of the option `emulation` is directly feeded into the lavaan functions under argument `mimic`. The programmer now need to make a conscious decision: Do we need and want to keep consistency with the underlying analysis package? If the answer is yes, then it is adviseable to make the consistency as tight as possible. This means that slight deviations from the our style-guide is possible if this helps keeping consistent with the underlying package. In this case, we could change the component accordingly

```qml
DropDown
{
	name:	"mimic"
	label:	qsTr("Mimic")
	info:	qsTr("Obtain results based on emulating popular SEM software. The option corresponds to the lavaan's `mimic` arument.")
	values: [
		{ value: "lavaan",	label: qsTr("Lavaan") 	},
		{ value: "Mplus",	label: qsTr("Mplus") 	},
		{ value: "EQS",		label: qsTr("EQS") 		}
	] 
}
```

Notice that we changed the option name to `mimic` so that the argument is called the same as in the `lavaan` package that runs the analysis. Further, we changed the label of value `lavaan` from "None" to "Lavaan" to still keep consistency between what is shown to the user in JASP, and what is shown to the user in R. Notice that we slightly deviate from our camelCasing rule for values `Mplus` and `EQS` for consistency with `lavaan`.

Such small deviations are permissible if they improve understanding of what out analysis does. We advise thinking critically about where to push this.The primary goal of the options are to make them as clear as possible to JASP and R users, and R packages not necessarily design their arguments to be easily translatable into a good GUI programme. If that is the case, consistency and clarity of the JASP programme and the JASP code should always take precedence above consistency with the underlying R package that runs the analysis.