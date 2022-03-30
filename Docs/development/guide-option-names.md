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

- if an option is labeled as `label: qsTr("Variables")`, the option name should be `name: "variables"`, and not `name: "dependent"`.
- `kruskalWallisTerms` (not `kwTerms`)

### Abbreviations are a single word

Abbreviations should not be used in general, except for common option names (see below). For these cases an abbreviation is treated as a single word - which is important to keep in mind with respect to the camelCase.

#### Examples

- `mcmcMethod`: `mcmc` is an abbreviation of markov chain monte carlo, and it is treated as a word. Do not do `MCMCMethod`
- `marginalMeanCi`: `Ci` is an abbreviation of confidence interval, and so is treated as a word. Do not do `marginalMeanCI`
- `contrastSeMethod`: `Se` is an abbreviation of a standard error, and so is treated as a word. Do not do `contrastSEMethod`


### Correspondence between options

If several options functionally belong together (e.g., relate to the same output), if they are grouped together in a `Section{}`, or if one option is a "child" of another option, it is often beneficial to use common naming so that it is clear that the options belong together.

#### Examples

##### One option is a child of another option

```qml
CheckBox
{
	label:		qsTr("Display error bars")
	name:		"errorBar"
	RadioButtonGroup
	{
		name: "errorBarType"
		RadioButton
		{
			value:					"ci"
			label:					qsTr("Confidence interval"); 
			checked: 				true
			childrenOnSameRow:		true
			CIField { name: "errorBarCiLevel" }
		}
		RadioButton
		{ 
			value: "se";	
			label: qsTr("Standard error") }
		}	
	}
}
```

Here, the `CheckBox` of name `errorBar` is a parent to the the radio button control and the `CIField` component. To make this clear, we use the `"errorBar"` as an indication that the rest of the options are related to the checkbox. The reason for this is that when the user calls the analysis from R, the relationship between the arguments is clear from the names:

```r
analysis(errorBar = TRUE, errorBarType = "ci", errorBarCiLevel = 0.95, ...)
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
		AvailableVariablesList { name: "marginalMeanTermsAvailable"; source: "modelTerms" }
		AssignedVariablesList {  name: "marginalMeanTerms" }
	}

	CheckBox
	{
		name: "marginalMeanBootstrap"; label: qsTr("From")
		childrenOnSameRow: true
		IntegerField
		{
			name: "marginalMeanBootstrapSamples"
			defaultValue: 1000
			fieldWidth: 50
			min: 100
			afterLabel: qsTr("bootstraps")
		}
	}
}
```

Here, we have a section for specifying marginal means. So every option starts with `marginalMean`. Further, the number of bootstrapping replicates is a child of the checkbox that enables bootstrapping, and so it starts with `marginalMeanBootstrap`.

### Nested option names

In the previous section we had options that are nested in the Qml file, but they are not nested in the R syntax. However, there are cases where an option is actually nested within the R syntax as well. In this case, special care needs to be taken to adhere to the style of "option names within options".

#### Examples

##### Radio Buttons

We actually already saw this in the previous section. Radio Buttons are a special control because they result in a single argument in the syntax, but can take different character values. These character values need to adhere to the code style as well.

```qml
RabioButtonGroup
{
	name:	"alternative"
	label:	qsTr("Alternative hypothesis")
	RadioButton
	{
		name:		"twoSided"
		label:		qsTr("Group 1 ≠ Group 2")
		checked:	true
	}
	RadioButton
	{
		name:		"greater"
		label:		qsTr("Group 1 > Group 2")
	}
	RadioButton
	{
		name:		"less"
		label:		qsTr("Group 1 < Group 2")
	}
}
```

This results in an option in the R syntax defined as `analysis(..., alternative = c("twoSided", "greater", "less"))` and its use requires to specify one of the options, e.g. `analysis(..., alternative = "greater")`. Note: the use of `greater`, `twoSided`, `less` is defined in the list of common option names below.

##### TabView

There are other, more complicated components than that. For example, `TabView` enables a collection of options that can be specified multiple times. A typical example is the SEM module which enables to specify multiple models in a single analysis.

```qml
TabView
{
	id: models
	name: "models"
	maximumItems: 9
	newItemName: qsTr("Model 1")
	optionKey: "name"

	content: TextArea { name: "syntax"; width: models.width; textType: JASP.TextTypeLavaan }
}
```

note that `name: "models"` names the option that can take a list of options. Each element of that list is a list that contains `Name` and `syntax` elements, and will be used in R like this:

```r
jaspSem::sem(
	models = list(
		list(
			name   = "Model 1 name",
			syntax = "#sem syntax for model 1"
		),
		list(
			name   = "Another model",
			syntax = "#sem syntax for another model"
		)
	),
	...
)
```

Notice that the options are nested within another option both in Qml and in the R syntax. Therefore, we do not need to prepend the nested options with the name of the parent option, i.e., we do not need to name the `TextArea` as `modelSyntax` or the `optionKey` as `modelName`, as it is clear from the structure of the argument itself that `syntax` and `name` are a part of the `models` option.

### Reserved option names

`data` and `formula` are reserved option names: **Do not use them for naming options in the Qml file.**

### Singular vs. plural form

Prefer singular form for names that relate to the output, *even if the output features multiple instances of the object*. Using plural is useful for indicating to the R syntax user that the argument of the analysis function accepts *multiple specification*.  

#### Examples

- `ci` not `cis` for confidence intervals (even if the output could feature multiple intervals)
- `pValue` not `pValues` for p-values (even if the output could feature multiple p-values)
- `marginalMeanTerms`: use singular `marginalMean` as an indicator of the output (regardles that there may be more marginal means in the output), but use plural `Terms` to indicate to the user that they may specify multiple terms.
- `variable`: use singular in case that the `AssignedVariableList` in Qml specifies `singleVariable: true` to indicate to the user that only one variable is allowed. On the other hand, if it's possible to specify multiple variables, use plural `variables` (and similarly, `fixedFactors`, `randomFactors`, `covariates`, etc.)
- And exception can be when the input is a number, such as in `bootstrapSamples`. In this case, the input is still a singular object (a number), but the plural indicates that it expects just that: a number.

### Rephrain from using verbs for options

Option names should preferably be nouns and not verbs. 

#### Examples

- `histogramPlot`, not `plotHistogram`
- `errorBars`, not `displayErrorBars`
- `marginalMean` not `computeMarginalMean`

### Long option names are not evil

Combining the basic principles above may seem to be silly and annoying because they can lead to long option names - and a lot of typing. This is true, but given that most of the time people use autocomplete features in text editors, it is a small price to pay for enhanced clarity - the best state of code documentation is when the user does not need additional documentation to understand what the code does. In the other extreme, the users would need to bury themselves in the documentation to understand what the arguments mean - which takes much more time than simply typing up a couple of extra letters.

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
- `terms`: for specifying terms (allowing interactions). This can be combined with what the terms relate to, e.g., `modelTerms`, `marginalMeanTerms`, `plotTerms`, etc.
- `naAction`: name for an option that specifies the action to take with missing values. The option values can be, for example, `pairwise` for pairwise deletion, `listwise` for listwise deletion, `perAnalysis` for deleting per analysis, `perDependent` for deleting per dependent variable, so that the analysis can be called as `analysis(..., naAction = "pairwise")`, for example.
- `alternative`: name for the option specifying the alternative hypothesis. Typical values of this option would be `greater`, `less`, `twoSided` for hypotheses such as "> Test Value". For hypotheses such as "Group one > Group two" use `greater`, for "Group one < Group two" use `less` and for "Group one != Group two" use `twoSided`. 
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
