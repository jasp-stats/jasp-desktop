# Option names style guide: Towards consistency in the syntax mode
This guide aims to help us specify consistent option names across JASP. By "options" we mean the input provided by the user in the left hand panel in JASP. This input is referred to by a "name" specified in the Qml form, which is internally referenced in R. For instance, if a drags variables into the "Fixed factors" box in JASP, the Qml file then identifies the user input by the "name: `fixedFactors`", and R is then presented with the variables that the user specified as `fixedFactors`. With these `fixedFactors` in hand, functions in R can then do the computations. Hence, this guide is concerned with how the options should be named in both the Qml file that defines the GUI and the R file that runs the analysis. Consistent option names help us (1) better organise and understand the JASP code, (2) simplify the review process, and  (3) allows for logical syntax.

In the past, option names were used only internally and never shown to the user - and so while we generally followed some general principles, complete consistency was never enforced. However, with syntax mode in JASP, the user will be exposed to the option names as arguments of an analysis function in R. Hence, we not only need to stick to good habits in naming the options, we also need to use consistent naming conventions for inputs that are used across JASP in different analyses.

This guide first goes through the basic principles of how to name the options. Then, it lists common options, which should be used across various analyses.

## Basic principles
The goal is clear human-readable code and we adopt the following principles:

1. Human readability: Long option and descriptives names are not evil.
2. Use camelCase: eachWordIsSeparatedByACapitalLetter.
3. Option names should be nouns, because they are things, whereas functions names should start with a verb, because they do things.
4. Singular: When calling objects the typical preference should go to the singular form of a noun. The plural form is allowed to indicate multiple inputs.

We first elaborate on these principles in R, and then discuss what they imply for the Qml forms. For Qml we use the same principles with only one exception, because Qml requires objects to begin with a capital letter, such as `CheckBox`, `Section`, `VariableList`. An R programmer most likely won't make Qml objects, but will use them. To keep things as consistent as possible, we opted for the same CamelCasing rules in Qml (as opposed to PascalCasing), but with a StartingUppercaseLetter.

### Long option and descriptives names are not evil
To enhance human readability, we recommend the use of descriptive names for objects and functions. This can lead to long option names - and a more typing, but given that most of the time people use autocomplete features in text editors, it is a small price to pay for enhanced clarity - the best state of code documentation is when the user does not need additional documentation to understand what the code does. At the other extreme, the users would need to bury themselves in the documentation to understand what the arguments mean - which takes much more time than simply typing a few extra letters.

### Use camelCase
The reason that we opted for camelCase is that it does not need an extra character (such as an underscore, or full stop/dots) to separate words. Each capital letter indicates a new word. In R the names of objects and functions should start in lowercase, but each concatenated word should begins with a capital letter. No underscores are allowed. Neither are full stops/dots allowed for objects, unless they are used for S3 dispatch.

### Rephrain from using verbs for option names
Option names should preferably be nouns and not verbs. Verbs emphasise actions, and are reserved to identify functions.

#### Examples
- `histogramPlot`: Not `plotHistogram`, which emphasises plotting, thus, the action of plotting, hence, a function name.
- `errorBar`: Not `displayErrorBars`, which emphasises the displaying, and is therefore reserved for a function.
- `marginalMean`: Not `computeMarginalMean`, as the user does not compute, but a function will.

### Abbreviations are single words
Abbreviations should not be used in general, except for common option names (see below). When they are used, an abbreviation is treated as a single word - which is important to keep in mind with respect to the camelCase.

#### Examples
- `modelTerms`: Do not use `ModelTerms` nor `model_terms`, or `model.terms`.
- `mcmcMethod`: `mcmc` is an abbreviation of Markov chain Monte Carlo, and it is treated as a word. Do not use `MCMCMethod`
- `marginalMeanCi`: `Ci` is an abbreviation of confidence interval, and treated as a single word. Do not use `marginalMeanCI`
- `qqPlot`: Do not use `QQplot` or `QQPlot`.
- `ppPlot`: Do not use `Ppplot` or `PPPlot`.
- `contrastSeMethod`: `Se` is an abbreviation of a standard error, and so is treated as a word. Do not use `contrastSEMethod`

### Singular vs. plural form
Prefer singular form of nouns, *even if the output features multiple instances of the object*. Plurals are allowed to indicate *multiple inputs*.  

#### Examples
- `ci`: Do not use `cis` for confidence intervals (even if the output could feature multiple intervals)
- `pValue`: Do not use `pValues` for p-values (even if the output could feature multiple p-values)
- `marginalMeanTerms`: Use the singular form `marginalMean` (regardless of there being more marginal means in the output), but the use of the plural `Terms` is allowed to indicate to the user that they may specify multiple terms in the input.
- `variable`: Use singular in case that the `AssignedVariableList` in Qml specifies `singleVariable: true` to indicate to the user that only one variable is allowed. On the other hand, if it's possible to specify multiple variables, use the plural `variables` (and similarly, `fixedFactors`, `randomFactors`, `covariates`, etc.)
- An exception is when the input is a number, such as in `bootstrapSamples`. In this case, the input is still a singular object (a number), but the plural indicates that it expects just that: a number.

## Same principles in Qml
The discussion so far was concerned with R code. In Qml objects have to start with a capital letter such as `CheckBox`, `Section`, `VariableList`. To keep things as consistent as possible, we opted for CamelCasing in Qml, so all rules elaborated above remain. In particular, the rule regarding abbreviations apply. An R programmer most likely won't make Qml objects, but will use them.

### Correspondence between option names (seen in R) and labels (seen in JASP)
A label in a Qml form is what is printed in JASP such as "Fixed Factors", whereas an option name is a reference in the code such as `fixedFactors`. The option name visible to R users should be easily deducible from the label visible to the user in R, and vice versa. This also includes avoiding shortcuts or abbreviations except for standardised option names (see below).

#### Examples
- if an option is labeled as `label: qsTr("Variables")`, the option name should be `name: "variables"`, and not `name: "dependent"`.
- The variables for which the Kruskall-Wallis Test needs to be computed `label: qsTr("Kruskall-Wallis Terms")`, should get the option name `name: kruskalWallisTerms` (not `kwTerms`).

### Correspondence between options
If several options functionally belong together (e.g., relate to the same output), if they are grouped together in a `Section{}`, or if one option is a "child" of another option, it is often beneficial to use a common naming convention so that it is clear that the options belong together.

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

Here, the `CheckBox` with option name `errorBar` is a parent of the radio button control and the `CIField` component. To make this clear, we use the `"errorBar"` as prefix in when naming the children component that inherited from the checkbox. The reason for this is that when the user calls the analysis from R, the relationship between the arguments is clear from the names:

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
		name: "marginalMeanBootstrap"
		label: qsTr("From")
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

Here, we have a section for specifying marginal means. A section is a separated tab in the GUI. Every option name is prefixed with `marginalMean`. Furthermore, the number of bootstrapping replicates is a child of the checkbox that enables bootstrapping, and so it starts with `marginalMeanBootstrap`.

### Nested option names
In the previous section we had options that are nested in the Qml file, but they are not nested in the R syntax. However, there are cases where an option is actually nested within the R syntax as well. In this case, special care needs to be taken to adhere to the style of "option names within options".

#### Examples
##### Radio Buttons
We already saw this in the previous section. Radio Buttons are a special control because they result in a single argument in the syntax, but can take different character values. These character values need to adhere to the code style as well.

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
There are other, more complicated components. For example, `TabView` enables a collection of options that can be specified multiple times. A typical example is the SEM module which enables the user to specify multiple models in a single analysis.

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

note that `name: "models"` is in plural form; the option that can take a list of options. Each element of that list is a list that contains `name` and `syntax` elements, and will be used in R like this:

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

Notice that the options are nested within another option both in Qml and in the R syntax. Therefore, we do not need to prefix the nested options with the name of the parent option, i.e., we do not need to name the `TextArea` as `modelSyntax` nor the `optionKey` as `modelName`, as it is clear from the structure of the argument itself that `syntax` and `name` are part of the `models` option.

### Reserved option names
`data` and `formula` are reserved option names: **Do not use them for naming options in the Qml file.**

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
