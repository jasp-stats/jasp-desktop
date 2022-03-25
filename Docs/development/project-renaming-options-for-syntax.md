# Preparing analyses options for the syntax mode

One of the long term plans for JASP is to provide a "Syntax mode" that will allow users to call JASP analyses from R in a convenient way (if you are interested in the plan of the project, see the [Syntax wiki](https://github.com/jasp-stats/INTERNAL-jasp/wiki/Syntax-Mode)). An important part of the project is to provide wrappers around each of the analyses in JASP that can be called in a straightforward way. The end goal is that the wrappers will be easily understandeable for R users. For example, running a t-test in R will look something like

```r
jaspTTests::IndependentSamplesTTest(
    data              = dataset, 
    dependent         = "contNormal", 
    group             = "contBinom", 
    tests             = c("student", "welch"), 
    effectSize        = TRUE, 
    effectSizeType    = "cohen", 
    effectSizeCi      = TRUE, 
    effectSizeCiLevel = 0.99)
```

These wrappers will be generated automatically from the Qml files that define the analysis options in JASP. In order for us to provide consistent syntax across all of JASP analyses, and that we are also able to generate helpful and consistent documentation both in JASP and in R, we need to make systematic changes to the current Qml files in all JASP modules.

This document serves as a guide for what needs to be done.

## Renaming options

Each option in the Qml file has the `name` and `label` (or `title`) property. The `label` is displayed to the user of JASP in the input form. `name` on the other hand, is the option name in R. For example

```qml
CheckBox{ name: "effectSize";   label:  qsTr("Effect Size")}
```

In JASP, the user sees the string "Effect Size" (or its translation), whereas the analysis functions in R see the option as part of the `options` list, i.e., `options[["effectSize"]]`. Up until this point, there was not much attention paid to how we use the `name` property, as the options were never exposed to the user and usually pertain to only one specific analysis. However, when we generate the analysis wrappers, the `name` property will be used as the name of the argument of the option in the analysis wrapper. This means that the `name`s will be exposed to syntax users. Hence, we need to ensure that the names are used consistently across all of JASP analyses, which requires us to go through all Qml files and rename the options as necessary. 

### 1. Read the style guide

To facilitate consistency in naming options, we created an [option names style guide](guide-option-names.md). **Read it carefully and make sure to follow it when you define new option names!** 

### 2. Qml files

The first 

#### Moving documentation to `info`

Should we do this at this stage or not? Should we do it in two waves instead?

### 3. R files

#### Run tests

### 4. Upgrades.qml 

#### Test upgrades

### 5. Make a PR

Follow the [git guide](git-guide.md) to make a clean PR. Assign @Kucharssim or @AlexanderLy as reviewers.






