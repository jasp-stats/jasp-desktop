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

Open the analysis Qml file that you need to check for consistency. Edit the option names (or values) according to the style guide.


### 3. R files

If you changed any option name in the Qml files, now it is time to make sure that the changes are also reflected in the R files. Edit the R files.

The option names need to be also updated in the test files in the `tests/testthat/` folder!

**Note**. Be careful with using "find and replace all" especially if there is a chance of changing code that you did not intend to change, e.g., due to partial matching. An easy way to prevent this to happen is to use wider context of the text to replace. E.g. if we renamed an option from `old_option_name` to `newOptionName`, you can run find: `options[["old_option_name"]]` and replace all: `options[["newOptionName"]]` to avoid any surprises. You still need to check manually whether there is anything that you missed. In some situations, you may find R Studio's feature "Rename in Scope" useful. Read about it [here](https://appsilon.com/rstudio-shortcuts-and-tips/#code-inserting).

#### Run tests

After you have made all your changes, run the tests using `jaspTools::testAll()` to test whether anything is broken. 

### 4. Upgrades.qml 

Renaming options means that .jasp files saved with an older JASP version will no longer be compatible. To ensure backwards compatibility, the changes made to the option names need to be listed in the upgrades file in `inst/Upgrades.qml`. Follow the [guide for option ugrades](jasp-upgrade-qml.md).

Be careful when specifying the version numbers of the upgrade. The current version of the module is specified in the `DESCRIPTION` and `inst/Description.qml` files.

#### Test upgrades

After you specified the upgrades in the `Upgrades.qml`, test whether they work. 

1. Open the analysis you are modifying using the latest official release of JASP. 
2. Run an analysis using all options that you changes during this process. 
3. Save the .jasp file. Close the file.
4. Install your current version of the module you are working on as a developer module.
5. Open the .jasp file you saved. Refresh the file. If the analysis looks like before, everything went smoothly.


### 5. Make a PR

Follow the [git guide](git-guide.md) to make a clean PR. Assign @Kucharssim or @AlexanderLy as reviewers.


## Tips

### Workflow

Steps 2-4 need not be done in the sequence as written in this document. On contrary, it may be helpful to edit the `Upgrades.qml` file already while renaming the options in the analysis form. For example, if you change an option name in `Analysis.qml` file, it is recommended to already write the corresponding upgrade in the `Upgrades.qml` file. This way, you are documenting the changes what you have done so far and make sure you do not forget anything.

### Code duplication between analyses

Sometimes, different analyses come with similar (or even the same) chunks of options. An example of this is ANOVA and ANCOVA analyses which are almost identical, barring just a couple of small differences. To reduce the need to duplicate the work when renaming the option names, and to make sure that the options are indeed consistent between the analyses, we recommend trying to reduce code duplication between the analyses by writting common parts of the QML forms as reusable QML components (see section 'Advanced Usage' in the [QML guide](jasp-qml-guide.md)).

If you decide to refactor some of the QML code to reduce code duplication, first make a separate PR introducing these changes, and only after this PR is merged, make another PR that renames the options. This way it is easier to catch potential issues with both the refactor and renaming options. PRs that introduce renaming options should only rename options and nothing else.

And example of this can be found in ANOVA module where we first refactored the QML forms in one PR ([https://github.com/jasp-stats/jaspAnova/pull/173](https://github.com/jasp-stats/jaspAnova/pull/173)), and only then renamed the options in another PR ([https://github.com/jasp-stats/jaspAnova/pull/180](https://github.com/jasp-stats/jaspAnova/pull/180)).




