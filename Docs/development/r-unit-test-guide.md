Unit Tests
==========

This guide will first explain how to setup the folder structure in your module to allow testing. Afterwards the method of creating and running tests is explained.  
Note that it is required to have the R package [jaspTools](https://github.com/jasp-stats/jaspTools) installed. Keep it up to date: several features described below (in particular [creating tests from .jasp files](#creating-tests-from-jasp-files)) need a recent version.  

- [Unit Tests](#unit-tests)
  - [Folder structure](#folder-structure)
    - [testthat](#testthat)
    - [_snaps](#_snaps)
    - [jaspfiles](#jaspfiles)
    - [testthat.R](#testthatr)
    - [.github/workflows/unittests.yml](#githubworkflowsunittestsyml)
    - [NAMESPACE](#namespace)
  - [Creating unit tests](#creating-unit-tests)
    - [Creating a test file](#creating-a-test-file)
    - [Adding table tests](#adding-table-tests)
    - [Adding plot tests](#adding-plot-tests)
    - [Creating tests automatically for tables and plots](#creating-tests-automatically-for-tables-and-plots)
    - [Creating tests from .jasp files](#creating-tests-from-jasp-files)
    - [Adding error handling tests](#adding-error-handling-tests)
    - [Adding other types of tests](#adding-other-types-of-tests)
    - [Using custom datasets in your tests](#using-custom-datasets-in-your-tests)
  - [Running the unit tests](#running-the-unit-tests)
    - [Compact output for scripts and agents](#compact-output-for-scripts-and-agents)
  - [Fixing the unit tests](#fixing-the-unit-tests)
    - [Failures](#failures)
      - [Plots](#plots)
    - [Errors](#errors)

## Folder structure
Your module should include the following structure:
- ModuleName/
  - [NAMESPACE](#namespace)
  - tests/
    - [testthat/](#testthat)
      - [_snaps/](#_snaps)
      - [jaspfiles/](#jaspfiles) (only if you create tests from .jasp files)
    - [testthat.R](#testthatr)
  - [.github/workflows/unittests.yml](#githubworkflowsunittestsyml)

### testthat
An empty folder, until you add tests, at that point it will contain a number of test files as described in [Creating a test file](#creating-a-test-file).

### _snaps
An empty folder, will be automatically created and filled later, see [Adding plot tests](#adding-plot-tests). Besides the reference .svg files it also holds the structural fallback snapshots of ggplot2 plots, see [Plots](#plots).

### jaspfiles
Only needed if you [create tests from .jasp files](#creating-tests-from-jasp-files). It contains the .jasp files that the tests are generated from, in three subfolders: `library/` (the example files from the JASP data library), `verified/` (the verified examples from the JASP verification project) and `other/` (any other .jasp file). Commit these files together with the tests, the tests read them from here.

### testthat.R
```
library(jaspTools)
library(testthat)

jaspTools::runTestsTravis(module = getwd())
```
Despite its name this is the entry point for any CI system, not only Travis. When it runs on CI (the environment variable `CI` or `R_COVR` is set) it points jaspTools at the module, runs all tests, and, if R is not running interactively, exits R with the testing status (non-zero if any test failed), so that the CI job fails too. In any other situation it simply calls `jaspTools::testAll()`. (The argument is called `modulePath`; `module` works through partial matching.)

### .github/workflows/unittests.yml
The CI workflow of a module does not contain the setup and test steps itself. It calls the reusable workflow in [jasp-stats/jasp-actions](https://github.com/jasp-stats/jasp-actions), which sets up R and the packages, and then runs `tests/testthat.R`. The default workflow is
```
on:
  push:
    paths: ['**.R', 'tests/**', '**.Rd', '**.c', '**.cpp', '**.h', '**.hpp', 'DESCRIPTION', 'NAMESPACE', 'MAKEVARS', 'MAKEVARS.win', '**.yml', 'renv.lock']
  pull_request:
    paths: ['**.R', 'tests/**', '**.Rd', '**.c', '**.cpp', '**.h', '**.hpp', 'DESCRIPTION', 'NAMESPACE', 'MAKEVARS', 'MAKEVARS.win', '**.yml', 'renv.lock']
  schedule:
    - cron: '13 12 * * 1-5'

name: unit-tests

jobs:

  unit-tests:
    # pull requests from a branch in this repository are already covered by the push trigger
    if: github.event_name != 'pull_request' || github.event.pull_request.head.repo.full_name != github.event.pull_request.base.repo.full_name
    uses: jasp-stats/jasp-actions/.github/workflows/unittests.yml@v1
    with:
      needs_JAGS: false
```
The R version and the operating systems (macOS, Windows and Linux) are fixed in jasp-actions, so the module workflow does not specify them. The package versions come from the `renv.lock` of your module: the jobs install exactly the packages listed there (so keep that file up to date). One extra Linux job tests with the latest package versions instead; it is allowed to fail without failing the run. If a test fails, the new plot images (`*.new.svg` in `tests/testthat/_snaps/`) are uploaded as the artifact `snapshot-diffs`. All jobs use that same artifact name, so you will typically get the images of only one of the failed jobs.

The workflow has the following inputs, all optional:
- `needs_JAGS` (default `false`): install JAGS. Set this to `true` if your module uses JAGS.
- `needs_igraph` (default `false`): documented as "recompile igraph (Linux only)", but the workflow does not use it at the moment, so it currently has no effect.
- `continue_on_schedule` (default `false`): documented as limiting the scheduled runs to repositories owned by `jasp-stats`. Note that the check in the workflow does not enforce this at the moment.
- `environment_retention_days` (default `30`): how long the recorded R environment of a run is kept.

`@v1` refers to major version 1 of the workflow. Some modules use `@master` instead, which always follows the latest version.

### NAMESPACE
jaspTools finds your analysis by looking in the `NAMESPACE` of the module, so the main R function of every analysis must be exported, either with `export(MyAnalysis)` or through an `exportPattern()`. If it is not, `jaspTools::analysisOptions("MyAnalysis")`, `jaspTools::runAnalysis()`, `jaspTools::testAnalysis()`, `jaspTools::agentTestAnalysis()` and `jaspTools::manageTestPlots("MyAnalysis")` stop with `Could not locate R function`. If the module also exports `MyAnalysisInternal`, jaspTools runs that function instead of `MyAnalysis`. The analysis must also be declared in `inst/Description.qml`; otherwise `runAnalysis()` and `analysisOptions("MyAnalysis")` stop with `Could not find analysis ... in the Description.qml of the module`.

## Creating unit tests
The analysis tests in modules are based on the R package `testthat` (for more information about this package see [this online book chapter by Hadley Wickam](http://r-pkgs.had.co.nz/tests.html)).

The basic idea is that your `testthat` folder will contain one or more files that each hold a number of unit tests. Each unit test (identified by `test_that(...)`) is be comprised of one or more expectations (identified by `expect_...()`). These expectations allow you to verify that certain aspects of the analysis output did not change when compared to earlier versions of your module.

A module should have one test file per analysis (test files generated from .jasp files, see [below](#creating-tests-from-jasp-files), come on top of that, one per .jasp file). This file will check multiple aspects of the analysis, such as if the error handling works, if the data of the output tables did not change (results of our calculations should almost never change!) and if the plots still look the same.

`jaspTools` offers some functionality to test tables (`expect_equal_tables()`) and plots (`expect_equal_plots()`), but if you wish to test other aspects of the analysis output, you should look into the functionality of `testthat`.

### Creating a test file
Before you create any unit tests, we first need to create the file(s) that will store these tests.  
The file needs to be located in `tests/testthat/` and named according to the scheme `test-'analysisname'.R`, where `'analysisname'` refers to the name of the main R function of an analysis (e.g., `test-anova.R`).  
Every analysis in the module should have a separate test file.

### Adding table tests
This section explains how to manually create a unit test for a table. For automatic creation of this type of test look [here](#creating-tests-automatically-for-tables-and-plots).

We want to make sure that the table data does not change as a result of unintended side effects of code changes. To ensure this, we create a snapshot of the table data and hardcode this in our test file. We then use `jaspTools::expect_equal_tables()` to check if the snapshot is identical to the table data obtained at a later point in time (and if it's not the test will fail).

`expect_equal_tables` has the following arguments:  
`test`: The new table data, this should point to the `[["data"]]` entry of a table in a results list obtained from `jaspTools::runAnalysis()`.  
`ref`: The table data snapshot obtained by applying `jaspTools::makeTestTable()` to the `[["data"]]` entry of a table.  
`label` [optional]: If you have multiple expectations in a single unit test, this allows you to identify a specific expectation.

The snapshot of the table data can be created by supplying a table to `jaspTools::makeTestTable()`:
```
options <- jaspTools::analysisOptions("BinomialTest")
options$variables <- "contBinom"
options$testValue <- 0.5
results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
table <- results[["results"]][["binomialTable"]][["data"]]
jaspTools::makeTestTable(table)
```
Note that the creation of the options shown in the above example can be done in various different ways as described [here](https://github.com/jasp-stats/jaspTools#obtaining-options).  
The above returns the output:

`list("TRUE", 58, 0, 0.133210619207213, 0.58, 100, "contBinom", "FALSE",
	 42, 1, 0.133210619207213, 0.42, 100, "contBinom")`

And with this we can write the table test:
```
test_that("Binomial table results match", {
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables <- "contBinom"
  options$testValue <- 0.5
  results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
  table <- results[["results"]][["binomialTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
		 list("TRUE", 58, 0, 0.133210619207213, 0.58, 100, "contBinom", "FALSE",
	           42, 1, 0.133210619207213, 0.42, 100, "contBinom"))
})
```

### Adding plot tests
This section explains how to manually create a unit test for a plot. For automatic creation of this type of test look [here](#creating-tests-automatically-for-tables-and-plots).

As with tables, we want to make sure that the plot does not change as a result of unintended side effects of code changes. To ensure this, we create a reference .svg of the plot and store this in `tests/testthat/_snaps/` (note that the storage will be done automatically). We then use `jaspTools::expect_equal_plots()` to check if the reference .svg is identical to the plot obtained at a later point in time (and if it's not the test will fail). This function is a wrapper around `vdiffr::expect_doppelganger()` (for more information about `vdiffr` see their [GitHub page](https://github.com/lionel-/vdiffr)).

`expect_equal_plots` has the following arguments:  
`test`: The new plot object, this should point to the `[["state"]][["figures"]][[INDEX/PLOTNAME]][["obj"]]` entry of a plot in a results list obtained from `jaspTools::runAnalysis()`.  
`name`: The name the plot (will be) stored under (commonly the name of the plot type, e.g., `"prior-posterior"`).  
`tolerance` [optional]: The numerical tolerance of the structural fallback comparison (default `1e-6`), see [Plots](#plots). Raise it (e.g., to `1e-3`) for plots whose data differ slightly between platforms, such as plots based on MCMC samples.  

The creation of the reference .svg is an automatic process which cannot be started until the entire unit test has been written.
So first we create a unit test for a specific descriptives plot:
```
test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables <- "contBinom"
  options$testValue <- 0.5
  options$descriptivesPlots <- TRUE
  results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
  plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_0"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives")
})
```
Note that the creation of the options shown in the above example can be done in various different ways as described [here](https://github.com/jasp-stats/jaspTools#obtaining-options).  
Notice also that we first obtain the plotname and then use this to index in the `state`. Plot objects are not stored in `results[["results"]]` but instead in `results[["state"]]`. You can also use a numeric index instead of the plotname.

After creating the unit test and storing it in a file we'll want to generate the reference image. To accomplish this you should run all unit tests (through `jaspTools::testAll()` or `jaspTools::testAnalysis()`). It does not matter when you create the reference image of a unit test: this could be after you create only a single plot unit test, or after multiple.
Running the unit tests the very first time causes a reference .svg to be created and placed in `tests/testthat/_snaps/` (in the above example: `tests/testthat/_snaps/BinomialTest/descriptives.svg`).  


In our example we created a plot test for one analysis so we can be specific in what test file we want to look:
```
jaspTools::testAnalysis("BinomialTest")
```

### Creating tests automatically for tables and plots
Of course, the above set of steps might be a bit tedious to perform for every single table and plot. You can let `jaspTools::runAnalysis()` take care of making unit tests by setting `makeTests = TRUE`. So if we take the example from the manual section:
```
options <- jaspTools::analysisOptions("BinomialTest")
options$variables <- "contBinom"
options$testValue <- 0.5
options$descriptivesPlots <- TRUE
results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options, makeTests = TRUE)
```
Which prints the following tests to the R console:

```
options <- analysisOptions("BinomialTest")
options$variables <- "contBinom"
options$descriptivesPlots <- TRUE
options$testValue <- 0.5
set.seed(1)
results <- runAnalysis("BinomialTest", "test.csv", options)

test_that("Binomial Test table results match", {
	table <- results[["results"]][["binomialTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("TRUE", 58, 0, 0.133210619207213, 0.58, 100, "contBinom", "FALSE",
			 42, 1, 0.133210619207213, 0.42, 100, "contBinom"))
})

test_that("0 plot matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_0"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "0")
})

test_that("1 plot matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "1")
})
```

You will be able to copy-paste the output directly into a test file. Just make sure that each test/expectation title makes some sense (e.g., `"Binomial Test table results match"` makes sense but `"1 plot matches"` could use some work).  
In the example above we should adjust the two plot tests, because "0" and "1" are not very informative:
```
test_that("Descriptives plot contBinom-level 0 matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_0"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives-0")
})

test_that("Descriptives plot contBinom-level 1 matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives-1")
})
```
Note that it is not recommended to add multiple tests for the same element if the tests do not test distinct aspects of the element.  
In our example, we could get rid of one of the two plots, in which case we end up with:
```
options <- analysisOptions("BinomialTest")
options$variables <- "contBinom"
options$descriptivesPlots <- TRUE
options$testValue <- 0.5
set.seed(1)
results <- runAnalysis("BinomialTest", "test.csv", options)

test_that("Binomial Test table results match", {
	table <- results[["results"]][["binomialTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("TRUE", 58, 0, 0.133210619207213, 0.58, 100, "contBinom", "FALSE",
			 42, 1, 0.133210619207213, 0.42, 100, "contBinom"))
})

test_that("Descriptives plot matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_0"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives")
})
```

You can do this for how ever many `jaspTools::runAnalysis()`'s to get a good coverage. If you have .jasp files that use your analyses, you can create many tests at once, see [Creating tests from .jasp files](#creating-tests-from-jasp-files).

As explained earlier (see [Adding plot tests](#adding-plot-tests)), to validate the new plots you should run:
```
jaspTools::testAll() # or jaspTools::testAnalysis()
```

### Creating tests from .jasp files
If you have .jasp files that use the analyses of your module (for example the example files of the JASP data library, or files you saved yourself), `jaspTools::makeTestsFromExamples()` turns them into test files in one go. For every analysis in a file it reads the options and the data from the .jasp file, runs the analysis, and writes the resulting tables and plots as expectations.

jaspTools reads .jasp files through [jaspSyntax](https://github.com/jasp-stats/jaspSyntax), so make sure jaspTools is up to date, and run `jaspTools::setupJaspTools()` once after every (re)install of jaspTools.

1. Put your .jasp files in one of the source folders in `tests/testthat/jaspfiles/`:
   - `library/`: the example files from the JASP data library. You find them in the jasp-desktop repository, in `Resources/Data Sets/Data Library/`, in one subfolder per module (for example `2. T-Tests/`). Copy the ones that use your analyses.
   - `verified/`: the verified examples from the JASP verification project. These are protected: regenerating tests does not touch them unless you ask for it explicitly.
   - `other/`: any other .jasp file, including files you import with `path` (see below).

   The folder determines the name of the generated test file and how the file is treated when you regenerate tests (see `overwrite` below).
2. Generate the tests, from the root of your module (or pass the module path as `module.dir`):
```
jaspTools::makeTestsFromExamples()
```
3. Run the tests once to create the reference images of the plots, see [Adding plot tests](#adding-plot-tests):
```
jaspTools::testAll() # or jaspTools::testAnalysis()
```

`makeTestsFromExamples()` has the following arguments, all optional:  
`path`: A folder with .jasp files that you have lying around. They are copied into `tests/testthat/jaspfiles/other/` (overwriting files with the same name) and tests are generated for them. `source` is ignored when `path` is given.  
`module.dir`: The path to your module, which must contain `inst/Description.qml`. Defaults to the working directory.  
`source`: Which source folders to process, any of `"library"`, `"verified"` and `"other"`. By default all three, or only `"library"` and `"other"` if `overwrite = TRUE`.  
`sanitize`: Replace characters in the .jasp file name that are not letters, digits or underscores by hyphens in the name of the test file (default `FALSE`).  
`overwrite`: By default (`FALSE`) existing test files are skipped, so that you can add a new .jasp file without touching the other tests. With `TRUE` they are regenerated. This leaves the `verified` folder alone unless you ask for it explicitly with `source = "verified"`; in an interactive R session you then have to confirm (answering "No" aborts without touching any file), in a non-interactive session you only get a warning and the files are overwritten.  

Every .jasp file becomes a single test file named `test-<source>-<name>.R`, for example `tests/testthat/test-library-Sleep.R`:
```
context("Library: Sleep")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("Descriptives results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Sleep.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("Descriptives", dataset, opts)

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "sf:4;dp:3", "number", 3.7, 0.75, "sf:4;dp:3", "number", -1.6,
     0, 1.78900965775916, 10, "extra", 2, "sf:4;dp:3", "number",
     5.5, 2.33, "sf:4;dp:3", "number", -0.1, 0, 2.00224873579683,
     10, "extra"))

})
```
A plot in the results is tested like this (from `test-library-Boxplot.R`):
```
  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_plot-1")
```
Things to know about the generated tests:
- If the .jasp file contains several analyses, each gets its own `test_that()` block (e.g., `"Descriptives (analysis 2) results match"`).
- Analyses that are not part of your module (they are not listed in `inst/Description.qml`) are skipped.
- If an analysis fails while the tests are generated you get a warning, and a test that only checks that the analysis `runs without error` is written instead. Fix the cause and regenerate with `overwrite = TRUE`. A common cause is an analysis function that is not exported in the [NAMESPACE](#namespace).
- If the results of an analysis contain no tables or plots (for example because the options in the .jasp file lead to a validation error), the block is still titled `... results match`, but it only checks that the analysis did not return an error (look for `# Basic check - analysis runs without error` in the generated files). Such a test says very little, so replace it or remove the file.
- Plots are named `analysis-<number of the analysis>_figure-<number of the plot>_<title>`.
- Regenerating a test on another machine, or with other package versions, can change the last digits of some numbers (differences around `1e-10` show up in the diff). `expect_equal_tables()` rounds the values before it compares them, so such tests keep passing and you do not need to commit these changes.
- The tests contain whatever your analysis returns at the moment of generating. That is a snapshot, not a verification that the results are correct: check the output against your expectations (or a reference implementation) before you commit the tests, and give unclear plot names a proper name.

You can also write such a test by hand, with the same functions the generated tests use:
```
jaspFile <- testthat::test_path("jaspfiles", "other", "myFile.jasp")
opts <- jaspTools::analysisOptions(jaspFile)
dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)
set.seed(1)
results <- jaspTools::runAnalysis("MyAnalysis", dataset, opts)
```
If the .jasp file holds a single analysis, `jaspTools::analysisOptions()` returns the options of that analysis. If it holds several, it returns a list with the options of each analysis (each one carries the name of its analysis), and you select one with `opts[[2]]`, for instance. The column names in the options and the dataset are the ones of the .jasp file, you do not have to rename or encode anything.

### Adding error handling tests
If you have `.hasErrors()` calls within your analysis, then it's a good idea to verify that they all work correctly.
You can do this by creating a unit test that checks if your analysis exits when you supply it incompatible data (e.g., data that contains infinities).
For these types of checks we use expectations provided by `testthat`:
```
test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("RegressionLinear")

  options$dependent <- "debInf"
  options$covariates <- "contGamma"
  options$modelTerms <- list(list(components="contGamma", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Inf dependent check")

  options$dependent <- "contNormal"
  options$covariates <- "debInf"
  options$modelTerms <- list(list(components="debInf", isNuisance=FALSE))
  results <- jaspTools::runAnalysis("RegressionLinear", "test.csv", options)
  expect_identical(results[["status"]], "validationError", label = "Inf covariate check")
})
```
Maybe your analysis does not exit when it has invalid data, but instead it logs this in a footnote
```
test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  
  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")
  
  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")
})
```

### Adding other types of tests
You can explore the `results` object returned by `jaspTools::runAnalysis()` and test any aspect of it.
It's even possible to create your [own expectations](https://cran.r-project.org/web/packages/testthat/vignettes/custom-expectation.html).

### Using custom datasets in your tests
`jaspTools` comes bundled with all the .csv datasets available in the data library of JASP.
Additionally, it has a copy of `debug.csv` named `test.csv`. It's recommended to use `test.csv` as it is guaranteed the data in this file will never change.  
If you would like to use something else still, that is also possible. There are three ways:

- Add a .csv file inside `tests/testthat` and refer to it with `testthat::test_path()`. Say you have `tests/testthat/someData.csv`:
```
jaspTools::runAnalysis("TTestIndependentSamples", testthat::test_path("someData.csv"), options)
```
`test_path()` gives the correct path both when the tests are run (the working directory is then `tests/testthat`) and when you run the code from the root of your module in the console.
- Build a `data.frame` in the test itself, which is convenient for small datasets, and pass it instead of the file name:
```
dataset <- data.frame(x = c(1, 2, 3, 4), g = c("a", "a", "b", "b"))
jaspTools::runAnalysis("TTestIndependentSamples", dataset, options)
```
- Take the dataset from a .jasp file with `jaspTools::extractDatasetFromJASPFile()`, see [Creating tests from .jasp files](#creating-tests-from-jasp-files).

Note that if you pass only a file name, such as `"someData.csv"`, jaspTools looks for it relative to the working directory and among the bundled datasets, but not in `tests/testthat`. While the tests are run by `testAll()` or `testAnalysis()`, the working directory is `tests/testthat`, so a bare file name works there; it does not work if you run the same lines in the console from the root of your module. `test_path()` works in both cases.

## Running the unit tests

To run all tests in a module, type
```
jaspTools::testAll()
```
Note that if you include several modules in `jaspTools::setPkgOption("module.dirs", ...)`, then the tests of all of these modules will be run sequentially.

Any test that fails is shown in the R console.  
Warnings may be ignored, but should be minimized. The exception is the warning about the structural fallback of a plot test, which tells you that a reference plot needs to be updated (see [Plots](#plots)).

It is also possible to test a specific analysis, as running all unit tests in a module may take some time
```
jaspTools::testAnalysis("Anova")
```
This runs `test-Anova.R`, and also the tests [generated from .jasp files](#creating-tests-from-jasp-files) (`test-library-*.R`, `test-verified-*.R` and `test-other-*.R`) that run this analysis (found by looking for a `runAnalysis("Anova", ...)` call in the file, so a call through a variable is not found). Use `includeAll = FALSE` to leave out the files that are only found because they call the analysis. Files whose name matches the analysis are always run, ignoring case: `test-Anova.R`, but also `test-verified-Anova.R`, `test-library-Anova.R` and `test-other-Anova.R`.

Both `jaspTools::testAll()` and `jaspTools::testAnalysis()` have an `onlyPlots` argument. With `onlyPlots = TRUE` only the tests that contain `expect_equal_plots()` are run, which saves time if you are only interested in generating or checking the reference images of plots.

### Compact output for scripts and agents
`jaspTools::agentTestAll()` and `jaspTools::agentTestAnalysis("Anova")` run the same tests but print only a short summary, which is convenient for scripts and for AI coding agents. The details (failures, warnings and backtraces) are written to a JSON log file, by default a temporary file. Its path is part of the summary and you can choose it yourself with the `logFile` argument. `agentTestAnalysis()` also has the `includeAll` argument. These functions need testthat 3.3.2 or newer (they use its `LlmReporter`), which is newer than what jaspTools itself requires, so update testthat if they fail.

The functions return (invisibly) an object with, among others, the fields `status` (0 if everything passed), `summary` (the numbers of passed, failed, skipped, etc., where `pass` counts expectations, not `test_that()` blocks), `failures`, `warnings`, `skips` (data frames with one row per failure, warning or skip), `tests` (one row per `test_that()` block) and `logFile`:
```
results <- jaspTools::agentTestAnalysis("Anova")
results$status
results$failures
```

## Fixing the unit tests
If a test shows up as failed, you should verify why this is and fix it before making a pull request.  
If you made a legitimate change that the test does not cover, then the unit test should be updated.  

You can locate the offending test inside `tests/testthat`; your R console will show the filename and line number -- e.g., `test-jagsmodule.R:56: failure:`.

### Failures
If `testthat` shows that a `failure` occurred then that means the unit test ran okay, but the results do not match. If it is anything but a plot (e.g., a table) the R console will notify you of the exact problem. You should determine why this change occurred and if it is desired. If it is desired, then you can update the unit test.

#### Plots
To view changes in plot tests you should run `jaspTools::manageTestPlots()` (e.g., `manageTestPlots("jagsModule")`). If the change is legitimate you can validate the failing plot in the Shiny application; the reference .svg in the `tests/testthat/_snaps/` folder will be updated automatically.

Plots that are drawn slightly differently on different platforms (font rendering, for instance) would make a test based on .svg files fail for no good reason. Therefore, if the plot is a ggplot2 plot and the comparison with the reference .svg fails, `expect_equal_plots()` falls back to comparing the structure of the plot: its labels, the data of its layers, its scales and its layout. This structure is stored in the `reference_plotobject` subfolder of the folder that holds the .svg: `tests/testthat/_snaps/<test name>/reference_plotobject/<plot name>.rds`, where `<test name>` is the name of the test file without `test-` and `.R` (case and spaces are kept, e.g. `library-Boxplot`) and, like for the .svg, the plot name is written in lower case with hyphens (e.g. `analysis-1-figure-1-plot-1.rds`). If the structure matches, the test passes with a warning that starts with `vdiffr mismatch for '<name>' accepted by structural fallback` (`<name>` is the name exactly as you gave it to `expect_equal_plots()`), and the new image is saved next to the reference as `<plot name>.new.svg`. Some things to know:
- A structural snapshot is created at the moment a plot gets its reference .svg for the first time. After that, a missing one is created whenever the .svg matches and you run the tests in an interactive R session (not on CI). Commit these files together with the .svg files.
- The fallback only works for plots that have a structural snapshot. If the .svg of a plot differs and there is no .rds for it, the test fails (`No ggplot structural snapshot exists`), and none is created at that moment.
- A warning about the fallback means that the .svg no longer matches. Run `jaspTools::manageTestPlots()` to inspect and accept the new .svg; otherwise every future run keeps going through the slower fallback. `testthat::snapshot_accept()`, run from the root of your module, accepts all new .svg files at once without showing them to you.
- To create structural snapshots for plots that only have an .svg (for instance in a module that existed before this fallback), or to update them after a legitimate change, run the tests once with `options(jaspTools.plotStructure.update = TRUE)` (or with the environment variable `JASP_PLOT_STRUCTURE_UPDATE=true`) and commit the changed .rds files. This only works for plots whose .svg matches. If the .svg changed as well, accept the new .svg first and run the tests in update mode afterwards.
- On CI a plot that has no reference .svg yet fails the test (`vdiffr created a new visual snapshot`), so create and commit the references locally. This needs testthat 3.3.0 or newer, which started failing new snapshots on CI.
- Plots that are not ggplot2 plots have no fallback, and plots that are `grob`s or recorded plots are skipped altogether. A `jaspGraphsPlot` (several plots in one) is checked per subplot, named `<name>-subplot-<i>`, and each subplot does get the fallback.
- The structural comparison (after rounding numbers to 10 significant digits) allows differences up to `1e-6`. You can change this per plot with the `tolerance` argument of `expect_equal_plots()`.

If a test fails on CI and you cannot reproduce it locally, download the `snapshot-diffs` artifact of the failed run (see [.github/workflows/unittests.yml](#githubworkflowsunittestsyml)) and compare the `*.new.svg` files with the reference .svg files.

### Errors
If `testthat` shows that an `error` occurred then that means the unit test could not run properly; it terminated too soon. To figure out the cause of termination it is often useful to run the unit test code directly in RStudio. This means going to the test file, locating the unit test and then running everything between `jaspTools::analysisOptions(...)` and `jaspTools::runAnalysis(...)` so see what the output of `runAnalysis()` is.
