Unit Tests
==========

This guide will first explain how to setup the folder structure in your module to allow testing. Afterwards the method of creating and running tests is explained.  
Note that it is required to have the R package [jaspTools](https://github.com/jasp-stats/jaspTools) installed.  

- [Unit Tests](#unit-tests)
  - [Folder structure](#folder-structure)
    - [figs](#figs)
    - [testthat](#testthat)
    - [testthat.R](#testthatr)
    - [.travis.yml](#travisyml)
  - [Creating unit tests](#creating-unit-tests)
    - [Creating a test file](#creating-a-test-file)
    - [Adding table tests](#adding-table-tests)
    - [Adding plot tests](#adding-plot-tests)
    - [Creating tests automatically for tables and plots](#creating-tests-automatically-for-tables-and-plots)
    - [Adding error handling tests](#adding-error-handling-tests)
    - [Adding other types of tests](#adding-other-types-of-tests)
    - [Using custom datasets in your tests](#using-custom-datasets-in-your-tests)
  - [Running the unit tests](#running-the-unit-tests)
  - [Fixing the unit tests](#fixing-the-unit-tests)
    - [Failures](#failures)
      - [Plots](#plots)
    - [Errors](#errors)
  - [Dependencies related to plots](#dependencies-related-to-plots)

## Folder structure
Your module should include the following structure:
- ModuleName/
  - tests/
    - [figs/](#figs)
    - [testthat/](#testthat)
    - [testthat.R](#testthatr)
  - [.travis.yml](#travisyml)
  
### figs
An empty folder, will be automatically filled later, see [Adding plot tests](#adding-plot-tests).

### testthat
An empty folder, until you add tests, at that point it will contain a number of test files as described in [Creating a test file](#creating-a-test-file).

### testthat.R
```
library(jaspTools)
library(testthat)

jaspTools::runTestsTravis(module = getwd())
```

### .travis.yml
```
os: osx
language: r
r: 3.6.1
before_install: 
  - "git clone --branch=MacOS-Original https://github.com/jasp-stats/jasp-required-files.git ~/pkgs"
install:
  - RScript -e ".libPaths(c(.libPaths(), '~/pkgs')); install.packages('remotes'); remotes::install_github('jasp-stats/jaspTools', upgrade = 'never')"
script:
  - R < tests/testthat.R --no-save
env:
  global:
    - R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
    - VDIFFR_RUN_TESTS=true
    - REQUIRED_PKGS=~/pkgs

```

## Creating unit tests
The analysis tests in modules are based on the R package `testthat` (for more information about this package see [this online book chapter by Hadley Wickam](http://r-pkgs.had.co.nz/tests.html)).

The basic idea is that your `testthat` folder will contain one or more files that each hold a number of unit tests. Each unit test (identified by `test_that(...)`) is be comprised of one or more expectations (identified by `expect_...()`). These expectations allow you to verify that certain aspects of the analysis output did not change when compared to earlier versions of your module.

A module should have one test file per analysis. This file will check multiple aspects of the analysis, such as if the error handling works, if the data of the output tables did not change (results of our calculations should almost never change!) and if the plots still look the same.

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

As with tables, we want to make sure that the plot does not change as a result of unintended side effects of code changes. To ensure this, we create a reference .svg of the plot and store this in `tests/figs` (note that the storage will be done automatically). We then use `jaspTools::expect_equal_plots()` to check if the reference .svg is identical to the plot obtained at a later point in time (and if it's not the test will fail). This function is a wrapper around `vdiffr::expect_doppelganger()` (for more information about `vdiffr` see their [GitHub page](https://github.com/lionel-/vdiffr)).

`expect_equal_plots` has the following arguments:  
`test`: The new plot object, this should point to the `[["state"]][["figures"]][[INDEX/PLOTNAME]][["obj"]]` entry of a plot in a results list obtained from `jaspTools::runAnalysis()`.  
`name`: The name the plot (will be) stored under (commonly the name of the plot type, e.g., `"prior-posterior"`).  
`dir`: 	The directory in `tests/figs` where the reference .svg will be/is stored (commonly the name of the analysis)

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
  expect_equal_plots(testPlot, "descriptives", dir = "BinomialTest")
})
```
Note that the creation of the options shown in the above example can be done in various different ways as described [here](https://github.com/jasp-stats/jaspTools#obtaining-options).  
Notice also that we first obtain the plotname and then use this to index in the `state`. Plot objects are not stored in `results[["results"]]` but instead in `results[["state"]]`. You can also use a numeric index instead of the plotname.

After creating the unit test and storing it in a file, we call `jaspTools::manageTestPlots()`. This function starts a Shiny application that allows you to view and then validate your new plot. Validating a plot creates the reference .svg in `tests/figs/` (in the above example: `tests/figs/BinomialTest/descriptives.svg`).  
It does not matter when you perform this validation step: this could be after 1 or multiple plot unit tests.

In our example we created a plot test for one analysis so we can be specific in what test file we want to look:
```
jaspTools::manageTestPlots("BinomialTest")
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
	jaspTools::expect_equal_plots(testPlot, "0", dir = "BinomialTest")
})

test_that("1 plot matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "1", dir = "BinomialTest")
})
```

You will be able to copy-paste the output directly into a test file. Just make sure that each test/expectation title makes some sense (e.g., `"Binomial Test table results match"` makes sense but `"1 plot matches"` could use some work).  
In the example above we should adjust the two plot tests, because "0" and "1" are not very informative:
```
test_that("Descriptives plot contBinom-level 0 matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_0"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives-0", dir = "BinomialTest")
})

test_that("Descriptives plot contBinom-level 1 matches", {
	plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_contBinom"]][["collection"]][["containerPlots_contBinom_1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives-1", dir="BinomialTest")
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
	jaspTools::expect_equal_plots(testPlot, "descriptives", dir = "BinomialTest")
})
```

You can do this for how ever many `jaspTools::runAnalysis()`'s to get a good coverage. If you use a .jasp file with multiple analysis calls as input to `jaspTools::analysisOptions()` then you can quickly create many tests:

```
options <- jaspTools::analysisOptions("path/to/file.jasp") # there are multiple analyses so options is a list of lists
jaspTools::runAnalysis("BinomialTest", "test.csv", options[[1]], makeTests = TRUE)
jaspTools::runAnalysis("BinomialTest", "test.csv", options[[2]], makeTests = TRUE)
```

As explained earlier (see [Adding plot tests](#adding-plot-tests)), to validate the new plots you should run:
```
jaspTools::manageTestPlots()
```

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
If you would like to use something else still, that is also possible -- simply add it inside `tests/testthat`.

Say you have `tests/testthat/someData.csv`, you will be able to use this like:
```
jaspTools::runAnalysis("TTestIndependentSamples", "someData.csv", options)
```
Just ensure that your working directory is set to `tests/testthat` when making the test. 

## Running the unit tests

To run all tests in a module, type
```
jaspTools::testAll()
```
Note that if you include several modules in `jaspTools::setPkgOption("module.dirs", ...)`, then the tests of all of these modules will be run sequentially.

Any test that fails is shown in the R console.  
Warnings may be ignored, but should be minimized.

It is also possible to test a specific analysis, as running all unit tests in a module may take some time
```
jaspTools::testAnalysis("Anova")
```

## Fixing the unit tests
If a test shows up as failed, you should verify why this is and fix it before making a pull request.  
If you made a legitimate change that the test does not cover, then the unit test should be updated.  

You can locate the offending test inside `tests/testthat`; your R console will show the filename and line number -- e.g., `test-jagsmodule.R:56: failure:`.

### Failures
If `testthat` shows that a `failure` occurred then that means the unit test ran okay, but the results do not match. If it is anything but a plot (e.g., a table) the R console will notify you of the exact problem. You should determine why this change occurred and if it is desired. If it is desired, then you can update the unit test.

#### Plots
To view changes in plot tests you should run `jaspTools::manageTestPlots()` (e.g., `manageTestPlots("jagsModule")`). If the change is legitimate you can validate the failing plot in the Shiny application; the reference .svg in the `tests/figs` folder will be updated automatically.

### Errors
If `testthat` shows that an `error` occurred then that means the unit test could not run properly; it terminated too soon. To figure out the cause of termination it is often useful to run the unit test code directly in RStudio. This means going to the test file, locating the unit test and then running everything between `jaspTools::analysisOptions(...)` and `jaspTools::runAnalysis(...)` so see what the output of `runAnalysis()` is.

## Dependencies related to plots
Note that it is very important that all plots are created with equal versions of certain dependencies.  
If this is not the case, then we cannot compare plots across different systems.
The settings used to create plots can be found in `tests/figs/deps.txt` and `tests/figs/jasp-deps.txt`. Both of these files are created automatically by `jaspTools::manageTestPlots()`. It's good practice to look at the dependencies of other modules and to match these on your system.

The files will look something like (but not necessarily the same as):
```
- vdiffr-svg-engine: 1.0
- vdiffr: 0.3.2.2
- freetypeharfbuzz: 0.2.5
```
You must never edit these files directly. If `jaspTools::manageTestPlots()` tells you that you have a newer version of some dependency, then that version will automatically be included in the txt file. All you must do is ensure that the newer version of the dependency does not have any adverse effects (i.e., the Shiny application showing plot changes that are not correct).
