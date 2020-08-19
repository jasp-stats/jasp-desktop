Unit Tests
==========

This guide will first explain how to setup the folder structure in your module to allow testing.
Afterwards the method of creating and running tests is explained.

## Folder structure
Your module should include the following structure:
- ModuleName/
  - tests/
    - [figs/](#figs)
    - [testthat/](#testthat)
    - [testthat.R](#testthatr)
  - [travis.yml](#travisyml)
  
#### figs
Can be an empty folder, will be automatically filled as described in [Creating tests](#Creatingtests).

#### testthat
Can be an empty folder until you add tests, at that point it will contain a number of test-<analysisName>.R files as described in [Creating tests](#Creatingtests).

#### testthat.R:
```
library(jaspTools)
library(testthat)

jaspTools::runTestsTravis(module = getwd())
```

#### travis.yml:
```
os: osx
language: r
r: 3.6.1
before_install: 
  - "git clone --branch=MacOS-Original https://github.com/jasp-stats/jasp-required-files.git ~/pkgs"
install:
  - RScript -e ".libPaths(c(.libPaths(), '~/pkgs')); install.packages('remotes'); remotes::install_github('jasp-stats/jaspTools')"
script:
  - R < tests/testthat.R --no-save
env:
  global:
    - R_REMOTES_NO_ERRORS_FROM_WARNINGS=true
    - VDIFFR_RUN_TESTS=true
    - REQUIRED_PKGS=~/pkgs

```

## Creating and running unit tests

### Obtaining jaspTools
To run tests in a module, the R package jaspTools is required. 
Click [here](https://github.com/jasp-stats/jaspTools) for instructions regarding installing and running jaspTools.

### Creating tests
The analysis tests in modules are based on the R package testthat.  
For more information about this package see [this online book chapter by Hadley Wickam](http://r-pkgs.had.co.nz/tests.html).
It's advised to learn about testthat before continuing with this guide.

#### Creating a test file
Each test file will be build up as follows:
-- They contain an analysis specific context and consists of tests that check that specific analysis.  
--- Each test in a file checks a specific expectation of a small portion of analysis functionality (e.g., a table, a plot, error handling, etc.)

To start you can create an empty R file titled test-<analysisName>.R in the testthat folder (e.g., test-Anova.R).
The first line should read `context("analysisName")`.

#### Adding checks
You can then start adding checks to it, for which there are two methods. The manual one and the - somewhat - automatic one.

##### Manually
The testthat package offers a number of expectations useful for testing.  
JASP offers two additional expectations:
```
expect_equal_tables(test, ref, ...)
expect_equal_plots(test, name, dir)
```
`expect_equal_tables` takes the data of a JASP table list and compares it to a reference list.  
This reference list can be created by supplying a table to jaspTools.
```
setPkgOption("module.dirs" = "path/to/jaspFrequencies")

options <- jaspTools::analysisOptions("BinomialTest")
options[["variables"]] <- "contBinom"
results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
table <- results[["results"]][["binomial"]][["data"]]
jaspTools::makeTestTable(table)
```
The above returns the output

`list("contBinom", 0, 58, 100, 0.58, 0.133210619207213, 0.477119195723914,  
 0.678014460645203, "TRUE", "contBinom", 1, 42, 100, 0.42, 0.133210619207213,  
 0.321985539354797, 0.522880804276086, "FALSE")`

We can now write the expectation
```
test_that("Binomial table results match", {
  options <- jaspTools::analysisOptions("BinomialTest")
  options[["variables"]] <- "contBinom"
  results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
  table <- results[["results"]][["binomial"]][["data"]]
  jaspTools::expect_equal_tables(table,
		 list(1.36997960729505, 58, 0, 0.477119195723914, 0.133210619207213,
			  0.58, 100, 0.678014460645203, "contBinom", 1.36997960729505,
			  42, 1, 0.321985539354797, 0.133210619207213, 0.42, 100, 0.522880804276086,
			  "contBinom"))
})
```

In a similar fashion `expect_equal_plots` may be used to test regression of plots.  
This function is a wrapper around `vdiffr::expect_doppelganger` (for more information about vdiffr see their [github page](https://github.com/lionel-/vdiffr)).
The function takes a plot object or recorded plot and compares it to a stored .svg file.  
This results in an expectation like
```
test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  results <- jaspTools::runAnalysis("TTestIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  jaspTools::expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
})
```
To validate a plot for a newly created test, run
```
manageTestPlots("TTestIndependentSamples")
```
This function starts a Shiny application that allows you to view and then validate your new plot.  
Validating a plot places it in figs/analysisName.

As noted earlier, testthat offers a number of expectations as well.  
You should use whatever is most suitable for the situation.

#### Automatically
Of course, the above set of steps might be a bit tedious to perform for every table and plot. There are two things you can do to make your life easier. Firstly, you can let `jaspTools::runAnalysis()` take care of making expectations by setting `makeTests=TRUE`. So if we take the example from the manual section, this would yield
```
  setPkgOption("module.dirs" = "path/to/jaspFrequencies")
  options <- jaspTools::analysisOptions("BinomialTest")
  options[["variables"]] <- "contBinom"
  jaspTools::runAnalysis("BinomialTest", "test.csv", options, makeTests=TRUE)
```
Which gives the expectation as the output:

```
test_that("Binomial Test table results match", {
	options <- jaspTools::analysisOptions("BinomialTest")
	options$variables <- "contBinom"
	set.seed(1)
	results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
	table <- results[["results"]][["binomialTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.36997960729505, 58, 0, 0.477119195723914, 0.133210619207213,
			 0.58, 100, 0.678014460645203, "contBinom", 1.36997960729505,
			 42, 1, 0.321985539354797, 0.133210619207213, 0.42, 100, 0.522880804276086,
			 "contBinom"))
})
```

A second thing we can do to make our life's easier is to use JASP to create the options for us. These options can then be supplied to `jaspTools::analysisOptions()`. To get the options from JASP. Open your JASP executable, navigate to your analysis of choice and set all options as you would like them for your unit tests. You can create multiple analyses at once. Now store your analyses as a .jasp file.
Head back to RStudio and supply the path of the .jasp file to `jaspTools::analysisOptions()`. Afterwards you can tell jaspTools to use these options to create unit tests. Your syntax should look like the following:
```
options <- jaspTools::analysisOptions("path/to/file.jasp")

# if there is a single analysis in the .jasp file you can run
jaspTools::runAnalysis(options=options, dataset="test.csv", makeTests=TRUE)

# if there are multiple analyses, you need to subset them one at a time
jaspTools::runAnalysis(options=options[[1]], dataset="test.csv", makeTests=TRUE)
jaspTools::runAnalysis(options=options[[2]], dataset="test.csv", makeTests=TRUE)
```

You will be able to copy-paste the output directly into a test-file. You can do this for how ever many `jaspTools::runAnalysis()`'s to get a good coverage. Just make sure that (1) each expectation title somewhat makes sense (e.g., `"Binomial Test table results match"` makes sense but `"1 plot matches"` could use some work), and (2) that the first line of the test-file defines the analysis specific context. This literally means having this as the first line of your testfile: `context("analysis name")` -- if this is not the case then testthat might throw an error.

As explained under the manual method, to validate the new plots you should run:
```
jaspTools::manageTestPlots()
```

### Running the tests
-----------------
To run all tests in a module, type
```
setPkgOption("module.dirs" = "path/to/jaspAnova")
testAll()
```
Note that if you include several module.dirs, then all tests in all of these modules will be run.

Any test that fails is shown in the console.  
Warnings may be ignored, but should be minimized.

It is also possible to test a specific analysis, as running all unit tests may take some time
```
setPkgOption("module.dirs" = "path/to/jaspAnova")
testAnalysis("Anova")
```

Fixing the tests
----------------
If a test shows up as failed, you should verify why this is and fix it before making a pull request.  
If you made a legitimate change that the test does not cover, then the unit test should be updated.  
Locate it in your testthat and change the offending test.  
Note that if the failed test was related to plotting, then you may use
```
manageTestPlots("Anova")
```
to inspect differences between the saved reference plot and the failing plot.  
If you validate the failing plot (because it was a legitimate change), it will replace the reference plot in the figs folder.
Please ensure that there are no changes to figs/deps.txt as a result of your change (see the section below about "Dependencies related to plots").


#### Dependencies related to plots
Note that it is very important that all plots are created with equal versions of certain dependencies.  
If this is not the case, then we cannot compare plots across different systems.
The settings used to create plots can be found in figs/deps.txt and figs/jasp-deps.txt. It's good practice to look at the dependencies of other modules and to match these on your system. The former should be automatically installed with `vdiffr`. The latter should be automatically installed when you first build JASP.
The files will look something like (but not necessarily the same as)
```
- vdiffr-svg-engine: 1.0
- vdiffr: 0.3.2.2
- freetypeharfbuzz: 0.2.5
```
You must never edit these files directly. If `jaspTools::manageTestPlots()` tells you that you have a newer version of some dependency, then that version will automatically be included in .the txt file. All you must do is ensure that the newer version of the dependency does not have any adverse effects (i.e., the Shiny application showing plot changes that are not correct).
