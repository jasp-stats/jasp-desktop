Unit Tests
==========

Running the tests
-----------------

Currently, the analysis unit tests are not integrated into the JASP-Tests app.  
To run the tests, the R package [jasptools](https://github.com/jasp-stats/jasptools) is required. 
This package is included with your clone of the jasp-desktop repository.
However, before you install it, make sure you install jaspResults first.

To install jaspResults:
- `install.packages("/path/to/jasp-desktop/JASP-R-Interface/jaspResults", type="source", repos=NULL)`

To install and use jasptools type:
- `install.packages("/path/to/jasp-desktop/Tools/jasptools", type="source", repos=NULL)`
- `library(jasptools)`
- `develop("path/to/jasp-desktop")`

To run all tests type
```
testAll()
```

Any test that fails is shown in the console.  
Warnings may be ignored, but should be minimized.

It is also possible to test a specific analysis, as running all unit tests may take some time
```
testAnalysis("Anova")
```

Fixing the tests
----------------
If a test shows up as failed, you should verify why this is and fix it before making a pull request.  
If you made a legitimate change that the test does not cover, then the unit test should be updated.  
Locate it under [JASP-Tests/R/tests/testthat](https://github.com/jasp-stats/jasp-desktop/tree/development/JASP-Tests/R/tests/testthat) and change the offending test.  
Note that if the failed test was related to plotting, then you may use
```
manageTestPlots("Anova")
```
to inspect differences between the saved reference plot and the failing plot.  
If you validate the failing plot (because it was a legitimate change), it will replace the reference plot in the figs folder.
Please ensure that there are no changes to figs/deps.txt as a result of your change (see the section below about "Dependencies related to plots").

Writing new tests
-----------------
The analysis tests in JASP are based on the R package testthat.  
For more information about this package see [this online book chapter by Hadley Wickam](http://r-pkgs.had.co.nz/tests.html).

The general structure of the tests is as follows:  
\- A folder titled testthat contains a number of test-analysisName.R files.  
-- Each test file has an analysis specific context and consists of tests that check that specific analysis.  
--- Each test in a file checks a specific expectation of a small portion of analysis functionality (e.g., a table, a plot, error handling, etc.)

It is possible to automatically generate the tests, but we'll first show how to do it manually.

#### Manually
The testthat package offers a number of expectations useful for testing.  
JASP offers two additional expectations:
```
expect_equal_tables(test, ref, ...)
expect_equal_plots(test, name, dir)
```
`expect_equal_tables` takes the data of a JASP table list and compares it to a reference list.  
This reference list can be created by supplying a table to jasptools.
```
options <- jasptools::analysisOptions("BinomialTest")
options[["variables"]] <- "contBinom"
results <- jasptools::run("BinomialTest", "debug.csv", options)
table <- results[["results"]][["binomial"]][["data"]]
jasptools::makeTestTable(table)
```
The above returns the output

`list("contBinom", 0, 58, 100, 0.58, 0.133210619207213, 0.477119195723914,  
 0.678014460645203, "TRUE", "contBinom", 1, 42, 100, 0.42, 0.133210619207213,  
 0.321985539354797, 0.522880804276086, "FALSE")`

We can now write the expectation
```
test_that("Binomial table results match", {
  options <- jasptools::analysisOptions("BinomialTest")
  options[["variables"]] <- "contBinom"
  results <- jasptools::run("BinomialTest", "debug.csv", options)
  table <- results[["results"]][["binomial"]][["data"]]
  expect_equal_tables(table,
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
  options <- jasptools::analysisOptions("TTestIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  results <- jasptools::run("TTestIndependentSamples", "debug.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestIndependentSamples")
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
Of course, the above set of steps might be a bit tedious to perform for every table and plot. There are two things you can do to make your life easier. Firstly, you can let `jasptools::run()` take care of making expectations by setting `makeTests=TRUE`. So if we take the example from the manual section, this would yield
```
  options <- jasptools::analysisOptions("BinomialTest")
  options[["variables"]] <- "contBinom"
  results <- jasptools::run("BinomialTest", "debug.csv", options, makeTests=TRUE)
```
Which gives the expectation as the output:

```
test_that("Binomial Test table results match", {
	options <- jasptools::analysisOptions("BinomialTest")
	options$variables <- "contBinom"
	set.seed(1)
	results <- jasptools::run("BinomialTest", "debug.csv", options)
	table <- results[["results"]][["binomialTable"]][["data"]]
	expect_equal_tables(table,
		list(1.36997960729505, 58, 0, 0.477119195723914, 0.133210619207213,
			 0.58, 100, 0.678014460645203, "contBinom", 1.36997960729505,
			 42, 1, 0.321985539354797, 0.133210619207213, 0.42, 100, 0.522880804276086,
			 "contBinom"))
})
```

A second thing we can do to make our life's easier is to use JASP to create the options for us. These options can then be supplied to `jasptools::analysisOptions()`. To get the options from JASP you first need to build it in debug mode from QtCreator. Once it is built, navigate to your analysis of choice and set all options as you would like them for your unit tests. Proceed to the "Application Output" in QtCreator and search for the last occurrence of `Engine::receiveAnalysisMessage:`. Now copy the json that follows after `Engine::receiveAnalysisMessage:` and make sure to include both `{` and `}` that surround the analysis call. The json should look something like
```
{
   "dynamicModuleCall" : "",
   "id" : 13,
   "imageBackground" : "white",
   "jaspResults" : true,
   "name" : "BinomialTest",
   "options" : {
      "VovkSellkeMPR" : true,
      "confidenceInterval" : true,
      "confidenceIntervalInterval" : 0.950,
      "descriptivesPlots" : true,
      "descriptivesPlotsConfidenceInterval" : 0.950,
      "hypothesis" : "notEqualToTestValue",
      "plotHeight" : 320,
      "plotWidth" : 480,
      "testValue" : 0.50,
      "variables" : [ "contBinom" ]
   },
   "perform" : "run",
   "ppi" : 192,
   "revision" : 4,
   "rfile" : "",
   "title" : "Binomial Test",
   "typeRequest" : "analysis"
}
```
Now head back to RStudio and supply this json string to `jasptools::analysisOptions()` -- make sure to use single quotes when doing so! Afterwards you can tell jasptools to use these options to create unit tests. Your syntax should look like the following:
```
options <- analysisOptions('{
   "dynamicModuleCall" : "",
   "id" : 13,
   "imageBackground" : "white",
   "jaspResults" : true,
   "name" : "BinomialTest",
   "options" : {
      "VovkSellkeMPR" : true,
      "confidenceInterval" : true,
      "confidenceIntervalInterval" : 0.950,
      "descriptivesPlots" : true,
      "descriptivesPlotsConfidenceInterval" : 0.950,
      "hypothesis" : "notEqualToTestValue",
      "plotHeight" : 320,
      "plotWidth" : 480,
      "testValue" : 0.50,
      "variables" : [ "contBinom" ]
   },
   "perform" : "run",
   "ppi" : 192,
   "revision" : 4,
   "rfile" : "",
   "title" : "Binomial Test",
   "typeRequest" : "analysis"
}')
jasptools::run(options=options, dataset="debug.csv", makeTests=TRUE)
```

You will be able to copy-paste the output directly in a test-file. You can do this for how ever many `jasptools::run()`'s to get a good coverage. Just make sure that (1) each expectation title somewhat makes sense (e.g., `"Binomial Test table results match"` makes sense but `"1 plot matches"` could use some work), and (2) that the first line of the test-file defines the analysis specific context. This literally means having this as the first line of your testfile: `context("analysis name")` -- if this is not the case then testthat might throw an error.

#### Dependencies related to plots
Note that it is very important that all plots are created with equal versions of certain dependencies.  
If this is not the case, then we cannot compare plots across different systems.  
The settings you must use can be found in [figs/deps.txt](https://github.com/jasp-stats/jasp-desktop/blob/development/JASP-Tests/R/tests/figs/deps.txt) and [figs/jasp-deps.txt](https://github.com/jasp-stats/jasp-desktop/blob/development/JASP-Tests/R/tests/figs/jasp-deps.txt). The former should be automatically installed with `vdiffr`. The latter should be automatically installed when you first build JASP.
The files will look something like (but not necessarily the same as)
```
- vdiffr-svg-engine: 1.0
- vdiffr: 0.3.0
- freetypeharfbuzz: 0.2.5
```
You must never edit these files directly. If `jasptools::manageTestPlots()` tells you that you have a newer version of some dependency, then that version will automatically be included in .the txt file. All you must do is ensure that the newer version of the dependency does not have any adverse effects (i.e., the Shiny application showing plot changes that are not correct).
