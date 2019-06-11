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
   list("contBinom", 0, 58, 100, 0.58, 0.133210619207213, 0.477119195723914,
    0.678014460645203, "TRUE", "contBinom", 1, 42, 100, 0.42, 0.133210619207213,
    0.321985539354797, 0.522880804276086, "FALSE"))
})
```

`expect_equal_tables` is a wrapper around `testthat::expect_equal` and accepts the same arguments.

In a similar fashion `expect_equal_plots` may be used to test regression of plots.  
This function is a wrapper around `vdiffr::expect_doppelganger` (for more information about vdffir see their [github page](https://github.com/lionel-/vdiffr)).
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

#### Dependencies related to plots
Note that it is very important that all plots are created with equal versions of certain dependencies.  
If this is not the case, then we cannot compare plots across different systems.  
The settings you must use can be found in [figs/deps.txt](https://github.com/jasp-stats/jasp-desktop/blob/development/JASP-Tests/R/tests/figs/deps.txt). Note that these should be automatically installed with `vdiffr`.
This file will look something like (but not necessarily the same as)
```
- vdiffr-svg-engine: 1.0
- vdiffr: 0.3.0
- freetypeharfbuzz: 0.2.5
```
You must never edit deps.txt unless the travis-CI config is updated as well.
If you have different versions of the dependencies installed the plots will be skipped.
In this case either update your configuration or rely on the automatic test that start when you make a pull request.
