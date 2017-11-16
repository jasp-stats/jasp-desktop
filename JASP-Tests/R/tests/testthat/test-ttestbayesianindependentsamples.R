context("Bayesian Independent Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - informed prior Normal/t distributions
# - error handling of plots

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeCauchyLocation <- 1
  options$informativeCauchyScale <- 0.5
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.123677493243643, 0.0895633315624481))
})

test_that("Prior posterior plot matches", {
  set.seed(0)
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
})

test_that("BF robustness check plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianIndependentSamples")
})

test_that("Sequential analysis plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianIndependentSamples")
})

test_that("Descriptives plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianIndependentSamples")
})

test_that("Descriptives table matches", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 58, -0.120135614827586, 1.10575982846952, 0.145193378675912,
         -0.410880340543859, 0.170609110888686, "TRUE", "contNormal",
         1, 42, -0.283499835571429, 0.994612407217046, 0.15347202634745,
         -0.593442880596763, 0.0264432094539058)
  )
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("TTestBayesianIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- JASPTools::run("TTestBayesianIndependentSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  expect_is(results, "expectedError", label="1-level factor check")
})
