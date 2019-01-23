context("Bayesian Independent Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - informed prior Normal/t distributions
# - error handling of plots

test_that("Main table results match", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeCauchyLocation <- 1
  options$informativeCauchyScale <- 0.5
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.123677493243643, 0.0895633315624481))
})

test_that("Prior posterior plot matches", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
})

test_that("BF robustness check plot matches", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianIndependentSamples")
})

test_that("Sequential analysis plot matches", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianIndependentSamples")
})

test_that("Descriptives plot matches", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianIndependentSamples")
})

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 0, 58, -0.120135614827586, 1.10575982846952, 0.145193378675912,
         -0.410880340543859, 0.170609110888686, "TRUE", "contNormal",
         1, 42, -0.283499835571429, 0.994612407217046, 0.15347202634745,
         -0.593442880596763, 0.0264432094539058)
  )
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  obj <- results
  obj$results$errorMessage <- NULL # error message can change and shouldn't tested.
  expect_equal(object = obj, expected = structure(list(
  	status = "error", results = structure(list(title = "error", error = 1L), .Names = c("title", "error"))),
  	.Names = c("status", "results")),
  	label = "Variance check"
  )

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  msg <- results[["results"]][["errorMessage"]]
  expect_true(any(grepl("levels", msg, ignore.case=TRUE)), label = "1-level factor check")
})

test_that("Analysis handles integer overflow", {
  set.seed(4491)
  dat <- data.frame(dependent_var = rnorm(2e5),
                    grouping      = rep(c(1, 2), each = 1e5))

  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- 'dependent_var'
  options$groupingVariable <- 'grouping'
  results <- jasptools::run("TTestBayesianIndependentSamples", dat, options)

  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table, list("dependent_var", 0.00511047418810093, 0.311955453811728))
})
