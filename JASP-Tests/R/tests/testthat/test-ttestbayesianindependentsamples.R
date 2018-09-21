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
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttestTable"]][["data"]]
  # expect_equal_tables(table, list("contNormal", 0.123677493243643, 0.0895633315624481))
  expect_equal_tables(table, list(0.0280859509755459, 0.0625753671100039, "contNormal")
)
})

# test_that("Prior posterior plot matches", {
#   set.seed(0)
#   options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$plotPriorAndPosterior <- TRUE
#   options$plotPriorAndPosteriorAdditionalInfo <- TRUE
#   results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
# })
#
# test_that("BF robustness check plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$plotBayesFactorRobustness <- TRUE
#   options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
#   results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianIndependentSamples")
# })
#
# test_that("Sequential analysis plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$plotSequentialAnalysis <- TRUE
#   options$plotSequentialAnalysisRobustness <- TRUE
#   results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianIndependentSamples")
# })
#
# test_that("Descriptives plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
#   options$variables <- "contNormal"
#   options$groupingVariable <- "contBinom"
#   options$descriptivesPlots <- TRUE
#   options$descriptivesPlotsCredibleInterval <- 0.90
#   results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianIndependentSamples")
# })

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Descriptives"]][["collection"]][["Descriptives_table"]][["data"]]
  expect_equal_tables(table,
    list(58, 0, -0.410880340543859, -0.120135614827586, 1.10575982846952,
  			 0.145193378675912, 0.170609110888686, "contNormal", 42, 1, -0.593442880596763,
  			 -0.283499835571428, 0.994612407217046, 0.15347202634745, 0.0264432094539058,
  			 "contNormal")
  )
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttestTable"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  expect_equal(object = results[["status"]], expected = "error",
  	label = "Variance check"
  )
  expect_false(startsWith(results[["results"]][["errorMessage"]], "This analysis terminated unexpectedly."))

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttestTable"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options, view=FALSE, quiet=TRUE)
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
  results <- jasptools::run("TTestBayesianIndependentSamples", dat, options, view=FALSE, quiet=TRUE)

  table <- results[["results"]][["ttestTable"]][["data"]]
  expect_equal_tables(table, list(0.00511047419082252, 0.311956739823362, "dependent_var"))
})
