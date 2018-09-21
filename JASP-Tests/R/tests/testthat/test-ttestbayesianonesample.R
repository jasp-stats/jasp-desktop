context("Bayesian One Sample T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

test_that("Main table results match", {
  options <- jasptools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttestTable"]][["data"]]
  expect_equal_tables(table, list(0.508160332089536, "contNormal", 2.80907441042415e-05))
})

# test_that("Prior posterior plot matches", {
#   set.seed(0)
#   options <- jasptools::analysisOptions("TTestBayesianOneSample")
#   options$variables <- "contNormal"
#   options$plotPriorAndPosterior <- TRUE
#   options$plotPriorAndPosteriorAdditionalInfo <- TRUE
#   results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianOneSample")
# })
#
# test_that("BF robustness check plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianOneSample")
#   options$variables <- "contNormal"
#   options$plotBayesFactorRobustness <- TRUE
#   options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
#   results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianOneSample")
# })
#
# test_that("Sequential analysis plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianOneSample")
#   options$variables <- "contNormal"
#   options$plotSequentialAnalysis <- TRUE
#   options$plotSequentialAnalysisRobustness <- TRUE
#   results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianOneSample")
# })
#
# test_that("Descriptives plot matches", {
#   options <- jasptools::analysisOptions("TTestBayesianOneSample")
#   options$variables <- "contNormal"
#   options$descriptivesPlots <- TRUE
#   options$descriptivesPlotsCredibleInterval <- 0.90
#   results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianOneSample")
# })

test_that("Descriptives table matches", {
  options <- jasptools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["Descriptives"]][["collection"]][["Descriptives_table"]][["data"]]
  expect_equal_tables(table,
    list(100, -0.398760810055084, -0.18874858754, 1.05841360919316, 0.105841360919316, 0.0212636349750834, "contNormal")
  )
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestBayesianOneSample")

  options$variables <- "debInf"
  results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttestTable"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttestTable"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- jasptools::run("TTestBayesianOneSample", "test.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttestTable"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
