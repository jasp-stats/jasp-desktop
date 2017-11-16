context("Bayesian One Sample T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$effectSizeStandardized <- "default"
  options$defaultStandardizedEffectSize <- "cauchy"
  options$priorWidth <- 0.707
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table, list("contNormal", 0.508160332089536, 2.80907441042415e-05))
})

test_that("Prior posterior plot matches", {
  set.seed(0)
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianOneSample")
})

test_that("BF robustness check plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianOneSample")
})

test_that("Sequential analysis plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianOneSample")
})

test_that("Descriptives plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianOneSample")
})

test_that("Descriptives table matches", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")
  options$variables <- "contNormal"
  options$descriptives <- TRUE
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 100, -0.18874858754, 1.05841360919316, 0.105841360919316,
         -0.398760810055083, 0.0212636349750834)
  )
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("TTestBayesianOneSample")

  options$variables <- "debInf"
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$variables <- "debMiss99"
  results <- JASPTools::run("TTestBayesianOneSample", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
