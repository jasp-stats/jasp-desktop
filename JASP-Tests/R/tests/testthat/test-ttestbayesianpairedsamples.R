context("Bayesian Paired Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - default cauchy and informed prior cauchy/Normal
# - error handling of plots

test_that("Main table results match", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeStandardizedEffectSize <- "t"
  options$informativeTLocation <- 0.2
  options$informativeTScale <- 0.5
  options$informativeTDf <- 2
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["ttest"]][["data"]]
  expect_equal_tables(table, list("contNormal", "-", "contGamma", 293915424756.037, 1.32616895200622e-19))
})

test_that("Prior posterior plot matches", {
  set.seed(0)
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianPairedSamples")
})

test_that("BF robustness check plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianPairedSamples")
})

test_that("Sequential analysis plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianPairedSamples")
})

test_that("Descriptives plot matches", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  testPlot <- results[["state"]][["figures"]][[1]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianPairedSamples")
})

test_that("Descriptives table matches", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")
  options$pairs <- list(c("contNormal", "contGamma"))
  options$descriptives <- TRUE
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["descriptives"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
    list("contNormal", 100, -0.18874858754, 1.05841360919316, 0.105841360919316,
         -0.398760810055083, 0.0212636349750834, "contGamma", 100, 2.03296079621,
         1.53241112621044, 0.153241112621044, 1.72889718286736, 2.33702440955264)
  )
})

test_that("Analysis handles errors", {
  options <- JASPTools::analysisOptions("TTestBayesianPairedSamples")

  options$pairs <- list(c("contNormal", "debInf"))
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$pairs <- list(c("contNormal", "debSame"))
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("variance", notes, ignore.case=TRUE)), label = "No variance check")

  options$pairs <- list(c("contNormal", "debMiss99"))
  results <- JASPTools::run("TTestBayesianPairedSamples", "debug.csv", options, view=FALSE, quiet=TRUE)
  notes <- unlist(results[["results"]][["ttest"]][["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")
})
