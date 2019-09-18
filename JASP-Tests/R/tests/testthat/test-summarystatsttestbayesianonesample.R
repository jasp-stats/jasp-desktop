context("Summary Statistics Bayesian One Sample T-Test")

test_that("Main table results match", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsTTestBayesianOneSample")
  options$tStatistic      <- 2.3
  options$n1Size          <- 23
  options$bayesFactorType <- "LogBF10"
  options$hypothesis      <- "greaterThanTestValue"
  results <- jasptools::run("SummaryStatsTTestBayesianOneSample", "debug.csv", options)
  
  table <- results[["results"]][["oneSampleTTestTable"]][["data"]]
  expect_equal_tables(table, list(1.32450670641619, 2.3, 23, 3.95778537281638e-05, 0.015654342509863))
})

# test_that("Prior posterior plot matches and BF robustness check plot matches", {
#   # with additional info
#   set.seed(0)
#   options <- analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
#   options$tStatistic            <- 2.3
#   options$n1Size                <- 10
#   options$n2Size                <- 13
#   options$bayesFactorType           <- "LogBF10"
#   options$plotPriorAndPosterior     <- TRUE
#   options$plotBayesFactorRobustness <- TRUE
#   results <- jasptools::run("SummaryStatsTTestBayesianIndependentSamples", "debug.csv", options)
#   testPlot <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]
#   expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
#   
#   # without additional info
# })
