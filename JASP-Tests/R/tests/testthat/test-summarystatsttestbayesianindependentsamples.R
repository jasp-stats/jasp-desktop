context("Summary Statistics Bayesian Independent Samples T-Test")

test_that("Main table results match", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
  options$tStatistic      <- 2.3
  options$n1Size          <- 10
  options$n2Size          <- 13
  options$bayesFactorType <- "LogBF10"
  results <- jasptools::run("SummaryStatsTTestBayesianIndependentSamples", "debug.csv", options)
  
  table <- results[["results"]][["indSamplesTTestTable"]][["data"]]
  expect_equal_tables(table, list(0.816974431461627, 2.3, 10, 0.000287271873137224, 0.0318022759865702,
                                  13))
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

