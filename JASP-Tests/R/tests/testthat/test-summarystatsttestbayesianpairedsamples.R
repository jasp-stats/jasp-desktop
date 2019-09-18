context("Summary Statistics Bayesian Paired Samples T-Test")

test_that("Main table results match", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsTTestBayesianPairedSamples")
  options$tStatistic      <- 2.3
  options$n1Size          <- 23
  options$bayesFactorType <- "LogBF10"
  results <- jasptools::run("SummaryStatsTTestBayesianPairedSamples", "debug.csv", options)
  
  table <- results[["results"]][["pairedSamplesTTestTable"]][["data"]]
  expect_equal_tables(table, list(1.32450670641619, 2.3, 23, 3.95778537281638e-05, 0.015654342509863))
})