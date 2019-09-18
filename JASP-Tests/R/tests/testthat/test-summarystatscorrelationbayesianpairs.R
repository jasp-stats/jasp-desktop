context("Summary Statistics Bayesian Correlation Pairs")

test_that("Main table results match", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsCorrelationBayesianPairs")
  options$correlationCoefficient <- "pearsonRho"
  options$sampleSize             <- 51
  options$pearsonRhoValue        <- 0.3
  options$priorWidth             <- 2
  options$bayesFactorType        <- "LogBF10"
  options$hypothesis             <- "correlated"
  results <- jasptools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
  
  table <- results[["results"]][["correlationTable"]][["data"]]
  expect_equal_tables(table, list(0.0801325678537714, 0.3, 51, 0.0324477786167482))
})