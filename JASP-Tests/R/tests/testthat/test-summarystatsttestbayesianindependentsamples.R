context("SummaryStatsTTestBayesianIndependentSamples")

options <- jasptools::analysisOptions("SummaryStatsTTestBayesianIndependentSamples")
options$bayesFactorType <- "BF01"
options$n1Size <- 15
options$n2Size <- 20
options$plotBayesFactorRobustness <- TRUE
options$plotPriorAndPosterior <- TRUE
options$tStatistic <- 2
set.seed(1)
results <- jasptools::run("SummaryStatsTTestBayesianIndependentSamples", "test.csv", options)


test_that("Bayesian Independent Samples T-Test table results match", {
  table <- results[["results"]][["table"]][["data"]]
  expect_equal_tables(table,
                      list(0.677140094220032, 2, 15, 0.00289871404159881, 0.0537862318825462,
                           20))
})

test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["inferentialPlots"]][["PriorPosteriorPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsTTestBayesianIndependentSamples")
})

test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["inferentialPlots"]][["BFrobustnessPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsTTestBayesianIndependentSamples")
})