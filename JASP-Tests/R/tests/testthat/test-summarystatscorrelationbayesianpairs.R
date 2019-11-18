context("SummaryStatsCorrelationBayesianPairs")
options <- jasptools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
options$n <- 50
options$rObs <- 0.7
options$ci <- TRUE
options$plotPriorPosterior <- TRUE
options$plotBfRobustness <- TRUE
options$kappa <- 1.2
set.seed(1)
results <- jasptools::run("SummaryStatsCorrelationBayesianPairs", "test.csv", options)


test_that("Bayesian Correlation Table results match", {
  table <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_corBayesTable"]][["data"]]
  expect_equal_tables(table,
                      list(973435.048301976, 0.510909781671819, 50, 1.53820662839905e-08,
                           0.7, 0.813457437427968))
})

test_that("Bayes Factor Robustness Check plot matches", {
  plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotBfRobustness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsCorrelationBayesianPairs")
})

test_that("Prior and Posterior plot matches", {
  plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotPriorPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsCorrelationBayesianPairs")
})