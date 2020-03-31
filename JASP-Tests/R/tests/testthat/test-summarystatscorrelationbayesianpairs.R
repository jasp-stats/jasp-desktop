context("SummaryStatsCorrelationBayesianPairs")
# Kendall
options <- jasptools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
options$n <- 50
options$rObs <- 0.7
options$tauObs <- -0.3
options$ci <- TRUE
options$ciValue <- 0.69
options$plotPriorPosterior <- TRUE
options$plotBfRobustness <- TRUE
options$kappa <- 1.2
options$method <- "kendall"
options$alternative <- "greater"
options$bayesFactorType <- "BF01"
set.seed(1)
results <- jasptools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)


test_that("Bayesian Correlation Table results match", {
  table <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_corBayesTable"]][["data"]]
  expect_equal_tables(table,
                      list(25.3830367605608, 0.0045022511255628, 50, 0.998944254496665, -0.3,
                           0.0500250125062531))
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