context("SummaryStatsCorrelationBayesianPairs")
## Kendall

test_that("Kendall Bayesian Correlation Table results match", {
  options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
  options$method <- "kendall"
  options$n <- 50
  options$tauObs <- -0.3
  options$ci <- TRUE
  options$ciValue <- 0.69
  options$kappa <- 1.2
  options$alternative <- "greater"
  options$bayesFactorType <- "BF01"
  
  set.seed(1)
  results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
  
  table <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_corBayesTable"]][["data"]]
  expect_equal_tables(table,
                      list(25.3830367605608, 0.0045022511255628, 50, 0.998944254496665, -0.3,
                           0.0500250125062531))
})

test_that("Kendall Bayes Factor Robustness Check plot matches", {
  options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
  options$method <- "kendall"
  options$n <- 50
  options$tauObs <- -0.3
  options$ci <- TRUE
  options$ciValue <- 0.69
  options$kappa <- 1.2
  options$alternative <- "greater"
  options$bayesFactorType <- "BF01"
  options$plotBfRobustness <- TRUE
  
  set.seed(1)
  results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
  
  plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotBfRobustness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsCorrelationBayesianPairs")
})

test_that("Kendall Prior and Posterior plot matches", {
  options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
  options$method <- "kendall"
  options$n <- 50
  options$tauObs <- -0.3
  options$ci <- TRUE
  options$ciValue <- 0.69
  options$kappa <- 1.2
  options$alternative <- "greater"
  options$bayesFactorType <- "BF01"
  options$plotPriorPosterior <- TRUE
  
  set.seed(1)
  results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
  
  plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotPriorPosterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsCorrelationBayesianPairs")
})


# context("SummaryStatsCorrelationBayesianPairs")
# ## Pearson 
# test_that("Pearson Bayesian Correlation Table results match", {
#   options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
#   options$method <- "pearson"
#   options$n <- 120
#   options$rObs <- 0.7
#   options$ci <- TRUE
#   options$ciValue <- 0.42
#   options$kappa <- 2
#   options$bayesFactorType <- "LogBF10"
#   
#   set.seed(1)
#   results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
#   
#   table <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_corBayesTable"]][["data"]]
#   expect_equal_tables(table,
#                       list(36.7814001246542, 0.667382482991797, 120, 5.79337195224991e-19,
#                            0.7, 0.720332241216609))
# })
# 
# 
# 
# 
# test_that("Pearson Bayes Factor Robustness Check plot matches", {
#   options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
#   options$method <- "pearson"
#   options$n <- 120
#   options$rObs <- 0.7
#   options$ci <- TRUE
#   options$ciValue <- 0.42
#   options$kappa <- 2
#   options$bayesFactorType <- "LogBF10"
#   options$plotBfRobustness <- TRUE
#   
#   set.seed(1)
#   results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
#   
#   plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotBfRobustness"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   expect_equal_plots(testPlot, "bayes-factor-robustness-check", dir="SummaryStatsCorrelationBayesianPairs")
# })
# 
# test_that("Pearson Prior and Posterior plot matches", {
#   options <- jaspTools::analysisOptions("SummaryStatsCorrelationBayesianPairs")
#   options$method <- "pearson"
#   options$n <- 120
#   options$rObs <- 0.7
#   options$ci <- TRUE
#   options$ciValue <- 0.42
#   options$kappa <- 2
#   options$bayesFactorType <- "LogBF10"
#   options$plotPriorPosterior <- TRUE
#   
#   set.seed(1)
#   results <- jaspTools::run("SummaryStatsCorrelationBayesianPairs", "debug.csv", options)
#   
#   plotName <- results[["results"]][["correlationContainer"]][["collection"]][["correlationContainer_plotContainer"]][["collection"]][["correlationContainer_plotContainer_plotPriorPosterior"]][["data"]]
#   testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
#   expect_equal_plots(testPlot, "prior-and-posterior", dir="SummaryStatsCorrelationBayesianPairs")
# })