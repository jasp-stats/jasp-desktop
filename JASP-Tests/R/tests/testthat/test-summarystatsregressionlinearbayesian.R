context("Summary Statistics Bayesian Linear Regression")


getMainTable <- function(results)
  results[["results"]][["mainContainer"]][["collection"]][["mainContainer_regressionTable"]][["data"]]

getRobustnessPlot <- function(results) {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotBayesFactorRobustness"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  return(testPlot)
}
  

test_that("Main table results match with one model", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$sampleSize                    <- 51
  options$numberOfCovariatesNull        <- 0
  options$unadjustedRSquaredNull        <- 0
  options$numberOfCovariatesAlternative <- 4
  options$unadjustedRSquaredAlternative <- 0.82
  options$priorWidth                    <- 0.5
  options$bayesFactorType               <- "LogBF10"
  results <- jaspTools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)
  
  table <- getMainTable(results)
  # fill in expected values
  expect_equal_tables(table, list(31.7268858756927, 0.82, 1.40664186301779e-06, 4, 51))
})

test_that("Main table results match with model comparison", {
  set.seed(0)
  options <- analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$sampleSize                    <- 51
  options$numberOfCovariatesNull        <- 2
  options$unadjustedRSquaredNull        <- 0.62
  options$numberOfCovariatesAlternative <- 4
  options$unadjustedRSquaredAlternative <- 0.82
  options$priorWidth                    <- 0.5
  options$bayesFactorType               <- "LogBF10"
  results <- jaspTools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)
  
  table <- getMainTable(results)
  # fill in expected values
  expect_equal_tables(table, list(-12.9889, 0.62, 2.01408039564443e-06, 2, "Null model",
                                  12.9889, 0.82, 1.40664186301779e-06, 4, "Alternative model"
                                  ))
})

test_that("Main table and plots match without additional info", {
  options <- jaspTools::analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$bayesFactorType <- "LogBF10"
  options$numberOfCovariatesAlternative <- 5
  options$numberOfCovariatesNull <- 4
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  options$sampleSize <- 750
  options$unadjustedRSquaredAlternative <- 0.55
  options$unadjustedRSquaredNull <- 0.45
  set.seed(1)
  results <- jaspTools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)

  table <- getMainTable(results)
  expect_equal_tables(table,
                      list(-71.56048, 0.45, 6.09707810829571e-07, 4, "Null model", 
                           71.56048, 0.55, 9.79848723927088e-06, 5, "Alternative model"))

  testPlot <- getRobustnessPlot(results)
  expect_equal_plots(testPlot, "robustness-plot", dir="SummaryStatsRegressionLinearBayesian")

})

test_that("Main table and plots match with additional info", {
  options <- jaspTools::analysisOptions("SummaryStatsRegressionLinearBayesian")
  options$plotBayesFactorRobustness <- TRUE
  set.seed(1)
  results <- jaspTools::run("SummaryStatsRegressionLinearBayesian", "debug.csv", options)
  
  table <- getMainTable(results)
  expect_equal_tables(table,
                      list(0.651407436025229, 0, 2.50272800102564e-05, 1, 3))
  
  testPlot <- getRobustnessPlot(results)
  expect_equal_plots(testPlot, "robustness-additional-info-plot", dir="SummaryStatsRegressionLinearBayesian")
  
})


