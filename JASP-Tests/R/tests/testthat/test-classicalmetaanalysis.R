context("Binomial Test Bayesian")

test_that("Main table results match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$hypothesis <- "notEqualToTestValue"
  options$priorA <- 4
  options$priorB <- 2
  options$testValue <- 0.2
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["bayesianBinomialTable"]][["data"]]
  expect_equal_tables(table,
    list(4.32337507642424e-15, "contBinom", 58, 0, 0.58, 100, 
         3.43240614623212e-05, "contBinom", 42, 1, 0.42, 100)
  )
})

options <- analysisOptions("ClassicalMetaAnalysis")
options$dependent <- "contNormal"
options$wlsWeights <- "contGamma"
jasptools::run("ClassicalMetaAnalysis", "test.csv",options)
options$regressionCoefficientsCovarianceMatrix <- TRUE
options$forestPlot <- TRUE
options$factors <- "contBinom"
options$modelTerms <- list(
  list(components="contBinom")
)
options$modelTerms
