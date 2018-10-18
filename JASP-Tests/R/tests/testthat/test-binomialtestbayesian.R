context("Binomial Test Bayesian")

test_that("Main table results match", {
  options <- jasptools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$hypothesis <- "notEqualToTestValue"
  options$priorA <- 4
  options$priorB <- 2
  options$testValue <- 0.2
  results <- jasptools::run("BinomialTestBayesian", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["binomial"]][["data"]]
  expect_equal_tables(table,
    list("contBinom", 0, 58, 100, 0.58, 4.32337507642424e-15, "TRUE", "contBinom",
         1, 42, 100, 0.42, 3.43240614623212e-05, "FALSE")
  )
})

# test_that("Prior posterior plots match", {
#   options <- jasptools::analysisOptions("BinomialTestBayesian")
#   options$variables <- "contBinom"
#   options$plotPriorAndPosterior <- TRUE
#   options$plotPriorAndPosteriorAdditionalInfo <- TRUE
#   results <- jasptools::run("BinomialTestBayesian", "test.csv", options, view=FALSE, quiet=TRUE)
#
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "prior-posterior-1", dir="BinomialTestBayesian")
#
#   testPlot <- results[["state"]][["figures"]][[2]]
#   expect_equal_plots(testPlot, "prior-posterior-2", dir="BinomialTestBayesian")
# })
# 
# test_that("Sequential analysis plots match", {
#   options <- jasptools::analysisOptions("BinomialTestBayesian")
#   options$variables <- "contBinom"
#   options$plotSequentialAnalysis <- TRUE
#   results <- jasptools::run("BinomialTestBayesian", "test.csv", options, view=FALSE, quiet=TRUE)
#
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "sequential-analysis-1", dir="BinomialTestBayesian")
#
#   testPlot <- results[["state"]][["figures"]][[2]]
#   expect_equal_plots(testPlot, "sequential-analysis-2", dir="BinomialTestBayesian")
# })
