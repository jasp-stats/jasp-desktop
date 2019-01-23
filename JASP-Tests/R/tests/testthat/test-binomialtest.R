context("Binomial Test")

test_that("Main table results match", {
  options <- jasptools::analysisOptions("BinomialTest")
  options$variables <- "contBinom"
  options$testValue <- 0.6
  options$hypothesis <- "greaterThanTestValue"
  options$confidenceInterval <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jasptools::run("BinomialTest", "test.csv", options, view = FALSE, quiet = TRUE)
  table <- results[["results"]][["binomialTable"]][["data"]]
  expect_equal_tables(table,
		list(1, 58, 0, 0.4928415, 0.6967399, 0.58, 100, 1, "contBinom", 
				 1, 42, 1, 0.3364797, 0.9999039, 0.42, 100, 1, "contBinom")
  )
})

# test_that("Descriptives plots match", {
#   options <- jasptools::analysisOptions("BinomialTest")
#   options$variables <- "contBinom"
#   options$descriptivesPlots <- TRUE
#   options$descriptivesPlotsConfidenceInterval <- 0.90
#   results <- jasptools::run("BinomialTest", "test.csv", options, view=FALSE, quiet=TRUE)
#
#   testPlot <- results[["state"]][["figures"]][[1]]
#   expect_equal_plots(testPlot, "descriptives-1", dir="BinomialTest")
#
#   testPlot <- results[["state"]][["figures"]][[2]]
#   expect_equal_plots(testPlot, "descriptives-2", dir="BinomialTest")
# })
