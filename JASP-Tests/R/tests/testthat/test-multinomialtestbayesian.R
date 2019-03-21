context("Multinomial Test Bayesian")

test_that("Main table results match", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "contBinom"
  options$bayesFactorType <- "BF10"
  options$hypothesis <- "multinomialTest"
  options$priorCounts <- list(list(levels = paste0('level', 1:2),
                                   name   = c('Counts'),
                                   values = rep(1, 2)))
  results <- jasptools::run("MultinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["multinomialTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.44414455326433, "Multinomial", 2)
  )
})

test_that("Descriptives plots match", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "contBinom"
  options$countProp <- "descProps"
  options$credibleInterval <- TRUE
  options$descriptivesPlot <- TRUE
  options$priorCounts <- list(list(levels = paste0('level', 1:2),
                                   name   = c('Counts'),
                                   values = rep(1, 2)))
  results <- jasptools::run("MultinomialTestBayesian", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "multinomialBayesianDescriptivesPlot", dir="MultinomialTestBayesian")
})

