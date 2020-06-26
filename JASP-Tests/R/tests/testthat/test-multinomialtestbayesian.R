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

test_that("Descriptives table results match", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "debString"
  options$countProp <- "descProps"
  options$credibleInterval <- TRUE
  options$descriptives <- TRUE
  options$credibleIntervalInterval <- 0.10
  options$priorCounts <- list(list(levels =letters,
                                   name   = c('Counts'),
                                   values = rep(1, length(letters))))
  results <- jasptools::run("MultinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["multinomialDescriptivesTable"]][["data"]]
  expect_equal_tables(table[[1]],
                      list(0.0384615384615385, "a", 0.0439729779027027, 0.05, 0.0594407448532589)
  )
})

test_that("Descriptives plots match", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "contBinom"
  options$countProp <- "descProps"
  options$credibleInterval <- TRUE
  options$descriptivesPlot <- TRUE
  options$priorCounts <- list(list(levels = c("0", "1"),
                                   name   = c('Counts'),
                                   values = rep(1, 2)))
  results <- jasptools::run("MultinomialTestBayesian", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "multinomialBayesianDescriptivesPlot", dir="MultinomialTestBayesian")
})

test_that("Bayesian Multinomial Test table results match in short data format", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "Month"
  options$counts <- "Stress.frequency"
  options$tableWidget <- list(list(levels = c("1", "2", "3", "4", "5", "6", "7", "8", 
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (a)", 
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                              1, 1)))
  options$priorCounts <- list(list(levels = c("1", "2", "3", "4", "5", "6", "7", "8", 
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "Counts", 
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                              1, 1)))
  set.seed(1)
  results <- jasptools::run("MultinomialTestBayesian", "Memory of Life Stresses.csv", options)
  table <- results[["results"]][["multinomialTable"]][["data"]]
  expect_equal_tables(table,
                      list(27.1062505863656, "Multinomial", 18))
})

test_that("Descriptives table correctly shows reordered factor levels", {
  options <- jasptools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "Month"
  options$counts <- "Stress.frequency"
  options$exProbVar <- "Expected.counts"
  options$descriptives <- TRUE
  options$tableWidget <- list(list(levels = c("3", "1", "2", "4", "5", "6", "7", "8", 
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (a)", 
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                              1, 1)))
  options$priorCounts <- list(list(levels = c("3", "1", "2", "4", "5", "6", "7", "8", 
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "Counts", 
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                              1, 1)))
  results <- jasptools::run("MultinomialTestBayesian", "Memory of Life Stresses.csv", options)
  table <- results[["results"]][["multinomialDescriptivesTable"]][["data"]]
  expect_equal_tables(table[1:4], list(7, 3, 14, 17, 1, 15, 5, 2, 11, 15, 4, 17))
})


