context("Bain One Sample T-Test")

options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
options$bayesFactorPlot <- TRUE
options$bayesFactorType <- "BF01"
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$testValue <- 51
options$variables <- list("age")
set.seed(1)
results <- jaspTools::run("BainTTestBayesianOneSample", "sesame.csv", options)

test_that("Bain One Sample T-test table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list("", 15.1135035681881, 15.8743731825382, "age", 0.885613607789322,
                           0.0585975054555463, 0.0557888867551317, "H0: Equal", "H1: Bigger",
                           "H2: Smaller"))
})

test_that("BF age plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlots"]][["collection"]][["bainContainer_bayesFactorPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "BF age", dir="BainTTestBayesianOneSample")
})

test_that("Descriptives age plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descripvies age", dir="BainTTestBayesianOneSample")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(240, 50.2165034595134, 51.0125, 6.29171019316781, 0.406128146621706,
                           51.8084965404866, "age"))
})