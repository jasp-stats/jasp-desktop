context("Bain One Sample T-Test")

options <- jasptools::analysisOptions("BainTTestBayesianOneSample")
options$bayesFactorPlot <- TRUE
options$bayesFactorType <- "BF01"
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$hypothesis <- "allTypes"
options$testValue <- 51
options$variables <- list("age")
set.seed(1)
results <- jasptools::run("BainTTestBayesianOneSample", "sesame.csv", options)

test_that("BF age plot matches", {
  plotName <- results[["results"]][["BFplots"]][["collection"]][["BFplots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "BF age", dir="BainTTestBayesianOneSample")
})

test_that("Bain One Sample T-test table results match", {
  table <- results[["results"]][["bainTable"]][["data"]]
  expect_equal_tables(table,
                      list("", 15.1135035681881, 15.8743731825382, "age", 0.885613607789322,
                           0.0585975054555463, 0.0557888867551317, "H0: Equal", "H1: Bigger",
                           "H2: Smaller"))
})

test_that("Descriptives age plot matches", {
  plotName <- results[["results"]][["descriptivesPlots"]][["collection"]][["descriptivesPlots_age"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives age", dir="BainTTestBayesianOneSample")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(240, 50.212, 51.013, 6.292, 0.406, 51.813, "age"))
})