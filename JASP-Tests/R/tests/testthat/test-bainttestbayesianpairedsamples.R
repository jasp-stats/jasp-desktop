context("Bain Paired Samples T-Test")

options <- jasptools::analysisOptions("BainTTestBayesianPairedSamples")
options$bayesFactorPlot <- TRUE
options$bayesFactorType <- "BF01"
options$descriptives <- TRUE
options$descriptivesPlots <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$pairs <- list(list("prenumb", "postnumb"))
set.seed(1)
results <- jasptools::run("BainTTestBayesianPairedSamples", "sesame.csv", options)

test_that("Bain Paired Samples T-Test table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bainTable"]][["data"]]
  expect_equal_tables(table,
                      list("", 276.004579526046, 2.76244529575416e-43, "prenumb - postnumb",
                           2.76244529575416e-43, 1.00086936981184e-45, 1, "H0: Equal",
                           "H1: Bigger", "H2: Smaller"))
})

test_that("BF prenumb - postnumb plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorPlots"]][["collection"]][["bainContainer_bayesFactorPlots_prenumb - postnumb"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "BF prenumb-postnumb", dir="BainTTestBayesianPairedSamples")
})

test_that("Descriptives prenumb - postnumb plot matches", {
  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_prenumb - postnumb"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "descriptives prenumb-postnumb", dir="BainTTestBayesianPairedSamples")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  expect_equal_tables(table,
                      list(240, 19.4150788423063, 20.7583333333333, 10.6173425930313, 0.685346517396481,
                           19.4837158661898, "prenumb", 240, 27.8574275457606, 29.45, 12.5879998643857,
                           0.81255189728045, 28.3025879127404, "postnumb", 240, -9.89599389735516,
                           -8.69166666666667, 9.51923473009165, 0.61446395963807, -7.48733943597818,
                           "prenumb - postnumb"))
})