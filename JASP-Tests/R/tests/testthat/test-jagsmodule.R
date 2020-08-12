context("JAGS")
skip("JAGS doesn't work on Travis")
options <- jaspTools::analysisOptions("JAGS")
options$.meta <- list(initialValues = list(list(levels = list(containsColumn = TRUE)), 
                                           list(levels = list(containsColumn = TRUE))), model = list(
                                             columns = list(containsColumn = TRUE), parameters = list(
                                               containsColumn = TRUE)), monitoredParametersList = list(
                                                 containsColumn = TRUE), parametersShown = list(containsColumn = TRUE), 
                      userData = list(list(levels = list(containsColumn = TRUE)), 
                                      list(levels = list(containsColumn = TRUE))))
options$initialValues <- list(list(levels = "Row 1", name = "Parameter", values = "theta"), 
                              list(levels = "Row 1", name = "R Code", values = "..."))
options$model <- list(columns = list(), model = "model{\n theta ~ dbeta(1, 1)\n k ~ dbinom(theta, n)\n mu ~ dnorm(0, 1)}", 
                      parameters = c("theta", "k"))
options$noBurnin <- 1
options$noChains <- 4
options$noSamples <- 50
options$parametersShown <- c("theta", "mu")
options$plotAutoCor <- TRUE
options$plotDensity <- TRUE
options$plotHistogram <- TRUE
options$plotTrace <- TRUE
options$plotBivarHex <- TRUE
options$userData <- list(list(levels = c("Row 0", "Row 1"), name = "Parameter", values = c("k", 
                                                                                           "n")), list(levels = c("Row 0", "Row 1"), name = "R Code", values = c("70", 
                                                                                                                                                                 "100")))
options$parameters <- c("\"theta\"", "\"mu\"")
set.seed(1)
results <- jaspTools::run("JAGS", "debug.csv", options)
print(results)
test_that("MCMC summary table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  expect_equal_tables(table,
                      list(-2.13189859421549, 0.0400235500930204, 1.75316153519098, 0.0150312557449907,
                           1.04100463312435, 162.211144594831, "mu", 1.28053408855522,
                           1.08774224099908, 0.614492436789889, 0.69902486500684, 0.777692716928423,
                           0.696777556636238, 0.0431041676553818, 232.297440897823, "theta",
                           1.25276545060289, 1.07739568349341))
})

test_that("plotAutoCor mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotAutoCor"]][["collection"]][["mainContainer_plotContainer_plotAutoCor_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotAutoCor-mu", dir="JAGS")
})

test_that("plotAutoCor theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotAutoCor"]][["collection"]][["mainContainer_plotContainer_plotAutoCor_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotAutoCor-theta", dir="JAGS")
})

test_that("plotDensity mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotDensity"]][["collection"]][["mainContainer_plotContainer_plotDensity_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotDensity-mu", dir="JAGS")
})

test_that("plotDensity theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotDensity"]][["collection"]][["mainContainer_plotContainer_plotDensity_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotDensity-theta", dir="JAGS")
})

test_that("plotHistogram mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotHistogram"]][["collection"]][["mainContainer_plotContainer_plotHistogram_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotHistogram-mu", dir="JAGS")
})

test_that("plotHistogram theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotHistogram"]][["collection"]][["mainContainer_plotContainer_plotHistogram_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotHistogram-theta", dir="JAGS")
})

test_that("plotTrace mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotTrace"]][["collection"]][["mainContainer_plotContainer_plotTrace_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotTrace-mu", dir="JAGS")
})

test_that("plotTrace theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotTrace"]][["collection"]][["mainContainer_plotContainer_plotTrace_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "plotTrace-theta", dir="JAGS")
})

test_that("Bivariate Scatter Plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotBivarHex"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bivariate-scatter-plot", dir="JAGS")
})
