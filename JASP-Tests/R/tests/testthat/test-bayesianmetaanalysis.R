context("Bayesian Meta Analysis")

options <- jaspTools::analysisOptions("BayesianMetaAnalysis")
options$effectSize <- "ES"
options$standardError <- "SE"
options$postTable <- TRUE
options$esTable <- TRUE
options$plotPrior <- TRUE
options$plotPosterior <- TRUE
options$addInfo <- TRUE
options$addLines <- TRUE
options$shade <- TRUE
options$priorH0FE <- 0.25
options$priorH1FE <- 0.25
options$priorH0RE <- 0.25
options$priorH1RE <- 0.25
options$iterMCMC <- 2000
options$priorES <- "cauchy"
options$priorSE <- "inverseGamma"
options$forestPlot <- "plotForestObserved"
options$orderForest <- "ascendingForest"
options$BFComputation <- "integration"
options$.meta <- list(confidenceInterval = list(containsColumn = TRUE), effectSize = list(
  containsColumn = TRUE), standardError = list(containsColumn = TRUE), 
  studyLabels = list(containsColumn = TRUE))
options$confidenceInterval <- list()
set.seed(1)
results <- jaspTools::run("BayesianMetaAnalysis", "BCG Vaccine.csv", options)


test_that("Posterior Estimates per Model table results match", {
  skip("rstan results differ across different devices")
  table <- results[["results"]][["bmaTable"]][["data"]]
  expect_equal_tables(table,
                      list("TRUE", 4.70377989938849e+21, -0.432435327277424, 0.0420585438841347,
                           -0.513917121730249, "Fixed effects", "<unicode>", -0.34975165098494,
                           "TRUE", 46.1842149375581, -0.701483459370223, 0.190926379651806,
                           -1.09159179179873, "Random effects", "<unicode>", -0.320025966772925,
                           1, "FALSE", 8.63965964148833e+26, 0.581261616746024, 0.156001990952058,
                           0.348061941762999, "Random effects", "<unicode>", 0.94873262454565,
                           2, "TRUE", 46.184214937558, -0.705584625207882, 0.188977656089491,
                           -1.0884429013773, "Averaged", "<unicode>", -0.328747434064055,
                           3, "FALSE", 8.82672917711143e+26, "", "", "", "Averaged", "<unicode>",
                           ""))
})

test_that("Effect Sizes per Study table results match", {
  skip("rstan results differ across different devices")
  table <- results[["results"]][["esTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.818917209995554, -1.68022241432964, 0.0128851026501171, -0.9387,
                           "Study 1", -1.27420969313588, -2.04817695876936, -0.562035765413615,
                           -1.6662, "Study 2", -0.994510727054973, -1.89743737628663, -0.142013271190813,
                           -1.3863, "Study 3", -1.40753989430264, -1.68982205836008, -1.13656688516052,
                           -1.4564, "Study 4", -0.291302823880975, -0.711807690516303,
                           0.1190018255709, -0.2191, "Study 5", -0.948346076337037, -1.13588558721523,
                           -0.757718459706663, -0.9581, "Study 6", -1.23592193390415, -2.03320162643305,
                           -0.49785073481531, -1.6338, "Study 7", 0.00220069288102787,
                           -0.122247001933177, 0.128673023793419, 0.012, "Study 8", -0.50509835214573,
                           -0.931936322046978, -0.0898434670377745, -0.4717, "Study 9",
                           -1.25789357015672, -1.76235379625804, -0.74961396704111, -1.4012,
                           "Study 10", -0.35517782099167, -0.575414583339846, -0.139350923020152,
                           -0.3408, "Study 11", -0.264999982769619, -1.15219084254386,
                           0.749181308457912, 0.4466, "Study 12", -0.152742286722515, -0.633216294491976,
                           0.351841801938981, -0.0173, "Study 13"))
})

test_that("Observed study effects plot matches", {
  skip("rstan results differ across different devices")
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "observed-study-effects", dir="BayesianMetaAnalysis")
})

test_that("Effect size plot matches", {
  skip("rstan results differ across different devices")
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "effect-size", dir="BayesianMetaAnalysis")
})

test_that("Heterogeneity plot matches", {
  skip("rstan results differ across different devices")
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "heterogeneity", dir="BayesianMetaAnalysis")
})

test_that("Model Probabilities table results match", {
  table <- results[["results"]][["postTable"]][["data"]]
  expect_equal_tables(table,
                      list("Fixed H<unicode>", 2.40853635687008e-49, 0.25, "Fixed H<unicode>",
                           1.13292249023919e-27, 0.25, "Random H<unicode>", 0.0211935284146057,
                           0.25, "Random H<unicode>", 0.978806471585394, 0.25))
})

test_that("Effect Size plot matches", {
  skip("rstan results differ across different devices")
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "effect-size", dir="BayesianMetaAnalysis")
})

test_that("Heterogeneity plot matches", {
  skip("rstan results differ across different devices")
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "heterogeneity", dir="BayesianMetaAnalysis")
})