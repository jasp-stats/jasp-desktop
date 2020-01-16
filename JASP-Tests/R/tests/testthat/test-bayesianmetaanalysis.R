context("Bayesian Meta Analysis")

options <- jasptools::analysisOptions("BayesianMetaAnalysis")
options$.meta <- list(confidenceInterval = list(containsColumn = TRUE), effectSize = list(
  containsColumn = TRUE), standardError = list(containsColumn = TRUE), 
  studyLabels = list(containsColumn = TRUE))
options$BFComputation <- "integration"
options$addInfo <- TRUE
options$addLines <- TRUE
options$confidenceInterval <- list()
options$effectSize <- "ES"
options$esTable <- TRUE
options$forestPlot <- "plotForestObserved"
options$iterMCMC <- 2000
options$orderForest <- "ascendingForest"
options$plotCumForest <- TRUE
options$plotPosterior <- TRUE
options$plotPrior <- TRUE
options$plotSeqPM <- TRUE
options$plotSequential <- TRUE
options$postTable <- TRUE
options$priorES <- "cauchy"
options$priorH0FE <- 0.25
options$priorH0RE <- 0.25
options$priorH1FE <- 0.25
options$priorH1RE <- 0.25
options$priorSE <- "inverseGamma"
options$shade <- TRUE
options$standardError <- "SE"
set.seed(1)
results <- jasptools::run("BayesianMetaAnalysis", "BCG Vaccine.csv", options)


test_that("Posterior Estimates per Model table results match", {
  table <- results[["results"]][["bmaTable"]][["data"]]
  expect_equal_tables(table,
                      list("TRUE", 4.70377989938849e+21, -0.434254495375077, 0.0413386758998794,
                           -0.51661689869485, "Fixed effects", "<unicode><unicode>", -0.354044245070268,
                           "TRUE", 46.1842149375581, -0.692806686013853, 0.19705023683889,
                           -1.08029372539509, "Random effects", "<unicode><unicode>", -0.298004382902606,
                           1, "FALSE", 8.63965964148833e+26, 0.581590540969456, 0.164579225962141,
                           0.342186021915771, "Random effects", "<unicode><unicode>", 0.982535492587413,
                           2, "TRUE", 46.184214937558, -0.691327583721835, 0.199773349389345,
                           -1.08085826208741, "Averaged", "<unicode><unicode>", -0.295412504628402,
                           3, "FALSE", 8.82672917711143e+26, "", "", "", "Averaged", "<unicode><unicode>",
                           ""))
})

test_that("Effect Sizes per Study table results match", {
  table <- results[["results"]][["esTable"]][["data"]]
  expect_equal_tables(table,
                      list(-0.801611834738253, -1.65847045840331, 0.0479461148361982, -0.9387,
                           "Study 1", -1.27376738820351, -2.03764953018419, -0.591068684840391,
                           -1.6662, "Study 2", -0.994340394486313, -1.90959255940211, -0.133732022141209,
                           -1.3863, "Study 3", -1.40256166901047, -1.67421470721358, -1.12563788945877,
                           -1.4564, "Study 4", -0.288902020007717, -0.710877482363825,
                           0.137959189514472, -0.2191, "Study 5", -0.948768538727828, -1.14086187658008,
                           -0.756685278730262, -0.9581, "Study 6", -1.23516451121386, -2.02965970642541,
                           -0.509557353159412, -1.6338, "Study 7", 0.00253133274902002,
                           -0.126505250991262, 0.129040638602368, 0.012, "Study 8", -0.510485074488074,
                           -0.948432260642302, -0.0680081743396815, -0.4717, "Study 9",
                           -1.25823336856425, -1.75670478039098, -0.766177022984406, -1.4012,
                           "Study 10", -0.356224829792452, -0.571698180127986, -0.136722919128629,
                           -0.3408, "Study 11", -0.262304308939451, -1.13607715402966,
                           0.733726559093854, 0.4466, "Study 12", -0.151542408255256, -0.641814588723302,
                           0.348874603463141, -0.0173, "Study 13"))
})

test_that("Cumulative forest plot matches", {
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_cumForestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "cumulative-forest-plot", dir="BayesianMetaAnalysis")
})

test_that("Observed study effects plot matches", {
  plotName <- results[["results"]][["forestContainer"]][["collection"]][["forestContainer_forestPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "observed-study-effects", dir="BayesianMetaAnalysis")
})

test_that("Effect size plot matches", {
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "effect-size", dir="BayesianMetaAnalysis")
})

test_that("Heterogeneity plot matches", {
  plotName <- results[["results"]][["postContainer"]][["collection"]][["postContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "heterogeneity", dir="BayesianMetaAnalysis")
})

test_that("Model Probabilities table results match", {
  table <- results[["results"]][["postTable"]][["data"]]
  expect_equal_tables(table,
                      list("Fixed H<unicode><unicode><unicode>", 2.40853635687008e-49, 0.25,
                           "Fixed H<unicode><unicode><unicode>", 1.13292249023919e-27,
                           0.25, "Random H<unicode><unicode><unicode>", 0.0211935284146057,
                           0.25, "Random H<unicode><unicode><unicode>", 0.978806471585394,
                           0.25))
})

test_that("Effect Size plot matches", {
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_ES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "effect-size", dir="BayesianMetaAnalysis")
})

test_that("Heterogeneity plot matches", {
  plotName <- results[["results"]][["priorContainer"]][["collection"]][["priorContainer_SE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "heterogeneity", dir="BayesianMetaAnalysis")
})

test_that("Posterior model probabilities plot matches", {
  plotName <- results[["results"]][["seqContainer"]][["collection"]][["seqContainer_seqPMPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "posterior-model-probabilities", dir="BayesianMetaAnalysis")
})

test_that("Bayes factors effect size plot matches", {
  plotName <- results[["results"]][["seqContainer"]][["collection"]][["seqContainer_seqPlotES"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factors-effect-size", dir="BayesianMetaAnalysis")
})

test_that("Bayes factors heterogeneity plot matches", {
  plotName <- results[["results"]][["seqContainer"]][["collection"]][["seqContainer_seqPlotSE"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "bayes-factors-heterogeneity", dir="BayesianMetaAnalysis")
})