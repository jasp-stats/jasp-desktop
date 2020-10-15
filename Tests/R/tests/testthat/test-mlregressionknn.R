context("Machine Learning KNN Regression")

options <- jaspTools::analysisOptions("mlRegressionKnn")
options$addIndicator <- FALSE
options$addValues <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationError"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotErrorVsK <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$seedBox <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$valueColumn <- ""
set.seed(1)
results <- jaspTools::run("mlRegressionKnn", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlRegressionKnn")
})

test_that("Mean Squared Error Plot matches", {
  plotName <- results[["results"]][["plotErrorVsK"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "mean-squared-error-plot", dir="mlRegressionKnn")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "predictive-performance-plot", dir="mlRegressionKnn")
})

test_that("K-Nearest Neighbors Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Euclidean", 4, 35, 114, 29, 0.575520302093519, 0.524262748766176,
                           "rectangular"))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list("MSE", 0.576, "RMSE", 0.759, "MAE", 0.63, "MAPE", "245.37%", "R<unicode><unicode>",
                           0.49))
})