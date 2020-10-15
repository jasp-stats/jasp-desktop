context("Machine Learning Boosting Regression")

options <- jaspTools::analysisOptions("mlRegressionBoosting")
options$addIndicator <- FALSE
options$addValues <- FALSE
options$classBoostRelInfTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationOOB"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotDeviance <- TRUE
options$plotOOBChangeDev <- TRUE
options$plotRelInf <- TRUE
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
results <- jaspTools::run("mlRegressionBoosting", "wine.csv", options)


test_that("Relative Influence table results match", {
  table <- results[["results"]][["classBoostRelInfTable"]][["data"]]
  expect_equal_tables(table,
                      list("Color", 60.3564909956942, "Proline", 28.9359792394392, "Phenols",
                           4.02747773147275, "Flavanoids", 2.65410848555859, "Hue", 1.59224895077339,
                           "Proanthocyanins", 1.14701822224105, "Malic", 0.744077365382741,
                           "Alcalinity", 0.542599009438009, "Ash", 0, "Magnesium", 0, "Nonflavanoids",
                           0, "Dilution", 0))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlRegressionBoosting")
})

test_that("Deviance Plot matches", {
  plotName <- results[["results"]][["plotDeviance"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "deviance-plot", dir="mlRegressionBoosting")
})

test_that("Out-of-bag Improvement Plot matches", {
  plotName <- results[["results"]][["plotOOBChangeDev"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "out-of-bag-improvement-plot", dir="mlRegressionBoosting")
})

test_that("Relative Influence Plot matches", {
  plotName <- results[["results"]][["plotRelInf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "relative-influence-plot", dir="mlRegressionBoosting")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "predictive-performance-plot", dir="mlRegressionBoosting")
})

test_that("Boosting Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Gaussian", 35, 114, 29, 0.1, 0.425591531163354, 24, 0.54136660008236
                      ))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list("MSE", 0.426, "RMSE", 0.653, "MAE", 0.524, "MAPE", "141.08%",
                           "R<unicode><unicode>", 0.652))
})