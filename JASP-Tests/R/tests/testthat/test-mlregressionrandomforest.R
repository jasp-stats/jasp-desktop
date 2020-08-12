context("Machine Learning Random Forest Regression")

options <- jaspTools::analysisOptions("mlRegressionRandomForest")
options$addIndicator <- FALSE
options$addValues <- FALSE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationError"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotDecreaseAccuracy <- TRUE
options$plotIncreasePurity <- TRUE
options$plotTreesVsModelError <- TRUE
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$seedBox <- TRUE
options$tableVariableImportance <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$valueColumn <- ""
set.seed(1)
results <- jaspTools::run("mlRegressionRandomForest", "wine.csv", options)


test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlRegressionRandomForest")
})

test_that("Mean Decrease in Accuracy plot matches", {
  plotName <- results[["results"]][["plotDecreaseAccuracy"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "mean-decrease-in-accuracy", dir="mlRegressionRandomForest")
})

test_that("Total Increase in Node Purity plot matches", {
  plotName <- results[["results"]][["plotIncreasePurity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "total-increase-in-node-purity", dir="mlRegressionRandomForest")
})

test_that("Out-of-bag Mean Squared Error Plot matches", {
  plotName <- results[["results"]][["plotTreesVsModelError"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "out-of-bag-mean-squared-error-plot", dir="mlRegressionRandomForest")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "predictive-performance-plot", dir="mlRegressionRandomForest")
})

test_that("Random Forest Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  expect_equal_tables(table,
                      list(35, 114, 29, 0.398951826523355, 3, 0.484311284922699, 97, 0.611083026333054
                      ))
})

test_that("Variable Importance table results match", {
  table <- results[["results"]][["tableVariableImportance"]][["data"]]
  expect_equal_tables(table,
                      list(0.475727661943587, 13.9713939210907, "Color", 0.133970922782775,
                           6.27626368698486, "Proline", 0.117761043024228, 3.97309320553953,
                           "Flavanoids", 0.0538014659638475, 3.65347091537435, "Phenols",
                           0.036092478567819, 3.64937884621186, "Alcalinity", 0.0202836370094357,
                           3.14440781708078, "Magnesium", 0.044396608949806, 2.94894309954197,
                           "Malic", 0.041519532754131, 2.62081725098105, "Dilution", 0.00929571084292303,
                           2.3343823192425, "Ash", 0.0384877159897528, 2.21651993528447,
                           "Hue", 0.00439249198505879, 1.83777482063651, "Proanthocyanins",
                           0.00557107935772629, 1.46246316922245, "Nonflavanoids"))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list("MSE", 0.484, "RMSE", 0.696, "MAE", 0.539, "MAPE", "169.83%",
                           "R<unicode><unicode>", 0.578))
})