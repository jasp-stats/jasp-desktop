context("Machine Learning Random Forest Classification")

options <- jaspTools::analysisOptions("mlClassificationRandomForest")
options$addClasses <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$classColumn <- ""
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationError"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotDecreaseAccuracy <- TRUE
options$plotIncreasePurity <- TRUE
options$plotTreesVsModelError <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$rocCurve <- TRUE
options$seedBox <- TRUE
options$tableVariableImportance <- TRUE
options$target <- "Type"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
set.seed(1)
results <- jaspTools::run("mlClassificationRandomForest", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "andrews-curves-plot", dir="mlClassificationRandomForest")
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.324561403508772, 0.310344827586207,
                           0.398876404494382, 2, 0.4, 0.368421052631579, 0.517241379310345,
                           0.269662921348315, 3, 0.228571428571429, 0.307017543859649,
                           0.172413793103448))
})

test_that("Random Forest Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  expect_equal_tables(table,
                      list(35, 114, 29, 1, 3, 0.971428571428571, 38, 0.931034482758621))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Observed", 1, 13, 0, 0, "", 2, 0, 13, 1, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlClassificationRandomForest")
})

test_that("Mean Decrease in Accuracy plot matches", {
  plotName <- results[["results"]][["plotDecreaseAccuracy"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "mean-decrease-in-accuracy", dir="mlClassificationRandomForest")
})

test_that("Total Increase in Node Purity plot matches", {
  plotName <- results[["results"]][["plotIncreasePurity"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "total-increase-in-node-purity", dir="mlClassificationRandomForest")
})

test_that("Out-of-bag Classification Accuracy Plot matches", {
  plotName <- results[["results"]][["plotTreesVsModelError"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "out-of-bag-classification-accuracy-plot", dir="mlClassificationRandomForest")
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "roc-curves-plot", dir="mlClassificationRandomForest")
})

test_that("Variable Importance table results match", {
  table <- results[["results"]][["tableVariableImportance"]][["data"]]
  expect_equal_tables(table,
                      list(0.0929184156013314, 0.11584466455711, "Color", 0.0746170330176111,
                           0.0740156345749835, "Alcohol", 0.227882931822773, 0.0519798563590929,
                           "Proline", 0.104210419210419, 0.038220702722991, "Dilution",
                           0.0392152459372139, 0.0269934390675145, "Hue", 0.148036748647974,
                           0.00844623148177441, "Flavanoids", 0.0192297690867485, 0.00698621553884712,
                           "Alcalinity", 0.00674978530241688, 0.00569058515854854, "Ash",
                           0.0214415086298743, 0.00440369101163704, "Malic", 0.00579683474420317,
                           0.0025062656641604, "Magnesium", 0.0831850839152417, -0.000437779601050258,
                           "Phenols", 0.0194222905441742, -0.000592846851428087, "Proanthocyanins",
                           0.0238594357587493, -0.00457882900654936, "Nonflavanoids"))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list(0.993006993006993, 1, 1, 1, 1, 13, 0.998299319727891, 0.962962962962963,
                           2, 1, 0.928571428571429, 14, 1, 0.941176470588235, 3, 0.888888888888889,
                           1, 8, 0.997102104244961, 0.971739807033925, "Average / Total",
                           0.974603174603175, 0.971428571428571, 35))
})