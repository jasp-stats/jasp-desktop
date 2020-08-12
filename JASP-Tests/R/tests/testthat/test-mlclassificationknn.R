context("Machine Learning KNN Classification")

options <- jaspTools::analysisOptions("mlClassificationKnn")
options$addClasses <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$classColumn <- ""
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationError"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotErrorVsK <- TRUE
options$predictors <- list("Alcohol", "Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", 
                           "Flavanoids", "Nonflavanoids", "Proanthocyanins", "Color", 
                           "Hue", "Dilution", "Proline")
options$rocCurve <- TRUE
options$seedBox <- TRUE
options$target <- "Type"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
set.seed(1)
results <- jaspTools::run("mlClassificationKnn", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "andrews-curves-plot", dir="mlClassificationKnn")
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.324561403508772, 0.310344827586207,
                           0.398876404494382, 2, 0.4, 0.368421052631579, 0.517241379310345,
                           0.269662921348315, 3, 0.228571428571429, 0.307017543859649,
                           0.172413793103448))
})

test_that("K-Nearest Neighbors Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  expect_equal_tables(table,
                      list("Euclidean", 1, 35, 114, 29, 0.942857142857143, 0.896551724137931,
                           "rectangular"))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Observed", 1, 13, 0, 0, "", 2, 1, 12, 1, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlClassificationKnn")
})

test_that("Classification Accuracy Plot matches", {
  plotName <- results[["results"]][["plotErrorVsK"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "classification-accuracy-plot", dir="mlClassificationKnn")
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "roc-curves-plot", dir="mlClassificationKnn")
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list(0.977272727272727, 0.962962962962963, 1, 0.928571428571429, 1,
                           13, 0.928571428571429, 0.923076923076923, 2, 1, 0.857142857142857,
                           14, 0.981481481481482, 0.941176470588235, 3, 0.888888888888889,
                           1, 8, 0.962441879108546, 0.942028777322895, "Average / Total",
                           0.948072562358277, 0.942857142857143, 35))
})