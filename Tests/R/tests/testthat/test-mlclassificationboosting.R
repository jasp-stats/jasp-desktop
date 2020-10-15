context("Machine Learning Boosting Classification")

options <- jaspTools::analysisOptions("mlClassificationBoosting")
options$addClasses <- FALSE
options$addIndicator <- FALSE
options$andrewsCurve <- TRUE
options$classBoostRelInfTable <- TRUE
options$classColumn <- ""
options$classProportionsTable <- TRUE
options$holdoutData <- "holdoutManual"
options$modelOpt <- "optimizationOOB"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$plotDeviance <- TRUE
options$plotOOBChangeDev <- TRUE
options$plotRelInf <- TRUE
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
results <- jaspTools::run("mlClassificationBoosting", "wine.csv", options)


test_that("Andrews Curves Plot matches", {
  plotName <- results[["results"]][["andrewsCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "andrews-curves-plot", dir="mlClassificationBoosting")
})

test_that("Relative Influence table results match", {
  table <- results[["results"]][["classBoostRelInfTable"]][["data"]]
  expect_equal_tables(table,
                      list("Proline", 31.1797927388636, "Color", 20.173253110114, "Flavanoids",
                           19.8438729446546, "Alcohol", 11.3080195393476, "Hue", 9.04159967448256,
                           "Dilution", 5.78903515515603, "Phenols", 2.06207785543184, "Malic",
                           0.318048182535448, "Magnesium", 0.198970273683845, "Ash", 0.0761054305928261,
                           "Nonflavanoids", 0.00505359720114565, "Alcalinity", 0.00416148218437122,
                           "Proanthocyanins", 1.00157520318044e-05))
})

test_that("Class Proportions table results match", {
  table <- results[["results"]][["classProportionsTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.331460674157303, 1, 0.371428571428571, 0.324561403508772, 0.310344827586207,
                           0.398876404494382, 2, 0.4, 0.368421052631579, 0.517241379310345,
                           0.269662921348315, 3, 0.228571428571429, 0.307017543859649,
                           0.172413793103448))
})

test_that("Boosting Classification table results match", {
  table <- results[["results"]][["classificationTable"]][["data"]]
  expect_equal_tables(table,
                      list(35, 114, 29, 0.1, 0.942857142857143, 84, 0.96551724137931))
})

test_that("Confusion Matrix table results match", {
  table <- results[["results"]][["confusionTable"]][["data"]]
  expect_equal_tables(table,
                      list("Observed", 1, 12, 0, 1, "", 2, 1, 13, 0, "", 3, 0, 0, 8))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlClassificationBoosting")
})

test_that("Deviance Plot matches", {
  plotName <- results[["results"]][["plotDeviance"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "deviance-plot", dir="mlClassificationBoosting")
})

test_that("Out-of-bag Improvement Plot matches", {
  plotName <- results[["results"]][["plotOOBChangeDev"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "out-of-bag-improvement-plot", dir="mlClassificationBoosting")
})

test_that("Relative Influence Plot matches", {
  plotName <- results[["results"]][["plotRelInf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "relative-influence-plot", dir="mlClassificationBoosting")
})

test_that("ROC Curves Plot matches", {
  plotName <- results[["results"]][["rocCurve"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "roc-curves-plot", dir="mlClassificationBoosting")
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list(0.951048951048951, 0.923076923076923, 1, 0.923076923076923, 0.923076923076923,
                           13, 1, 0.962962962962963, 2, 1, 0.928571428571429, 14, 1, 0.941176470588235,
                           3, 0.888888888888889, 1, 8, 0.983682983682984, 0.943168378462496,
                           "Average / Total", 0.946031746031746, 0.942857142857143, 35
                      ))
})