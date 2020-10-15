context("Machine Learning Regularized Linear Regression")

options <- jaspTools::analysisOptions("mlRegressionRegularized")
options$addIndicator <- FALSE
options$addValues <- FALSE
options$coefTable <- TRUE
options$holdoutData <- "holdoutManual"
options$lambdaEvaluation <- TRUE
options$modelOpt <- "optMin"
options$modelValid <- "validationManual"
options$noOfFolds <- 5
options$predictedPerformancePlot <- TRUE
options$predictors <- list("Malic", "Ash", "Alcalinity", "Magnesium", "Phenols", "Flavanoids", 
                           "Nonflavanoids", "Proanthocyanins", "Color", "Hue", "Dilution", 
                           "Proline")
options$seedBox <- TRUE
options$target <- "Alcohol"
options$testDataManual <- 0.2
options$testIndicatorColumn <- ""
options$testSetIndicatorVariable <- ""
options$thresh <- 1e-07
options$validationDataManual <- 0.2
options$validationMeasures <- TRUE
options$valueColumn <- ""
options$variableTrace <- TRUE
set.seed(1)
results <- jaspTools::run("mlRegressionRegularized", "wine.csv", options)


test_that("Regression Coefficients table results match", {
  table <- results[["results"]][["coefTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.061127975064724, "(Intercept)", 0.142374835142716, "Malic",
                           0.0636996037701241, "Ash", -0.116779483193431, "Alcalinity",
                           0, "Magnesium", 0.0368807604820878, "Phenols", 0.204299292659466,
                           "Flavanoids", -0.0391549243145209, "Nonflavanoids", -0.162348453489162,
                           "Proanthocyanins", 0.527743708127644, "Color", 0.0327635272841201,
                           "Hue", 0.150491111131655, "Dilution", 0.226659953230796, "Proline"
                      ))
})

test_that("Data Split plot matches", {
  plotName <- results[["results"]][["dataSplitPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "data-split", dir="mlRegressionRegularized")
})

test_that("Lambda Evaluation Plot matches", {
  plotName <- results[["results"]][["lambdaEvaluation"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "lambda-evaluation-plot", dir="mlRegressionRegularized")
})

test_that("Predictive Performance Plot matches", {
  plotName <- results[["results"]][["predictedPerformancePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "predictive-performance-plot", dir="mlRegressionRegularized")
})

test_that("Regularized Linear Regression table results match", {
  table <- results[["results"]][["regressionTable"]][["data"]]
  expect_equal_tables(table,
                      list(0.0160411963895373, 35, 114, 29, "L1 (Lasso)", 0.479047646229094,
                           0.514015190983995))
})

test_that("Evaluation Metrics table results match", {
  table <- results[["results"]][["validationMeasures"]][["data"]]
  expect_equal_tables(table,
                      list("MSE", 0.479, "RMSE", 0.692, "MAE", 0.527, "MAPE", "105.95%",
                           "R<unicode><unicode>", 0.549))
})

test_that("Variable Trace Plot matches", {
  plotName <- results[["results"]][["variableTrace"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_equal_plots(testPlot, "variable-trace-plot", dir="mlRegressionRegularized")
})