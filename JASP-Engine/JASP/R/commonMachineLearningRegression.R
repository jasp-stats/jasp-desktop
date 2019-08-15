#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

.readDataRegressionAnalyses <- function(dataset, options){
  target                    <- NULL
  testSetIndicator          <- NULL 
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  predictors                <- unlist(options[["predictors"]])
  predictors                <- predictors[predictors != ""]
  if(options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator")
    testSetIndicator                  <- options[["testSetIndicatorVariable"]]
  variables.to.read         <- c(target, predictors, testSetIndicator)
  if (is.null(dataset)){
    dataset <- .readDataSetToEnd(columns = variables.to.read, exclude.na.listwise = variables.to.read)
  }
  if(length(unlist(options[["predictors"]])) > 0 && options[["target"]] != "" && options[["scaleEqualSD"]])
    dataset[,.v(c(options[["predictors"]], options[["target"]]))] <- .scaleNumericData(dataset[,.v(c(options[["predictors"]], options[["target"]]))])
  return(dataset)
}

.errorHandlingRegressionAnalyses <- function(dataset, options){
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if(options[["target"]] != "")
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                       all.target = variables.to.read,
                       observations.amount = "< 2",
                       exitAnalysisIfErrors = TRUE)

  dataset <- na.omit(dataset)
  if(options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator" && nlevels(factor(dataset[,.v(options[["testSetIndicatorVariable"]])])) != 2){
    JASP:::.quitAnalysis("Your test set indicator should be binary, containing only 1 (included in test set) and 0 (excluded from test set).")
  }
}

.regressionAnalysesReady <- function(options, type){
  if(type == "randomForest" || type == "boosting" || type == "regularized"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if(type == "knn"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.regressionFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- formula(paste(target, "~", paste(predictors, collapse=" + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.regressionMachineLearning <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["regressionResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    .regressionFormula(options, jaspResults)
    
    if(type == "knn"){
      regressionResult <- .knnRegression(dataset, options, jaspResults)
    } else if(type == "regularized"){
      regressionResult <- .regularizedRegression(dataset, options, jaspResults)
    } else if(type == "randomForest"){
      regressionResult <- .randomForestRegression(dataset, options, jaspResults)
    } else if(type == "boosting"){
      regressionResult <- .boostingRegression(dataset, options, jaspResults)
    }
    jaspResults[["regressionResult"]] <- createJaspState(regressionResult)
    jaspResults[["regressionResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "maxTrees",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                                              "holdoutData", "testDataManual"))
  }
}

.regressionMachineLearningTable <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["regressionTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Regression",
                      "regularized" = "Regularized Linear Regression",
                      "randomForest" = "Random Forest Regression",
                      "boosting" = "Boosting Regression")

  regressionTable <- createJaspTable(title)
  regressionTable$position <- position
  regressionTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "maxTrees",
                                          "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac", "intDepth", "nNode", "distance",
                                          "testSetIndicatorVariable", "testSetIndicator", "validationDataManual","holdoutData", "testDataManual"))

  if(type == "knn"){

    regressionTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    regressionTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    regressionTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')

  } else if(type == "regularized"){

    regressionTable$addColumnInfo(name = 'penalty', title = 'Penalty', type = 'string')
    if(options[["penalty"]] == "elasticNet")
      regressionTable$addColumnInfo(name = 'alpha', title = '\u03B1', type = 'number')
    regressionTable$addColumnInfo(name = 'lambda', title = '\u03BB', type = 'number')

  } else if(type == "randomForest"){

    regressionTable$addColumnInfo(name = 'trees', title = 'Trees', type = 'integer')
    regressionTable$addColumnInfo(name = 'preds', title = 'Predictors per split', type = 'integer')
  
  } else if(type == "boosting"){

    regressionTable$addColumnInfo(name = 'trees', title = 'Trees', type = 'integer')
    regressionTable$addColumnInfo(name = 'shrinkage', title = 'Shrinkage', type = 'number')
    regressionTable$addColumnInfo(name = 'distribution', title = 'Loss function', type = 'integer')

  }

  regressionTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  regressionTable$addColumnInfo(name = 'nvalid', title = 'n(Validation)', type = 'integer')
  regressionTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  regressionTable$addColumnInfo(name = 'validMSE', title = 'Validation MSE', type = 'number', format = 'dp:3')
  regressionTable$addColumnInfo(name = 'testMSE', title = 'Test MSE', type = 'number', format = 'dp:3')

  if(type == "randomForest"){
    regressionTable$addColumnInfo(name = 'oob', title = 'OOB Error', type = 'number')
  }

  requiredVars <- ifelse(type == "knn", yes = 1, no = 2)
  if(!ready)
    regressionTable$addFootnote(message = paste0("Please provide a target variable and at least ", requiredVars, " predictor variable(s)."), symbol = "<i>Note.</i>")

  jaspResults[["regressionTable"]] <- regressionTable
  
  if(!ready)  return()

  .regressionMachineLearning(dataset, options, jaspResults, ready, type = type)

  regressionResult <- jaspResults[["regressionResult"]]$object

  # Adjust train and test numbers for cross-validation
  nTrain <- regressionResult[["ntrain"]]
  nValid <- regressionResult[["nvalid"]]
  if(options[["modelValid"]] == "validationKFold"){
    nValid <- floor(nValid / options[["noOfFolds"]])
    nTrain <- nTrain - nValid
  } else if(options[["modelValid"]] == "validationLeaveOneOut"){
    nValid <- 1
    nTrain <- nTrain - 1
  }
  
  if(type == "knn"){

    if(options[["modelOpt"]] == "optimizationError")
      regressionTable$addFootnote(message="The model is optimized with respect to the <i>validation set mean squared error</i>.", symbol="<i>Note.</i>")

    if(regressionResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      regressionTable$addFootnote(message="The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization.", symbol="<i>Note.</i>")
    }

    distance  <- ifelse(regressionResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidian")    
    row <- data.frame(nn = regressionResult[["nn"]], 
                      weights = regressionResult[["weights"]], 
                      distance = distance, 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = regressionResult[["ntest"]], 
                      validMSE = regressionResult[["validMSE"]],
                      testMSE = regressionResult[["testMSE"]])
    regressionTable$addRows(row)

  } else if(type == "regularized"){

    if(options[["shrinkage"]] != "manual")
      regressionTable$addFootnote(message="The model is optimized with respect to the <i>validation set mean squared error</i>.", symbol="<i>Note.</i>")

    if (regressionResult[["lambda"]] == 0)
      regressionTable$addFootnote("When \u03BB is set to 0 linear regression is performed.", symbol="<i>Note.</i>") 

    row <- data.frame(penalty = regressionResult[["penalty"]], 
                      lambda = regressionResult[["lambda"]], 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = regressionResult[["ntest"]], 
                      validMSE = regressionResult[["validMSE"]],
                      testMSE = regressionResult[["testMSE"]])
    if(options[["penalty"]] == "elasticNet")
      row <- cbind(row, alpha = regressionResult[["alpha"]])
    regressionTable$addRows(row)

  } else if(type == "randomForest"){

    if(options[["modelOpt"]] == "optimizationError")
      regressionTable$addFootnote(message="The model is optimized with respect to the <i>out-of-bag mean squared error</i>.", symbol="<i>Note.</i>")

    row <- data.frame(trees = regressionResult[["noOfTrees"]], 
                      preds = regressionResult[["predPerSplit"]], 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = regressionResult[["ntest"]], 
                      validMSE = regressionResult[["validMSE"]], 
                      testMSE = regressionResult[["testMSE"]], 
                      oob = regressionResult[["oobError"]])
    regressionTable$addRows(row)

  } else if(type == "boosting"){

    if(options[["modelOpt"]] == "optimizationOOB")
      regressionTable$addFootnote(message="The model is optimized with respect to the <i>out-of-bag mean squared error</i>.", symbol="<i>Note.</i>")

    distribution <- base::switch(options[["distance"]], "tdist" = "t", "gaussian" = "Gaussian", "laplace" = "Laplace")
    row <- data.frame(trees = regressionResult[["noOfTrees"]], 
                      shrinkage = options[["shrinkage"]], 
                      distribution = distribution, 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = regressionResult[["ntest"]], 
                      validMSE = regressionResult[["validMSE"]],
                      testMSE = regressionResult[["testMSE"]])
    regressionTable$addRows(row)

  }
}

.regressionEvaluationMetrics <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) return()
  
  validationMeasures <- createJaspTable(title = "Evaluation Metrics")
  validationMeasures$position <- position
  validationMeasures$dependOn(options = c("validationMeasures", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "maxTrees",
                                                              "holdoutData", "testDataManual"))

  validationMeasures$addColumnInfo(name = "measures", title = "Metric", type = "string")
  validationMeasures$addColumnInfo(name = "values", title = "", type = "string")

  measures <- c("MSE", "RMSE", "MAE", "MAPE", "R\u00B2")
  validationMeasures[["measures"]] <- measures
  
  jaspResults[["validationMeasures"]] <- validationMeasures

  if(!ready)  return()

  regressionResult <- jaspResults[["regressionResult"]]$object

  obs <- regressionResult[["testReal"]]
  pred <- regressionResult[["testPred"]]

  mse <- round(regressionResult[["testMSE"]], 3)
  rmse <- round(sqrt(mse), 3)
  mae <- round(mean(abs(obs - pred)), 3)
  mape <- paste0(round(mean( abs((obs - pred) / obs) ) * 100, 2), "%")
  r_squared <- round(cor(obs, pred)^2, 3)

  values <- c(mse, rmse, mae, mape, r_squared)

  validationMeasures[["values"]] <- values
  
}

.regressionPredictedPerformancePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["predictedPerformancePlot"]]) || !options[["predictedPerformancePlot"]]) return()

  predictedPerformancePlot <- createJaspPlot(plot = NULL, title = "Predictive Performance Plot", width = 400, height = 300)
  predictedPerformancePlot$position <- position
  predictedPerformancePlot$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid", "predictedPerformancePlot",
                                                            "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                            "intDepth", "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "maxTrees",
                                                            "holdoutData", "testDataManual"))
  jaspResults[["predictedPerformancePlot"]] <- predictedPerformancePlot

  if(!ready) return()

  regressionResult <- jaspResults[["regressionResult"]]$object
  
  predPerformance <- data.frame(true = c(regressionResult[["testReal"]]), predicted = regressionResult[["testPred"]])

  allBreaks <- JASPgraphs::getPrettyAxisBreaks(predPerformance[, 1], min.n = 4)

  p <- ggplot2::ggplot(data = predPerformance, mapping = ggplot2::aes(x = true, y = predicted)) +
        JASPgraphs::geom_line(data = data.frame(x = c(allBreaks[1], allBreaks[length(allBreaks)]), y = c(allBreaks[1], allBreaks[length(allBreaks)])), mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1) +
        JASPgraphs::geom_point() +
        ggplot2::scale_x_continuous("Observed test values", breaks = allBreaks, labels = allBreaks) +
        ggplot2::scale_y_continuous("Predicted test values", breaks = allBreaks, labels = allBreaks)
  p <- JASPgraphs::themeJasp(p)

  predictedPerformancePlot$plotObject <- p
}

.dataSplitPlot <- function(dataset, options, jaspResults, ready, position, purpose, type){

  if(!is.null(jaspResults[["dataSplitPlot"]]) || !options[["dataSplitPlot"]]) return()

  dataSplitPlot <- createJaspPlot(plot = NULL, title = "Data Split", width = 800, height = 70)
  dataSplitPlot$position <- position
  dataSplitPlot$dependOn(options = c("dataSplitPlot", "target", "predictors", "trainingDataManual", "modelValid", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))
  jaspResults[["dataSplitPlot"]] <- dataSplitPlot

  if(!ready) return()

    result <- base::switch(purpose,
  						"classification" = jaspResults[["classificationResult"]]$object,
						  "regression" = jaspResults[["regressionResult"]]$object)

  if(options[["modelValid"]] == "validationManual" || type == "randomForest" || type == "regularized" || type == "lda"){

    nTrain    <- result[["ntrain"]]
    nValid    <- result[["nvalid"]]
    nTest     <- result[["ntest"]]

    d <- data.frame(y = c(nTrain, nValid, nTest), x = c("Train", "Validation", "Test"), group = c(1,1,1))

    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Validation", "Train")))) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
          ggplot2::scale_y_continuous(limits = c(0, nTrain + nValid + nTest + ((nTrain + nValid + nTest)/5))) + # adjust limits to include "Total" text
          ggplot2::coord_flip() +
          ggplot2::labs(fill = "") +
          ggplot2::xlab("") +
          ggplot2::ylab("") +
          ggplot2::scale_fill_manual(values = c("tomato2", "darkgoldenrod2", "steelblue2")) +
          ggplot2::annotate("text", y = c(0, nTrain, nTrain + nValid, nTrain + nValid + nTest), x = 1, label = c(paste0("Train: ", nTrain), paste0("Validation: ", nValid), paste0("Test: ", nTest), paste0("Total: ", nTrain + nValid + nTest)), size = 4, vjust = 0.5, hjust = -0.1) 
    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), 
                            axis.text.y = ggplot2::element_blank(), 
                            axis.text.x = ggplot2::element_blank())

  } else {

    nTrainAndValid    <- result[["nvalid"]]
    nTest             <- result[["ntest"]]

    d <- data.frame(y = c(nTrainAndValid, nTest), x = c("Train and validation", "Test"), group = c(1,1))

    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Train and validation")))) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
          ggplot2::scale_y_continuous(limits = c(0, nTrainAndValid + nTest + ((nTrainAndValid + nTest)/5))) + # adjust limits to include "Total" text
          ggplot2::coord_flip() +
          ggplot2::labs(fill = "") +
          ggplot2::xlab("") +
          ggplot2::ylab("") +
          ggplot2::scale_fill_manual(values = c("tomato2", "seagreen2")) +
          ggplot2::annotate("text", y = c(0, nTrainAndValid, nTrainAndValid + nTest), x = 1, label = c(paste0("Train and validation: ", nTrainAndValid), paste0("Test: ", nTest), paste0("Total: ", nTrainAndValid + nTest)), size = 4, vjust = 0.5, hjust = -0.1) 
    p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

    p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  }

  dataSplitPlot$plotObject <- p

}

.addTestIndicatorToData <- function(options, jaspResults, ready, purpose){
  if(!ready || !options[["addIndicator"]] || options[["holdoutData"]] != "holdoutManual" || options[["testIndicatorColumn"]] == "")  return()

  result <- base::switch(purpose,
          "classification" = jaspResults[["classificationResult"]]$object,
          "regression" = jaspResults[["regressionResult"]]$object)

  if(is.null(jaspResults[["testIndicatorColumn"]])){
    testIndicatorColumn <- result[["testIndicatorColumn"]]
    jaspResults[["testIndicatorColumn"]] <- createJaspColumn(columnName = options[["testIndicatorColumn"]])
    jaspResults[["testIndicatorColumn"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "maxTrees",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "confusionProportions", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                                              "holdoutData", "testDataManual", "testIndicatorColumn", "addIndicator"))
    jaspResults[["testIndicatorColumn"]]$setNominal(testIndicatorColumn)
  }  
}

# these could also extend the S3 method scale although that could be somewhat unexpected
.scaleNumericData <- function(x, ...) {
  UseMethod(".scaleNumericData", x)
}

.scaleNumericData.data.frame <- function(x, center = TRUE, scale = TRUE) {
  idx <- sapply(x, is.numeric)
  x[, idx] <- scale(x[, idx, drop = FALSE], center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale")  <- NULL
  return(x)
}

.scaleNumericData.matrix <- function(x, center = TRUE, scale = TRUE) {
  x <- scale(x, center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale")  <- NULL
  return(x)
}

.scaleNumericData.numeric <- function(x, center = TRUE, scale = TRUE) {
  if (center)
    x <- x - mean(x)
  if (scale)
    x <- x / sd(x)
  return(x)
}

