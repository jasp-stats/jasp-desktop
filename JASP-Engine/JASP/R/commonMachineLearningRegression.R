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

.readDataRegressionAnalyses <- function(dataset, options, jaspResults){
  if (is.null(dataset))
    dataset <- .readDataClassificationRegressionAnalyses(dataset, options)
  
  if (length(unlist(options[["predictors"]])) > 0 && options[["target"]] != "" && options[["scaleEqualSD"]])
    dataset[,.v(c(options[["predictors"]], options[["target"]]))] <- .scaleNumericData(dataset[,.v(c(options[["predictors"]], options[["target"]])), drop = FALSE])
  
  return(dataset)
}

.readDataClassificationRegressionAnalyses <- function(dataset, options) {
  target <- NULL
  if (options[["target"]] != "")
    target <- options[["target"]]
  
  predictors <- NULL
  if (length(options[["predictors"]]) > 0)
    predictors <- unlist(options[["predictors"]])
  
  testSetIndicator <- NULL 
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator")
    testSetIndicator <- options[["testSetIndicatorVariable"]]
  
  return(.readAndAddCompleteRowIndices(dataset, columns = c(target, predictors), columnsAsNumeric = testSetIndicator))
}

.readAndAddCompleteRowIndices <- function(dataset, columns = NULL, columnsAsNumeric = NULL){
    dataset <- .readDataSetToEnd(columns = columns, columns.as.numeric = columnsAsNumeric)
    
    complete.index      <- which(complete.cases(dataset))
    dataset             <- na.omit(dataset)
    rownames(dataset)   <- as.character(complete.index)
    
    return(dataset)
}

.errorHandlingRegressionAnalyses <- function(dataset, options, type){
  .errorHandlingClassificationRegressionAnalyses(dataset, options, type)

  if (type == "regularized" &&
      "weights" %in% names(options) && !is.null(options[["weights"]]) && options[["weights"]] != "")
    .hasErrors(dataset, type = c("infinity", "limits", "observations"),
              all.target = options[["weights"]], limits.min = 0, observations.amount = "< 2",
              exitAnalysisIfErrors = TRUE)
}

.errorHandlingClassificationRegressionAnalyses <- function(dataset, options, type) {
  predictors                <- unlist(options['predictors'])
  predictors                <- predictors[predictors != ""]
  target                    <- NULL
  if (options[["target"]] != "")
    target                  <- options[["target"]]
  variables.to.read         <- c(predictors, target)
  
  if (length(variables.to.read) == 0)
    return()
  
  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator") {
    
    if (options[["testSetIndicatorVariable"]] %in% predictors)
      JASP:::.quitAnalysis(gettextf("The variable '%s' can't be both a predictor and a test set indicator.", options[["testSetIndicatorVariable"]]))
  
    indicatorVals <- unique(dataset[,.v(options[["testSetIndicatorVariable"]])])
    if (length(indicatorVals) != 2 || !all(0:1 %in% indicatorVals))
      JASP:::.quitAnalysis(gettext("Your test set indicator should be binary, containing only 1 (included in test set) and 0 (excluded from test set)."))
    
  }
  
  customChecks <- .getCustomErrorChecksKnnBoosting(dataset, options, type)
  .hasErrors(dataset, type = c('infinity', 'observations'), custom = customChecks,
             all.target = variables.to.read,
             observations.amount = "< 2",
             exitAnalysisIfErrors = TRUE)
}

.getCustomErrorChecksKnnBoosting <- function(dataset, options, type) {
  if (!type %in% c("knn", "boosting"))
    return()

  if (options[["testSetIndicatorVariable"]] != "" && options[["holdoutData"]] == "testSetIndicator")
    nTrainAndValid <- length(which(dataset[, .v(options[["testSetIndicatorVariable"]])] == 0))
  else
    nTrainAndValid <- ceiling(nrow(dataset) - nrow(dataset)*options[['testDataManual']])

  # check for too many nearest neighbors (nn > nTrain) before the analysis starts
  checkNearestNeighbors <- function() {
    if (type != "knn")
      return()

    nn <- base::switch(options[["modelOpt"]], "optimizationManual" = options[["noOfNearestNeighbours"]], "optimizationError" = options[["maxK"]])

    valueToTest <- nTrainAndValid
    if (options[["modelOpt"]] == "optimizationError") {
      if (options[["modelValid"]] == "validationManual")
        nTrain <- ceiling(nTrainAndValid - nTrainAndValid * options[['validationDataManual']])
      if (options[["modelValid"]] == "validationKFold")
        nTrain <- ceiling(nTrainAndValid - nTrainAndValid / (options[["noOfFolds"]] - 1))
      if (options[["modelValid"]] == "validationLeaveOneOut")
        nTrain <- nTrainAndValid - 1
      valueToTest <- nTrain
    }
    
    if (nn >= valueToTest)
      return(gettextf("You have specified more nearest neighbors than there are observations in the training set. Please choose a number lower than %d.", as.integer(valueToTest)))
  }
  
  # check for too many folds (folds > nTrain+validation) before the analysis starts
  checkIfFoldsExceedValidation <- function() {
    if (options[["modelValid"]] == "validationKFold")  {
      kFolds <- options[["noOfFolds"]]
      if (kFolds > nTrainAndValid)
        return(gettextf("You have specified more folds than there are observations in the training and validation set. Please choose a number lower than %d.", as.integer(nTrainAndValid + 1)))
    }
  }
  
  # check for too many observations in end nodes before the analysis starts
  checkMinObsNode <- function() {
    if (type != "boosting")
      return()
      
    procentTrain <- (1 - options[["testDataManual"]])
    if (options[["modelOpt"]] == "optimizationOOB")
      procentTrain <- procentTrain * (1 - options[["validationDataManual"]])
      
    nTrain <- nrow(dataset) * procentTrain
    bag.fraction <- options[["bagFrac"]]
    n.minobsinnode <- options[["nNode"]]
    if (nTrain * bag.fraction <= 2 * n.minobsinnode + 1)
      return(gettextf(
        "The minimum number of observations per node is too large. Ensure that `2 * Min. observations in node + 1` > `Training data used per tree * available training data` (in this case the minimum can be %.3f at most).",
        nTrain * bag.fraction / 2 - 1
      ))
  }
  
  return(list(checkNearestNeighbors, checkIfFoldsExceedValidation, checkMinObsNode))
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
                      "knn" = gettext("K-Nearest Neighbors Regression"),
                      "regularized" = gettext("Regularized Linear Regression"),
                      "randomForest" = gettext("Random Forest Regression"),
                      "boosting" = gettext("Boosting Regression"))

  regressionTable <- createJaspTable(title)
  regressionTable$position <- position
  regressionTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                          "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "maxTrees",
                                          "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac", "intDepth", "nNode", "distance",
                                          "testSetIndicatorVariable", "testSetIndicator", "validationDataManual","holdoutData", "testDataManual"))

  # Add analysis-specific columns
  if(type == "knn"){

    regressionTable$addColumnInfo(name = 'nn',       title = gettext('Nearest neighbors'), type = 'integer')
    regressionTable$addColumnInfo(name = 'weights',  title = gettext('Weights'),           type = 'string')
    regressionTable$addColumnInfo(name = 'distance', title = gettext('Distance'),          type = 'string')

  } else if(type == "regularized"){

    regressionTable$addColumnInfo(name = 'penalty', title = gettext('Penalty'), type = 'string')
    if(options[["penalty"]] == "elasticNet")
      regressionTable$addColumnInfo(name = 'alpha', title = '\u03B1', type = 'number')
    regressionTable$addColumnInfo(name = 'lambda', title = '\u03BB', type = 'number')

  } else if(type == "randomForest"){

    regressionTable$addColumnInfo(name = 'trees', title = gettext('Trees'),                type = 'integer')
    regressionTable$addColumnInfo(name = 'preds', title = gettext('Predictors per split'), type = 'integer')
  
  } else if(type == "boosting"){

    regressionTable$addColumnInfo(name = 'trees',        title = gettext('Trees'),         type = 'integer')
    regressionTable$addColumnInfo(name = 'shrinkage',    title = gettext('Shrinkage'),     type = 'number')
    regressionTable$addColumnInfo(name = 'distribution', title = gettext('Loss function'), type = 'integer')

  }

  # Add common columns
  regressionTable$addColumnInfo(name = 'ntrain', title = gettext('n(Train)'), type = 'integer')

  if(options[["modelOpt"]] != "optimizationManual")
    regressionTable$addColumnInfo(name = 'nvalid', title = gettext('n(Validation)'), type = 'integer')

  regressionTable$addColumnInfo(name = 'ntest', title = gettext('n(Test)'), type = 'integer')

  if(options[["modelOpt"]] != "optimizationManual")
    regressionTable$addColumnInfo(name = 'validMSE', title = gettext('Validation MSE'), type = 'number')

  regressionTable$addColumnInfo(name = 'testMSE', title = gettext('Test MSE'), type = 'number')

  # Add analysis-specific columns after common columns
  if(type == "randomForest"){
    regressionTable$addColumnInfo(name = 'oob', title = gettext('OOB Error'), type = 'number')
  }

# If no analysis is run, specify the required variables in a footnote
  requiredVars <- if(type == "knn") 1L else 2L
  if(!ready)
    regressionTable$addFootnote(gettextf("Please provide a target variable and at least %d predictor variable(s).", requiredVars))

  jaspResults[["regressionTable"]] <- regressionTable
  
  if(!ready)  return()

  .regressionMachineLearning(dataset, options, jaspResults, ready, type = type)

  regressionResult <- jaspResults[["regressionResult"]]$object

  nTrain <- regressionResult[["ntrain"]]

  if(options[["modelOpt"]] != "optimizationManual"){
    nValid <- regressionResult[["nvalid"]]

    if(options[["modelValid"]] == "validationKFold"){
      # Adjust displayed train and test size for cross-validation
      nValid <- floor(nValid / options[["noOfFolds"]])
      nTrain <- nTrain - nValid
    } else if(options[["modelValid"]] == "validationLeaveOneOut"){
      nValid <- 1
      nTrain <- nTrain - 1
    }
  }
  
  # Fill the table per analysis
  if(type == "knn"){

    if(options[["modelOpt"]] == "optimizationError")
      regressionTable$addFootnote(gettext("The model is optimized with respect to the <i>validation set mean squared error</i>."))

    if(regressionResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      regressionTable$addFootnote(gettext("The optimum number of nearest neighbors is the maximum number. You might want to adjust the range of optimization."))
    }

    distance  <- ifelse(regressionResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidean")    
    row <- data.frame(nn = regressionResult[["nn"]], 
                      weights = regressionResult[["weights"]], 
                      distance = distance, 
                      ntrain = nTrain,
                      ntest = regressionResult[["ntest"]],
                      testMSE = regressionResult[["testMSE"]])
    if(options[["modelOpt"]] != "optimizationManual")
      row <- cbind(row, nvalid = nValid, validMSE = regressionResult[["validMSE"]])
    regressionTable$addRows(row)

  } else if(type == "regularized"){

    if(options[["modelOpt"]] != "optimizationManual")
      regressionTable$addFootnote(gettext("The model is optimized with respect to the <i>validation set mean squared error</i>."))

    if (regressionResult[["lambda"]] == 0)
      regressionTable$addFootnote(gettextf("When %s is set to 0 linear regression is performed.", "\u03BB"))

    row <- data.frame(penalty = regressionResult[["penalty"]], 
                      lambda = regressionResult[["lambda"]], 
                      ntrain = nTrain, 
                      ntest = regressionResult[["ntest"]], 
                      testMSE = regressionResult[["testMSE"]])
    if(options[["modelOpt"]] != "optimizationManual")
      row <- cbind(row, nvalid = nValid, validMSE = regressionResult[["validMSE"]])
    if(options[["penalty"]] == "elasticNet")
      row <- cbind(row, alpha = regressionResult[["alpha"]])
    regressionTable$addRows(row)

  } else if(type == "randomForest"){

    if(options[["modelOpt"]] == "optimizationError")
      regressionTable$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag mean squared error</i>."))

    row <- data.frame(trees = regressionResult[["noOfTrees"]], 
                      preds = regressionResult[["predPerSplit"]], 
                      ntrain = nTrain,
                      ntest = regressionResult[["ntest"]],
                      testMSE = regressionResult[["testMSE"]], 
                      oob = regressionResult[["oobError"]])
    if(options[["modelOpt"]] != "optimizationManual")
      row <- cbind(row, nvalid = nValid, validMSE = regressionResult[["validMSE"]])
    regressionTable$addRows(row)

  } else if(type == "boosting"){

    if(options[["modelOpt"]] == "optimizationOOB")
      regressionTable$addFootnote(gettext("The model is optimized with respect to the <i>out-of-bag mean squared error</i>."))

    distribution <- .regressionGetDistributionFromDistance(options[["distance"]])
    row <- data.frame(trees = regressionResult[["noOfTrees"]], 
                      shrinkage = options[["shrinkage"]], 
                      distribution = distribution, 
                      ntrain = nTrain,
                      ntest = regressionResult[["ntest"]],
                      testMSE = regressionResult[["testMSE"]])
    if(options[["modelOpt"]] != "optimizationManual")
      row <- cbind(row, nvalid = nValid, validMSE = regressionResult[["validMSE"]])
    regressionTable$addRows(row)

  }
}

.regressionGetDistributionFromDistance <- function(distance) {
  return(switch(
    distance, 
    "tdist"    = gettext("t"), 
    "gaussian" = gettext("Gaussian"), 
    "laplace"  = gettext("Laplace")
  ))
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

  validationMeasures$addColumnInfo(name = "measures", title = "", type = "string")
  validationMeasures$addColumnInfo(name = "values", title = gettext("Value"), type = "string")

  measures <- c("MSE", "RMSE", "MAE", "MAPE", "R\u00B2")
  validationMeasures[["measures"]] <- measures
  
  jaspResults[["validationMeasures"]] <- validationMeasures

  if(!ready)  return()

  regressionResult <- jaspResults[["regressionResult"]]$object

  predDat <- data.frame(obs = unname(regressionResult[["testReal"]]), pred = unname(regressionResult[["testPred"]]))
  predDat <- predDat[complete.cases(predDat), ]
  obs <- predDat[["obs"]]
  pred <- predDat[["pred"]]

  mse <- round(regressionResult[["testMSE"]], 3)
  rmse <- round(sqrt(mse), 3)
  mae <- round(mean(abs(obs - pred)), 3)
  mape <- paste0(round(mean( abs((obs - pred) / obs) ) * 100, 2), "%")
  r_squared <- round(cor(obs, pred)^2, 3)

  values <- c(mse, rmse, mae, mape, r_squared)

  validationMeasures[["values"]] <- values

  if(is.na(r_squared))
    validationMeasures$addFootnote(gettextf("R%s cannot be computed due to lack of variance in the predictions.</i>", "\u00B2"))
  
}

.regressionPredictedPerformancePlot <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["predictedPerformancePlot"]]) || !options[["predictedPerformancePlot"]]) return()

  predictedPerformancePlot <- createJaspPlot(plot = NULL, title = gettext("Predictive Performance Plot"), width = 400, height = 300)
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
        ggplot2::scale_x_continuous(gettext("Observed test values"), breaks = allBreaks, labels = allBreaks) +
        ggplot2::scale_y_continuous(gettext("Predicted test values"), breaks = allBreaks, labels = allBreaks)
  p <- JASPgraphs::themeJasp(p)

  predictedPerformancePlot$plotObject <- p
}

.dataSplitPlot <- function(dataset, options, jaspResults, ready, position, purpose, type){

  if(!is.null(jaspResults[["dataSplitPlot"]]) || !options[["dataSplitPlot"]]) return()

  dataSplitPlot <- createJaspPlot(plot = NULL, title = gettext("Data Split"), width = 800, height = 70)
  dataSplitPlot$position <- position
  dataSplitPlot$dependOn(options = c("dataSplitPlot", "target", "predictors", "trainingDataManual", "modelValid", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual", "modelOpt"))
  jaspResults[["dataSplitPlot"]] <- dataSplitPlot

  if(!ready) return()

    result <- base::switch(purpose,
  						"classification" = jaspResults[["classificationResult"]]$object,
						  "regression" = jaspResults[["regressionResult"]]$object)
  
  if(options[["modelOpt"]] == "optimizationManual"){
    # For a fixed model, draw only a training and a test set

      nTrain    <- result[["ntrain"]]
      nTest     <- result[["ntest"]]

      d <- data.frame(y = c(nTrain, nTest), x = c("Train", "Test"), group = c(1,1))

      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = group, y = y, fill = factor(x, levels = c("Test", "Train")))) +
            ggplot2::geom_bar(stat = "identity", col = "black", size = 0.5) +
            ggplot2::scale_y_continuous(limits = c(0, nTrain + nTest + ((nTrain + nTest)/5))) + # adjust limits to include "Total" text
            ggplot2::coord_flip() +
            ggplot2::labs(fill = "") +
            ggplot2::xlab("") +
            ggplot2::ylab("") +
            ggplot2::scale_fill_manual(values = c("tomato2", "steelblue2")) +
            ggplot2::annotate("text", y = c(0, nTrain, nTrain + nTest), x = 1, label = c(gettextf("Train: %d", nTrain), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrain + nTest)), size = 4, vjust = 0.5, hjust = -0.1) 
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

      p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), 
                              axis.text.y = ggplot2::element_blank(), 
                              axis.text.x = ggplot2::element_blank())

  } else {
    # For an optimized model, draw a training, a validation, and a test set
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
            ggplot2::annotate("text", y = c(0, nTrain, nTrain + nValid, nTrain + nValid + nTest), x = 1, 
                              label = c(gettextf("Train: %d", nTrain), gettextf("Validation: %d", nValid), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrain + nValid + nTest)), 
                              size = 4, vjust = 0.5, hjust = -0.1) 
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
            ggplot2::annotate("text", y = c(0, nTrainAndValid, nTrainAndValid + nTest), x = 1, 
                              label = c(gettextf("Train and validation: %d", nTrainAndValid), gettextf("Test: %d", nTest), gettextf("Total: %d", nTrainAndValid + nTest)), 
                              size = 4, vjust = 0.5, hjust = -0.1) 
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

      p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

    }

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
  if (nrow(x) == 0)
    return(x)
  idx <- sapply(x, is.numeric)
  x[, idx] <- scale(x[, idx, drop = FALSE], center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale")  <- NULL
  return(x)
}

.scaleNumericData.matrix <- function(x, center = TRUE, scale = TRUE) {
  if (!is.numeric(x)) {
    warning(sprintf("Object passed to .scaleNumericData.matrix was not numeric!"))
    return(x)
  }
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

# fallback when .scaleNumericData is called with factor/ character data
.scaleNumericData.default <- function(x, center = TRUE, scale = TRUE) return(x)

.regressionAddValuesToData <- function(dataset, options, jaspResults, ready){
  if(!ready || !options[["addValues"]] || options[["valueColumn"]] == "")  return()

  regressionResult <- jaspResults[["regressionResult"]]$object

  if(is.null(jaspResults[["valueColumn"]])){
    predictions <- regressionResult[["values"]]
    valueColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    valueColumn[as.numeric(rownames(dataset))] <- predictions
    jaspResults[["valueColumn"]] <- createJaspColumn(columnName=options[["valueColumn"]])
    jaspResults[["valueColumn"]]$dependOn(options = c("valueColumn", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "maxTrees",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                                              "penalty", "alpha", "thresh", "intercept", "shrinkage", "lambda", "noOfTrees", "noOfPredictors", "numberOfPredictors", "bagFrac",
                                                              "intDepth", "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                                              "holdoutData", "testDataManual", "testIndicatorColumn"))
    jaspResults[["valueColumn"]]$setScale(valueColumn)
  }  
}

