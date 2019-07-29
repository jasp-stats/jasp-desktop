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

mlRegressionRandomForest <- function(jaspResults, dataset, options, ...) {
  
	# Preparatory work
	dataset <- .readDataRegressionAnalyses(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options)
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "randomForest")

  # Compute results and create the model summary table
	.regressionMachineLearningTable(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "randomForest")

  # Create the evaluation metrics table
	.regressionEvaluationMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the variable importance table
  .randomForestVariableImportance(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the trees vs model error plot
  .randomForestTreesErrorPlot(options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 6)

  # Create the mean decrease in accuracy plot
  .randomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 7, purpose = "regression")

  # Create the total increase in node purity plot
  .randomForestPlotIncreasePurity(options, jaspResults, ready, position = 8, purpose = "regression")

}

.randomForestRegression <- function(dataset, options, jaspResults){
  
  dataset                   <- na.omit(dataset)
  if(options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != ""){
    train.index             <- which(dataset[,.v(options[["testSetIndicatorVariable"]])] == 0)
  } else {
    train.index             <- sample.int(nrow(dataset), size = ceiling( (1 - options[['testDataManual']]) * nrow(dataset)))
  }
  trainAndValid           <- dataset[train.index, ]
  valid.index             <- sample.int(nrow(trainAndValid), size = ceiling(options[['validationDataManual']] * nrow(trainAndValid)))
  test                    <- dataset[-train.index, ]
  valid                   <- trainAndValid[valid.index, ]
  train                   <- trainAndValid[-valid.index, ]

  train_predictors <- train[, .v(options[["predictors"]])]
  train_target <- train[, .v(options[["target"]])]
  valid_predictors <- valid[, .v(options[["predictors"]])]
  valid_target <- valid[, .v(options[["target"]])]
  test_predictors <- test[, .v(options[["predictors"]])]
  test_target <- test[, .v(options[["target"]])]

  if(options[["noOfPredictors"]] == "manual") {
    noOfPredictors <- options[["numberOfPredictors"]]
  } else {
    noOfPredictors <- floor(sqrt(length(options[["predictors"]])))
  }

  if(options[["modelOpt"]] == "optimizationManual"){

      rfit_valid <- randomForest::randomForest(x = train_predictors, y = train_target, xtest = valid_predictors, ytest = valid_target,
                                              ntree = options[["noOfTrees"]], mtry = noOfPredictors,
                                              sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                              importance = TRUE, keep.forest = TRUE)
      rfit_test <- randomForest::randomForest(x = train_predictors, y = train_target, xtest = test_predictors, ytest = test_target,
                                              ntree = options[["noOfTrees"]], mtry = noOfPredictors,
                                              sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                              importance = TRUE, keep.forest = TRUE)
      noOfTrees <- options[["noOfTrees"]]

  } else if(options[["modelOpt"]] == "optimizationError"){

    rfit_valid <- randomForest::randomForest(x = train_predictors, y = train_target, xtest = valid_predictors, ytest = valid_target,
                                        ntree = options[["maxTrees"]], mtry = noOfPredictors,
                                        sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                        importance = TRUE, keep.forest = TRUE)
    oobError <- rfit_valid$mse
    optimTrees <- which.min(oobError)[length(which.min(oobError))]

    rfit_test <- randomForest::randomForest(x = train_predictors, y = train_target, xtest = test_predictors, ytest = test_target,
                                            ntree = optimTrees, mtry = noOfPredictors,
                                            sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                            importance = TRUE, keep.forest = TRUE)

    noOfTrees <- optimTrees

  }

  rfit_train <- randomForest::randomForest(x = train_predictors, y = train_target, xtest = train_predictors, ytest = train_target,
                                    ntree = noOfTrees, mtry = noOfPredictors,
                                    sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)),
                                    importance = TRUE, keep.forest = TRUE)

  # Create results object
  regressionResult <- list()
  regressionResult[["rfit_test"]]           <- rfit_test
  regressionResult[["rfit_valid"]]          <- rfit_valid
  regressionResult[["rfit_train"]]          <- rfit_train

  regressionResult[["noOfTrees"]]           <- noOfTrees
  regressionResult[["predPerSplit"]]        <- noOfPredictors
  regressionResult[["bagFrac"]]             <- ceiling(options[["bagFrac"]]*nrow(dataset))

  regressionResult[["validMSE"]]            <- mean((rfit_valid$test[["predicted"]] - valid[,.v(options[["target"]])])^2)
  regressionResult[["testMSE"]]             <- mean((rfit_test$test[["predicted"]] - test[,.v(options[["target"]])])^2)

  regressionResult[["testPred"]]            <- rfit_test$test[["predicted"]]
  regressionResult[["testReal"]]            <- test[,.v(options[["target"]])]

  regressionResult[["oobError"]]            <- rfit_test$mse[length(rfit_test$mse)]
  regressionResult[["varImp"]]              <- plyr::arrange(data.frame(
                                                              Variable = .unv(as.factor(names(rfit_test$importance[,1]))),
                                                              MeanIncrMSE = rfit_test$importance[, 1],
                                                              TotalDecrNodeImp = rfit_test$importance[, 2]
                                                            ), -TotalDecrNodeImp)

  regressionResult[["ntrain"]]              <- nrow(train)
  regressionResult[["nvalid"]]              <- nrow(valid)
  regressionResult[["ntest"]]               <- nrow(test)

  regressionResult[["train"]]               <- train
  regressionResult[["valid"]]               <- valid
  regressionResult[["test"]]                <- test

  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0
  regressionResult[["testIndicatorColumn"]] <- testIndicatorColumn
   
  return(regressionResult)
}

.randomForestVariableImportance <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["tableVariableImportance"]]) || !options[["tableVariableImportance"]]) return()
  
  tableVariableImportance <- createJaspTable(title = "Variable Importance")
  tableVariableImportance$position <- position
  tableVariableImportance$dependOn(options = c("tableVariableImportance", "scaleEqualSD", "target", "predictors", "modelOpt", "maxTrees",
                                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox",
                                                "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))

  tableVariableImportance$addColumnInfo(name = "predictor",  title = " ", type = "string")
  tableVariableImportance$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number")
  tableVariableImportance$addColumnInfo(name = "MDiNI",  title = "Total increase in node purity", type = "number")
  
  jaspResults[["tableVariableImportance"]] <- tableVariableImportance

  if(!ready)  return()

  result <- base::switch(purpose,
                          "classification" = jaspResults[["classificationResult"]]$object,
                          "regression" = jaspResults[["regressionResult"]]$object)

  varImpOrder <- sort(result[["rfit_test"]]$importance[,1], decr = TRUE, index.return = TRUE)$ix
  
  tableVariableImportance[["predictor"]] <- .unv(.v(result[["varImp"]]$Variable))
  tableVariableImportance[["MDiA"]]      <- result[["varImp"]]$MeanIncrMSE    
  tableVariableImportance[["MDiNI"]]     <- result[["varImp"]]$TotalDecrNodeImp
  
}

.randomForestTreesErrorPlot <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotTreesVsModelError"]]) || !options[["plotTreesVsModelError"]]) return()

  title <- base::switch(purpose, "classification" = "Out-of-bag Classification Accuracy Plot", "regression" = "Out-of-bag Mean Squared Error Plot")

  plotTreesVsModelError <- createJaspPlot(plot = NULL, title = title, width = 500, height = 300)
  plotTreesVsModelError$position <- position
  plotTreesVsModelError$dependOn(options = c("plotTreesVsModelError", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
                                            "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))
  jaspResults[["plotTreesVsModelError"]] <- plotTreesVsModelError

  if(!ready) return()

  result <- base::switch(purpose,
                  "classification" = jaspResults[["classificationResult"]]$object,
                  "regression" = jaspResults[["regressionResult"]]$object)
  xTitle <- base::switch(purpose,
                          "classification" = "Out-of-bag \nClassification Accuracy",
                          "regression" = "Out-of-bag \nMean Squared Error")

  values <- base::switch(purpose,
                        "classification" = 1 - result[["rfit_valid"]]$err.rate[1:result[["noOfTrees"]],1],
                        "regression" = result[["rfit_valid"]]$mse[1:result[["noOfTrees"]]])

  values2 <- base::switch(purpose,
                          "classification" = 1 - result[["rfit_train"]]$err.rate[,1],
                          "regression" = result[["rfit_train"]]$mse)
  values <- c(values, values2)

  treesMSE <- data.frame(
    trees = rep(1:length(values2), 2),
    error = values, 
    type = rep(c("Validation set", "Training set"), each = length(values2))
  )

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["trees"]], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(treesMSE[["error"]], min.n = 4)
  
  p <- ggplot2::ggplot(data = treesMSE, mapping = ggplot2::aes(x = trees, y = error, linetype = type)) +
        JASPgraphs::geom_line()

  p <- p + ggplot2::scale_x_continuous(name = "Number of Trees", labels = xBreaks, breaks = xBreaks) +
            ggplot2::scale_y_continuous(name = xTitle, labels = yBreaks, breaks = yBreaks) +
            ggplot2::labs(linetype = "") +
            ggplot2::scale_linetype_manual(values = c(2,1))
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  plotTreesVsModelError$plotObject <- p
}

.randomForestPlotDecreaseAccuracy <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotDecreaseAccuracy"]]) || !options[["plotDecreaseAccuracy"]]) return()

  plotDecreaseAccuracy <- createJaspPlot(plot = NULL, title = "Mean Decrease in Accuracy", width = 500, height = 300)
  plotDecreaseAccuracy$position <- position
  plotDecreaseAccuracy$dependOn(options = c("plotDecreaseAccuracy", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
                                            "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))
  jaspResults[["plotDecreaseAccuracy"]] <- plotDecreaseAccuracy

  if(!ready) return()

  result <- base::switch(purpose,
                        "classification" = jaspResults[["classificationResult"]]$object,
                        "regression" = jaspResults[["regressionResult"]]$object)
  
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::labs(x = "", y = "Mean Decrease in Accuracy")
  p <-JASPgraphs::themeJasp(p, horizontal = TRUE, xAxis = FALSE) + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  
  plotDecreaseAccuracy$plotObject <- p
}

.randomForestPlotIncreasePurity <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotIncreasePurity"]]) || !options[["plotIncreasePurity"]]) return()

  plotIncreasePurity <- createJaspPlot(plot = NULL, title = "Total Increase in Node Purity", width = 500, height = 300)
  plotIncreasePurity$position <- position
  plotIncreasePurity$dependOn(options = c("plotIncreasePurity", "trainingDataManual", "scaleEqualSD", "modelOpt", "maxTrees",
                                            "target", "predictors", "seed", "seedBox", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors",
                                            "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))
  jaspResults[["plotIncreasePurity"]] <- plotIncreasePurity

  if(!ready) return()

  result <- base::switch(purpose,
                      "classification" = jaspResults[["classificationResult"]]$object,
                      "regression" = jaspResults[["regressionResult"]]$object)
  
  p <- ggplot2::ggplot(result[["varImp"]], ggplot2::aes(x = reorder(Variable, TotalDecrNodeImp), y = TotalDecrNodeImp)) +
        ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
        ggplot2::labs(x = "", y = "Total Increase in Node Purity")
  p <- JASPgraphs::themeJasp(p, horizontal = TRUE, xAxis = FALSE) + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  plotIncreasePurity$plotObject <- p
}
