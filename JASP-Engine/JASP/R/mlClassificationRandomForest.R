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

mlClassificationRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "randomForest")

  # Compute results and create the model summary table
  .classificationTable(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "randomForest")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .classificationClassProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the variable importance table
  .randomForestVariableImportance(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the trees vs model error plot
  .randomForestTreesErrorPlot(options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 8, type = "randomForest")

  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 9)

  # Create the mean decrease in accuracy plot
  .randomForestPlotDecreaseAccuracy(options, jaspResults, ready, position = 10, purpose = "classification")

  # Create the total increase in node purity plot
  .randomForestPlotIncreasePurity(options, jaspResults, ready, position = 11, purpose = "classification")

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 12, type = "randomForest")
  
}

.randomForestClassification <- function(dataset, options, jaspResults){
  
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
    oobAccuracy <- 1 - rfit_valid$err.rate[, 1]
    optimTrees <- which.max(oobAccuracy)

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

  # Calculate AUC
  lvls <- levels(factor(test[, .v(options[["target"]])]))
  auc <- numeric(length(lvls)) 

  predictorNames <- .v(options[["predictors"]])
  AUCformula <- formula(paste("levelVar", "~", paste(predictorNames, collapse=" + ")))

  for(i in 1:length(lvls)){

    levelVar <- train[,.v(options[["target"]])] == lvls[i]
    typeData <- cbind(train, levelVar = factor(levelVar))
    column <- which(colnames(typeData) == .v(options[["target"]]))
    typeData <- typeData[, -column]

    column <- which(colnames(typeData) == "levelVar")
    typeData <- typeData[, -column]
    rfit_auc <- randomForest::randomForest(x = typeData, y = factor(levelVar), ntree = noOfTrees, mtry = noOfPredictors,
                                            sampsize = ceiling(options[["bagFrac"]]*nrow(dataset)), importance = TRUE, keep.forest = TRUE)
    score <- predict(rfit_auc, test, type = "prob")[, 'TRUE']
    actual.class <- test[,.v(options[["target"]])] == lvls[i]

    pred <- ROCR::prediction(score, actual.class)
    auc[i] <- ROCR::performance(pred, "auc")@y.values[[1]]
  }

  # Create results object
  classificationResult <- list()
  classificationResult[["rfit_test"]]           <- rfit_test
  classificationResult[["rfit_valid"]]          <- rfit_valid
  classificationResult[["rfit_train"]]          <- rfit_train

  classificationResult[["noOfTrees"]]           <- noOfTrees
  classificationResult[["predPerSplit"]]        <- noOfPredictors
  classificationResult[["bagFrac"]]             <- ceiling(options[["bagFrac"]]*nrow(dataset))

  classificationResult[["validationConfTable"]] <- table('Pred' = rfit_valid$test[["predicted"]], 'Real' = valid[,.v(options[["target"]])])
  classificationResult[['validAcc']]            <- sum(diag(prop.table(classificationResult[['validationConfTable']])))
  classificationResult[['confTable']]           <- table('Pred' = rfit_test$test[["predicted"]], 'Real' = test[,.v(options[["target"]])])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["auc"]]                 <- auc

  classificationResult[["testPred"]]            <- rfit_test$test[["predicted"]]
  classificationResult[["testReal"]]            <- test[,.v(options[["target"]])]

  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["nvalid"]]              <- nrow(valid)
  classificationResult[["ntest"]]               <- nrow(test)

  classificationResult[["train"]]               <- train
  classificationResult[["valid"]]               <- valid
  classificationResult[["test"]]                <- test

  classificationResult[["oobAccuracy"]]            <- 1 - rfit_valid$err.rate[length(rfit_valid$err.rate)]
  classificationResult[["varImp"]]              <- plyr::arrange(data.frame(
                                                                  Variable = .unv(as.factor(names(rfit_valid$importance[,1]))),
                                                                  MeanIncrMSE  = rfit_valid$importance[, 1],
                                                                  TotalDecrNodeImp = rfit_valid$importance[, 2]
                                                                ), -TotalDecrNodeImp)

  if(options[["modelOpt"]] == "optmizationError")
   classificationResult[["oobValidStore"]] <- oobAccuracy

  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn

  return(classificationResult)
}
