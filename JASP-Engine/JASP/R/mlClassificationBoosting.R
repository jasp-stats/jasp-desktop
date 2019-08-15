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

mlClassificationBoosting <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "boosting")

  # Compute results and create the model summary table
  .classificationTable(dataset, options, jaspResults, ready, position = 1, type = "boosting")

  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "boosting")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .classificationClassProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the relative influence table
  .boostingRelativeInfluenceTable(options, jaspResults, ready, position = 6, purpose = "classification")

  # Create the OOB improvement plot
  .boostingOOBimprovementPlot(options, jaspResults, ready, position = 7, purpose = "classification")

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 8, type = "boosting")

  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 9)

  # Create the deviance plot
  .boostingDeviancePlot(options, jaspResults, ready, position = 10, purpose = "classification")

  # Create the relative influence plot
  .boostingRelativeInfluencePlot(options, jaspResults, ready, position = 11, purpose = "classification")

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 12, type = "boosting")
  
}

.boostingClassification <- function(dataset, options, jaspResults){

  formula <- jaspResults[["formula"]]$object
  
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


  if(options[["modelValid"]] == "validationManual"){
    noOfFolds <- 0
  } else if(options[["modelValid"]] == "validationKFold"){
    noOfFolds <- options[["noOfFolds"]]
    train <- trainAndValid
    valid <- trainAndValid
  }

  assignFunctionInPackage(fakeGbmCrossValModelBuild, "gbmCrossValModelBuild", "gbm")
  assignFunctionInPackage(fakeGbmCrossValErr,        "gbmCrossValErr",        "gbm")
  # gbm expects the columns in the data to be in the same order as the variables...
  train <- train[, match(names(train), all.vars(formula))]

  trees <- base::switch(options[["modelOpt"]], "optimizationManual" = options[["noOfTrees"]], "optimizationOOB" = options[["maxTrees"]])

  bfit <- gbm::gbm(formula = formula, data = train, n.trees = trees,
                          shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                          cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                          distribution = "multinomial", n.cores = 1, keep.data = TRUE) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

  if(options[["modelOpt"]] == "optimizationManual"){
    
    noOfTrees <- options[["noOfTrees"]]

  } else if(options[["modelOpt"]] == "optimizationOOB"){

    noOfTrees <- gbm::gbm.perf(bfit, plot.it = FALSE, method = "OOB")[1]
    bfit <- gbm::gbm(formula = formula, data = train, n.trees = noOfTrees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

  }

  probs_valid <- gbm::predict.gbm(bfit, newdata = valid, n.trees = noOfTrees, type = "response")
  pred_valid <- colnames(probs_valid)[apply(probs_valid, 1, which.max)]

  probs_test <- gbm::predict.gbm(bfit, newdata = test, n.trees = noOfTrees, type = "response")
  pred_test <- colnames(probs_test)[apply(probs_test, 1, which.max)]

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
    levelVar <- as.character(levelVar)
    levelVar[levelVar == "TRUE"] <- 1
    levelVar[levelVar == "FALSE"] <- 0
    levelVar <- as.numeric(levelVar)
    column <- which(colnames(typeData) == "levelVar")
    typeData <- typeData[, -column]
    typeData <- cbind(typeData, levelVar = levelVar)

    bfit <- gbm::gbm(formula = AUCformula, data = typeData, n.trees = noOfTrees,
                shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                distribution = "bernoulli", n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
    score <- predict(bfit, newdata = test, n.trees = noOfTrees, type = "response")
    actual.class <- test[,.v(options[["target"]])] == lvls[i]

    pred <- ROCR::prediction(score, actual.class)
    auc[i] <- ROCR::performance(pred, "auc")@y.values[[1]]
  }


  # Create results object
  classificationResult <- list()
  classificationResult[["model"]]               <- bfit
  classificationResult[["formula"]]             <- formula
  classificationResult[["noOfFolds"]]           <- noOfFolds
  classificationResult[["noOfTrees"]]           <- noOfTrees

  classificationResult[["validationConfTable"]] <- table('Pred' = pred_valid, 'Real' = valid[,.v(options[["target"]])])
  classificationResult[['validAcc']]            <- sum(diag(prop.table(classificationResult[['validationConfTable']])))
  classificationResult[['confTable']]           <- table('Pred' = pred_test, 'Real' = test[,.v(options[["target"]])])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["relInf"]]              <- summary(bfit, plot = FALSE)
  classificationResult[["auc"]]                 <- auc
  
  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["nvalid"]]              <- nrow(valid)
  classificationResult[["ntest"]]               <- nrow(test)

  classificationResult[["testPred"]]            <- pred_test
  classificationResult[["testReal"]]            <- test[,.v(options[["target"]])]

  classificationResult[["train"]]               <- train
  classificationResult[["valid"]]               <- valid
  classificationResult[["test"]]                <- test

  classificationResult[["method"]]      <- ifelse(options[["modelValid"]] == "validationManual", yes = "OOB", no = "")

  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn

  return(classificationResult)
}
