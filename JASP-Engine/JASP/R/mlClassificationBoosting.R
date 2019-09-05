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

  # If the user wants to add the classes to the data set
  .classificationAddClassesToData(dataset, options, jaspResults, ready)

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

  assignFunctionInPackage(fakeGbmCrossValModelBuild, "gbmCrossValModelBuild", "gbm")
  assignFunctionInPackage(fakeGbmCrossValErr,        "gbmCrossValErr",        "gbm")

  # Import model formula from jaspResults
  formula <- jaspResults[["formula"]]$object

  # Set model-specific parameters
  trees <- base::switch(options[["modelOpt"]], "optimizationManual" = options[["noOfTrees"]], "optimizationOOB" = options[["maxTrees"]])

  # Split the data into training and test sets
  if(options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != ""){
    # Select observations according to a user-specified indicator (included when indicator = 1)
    train.index             <- which(dataset[,.v(options[["testSetIndicatorVariable"]])] == 0)
  } else {
    # Sample a percentage of the total data set
    train.index             <- sample.int(nrow(dataset), size = ceiling( (1 - options[['testDataManual']]) * nrow(dataset)))
  }
  trainAndValid             <- dataset[train.index, ]

  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0

  # gbm expects the columns in the data to be in the same order as the variables...
  trainAndValid <- trainAndValid[, match(names(trainAndValid), all.vars(formula))]

  if(options[["modelOpt"]] == "optimizationManual"){
    # Just create a train and a test set (no optimization)
    train                   <- trainAndValid
    test                    <- dataset[-train.index, ]
    noOfFolds               <- 0

    bfit <- gbm::gbm(formula = formula, data = train, n.trees = trees,
                      shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                      cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                      distribution = "multinomial", n.cores = 1, keep.data = TRUE) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
 
    noOfTrees <- options[["noOfTrees"]]

  } else if(options[["modelOpt"]] == "optimizationOOB"){

    # Create a train, validation and test set (optimization)
    valid.index             <- sample.int(nrow(trainAndValid), size = ceiling(options[['validationDataManual']] * nrow(trainAndValid)))
    test                    <- dataset[-train.index, ]
    valid                   <- trainAndValid[valid.index, ]
    train                   <- trainAndValid[-valid.index, ]

    if(options[["modelValid"]] == "validationManual"){
      noOfFolds             <- 0
    } else if(options[["modelValid"]] == "validationKFold"){
      noOfFolds             <- options[["noOfFolds"]]
      train                 <- trainAndValid
      valid                 <- trainAndValid
    }

    bfit <- gbm::gbm(formula = formula, data = train, n.trees = trees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores = 1, keep.data = TRUE) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

    noOfTrees <- gbm::gbm.perf(bfit, plot.it = FALSE, method = "OOB")[1]
    bfit <- gbm::gbm(formula = formula, data = train, n.trees = noOfTrees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

    probs_valid <- gbm::predict.gbm(bfit, newdata = valid, n.trees = noOfTrees, type = "response")
    pred_valid <- colnames(probs_valid)[apply(probs_valid, 1, which.max)]
  }

  # Calculate AUC
  auc <- .classificationCalcAUC(test, train, options, "boostingClassification", noOfFolds=noOfFolds, noOfTrees=noOfTrees)

  # Use the specified model to make predictions for dataset
  probs_data <- gbm::predict.gbm(bfit, newdata = dataset, n.trees = noOfTrees, type = "response")
  predictions <- colnames(probs_data)[apply(probs_data, 1, which.max)]

  # Predictions for test set
  pred_test <- predictions[-train.index]

  # Create results object
  classificationResult <- list()
  classificationResult[["model"]]               <- bfit
  classificationResult[["formula"]]             <- formula
  classificationResult[["noOfFolds"]]           <- noOfFolds
  classificationResult[["noOfTrees"]]           <- noOfTrees
  classificationResult[['confTable']]           <- table('Pred' = pred_test, 'Real' = test[,.v(options[["target"]])])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["relInf"]]              <- summary(bfit, plot = FALSE)
  classificationResult[["auc"]]                 <- auc 
  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["ntest"]]               <- nrow(test)
  classificationResult[["testPred"]]            <- pred_test
  classificationResult[["testReal"]]            <- test[,.v(options[["target"]])]
  classificationResult[["train"]]               <- train
  classificationResult[["test"]]                <- test
  classificationResult[["method"]]              <- ifelse(options[["modelValid"]] == "validationManual", yes = "OOB", no = "")
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn
  classificationResult[["classes"]]             <- predictions

  if(options[["modelOpt"]] != "optimizationManual"){
    classificationResult[["validationConfTable"]] <- table('Pred' = pred_valid, 'Real' = valid[,.v(options[["target"]])])
    classificationResult[['validAcc']]            <- sum(diag(prop.table(classificationResult[['validationConfTable']])))
    classificationResult[["nvalid"]]              <- nrow(valid)
    classificationResult[["valid"]]               <- valid
  }

  return(classificationResult)
}
