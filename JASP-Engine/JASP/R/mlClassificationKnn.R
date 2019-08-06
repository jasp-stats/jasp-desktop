#
# Copyright (C) 2017 University of Amsterdam
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

mlClassificationKnn <- function(jaspResults, dataset, options, ...) {
  
    # Preparatory work
    dataset <- .readDataClassificationAnalyses(dataset, options)
    .errorHandlingClassificationAnalyses(dataset, options)
    
    # Check if analysis is ready to run
    ready <- .classificationAnalysesReady(options, type = "knn")
    
    # Compute results and create the model summary table
    .classificationTable(dataset, options, jaspResults, ready, position = 1, type = "knn")

    # Add test set indicator to data
    .addTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

    # Create the data split plot
	  .dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "knn")
    
    # Create the confusion table
    .classificationConfusionTable(dataset, options, jaspResults, ready, position = 3)

    # Create the class proportions table
    .classificationClassProportions(dataset, options, jaspResults, ready, position = 4)

    # Create the validation measures table
    .classificationEvaluationMetrics(dataset, options, jaspResults, ready, position = 5)
    
    # Create the classification error plot
    .knnErrorPlot(dataset, options, jaspResults, ready, position = 6, purpose = "classification")

    # Create the ROC curve
    .rocCurve(dataset, options, jaspResults, ready, position = 7, type = "knn")

    # Create the Andrews curves
    .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 8)

    # Decision boundaries
    .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 9, type = "knn")
    
}

.knnClassification <- function(dataset, options, jaspResults){

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

    if(options[["modelOpt"]] == "optimizationManual"){

        kfit_valid <- kknn::kknn(formula = formula, train = train, test = valid, k = options[['noOfNearestNeighbours']], 
            distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
        kfit_test <- kknn::kknn(formula = formula, train = train, test = test, k = options[['noOfNearestNeighbours']], 
                    distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
        nn <- options[['noOfNearestNeighbours']]

    } else { 

      nnRange             <- 1:options[["maxK"]]
      accuracyStore       <- numeric(length(nnRange))
      trainAccuracyStore  <- numeric(length(nnRange))
      startProgressbar(length(nnRange))

      for(i in nnRange){
          kfit_valid <- kknn::kknn(formula = formula, train = train, test = valid, k = i, 
              distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
          accuracyStore[i] <- sum(diag(prop.table(table(kfit_valid$fitted.values, valid[,.v(options[["target"]])]))))
          kfit_train <- kknn::kknn(formula = formula, train = train, test = train, k = i, 
				      distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
			    trainAccuracyStore[i] <- sum(diag(prop.table(table(kfit_train$fitted.values, train[,.v(options[["target"]])]))))
          progressbarTick()
      }

      nn <- base::switch(options[["modelOpt"]],
                          "optimizationError" = nnRange[which.max(accuracyStore)])
      kfit_test <- kknn::kknn(formula = formula, train = train, test = test, k = nn, 
                distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)

    }

    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]

  } else if(options[["modelValid"]] == "validationLeaveOneOut"){

    if(options[["modelOpt"]] == "optimizationManual"){

      kfit_valid <- kknn::train.kknn(formula = formula, data = trainAndValid, ks = options[['noOfNearestNeighbours']], scale = FALSE, distance = options[['distanceParameterManual']], kernel = options[['weights']])
      nn <- options[['noOfNearestNeighbours']]

    } else {

      nnRange <- 1:options[["maxK"]]
      kfit_valid <- kknn::train.kknn(formula = formula, data = trainAndValid, ks = nnRange, scale = FALSE, distance = options[['distanceParameterManual']], kernel = options[['weights']])  
      accuracyStore <- as.numeric(1 - kfit_valid$MISCLASS)
      nn <- base::switch(options[["modelOpt"]],
                            "optimizationError" = nnRange[which.max(accuracyStore)])

    }

    kfit_valid <- list(fitted.values = kfit_valid[["fitted.values"]][[1]])

    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]

    kfit_test <- kknn::kknn(formula = formula, train = trainAndValid, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

    train   <- trainAndValid
    valid   <- trainAndValid
		test    <- test

  } else if(options[["modelValid"]] == "validationKFold"){

    if(options[["modelOpt"]] == "optimizationManual"){

      kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = options[['noOfNearestNeighbours']])
      nn <- options[['noOfNearestNeighbours']]

    } else {

      nnRange <- 1:options[["maxK"]]
      accuracyStore <- numeric(length(nnRange))
      startProgressbar(length(nnRange))

      for(i in nnRange){

          kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = i)
          accuracyStore[i] <- sum(diag(prop.table(table(kfit_valid[[1]][,1], kfit_valid[[1]][,2]))))
          progressbarTick()

      }

      nn <- base::switch(options[["modelOpt"]],
                        "optimizationError" = nnRange[which.max(accuracyStore)])

      kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = options[['distanceParameterManual']], kernel = options[['weights']],
                            kcv = options[['noOfFolds']], k = nn)
    }
      
    kfit_valid <- list(fitted.values = kfit_valid[[1]][, 2])

    weights <- options[["weights"]]
    distance <- options[["distanceParameterManual"]]

    kfit_test <- kknn::kknn(formula = formula, train = trainAndValid, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

    train <- trainAndValid
    valid <- trainAndValid
    test <- test

  }

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

    kfit_auc <- kknn::kknn(formula = AUCformula, train = typeData, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

    score <- predict(kfit_auc, test, type = 'prob')[, 'TRUE']
    actual.class <- test[,.v(options[["target"]])] == lvls[i]

    pred <- ROCR::prediction(score, actual.class)
    auc[i] <- ROCR::performance(pred, "auc")@y.values[[1]]
  }

  # Create results object
  classificationResult <- list()

  classificationResult[["formula"]]             <- formula
  classificationResult[["model"]]               <- kfit_test
  classificationResult[["nn"]]                  <- nn
  classificationResult[["weights"]]             <- weights
  classificationResult[["distance"]]            <- distance

  classificationResult[["validationConfTable"]] <- table('Pred' = kfit_valid$fitted.values, 'Real' = valid[,.v(options[["target"]])])
  classificationResult[['validAcc']]            <- sum(diag(prop.table(classificationResult[['validationConfTable']])))
  classificationResult[['confTable']]           <- table('Pred' = kfit_test$fitted.values, 'Real' = test[,.v(options[["target"]])])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["auc"]]                 <- auc
  
  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["ntest"]]               <- nrow(test)
  classificationResult[["nvalid"]]              <- nrow(valid)

  classificationResult[["testReal"]]            <- test[,.v(options[["target"]])]
  classificationResult[["testPred"]]            <- kfit_test$fitted.values

  classificationResult[["train"]]               <- train 
  classificationResult[["test"]]                <- test 
  classificationResult[["valid"]]               <- valid

  if(options[["modelOpt"]] == "optimizationError")
    classificationResult[["accuracyStore"]] <- accuracyStore
  if(options[["modelOpt"]] == "optimizationError" && options[["modelValid"]] == "validationManual")
		classificationResult[["trainAccuracyStore"]] <- trainAccuracyStore

  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn

  return(classificationResult)
}