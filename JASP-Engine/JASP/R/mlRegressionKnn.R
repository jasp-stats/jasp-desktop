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

mlRegressionKnn <- function(jaspResults, dataset, options, state=NULL) {

	# Preparatory work
	dataset <- .readDataRegressionAnalyses(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options, type = "knn")
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "knn")		

	# Compute results and create the model summary table
	.regressionMachineLearningTable(dataset, options, jaspResults, ready, position = 1, type = "knn")

	# If the user wants to add the values to the data set
  	.regressionAddValuesToData(dataset, options, jaspResults, ready)

	# Add test set indicator to data
  	.addTestIndicatorToData(options, jaspResults, ready, purpose = "regression")
	
	# Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "knn")

	# Create the evaluation metrics table
	.regressionEvaluationMetrics(dataset, options, jaspResults, ready, position = 3)

	# Create the mean squared error plot
	.knnErrorPlot(dataset, options, jaspResults, ready, position = 4, purpose = "regression")

	# Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 5)

}

.knnRegression <- function(dataset, options, jaspResults, ready){
	
	# Import model formula from jaspResults
	formula <- jaspResults[["formula"]]$object

	# Set model specific parameters
	weights <- options[["weights"]]
	distance <- options[["distanceParameterManual"]]

	# Split the data into training and test sets
	if(options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != ""){
		# Select observations according to a user-specified indicator (included when indicator = 1)
		train.index             <- which(dataset[,.v(options[["testSetIndicatorVariable"]])] == 0)
	} else {
		# Sample a percentage of the total data set
		train.index             <- sample.int(nrow(dataset), size = ceiling( (1 - options[['testDataManual']]) * nrow(dataset)))
	}
	trainAndValid           <- dataset[train.index, ]

	# Create the generated test set indicator
	testIndicatorColumn <- rep(1, nrow(dataset))
  	testIndicatorColumn[train.index] <- 0
	
	if(options[["modelOpt"]] == "optimizationManual"){
		# Just create a train and a test set (no optimization)
		train                   <- trainAndValid
		test                    <- dataset[-train.index, ]

		kfit_test <- kknn::kknn(formula = formula, train = train, test = test, k = options[['noOfNearestNeighbours']], 
			distance = distance, kernel = weights, scale = FALSE)
		nn <- options[['noOfNearestNeighbours']]

  	} else if(options[["modelOpt"]] == "optimizationError"){
		# Create a train, validation and test set (optimization)
		valid.index             <- sample.int(nrow(trainAndValid), size = ceiling(options[['validationDataManual']] * nrow(trainAndValid)))
		test                    <- dataset[-train.index, ]
		valid                   <- trainAndValid[valid.index, ]
		train                   <- trainAndValid[-valid.index, ]

		if(options[["modelValid"]] == "validationManual"){

			nnRange <- 1:options[["maxK"]]
			errorStore <- numeric(length(nnRange))
			trainErrorStore <- numeric(length(nnRange))
			startProgressbar(length(nnRange))

			for(i in nnRange){
				
				kfit_valid <- kknn::kknn(formula = formula, train = train, test = valid, k = i, 
					distance = distance, kernel = weights, scale = FALSE)
				errorStore[i] <- mean( (kfit_valid$fitted.values -  valid[,.v(options[["target"]])])^2 )
				kfit_train <- kknn::kknn(formula = formula, train = train, test = train, k = i, 
							distance = distance, kernel = weights, scale = FALSE)
				trainErrorStore[i] <- mean( (kfit_train$fitted.values -  train[,.v(options[["target"]])])^2 )
				progressbarTick()

			}

			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])
			kfit_test <- kknn::kknn(formula = formula, train = train, test = test, k = nn, 
						distance = distance, kernel = weights, scale = FALSE)

		} else if(options[["modelValid"]] == "validationKFold"){

			nnRange <- 1:options[["maxK"]]
			errorStore <- numeric(length(nnRange))
			startProgressbar(length(nnRange))

			for(i in nnRange){
				kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = distance, kernel = weights,
									kcv = options[['noOfFolds']], k = i)
				errorStore[i] <- mean( (kfit_valid[[1]][,1] -  kfit_valid[[1]][,2])^2 )
				progressbarTick()
			}

			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])

			kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = distance, kernel = weights,
					kcv = options[['noOfFolds']], k = nn)

			kfit_valid <- list(fitted.values = as.numeric(kfit_valid[[1]][, 2]))

			kfit_test <- kknn::kknn(formula = formula, train = trainAndValid, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

			train <- trainAndValid
			valid <- trainAndValid
			test <- test

		} else if(options[["modelValid"]] == "validationLeaveOneOut"){

			nnRange <- 1:options[["maxK"]]
      		kfit_valid <- kknn::train.kknn(formula = formula, data = trainAndValid, ks = nnRange, scale = FALSE, distance = distance, kernel = weights)   
			errorStore <- as.numeric(kfit_valid$MEAN.SQU)
			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])

			kfit_valid <- list(fitted.values = kfit_valid[["fitted.values"]][[1]])

			kfit_test <- kknn::kknn(formula = formula, train = trainAndValid, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

			train   <- trainAndValid
			valid   <- trainAndValid
			test    <- test

		}

  	}

	# Use the specified model to make predictions for dataset
	predictions <- predict(kknn::kknn(formula = formula, train = train, test = dataset, k = nn, distance = distance, kernel = weights, scale = FALSE))

	# Create results object
	regressionResult <- list()
	regressionResult[["formula"]]     	<- formula
	regressionResult[["model"]]       	<- kfit_test
	regressionResult[["nn"]]          	<- nn
	regressionResult[["weights"]]     	<- weights
	regressionResult[["distance"]]    	<- distance
	regressionResult[['testMSE']]     	<- mean( (kfit_test$fitted.values -  test[,.v(options[["target"]])])^2 )
	regressionResult[["ntrain"]]      	<- nrow(train)
	regressionResult[["ntest"]]       	<- nrow(test)
	regressionResult[["testReal"]]	  	<- test[, .v(options[["target"]])]
	regressionResult[["testPred"]]		<- kfit_test$fitted.values
	regressionResult[["testIndicatorColumn"]] <- testIndicatorColumn
	regressionResult[["values"]] 		<- predictions

	if(options[["modelOpt"]] != "optimizationManual"){
		regressionResult[["accuracyStore"]] <- errorStore
		regressionResult[['validMSE']]    <- mean( (kfit_valid$fitted.values -  valid[,.v(options[["target"]])])^2 )
		regressionResult[["nvalid"]]      <- nrow(valid)
		regressionResult[["valid"]]       <- valid

		if(options[["modelValid"]] == "validationManual")
			regressionResult[["trainAccuracyStore"]] <- trainErrorStore
	}

	return(regressionResult)
}

.knnErrorPlot <- function(dataset, options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotErrorVsK"]]) || !options[["plotErrorVsK"]] || options[["modelOpt"]] == "optimizationManual") return()

  plotTitle <- base::switch(purpose, "classification" = gettext("Classification Accuracy Plot"), "regression" = gettext("Mean Squared Error Plot"))

  plotErrorVsK <- createJaspPlot(plot = NULL, title = plotTitle, width = 500, height = 300)
  plotErrorVsK$position <- position
  plotErrorVsK$dependOn(options = c("plotErrorVsK","noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid",
															"testSetIndicatorVariable", "testSetIndicator", "validationDataManual", "holdoutData", "testDataManual"))
  jaspResults[["plotErrorVsK"]] <- plotErrorVsK

  if(!ready) return()

  result <- base::switch(purpose,
  						"classification" = jaspResults[["classificationResult"]]$object,
						"regression" = jaspResults[["regressionResult"]]$object)

  ylabel <- base::switch(purpose,
                "classification" = gettext("Classification Accuracy"),
                "regression"     = gettext("Mean Squared Error"))

  if(options[["modelValid"]] == "validationManual"){

    xvalues <- rep(1:options[["maxK"]], 2)
    yvalues1 <- result[["accuracyStore"]]  
    yvalues2 <- result[["trainAccuracyStore"]] 
    yvalues <- c(yvalues1, yvalues2)
    type <- rep(c(gettext("Validation set"), gettext("Training set")), each = length(yvalues1))
    d <- data.frame(x = xvalues, y = yvalues, type = type)

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, d$x), min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

    pointData <- data.frame(x = result[["nn"]], 
                            y = yvalues1[result[["nn"]]],
                            type = gettext("Validation set"))

    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type)) + 
			JASPgraphs::geom_line() +
			ggplot2::scale_x_continuous(name = gettext("Number of Nearest Neighbors"), breaks = xBreaks, labels = xBreaks, limits = c(0, max(xBreaks))) + 
			ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, labels = yBreaks) +
			ggplot2::labs(linetype = "") +
			ggplot2::scale_linetype_manual(values = c(2,1)) + 
			JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, linetype = type), fill = "red")
    p <- JASPgraphs::themeJasp(p, legend.position = "top")

  } else if(options[["modelValid"]] != "validationManual"){

    xvalues <- 1:options[["maxK"]]
    yvalues <- result[["accuracyStore"]]     
	type <- rep(gettext("Training and validation set"), each = length(xvalues))
    d <- data.frame(x = xvalues, y = yvalues, type = type)

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, d$x), min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)
      
    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type)) + 
			JASPgraphs::geom_line() +
			ggplot2::scale_x_continuous(name = gettext("Number of Nearest Neighbors"), breaks = xBreaks, labels = xBreaks, limits = c(0, max(xBreaks))) + 
			ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, labels = yBreaks) + 
			JASPgraphs::geom_point(ggplot2::aes(x = x, y = y, linetype = type), data = data.frame(x = result[["nn"]], y = yvalues[result[["nn"]]], type = gettext("Training and validation set")), fill = "red") +
			ggplot2::labs(linetype = "")
    p <- JASPgraphs::themeJasp(p, legend.position = "top")

  }

  plotErrorVsK$plotObject <- p
}

# kknn::kknn calls stats::model.matrix which needs these two functions and looks for them by name in the global namespace
contr.dummy   <- kknn::contr.dummy
contr.ordinal <- kknn::contr.ordinal
