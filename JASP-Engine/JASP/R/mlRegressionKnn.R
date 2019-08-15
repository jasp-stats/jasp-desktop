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
	.errorHandlingRegressionAnalyses(dataset, options)
	
	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "knn")		

	# Compute results and create the model summary table
	.regressionMachineLearningTable(dataset, options, jaspResults, ready, position = 1, type = "knn")

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

			nnRange <- 1:options[["maxK"]]
			errorStore <- numeric(length(nnRange))
			trainErrorStore <- numeric(length(nnRange))
			startProgressbar(length(nnRange))

			for(i in nnRange){
				
				kfit_valid <- kknn::kknn(formula = formula, train = train, test = valid, k = i, 
					distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
				errorStore[i] <- mean( (kfit_valid$fitted.values -  valid[,.v(options[["target"]])])^2 )
				kfit_train <- kknn::kknn(formula = formula, train = train, test = train, k = i, 
							distance = options[['distanceParameterManual']], kernel = options[['weights']], scale = FALSE)
				trainErrorStore[i] <- mean( (kfit_train$fitted.values -  train[,.v(options[["target"]])])^2 )
				progressbarTick()

			}

			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])
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
			errorStore <- as.numeric(kfit_valid$MEAN.SQU)
			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])

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
			errorStore <- numeric(length(nnRange))
			startProgressbar(length(nnRange))

			for(i in nnRange){
				kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = options[['distanceParameterManual']], kernel = options[['weights']],
									kcv = options[['noOfFolds']], k = i)
				errorStore[i] <- mean( (kfit_valid[[1]][,1] -  kfit_valid[[1]][,2])^2 )
				progressbarTick()
			}

			nn <- base::switch(options[["modelOpt"]],
								"optimizationError" = nnRange[which.min(errorStore)])

			kfit_valid <- kknn::cv.kknn(formula = formula, data = trainAndValid, distance = options[['distanceParameterManual']], kernel = options[['weights']],
					kcv = options[['noOfFolds']], k = nn)

		}

		kfit_valid <- list(fitted.values = as.numeric(kfit_valid[[1]][, 2]))

		weights <- options[["weights"]]
		distance <- options[["distanceParameterManual"]]

		kfit_test <- kknn::kknn(formula = formula, train = trainAndValid, test = test, k = nn, distance = distance, kernel = weights, scale = FALSE)

		train <- trainAndValid
		valid <- trainAndValid
		test <- test

	}

	# Create results object
	regressionResult <- list()
	regressionResult[["formula"]]     	<- formula
	regressionResult[["model"]]       	<- kfit_test
	regressionResult[["nn"]]          	<- nn
	regressionResult[["weights"]]     	<- weights
	regressionResult[["distance"]]    	<- distance

	regressionResult[['validMSE']]    	<- mean( (kfit_valid$fitted.values -  valid[,.v(options[["target"]])])^2 )
	regressionResult[['testMSE']]     	<- mean( (kfit_test$fitted.values -  test[,.v(options[["target"]])])^2 )

	regressionResult[["ntrain"]]      	<- nrow(train)
	regressionResult[["nvalid"]]      	<- nrow(valid)
	regressionResult[["ntest"]]       	<- nrow(test)

	regressionResult[["testReal"]]	  	<- test[, .v(options[["target"]])]
	regressionResult[["testPred"]]		<- kfit_test$fitted.values

	if(options[["modelOpt"]] == "optimizationError")
		regressionResult[["accuracyStore"]] <- errorStore
	if(options[["modelOpt"]] == "optimizationError" && options[["modelValid"]] == "validationManual")
		regressionResult[["trainAccuracyStore"]] <- trainErrorStore

	testIndicatorColumn <- rep(1, nrow(dataset))
  	testIndicatorColumn[train.index] <- 0
  	regressionResult[["testIndicatorColumn"]] <- testIndicatorColumn

	return(regressionResult)
}

.knnErrorPlot <- function(dataset, options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotErrorVsK"]]) || !options[["plotErrorVsK"]] || options[["modelOpt"]] != "optimizationError") return()

  plotTitle <- base::switch(purpose, "classification" = "Classification Accuracy Plot", "regression" = "Mean Squared Error Plot")

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
  							"classification" = "Classification Accuracy",
							"regression" = "Mean Squared Error")

  if(options[["modelOpt"]] == "optimizationError" && options[["modelValid"]] == "validationManual"){

    xvalues <- rep(1:options[["maxK"]], 2)
    yvalues1 <- result[["accuracyStore"]]  
    yvalues2 <- result[["trainAccuracyStore"]] 
    yvalues <- c(yvalues1, yvalues2)
    type <- rep(c("Validation set", "Training set"), each = length(yvalues1))
    d <- data.frame(x = xvalues, y = yvalues, type = type)

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

    pointData <- data.frame(x = result[["nn"]], 
                            y = yvalues1[result[["nn"]]],
                            type = "Validation set")

    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type)) + 
           JASPgraphs::geom_line()

    p <- p + ggplot2::scale_x_continuous(name = "Number of Nearest Neighbors", breaks = xBreaks, labels = xBreaks) + 
              ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, labels = yBreaks) +
              ggplot2::labs(linetype = "") +
			  ggplot2::scale_linetype_manual(values = c(2,1))
    p <- p + JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, linetype = type), fill = "red")
    p <- JASPgraphs::themeJasp(p, legend.position = "top")

  } else {

    xvalues <- 1:options[["maxK"]]
    yvalues <- result[["accuracyStore"]]      
    d <- data.frame(x = xvalues, y = yvalues)
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)
      
    p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) + 
          JASPgraphs::geom_line()

    p <- p + ggplot2::scale_x_continuous(name = "Number of Nearest Neighbors", breaks = xBreaks, labels = xBreaks) + 
              ggplot2::scale_y_continuous(name = ylabel, breaks = yBreaks, labels = yBreaks) + 
              JASPgraphs::geom_point(ggplot2::aes(x = x, y = y), data = data.frame(x = result[["nn"]], y = yvalues[result[["nn"]]]), fill = "red")
    p <- JASPgraphs::themeJasp(p)

  }

  plotErrorVsK$plotObject <- p
  
}

# kknn::kknn calls stats::model.matrix which needs these two functions and looks for them by name in the global namespace
contr.dummy   <- kknn::contr.dummy
contr.ordinal <- kknn::contr.ordinal
