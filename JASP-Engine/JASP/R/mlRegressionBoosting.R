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

mlRegressionBoosting <- function(jaspResults, dataset, options, ...) {

	# Preparatory work
	dataset <- .readDataRegressionAnalyses(dataset, options)
	.errorHandlingRegressionAnalyses(dataset, options, type = "boosting")

	# Check if analysis is ready to run
	ready <- .regressionAnalysesReady(options, type = "boosting")

  # Compute results and create the model summary table
	.regressionMachineLearningTable(dataset, options, jaspResults, ready, position = 1, type = "boosting")

  # If the user wants to add the values to the data set
  .regressionAddValuesToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "regression")

  # Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "regression", type = "boosting")

  # Create the evaluation metrics table
	.regressionEvaluationMetrics(dataset, options, jaspResults, ready, position = 3)

  # Create the relative influence table
  .boostingRelativeInfluenceTable(options, jaspResults, ready, position = 4, purpose = "regression")

  # Create the OOB improvement plot
  .boostingOOBimprovementPlot(options, jaspResults, ready, position = 5, purpose = "regression")

  # Create the predicted performance plot
	.regressionPredictedPerformancePlot(options, jaspResults, ready, position = 6)

  # Create the deviance plot
  .boostingDeviancePlot(options, jaspResults, ready, position = 7, purpose = "regression")

  # Create the relative influence plot
  .boostingRelativeInfluencePlot(options, jaspResults, ready, position = 8, purpose = "regression")

}

.boostingRegression <- function(dataset, options, jaspResults){

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

    bfit <- gbm::gbm(formula = formula, data = trainAndValid, n.trees = trees,
                            shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                            cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]],
                            n.minobsinnode = options[["nNode"]], distribution = options[["distance"]], n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

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
                          cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]],
                          n.minobsinnode = options[["nNode"]], distribution = options[["distance"]], n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372


    noOfTrees <- gbm::gbm.perf(bfit, plot.it = FALSE, method = "OOB")[1] # pick the optimal number of trees

    bfit <- gbm::gbm(formula = formula, data = train, n.trees = noOfTrees,
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = noOfFolds, bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = options[["distance"]], n.cores = 1) # Multiple cores breaks modules in JASP, see: INTERNAL-jasp#372

    pred_valid <- gbm::predict.gbm(bfit, valid, n.trees = noOfTrees, type = "response")
  }

  # Use the specified model to make predictions for dataset
  predictions <- gbm::predict.gbm(bfit, dataset, n.trees = noOfTrees, type = "response")

  # Predictions for test set
  pred_test <- predictions[-train.index]

  # Create results object
  regressionResult <- list()
  regressionResult[["model"]]       <- bfit
  regressionResult[["formula"]]     <- formula
  regressionResult[["noOfFolds"]]   <- noOfFolds
  regressionResult[["noOfTrees"]]   <- noOfTrees
  regressionResult[["method"]]      <- ifelse(options[["modelValid"]] == "validationManual", yes = "OOB", no = "")
  regressionResult[['testMSE']]     <- mean((pred_test - test[,.v(options[["target"]])])^2)
  regressionResult[["relInf"]]      <- summary(bfit, plot = FALSE)
  regressionResult[["ntrain"]]      <- nrow(train)
  regressionResult[["ntest"]]       <- nrow(test)
  regressionResult[["testPred"]]    <- pred_test
  regressionResult[["testReal"]]    <- test[,.v(options[["target"]])]
  regressionResult[["train"]]       <- train
  regressionResult[["test"]]        <- test
  regressionResult[["testIndicatorColumn"]] <- testIndicatorColumn
  regressionResult[["values"]]      <- predictions

  if(options[["modelOpt"]] != "optimizationManual"){
    regressionResult[['validMSE']]    <- mean((pred_valid - valid[,.v(options[["target"]])])^2)
    regressionResult[["nvalid"]]      <- nrow(valid)
    regressionResult[["valid"]]       <- valid
  }

  return(regressionResult)
}

.boostingRelativeInfluenceTable <- function(options, jaspResults, ready, position, purpose){

  if (!options[["classBoostRelInfTable"]] || !is.null(jaspResults[["classBoostRelInfTable"]])) return()

  classBoostRelInfTable <- createJaspTable(title = "Relative Influence")
  classBoostRelInfTable$position <- position
  classBoostRelInfTable$dependOn(options = c("classBoostRelInfTable", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
                                                "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                                "holdoutData", "testDataManual"))

  classBoostRelInfTable$addColumnInfo(name = "predictor",  title = "", type = "string")
  classBoostRelInfTable$addColumnInfo(name = "relIn",  title = "Relative Influence", type = "number")

  jaspResults[["classBoostRelInfTable"]] <- classBoostRelInfTable

  if(!ready)  return()

  result <- base::switch(purpose,
                          "classification" = jaspResults[["classificationResult"]]$object,
                          "regression" = jaspResults[["regressionResult"]]$object)

  classBoostRelInfTable[["predictor"]]  <- .unv(as.character(result[["relInf"]]$var))
  classBoostRelInfTable[["relIn"]]  <- result[["relInf"]]$rel.inf
}

.boostingOOBimprovementPlot <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotOOBChangeDev"]]) || !options[["plotOOBChangeDev"]]) return()

  plotOOBChangeDev <- createJaspPlot(plot = NULL, title = "Out-of-bag Improvement Plot", width = 500, height = 300)
  plotOOBChangeDev$position <- position
  plotOOBChangeDev$dependOn(options = c("plotOOBChangeDev", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
                                "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                "holdoutData", "testDataManual"))
  jaspResults[["plotOOBChangeDev"]] <- plotOOBChangeDev

  if(!ready) return()

  result <- base::switch(purpose,
                      "classification" = jaspResults[["classificationResult"]]$object,
                      "regression" = jaspResults[["regressionResult"]]$object)

  oobDev <- data.frame(trees = 1:result[["model"]]$n.trees, oobImprove = result[["model"]]$oobag.improve, type = "Training set")

  if(purpose == "classification"){
    if (nlevels(result[["test"]][,.v(options[["target"]])]) > 2L) {
      ylab <- "OOB Change in \n Multinomial Deviance"
    } else {
      ylab <- "OOB Change in \n Binomial Deviance"
    }
  } else {
    distribution <- base::switch(options[["distance"]], "tdist" = "t", "gaussian" = "Gaussian", "laplace" = "Laplace")
    ylab <- paste0("OOB Change in \n", distribution, " Deviance")
  }

  if (nrow(oobDev) <= 5L) {
    geom <- JASPgraphs::geom_point
    xend <- nrow(oobDev) + 1L
    xBreaks <- 1:xend
  } else {
    geom <- JASPgraphs::geom_line
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(oobDev[["trees"]], min.n = 4)
    xend <- nrow(oobDev)
  }
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, oobDev[["oobImprove"]]), min.n = 4)
  xLabels <- JASPgraphs::axesLabeller(xBreaks)
  yLabels <- JASPgraphs::axesLabeller(yBreaks)

  p <- ggplot2::ggplot(data = oobDev, mapping = ggplot2::aes(x = trees, y = oobImprove, linetype = type)) +
    ggplot2::geom_segment(data = data.frame(xstart = 0, xend = xend, ystart = 0, yend = 0), ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), linetype = 2, col = "darkgrey") +
    geom() +
    ggplot2::geom_smooth(size = 1, colour = "darkred", se = FALSE) +
    ggplot2::scale_x_continuous(name = "Number of Trees", labels = xLabels, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = ylab,              labels = yLabels, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(linetype = "")

  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  plotOOBChangeDev$plotObject <- p
}

.boostingDeviancePlot <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotDeviance"]]) || !options[["plotDeviance"]]) return()

  plotDeviance <- createJaspPlot(plot = NULL, title = "Deviance Plot", width = 500, height = 300)
  plotDeviance$position <- position
  plotDeviance$dependOn(options = c("plotDeviance", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
                                "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                "holdoutData", "testDataManual"))
  jaspResults[["plotDeviance"]] <- plotDeviance

  if(!ready) return()

  result <- base::switch(purpose,
                        "classification" = jaspResults[["classificationResult"]]$object,
                        "regression" = jaspResults[["regressionResult"]]$object)

  deviance <- data.frame(
    trees = 1:result[["model"]]$n.trees,
    trainError = c(result[["model"]]$train.error, result[["model"]]$cv.error),
    what = rep(c("Out-of-bag", "Cross-validated"), c(length(result[["model"]]$train.error), length(result[["model"]]$cv.error)))
  )

  if(purpose == "classification"){
    if (nlevels(result[["test"]][,.v(options[["target"]])]) > 2L) {
      ylab <- "Multinomial Deviance"
    } else {
      ylab <- "Binomial Deviance"
    }
  } else {
    distribution <- base::switch(options[["distance"]], "tdist" = "t", "gaussian" = "Gaussian", "laplace" = "Laplace")
    ylab <- paste0(distribution, " Deviance")
  }

  if (max(deviance[["trees"]]) <= 5L) {
    geom <- JASPgraphs::geom_point
    xend <- max(deviance[["trees"]]) + 1L
    xBreaks <- 1:xend
  } else {
    geom <- ggplot2::geom_line
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(deviance[["trees"]], min.n = 4)
    xend <- length(result[["model"]]$train.error)
  }
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, deviance[["trainError"]]), min.n = 4)

  xLabels <- JASPgraphs::axesLabeller(xBreaks)
  yLabels <- JASPgraphs::axesLabeller(yBreaks)

  p <- ggplot2::ggplot(data = deviance, mapping = ggplot2::aes(x = trees, y = trainError, group = what, color = what)) +
        ggplot2::geom_segment(data = data.frame(xstart = 1, xend = xend, ystart = 0, yend = 0, group = "Out-of-bag", what = "Out-of-bag"),
                              ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), linetype = 2, col = "darkgrey") +
        geom(show.legend = result[["method"]] != "OOB") +
        ggplot2::scale_x_continuous(name = "Number of Trees", labels = xLabels, breaks = xBreaks, limits = range(xBreaks)) +
        ggplot2::scale_y_continuous(name = ylab,              labels = yLabels, breaks = yBreaks, limits = range(yBreaks)) +
        ggplot2::scale_color_manual(name = "", values = c("Out-of-bag" = "gray20", "Cross-validated" = "#99c454"))
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  plotDeviance$plotObject <- p
}

.boostingRelativeInfluencePlot <- function(options, jaspResults, ready, position, purpose){

  if(!is.null(jaspResults[["plotRelInf"]]) || !options[["plotRelInf"]]) return()

  plotRelInf <- createJaspPlot(plot = NULL, title = "Relative Influence Plot", width = 500, height = 300)
  plotRelInf$position <- position
  plotRelInf$dependOn(options = c("plotRelInf", "target", "predictors", "modelOpt", "maxTrees", "intDepth", "shrinkage",
                                "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "seed", "seedBox", "modelValid",
                                "nNode", "distance", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                "holdoutData", "testDataManual"))
  jaspResults[["plotRelInf"]] <- plotRelInf

  if(!ready) return()

  result <- base::switch(purpose,
                        "classification" = jaspResults[["classificationResult"]]$object,
                        "regression" = jaspResults[["regressionResult"]]$object)

  p <- ggplot2::ggplot(result[["relInf"]], ggplot2::aes(x = reorder(.unv(as.factor(var)), rel.inf), y = rel.inf)) +
        ggplot2::geom_bar(stat = "identity", fill = "gray", col = "black", size = .3) +
        ggplot2::labs(x = "", y = "Relative Influence")
  p <- JASPgraphs::themeJasp(p, horizontal = TRUE, xAxis = FALSE) + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  plotRelInf$plotObject <- p
}
