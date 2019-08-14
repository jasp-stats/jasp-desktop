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

.readDataClassificationAnalyses <- function(dataset, options){
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
  if(length(unlist(options[["predictors"]])) > 0 && options[["scaleEqualSD"]])
    dataset[,.v(options[["predictors"]])] <- .scaleNumericData(dataset[,.v(options[["predictors"]])])
  if(options[["target"]] != "")
    dataset[, .v(options[["target"]])] <- factor(dataset[, .v(options[["target"]])])
  return(dataset)
}

.errorHandlingClassificationAnalyses <- function(dataset, options){
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

.classificationAnalysesReady <- function(options, type){
  if(type == "lda" || type == "randomForest" || type == "boosting"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2 && options[["target"]] != ""
  } else if(type == "knn"){
    ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 1 && options[["target"]] != ""
  }
  return(ready)
}

.classificationFormula <- function(options, jaspResults){
  predictors <- .v(options[["predictors"]])
  target <- .v(options[["target"]])
  formula <- formula(paste(target, "~", paste(predictors, collapse=" + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("predictors", "target"))
}

.classification <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["classificationResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    .classificationFormula(options, jaspResults)
    
    if(type == "knn"){
      classificationResult <- .knnClassification(dataset, options, jaspResults)
    } else if(type == "lda"){
      classificationResult <- .ldaClassification(dataset, options, jaspResults)
    } else if(type == "randomForest"){
      classificationResult <- .randomForestClassification(dataset, options, jaspResults)
    } else if(type == "boosting"){
      classificationResult <- .boostingClassification(dataset, options, jaspResults)
    }
    jaspResults[["classificationResult"]] <- createJaspState(classificationResult)
    jaspResults[["classificationResult"]]$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "validationDataManual",
                                                              "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                                              "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode",
                                                              "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual"))
  }
}

.classificationTable <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["classificationTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                      "knn" = "K-Nearest Neighbors Classification",
                      "lda" = "Linear Discriminant Classification",
                      "randomForest" = "Random Forest Classification",
                      "boosting" = "Boosting Classification")

  classificationTable <- createJaspTable(title)
  classificationTable$position <- position
  classificationTable$dependOn(options =c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "validationDataManual",
                                          "target", "predictors", "seed", "seedBox", "validationLeaveOneOut", "maxK", "noOfFolds", "modelValid",
                                          "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode",
                                          "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual"))

  if(type == "knn"){

    classificationTable$addColumnInfo(name = 'nn', title = 'Nearest neighbors', type = 'integer')
    classificationTable$addColumnInfo(name = 'weights', title = 'Weights', type = 'string')
    classificationTable$addColumnInfo(name = 'distance', title = 'Distance', type = 'string')
  
  } else if(type =="lda"){

    classificationTable$addColumnInfo(name = 'lda', title = 'Linear Discriminants', type = 'integer')
    classificationTable$addColumnInfo(name = 'method', title = 'Method', type = 'string')
  
  } else if(type == "randomForest"){
    classificationTable$addColumnInfo(name = 'trees', title = 'Trees', type = 'integer')
    classificationTable$addColumnInfo(name = 'preds', title = 'Predictors per split', type = 'integer')
  
  } else if(type == "boosting"){

    classificationTable$addColumnInfo(name = 'trees', title = 'Trees', type = 'integer')
    classificationTable$addColumnInfo(name = 'shrinkage', title = 'Shrinkage', type = 'number')
  
  }
  
  classificationTable$addColumnInfo(name = 'ntrain', title = 'n(Train)', type = 'integer')
  classificationTable$addColumnInfo(name = 'nvalid', title = 'n(Validation)', type = 'integer')
  classificationTable$addColumnInfo(name = 'ntest', title = 'n(Test)', type = 'integer')
  classificationTable$addColumnInfo(name = 'validAcc', title = 'Validation Accuracy', type = 'number')
  classificationTable$addColumnInfo(name = 'testAcc', title = 'Test Accuracy', type = 'number')
  
  if(type == "randomForest"){
    classificationTable$addColumnInfo(name = 'oob', title = 'OOB Accuracy', type = 'number')
  }

  requiredVars <- ifelse(type == "knn", yes = 1, no = 2)
  if(!ready)
    classificationTable$addFootnote(message = paste0("Please provide a target variable and at least ", requiredVars, " predictor variable(s)."), symbol = "<i>Note.</i>")

  jaspResults[["classificationTable"]] <- classificationTable
  
  if(!ready)  return()

  # Run the analysis
  .classification(dataset, options, jaspResults, ready, type = type)

  classificationResult <- jaspResults[["classificationResult"]]$object

  # Adjust train and test numbers for cross-validation
  nTrain <- classificationResult[["ntrain"]]
  nValid <- classificationResult[["nvalid"]]
  if(options[["modelValid"]] == "validationKFold"){
    nValid <- floor(nValid / options[["noOfFolds"]])
    nTrain <- nTrain - nValid
  } else if(options[["modelValid"]] == "validationLeaveOneOut"){
    nValid <- 1
    nTrain <- nTrain - 1
  }
  
  if(type == "knn"){

    if(options[["modelOpt"]] == "optimizationError")
      classificationTable$addFootnote(message="The model is optimized with respect to the <i>validation set accuracy</i>.", symbol="<i>Note.</i>")

    if(classificationResult[["nn"]] == options[["maxK"]] && options[["modelOpt"]] != "validationManual"){
      classificationTable$addFootnote(message="The optimum number of nearest neighbors is the maximum number. You might want to adjust the range op optimization.", symbol="<i>Note.</i>")
    }

    distance  <- ifelse(classificationResult[["distance"]] == 1, yes = "Manhattan", no = "Euclidian")    
    row <- data.frame(nn = classificationResult[["nn"]], 
                      weights = classificationResult[["weights"]], 
                      distance = distance, 
                      ntrain = nTrain, 
                      nvalid = nValid, 
                      ntest = classificationResult[["ntest"]], 
                      validAcc = classificationResult[["validAcc"]], 
                      testAcc = classificationResult[["testAcc"]])
    classificationTable$addRows(row)

  } else if(type =="lda"){

    method <- base::switch(options[["estimationMethod"]], "moment" = "Moment", "mle" = "MLE", "covMve" = "MVE","t" = "t")
    row <- data.frame(lda = ncol(classificationResult[["scaling"]]), 
                      method = method, 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = classificationResult[["ntest"]], 
                      validAcc = classificationResult[["validAcc"]],
                      testAcc = classificationResult[["testAcc"]])
    classificationTable$addRows(row)

  } else if(type == "randomForest"){

    if(options[["modelOpt"]] == "optimizationError")
      classificationTable$addFootnote(message="The model is optimized with respect to the <i>out-of-bag accuracy</i>.", symbol="<i>Note.</i>")

    row <- data.frame(trees = classificationResult[["noOfTrees"]], 
                      preds = classificationResult[["predPerSplit"]], 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = classificationResult[["ntest"]], 
                      validAcc = classificationResult[["validAcc"]],
                      testAcc = classificationResult[["testAcc"]], 
                      oob = classificationResult[["oobAccuracy"]])
    classificationTable$addRows(row)

  } else if(type == "boosting"){

    if(options[["modelOpt"]] == "optimizationOOB")
      classificationTable$addFootnote(message="The model is optimized with respect to the <i>out-of-bag accuracy</i>.", symbol="<i>Note.</i>")

    row <- data.frame(trees = classificationResult[["noOfTrees"]], 
                      shrinkage = options[["shrinkage"]], 
                      ntrain = nTrain, 
                      nvalid = nValid,
                      ntest = classificationResult[["ntest"]], 
                      validAcc = classificationResult[["validAcc"]],
                      testAcc = classificationResult[["testAcc"]])
    classificationTable$addRows(row)

  }
}

.classificationConfusionTable <- function(dataset, options, jaspResults, ready, position){

  if (!is.null(jaspResults[["confusionTable"]]) || !options[["confusionTable"]]) return()
  
  confusionTable <- createJaspTable(title = "Confusion Matrix")
  confusionTable$position <- position
  confusionTable$dependOn(options = c("noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt", "validationDataManual",
                                          "target", "predictors", "seed", "seedBox", "confusionTable", "confusionProportions", "maxK", "noOfFolds", "modelValid", 
                                          "estimationMethod", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode",
                                          "testSetIndicatorVariable", "testSetIndicator", "holdoutData", "testDataManual"))
  
  jaspResults[["confusionTable"]] <- confusionTable
  
  if(ready) {

    classificationResult <- jaspResults[["classificationResult"]]$object
    
    confusionTable$addColumnInfo(name = "obs_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_obs", title = "", type = "string")

    confTable <- classificationResult[["confTable"]]
    if(options[["confusionProportions"]])
      confTable <- round(confTable / classificationResult[["ntest"]], 2)
    
    confusionTable[["obs_name"]] <- c("Observed", rep("", nrow(confTable)-1))
    confusionTable[["varname_obs"]] <- colnames(confTable)
    
    for(i in 1:length(rownames(confTable))){
      name <- paste("varname_pred", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = rownames(confTable)[i], type = "integer", overtitle = "Predicted")
      confusionTable[[name]] <- confTable[i, ]  
    }
    
  } else if(options[["target"]] != "" && !ready) {
    
    confusionTable$addColumnInfo(name = "obs_name", title = "", type = "string")
    confusionTable$addColumnInfo(name = "varname_obs", title = "", type = "string")

    factorLevels <- levels(dataset[, .v(options[["target"]])])
    
    confusionTable[["obs_name"]] <- c("Observed", rep("", length(factorLevels) - 1))
    confusionTable[["varname_obs"]] <- factorLevels
    
    for(i in 1:length(factorLevels)){ 
      name <- paste("varname_pred", i, sep = "")
      confusionTable$addColumnInfo(name = name, title = factorLevels[i], type = "integer", overtitle = "Predicted")
      confusionTable[[name]] <- rep(".", length(factorLevels)) 
    }
    
  } else {
    
    confusionTable$addColumnInfo(name = "obs_name"    , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_obs" , title = "" , type = "string")
    confusionTable$addColumnInfo(name = "varname_pred1", title = ".", type = "integer")
    confusionTable$addColumnInfo(name = "varname_pred2", title = ".", type = 'integer')
    
    confusionTable[["obs_name"]] <- c("Observed", "")
    confusionTable[["varname_obs"]] <- rep(".", 2)
    confusionTable[["varname_pred1"]] <- rep("", 2)
    confusionTable[["varname_pred2"]] <- rep("", 2)
    
  }
  
}

.classificationDecisionBoundaries <- function(dataset, options, jaspResults, ready, position, type){

  if (!is.null(jaspResults[["decisionBoundary"]]) || !options[["decisionBoundary"]]) return()
  
  decisionBoundary <- createJaspPlot(title = "Decision Boundary Matrix", height = 400, width = 300)
  decisionBoundary$position <- position
  decisionBoundary$dependOn(options = c("decisionBoundary", "plotDensities", "plotStatistics", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod", 
                                          "maxK", "noOfFolds", "modelValid", "noOfNearestNeighbors", "distanceParameterManual", "weights",
                                          "plotLegend", "plotPoints", "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", 
                                          "shrinkage", "intDepth", "nNode", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual", 
                                          "holdoutData", "testDataManual"))
  jaspResults[["decisionBoundary"]] <- decisionBoundary 

  if(!ready || length(options[["predictors"]]) < 2)  return()
  
  .classificationFillDecisionBoundary(dataset, options, jaspResults, decisionBoundary, type)
}

.classificationFillDecisionBoundary <- function(dataset, options, jaspResults, decisionBoundary, type){

  classificationResult <- jaspResults[["classificationResult"]]$object

  variables <- options[["predictors"]]
  l <- length(variables)

  if (l <= 2) {
    width <- 580
    height <- 580
  } else {
    width <- 250 * l
    height <- 250 * l
  }

  decisionBoundary[["width"]]  <- width
  decisionBoundary[["height"]] <- height
  
  cexText <- 1.6
  
  plotMat <- matrix(list(), l - 1, l - 1)
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm")) 
  oldFontSize <- JASPgraphs::getGraphOption("fontsize")
  JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
  
  target <- dataset[, .v(options[["target"]])]
  startProgressbar(length(plotMat)+1)

  for (row in 2:l) {
    for (col in 1:(l-1)) {
      if (row == col) {     
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
      }  
      if (col < row) {
          predictors <- dataset[, .v(options[["predictors"]])]
          predictors <- predictors[, c(col, row)]
          formula <- formula(paste(.v(options[["target"]]), "~", paste(colnames(predictors), collapse=" + ")))
          plotMat[[row-1, col]] <- .decisionBoundaryPlot(dataset, options, jaspResults, predictors, target, formula, l, type = type)
      } 
      if (col > row) {
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
      }
      if(l > 2)
      if(options[["plotLegend"]])
        plotMat[[1, 2]] <- .legendPlot(dataset, options, col)
      progressbarTick()
    }
  }
  
  JASPgraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = variables[-1], topLabels = variables[-length(variables)],
                                scaleXYlabels = NULL, labelPos = labelPos)
  
  progressbarTick()
  decisionBoundary$plotObject <- p
}

.decisionBoundaryPlot <- function(dataset, options, jaspResults, predictors, target, formula, l, type){ 

    x_min <- min(predictors[,1])-0.5; x_max <- max(predictors[,1])+0.5
    y_min <- min(predictors[,2])-0.5; y_max <- max(predictors[,2])+0.5

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(predictors[, 1], min.n = 4)
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(predictors[, 2], min.n = 4)

    x_min <- xBreaks[1]; x_max <- xBreaks[length(xBreaks)]
    y_min <- yBreaks[1]; y_max <- yBreaks[length(yBreaks)]

    # Adjust the graining
    hs <- min(c(diff(range(xBreaks)), diff(range(yBreaks)))) / 50

    grid <- as.data.frame(expand.grid(seq(x_min, x_max, by = hs), seq(y_min, y_max, by =hs)))
    colnames(grid) <- colnames(predictors)

    classificationResult <- jaspResults[["classificationResult"]]$object

    if(type == "lda"){
      ldafit <- MASS::lda(formula, data = dataset)
      preds <- predict(ldafit, newdata = grid)$class
    } else if(type == "knn"){
      kfit <- kknn::train.kknn(formula = formula, data = dataset, ks = classificationResult[["nn"]], 
                  distance = classificationResult[['distance']], kernel = classificationResult[['weights']], scale = FALSE)
      preds <- predict(kfit, newdata = grid)
    } else if(type == "randomForest"){
      rfit <- randomForest::randomForest(x = predictors, y = target,
                                                 ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
                                                 sampsize = classificationResult[["bagFrac"]], importance = TRUE, keep.forest = TRUE)
      preds <- predict(rfit, newdata = grid)
    } else if(type == "boosting"){
      bfit <- gbm::gbm(formula = formula, data = dataset, n.trees = classificationResult[["noOfTrees"]],
                        shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                        cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                        distribution = "multinomial", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
      probabilities <- gbm::predict.gbm(bfit, newdata = grid, n.trees = classificationResult[["noOfTrees"]], type = "response")
      preds <- colnames(probabilities)[apply(probabilities, 1, which.max)]
    }

    gridData <- data.frame(x = grid[, 1], y = grid[, 2])
    pointData <- data.frame(x=predictors[, 1], y=predictors[, 2])

    p <- ggplot2::ggplot(data = gridData, mapping = ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_tile(ggplot2::aes(fill = preds), alpha = 0.3, show.legend = FALSE) +
          ggplot2::labs(fill = options[["target"]]) + 
          ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
    p <- p + ggplot2::scale_x_continuous(name = "", breaks = xBreaks, limits = range(xBreaks))
    p <- p + ggplot2::scale_y_continuous(name = "", breaks = yBreaks, limits = range(yBreaks))
    if(options[["plotPoints"]])
      p <- p + JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, fill = target))
    if(l <= 2){
      p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE, legend.position = "right")
    } else {
      p <- JASPgraphs::themeJasp(p, xAxis = TRUE, yAxis = TRUE)
    }
    return(p)
}

.legendPlot <- function(dataset, options, col){

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]
  predictors <- predictors[, 1]
  lda.data <- data.frame(target = target, predictors = predictors)
  
  p <- ggplot2::ggplot(lda.data, ggplot2::aes(y = target, x = target, show.legend = TRUE)) +
        JASPgraphs::geom_point(ggplot2::aes(fill = target), alpha = 0) +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::theme(legend.key = ggplot2::element_blank()) +
        ggplot2::labs(fill = options[["target"]]) + 
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target))))
  p <- JASPgraphs::themeJasp(p, yAxis = FALSE, xAxis = FALSE, legend.position = "left")
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  p <- p + ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  
  return(p)
}

.rocCurve <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["rocCurve"]]) || !options[["rocCurve"]]) return()

    rocCurve <- createJaspPlot(plot = NULL, title = "ROC Curves Plot", width = 500, height = 300)
    rocCurve$position <- position
    rocCurve$dependOn(options = c("rocCurve", "trainingDataManual", "scaleEqualSD", "modelOpt", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                    "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod",
                                    "maxK", "noOfFolds", "modelValid", "noOfNearestNeighbors", "distanceParameterManual", "weights",
                                    "noOfTrees", "bagFrac", "noOfPredictors", "numberOfPredictors", "shrinkage", "intDepth", "nNode", "holdoutData", "testDataManual"))
    jaspResults[["rocCurve"]] <- rocCurve

    if(!ready) return()

    classificationResult <- jaspResults[["classificationResult"]]$object 

    train <- classificationResult[["train"]]
    test <- classificationResult[["test"]]

    lvls <- levels(factor(train[, .v(options[["target"]])]))

    predictors <- .v(options[["predictors"]])
    formula <- formula(paste("levelVar", "~", paste(predictors, collapse=" + ")))

    linedata <- data.frame(x = c(0,1), y = c(0,1))
    p <- ggplot2::ggplot(linedata, ggplot2::aes(x = x, y = y)) +
          JASPgraphs::geom_line(col = "black", linetype = 2) +
          ggplot2::xlab("False positive rate") +
          ggplot2::ylab("True positive rate")

    rocXstore <- NULL
    rocYstore <- NULL
    rocNamestore <- NULL

    for(i in 1:length(lvls)){

      levelVar <- train[,.v(options[["target"]])] == lvls[i]
      typeData <- cbind(train, levelVar = factor(levelVar))
      column <- which(colnames(typeData) == .v(options[["target"]]))
      typeData <- typeData[, -column]

      if(type == "knn"){

        kfit <- kknn::kknn(formula = formula, train = typeData, test = test, k = classificationResult[["nn"]], 
                    distance = classificationResult[['distance']], kernel = classificationResult[['weights']], scale = FALSE)
        score <- predict(kfit, test, type = 'prob')[, 'TRUE']

      } else if(type == "lda"){

        ldafit <- MASS::lda(formula = formula, data = typeData, method = classificationResult[["method"]], CV = FALSE)
        score <- predict(ldafit, test, type = "prob")$posterior[, 'TRUE']

      } else if(type == "boosting"){

        levelVar <- as.character(levelVar)
        levelVar[levelVar == "TRUE"] <- 1
        levelVar[levelVar == "FALSE"] <- 0
        levelVar <- as.numeric(levelVar)
        column <- which(colnames(typeData) == "levelVar")
        typeData <- typeData[, -column]
        typeData <- cbind(typeData, levelVar = levelVar)

        bfit <- gbm::gbm(formula = formula, data = typeData, n.trees = classificationResult[["noOfTrees"]],
                    shrinkage = options[["shrinkage"]], interaction.depth = options[["intDepth"]],
                    cv.folds = classificationResult[["noOfFolds"]], bag.fraction = options[["bagFrac"]], n.minobsinnode = options[["nNode"]],
                    distribution = "bernoulli", n.cores=1) #multiple cores breaks modules in JASP, see: INTERNAL-jasp#372
        score <- predict(bfit, newdata = test, n.trees = classificationResult[["noOfTrees"]], type = "response")

      } else if(type == "randomForest"){
        
        column <- which(colnames(typeData) == "levelVar")
        typeData <- typeData[, -column]
        rfit <- randomForest::randomForest(x = typeData, y = factor(levelVar),
                   ntree = classificationResult[["noOfTrees"]], mtry = classificationResult[["predPerSplit"]],
                   sampsize = classificationResult[["bagFrac"]], importance = TRUE, keep.forest = TRUE)
        score <- predict(rfit, test, type = "prob")[, 'TRUE']

      }

      actual.class <- test[,.v(options[["target"]])] == lvls[i]
  
      pred <- ROCR::prediction(score, actual.class)
      nbperf <- ROCR::performance(pred, "tpr", "fpr")

      rocXstore <- c(rocXstore, unlist(nbperf@x.values))
      rocYstore <- c(rocYstore, unlist(nbperf@y.values))
      rocNamestore <- c(rocNamestore, rep(lvls[i], length(unlist(nbperf@y.values))))
    }

    rocData <- data.frame(x = rocXstore, y = rocYstore, name = rocNamestore)
    p <- p + JASPgraphs::geom_line(data = rocData, mapping = ggplot2::aes(x = x, y = y, col = name)) +
              ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(lvls))) +
              ggplot2::scale_y_continuous(limits = c(0, 1.1), breaks = seq(0,1,0.2)) +
              ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0,1,0.2)) +
              ggplot2::labs(color = options[["target"]]) +
              JASPgraphs::geom_point(data = data.frame(x = 0, y = 1), mapping = ggplot2::aes(x = x, y = y)) +
              ggrepel::geom_text_repel(data = data.frame(x = 0, y = 1), ggplot2::aes(label= "Perfect separation", x = x, y = y), hjust = -0.2, vjust = -0.8)
    p <- JASPgraphs::themeJasp(p, legend.position = "right")

    rocCurve$plotObject <- p
}

.classificationAndrewsCurves <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["andrewsCurve"]]) || !options[["andrewsCurve"]]) return()

  andrewsCurve <- createJaspPlot(plot = NULL, title = "Andrews Curves Plot", width = 500, height = 300)
  andrewsCurve$position <- position
  andrewsCurve$dependOn(options = c("andrewsCurve", "scaleEqualSD", "target", "predictors", "seed", "seedBox"))
  jaspResults[["andrewsCurve"]] <- andrewsCurve

  if(!ready) return()

  if(nrow(dataset)> 500){
    sample <- sample(1:nrow(dataset), size = 500, replace = FALSE) # Sample to prevent crazy long loading times with big data
  } else {
    sample <- 1:nrow(dataset)
  }

  predictors <- dataset[sample, .v(options[["predictors"]])]
  target <- dataset[sample, .v(options[["target"]])]

  # Taken from function `andrewsplot()` in R package "andrewsplot", thanks!
  n <- nrow(predictors)
  m <- ncol(predictors)

  npts <- 100

  xpts <- seq(0 - pi, 0 + pi, length = npts)
  Y <- matrix(NA, nrow = n, ncol = npts)

  for (i in 1:n) {
    xs <- as.numeric(predictors[i, ])
    ypts <- c()
    for (p in xpts) {
      y <- xs[1]
      for (j in 2:m) {
        if (j%%2 == 1) {
          y <- y + xs[j] * sin((j%/%2) * p)
        }
        else {
          y <- y + xs[j] * cos((j%/%2) * p)
        }
      }
      ypts <- c(ypts, y)
    }
    Y[i, ] <- as.numeric(ypts)
  }

  Yvec <- NULL
  for(i in 1:nrow(Y)){
    Yvec <- c(Yvec, Y[i, ])
  }

  d <- data.frame(x = rep(xpts, n),
                  y = Yvec,
                  target = rep(target, each = length(xpts)),
                  observation = rep(1:n, each = length(xpts)))

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = x, y = y, color = target, group = observation)) +
        JASPgraphs::geom_line(size = 0.2) +
        ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = xBreaks) + 
        ggplot2::scale_y_continuous(name = "", breaks = yBreaks, labels = yBreaks) +
        ggplot2::labs(color = options[["target"]]) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1)))
  p <- JASPgraphs::themeJasp(p, legend.position = "right")

  andrewsCurve$plotObject <- p

}

.boxM <- function (data, grouping){
  # Taken from the R package "biotools", thanks!
    dname <- deparse(substitute(data))
    data <- as.matrix(data)
    grouping <- as.factor(as.character(grouping))
    p <- ncol(data)
    nlev <- nlevels(grouping)
    lev <- levels(grouping)
    dfs <- tapply(grouping, grouping, length) - 1
    mats <- aux <- list()
    for (i in 1:nlev) {
        mats[[i]] <- cov(data[grouping == lev[i], ])
        aux[[i]] <- mats[[i]] * dfs[i]
    }
    names(mats) <- lev
    pooled <- Reduce("+", aux)/sum(dfs)
    logdet <- log(unlist(lapply(mats, det)))
    minus2logM <- sum(dfs) * log(det(pooled)) - sum(logdet * 
        dfs)
    sum1 <- sum(1/dfs)
    Co <- (((2 * p^2) + (3 * p) - 1)/(6 * (p + 1) * (nlev - 1))) * 
        (sum1 - (1/sum(dfs)))
    X2 <- minus2logM * (1 - Co)
    dfchi <- (choose(p, 2) + p) * (nlev - 1)
    pval <- pchisq(X2, dfchi, lower.tail = FALSE)
    out <- structure(list(statistic = c(`Chi-Sq (approx.)` = X2), 
        parameter = c(df = dfchi), p.value = pval, cov = mats, 
        pooled = pooled, logDet = logdet, data.name = dname, 
        method = " Box's M-test for Homogeneity of Covariance Matrices"), 
        class = c("htest", "boxM"))
    return(out)
}

.classificationEvaluationMetrics <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["validationMeasures"]]) || !options[["validationMeasures"]]) return()
  
  validationMeasures <- createJaspTable(title = "Evaluation Metrics")
  validationMeasures$position <- position
  validationMeasures$dependOn(options = c("validationMeasures", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid", "holdoutData", "testDataManual",
                                                            "estimationMethod", "shrinkage", "intDepth", "nNode", "validationDataManual", "testSetIndicatorVariable", "testSetIndicator"))

  validationMeasures$addColumnInfo(name = "group", title = "", type = "string")
  validationMeasures$addColumnInfo(name = "precision", title = "Precision", type = "number")
  validationMeasures$addColumnInfo(name = "recall", title = "Recall", type = "number")
  validationMeasures$addColumnInfo(name = "f1", title = "F1 Score", type = "number")
  validationMeasures$addColumnInfo(name = "support", title = "Support", type = "integer")
  validationMeasures$addColumnInfo(name = "auc", title = "AUC", type = "number")

  validationMeasures$addFootnote(message= "Area Under Curve (AUC) is calculated for every class against all other classes." , symbol="<i>Note.</i>")

  if(options[["target"]] != "")
    validationMeasures[["group"]] <- c(levels(factor(dataset[, .v(options[["target"]])])), "Average / Total")
  
  jaspResults[["validationMeasures"]] <- validationMeasures

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  pred <- factor(classificationResult[["testPred"]])
  real <- factor(classificationResult[["testReal"]])

  lvls <- levels(as.factor(real))

  precision       <- numeric()
  recall          <- numeric()
  f1              <- numeric()
  support         <- numeric()
  auc             <- classificationResult[["auc"]]

  for(i in 1:length(lvls)){

    TP                <- length(which(pred == lvls[i] & real == lvls[i]))
    FN                <- length(which(pred != lvls[i] & real == lvls[i]))
    FP                <- length(which(pred == lvls[i] & real != lvls[i]))

    precision_tmp     <- TP / (TP + FP)
    recall_tmp        <- TP / (TP + FN)
    f1_tmp            <- 2 * ( ( precision_tmp * recall_tmp ) / ( precision_tmp + recall_tmp ) )
    support_tmp       <- length(which(real == lvls[i]))

    precision[i]      <- precision_tmp
    recall[i]         <- recall_tmp 
    f1[i]             <- f1_tmp
    support[i]        <- support_tmp
  }

  precision[length(precision) + 1]    <- sum(precision * support, na.rm = TRUE) / sum(support, na.rm = TRUE)
  recall[length(recall) + 1]          <- sum(recall * support, na.rm = TRUE) / sum(support, na.rm = TRUE)
  f1[length(f1) + 1]                  <- sum(f1 * support, na.rm = TRUE) / sum(support, na.rm = TRUE)
  support[length(support) + 1]        <- sum(support, na.rm = TRUE)
  auc[length(auc) + 1]                <- mean(auc, na.rm = TRUE)

  validationMeasures[["precision"]]   <- precision
  validationMeasures[["recall"]]      <- recall
  validationMeasures[["f1"]]          <- f1
  validationMeasures[["support"]]     <- support
  validationMeasures[["auc"]]         <- auc
}

.classificationClassProportions <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["classProportionsTable"]]) || !options[["classProportionsTable"]]) return()
  
  classProportionsTable <- createJaspTable(title = "Class Proportions")
  classProportionsTable$position <- position
  classProportionsTable$dependOn(options = c("classProportionsTable", "noOfNearestNeighbours", "trainingDataManual", "distanceParameterManual", "weights", "scaleEqualSD", "modelOpt",
                                                            "target", "predictors", "seed", "seedBox", "modelValid", "maxK", "noOfFolds", "modelValid", "holdoutData", "testDataManual",
                                                            "estimationMethod", "shrinkage", "intDepth", "nNode", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual"))

  classProportionsTable$addColumnInfo(name = "group", title = "", type = "string")
  classProportionsTable$addColumnInfo(name = "dataset", title = "Data Set", type = "number")
  classProportionsTable$addColumnInfo(name = "train", title = "Training Set", type = "number")
  classProportionsTable$addColumnInfo(name = "valid", title = "Validation Set", type = "number")
  classProportionsTable$addColumnInfo(name = "test", title = "Test Set", type = "number")

  if(options[["target"]] != ""){
    classProportionsTable[["group"]] <- levels(factor(dataset[, .v(options[["target"]])]))
    Dlevels <- levels(factor(dataset[, .v(options[["target"]])]))
  }
  
  jaspResults[["classProportionsTable"]] <- classProportionsTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object 

  dataValues      <- rep(0, length(classProportionsTable[["group"]]))
  trainingValues  <- rep(0, length(classProportionsTable[["group"]]))
  validValues     <- rep(0, length(classProportionsTable[["group"]]))
  testValues      <- rep(0, length(classProportionsTable[["group"]]))

  dataTable       <- prop.table(table(dataset[,.v(options[["target"]])]))
  trainingTable   <- prop.table(table(classificationResult[["train"]][,.v(options[["target"]])]))
  validTable      <- prop.table(table(classificationResult[["valid"]][,.v(options[["target"]])]))
  testTable       <- prop.table(table(classificationResult[["test"]][,.v(options[["target"]])]))

  for(i in 1:length(Dlevels)){
    # Dataset
    dataIndex                       <- which(names(dataTable) == as.character(Dlevels)[i])
    dataValues[dataIndex]           <- as.numeric(dataTable)[dataIndex]
    # Training data
    trainingIndex                   <- which(names(trainingTable) == as.character(Dlevels)[i])
    trainingValues[trainingIndex]   <- as.numeric(trainingTable)[trainingIndex]
    # Validation set
    validIndex                      <- which(names(validTable) == as.character(Dlevels)[i])
    validValues[validIndex]           <- as.numeric(validTable)[validIndex]
    # Test set
    testIndex                       <- which(names(testTable) == as.character(Dlevels)[i])
    testValues[testIndex]           <- as.numeric(testTable)[testIndex]
  }

  classProportionsTable[["dataset"]] <- dataValues
  classProportionsTable[["train"]]   <- trainingValues
  classProportionsTable[["valid"]]   <- validValues
  classProportionsTable[["test"]]    <- testValues

}
