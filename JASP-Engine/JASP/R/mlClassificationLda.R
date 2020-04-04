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

mlClassificationLda <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClassificationAnalyses(dataset, options)
  .errorHandlingClassificationAnalyses(dataset, options, type = "lda")
  
  # Check if analysis is ready to run
  ready <- .classificationAnalysesReady(options, type = "lda")
  
  # Compute results and create the model summary table
  .classificationTable(dataset, options, jaspResults, ready, position = 1, type = "lda")

  # If the user wants to add the classes to the data set
  .classificationAddClassesToData(dataset, options, jaspResults, ready)

  # Add test set indicator to data
  .addTestIndicatorToData(options, jaspResults, ready, purpose = "classification")

  # Create the data split plot
	.dataSplitPlot(dataset, options, jaspResults, ready, position = 2, purpose = "classification", type = "lda")

  # Create the confusion table
  .classificationConfusionTable(dataset, options, jaspResults, ready, position = 3)

  # Create the class proportions table
  .classificationClassProportions(dataset, options, jaspResults, ready, position = 4)

  # Create the validation measures table
  .classificationEvaluationMetrics(dataset, options, jaspResults, ready, position = 5)

  # Create the coefficients table
  .ldaClassificationCoefficients(options, jaspResults, ready, position = 6)

  # Create the prior and posterior table
  .ldaClassificationPriorPosterior(options, jaspResults, ready, position = 7)

  # Create the group means table
  .ldaClassificationMeans(options, jaspResults, ready, position = 8)

  # Create the test of equality of means table
  .ldaEqualityOfClassMeans(dataset, options, jaspResults, ready, position = 9)

  # Create the test of equality of covariance matrices table
  .ldaEqualityOfCovarianceMatrices(dataset, options, jaspResults, ready, position = 10)

  # Create the multicollinearity table
  .ldaMulticollinearity(dataset, options, jaspResults, ready, position = 11)

  # Create the ROC curve
  .rocCurve(dataset, options, jaspResults, ready, position = 12, type = "lda")

  # Create the Andrews curves
  .classificationAndrewsCurves(dataset, options, jaspResults, ready, position = 13)

  # Create the LDA matrix plot 
  .ldaMatricesPlot(dataset, options, jaspResults, ready, position = 14)

  # Decision boundaries
  .classificationDecisionBoundaries(dataset, options, jaspResults, ready, position = 15, type = "lda")
  
}

# Error handling 
.classLdaErrorHandling <- function(dataset, options){
  # Error Check 1: There should be at least 5 observations in the target variable
  .hasErrors(dataset = dataset, type = c('observations', 'variance', 'infinity'),
             all.target = options$target, observations.amount = '< 5', exitAnalysisIfErrors = TRUE)
  
  # Error Check 2: The target variable should have at least 2 classes
  if (nlevels(dataset[, .v(options$target)]) < 2){
    JASP:::.quitAnalysis(gettext("The target variable should have at least 2 classes."))
  }
  
}

# Compute results 
.ldaClassification <- function(dataset, options, jaspResults){

  # Import model formula from jaspResults
	formula <- jaspResults[["formula"]]$object

	# Split the data into training and test sets
	if(options[["holdoutData"]] == "testSetIndicator" && options[["testSetIndicatorVariable"]] != ""){
		# Select observations according to a user-specified indicator (included when indicator = 1)
		train.index             <- which(dataset[,.v(options[["testSetIndicatorVariable"]])] == 0)
	} else {
		# Sample a percentage of the total data set
		train.index             <- sample.int(nrow(dataset), size = ceiling( (1 - options[['testDataManual']]) * nrow(dataset)))
	}
	train                     <- dataset[train.index, ]
  test                      <- dataset[-train.index, ]

  # Create the generated test set indicator
	testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[train.index] <- 0

  method <- base::switch(options[["estimationMethod"]], 
                          "moment" = "moment",
                          "mle" = "mle",
                          "covMve" = "mve",
                          "t" = "t")
  ldafit      <- MASS::lda(formula = formula, data = train, method = method, CV = FALSE)
  pred_test   <- stats::predict(ldafit, newdata = test)

  # Calculate AUC
  auc <- .classificationCalcAUC(test, train, options, "ldaClassification", LDAmethod=method)

  # Use the specified model to make predictions for dataset
  predictions <- predict(ldafit, newdata = dataset)$class

  # Create results object
  classificationResult <- list()
  classificationResult[["model"]]               <- ldafit
  classificationResult[["method"]]              <- method
  classificationResult[["scaling"]]             <- ldafit[["scaling"]]
  classificationResult[['confTable']]           <- table('Pred' = pred_test[["class"]], 'Real' = test[,.v(options[["target"]])])
  classificationResult[['testAcc']]             <- sum(diag(prop.table(classificationResult[['confTable']])))
  classificationResult[["auc"]]                 <- auc
  classificationResult[["testPred"]]            <- pred_test[["class"]]
  classificationResult[["testReal"]]            <- test[,.v(options[["target"]])]
  classificationResult[["meanTable"]]           <- ldafit[["means"]]
  classificationResult[["relInf"]]              <- summary(ldafit, plot = FALSE)
  classificationResult[["prior"]]               <- ldafit[["prior"]]
  classificationResult[["postprob"]]            <- colMeans(pred_test[["posterior"]])
  classificationResult[["ntrain"]]              <- nrow(train)
  classificationResult[["ntest"]]               <- nrow(test)
  classificationResult[["train"]]               <- train
  classificationResult[["test"]]                <- test
  classificationResult[["testIndicatorColumn"]] <- testIndicatorColumn
  classificationResult[["classes"]]             <- predictions
  
  return(classificationResult)
}

.ldaClassificationCoefficients <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["coefficientsTable"]]) || !options[["coefficientsTable"]]) return()
  
  coefficientsTable <- createJaspTable(title = gettext("Linear Discriminant Coefficients"))
  coefficientsTable$position <- position
  coefficientsTable$dependOn(options = c("coefficientsTable", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod", 
                                          "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                          "holdoutData", "testDataManual"))
  coefficientsTable$addColumnInfo(name = "pred_level", title = "", type = "string")
  
  jaspResults[["coefficientsTable"]] <- coefficientsTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object

  for (ldacoef in colnames(classificationResult[["scaling"]])){
    coefficientsTable$addColumnInfo(name = ldacoef, type = "number")
  }

  coefficients <- classificationResult[["scaling"]]

  # For constants see: https://stats.stackexchange.com/questions/166942/why-are-discriminant-analysis-results-in-r-lda-and-spss-different-constant-t
  groupmean <- (classificationResult[["model"]]$prior %*% classificationResult[["model"]]$means)
  constants <- (groupmean %*% classificationResult[["scaling"]])

  row <- cbind(pred_level = c(gettext("(Constant)"), .unv(rownames(coefficients))), 
                as.data.frame(rbind(constants, coefficients)))
    
  coefficientsTable$addRows(row) 
}

.ldaClassificationPriorPosterior <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["priorTable"]]) || !options[["priorTable"]]) return()
  
  priorTable <- createJaspTable(title = gettext("Prior and Posterior Class Probabilities"))
  priorTable$position <- position
  priorTable$dependOn(options = c("priorTable", "trainingDataManual", "scaleEqualSD", "modelOpt",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod",
                                          "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                          "holdoutData", "testDataManual"))

  priorTable$addColumnInfo(name = "typeprob",  title = "",                   type = "string")
  priorTable$addColumnInfo(name = "prior",     title = gettext("Prior"),     type = "number")
  priorTable$addColumnInfo(name = "posterior", title = gettext("Posterior"), type = "number")
  
  jaspResults[["priorTable"]] <- priorTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
    
  levelPriors <- classificationResult[["prior"]]
  levelPost <- classificationResult[["postprob"]]

  row <- data.frame(typeprob = names(levelPriors), prior = levelPriors, posterior = levelPost)
  priorTable$addRows(row)
}

.ldaClassificationMeans <- function(options, jaspResults, ready, position){

  if(!is.null(jaspResults[["meanTable"]]) || !options[["meanTable"]]) return()
  
  meanTable <- createJaspTable(title = gettext("Class Means in Training Data"))
  meanTable$position <- position
  meanTable$dependOn(options = c("meanTable", "trainingDataManual", "scaleEqualSD", "modelOpt", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod",
                                          "holdoutData", "testDataManual"))

  meanTable$addColumnInfo(name = "target_level", title = "", type = "string")
  for (i in options[["predictors"]]){
    meanTable$addColumnInfo(name = i, type = "number", title = i)
  }
  
  jaspResults[["meanTable"]] <- meanTable

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  groupMeans <- classificationResult[["meanTable"]]
  colnames(groupMeans) <- .unv(colnames(groupMeans))
  
  row <- cbind(target_level = rownames(groupMeans), as.data.frame(groupMeans))  
  meanTable$addRows(row)
}

.ldaMatricesPlot <- function(dataset, options, jaspResults, ready, position){

  if (!is.null(jaspResults[["matrixplot"]]) || !options[["matrixplot"]]) return()
  
  matrixplot <- createJaspPlot(title = gettext("Linear Discriminant Matrix"), height = 400, width = 300)
  matrixplot$position <- position
  matrixplot$dependOn(options = c("matrixplot", "plotDensities", "plotStatistics", "trainingDataManual", "scaleEqualSD", "modelOpt", "holdoutData", "testDataManual",
                                          "target", "predictors", "seed", "seedBox", "modelValid", "estimationMethod", "testSetIndicatorVariable", "testSetIndicator", "validationDataManual"))
  jaspResults[["matrixplot"]] <- matrixplot 

  if(!ready)  return()

  classificationResult <- jaspResults[["classificationResult"]]$object
  
  .ldaFillMatrixPlot(dataset, options, jaspResults, classificationResult, matrixplot)
}

.ldaFillMatrixPlot <- function(dataset, options, jaspResults, classificationResult, matrixplot) {

  variables <- colnames(classificationResult[["scaling"]])
  l <- length(variables)

  if (l <= 2 && (options[["plotDensities"]] || options[["plotStatistics"]])) {
    width <- 580
    height <- 580
  } else if (l <= 2) {
    width <- 580
    height <- 580
  } else {
    width <- 250 * l
    height <- 250 * l
  }

  matrixplot[["width"]]  <- width
  matrixplot[["height"]] <- height
  
  cexText <- 1.6
  
  plotMat <- matrix(list(), l, l)
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm")) 
  oldFontSize <- JASPgraphs::getGraphOption("fontsize")
  JASPgraphs::setGraphOption("fontsize", .85 * oldFontSize)
  
  startProgressbar(length(plotMat)+1)
  for (row in seq_len(l)) {
    for (col in seq_len(l)) {
      if (row == col) {
        if (options[["plotDensities"]]) {
            plotMat[[row, col]] <- .ldaDensityplot(classificationResult, options, col) + adjMargin # plot marginal (histogram with density estimator)
        } else {
          
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
        }
      }
      
      if (col > row) {
        if (options[["plotStatistics"]]) {
            plotMat[[row, col]] <- .ldaScatterPlot(classificationResult, options, col) + adjMargin # plot scatterplot
          
        } else {
          
          p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
          p <- p + ggplot2::xlab("")
          p <- p + ggplot2::ylab("")
          p <- JASPgraphs::themeJasp(p)
          
          plotMat[[row, col]] <- p
        }
      }
      
      if (col < row) {
        if (l < 7) {
          if (options[["plotStatistics"]]) {
            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
            p <- p + ggplot2::xlab("")
            p <- p + ggplot2::ylab("")
            p <- JASPgraphs::themeJasp(p)
            
            plotMat[[row, col]] <- p
          
          } else {
            
            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE) + adjMargin
            p <- p + ggplot2::xlab("")
            p <- p + ggplot2::ylab("")
            p <- JASPgraphs::themeJasp(p)
            
            plotMat[[row, col]] <- p
          }
        }
        
        if (col == 1 && row == 2){
          plotMat[[2, 1]] <- .legendPlot(dataset, options, col) 
        }
        progressbarTick()
      }
    }
  }  
  JASPgraphs::setGraphOption("fontsize", oldFontSize)
  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = variables, topLabels = variables,
                                scaleXYlabels = NULL, labelPos = labelPos)
  
  progressbarTick()
  matrixplot$plotObject <- p
}
  
.ldaDensityplot <- function(classificationResult, options, col){

  target <- classificationResult[["train"]][, .v(options[["target"]])]
  lda.fit.scaled <- cbind.data.frame(
    LD = .ldaModelMatrix(classificationResult[["model"]], classificationResult[["train"]]) %*% classificationResult[["scaling"]][, col],
    V2 = classificationResult[["train"]][,.v(options[["target"]])]
  )
  lda.fit.scaled[["V2"]] <- as.factor(lda.fit.scaled[["V2"]])

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(lda.fit.scaled[, "LD"], min.n = 4)
  
  if (length(colnames(classificationResult[["scaling"]])) == 1) {

    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = LD, group = V2, color = V2, show.legend = TRUE)) +
          JASPgraphs::geom_line(stat = "density") + 
          ggplot2::labs(color = options[["target"]]) + 
          ggplot2::theme(legend.key = ggplot2::element_blank()) +
          ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(unique(target)))) +
          ggplot2::scale_x_continuous(name = "", breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = gettext("Density"))
    
    p <- JASPgraphs::themeJasp(p, legend.position = "right")
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
    p <- p + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 21)))
    
  } else {

    p <- ggplot2::ggplot(data = lda.fit.scaled, ggplot2::aes(x = LD, group = V2, color = V2)) +
          JASPgraphs::geom_line(stat = "density") + 
          ggplot2::scale_color_manual(values = colorspace::qualitative_hcl(n = length(unique(target)))) +
          ggplot2::scale_x_continuous(name = "", breaks = xBreaks, limits = range(xBreaks)) +
          ggplot2::scale_y_continuous(name = gettext("Density"))
    p <- JASPgraphs::themeJasp(p)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

  }
  return(p)
}

.ldaScatterPlot <- function(classificationResult, options, col){

  data <- classificationResult[["train"]]
  target <- data[, .v(options[["target"]])]
  model <- classificationResult[["model"]]

  pred.values <- stats::predict(model, newdata = data)$x[,c(col, col - 1)]
  lda.data <- data.frame(pred.values, target = target)
  colnames(lda.data) <- c("x", "y", "target")

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(lda.data[, "x"], min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(lda.data[, "y"], min.n = 4)
  
  p <- ggplot2::ggplot(lda.data, ggplot2::aes(x = x, y = y)) +
        JASPgraphs::geom_point(ggplot2::aes(fill = target)) + 
        ggplot2::labs(fill = options[["target"]]) + 
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = length(unique(target)))) +
        ggplot2::scale_x_continuous(name = "", breaks = xBreaks, limits = range(xBreaks)) + 
        ggplot2::scale_y_continuous(name = "", breaks = yBreaks, limits = range(yBreaks))
  p <- JASPgraphs::themeJasp(p)    
  
  return(p)
}

.ldaEqualityOfClassMeans <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["manovaTable"]]) || !options[["manovaTable"]]) return()
  
  manovaTable <- createJaspTable(title = gettext("Tests of Equality of Class Means"))
  manovaTable$position <- position
  manovaTable$dependOn(options = c("manovaTable", "scaleEqualSD", "target", "predictors"))
  manovaTable$addColumnInfo(name = "model", title = "", type = "string")
  manovaTable$addColumnInfo(name = "f", title = "F", type = "number")
  manovaTable$addColumnInfo(name = "df1", title = "df1", type = "integer")
  manovaTable$addColumnInfo(name = "df2", title = "df2", type = "integer")
  manovaTable$addColumnInfo(name = "p", title = "p", type = "pvalue")

  manovaTable$addFootnote(gettext("The null hypothesis specifies equal class means."))
  
  jaspResults[["manovaTable"]] <- manovaTable

  if(!ready)  return()

  target <- as.numeric(dataset[, .v(options[["target"]])])
  predictors <- as.matrix(dataset[, .v(options[["predictors"]])])

  tryCatch({
    manovaResult <- manova(predictors ~ target)
    manovaSummary <- summary(manovaResult, test="Wilks")
    
    # Individual models
    anovaSummary <- summary.aov(manovaResult)
    for(i in 1:length(anovaSummary)){
      sumTmp <- as.matrix(anovaSummary[[i]])
      Fstat <- sumTmp[1, 4]
      df1 <- sumTmp[1, 1]
      df2 <- sumTmp[2, 1]
      p <- sumTmp[1, 5]
      row <- data.frame(model = options[["predictors"]][i], f = Fstat, df1 = df1, df2 = df2, p = p)
      manovaTable$addRows(row)
    }
  }, error = function(e) manovaTable$setError(.extractErrorMessage(e))
  )

}

.ldaEqualityOfCovarianceMatrices <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["boxTest"]]) || !options[["boxTest"]]) return()
  
  boxTest <- createJaspTable(title = gettext("Tests of Equality of Covariance Matrices"))
  boxTest$position <- position
  boxTest$dependOn(options = c("boxTest", "scaleEqualSD", "target", "predictors"))

  boxTest$addColumnInfo(name = "test", title = "",             type = "string")
  boxTest$addColumnInfo(name = "x",    title = "\u03C7\u00B2", type = "number")
  boxTest$addColumnInfo(name = "df",   title = gettext("df"),  type = "integer")
  boxTest$addColumnInfo(name = "p",    title = gettext("p"),   type = "pvalue")

  boxTest$addFootnote(gettext("The null hypothesis specifies equal covariance matrices."))
  
  jaspResults[["boxTest"]] <- boxTest

  if(!ready)  return()

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]

  testLabel <- "Box's M"
  boxSum <- .boxM(predictors, target)
  chi <- as.numeric(boxSum[["statistic"]])
  df <- as.numeric(boxSum[["parameter"]])
  p <- as.numeric(boxSum[["p.value"]])

  row <- data.frame(test = testLabel, x = chi, df = df, p = p)
  boxTest$addRows(row)
}

.ldaMulticollinearity <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["multicolTable"]]) || !options[["multicolTable"]]) return()
  
  multicolTable <- createJaspTable(title = gettext("Pooled Within-Class Matrices Correlations"))
  multicolTable$position <- position
  multicolTable$dependOn(options = c("multicolTable", "scaleEqualSD", "target", "predictors"))
  
  jaspResults[["multicolTable"]] <- multicolTable

  if(!ready)  return()

  target <- dataset[, .v(options[["target"]])]
  predictors <- dataset[, .v(options[["predictors"]])]

  boxSum <- .boxM(predictors, target)
  corPooled <- cor(boxSum[["pooled"]])

  multicolTable$addColumnInfo(name = "empty", title = "", type = "string")

  for(i in 1:ncol(corPooled)){
    multicolTable$addColumnInfo(name = paste0("v", i), title = unlist(options[["predictors"]])[i], type = "number")
  }

  for(i in 1:nrow(corPooled)){
    row <- data.frame(empty = unlist(options[["predictors"]])[i])
    for(j in 1:ncol(corPooled)){
      if(j <= i)
        row[[paste0("v", j)]] <- corPooled[j, i]
    }
    multicolTable$addRows(row)
  }

}

.ldaModelMatrix <- function(ldafit, data) {

  # adapted from MASS:::lda.formula

  x <- model.matrix(ldafit$terms, data)
  xint <- match("(Intercept)", colnames(x), nomatch = 0L)
  if (xint > 0L)
    x <- x[, -xint, drop = FALSE]
  x
}

