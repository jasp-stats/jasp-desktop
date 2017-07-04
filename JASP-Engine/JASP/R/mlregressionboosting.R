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

MLRegressionBoosting <- function (dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), state = NULL, ...) {

	return(MLBoostingMain("regression", dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), state = NULL, ...))
}

MLBoostingMain <- function (analysisType, dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), state = NULL, ...) {

	## Read Dataset ## ----
	analysisType <<- analysisType

	predictors <- unlist(options[["predictors"]])
	predictorsList <- options[["predictors"]]

	target <- options[["target"]]

	if (target == ""){
		target <- NULL
	}

	indicator <- options[["indicator"]]

	if(indicator == ""){
		indicator <- NULL
	}

	variablesToRead <- c(unlist(options$target), unlist(options$predictors))
	variablesToRead <- variablesToRead[variablesToRead != ""]

	if(is.null(dataset) && !is.null(target) && !is.null(predictors)){

		if (perform == "run"){

			if(analysisType == "regression"){
				dataset <- .readDataSetToEnd(columns.as.numeric = variablesToRead, columns.as.factor = indicator, exclude.na.listwise = c(predictors, indicator))
			}
			if(analysisType == "classification"){
				# dataset <- .readDataSetToEnd(columns.as.numeric = predictors, columns.as.factor = c(indicator,target), exclude.na.listwise = c(predictors, indicator))
				dataset <- .readDataSetToEnd(columns.as.numeric = predictors, columns = c(indicator,target), exclude.na.listwise = c(predictors, indicator))
			}

		} else {

			dataset <- .readDataSetHeader(columns = variablesToRead)

			}

	}

	## initialize result ## ----

	results <- list()

	meta <- list(
		list(name="title", type="title"),
		list(name="tableFE", type = "table"),
		list(name="tablePredictionSummary", type = "table"),
		list(name="tablePrediction", type = "table"),
		list(name="tableRF", type="table"),
		list(name="tableChisqTest", type="table"),
		list(name="plotError", type="image"),
		list(name="plotRF", type="image"),
		list(name="plotMarginalPlot", type="image"),
		list(name="tableBestTune", type = "table"))

	results[[".meta"]] = meta

	results[["title"]] = ifelse(analysisType == "regression","Boosting Regression","Boosting Classification")

	stateKey <- list(
		res = c("OOBBox",
				"Shrinkage",
				"TreeStructureBox",
				"distribution",
				"indicator",
				"methodCV",
				"numberOfIterations",
				"predictors",
				"seedBox",
				"target",
				"testBox",
				"indicator",
				"predictNA",
				"value_seed",
				"value_numberOfFold",
				"value_subsample",
				"value_percentageTraining",
				"value_numberOfCore",
				"value_MinTermNodeSide",
				"value_depthOfTree",
				"value_depthOfTreeMin",
				"value_depthOfTreeMax",
				"value_depthOfTreeStep",
				"value_MinTermNodeSideMin",
				"value_MinTermNodeSideMax",
				"value_MinTermNodeSideStep")
		)
	keep <- NULL



	## Do analysis ## ----


	if(is.null(state[["res"]])){


		errorDataType <- NULL
		res <- NULL
		hasError <- FALSE
		errorMessage <- NULL

		print(is.null(dataset))

		if(perform == "run" && !is.null(dataset)){



			validIdx <- is.na(dataset[,.v(target)])
			datasetNoNa <- dataset[!validIdx,]

			if(!is.null(indicator)){
				indicatorDotV <- .v(indicator)
				if(is.factor(datasetNoNa[,indicatorDotV])){
					appIdx <- as.logical(as.numeric(as.character(datasetNoNa[,indicatorDotV])))
				}else if(is.character(datasetNoNa[,indicatorDotV])){
					appIdx <- as.logical(as.numeric(datasetNoNa[,indicatorDotV]))
				}else if(is.numeric(datasetNoNa[,indicatorDotV])){
					appIdx <- as.logical(datasetNoNa[,indicatorDotV])
				}else if(is.logical(datasetNoNa[,indicatorDotV])){
					appIdx <- datasetNoNa[,indicatorDotV]
				}
				fitData <- datasetNoNa[!appIdx,]
				appData <- datasetNoNa[appIdx,]

			}else if (options[["predictNA"]]) {
				fitData <- datasetNoNa
				appData <- dataset[validIdx,]

			}else{

				fitData <- datasetNoNa
				appData <- NULL
			}

			customChecks <- list(
				# check custom n.cores
				function(){
					maxCore <- parallel::detectCores()-1
					if (options[["coreBox"]] == "manual_numberOfCore" && (options[["value_numberOfCore"]] > maxCore)){
						return (sprintf("Core number should not be greater than %d",maxCore))
					}
				},
				# check manual percentage training
				function(){
					if(options[["testBox"]]=="manual_percentageTrain"){
						if(options[["value_percentageTraining"]] <= 10000*options[["value_MinTermNodeSide"]]/(options[["value_subsample"]]*nrow(fitData))){
							return("Percentage of training too small!")
						}
						if(options[["value_percentageTraining"]] > 99){
							return("Percentage of training too large to run test-set validation")
						}
					}
				},
				# check manual subsample ratio
				function(){
					if(options[["OOBBox"]]=="manual_subsample" ){
						if (options[["value_subsample"]] < 10000*options[["value_MinTermNodeSide"]]/(options[["value_percentageTraining"]]*nrow(fitData))){
							return("Percentage of subsample too small!")
						}
						if (options[["value_subsample"]] > 99){
							return("Percentage of subsample too large to run out-of-bag validation!")
						}
					}
				},
				# check optimization search range
				function(){
					if(options[["TreeStructureBox"]] == "optimized_TS"){
						if(options[["value_depthOfTreeMin"]] > options[["value_depthOfTreeMax"]]){
							return("Not a valid range for depth of tree !")
						}
						if(options[["value_MinTermNodeSideMin"]] > options[["value_MinTermNodeSideMax"]]){
							return("Not a a valid range for min leaf node size")
						}
					}
				},
				# check depth of tree is calculates H statistic
				function(){
					if(options[["plotHStatistics"]]){
						if(options[["value_depthOfTree"]] < 2){
							return("Depth of tree should be grater than 1 to compute H statistic")
						}
						if(!is.null(predictors) && length(predictors) < 2){
							return("Number of predictor should be greater than 1 to compute H statistics")
						}
					}
				},
				# check indicator type: should be boolean
				function(){
					print("check indicator type")
					if(!options[["predictNA"]] && !is.null(indicator) && !base::identical(levels(as.factor(fitData[,.v(indicator)])), c("0","1"))){
						return("Indicator should be {0,1}")
					}
				},
				# deal with "Cannot compute OOB estimate or the OOB curve. No finite OOB estimates of improvement" problem
				function(){

				}
			)

			# hasError <- FALSE
			# anyErrors <- .hasErrors(dataset = datasetNoNa, perform = perform, type = c("infinity", "variance"), custom = customChecks, exitAnalysisIfErrors=TRUE)
			anyErrors <- .hasErrors(dataset = datasetNoNa, perform = perform, type = "variance", custom = customChecks, exitAnalysisIfErrors=TRUE)
			hasError <- base::identical(anyErrors, TRUE)

			if(!hasError){

				res <- try(silent = FALSE, expr ={
					.BoostingAnalysis(fitData, appData, options, predictors, target, indicator, perform = perform)
					})
			}else{

				results[["tableFE"]][["error"]] <- list(errorType = "badData", errorMessage = anyErrors[["message"]])

			}

		}

		if(isTryError(res)){
			errorMessageTmp <- .extractErrorMessage(res)

		}

	}else{

		res <- state[["res"]]

	}




	## Create output ## ----
	results[["tableFE"]] <- .tableFE(res,predictors,target,perform)

	if (options[["tableRF"]]){
		results[["tableRF"]] <- .tableRF(res,predictors,target, perform)
	}

	if (options[["tableChisq"]])
		results[["tableChisqTest"]] <- .tableChisqTest(res,predictors, target, perform)

	if (options[["plotRF"]])
		results[["plotRF"]] <- .plotRF(res,predictors,target,perform)#predictorsList


	if(options[["plotMarginalPlot"]])
		results[["plotMarginalPlot"]] <- .plotMarginal(res,options,predictors,target,perform)#predictorsList

	if(options[["modelPerforamceShowError"]])
		results[["plotError"]] <- .plotError(res, options, predictors, target, perform)


	if(options[["TreeStructureBox"]] == "optimized_TS")
		results[["tableBestTune"]] <- .tableBestTune(res, options, predictors, target, perform = perform)

	if(options[["predictNA"]] || !is.null(indicator)){
		results[["tablePrediction"]] <- .tablePrediction(res, predictors, target, perform = perform)
		if(analysisType == "classification"){
			results[["tablePredictionSummary"]] <- .tablePredictionSummary(res, predictors, target, perform = perform)
		}else{
			results[["tablePredictionSummary"]] <- NULL
		}
	}


	state <- list(
		options = options,
		res = res
	)

	attr(state, "key") <- stateKey


	if (perform == "init") {

		return(list(results = results, status = "inited", state = state, keep = keep))

	} else {

		return(list(results = results, status = "complete", state = state, keep = keep))

	}

}



.BoostingAnalysis <- function(data, appData, options, predictors, target, indicator, perform = perform){


	if (options[["seedBox"]] == "auto_seed"){
		seed <- as.integer(runif(1, -(2^31 - 1), 2^31))
	}else{
		seed <- as.numeric(options[["value_seed"]])
	}

	set.seed(seed)
	shuffleIdx <- sample(1:nrow(data),nrow(data))
	data <- data[shuffleIdx,]


	# claim target, predictors and formula
	formula = as.formula(paste(.v(target),"~",paste(.v(predictors),collapse=" + ")))

	# check whether options have contradicted with one another, assigned value to parameters
	if (!options[["methodCV"]]){
		cv.folds <- 0
	}else{
		cv.folds <- options[["value_numberOfFold"]]
	}

	# setup subsample ratio
	if (options[["OOBBox"]]=="auto_subsample"){
		bag.fraction <- 0.5
	}else{
		bag.fraction <- as.numeric(options[["value_subsample"]])
		bag.fraction <- bag.fraction/100
	}

	# setup No. cores
	if (options[["coreBox"]] == "auto_numberOfCore"){
		n.cores <- 1
	}else{
		n.cores <- options[["value_numberOfCore"]]
	}

	if (options[["testBox"]]=="auto_percentageTrain"){
		train.fraction <- 0.5
	}else if (options[["testBox"]]=="manual_percentageTrain"){
		train.fraction <- options[["value_percentageTraining"]]
		train.fraction <- train.fraction/100
	}else{
		nTrain <- options[["value_numberTraining"]]
	}

	# setup tree structure
	optStruc <- NULL
	if (options[["TreeStructureBox"]] == "auto_TS"){
		interaction.depth <- 1
		n.minobsinnode <- 10
	}else if (options[["TreeStructureBox"]] == "optimized_TS"){

		optStruc <- .findBestTreeStrucure(data, target, predictors, perform, 
			options[["numberOfIterations"]], 
			options[["Shrinkage"]], 
			# options[["distribution"]], 
			bag.fraction, train.fraction,
			options[["value_depthOfTreeMin"]],
			options[["value_depthOfTreeMax"]],
			options[["value_depthOfTreeStep"]],
			options[["value_MinTermNodeSideMin"]],
			options[["value_MinTermNodeSideMax"]],
			options[["value_MinTermNodeSideStep"]])
		interaction.depth <- optStruc[1,1]
		n.minobsinnode <- optStruc[1,2]
	}else{
		interaction.depth <- options[["value_depthOfTree"]]
		n.minobsinnode <- options[["value_MinTermNodeSide"]]
	}

	if(options[["distribution"]] == "default"){

		model <- gbm(
				formula = formula,
				data = data,
				# distribution = options[["distribution"]],
				n.trees = options[["numberOfIterations"]],
				shrinkage = options[["Shrinkage"]],
				interaction.depth = interaction.depth,
				bag.fraction = bag.fraction,
				train.fraction = train.fraction,
				n.minobsinnode = n.minobsinnode,
				cv.folds = cv.folds,
				keep.data = TRUE,
				verbose = FALSE,
				n.cores = n.cores)
	}else{
		model <- gbm(
			formula = formula,
			data = data,
			distribution = options[["distribution"]],
			n.trees = options[["numberOfIterations"]],
			shrinkage = options[["Shrinkage"]],
			interaction.depth = interaction.depth,
			bag.fraction = bag.fraction,
			train.fraction = train.fraction,
			n.minobsinnode = n.minobsinnode,
			cv.folds = cv.folds,
			keep.data = TRUE,
			verbose = FALSE,
			n.cores = n.cores)
	}


	best.iter_cv = NULL
	best.iter_test = NULL
	best.iter_oob = gbm::gbm.perf(model,plot.it = FALSE,method="OOB")
	# at least one case remain in the test set 
	if (train.fraction < (1 - 1/nrow(data))){
		best.iter_test = gbm::gbm.perf(model,plot.it = FALSE,method="test")
	}

	if (options[["methodCV"]]){

		best.iter = gbm::gbm.perf(model,plot.it = FALSE,method="cv")
		best.iter_cv = best.iter

	}else if(!is.null(best.iter_test)){ 

		best.iter = best.iter_test

	}else{

		best.iter = best.iter_oob

	}


	if(is.null(appData) || (!is.null(appData) && nrow(appData) == 0)){
		prediction <- NULL
	}else{
		if(analysisType == "regression"){
			prediction <- gbm::predict.gbm(
							model,
							newdata=appData,
							n.trees=best.iter,
							distribution = options[["distribution"]],
							shrinkage = options[["Shrinkage"]],
							interaction.depth=interaction.depth,
							n.minobsinnode = n.minobsinnode)
		}else{
			prediction <- gbm::predict.gbm(
							model,
							newdata=appData,
							n.trees=best.iter,
							distribution = options[["distribution"]],
							shrinkage = options[["Shrinkage"]],
							interaction.depth=interaction.depth,
							n.minobsinnode = n.minobsinnode,
							type = "response")
		}
	}



	return(list(model=model, 
		appData = appData, 
		prediction = prediction, 
		data = data, 
		best.iter = best.iter, 
		best.iter_cv = best.iter_cv, 
		best.iter_test = best.iter_test, 
		best.iter_oob = best.iter_oob, 
		optStruc=optStruc))

}


.varRF <- function(res){

	if(!is.null(res)){

		table <- summary(res$model,
			n.trees=res$best.iter,
			plotit=FALSE,
			order=TRUE)
	}

	test <- chisq.test(as.numeric(table[,2]))
	table_output <- cbind(table[,2],test$residuals)
	chisqTestInfo <- sprintf("The Chi-square test value is: %.02f (%d), p = %.02f", test$statistic,test$parameter,test$p.value)
	chisqTestStas <- c(test$statistic,test$parameter,test$p.value)
	rowNames <- rownames(table)

	return(list(table=table_output, rowNames = rowNames, chisqTestInfo = chisqTestInfo, chisqTestStas = chisqTestStas))

}

.tableFE <- function(res, predictors, target, perform){

	table <- list(title="Fitting error with best iteration")
	rowNames = c("OOB", "Test", "CV")
	colNames = c("Best Iteration", "Minimum Error")

	if(any(perform == "init", is.null(predictors), is.null(target), is.null(res), isTryError(res))){

		toTable <- matrix(".", nrow = 3, ncol = 2,
				  dimnames = list(rowNames, colNames))

		if(isTryError(res)){
			footnotes <- .newFootnotes()
			errorMessage <- .extractErrorMessage(res)
			.addFootnote(footnotes, symbol="<em>Error.</em>", text=errorMessage)
			table[["footnotes"]] <- as.list(footnotes)
		}


	}else{

		best.iter_oob = res$best.iter_oob
		train.error = res$model$train.error[best.iter_oob]

		if(is.null(res$best.iter_test)){
			best.iter_test = 0
			valid.error = 0
		}else{
			best.iter_test = res$best.iter_test
			valid.error = res$model$valid.error[best.iter_test]
		}

		if(is.null(res$best.iter_cv)){
			best.iter_cv = 0
			cv.error = 0
		}else{
			best.iter_cv = res$best.iter_cv
			cv.error = res$model$cv.error[best.iter_cv]
		}


		fittingInfo <- c(best.iter_oob, best.iter_test, best.iter_cv, 
			train.error, valid.error, cv.error)
		toTable <- matrix(fittingInfo, nrow = 3, ncol = 2, byrow = FALSE)
		colnames(toTable) <- colNames
		rownames(toTable) <- rowNames
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="integer", format="dp:0"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="number", format="dp:3"))
	)



	table[["data"]] <- .MLRFTables(toTable)
	return(table)
}

.tableChisqTest <- function(res, predictors,target,  perform){
	table <- list(title="Chi-square Test")
	colNames = c("Value", "df", "p")
	rowNames = c("Variables")

	if(any(perform == "init", is.null(predictors), is.null(target), isTryError(res), is.null(res))){

		toTable <- matrix(".", nrow = 1, ncol = 3,
				  dimnames = list(rowNames, colNames))

		if(isTryError(res)){
			footnotes <- .newFootnotes()
			errorMessage <- .extractErrorMessage(res)
			.addFootnote(footnotes, symbol="<em>Error.</em>", text=errorMessage)
			table[["footnotes"]] <- as.list(footnotes)
		}

	}else{
		varRF <- .varRF(res)
		toTable <- varRF$chisqTestStas
		toTable <- matrix(toTable, nrow = 1, ncol = 3, byrow = TRUE)
		colnames(toTable) <- colNames
		rownames(toTable) <- rowNames
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="number", format="dp:3"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="integer", format="dp:0"),
				  list(name = colNames[3], title = colNames[3], type="number", format="dp:3"))
	)

	table[["data"]] <- .MLRFTables(toTable)
	return(table)
}




.tableRF <- function(res,predictors,target,perform){


	table <- list(title = "Relative Importance of Variables")
	colNames = c("relative influence","chisq residuals")

	# if(any(perform == "init", is.null(res), is.null(predictors))){
	if(any(perform == "init", is.null(predictors), is.null(target), is.null(res),isTryError(res))){

		toTable <- matrix(".", nrow = 1, ncol = 2,
				  dimnames = list(".", colNames))

		if(isTryError(res)){
			footnotes <- .newFootnotes()
			errorMessage <- .extractErrorMessage(res)
			.addFootnote(footnotes, symbol="<em>Error.</em>", text=errorMessage)
			table[["footnotes"]] <- as.list(footnotes)
		}

	}else{

		varRF <- .varRF(res)
		toTable <- varRF$table
		colnames(toTable) <- colNames
		rownames(toTable) <- .unv(varRF$rowNames)
		footnotes <- .newFootnotes()
		.addFootnote(footnotes, varRF$chisqTestInfo, symbol="<i>Note</i>.")
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="number", format="dp:3"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="number", format="dp:3"))
	)

	table[["data"]] <- .MLRFTables(toTable)

	return(table)
}


.tableBestTune <- function(res, options, predictors, target, perform = perform){

	table <- list(title="Parameter Tuning for Tree Structure")
	lossType <- ifelse(analysisType == "regression", "Test Root-Mean-Square Error ", "Test Error in Accuracy")
	colNames = c("Depth of Tree", "Minimum Terminal Node Size", lossType)

	footnotes <- .newFootnotes()

	.addFootnote(footnotes, symbol="<em>Note.</em>", text = "Sorted by test set error")

	if(any(perform == "init", is.null(predictors), is.null(target), is.null(res),isTryError(res))){

		toTable <- matrix(".", nrow = 1, ncol = 3,
				  dimnames = list("", colNames))

		if(isTryError(res)){
			errorMessage <- .extractErrorMessage(res)
			.addFootnote(footnotes, symbol="<em>Error.</em>", text=errorMessage)
		}

	}else{
		data <- res$optStruc
		data <- data[order(data[,ncol(data)]),]
		data[,ncol(data)] <- round(data[,ncol(data)],3)
		data[,ncol(data)] <- sapply(data[,ncol(data)], function(x) x = ifelse(is.na(x), ".",x))
		toTable <- matrix(data, ncol = 3, byrow = FALSE)
		colnames(toTable) <- colNames
		rownames(toTable) <- as.character(1:nrow(toTable))
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="string"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="string"),
				  list(name = colNames[3], title = colNames[3], type="string"))
	)

	table[["footnotes"]] <- as.list(footnotes)

	table[["data"]] <- .MLRFTables(toTable)
	return(table)
}


.tablePrediction <- function(res, predictors, target, perform = perform){

	table <- list(title="Model Prediction")
	if(analysisType == "regression"){
		colNames <- c(unlist(predictors), sprintf("Predicted %s", target))
	}else{
		colNames <- c(unlist(predictors), sprintf("Predicted %s", target), "Probability")
	}

	mFeatures <- length(predictors)
	if(any(perform == "init", is.null(target), is.null(predictors), is.null(res),isTryError(res))){
		print("table prediction debug 1")
		toTable <- matrix(".", nrow = 1, ncol = length(colNames),
				  dimnames = list(".", colNames))

		if(isTryError(res)){
			footnotes <- .newFootnotes()
			errorMessage <- .extractErrorMessage(res)
			.addFootnote(footnotes, symbol="<em>Error.</em>", text=errorMessage)
			table[["footnotes"]] <- as.list(footnotes)
		}

	}else if(is.null(res$prediction)){
		print("table prediction debug 2")
		toTable <- matrix(".", nrow = 1, ncol = length(colNames),
				  dimnames = list(".", colNames))
		footnotes <- .newFootnotes()
		.addFootnote(footnotes, symbol="<em>Note.</em>", text="No case in application set")
		table[["footnotes"]] <- as.list(footnotes)
	}else{
		print("table prediction debug 3")
		appData <- res$appData
		if(analysisType == "regression") {
			predOutput <- round(res$prediction,3)
			predOutput <- sapply(predOutput,function(x) x = ifelse(is.na(x),".",x))
		}else{
			prediction <- res$prediction
			className <- levels(res$data[,.v(target)])
			predClassIdx <-apply(prediction,1,which.max)
			predClassName <- className[predClassIdx]
			predClassName <- sapply(predClassName,function(x) x = ifelse(is.na(x),".",x))
			predClassProb <- round(apply(prediction,1,max),3)
			predClassProb <- sapply(predClassProb,function(x) x = ifelse(is.na(x),".",x))
			predOutput <- cbind(predClassName,predClassProb)
		}

		data <- cbind(appData[,.v(predictors)],predOutput)
		toTable <- as.matrix(data)
		colnames(toTable) <- colNames
		rownames(toTable) <- as.character(1:nrow(toTable))
	}

	fields <- list(list(name="case", title="", type="string"))
	for (i in 1:(mFeatures)){
		fields[[length(fields)+1]] <- list(name = colNames[i], title = colNames[i], type="string")#type="number", format="dp:3"
	}
	if(analysisType == "regression"){
		fields[[length(fields)+1]] <- list(name = colNames[mFeatures+1], title = colNames[mFeatures+1], type="string")
	}else{
		fields[[length(fields)+1]] <- list(name = colNames[mFeatures+1], title = colNames[mFeatures+1], type="string")
		fields[[length(fields)+1]] <- list(name = colNames[mFeatures+2], title = colNames[mFeatures+2], type="string")
	}

	table[["schema"]] <- list(fields=fields)
	table[["data"]] <- .MLRFTables(toTable)

	return(table)

}

.tablePredictionSummary <- function(res, predictors, target, perform = perform){

	table <- list(title="Model Prediction Summary")
	colNames <- c("Frequency","Percentage")
	# rowNames <- levels(res$data[,.v(target)])

	if(any(perform == "init", is.null(target), is.null(predictors), is.null(res),isTryError(res))){
		toTable <- matrix(".", nrow = 1, ncol = 2,
				  dimnames = list(".", colNames))
	}else if(!is.null(res) && is.null(res$prediction)){
		toTable <- matrix(".", nrow = 1, ncol = 2,
				  dimnames = list(".", colNames))
	}else{
		className <- levels(res$data[,.v(target)])
		prediction <- res$prediction
		predClassIdx <-apply(prediction,1,which.max)
		freq <- table(predClassIdx)
		data <- c(freq,freq/sum(freq))
		toTable <- matrix(data, ncol = 2, byrow = FALSE)
		colnames(toTable) <- colNames
		print("debug:")
		print(className)
		print(toTable)
		rownames(toTable) <- className[as.numeric(row.names(freq))]
	}

	table[["schema"]] <- list(
	fields = list(list(name="case", title="", type="string"),
				  list(name = colNames[1], title = colNames[1], type="integer", format="dp:0"),#sf:4;dp:3
				  list(name = colNames[2], title = colNames[2], type="number", format="dp:3"))
	)
	table[["data"]] <- .MLRFTables(toTable)

	return(table)
}

.MLRFTables <- function(x) {

	n = nrow(x)
	m = ncol(x)

	fieldNames = c("case", colnames(x))
	rmns = rownames(x)

	emptyRow <- vector("list", length = length(fieldNames))
	names(emptyRow) <- fieldNames

	data <- rep(list(emptyRow), n)

	if (is.numeric(x)) { # implies .clean must be used
		for (i in seq_len(n)) {

			data[[i]] <- c(case = rmns[i], lapply(x[i, ], .clean))

		}
	} else {
		for (i in seq_len(n)) {

			data[[i]] <- c(case = rmns[i], x[i, ])

		}
	}

	return(data)

}


.plotRF <- function(res,predictors,target,perform){

	plotRF <- list(title = "Relative Importance")

	if(perform == "run" && !isTryError(res) && !is.null(predictors) && !is.null(target)){


		len = length(predictors)

		if (len < 4){
			width <- 400
			height <- 300
		}else{
			width <- 400
			height <- 300 + (len-3)*30
		}

		plotRF[["width"]] <- width
		plotRF[["height"]] <- height

		p <- try(silent = FALSE, expr = {

			varRF <- summary(res$model,
				n.trees=res$best.iter,
				plotit=FALSE,
				order=TRUE
			)


			toPlot <- data.frame(
				Feature = .unv(varRF[,1]),
				Importance = as.numeric(varRF[,2])
			)

			# sort variables on desending order
			# toPlot <- toPlot[order(toPlot[,2],toPlot[,1]), ]

			toPlot <- toPlot[order(toPlot[["Importance"]], decreasing = TRUE), ]
			axisLimits <- range(pretty(toPlot[["Importance"]]))
			axisLimits[1] <- min(c(0, axisLimits[1]))
			axisLimits[2] <- max(c(0, axisLimits[2]))

			# image <- .beginSaveImage(width = plotRF[["width"]], height = plotRF[["height"]])
			# par(mar=c(5.1, 6.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
			# barplot(toPlot[[2]], rep(1,len), legend.text=TRUE,beside=TRUE,
			# 	names.arg = toPlot[[1]],
			# 	horiz = TRUE, xlim = c(axisLimits[1],axisLimits[2]), 
			# 	xlab = "",cex.names=1.5, las=1)
			# content <- .endSaveImage(image)
########################################################
			# image <- .beginSaveImage(width = plotRF[["width"]], height = plotRF[["height"]])
			# library(JASPgraphs)
			g <- drawCanvas(xName = "", yName = "")
			g <- drawBars(g, dat = toPlot, size = 4, stat = "identity",mapping = NULL)#+
				# ggplot2::scale_x_discrete(limits = c(axisLimits[1],axisLimits[2]))
			g <- themeJasp(g, horizontal = TRUE)
			# print(g)
			# content <- .endSaveImage(image)
########################################################

			content <- .writeImage(width = width, height = height, plot = g, obj = TRUE)

			plotRF[["data"]] <- content
			plotRF[["convertible"]] <- TRUE
			plotRF[["obj"]] <- content[["obj"]]
			plotRF[["data"]] <- content[["png"]]
			plotRF[["status"]] <- "complete"

		})

		if(isTryError(p)){
			errorMessage <- .extractErrorMessage(res)
			plotRF[["error"]] <- list(error="badData", errorMessage=errorMessage)
		}

	}else if(isTryError(res)){

		errorMessage <- .extractErrorMessage(res)
		plotRF[["error"]] <- list(error="badData", errorMessage=errorMessage)
	}

	return(plotRF)
}

.plotError <- function(res, options, predictors, target, perform){

	yName = list(default = "Squared error loss", bernoulli="Bernoulli deviance", multinomial="Multinomial deviance", gaussian="Squared error loss", laplace="Absolute loss", tdist="t-distribution loss", huberized = "Huberized hinge loss", adaboost="AdaBoost exponential loss", 
		poisson = "Poisson loss")

	ylab = yName[[options[["distribution"]]]]

	plot <- list(
		title = "Error vs Iteration",
		width = 600,
		height = 500
	)

	legendName <- c()
	legendColor <- c()
	yUpper <- 0

	if(perform == "run" && !isTryError(res) && !is.null(predictors) && !is.null(target) && !is.null(res)){


		model <- res$model

		train.error <- model$train.error
		valid.error <- model$valid.error
		cv.error <- model$cv.error
		n.trees <- model$n.trees

		iteration <- c(seq(1,n.trees,1),seq(1,n.trees,1))
		error <- c(train.error,valid.error)
		methodType <- c(rep("train",n.trees),rep("test",n.trees))

		if(options[["methodCV"]]){
			yUpper <- max(train.error,valid.error,cv.error)
			iteration <- c(iteration,seq(1,n.trees,1))
			error <- c(error,cv.error)
			methodType <- c(methodType,rep("cv",n.trees))
		}else{
			yUpper <- max(train.error,valid.error)
		}
		yUpper = round(yUpper)+1

		p <- try(silent=FALSE, expr = {

			# image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
			# # plot(1,type="n",ylab=ylab,xlab='Iteration',ylim=c(-1,yUpper),xlim=c(0,n.trees),main="error v.s. iteration",cex = 1.5, font = 2)
			# plot(1,type="n",ylab = "", xlab = "",ylim=c(0,yUpper),xlim=c(0,n.trees),main="",cex.lab = 1.5, font.lab = 2, axes = F, cex.axis = 1.3, bty = "n", las = 1)

			# axis(1, at = seq(0,n.trees+1,50),)
			# mtext("Iteration", side = 1, line = 3, cex = 1.5, font = 2)
			# axis(2, pos = -1,)
			# # par(las = 0)
			# mtext(ylab, side = 2, line = 2, cex = 1.5, font = 2)
			# # train error
			# lines(seq(1,n.trees,1),train.error,col=1)
			# legendName <- c(legendName,sprintf("OOB"))
			# legendColor <- c(legendColor,1)

			# # test error
			# lines(seq(1,n.trees,1),valid.error,col=2)
			# legendName <- c(legendName,sprintf("test"))
			# legendColor <- c(legendColor,2)

			# # cv error
			# if (options[["methodCV"]]){
			# 	print("ploting cv error")
			# 	lines(seq(1,n.trees,1),cv.error,col=3)
			# 	legendName <- c(legendName,sprintf("cv"))
			# 	legendColor <- c(legendColor,3)
			# }

			# legend("topright",legendName, lty=c(1,1,1),col = legendColor)

			# plot[["data"]] <- .endSaveImage(image)

###########################################################################################

			toPlot <- data.frame(x=iteration, y=error, methodType = methodType)
			g <- drawCanvas(xName = "Iteration", yName = ylab, dat = toPlot)
			g <- drawLines(g, dat = toPlot, alpha = 0.8 ,mapping = ggplot2::aes(x = x, y = y, color = methodType))
			g <- themeJasp(g)

			content <- .writeImage(width = plot$width, height = plot$height, plot = g, obj = TRUE)

			plot[["data"]] <- content
			plot[["convertible"]] <- TRUE
			plot[["obj"]] <- content[["obj"]]
			plot[["data"]] <- content[["png"]]
			plot[["status"]] <- "complete"



		})

		if(isTryError(p)){
			errorMessage <- .extractErrorMessage(res)
			plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
		}

	}else if(isTryError(res)){

		errorMessage <- .extractErrorMessage(res)
		plot[["error"]] <- list(error="badData", errorMessage=errorMessage)

	}

	return(plot)


}



# multi plots with shared legend
# original script copy from https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend_mixed <- function(plots, ncol = length(plots), nrow = 1, position = c("bottom", "right")) {

	gl <- list()
	legend <- NULL

	for (i in 1:length(plots)){
		if(is.null(legend)){
			# g <- try(ggplot2::ggplotGrob(plots[[i]] + ggplot2::theme(legend.position = position))$grobs, silent = TRUE)
			g <- ggplot2::ggplotGrob(plots[[i]] + ggplot2::theme(legend.position = position))$grobs
			legend <- try(g[[which(sapply(g, function(x) x$name) == "guide-box")]], silent = TRUE)
			if(isTryError(legend)) {

				legend <- NULL
			}
		}
		p <- try(plots[[i]] + ggplot2::theme(legend.position="none"), silent=TRUE)
		if(isTryError(p)) p <- plots[[i]]
		# gl <- c(gl,p)
		gl[[i]] <- p
	}
	gl <- c(gl, ncol = ncol, nrow = nrow)

	if(!is.null(legend)){

		lheight <- sum(legend$height)
		lwidth <- sum(legend$width)
		position <- match.arg(position)

		combined <- switch(position,
			"bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
						legend,
						ncol = 1,
						heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
			"right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
						legend,
						ncol = 2,
						widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))

	}else{

		combined <- gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl))
		# combined <- gridExtra::arrangeGrob(gl)
	}

	grid::grid.newpage()

	grid::grid.draw(combined)
	# return gtable invisibly
	invisible(combined)

}


.plotMarginal <- function(res,options,predictors,target,perform){


	plot <- list(title = "Marginal plot")

	if(perform == "run" && !isTryError(res) && !is.null(predictors) && !is.null(target) && !is.null(res)){

		# set plot size
		varNum = length(predictors)

		len <- length(predictors)
		if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]] && !options[["plotHStatistics"]]){
			width <- 450
			height <- 300*len
		}else{
			if (len < 3){
				width <- 900
				height <- 900
			}else{
				width <- 450*len
				height <- 450*len
			}
		}

		plot[["width"]] <- width
		plot[["height"]] <- height


		p <- try(silent = FALSE, expr ={

			# image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
			# 1 way partial dependent plot only
			if(options[["plotMarginalPlotOneWay"]] && !options[["plotMarginalPlotTwoWay"]]){

				plotLists <- list()
				for (i in 1:varNum){

					grid <- plot(res$model,i.var=i,n.trees=res$best.iter,return.grid=TRUE)
					p <- .plotMarginalOneWay(grid,i,predictors[i],withHeader = TRUE, withYLab = FALSE)
					plotLists[[i]] <- p
				}

				content <- .writeImage(width = width, height = height,
					plot = grid_arrange_shared_legend_mixed(plotLists, nrow = varNum, ncol = 1, position = "bottom"), obj = TRUE)
				plot[["data"]] <- content
				plot[["convertible"]] <- TRUE
				plot[["obj"]] <- content[["obj"]]
				plot[["data"]] <- content[["png"]]
				plot[["status"]] <- "complete"

			}

			if(options[["plotMarginalPlotTwoWay"]] || options[["plotHStatistics"]]){
				image <- .beginSaveImage(width = plot[["width"]], height = plot[["height"]])
				par(mfrow= c(varNum, varNum), cex.axis= 1.3, mar= c(3, 4, 2, 1.5) + 0.1, oma= c(0.2, 2.2, 2, 0))

				grid::grid.newpage()
				grid::pushViewport(grid::viewport(layout = grid::grid.layout(varNum, varNum)))

				for (row in 1:varNum){

					withHeader <- FALSE

					for (col in 1:varNum){

						withYLab <- FALSE

						if (row==col){

							if(options[["plotMarginalPlotOneWay"]]){

								grid <- plot(res$model,i.var=row,n.trees=res$best.iter,return.grid=TRUE)
								p <- .plotMarginalOneWay(grid,row,predictors[row],withHeader = withHeader, withYLab = withYLab) +
									ggplot2::theme(legend.position="none")
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								print(p, vp=grid::viewport(layout.pos.row=row,layout.pos.col=col))
							}else{
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
							}

						}
						if(col>row){

							if(options[["plotMarginalPlotTwoWay"]]){
								grid <- plot(res$model,i.var=c(row,col),return.grid = T)
								print(.plotMarginalTwoWay(grid,row,col, predictors[col], withHeader, theta), vp=grid::viewport(layout.pos.row=row,layout.pos.col=col))
							}else{
								main <- ""
								print(plot(1, type= "n", axes= FALSE, ylab="", xlab="", main = main), vp=grid::viewport(layout.pos.row=row,layout.pos.col=col))
							}
						}

						if(col<row){

							if(options[["plotHStatistics"]]){
								HValue <- gbm::interact.gbm(res$model,
									res$data,
									i.var = c(row,col),
									n.trees = res$best.iter
								)
								p <- .plotHstatistics(HValue, col, row, predictors[row], withYLab)
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
								print(p, vp=grid::viewport(layout.pos.row=row,layout.pos.col=col))
							}else{

								# yName <-""
								# p <- drawCanvas(xName = "", yName = yName) + ggplot2::geom_blank()
								plot(1, type= "n", axes= FALSE, ylab="", xlab="")
							}
						}


					}
				}

				# draw name of each variables
				if (len > 2 || ((len == 2 && options[["plotMarginalPlotOneWay"]]) || (len == 2 && options[["plotHStatistics"]]))) {

					textpos <- seq(1/(len*2), (len*2-1)/(len*2), 2/(len*2))

					if (! options[["plotMarginalPlotOneWay"]] && !options[["plotHStatistics"]]) {

							for (t in seq_along(textpos)) {

								mtext(text = predictors[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)

								if (t < length(textpos)) {

									mtext(text = predictors[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)

								}
							}

					} else {

						for (t in seq_along(textpos)) {

								mtext(text = predictors[t], side = 3, outer = TRUE, at= textpos[t], cex=1.5, line= -0.8)
								mtext(text = predictors[t], side = 2, outer = TRUE, at= rev(textpos)[t], cex=1.5, line= -0.1, las= 0)
						}
					}
				}
				plot[["data"]] <- .endSaveImage(image)
			}

			# plot[["data"]] <- content
			# plot[["convertible"]] <- TRUE
			# plot[["obj"]] <- content[["obj"]]
			# plot[["data"]] <- content[["png"]]
			# plot[["status"]] <- "complete"

		})

		if(isTryError(p)){
			errorMessage <- .extractErrorMessage(p)
			plot[["error"]] <- list(error="badData", errorMessage=errorMessage)
		}

	}else if(isTryError(res)){

		errorMessage <- .extractErrorMessage(res)
		plot[["error"]] <- list(error="badData", errorMessage=errorMessage)

	}

	return (plot)

}

.plotMarginalOneWay <- function(returnGrid,varIdx,varName,withHeader,withYLab){


	yLower = round(min(returnGrid[,2]))
	yUpper = round(max(returnGrid[,2])) + 1
	xMin = round(min(returnGrid[,1]))
	xMax = round(max(returnGrid[,1])) + 1

	main <- ifelse(withHeader,varName,"")
	yName <- ifelse(withYLab,varName,"")



####################################### ggplot version ####################################################
	# if (length(returnGrid[,2])>length(returnGrid[,1])){
	if(analysisType == "classification"){

		className <- colnames(returnGrid[,2])
		x <- NULL
		y <- NULL
		tag <- NULL
		# plot(1, type= "n", axes= TRUE, ylab=varName, xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
		for (i in 1:length(className)){
			# lines(returnGrid[,1],returnGrid[,2][,i],col=i)
			x <- c(x,returnGrid[,1])
			y <- c(y,returnGrid[,2][,i])
			tag <- c(tag,rep(className[i],length(returnGrid[,1])))
		}
		toPlot = data.frame(x = x, y = y, tag = tag)
		g <- drawCanvas(xName = "", yName = yName, dat = toPlot)
		g <- drawLines(g, dat = toPlot, alpha = 1, mapping = ggplot2::aes(x = x, y = y, color = tag), , show.legend = TRUE) + 
			ggplot2::ggtitle(main)
		g <- themeJasp(g)

	}else{
		x <- returnGrid[,1]
		y <- returnGrid[,2]
		toPlot = data.frame(x = x, y = y)
		g <- drawCanvas(xName = "", yName = yName, dat = toPlot)
		g <- drawLines(g, dat = toPlot, alpha = .8) +
			ggplot2::ggtitle(main)
		g <- themeJasp(g)
	}

	return(g)
############################################ basic version ####################################
	# if(analysisType == "classification"){
	# 	className <- colnames(returnGrid[,2])
	# 	plot(1, type= "n", axes= TRUE, ylab=varName, xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=2)
	# 	for (i in 1:length(className)){
	# 		lines(returnGrid[,1],returnGrid[,2][,i],col=i)
	# 	}
	# }else{
	# 	plot(returnGrid[,1],returnGrid[,2], type= "l", ylab= varName, xlab="",main=main,ylim=c(yLower,yUpper),xlim=c(xMin,xMax),cex.lab=3)
	# }
##############################################################################################
}

.plotMarginalTwoWay <- function(partial,varIdx1,varIdx2,main,withHeader, theta = NULL){

	main <- ifelse(withHeader,main,"")
	# if(is.null(theta)) theta <- 120
	theta <- 120
	mat <- reshape2::acast(data = partial, 
		formula = as.formula(paste(colnames(partial)[1],"~",colnames(partial)[2])), 
		value.var = colnames(partial)[3])
##################################### 3D surface plot###############################

	persp(x = as.numeric(colnames(mat)), 
		y = as.numeric(rownames(mat)), 
		z=mat,
		zlab = "",#'Partial dependence', 
		xlab = "",#colnames(partial)[1]
		ylab = "",# colnames(partial)[2]
		main = main,
		theta = theta,
		ticktype = "detailed")


################################### heat map #######################################
	# # toPlot <- data.frame(x = as.numeric(colnames(mat)), y = as.numeric(rownames(mat)), z = mat)
	# g <- drawCanvas(xName = "", yName = "", xLabels = NULL, yLabels = NULL)
	# g <- drawHeatmap(graph = g, dat = mat, interpolate = TRUE, show.legend = TRUE) +
	# 	ggplot2::ggtitle(main)
	# g <- themeJasp(g)
#####################################################################################

}

.plotHstatistics <- function(HValueAll, colVarIdx, rowVarIdx, yName, withYLab){

	yName <- ifelse(withYLab,yName,"")
####################################### ggplot version #####################################
	if(analysisType == "regression"){
		toPlot <- data.frame(x = c(0,1,2), y = c(0,HValueAll,0))
		g <- drawCanvas(xName = "", yName = yName, dat = toPlot, xLabels = NULL)
		g <- drawBars(g, dat = toPlot, size = 3, stat = "identity", fill="black", alpha = 0.7) +
			ggplot2::annotate("text", x = 1, y = HValueAll + 0.005, label= sprintf("%0.2f",HValueAll)) +
			ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
		g <- themeJasp(g, horizontal = FALSE)
	}else{
		toPlot <- data.frame(x = names(HValueAll), y = unname(HValueAll))
		g <- drawCanvas(xName = "", yName = yName, dat = toPlot)
		g <- drawBars(g, dat = toPlot, mapping = ggplot2::aes(x = x, y = y, fill = x), size = 3, stat = "identity")
		for (i in 1:length(HValueAll)){
			g <- g + ggplot2::annotate("text", x= i , y=HValueAll[i]+0.01, label= sprintf("%0.2f",HValueAll[i]))
		}

		g <- themeJasp(g, horizontal = FALSE)
	}
	return(g)

####################################### basic version #######################################
	# if(analysisType == "regression"){
	# 	# barplot(HValueAll, main="", xlab="", ylab="", las = 1, col = "grey", cex.lab = 1.7, 
	# 	# 	cex.main = 1.5, axes = TRUE, ylim = c(0, 0.5))
	# 	text(0.5,0.5, labels=sprintf("H = %0.2f",HValueAll),cex=2)
	# }else{
	# 	barplot(HValueAll, main="", xlab="", ylab=yName, las = 1, col = "grey", cex.lab = 1.7, 
	# 			cex.main = 1.5, axes = TRUE, ylim = c(0, 0.5))
	# 	for (i in 1:length(HValueAll)) {
	# 		text(i-0.5,HValueAll[i]+0.05,labels=sprintf("H = %0.2f",HValueAll[i]),cex=2)
	# 	}
	# }
############################################################################################
}


.findBestTreeStrucure <- function(data, target, predictors, perform, 
	n.trees, shrinkage, bag.fraction, train.fraction,
	interaction.depth_min=1, interaction.depth_max=5, interaction.depth_step=1,
	n.minobsinnode_min=5, n.minobsinnode_max=20, n.minobsinnode_step=5){

	cv.folds = 3
	repeatTime = 3


	target1 <- .v(target)
	formula = as.formula(paste(.v(target),"~",paste(.v(predictors),collapse=" + ")))

	coreMax <- parallel::detectCores()
	doParallel::registerDoParallel(cores = coreMax)

	"%dopar%" <- foreach::"%dopar%"
	"%do%" <- foreach::"%do%"


	resAll_rp <- NULL
	foreach::foreach(j=1:repeatTime) %dopar%{

	# shuffle the dataset
	seeds <- as.integer(runif(1, -(2^31 - 1), 2^31))
	set.seed(seeds)
	data <- data[sample(nrow(data)),]

	#Create sub folds
	folds <- cut(seq(1,nrow(data)),breaks=cv.folds,labels=FALSE)
	resAll <- NULL

		foreach::foreach(i = 1:cv.folds) %dopar% {

			#Segment data by fold using the which() function
			interaction.depthInterval <- seq(interaction.depth_min,interaction.depth_max, interaction.depth_step)
			n.minobsinnodeInterval <- seq(n.minobsinnode_min, n.minobsinnode_max,n.minobsinnode_step)
			IDLen = length(interaction.depthInterval)
			NMLen = length(n.minobsinnodeInterval)
			res <- rep(NaN,IDLen*NMLen)
			# res <- NULL
			testIndexes <- which(folds==i,arr.ind=TRUE)
			testData <- data[testIndexes, ]
			testY <- data[testIndexes, target1]
			trainData <- data[-testIndexes, ]

			#Use the test and train data partitions however you desire...
			# cv.sse <- foreach::foreach(interaction.depth = interaction.depthInterval, .combine = rbind) %do% {
			# 	foreach::foreach(n.minobsinnode = n.minobsinnodeInterval, .combine = rbind) %do% {
			cv.sse <- foreach::foreach(pointer1 = seq(1,IDLen,1), .combine = rbind) %do% {
				foreach::foreach(pointer2 = seq(1,NMLen,1), .combine = rbind) %do% {

					# # TRAIN A PROJECTION PURSUIT REGRESSION WITH VARIOUS SETTINGS AND TRAINING DATA
					interaction.depth <- interaction.depthInterval[pointer1]
					n.minobsinnode <- n.minobsinnodeInterval[pointer2]

					if(analysisType == "regression"){

						model_gbm <- try(silent = TRUE, expr = {
							gbm::gbm(
								formula = formula,
								data = trainData,
								# distribution = distribution,
								n.trees=n.trees, # number of trees
								shrinkage=shrinkage, # shrinkage or learning rate,
								# # 0.001 to 0.1 usually work
								interaction.depth=interaction.depth, # 1: additive model, 2: two-way interactions, etc.
								bag.fraction = bag.fraction, # subsampling fraction, 0.5 is probably best
								train.fraction = train.fraction, # fraction of data for training,
								## first train.fraction*N used for training
								n.minobsinnode = n.minobsinnode, # minimum total weight needed in each node
								# class.stratify.cv = TRUE,
								keep.data=FALSE, # keep a copy of the dataset with the object
								verbose=FALSE, # don't print out progress
								# cv.folds = 3,
								n.cores=coreMax
							)})
						if(isTryError(model_gbm)){

							test.error <- NaN

						}else{
							yHat <- gbm::predict.gbm(
								model_gbm,
								newdata=testData,
								n.trees=n.trees,
								shrinkage = shrinkage,
								interaction.depth=interaction.depth,
								n.minobsinnode = n.minobsinnode
							)

							test.error <- sqrt(mean((yHat-testY)^2))
						}

					}else{

						model_gbm <- try(silent = TRUE, expr = {
							gbm::gbm(
								formula = formula,
								data = trainData,
								distribution = distribution,
								n.trees=n.trees, # number of trees
								shrinkage=shrinkage, # shrinkage or learning rate,
								# # 0.001 to 0.1 usually work
								interaction.depth=interaction.depth, # 1: additive model, 2: two-way interactions, etc.
								bag.fraction = bag.fraction, # subsampling fraction, 0.5 is probably best
								train.fraction = train.fraction, # fraction of data for training,
								## first train.fraction*N used for training
								n.minobsinnode = n.minobsinnode, # minimum total weight needed in each node
								class.stratify.cv = TRUE,
								keep.data=FALSE, # keep a copy of the dataset with the object
								verbose=FALSE, # don't print out progress
								# cv.folds = 3,
								n.cores=coreMax
							)})
						if(isTryError(model_gbm)){
							test.error <- NaN
						}else{
							yHat <- gbm::predict.gbm(
								model_gbm,
								newdata=testData,
								n.trees=n.trees,
								shrinkage = shrinkage,
								interaction.depth=interaction.depth,
								n.minobsinnode = n.minobsinnode,
								type = "response"
							)
							className <- levels(data[,target1])
							yHatMat <- matrix(yHat,ncol = length(className), byrow = FALSE)
							# colnames(yHatMat) <- dimnames(yHat)[[2]]
							testClassIdx <- unlist(lapply(testY, function(x) which(className == x))) # covert class name to col number
							prob <- diag(yHatMat[,testClassIdx]) # for each row, select specific col given index in vector testIdx
							test.error <- 1-mean(prob)
						}
					}
					# res <- c(res,test.error)
					res[(pointer1-1)*IDLen + pointer2] <- test.error
				}
			}
			# store result in file, since use foreach
			cat(dput(res), file = paste0("res_", i, "_", j, ".txt"))

		}

		for (i in 1:cv.folds){

			res <- NULL
			res <- read.table(paste0("res_", i, "_", j, ".txt"))
			resAll <- cbind(resAll,as.numeric(res))
			file.remove(paste0("res_", i, "_", j, ".txt"))
		}

		resAll <- apply(resAll,1,function(x) mean(x,na.rm = TRUE))
		cat(dput(resAll), file = paste0("resAll_", j, ".txt"))

	}
	resAll_rp <- NULL
	for (j in 1:repeatTime){
		resAll<- NULL
		resAll <- read.table(paste0("resAll_", j, ".txt"))
		resAll_rp <- cbind(resAll_rp,as.numeric(resAll))
		file.remove(paste0("resAll_", j, ".txt"))
	}

	# lossType <- ifelse(analysisType == "regression","root mean square error","test error percentage")
	# print(analysisType)
	# print(lossType)
	resAll_rp <- apply(resAll_rp,1,function(x) mean(x,na.rm = TRUE))
	resAll_rp <- matrix(cbind(seq(interaction.depth_min,interaction.depth_max, interaction.depth_step),
		seq(n.minobsinnode_min, n.minobsinnode_max,n.minobsinnode_step),resAll_rp),ncol = 3,byrow = FALSE)

	# colnames(resAll_rp) <- c("interaction.depth","n.minobsinnode",lossType)

	return(resAll_rp)

}

###############################################################################
# Following is the source code from gbm package (https://github.com/harrysouthworth/gbm/tree/master/R)
# If called through "gbm::gbm", there is an error "Rscript execution error: No such file or directory"
# which possibly come from function gbmCluster(), in which the parallel package is called
# and leads to an error "not a valid cluster" in its function default.cluster()
# which seem to be a bug in parallel package
# see https://stackoverflow.com/questions/28415654/parallelization-of-bnlearn-with-parallel-package
#################################################################################

gbm <- function(formula = formula(data),
                distribution = "bernoulli",
                data = list(),
                weights,
                subset = NULL,
                offset = NULL,
                var.monotone = NULL,
                n.trees = 100,
                interaction.depth = 1,
                n.minobsinnode = 10,
                shrinkage = 0.001,
                bag.fraction = 0.5,
                train.fraction = 1.0,
                mFeatures = NULL,
                cv.folds=0,
                keep.data = TRUE,
                verbose = 'CV',
                class.stratify.cv=NULL,
                n.cores=NULL){
   theCall <- match.call()


   lVerbose <- if (!is.logical(verbose)) { FALSE }
               else { verbose }

   mf <- match.call(expand.dots = FALSE)
   m <- match(c("formula", "data", "weights", "subset", "offset"), names(mf), 0)
   mf <- mf[c(1, m)]
   mf$drop.unused.levels <- TRUE
   mf$na.action <- na.pass
   mf[[1]] <- as.name("model.frame")
   m <- mf
   mf <- eval(mf, parent.frame())
   Terms <- attr(mf, "terms")
   y <- model.response(mf)

   if (missing(distribution)){ distribution <- gbm::guessDist(y) }
   else if (is.character(distribution)){ distribution <- list(name=distribution) }

   w <- model.weights(mf)
   offset <- model.offset(mf)

   # get the character name of the response variable
   response.name <- as.character(formula[[2]])

   var.names <- attributes(Terms)$term.labels
   x <- model.frame(terms(reformulate(var.names)),
                    data,
                    na.action=na.pass,
                    subset=subset)

#  x <- mf[, !is.element(names(mf), response.name)]

   lVerbose <- if (!is.logical(verbose)) { FALSE }
               else { verbose }

   class.stratify.cv <- gbm::getStratify(class.stratify.cv, distribution)

   # groups (for pairwise distribution only)
   group      <- NULL
   num.groups <- 0

   # determine number of training instances
   if (distribution$name != "pairwise"){
      nTrain <- floor(train.fraction * nrow(x))
   }
   else {
      # distribution$name == "pairwise":
      # Sampling is by group, so we need to calculate them here
      distribution.group <- distribution[["group"]]
      if (is.null(distribution.group))
      {
         stop("For pairwise regression, the distribution parameter must be a list with a parameter 'group' for the list of the column names indicating groups, for example list(name=\"pairwise\",group=c(\"date\",\"session\",\"category\",\"keywords\")).")
      }

      # Check if group names are valid
      i <- match(distribution.group, colnames(data))
      if (any(is.na(i)))
      {
         stop("Group column does not occur in data: ", distribution.group[is.na(i)])
      }

      # Construct group index
      group <- factor(do.call(paste, c(data[,distribution.group, drop=FALSE], sep=":")))

      # Check that weights are constant across groups
      if ((!missing(weights)) && (!is.null(weights)))
      {
         w.min <- tapply(w, INDEX=group, FUN=min)
         w.max <- tapply(w, INDEX=group, FUN=max)

         if (any(w.min != w.max))
         {
            stop("For distribution 'pairwise', all instances for the same group must have the same weight")
         }

         # Normalize across groups
         w <- w * length(w.min) / sum(w.min)
      }

      # Shuffle groups, to remove bias when splitting into train/test set and/or CV folds
      perm.levels  <- levels(group)[sample(1:nlevels(group))]
      group        <- factor(group, levels=perm.levels)

      # The C function expects instances to be sorted by group and descending by target
      ord.group    <- order(group, -y)
      group        <- group[ord.group]
      y            <- y[ord.group]
      x            <- x[ord.group,,drop=FALSE]
      w            <- w[ord.group]

      # Split into train and validation set, at group boundary
      num.groups.train <- max(1, round(train.fraction * nlevels(group)))

      # include all groups up to the num.groups.train
      nTrain           <- max(which(group==levels(group)[num.groups.train]))
      Misc             <- group
   } # close if(distribution$name=="coxph") ...

    #Determine the number of features to consider at each node
    if (is.null(mFeatures)) {
      mFeatures <- ncol(x)
    } else {
      if (mFeatures > ncol(x)) {
        print("mFeatures was greater than the number of columns. It was reset to the available features.")
        mFeatures <- ncol(x)
      } else {
        mFeatures <- max(mFeatures, 1)
      }
    }

   cv.error <- NULL

   # If CV is used, final model is calculated within the cluster
   if(cv.folds>1) {
     cv.results <- gbmCrossVal(cv.folds, nTrain, n.cores,
                               class.stratify.cv, data,
                               x, y, offset, distribution, w, var.monotone,
                               n.trees, interaction.depth, n.minobsinnode,
                               shrinkage, bag.fraction, mFeatures,
                               var.names, response.name, group, lVerbose, keep.data)
     cv.error <- cv.results$error
     p        <- cv.results$predictions
     gbm.obj  <- cv.results$all.model
   } 

   else {
   gbm.obj <- gbm::gbm.fit(x,y,
                      offset = offset,
                      distribution = distribution,
                      w = w,
                      var.monotone = var.monotone,
                      n.trees = n.trees,
                      interaction.depth = interaction.depth,
                      n.minobsinnode = n.minobsinnode,
                      shrinkage = shrinkage,
                      bag.fraction = bag.fraction,
                      nTrain = nTrain,
                      # mFeatures = mFeatures,
                      keep.data = keep.data,
                      verbose = lVerbose,
                      var.names = var.names,
                      response.name = response.name,
                      group = group)
   }

   gbm.obj$train.fraction <- train.fraction
   gbm.obj$Terms <- Terms
   gbm.obj$cv.error <- cv.error
   gbm.obj$cv.folds <- cv.folds
   gbm.obj$call <- theCall
   gbm.obj$m <- m
   if (cv.folds > 1){ gbm.obj$cv.fitted <- p }

   if (distribution$name == "pairwise")
   {
      # Data has been reordered according to queries.
      # We need to permute the fitted values to correspond
      # to the original order.
      gbm.obj$ord.group <- ord.group
      gbm.obj$fit <- gbm.obj$fit[order(ord.group)]
   }

   return(gbm.obj)
}


gbmCrossVal <- function(cv.folds, nTrain, n.cores,
                        class.stratify.cv, data,
                        x, y, offset, distribution, w, var.monotone,
                        n.trees, interaction.depth, n.minobsinnode,
                        shrinkage, bag.fraction, mFeatures,
                        var.names, response.name, group, lVerbose, keep.data) {
  i.train <- 1:nTrain
  cv.group <- gbm::getCVgroup(distribution, class.stratify.cv, y,
                         i.train, cv.folds, group)
  ## build the models
  cv.models <- gbmCrossValModelBuild(cv.folds, cv.group, n.cores,
                                     i.train, x, y, offset,
                                     distribution, w, var.monotone,
                                     n.trees, interaction.depth,
                                     n.minobsinnode, shrinkage,
                                     bag.fraction, mFeatures, var.names,
                                     response.name, group, lVerbose, keep.data, nTrain)

  # First element is final model
  all.model <- cv.models[[1]]
  cv.models <- cv.models[-1]

  ## get the errors

  cv.error  <- gbm::gbmCrossValErr(cv.models, cv.folds, cv.group, nTrain, n.trees)
  best.iter.cv <- which.min(cv.error)

  ## get the predictions
  predictions <- gbm::gbmCrossValPredictions(cv.models, cv.folds, cv.group,
                                        best.iter.cv, distribution,
                                        data[i.train,,drop=FALSE], y)
  list(error=cv.error,
       predictions=predictions,
       all.model=all.model)
}



## Perform gbm cross-validation
##
## This function has far too many arguments.
gbmCrossValModelBuild <- function(cv.folds, cv.group, n.cores, i.train,
                                  x, y, offset, distribution,
                                  w, var.monotone, n.trees,
                                  interaction.depth, n.minobsinnode,
                                  shrinkage, bag.fraction, mFeatures,
                                  var.names, response.name,
                                  group, lVerbose, keep.data, nTrain) {
  ## set up the cluster and add a finalizer
  # print("gbmCluster")
  cluster <- gbmCluster(n.cores)
  on.exit(if (!is.null(cluster)){ parallel::stopCluster(cluster) })

  ## get ourselves some random seeds
  seeds <- as.integer(runif(cv.folds, -(2^31 - 1), 2^31))

  ## now do the cross-validation model builds
  # cluster <- NULL
  # there seem to be a bug in parLapply according to 
  # https://stackoverflow.com/questions/28415654/parallelization-of-bnlearn-with-parallel-package
  if ( ! is.null(cluster) ){
    parallel::parLapply(cl=cluster, X=0:cv.folds,
            gbmDoFold, i.train, x, y, offset, distribution,
            w, var.monotone, n.trees,
            interaction.depth, n.minobsinnode, shrinkage,
            bag.fraction, mFeatures,
            cv.group, var.names, response.name, group, seeds, lVerbose, keep.data, nTrain)
  }
  else {
    lapply(X=0:cv.folds,
            gbmDoFold, i.train, x, y, offset, distribution,
            w, var.monotone, n.trees,
            interaction.depth, n.minobsinnode, shrinkage,
            bag.fraction, mFeatures,
            cv.group, var.names, response.name, group, seeds, lVerbose, keep.data, nTrain)
  }

}


gbmCluster <- function(n){
    # If number of cores (n) not given, try to work it out from the number
    # that appear to be available and the number of CV folds.
    if (is.null(n)){
      n <- max(1, parallel::detectCores() - 1)
    }
    if (n == 1){ NULL }
    else { parallel::makeCluster(n) }
}


gbmDoFold <- function(X,
         i.train, x, y, offset, distribution, w, var.monotone, n.trees,
         interaction.depth, n.minobsinnode, shrinkage, bag.fraction, mFeatures,
         cv.group, var.names, response.name, group, s, lVerbose, keep.data, nTrain){
    # Do specified cross-validation fold - a self-contained function for
    # passing to individual cores.

    # library(gbm, quietly=TRUE)
    
    # Handle the final model case separately
    if (X == 0){
      res <- gbm::gbm.fit(x,y,
                       offset = offset,
                       distribution = distribution,
                       w = w,
                       var.monotone = var.monotone,
                       n.trees = n.trees,
                       interaction.depth = interaction.depth,
                       n.minobsinnode = n.minobsinnode,
                       shrinkage = shrinkage,
                       bag.fraction = bag.fraction,
                       nTrain = nTrain,
                       # mFeatures = mFeatures,
                       keep.data = keep.data,
                       verbose = lVerbose,
                       var.names = var.names,
                       response.name = response.name,
                       group = group)
    } else {
      if (lVerbose) message("CV:", X, "\n")
      set.seed(s[[X]])
      i <- order(cv.group == X)
      x <- x[i.train,,drop=FALSE][i,,drop=FALSE]
      y <- y[i.train][i]
      offset <- offset[i.train][i]
      nTrain <- length(which(cv.group != X))
      group <- group[i.train][i]

      res <- gbm::gbm.fit(x, y,
                     offset=offset, distribution=distribution,
                     w=w, var.monotone=var.monotone, n.trees=n.trees,
                     interaction.depth=interaction.depth,
                     n.minobsinnode=n.minobsinnode,
                     shrinkage=shrinkage,
                     bag.fraction=bag.fraction,
                     nTrain=nTrain, 
                     # mFeatures=mFeatures, 
                     keep.data=FALSE,
                     verbose=FALSE, 
                     response.name=response.name,
                     group=group)
  }
  res
}