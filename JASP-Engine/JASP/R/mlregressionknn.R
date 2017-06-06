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

MLRegressionKNN <- function(dataset=NULL, options, state = NULL, perform="run", callback=function(...) 0, ...) {
	
    # state creation ##
    
    state[["options"]] <- options
    
    stateKey <- list()
    stateKey[["Descriptions"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut", "validationKFold")
    stateKey[["optimization"]] <- c("optimizeModel", "optimizeModelMaxK")
    stateKey[['Predictions']] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD","tablePredictionsConfidence")
    stateKey[['Distances']] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["Weights"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["Plot"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["optimizationPlot"]] <- c("optimizeModel", "optimizeModelMaxK")
    
    attr(state, "key") <- stateKey
    
    # Read variables ##
    
	predictors <- unlist(options['predictors'])
	if (length(options['target']) > 0){
		target <- unlist(options['target'])
	}
	variables.to.read <- c(predictors, target)
	variables.to.read <- variables.to.read[variables.to.read != ""]
	
	# read dataset ##
	
	if (is.null(dataset)) {
		
		if (perform == "run") {
			
			dataset <- .readDataSetToEnd(columns.as.numeric=variables.to.read)
			
		} else {
			
			dataset <- .readDataSetHeader(dataset, columns.as.numeric=variables.to.read)
		}
		
	} else {
		
		dataset <- .vdf(columns.as.numeric=variables.to.read)
	}
	
	# error handling ##
	
	for(i in 1:length(variables.to.read)){
	    
	    errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
	                         all.target = variables.to.read[i],
	                         observations.amount = "< 2",
	                         exitAnalysisIfErrors = TRUE)
	    
	}
	
	# set the seed so that every time the same set is chosen (to prevent random results) ##
	
	set.seed(1)
	
	# create results bundle ##
	
	results <- list()
	results[['title']] <- 'k-Nearest neighbors regression'
	
	# Provide the Meta to the results bundle ##
	
	meta <- list(list(name = 'KNN regression', type = 'title'),
	             list(name = 'Descriptions', type = 'table'),
	             list(name = "optimization", type = "table"),
	             list(name = 'Predictions', type = 'table'),
	             list(name = 'Weights', type = 'table'),
	             list(name = 'Distances', type = 'table'),
	             list(name = 'Plot', type = 'image'),
	             list(name = "optimizationPlot", type = "image"))
	results[['.meta']] <- meta
	
	# init state ##
	
	if(perform == "init"){
	    
	    results <- .initKnnregression(options,results, state)
	    
	    return(list(results = results, status = "inited", state = state))
	    
	} 
	
	# run state ##
	
	else {
	
	# Set the right options for the analysis ##
	    
	opt <- .setOptions(options,dataset)
	
	# code variable names into base64 ##
	
	if(length(predictors[predictors!='']) > 0){
		predictors <- .v(predictors)
	}
	if(length(target[target!='']) > 0){
		target <- .v(target)
	}
	
	# Create formula ##
	
	formula <- .makeformula(predictors,target)
	
	# Run the analysis ##
	
	if(length(predictors[predictors!='']) > 0 & length(target[target!='']) > 0){
		
		train.index <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(opt[['ntrain']]*0.01,1-(opt[['ntrain']]*0.01)))
		train <- dataset[which(train.index == TRUE), ]
		test <- dataset[which(train.index == FALSE), ]
		
		res <- .DoKNNregression(dataset,options,opt,train,test,train.index,formula,target)
		
	} else {
	    
	    res <- NULL
	    
	}
	
	# Create the Summary table ##
	
	results[['Descriptions']] <- .DescriptionsTable(predictors, target, opt, options, res, dataset, formula)
	state[["Descriptions"]] <- results[["Descriptions"]]
	

	# Create optimization table ##
	
	if(options[["optimizeModel"]]){
	    
	    results[["optimization"]] <- .optimizationTableRegression(formula, dataset, options, res)
	    state[["optimization"]] <- results[["optimization"]]
	    
	}
	
	if ( ! .shouldContinue(callback(results)))
	    return()
	
	# Create the predictions table ##
	
	if(options[['tablePredictions']]){
		
		results[['Predictions']] <- .PredictionsTable(options, opt, predictors, target, res)
		state[['Predictions']] <- results[['Predictions']]
		
	}
	
	if ( ! .shouldContinue(callback(results)))
	    return()
	
	# Create distances table
	if(options[['tableDistances']]){
		
		results[['Distances']] <- .DistancesTable(predictors,target, opt, options, res)
		state[['Distances']] <- results[['Distances']]
		
	}
	
	if ( ! .shouldContinue(callback(results)))
	    return()
	
	# Create the weights table ##
	
	if(options[['tableWeights']]){
		
		results[['Weights']] <- .WeightsTable(predictors, target, opt, options, res)
		state[['Weights']] <- results[['Weights']]
		
	}
	
	if ( ! .shouldContinue(callback(results)))
	    return()
	
	# Create the Error vs K plot ##
	
	if(options[['plotErrorVsK']] & !is.null(res)){
		
		if(options[['noOfNearestNeighbours']] == 'optimized' | options[['validationLeaveOneOut']]){
		
		results[['Plot']] <- .PlotErrorVsK(res,opt,options,dataset,formula)
		state[["Plot"]] <- results[['Plot']]
		
		}
		
	}
	
	# Create the optimization plot ## 
	
	if(options[["optimizeModel"]] && !is.null(res)){
	    
	    .plotFunc <- function(){
	        .plotOptimizationRegression(formula,dataset,options)
	    }
	    
	    imgObj <- .writeImage(width = options$plotWidth, 
	                          height = options$plotHeight, 
	                          plot = .plotFunc)
	    
	    plot <- list()
	    
	    plot[["title"]] <- "Optimization plot"
	    plot[["data"]] <- imgObj[["png"]]
	    plot[["obj"]] <- imgObj[["obj"]]
	    plot[["convertible"]] <- TRUE
	    plot[["status"]] <- "complete"
	    
	    results[["optimizationPlot"]] <- plot
	    state[["optimizationPlot"]] <- results[["optimizationPlot"]]
	    
	}
	
	# return the results bundle ##
	
	return(list(results = results, status = "complete", state = state))
	
	}
	
}

.setOptions <- function(options,dataset){
	opt <- list()
	# set K
	ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) <= 1000,
		   yes = opt[['NN']] <- 1,
		   no = ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) < 20000,
		   			yes = opt[['NN']] <- 2*round(((nrow(dataset)*0.001)+1)/2)-1, 
		   			no = ifelse(test = options[['noOfNearestNeighbours']] == 'auto' & nrow(dataset) > 21000, 
		   						yes = opt[['NN']] <- 21,
		   						no = opt[['NN']] <- 0)))
	if (options[['noOfNearestNeighbours']] == 'manual'){
		opt[['NN']] <- options[['nearestNeighboursCount']]
	} else if (options[['noOfNearestNeighbours']] == 'optimized'){
		opt[['NN']] <- options[['optimizedFrom']]:options[['optimizedTo']]
	}
	# set training data
	if(options[['percentageTrainingData']] == 'auto'){
		opt[['ntrain']] <- 80
	} else if(options[['percentageTrainingData']] == 'manual'){
		opt[['ntrain']] <- options[['trainingDataManual']]
	}
	# set distance parameter
	if(options[['distanceParameter']] == 'auto'){
		opt[['distance']] <- 2 
	} else if (options[['distanceParameter']] == 'manual'){
		opt[['distance']] <- options[['distanceParameterManual']]
	} else if (options[['distanceParameter']] == 'optimized'){
		opt[['distance']] <- 2
	}
	# set weights
	if(options[['weights']]=='unweighted'){
		opt[['weights']] <- 'rectangular'
	} else {
		opt[['weights']] <- options[['weights']]
	}
	# set NA action
	if(options[['naAction']] == 'deleteListwise'){
		opt[['NA']] <- na.omit
	} else if (options[['naAction']] == 'predict'){
		opt[['NA']] <- napredict 							# still has to be looked at
	}
	return(opt)
}

.makeformula <- function(predictors,target){
	formula <- paste(target, "~", paste(predictors, collapse=" + "))
	return(formula)
}

.DoKNNregression <- function(dataset,options,opt,train,test,train.index,formula,target){
	
	if(options[['noOfNearestNeighbours']] == 'auto' | options[['noOfNearestNeighbours']] == 'manual'){
		
		res <- .OneKregression(dataset,options,opt,train,test,train.index,formula,target)
		
	} else if (options[['noOfNearestNeighbours']] == 'optimized' & options[['optimizedFrom']] > 0){
		
		res <- .OptimizeKregression(dataset,options,opt,train,test,train.index,formula,target)
		
	} else {
		
		res <- NULL
		
	}
	
	return(res)
	
}

.OneKregression <- function(dataset,options,opt,train,test,train.index,formula,target){

	knn.fit <- kknn::kknn(formula = formula,
						  train = train,
						  test = test,
						  k = opt[['NN']],
						  distance = opt[['distance']],
						  kernel = opt[['weights']],
						  na.action = opt[['NA']],
						  scale = options[['scaleEqualSD']])
	res <- list(
		'predictions'=NULL,
		'RMSE'=NULL,
		'K' = NULL
	)
	y <- dataset[which(train.index == FALSE),target]
	res[['predictions']] <- data.frame(
		'Observation' = 1:nrow(test),
		'Real' = y,
		'Prediction'= knn.fit$fitted.values)
	res[['RMSE']] <- mean((knn.fit$fitted.values - y)^2)^0.5
	res[['Optimal.K']] <- opt[['NN']]
	res[['Weights']] <- as.matrix(knn.fit$W)
	res[['Distances']] <- as.matrix(knn.fit$D)
	return(res)
}

.OptimizeKregression <- function(dataset,options,opt,train,test,train.index,formula,target){

	RMSE <- seq_along(opt[['NN']])
	count <- 1
	for( i in opt[['NN']]){
		knn.fit <- kknn::kknn(formula = formula,
							  train = train,
							  test = test,
							  k = i,
							  distance = opt[['distance']],
							  kernel = opt[['weights']],
							  na.action = opt[['NA']],
							  scale = options[['scaleEqualSD']])
		y <- dataset[which(train.index == FALSE),target]
		RMSE[count] <- mean((knn.fit$fitted.values - y)^2)^0.5
		count <- count +1
	}
	knn.fit <- kknn::kknn(formula = formula,
						  train = train,
						  test = test,
						  k = opt[['NN']][which.min(RMSE)],
						  distance = opt[['distance']],
						  kernel = opt[['weights']],
						  na.action = opt[['NA']],
						  scale = options[['scaleEqualSD']])
	res <- list(
		'predictions' = NULL,
		'RMSE' = NULL,
		'Optimal.K' = NULL,
		'Minimal.RMSE' = NULL
	)
	res[['predictions']] <- data.frame(
		'Observation' = 1:nrow(test),
		'Real' = y,
		'Prediction'= knn.fit$fitted.values)
	res[['Weights']] <- as.matrix(knn.fit$W)
	res[['Distances']] <- as.matrix(knn.fit$D)
	res[['RMSE']] <- RMSE
	res[['Minimal.RMSE']] <- min(RMSE)
	res[['Optimal.K']] <- opt[['NN']][which.min(RMSE)]
	res[['range']] <- opt[['NN']]
	return(res)
}

.PlotKoptimizedRegression <- function(res,opt){
	plot(opt[['NN']],
		 res[['RMSE']],
		 type = 'b',
		 xlab = '',
		 ylab = '',
		 las = 1,
		 main = '',
		 bty = 'n')
	mtext(expression('RMSE'), side = 2, line = 2, cex = 1.5, font = 2)
	mtext("K", side = 1, line = 3, cex = 1.5, font = 2)
	points(opt[['NN']][which.min(res[['RMSE']])],
		   min(res[['RMSE']]),
		   pch = 19,
		   col = 'red')
	
}

.DescriptionsTable <- function(predictors, target, opt, options, res, dataset, formula){
	
	### descriptions table
	###
	
	fields_descriptions <- list(list(name = 'model', title = '', type = 'string'))
	
	if(options[["validationLeaveOneOut"]] | options[["validationKFold"]]){
	    
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type1]", title = "", type = "string")
	    
	}
	
	fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'nnc[nn]', title = 'No. Nearest Neighbors', type = 'integer')
	fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'r[rmse]', title = 'RMSE', type = 'number', format = 'dp:3')
	
	if (options[['validationLeaveOneOut']]){
	    
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type2]", title = "", type = "string")
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnloo]", title = "LOOCV nn", type = 'integer')
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmseoptim]", title = "RMSE", type = 'number', format = "dp:3")
	    
	}
	
	if (options[['validationKFold']]){
	    
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type3]", title = "", type = "string")
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnkfold]", title = "K-fold nn", type = 'integer')
	    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmsekfold]", title = "RMSE", type = 'number', format = "dp:3")
	    
	}
	
	data_descriptions <- list()
	
	if(!is.null(res)){
		
		if(options[['noOfNearestNeighbours']] == 'auto'){
			
			data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = opt[['NN']], "r[rmse]" = res[['RMSE']], "optim[type1]" = 'Auto')
			
		} else if (options[['noOfNearestNeighbours']] == 'manual'){
		 
		    data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = opt[['NN']], "r[rmse]" = res[['RMSE']], "optim[type1]" = 'Manual')
		       
		} else if (options[['noOfNearestNeighbours']] == 'optimized'){
				
				data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = res[['Optimal.K']], "r[rmse]" = res[['Minimal.RMSE']], "optim[type1]" = "optimized")
			
		}
	    
	    footnotes_N <- .newFootnotes()
	    .addFootnote(footnotes_N,paste('The model is tested on ',nrow(res[["predictions"]]), "observations"), symbol = "")
	    footnotes_N <- as.list(footnotes_N)
		
	} else {
		
		data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = ".", "r[rmse]" = ".", "optim[type1]" = "")
		footnotes_N <- .newFootnotes()
		.addFootnote(footnotes_N,paste("The model has not been applied to any data yet"), symbol = "")
		footnotes_N <- as.list(footnotes_N)
		
	}

	if(options[['validationLeaveOneOut']] & !is.null(res)){
		result <- .LOOCVregression(dataset,options,opt,formula)
		data_descriptions[[1]][["optim[type2]"]] <- "Leave-one-out"
		data_descriptions[[1]][["nnc[nnloo]"]] <- result[['OptimalK']]
		data_descriptions[[1]][["r[rmseoptim]"]] <- sqrt(result[['Minimal.MSE']])
	}
	
	if(options[['validationKFold']] & !is.null(res)){
		result_fold <- .Kfoldregression(dataset,options,opt,formula,res)
		data_descriptions[[1]][["optim[type3]"]] <- "K-fold"
		data_descriptions[[1]][["nnc[nnkfold]"]] <- result_fold[['OptimalK']]
		data_descriptions[[1]][['r[rmsekfold]']] <- result_fold[['RMSE']]
	}

	return(list(title = 'Summary',
				schema = list(fields = fields_descriptions),
				data = data_descriptions,
				footnotes = footnotes_N))
	
}

.PredictionsTable <- function(options, opt, predictors, target, res){
    
    from <- options[['predictionsFrom']]
    to <- options[["predictionsTo"]]

	
	fields <- list(
		list(name="number", title="Obs. number", type="integer"),
		list(name="real", title="Observed", type="number",format = 'dp:2'),
		list(name='predicted',title = 'Predicted', type = 'number', format = 'dp:2')
	)
	
	if(is.null(res)){
	    
	    data <- list(list(number = ".",
	                 real = ".",
	                 predicted = "."))
	    
	} else {
	    
		data <- list()
			
			for(i in from:to){
				
				data[[length(data)+1]] <- list(number = res[['predictions']][i,1],
											   real = res[['predictions']][i,2],
											   predicted = res[['predictions']][i,3],
											   .isMainRow  = FALSE)
				
			
		}
		
	}
	
	return(list(title = 'Predictions',
				schema = list(fields = fields),
				data = data))
	
}

.DistancesTable <- function(predictors,target, opt, options, res){
    
    from <- options[['predictionsFrom']]
    to <- options[["predictionsTo"]]
	
	fields_distances <- list(
		list(name="number", title="Obs. number", type="integer")
	)
	if(!is.null(res)){
	for(i in 1:res[['Optimal.K']]){
		fields_distances[[length(fields_distances)+1]] <- list(name =paste('distance',i,sep = ''),title = paste('Distance',i,sep = ' '), type = 'number', format = 'dp:2')
	} 
	} else {
	    fields_distances[[2]] <- list(name = 'distance', title = "Distance", type = 'integer')
	}
	
	if(is.null(res)){
	    
	    data_distances <- list(list(number = ".", distance = "."))
	    
	} else {
		
		data_distances <- list()
		
		for(i in from:to){	
			data_distances[[i]] <- list(number = i)
			
			if(options[['noOfNearestNeighbours']] == 'auto' | options[['noOfNearestNeighbours']] == 'manual'){
				
				for(k in 1:opt[['NN']]){
					
					data_distances[[i]][[paste('distance',k,sep = '')]] <- res[['Distances']][i,k]
					
				} 
			} else if (options[['noOfNearestNeighbours']] == 'optimized'){
				
				for(k in 1:res[['Optimal.K']]){
					
					data_distances[[i]][[paste('distance',k,sep = '')]] <- res[['Distances']][i,k]
					
				} 
				
			}
			
			data_distances[[i]][['.isMainRow']] <- FALSE
			
		}
		
	}
	
	footnotes_distances <- .newFootnotes()
	if(opt[['distance']]==1){
	.addFootnote(footnotes_distances,paste('Distances shown are the Manhattan distances'))
	} else if (opt[['distance']] == 2){
		.addFootnote(footnotes_distances,paste('Distances shown are the Euclidian distances'))	
	}
	footnotes_distances <- as.list(footnotes_distances)
	
	return(list(title = 'Distances',
				schema = list(fields = fields_distances),
				data = data_distances,
				footnotes = footnotes_distances))
	
}

.WeightsTable <- function(predictors, target, opt, options, res){
    
    from <- options[['predictionsFrom']]
    to <- options[["predictionsTo"]]
	
	fields_weights <- list(
		list(name="number", title="Obs. number", type="integer")
	)
	if(!is.null(res)){
	for(i in 1:res[['Optimal.K']]){
		
		fields_weights[[length(fields_weights)+1]] <- list(name =paste('weight',i,sep = ''),title = paste('Weight',i,sep = ' '), type = 'number', format = 'dp:2')
		
	}
	} else {
	    fields_weights[[2]] <- list(name = 'weights', title = 'Weights', type = "integer")
	}
	
	if(is.null(res)){
	    
	    data_weights <- list(list(number = ".", weights = "."))
	    
	} else {
		
		data_weights <- list()
		
		for(i in from:to){	
			
			data_weights[[i]] <- list(number = i)
			
			if(options[['noOfNearestNeighbours']] == 'auto' | options[['noOfNearestNeighbours']] == 'manual'){
				
				for(k in 1:opt[['NN']]){
					
					data_weights[[i]][[paste('weight',k,sep = '')]] <- res[['Weights']][i,k]
					
				}
				
			} else if (options[['noOfNearestNeighbours']] == 'optimized'){
				
				for(k in 1:res[['Optimal.K']]){
					
					data_weights[[i]][[paste('weight',k,sep = '')]] <- res[['Weights']][i,k]
					
				}
				
			}
			
			data_weights[[i]][['.isMainRow']] <- FALSE
			
		}	
		
	}
	
	footnotes_weights <- .newFootnotes()
	.addFootnote(footnotes_weights,paste('Weights are calculated using the',opt[['weights']], 'weighting scheme.'))
	footnotes_weights <- as.list(footnotes_weights)
	
	return(list(title = 'Weights',
				schema = list(fields = fields_weights),
				data = data_weights,
				footnotes = footnotes_weights))
	
}

.PlotErrorVsK <- function(res,opt,options,dataset,formula){
	
	if(options[['noOfNearestNeighbours']] == 'optimized' & options[['optimizedFrom']] > 0){
	
	image <- .beginSaveImage(640,480)
	.PlotKoptimizedRegression(res,opt)
	content <- .endSaveImage(image)
	
	} else if (options[['validationLeaveOneOut']] & options[['maxK']] > 0){
		
		image <- .beginSaveImage(640,480)
		.PlotLOOCVregression(.LOOCVregression(dataset,options,opt,formula))
		content <- .endSaveImage(image)
		
	}
	
	return(list(title = 'Error vs K',
				width = 640,
				height = 480,
				data = content))
	
}

.LOOCVregression <- function(dataset,options,opt,formula){

	knn.fit <- kknn::train.kknn(formula = formula,
								data = dataset,
								kmax = options[['maxK']],
								distance = opt[['distance']],
								kernel = opt[['weights']],
								na.action = opt[['NA']])
	res <- list(
		'MSE' = NULL,
		'OptimalK' = NULL,
		'optimal.weights' = NULL,
		'Minimal.MSE' = NULL
	)
	res[['MSE']] <- as.numeric(knn.fit$MEAN.SQU)
	res[['OptimalK']] <- knn.fit$best.parameters[['k']]
	res[['optimal.weights']] <- knn.fit$best.parameters[['kernel']]
	res[['Minimal.MSE']] <- min(res[['MSE']])
	return(res)
}

.PlotLOOCVregression <- function(res){
	plot(seq_along(res[['MSE']]), 
		 sqrt(res[['MSE']]), 
		 xlab="", 
		 main = '',
		 type = 'b',
		 las = 1,
		 ylab = '',
		 bty = 'n',
		 pch = 19)
	mtext(expression('RMSE'), side = 2, line = 2, cex = 1.5, font = 2)
	mtext("K", side = 1, line = 3, cex = 1.5, font = 2)
	points(seq_along(res[['MSE']])[which.min(sqrt(res[['MSE']]))],
		   min(sqrt(res[['MSE']])),
		   pch = 19,
		   col = 'red')
}

.Kfoldregression <- function(dataset,options,opt,formula,res){
	knn.fit <- kknn::cv.kknn(formula = formula,
							 data = dataset,
							 distance = opt[['distance']],
							 kernel = opt[['weights']],
							 na.action = opt[['NA']],
							 kcv = options[['noOfFolds']],
							 k = res[['Optimal.K']])
	RMSE <- mean((knn.fit[[1]][,1] - knn.fit[[1]][,2])^2)^0.5
	result <- list(
		'Predictions' = NULL,
		'RMSE' = NULL
	)
	result[['Predictions']] <- data.frame(
		'Observation' = 1:nrow(dataset),
		'True' = as.numeric(knn.fit[[1]][,1]),
		'Prediction' = as.numeric(knn.fit[[1]][,2]))
	result[['RMSE']] <- RMSE
	result[['OptimalK']] <- res[['Optimal.K']]
	return(result)
}

.initKnnregression <- function(options,results, state){
    
    if(!is.null(state[["Descriptions"]])){
        
        results[["Descriptions"]] <- state[["Descriptions"]]
        
    } else {
    
    fields_descriptions <- list(list(name = 'model', title = '', type = 'string'))
    
    if(options[["validationLeaveOneOut"]] | options[["validationKFold"]]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type1]", title = "", type = "string")
        
    }
    
    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'nnc[nn]', title = 'No. Nearest Neighbors', type = 'integer')
    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'r[rmse]', title = 'RMSE', type = 'number', format = 'dp:3')
    
    if (options[['validationLeaveOneOut']]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type2]", title = "", type = "string")
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnloo]", title = "LOOCV nn", type = 'integer')
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmseoptim]", title = "RMSE", type = 'number', format = "dp:3")
        
    }
    
    if (options[['validationKFold']]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type3]", title = "", type = "string")
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnkfold]", title = "K-fold nn", type = 'integer')
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmsekfold]", title = "RMSE", type = 'number', format = "dp:3")
        
    }
    
    data_descriptions <- list(list(model = 'k-NN model', nn = ".", rmse = "."))
    
    results[['Descriptions']] <- list(title = 'Summary',
                                      schema = list(fields = fields_descriptions),
                                      data = data_descriptions)
    
    }
    
    if(options[["tablePredictions"]]){
        
        if(!is.null(state[["Predictions"]])){
            
            results[["Predictions"]] <- state[["Predictions"]]
            
        } else {

        fields_predictions <- list(
            list(name="number", title="Obs. number", type="integer"),
            list(name="real", title="Observed", type="number",format = 'dp:2'),
            list(name='predicted',title = 'Predicted', type = 'number', format = 'dp:2')
        )

        data_predictions <- list(list(number = ".",
                                      real = ".",
                                      predicted = "."))

        results[['Predictions']] <- list(title = 'Predictions',
                                         schema = list(fields = fields_predictions),
                                         data = data_predictions)

        }
        
    }
    
    if(options[["optimizeModel"]]){
        
        if(!is.null(state[["optimization"]])){
            
            results[["optimization"]] <- state[["optimization"]]
            
        } else {
            
            optimizationTable <- list()
            
            optimizationTable[["title"]] <- "Model optimization"
            
            fields <- list(
                list(name = "model", title = "", type = "string"),
                list(name = "RMSE", title = "RMSE", type = "number", format = "sf:4;dp:3"),
                list(name = "k", title = "No. nearest neighbors", type = "integer"),
                list(name = "weights", title = "Weights", type = "string"),
                list(name = "distance", title = "Distance parameter", type = "number", format = "dp:3")
            )
            
            optimizationTable[["schema"]] <- list(fields = fields)
            
            data <- list(
                list(model = "Optimal parameters", RMSE = ".", k = ".", weights = ".", distance = ".")
            )
            
            optimizationTable[["data"]] <- data
            
            results[["optimization"]] <- optimizationTable
            
        }
        
    }

    if(options[["tableDistances"]]){
        
        if(!is.null(state[["Distances"]])){
            
            results[["Distances"]] <- state[["Distances"]]
            
        } else {

        fields_distances <- list(list(name="number", title="Obs. number", type="integer"),
                                 list(name = 'distance', title = "Distance", type = 'integer'))
        
        data_distances <- list(list(number = ".", distance = "."))
        
        results[['Distances']] <- list(title = 'Distances',
                                       schema = list(fields = fields_distances),
                                       data = data_distances)

        }
        
    }

    if(options[["tableWeights"]]){
        
        if(!is.null(state[["Weights"]])){
            
            results[["Weights"]] <- state[["Weights"]]
            
        } else {

        fields_weights <- list(list(name="number", title="Obs. number", type="integer"),
                                 list(name = 'weight', title = "Weight", type = 'integer'))
        
        data_weights <- list(list(number = ".", weight = "."))
        
        results[['Weights']] <- list(title = 'Weights',
                                       schema = list(fields = fields_weights),
                                       data = data_weights) 

    }
    
    }
    
    return(results)
    
}

.plotOptimizationRegression <- function(formula,dataset,options){
    
    res <- .optimizerKNN(formula,dataset,kmax = options[["optimizeModelMaxK"]],distance_from = 0.1,distance_to = 10)
    
    plot3D::scatter2D(y=res[["value"]], 
                      x = res[["dist"]], 
                      type = "l", 
                      bty = "n",
                      xlab = "Distance parameter", 
                      ylab = res[["lab"]],
                      las = 1, 
                      lwd = 4,
                      colvar = res[["k"]],
                      clim = switch(EXPR = length(unique(res[["k"]]))<3, 
                                    clim = c(1,res[["kmax"]]),
                                    clim = range(res[["k"]])),
                      clab = "No. nearest neighbors",
                      main = "",
                      xlim = range(res[["dist"]]),
                      NAcol = "white")
    
}

.optimizationTableRegression <- function(formula, dataset, options, res){
    
    optimizationTable <- list()
    
    optimizationTable[["title"]] <- "Model optimization"
    
    fields <- list(
        list(name = "model", title = "", type = "string"),
        list(name = "RMSE", title = "RMSE", type = "number", format = "sf:4;dp:3"),
        list(name = "k", title = "No. nearest neighbors", type = "integer"),
        list(name = "weights", title = "Weights", type = "string"),
        list(name = "distance", title = "Distance parameter", type = "number", format = "dp:1")
    )
    
    optimizationTable[["schema"]] <- list(fields = fields)
    
    if(!is.null(res)){
        
        result <- .optimizerKNN(formula = formula,dataset = dataset,kmax = options[["optimizeModelMaxK"]],distance_from = 0.1,distance_to = 10)
        
        data <- list(
            list(model = "Optimal parameters", RMSE = result[["MinStatistic"]], k = result[["OptK"]], weights = result[["OptWeights"]], distance = result[["OptDistance"]])
        )
        
    } else {
        
        data <- list(
            list(model = "Optimal parameters", RMSE = ".", k = ".", weights = ".", distance = ".")
        )  
        
    }
    
    optimizationTable[["data"]] <- data
    
    return(optimizationTable)
    
}