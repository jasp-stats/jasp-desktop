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

MLClassificationKNN <- function(dataset=NULL, state = NULL, options, perform="run", callback=function(...) 0, ...) {
    
    # state creation ##
    
    state[["options"]] <- options
    
    stateKey <- list()
    stateKey[["Descriptions"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD", "validationLeaveOneOut", "validationKFold")
    stateKey[["optimization"]] <- c("optimizeModel", "optimizeModelMaxK", "optimizeModelMaxD")
    stateKey[["Confusion"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "scaleEqualSD")
    stateKey[['Predictions']] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[['Distances']] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["Weights"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["newData"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["Plot"]] <- c("noOfNearestNeighbours", "nearestNeighboursCount", "percentageTrainingData", "trainingDataManual", "distanceParameter", "distanceParameterManual", "weights", "optimizedFrom", "optimizedTo", "naAction", "predictionsFrom", "predictionsTo", "scaleEqualSD")
    stateKey[["optimizationPlot"]] <- c("optimizeModel", "optimizeModelMaxK")
    
    attr(state, "key") <- stateKey
    
    # Read variables ##
    
    predictors <- unlist(options['predictors'])
    
    if (length(options['target']) > 0){
        target <- unlist(options['target'])
    }
    if(length(options[["indicator"]]) > 0){
        indicator <- options[["indicator"]]
    }
    
    variables.to.read <- c(predictors, target, indicator)
    variables.to.read <- variables.to.read[variables.to.read != ""]
    
    # read dataset ##
    
    if (is.null(dataset)) {
        
        if (perform == "run") {
            
            dataset <- .readDataSetToEnd(columns = variables.to.read)
            
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
    results[['title']] <- 'k-nearest neighbors classification'
    
    # Provide the Meta to the results bundle ##
    
    meta <- list(list(name = 'knn Classification', type = 'title'),
                 list(name = 'Descriptions', type = 'table'),
                 list(name = "optimization", type = "table"),
                 list(name = "Confusion", type = "table"),
                 list(name = 'Predictions', type = 'table'),
                 list(name = 'Weights', type = 'table'),
                 list(name = 'Distances', type = 'table'),
                 list(name = "newData", type = "table"),
                 list(name = 'Plot', type = 'image'),
                 list(name = "optimizationPlot", type = "image"))
    
    results[['.meta']] <- meta
    
    # init state ##
    
    if(perform == "init"){
        
        results <- .initKnnClassification(options = options, results = results, state = state)
        
        return(list(results = results, status = "inited", state = state))
        
    } 
    
    # run state ##
    
    else {
        
        # Set the right options for the analysis ##
        
        opt <- .setOptionsClassification(options = options,dataset = dataset)
        
        # code variable names into base64 ##
        
        if(length(predictors[predictors!='']) > 0){
            predictors <- .v(predictors)
        }
        if(length(target[target!='']) > 0){
            target <- .v(target)
        }
        
        # Create formula ##
        
        formula <- .makeformulaClassification(predictors = predictors,target = target)
        
        # Run the analysis ##
        
        if(length(predictors[predictors!='']) > 0 & length(target[target!='']) > 0){
            
            dataset <- na.omit(dataset)
            
            train.index <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(opt[['ntrain']]*0.01,1-(opt[['ntrain']]*0.01)))
            train <- dataset[which(train.index == TRUE), ]
            test <- dataset[which(train.index == FALSE), ]
            
            res <- .DoKNNClassification(dataset = dataset,options = options,opt = opt,train = train,test = test,train.index = train.index,formula = formula,target = target)
            
        } else {
            
            res <- NULL
            
        }
       
         # create the summary table ##
        
        results[['Descriptions']] <- .DescriptionsTableClassification(predictors = predictors, target = target, opt = opt, options = options, res = res, dataset = dataset, formula = formula)
        state[["Descriptions"]] <- results[['Descriptions']]
        
        
        # create the optimization table ##
        
        if(options[["optimizeModel"]]){
            
            tmp_results <- .optimizationTable(formula, dataset, options, res)
            
            results[["optimization"]] <- tmp_results[["optimizationTable"]]
            state[["optimization"]] <- results[["optimization"]]
            
            # save result for plot 
            plot_data <- tmp_results[["plot_data"]]
    
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # Create the confusion table ##
        
        if(options[['confusionTable']]){
            
            results[['Confusion']] <- .ConfusionTableClassifiaction(res = res, target = target, dataset = dataset)
            state[["Confusion"]] <- results[['Confusion']]
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # Create the predictions table ##
        
        if(options[['tablePredictions']]){
            
            results[['Predictions']] <- .PredictionsTableClassification(options = options, opt = opt, predictors = predictors, target = target, res = res)
            state[['Predictions']] <- results[['Predictions']]
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # Create the distances table ##
        
        if(options[['tableDistances']]){
            
            results[['Distances']] <- .DistancesTableClassification(predictors = predictors,target = target, opt = opt, options = options, res = res)
            state[['Distances']] <- results[['Distances']]
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # Create the weights table ##
        
        if(options[['tableWeights']]){
            
            results[['Weights']] <- .WeightsTableClassification(predictors = predictors, target = target, opt = opt, options = options, res = res)
            state[["Weights"]] <- results[['Weights']]
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        if(options[["indicator"]] != "" && !is.null(res)){
            
            results[["newData"]] <- .predictKNNclassification(dataset, options, predictors, formula, res, indicator, opt)
            state[["newData"]] <- results[["newData"]]
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # Create the Error vs K plot ##
        
        if(options[['plotErrorVsK']] & !is.null(res)){
            
            if(options[['noOfNearestNeighbours']] == 'optimized'){
                
                results[['Plot']] <- .PlotErrorVsKClassification(res = res,opt = opt,options = options,dataset = dataset,formula = formula)
                state[["Plot"]] <- results[["Plot"]]
                
            }
            
        }
        
        if ( ! .shouldContinue(callback(results)))
            return()
        
        # create the optimization plot ##
        
        if(options[["optimizeModel"]] && !is.null(res)){
            
            .plotFunc <- function(){
                .plotOptimization(plot_data)
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
        
        # return the results ##
        
        return(list(results = results, status = "complete", state = state))
        
    }
    
}

.setOptionsClassification <- function(options,dataset){
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

.makeformulaClassification <- function(predictors,target){
    formula <- paste(target, "~", paste(predictors, collapse=" + "))
    return(formula)
}

.DoKNNClassification <- function(dataset,options,opt,train,test,train.index,formula,target){
    
    if(options[['noOfNearestNeighbours']] == 'auto' | options[['noOfNearestNeighbours']] == 'manual'){
        
        res <- .OneKClassification(dataset,options,opt,train,test,train.index,formula,target)
        
    } else if (options[['noOfNearestNeighbours']] == 'optimized'){
        
        res <- .OptimizeKClassification(dataset,options,opt,train,test,train.index,formula,target)
        
    } else {
        
        res <- NULL
        
    }
    
    return(res)
    
}

.OneKClassification <- function(dataset,options,opt,train,test,train.index,formula,target){
    
    knn.fit <- kknn::kknn(formula = formula,
                          train = train,
                          test = test,
                          k = opt[['NN']],
                          distance = opt[['distance']],
                          kernel = opt[['weights']],
                          na.action = opt[['NA']],
                          scale = options[['scaleEqualSD']])
    res <- list()
    if(is.numeric(knn.fit$fitted.values)){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = round(knn.fit$fitted.values,0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = as.character(knn.fit$fitted.values))
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values,'Real'=test[,target])
    }
    res[['confidence']] <- as.matrix(knn.fit$prob)
    res[['model.error']] <- 1 - sum(diag(prop.table(res[['confusion.table']])))
    res[['Optimal.K']] <- opt[['NN']]
    res[['Weights']] <- as.matrix(knn.fit$W)
    res[['Distances']] <- as.matrix(knn.fit$D)
    res[['predictions']] <- as.matrix(res[['predictions']])
    return(res)
}

.OptimizeKClassification <- function(dataset,options,opt,train,test,train.index,formula,target){
    
    error <- seq_along(opt[['NN']])
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
        if(is.numeric(knn.fit$fitted.values)){
            confusion_table <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
        } else {
            confusion_table <- table('Pred'=knn.fit$fit,'Real'=test[,target])
        }
        error[count] <- 1 - sum(diag(prop.table(confusion_table)))
        count <- count + 1
    }
    knn.fit <- kknn::kknn(formula = formula,
                          train = train,
                          test = test,
                          k = opt[['NN']][which.min(error)],
                          distance = opt[['distance']],
                          kernel = opt[['weights']],
                          na.action = opt[['NA']],
                          scale = options[['scaleEqualSD']])
    res <- list()
    if(is.numeric(knn.fit$fitted.values)){
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = round(knn.fit$fitted.values,0))
        res[['confusion.table']] <- table('Pred'=round(knn.fit$fitted.values,0),'Real'=test[,target])
    } else {
        res[['predictions']] <- data.frame(
            'Observation' = 1:nrow(test),
            'Real' = as.character(test[,target]),
            'Prediction' = knn.fit$fitted.values)
        res[['confusion.table']] <- table('Pred'=knn.fit$fitted.values,'Real'=test[,target])
    }
    res[['confidence']] <- knn.fit$prob
    res[['Model.error']] <- error
    res[['Minimal.error']] <- min(error)
    res[['Optimal.K']] <- opt[['NN']][which.min(error)]
    res[['Weights']] <- as.matrix(knn.fit$W)
    res[['Distances']] <- as.matrix(knn.fit$D)
    res[['range']] <- opt[['NN']]
    return(res)
}

.PlotKoptimizedClassification <- function(res,opt){
    # plot(opt[['NN']],
    #      1- res[['Model.error']],
    #      type = 'b',
    #      xlab = '',
    #      ylab = '',
    #      las = 1,
    #      main = '',
    #      bty = 'n')
    # mtext(expression('Accuracy'), side = 2, line = 2, cex = 1.5, font = 2)
    # mtext("K", side = 1, line = 3, cex = 1.5, font = 2)
    # points(opt[['NN']][which.min(res[["Model.error"]])],
    #        min(res[['Model.error']]),
    #        pch = 19,
    #        col = 'red')
    
    library(JASPgraphs)
    
    xName = "No. nearest neighbors"
    yName = "Accuracy"
    x <- opt[["NN"]]
    y <- 1-res[["Model.error"]]
    toPlot = data.frame(x = x, y = y)
    
    g <- drawCanvas(xName = xName, yName = yName, dat = toPlot, xBreaks = seq(min(x), max(x), 1), xLabels = seq(min(x), max(x), 1))
    g <- drawLines(g, dat = toPlot, alpha = .25)
    g <- drawPoints(g, dat = toPlot, size = 5, alpha = .65)
    g <- drawPoints(g, dat = data.frame(x = opt[['NN']][which.min(res[["Model.error"]])], y = max(y)),fill = "red", size = 5)
    g <- themeJasp(g)
    
    return(g)
    
}

.DescriptionsTableClassification <- function(predictors, target, opt, options, res, dataset, formula){
    
    ### descriptions table
    ###
    
    fields_descriptions <- list(list(name = 'model', title = '', type = 'string'))
    
    if(options[["validationLeaveOneOut"]] | options[["validationKFold"]]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type1]", title = "", type = "string")
        
    }
    
    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'nnc[nn]', title = 'No. nearest neighbors', type = 'integer')
    fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'r[rmse]', title = 'Accuracy', type = 'number', format = 'dp:3')
    
    if (options[['validationLeaveOneOut']]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type2]", title = "", type = "string")
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnloo]", title = "LOOCV nn", type = 'integer')
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmseoptim]", title = "Accuracy", type = 'number', format = "dp:3")
        
    }
    
    if (options[['validationKFold']]){
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type3]", title = "", type = "string")
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnkfold]", title = "K-fold nn", type = 'integer')
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmsekfold]", title = "Accuracy", type = 'number', format = "dp:3")
        
    }
    
    data_descriptions <- list()
    
    if(!is.null(res)){
        
        if(options[['noOfNearestNeighbours']] == 'auto'){
            
            data_descriptions[[1]] <- list(model = 'k-nn model', "nnc[nn]" = opt[['NN']], "r[rmse]" = 1-res[['model.error']], "optim[type1]" = 'Auto')
            
        } else if (options[['noOfNearestNeighbours']] == 'manual'){
            
            data_descriptions[[1]] <- list(model = 'k-nn model', "nnc[nn]" = opt[['NN']], "r[rmse]" = 1-res[['model.error']], "optim[type1]" = 'Manual')
            
        } else if (options[['noOfNearestNeighbours']] == 'optimized'){
            
            data_descriptions[[1]] <- list(model = 'k-nn model', "nnc[nn]" = res[['Optimal.K']], "r[rmse]" = 1-res[['Minimal.error']], "optim[type1]" = "Optimized")
            
        }
        
        footnotes_N <- .newFootnotes()
        .addFootnote(footnotes_N,paste('The model is tested on ',nrow(res[["predictions"]]), "observations"), symbol = "")
        footnotes_N <- as.list(footnotes_N)
        
    } else {
        
        data_descriptions[[1]] <- list(model = 'k-nn model', "nnc[nn]" = ".", "r[rmse]" = ".", "optim[type1]" = "")
        footnotes_N <- .newFootnotes()
        .addFootnote(footnotes_N,paste("The model has not been applied to any data yet"), symbol = "")
        footnotes_N <- as.list(footnotes_N)
    }
    
    if(options[['validationLeaveOneOut']] & !is.null(res)){
        result <- .LOOCVClassification(dataset,options,opt,formula, res)
        data_descriptions[[1]][["optim[type2]"]] <- "Leave-one-out"
        data_descriptions[[1]][["nnc[nnloo]"]] <- result[['Optimal.K']]
        data_descriptions[[1]][["r[rmseoptim]"]] <- 1 - sqrt(result[['minimal.error']])
    }
    
    if(options[['validationKFold']] & !is.null(res)){
        result_fold <- .KfoldClassification(dataset,options,opt,formula,res)
        data_descriptions[[1]][["optim[type3]"]] <- paste(options[['noOfFolds']],"-fold", sep = "")
        data_descriptions[[1]][["nnc[nnkfold]"]] <- result_fold[['Optimal.K']]
        data_descriptions[[1]][['r[rmsekfold]']] <- 1 - result_fold[['minimal.error']]
    }
        
        return(list(title = 'Summary',
                    schema = list(fields = fields_descriptions),
                    data = data_descriptions,
                    footnotes = footnotes_N))

    
}

.PredictionsTableClassification <- function(options, opt, predictors, target, res){
    
    from <- ifelse(test = options[["predictionsFrom"]] > nrow(res[["predictions"]]),yes = 1,no = options[["predictionsFrom"]])
    
    to <- ifelse(test = options[['predictionsTo']] > nrow(res[["predictions"]]),yes = nrow(res[["predictions"]]), no = options[["predictionsTo"]])
    
    fields <- list(
        list(name="number", title="Obs. number", type="integer"),
        list(name="real", title="Observed", type="string"),
        list(name='predicted',title = 'Predicted', type = 'string')
    )
    if(options[['tablePredictionsConfidence']]){
        fields[[length(fields)+1]] <- list(name ='confidence',title = 'Confidence', type = 'number', format = 'dp:2')
    }
    
    if(is.null(res) & !options[['tablePredictionsConfidence']]){
        
        data <- list(list(number = ".",
                          real = ".",
                          predicted = "."))
        
    } else if (is.null(res) & options[["tablePredictionsConfidence"]]) {
        
        data <- list(list(number = ".",
                          real = ".",
                          predicted = ".",
                          confidence = "."))
        
    } else {
        
        data <- list()
        
        if(options[['tablePredictionsConfidence']]){
            
            for(i in from:to){
                
                data[[length(data)+1]] <- list(number = as.numeric(res[['predictions']][i,1]),
                                               real = as.character(res[['predictions']][i,2]),
                                               predicted = as.character(res[['predictions']][i,3]),
                                               confidence = as.numeric(max(res[["confidence"]][i,])))
                
            }
            
        } else {
            
            for(i in from:to){
                
                data[[length(data)+1]] <- list(number = as.numeric(res[['predictions']][i,1]),
                                               real = as.character(res[['predictions']][i,2]),
                                               predicted = as.character(res[['predictions']][i,3]))
                
            }
            
        }
        
    }
    
    data <- data[!sapply(data,is.null)]
    
    if (!is.null(res) && (options[["predictionsFrom"]] > options[["predictionsTo"]])){
        
        error <- list(errorType = "badData",
                      errorMessage = "Please specify a valid range of observation values")
        
        return(list(title = 'Predictions',
                    schema = list(fields = fields),
                    data = data,
                    error = error))
        
    } else {
    
    return(list(title = 'Predictions',
                schema = list(fields = fields),
                data = data))
        
    }
    
}

.DistancesTableClassification <- function(predictors,target, opt, options, res){
    
    from <- ifelse(test = options[["predictionsFrom"]] > nrow(res[["predictions"]]),yes = 1,no = options[["predictionsFrom"]])
    
    to <- ifelse(test = options[['predictionsTo']] > nrow(res[["predictions"]]),yes = nrow(res[["predictions"]]), no = options[["predictionsTo"]])
    
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
            
        }
        
    }
    
    data_distances <- data_distances[!sapply(data_distances,is.null)]
    
    footnotes_distances <- .newFootnotes()
    if(opt[['distance']]==1){
        .addFootnote(footnotes_distances,paste('Distances shown are the Manhattan distances'))
    } else if (opt[['distance']] == 2){
        .addFootnote(footnotes_distances,paste('Distances shown are the Euclidian distances'))	
    }
    footnotes_distances <- as.list(footnotes_distances)
    
    if (!is.null(res) && (options[["predictionsFrom"]] > options[["predictionsTo"]])){
        
        error <- list(errorType = "badData",
                      errorMessage = "Please specify a valid range of observation values")
        
        return(list(title = 'Distances',
                    schema = list(fields = fields_distances),
                    data = data_distances,
                    footnotes = footnotes_distances,
                    error = error))
        
    } else {
        
        return(list(title = 'Distances',
                    schema = list(fields = fields_distances),
                    data = data_distances,
                    footnotes = footnotes_distances))
        
    }
    
}

.WeightsTableClassification <- function(predictors, target, opt, options, res){
    
    from <- ifelse(test = options[["predictionsFrom"]] > nrow(res[["predictions"]]),yes = 1,no = options[["predictionsFrom"]])
    
    to <- ifelse(test = options[['predictionsTo']] > nrow(res[["predictions"]]),yes = nrow(res[["predictions"]]), no = options[["predictionsTo"]])
    
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
            
        }	
        
    }
    
    data_weights <- data_weights[!sapply(data_weights,is.null)]
    
    footnotes_weights <- .newFootnotes()
    .addFootnote(footnotes_weights,paste('Weights are calculated using the',opt[['weights']], 'weighting scheme.'))
    footnotes_weights <- as.list(footnotes_weights)
    
    if (!is.null(res) && (options[["predictionsFrom"]] > options[["predictionsTo"]])){
        
        error <- list(errorType = "badData",
                      errorMessage = "Please specify a valid range of observation values")
        
        return(list(title = 'Weights',
                    schema = list(fields = fields_weights),
                    data = data_weights,
                    footnotes = footnotes_weights,
                    error = error))
        
    } else {
    
        return(list(title = 'Weights',
                    schema = list(fields = fields_weights),
                    data = data_weights,
                    footnotes = footnotes_weights))
        
    }
    
}

.PlotErrorVsKClassification <- function(res,opt,options,dataset,formula){
    
    plot <- list()
    
    if(options[['noOfNearestNeighbours']] == 'optimized'){
        
        g <- .PlotKoptimizedClassification(res,opt)
        
        imgObj <- .writeImage(width = options$plotWidth, 
                              height = options$plotHeight, 
                              plot = g)

        
    }
    
    plot[["title"]] <- "Accuracy vs. No. nearest neighbors"
    plot[["data"]] <- imgObj[["png"]]
    plot[["obj"]] <- imgObj[["obj"]]
    plot[["convertible"]] <- TRUE
    plot[["status"]] <- "complete"
    
    return(plot)
    
}

.LOOCVClassification <- function(dataset,options,opt,formula, res){
    
    knn.fit <- kknn::train.kknn(formula = formula,
                                data = dataset,
                                ks = res[["Optimal.K"]],
                                distance = opt[['distance']],
                                kernel = opt[['weights']],
                                na.action = opt[['NA']],
                                scale = options[["scaleEqualSD"]])
    res <- list()
    res[['error']] <- as.numeric(knn.fit$MISCLASS)
    res[['Optimal.K']] <- knn.fit$best.parameters$k
    res[['optimal.weights']] <- knn.fit$best.parameters$kernel
    res[['minimal.error']] <- min(res[['error']])
    return(res)
}

.KfoldClassification <- function(dataset,options,opt,formula,res){
    
    knn.fit <- kknn::cv.kknn(formula = formula,
                             data = dataset,
                             distance = opt[['distance']],
                             kernel = opt[['weights']],
                             na.action = opt[['NA']],
                             kcv = options[['noOfFolds']],
                             k = res[['Optimal.K']])
    error <- 1 - length(which(knn.fit[[1]][,1] == knn.fit[[1]][,2]))/nrow(dataset)
    result <- list()
    result[['error']] <- error
    result[['Optimal.K']] <- res[['Optimal.K']]
    result[["minimal.error"]] <- min(error)
    return(result)    
}

.initKnnClassification <- function(options,results,state){
    
    if(!is.null(state[["Descriptions"]])){
        
        results[["Descriptions"]] <- state[["Descriptions"]]
        
    } else {
        
        fields_descriptions <- list(list(name = 'model', title = '', type = 'string'))
        
        if(options[["validationLeaveOneOut"]] | options[["validationKFold"]]){
            
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type1]", title = "", type = "string")
            
        }
        
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'nnc[nn]', title = 'No. Nearest Neighbors', type = 'integer')
        fields_descriptions[[length(fields_descriptions)+1]] <- list(name = 'r[rmse]', title = 'Model error', type = 'number', format = 'dp:3')
        
        if (options[['validationLeaveOneOut']]){
            
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type2]", title = "", type = "string")
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnloo]", title = "LOOCV nn", type = 'integer')
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmseoptim]", title = "Model error", type = 'number', format = "dp:3")
            
        }
        
        if (options[['validationKFold']]){
            
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "optim[type3]", title = "", type = "string")
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "nnc[nnkfold]", title = "K-fold nn", type = 'integer')
            fields_descriptions[[length(fields_descriptions)+1]] <- list(name = "r[rmsekfold]", title = "Model error", type = 'number', format = "dp:3")
            
        }
        
        data_descriptions <- list(list(model = 'k-nn model', nn = ".", rmse = "."))
        
        results[['Descriptions']] <- list(title = 'Summary',
                                          schema = list(fields = fields_descriptions),
                                          data = data_descriptions)
        
    }
    
    if(options[["optimizeModel"]]){
        
        if(!is.null(state[["optimization"]])){
            
            results[["optimization"]] <- state[["optimization"]]
            
        } else {
            
            optimizationTable <- list()
            
            optimizationTable[["title"]] <- "Model optimization"
            
            fields <- list(
                list(name = "model", title = "", type = "string"),
                list(name = "RMSE", title = "Accuracy", type = "number", format = "dp:3"),
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
    
    if(options[['confusionTable']]){
        
        if(!is.null(state[["Confusion"]])){
            
            results[["Confusion"]] <- state[["Confusion"]]
            
        } else {
            
            fields_confusion <- list(list(name = "varname_pred", title = "", type = "string"))
            
            for( i in 1:2){
                
                fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = ".", type = "integer")
                
            }
            
            data_confusion <- list()
            
            for(i in 1:2){
                
                dat_tmp <- list("varname_pred" = ".")
                
                for(j in 1:2){
                    
                    dat_tmp[[paste("varname_real",j,sep="")]] <- "."
                    
                }
                
                data_confusion[[length(data_confusion)+1]] <- dat_tmp
                
            }
            
            results[["Confusion"]] <- list(title = "Confusion table",
                                           schema = list(fields = fields_confusion),
                                           data = data_confusion)
            
        }
        
    }
    
    if(options[["tablePredictions"]]){
        
        if(!is.null(state[["Predictions"]])){
            
            results[["Predictions"]] <- state[["Predictions"]]
            
        } else {
            
            if(options[["tablePredictionsConfidence"]]){
                
                fields_predictions <- list(
                    list(name="number", title="Obs. number", type="integer"),
                    list(name="real", title="Observed", type="number",format = 'dp:2'),
                    list(name='predicted',title = 'Predicted', type = 'number', format = 'dp:2'),
                    list(name="confidence", title = "Confidence", type = "number", format = "dp:2")
                )
                
                data_predictions <- list(list(number = ".",
                                              real = ".",
                                              predicted = ".",
                                              confidence = "."))
                
            } else if (!options[["tablePredictionsConfidence"]]){
                
                fields_predictions <- list(
                    list(name="number", title="Obs. number", type="integer"),
                    list(name="real", title="Observed", type="number",format = 'dp:2'),
                    list(name='predicted',title = 'Predicted', type = 'number', format = 'dp:2')
                )
                
                data_predictions <- list(list(number = ".",
                                              real = ".",
                                              predicted = "."))
            }
            
            results[['Predictions']] <- list(title = 'Predictions',
                                             schema = list(fields = fields_predictions),
                                             data = data_predictions)
            
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
    
    if(options[["indicator"]] != "" && !is.null(state[["newData"]])){
        
        results[["newData"]] <- state[["newData"]]
        
    }
    
    
    return(results)
    
}

.ConfusionTableClassifiaction <- function(res, target, dataset){
    
    title_observed <- "Observed"
    
    if(!is.null(res)){
        
        fields_confusion <- list()
        
        fields_confusion[[length(fields_confusion)+1]] <- list(name = "pred_name", title = "", type = "string")
        
        fields_confusion[[length(fields_confusion)+1]] <- list(name = "varname_pred", title = "", type = "string")
        
        for( i in 1:length(rownames(res[["confusion.table"]]))){
            
            fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = as.character(rownames(res[["confusion.table"]])[i]), type = "integer", overTitle = title_observed)
            
        }
        
        data_confusion <- list()
        
        for( i in 1:length(rownames(res[["confusion.table"]]))){
            
            dat_tmp <- list("varname_pred" = as.character(rownames(res[["confusion.table"]]))[i])
            
            for(j in 1:length(rownames(res[["confusion.table"]]))){
                
                dat_tmp[[paste("varname_real",j,sep="")]] <- res[["confusion.table"]][i,j]
                
            }
            
            if(i == 1){
                dat_tmp[["pred_name"]] <- "Predicted"
            } else {
                dat_tmp[["pred_name"]] <- ""
            }
            
            data_confusion[[length(data_confusion)+1]] <- dat_tmp
            
        }
        
        return(list(title = "Confusion table",
                    schema = list(fields = fields_confusion),
                    data = data_confusion))
        
    } else if (target != "" & is.null(res)){
        
        fields_confusion <- list()
        
        fields_confusion[[length(fields_confusion)+1]] <- list(name = "pred_name", title = "", type = "string")
        
        fields_confusion[[length(fields_confusion)+1]] <- list(name = "varname_pred", title = "", type = "string")
        
        for( i in 1:length(unique(dataset[,target]))){
            
            fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i], type = "integer",overTitle = title_observed)
            
        }
        
        data_confusion <- list()
        
        for( i in 1:length(unique(dataset[,target]))){
            
            dat_tmp <- list("varname_pred" = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i])
            
            for(j in 1:length(unique(dataset[,target]))){
                
                dat_tmp[[paste("varname_real",j,sep="")]] <- "."
                
            }
            
            if(i == 1){
                dat_tmp[["pred_name"]] <- "Predicted"
            } else {
                dat_tmp[["pred_name"]] <- ""
            }
            
            data_confusion[[length(data_confusion)+1]] <- dat_tmp
            
        }
        
        return(list(title = "Confusion table",
                    schema = list(fields = fields_confusion),
                    data = data_confusion))
        
        
    } else {
        
        fields_confusion <- list(list(name = "pred_name", title = "", type = "string"),
                                 list(name = "varname_pred", title = "", type = "string"),
                                 list(name = "varname_real1", title = ".", type = "integer", overTitle = title_observed),
                                 list(name = "varname_real2", title = ".", type = 'integer', overTitle = title_observed))
        
        data_confusion <- list(list(pred_name = "Predicted", varname_pred = ".", varname_real1 = "", varname_real2= ""),
                               list(pred_name = "", varname_pred = ".", varname_real1= "", varname_real2= ""))
        
        return(list(title = "Confusion table",
                    schema = list(fields = fields_confusion),
                    data = data_confusion))
        
    } 
    
}

.optimizerKNN <- function(formula,dataset,kmax = 10,distance_from = 0.1,distance_to = 10){
    
    dist <- seq(distance_from,distance_to,by = 0.1)
    
    # make empty vector to save which distance parameters are used
    d <- numeric()
    # make empty vector to save which values are created
    value <- numeric()
    # same for k
    k<-numeric()
    # and kernel
    kernel <- numeric()
    
    for(i in 1:length(dist)){
        
        d <- dist[i]
        
        kfit <- kknn::train.kknn(formula,dataset,kmax,distance = d)
        if(kfit$response == "continuous"){
            value[i] <- min(sqrt(kfit$MEAN.SQU))
        } else {
            value[i] <- min(kfit$MISCLASS)
        }
        k[i] <- kfit$best.parameters$k
        kernel[i] <- kfit$best.parameters$kernel
        
    }
    
    if(kfit$response=="continuous"){
        lab = "RMSE"
    } else {
        lab = "Misclassification"
    }
    
    index <- which.min(value)
    optimal.k <- k[index]
    optimal.weights <- kernel[index]
    optimal.distance <- dist[index]
    statistic <- min(value)
    
    return(list("OptK" = optimal.k,
                "OptWeights" = optimal.weights,
                "OptDistance" = optimal.distance,
                "MinStatistic" = statistic,
                "value" = value,
                "dist" = dist,
                "lab" = lab,
                "k" = k,
                "kmax" = kmax,
                "plot_value" = value,
                "plot_lab" = lab,
                "plot_xlim" = range(dist),
                "plot_clim" = switch(EXPR = length(unique(k))<3, 
                                     clim = c(1,kmax),
                                     clim = range(k))))
    
}

.plotOptimization <- function(plot_data){
    
    plot3D::scatter2D(y=plot_data[["plot_value"]], 
              x = plot_data[["dist"]], 
              type = "l", 
              bty = "n",
              xlab = "Distance parameter", 
              ylab = plot_data[["plot_lab"]],
              las = 1, 
              lwd = 4,
              colvar = plot_data[["k"]],
              clim = plot_data[["plot_clim"]],
              clab = "No. nearest neighbors",
              main = "",
              xlim = plot_data[["plot_xlim"]],
              NAcol = "white")
    
}

.optimizationTable <- function(formula, dataset, options, res){
    
    optimizationTable <- list()
    
    optimizationTable[["title"]] <- "Model optimization"
    
    fields <- list(
        list(name = "model", title = "", type = "string"),
        list(name = "RMSE", title = "Accuracy", type = "number", format = "sf:4;dp:3"),
        list(name = "k", title = "No. nearest neighbors", type = "integer"),
        list(name = "weights", title = "Weights", type = "string"),
        list(name = "distance", title = "Distance parameter", type = "number", format = "dp:1")
    )
    
    optimizationTable[["schema"]] <- list(fields = fields)
    
    if(!is.null(res)){
    
    result <- .optimizerKNN(formula = formula,dataset = dataset,kmax = options[["optimizeModelMaxK"]],distance_from = 0.1,distance_to = options[["optimizeModelMaxD"]])
    
    data <- list(
        list(model = "Optimal parameters", RMSE = 1-result[["MinStatistic"]], k = result[["OptK"]], weights = result[["OptWeights"]], distance = result[["OptDistance"]])
    )
    
    } else {
        
        data <- list(
            list(model = "Optimal parameters", RMSE = ".", k = ".", weights = ".", distance = ".")
        ) 
        
        result <- NULL
        
    }
    
    optimizationTable[["data"]] <- data
    
    return(list(optimizationTable = optimizationTable, plot_data = result))
    
}

.predictKNNclassification <- function(dataset, options, predictors, formula, res, indicator, opt){
    
    index_1 <- which(dataset[,.v(indicator)] == 1)
    index_0 <- which(dataset[,.v(indicator)] == 0)
    
    knn.fit <- kknn::train.kknn(formula = formula,
                                data = dataset[index_0,],
                                ks = res[["Optimal.K"]],
                                distance = opt[['distance']],
                                kernel = opt[['weights']],
                                na.action = opt[['NA']],
                                scale = FALSE)
    
    predictions <- predict(knn.fit,newdata = dataset[index_1,])
    
    fields <- list()
    
    if(!is.null(res)){
        
        for(i in 1:length(predictors)){
            
            fields[[length(fields)+1]] <- list(name = paste("predictor",i,sep = ""),title = .unv(predictors[i]), type = 'number', format = 'dp:2')
            
        }
        
        fields[[length(fields)+1]] <- list(name = "predicted", title = paste("Predicted",options[['target']], sep=' '), type = "number", format = "dp:2")
        
    } else {
        fields[[1]] <- list(name = 'predictor1', title = "Predictor", type = "number")
        fields[[2]] <- list(name = "predicted", title = paste("Predicted",options[['target']], sep=' '), type = "number")
    }
    
    if(is.null(res)){
        
        data <- list(list("predictor1" = ".", "predicted" = "."))
        
    } else {
        
        data <- list()
        
        for(i in 1:length(predictions)){	
            
            data[[i]] <- list()
            
            for(k in 1:length(predictors)){
                
                data[[i]][[paste('predictor',k,sep = '')]] <- .clean(dataset[index_1[i],predictors[k]])
                
            }
            
            data[[i]]["predicted"] <- .clean(predictions[i])
            
        }
        
    }
    
    return(list(title = 'Predictions for new data',
                schema = list(fields = fields),
                data = data))
           
}