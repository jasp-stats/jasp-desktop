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

MLClassificationKNN <- function(dataset=NULL, options, perform="run", callback=function(...) 0, ...) {
    
    # Read variables
    predictors <- unlist(options['predictors'])
    if (length(options['target']) > 0){
        target <- unlist(options['target'])
    }
    variables.to.read <- c(predictors, target)
    variables.to.read <- variables.to.read[variables.to.read != ""]
    
    if (is.null(dataset)) {
        
        if (perform == "run") {
            
            dataset <- .readDataSetToEnd(columns = variables.to.read)
            
        } else {
            
            dataset <- .readDataSetHeader(dataset, columns.as.numeric=variables.to.read)
        }
        
    } else {
        
        dataset <- .vdf(columns.as.numeric=variables.to.read)
    }
    
    for(i in 1:length(variables.to.read)){
        
        errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                             all.target = variables.to.read[i],
                             observations.amount = "< 2",
                             exitAnalysisIfErrors = TRUE)
        
    }
    
    # Set a seed
    set.seed(1)
    
    # create the results bundle
    results <- list()
    
    # Provide the Meta to the results bundle
    meta <- list(list(name = 'KNN Classification', type = 'title'),
                 list(name = 'Descriptions', type = 'table'),
                 list(name = "Confusion", type = "table"),
                 list(name = 'Predictions', type = 'table'),
                 list(name = 'Weights', type = 'table'),
                 list(name = 'Distances', type = 'table'),
                 list(name = 'Plot', type = 'image'))
    results[['.meta']] <- meta
    
    results[['title']] <- 'K-Nearest Neighbors Classification'
    
    if(perform == "init"){
        
        results <- .initKnnClassification(options,results)
        
        return(list(results = results, status = "inited"))
        
    } else {
        
        # Set the analysis options from the options
        opt <- .setOptionsClassification(options,dataset)
        
        # code variable names into base64
        if(length(predictors[predictors!='']) > 0){
            predictors <- .v(predictors)
        }
        if(length(target[target!='']) > 0){
            target <- .v(target)
        }
        
        # Create formula for the model
        formula <- .makeformulaClassification(predictors,target)
        
        # Do the analysis
        if(length(predictors[predictors!='']) > 0 & length(target[target!='']) > 0){
            
            dataset <- na.omit(dataset)
            
            train.index <- sample(c(TRUE,FALSE),nrow(dataset),replace = TRUE,prob = c(opt[['ntrain']]*0.01,1-(opt[['ntrain']]*0.01)))
            train <- dataset[which(train.index == TRUE), ]
            test <- dataset[which(train.index == FALSE), ]
            
            res <- .DoKNNClassification(dataset,options,opt,train,test,train.index,formula,target)
            
        } else {
            
            res <- NULL
            
        }
        
        results[['Descriptions']] <- .DescriptionsTableClassification(predictors, target, opt, options, res, dataset, formula)
        
        if(options[['confusionTable']]){
            
            results[['Confusion']] <- .ConfusionTableClassifiaction(res, target, dataset)
            
        }
        
        # Create predictions table
        if(options[['tablePredictions']]){
            
            results[['Predictions']] <- .PredictionsTableClassification(options, opt, predictors, target, res)
            
        }
        
        # Create distances table
        if(options[['tableDistances']]){
            
            results[['Distances']] <- .DistancesTableClassification(predictors,target, opt, options, res)
            
        }
        
        # Create weights table
        if(options[['tableWeights']]){
            
            results[['Weights']] <- .WeightsTableClassification(predictors, target, opt, options, res)
            
        }
        
        # Create the Error vs K plot
        if(options[['plotErrorVsK']] & !is.null(res)){
            
            if(options[['noOfNearestNeighbours']] == 'optimized' | options[['validationLeaveOneOut']]){
                
                results[['Plot']] <- .PlotErrorVsKClassification(res,opt,options,dataset,formula)
                
            }
            
        }
        
        return(list(results = results, status = "complete"))
        
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
    if(!"kknn" %in% installed.packages()){
    install.packages('kknn', repos="http://cran.rstudio.com/")
    }
    library(kknn)
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
    if(!"kknn" %in% installed.packages()){
        install.packages('kknn', repos="http://cran.rstudio.com/")
    }
    library(kknn)
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
    plot(opt[['NN']],
         res[['Model.error']],
         type = 'b',
         xlab = '',
         ylab = '',
         las = 1,
         main = '',
         bty = 'n')
    mtext(expression('Model error'), side = 2, line = 2, cex = 1.5, font = 2)
    mtext("K", side = 1, line = 3, cex = 1.5, font = 2)
    points(opt[['NN']][which.min(res[["Model.error"]])],
           min(res[['Model.error']]),
           pch = 19,
           col = 'red')
    
}

.DescriptionsTableClassification <- function(predictors, target, opt, options, res, dataset, formula){
    
    ### descriptions table
    ###
    
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
    
    data_descriptions <- list()
    
    if(!is.null(res)){
        
        if(options[['noOfNearestNeighbours']] == 'auto'){
            
            data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = opt[['NN']], "r[rmse]" = res[['model.error']], "optim[type1]" = 'Auto')
            
        } else if (options[['noOfNearestNeighbours']] == 'manual'){
            
            data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = opt[['NN']], "r[rmse]" = res[['model.error']], "optim[type1]" = 'Manual')
            
        } else if (options[['noOfNearestNeighbours']] == 'optimized'){
            
            data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = res[['Optimal.K']], "r[rmse]" = res[['Minimal.error']], "optim[type1]" = "Optimized")
            
        }
        
    } else {
        
        data_descriptions[[1]] <- list(model = 'k-NN model', "nnc[nn]" = ".", "r[rmse]" = ".", "optim[type1]" = "")
        
    }
    
    if(options[['validationLeaveOneOut']] & !is.null(res)){
        result <- .LOOCVClassification(dataset,options,opt,formula)
        data_descriptions[[1]][["optim[type2]"]] <- "Leave-one-out"
        data_descriptions[[1]][["nnc[nnloo]"]] <- result[['Optimal.K']]
        data_descriptions[[1]][["r[rmseoptim]"]] <- sqrt(result[['minimal.error']])
    }
    
    if(options[['validationKFold']] & !is.null(res)){
        result_fold <- .KfoldClassification(dataset,options,opt,formula,res)
        data_descriptions[[1]][["optim[type3]"]] <- "K-fold"
        data_descriptions[[1]][["nnc[nnkfold]"]] <- result_fold[['Optimal.K']]
        data_descriptions[[1]][['r[rmsekfold]']] <- result_fold[['minimal.error']]
    }
    
    return(list(title = 'Evaluation',
                schema = list(fields = fields_descriptions),
                data = data_descriptions))
    
}

.PredictionsTableClassification <- function(options, opt, predictors, target, res){
    
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
            
            for(i in 1:nrow(res[['predictions']])){
                
                data[[length(data)+1]] <- list(number = as.numeric(res[['predictions']][i,1]),
                                               real = as.character(res[['predictions']][i,2]),
                                               predicted = as.character(res[['predictions']][i,3]),
                                               confidence = as.numeric(max(res[["confidence"]][i,])))
                
            }
            
        } else {
            
            for(i in 1:nrow(res[['predictions']])){
                
                data[[length(data)+1]] <- list(number = as.numeric(res[['predictions']][i,1]),
                                               real = as.character(res[['predictions']][i,2]),
                                               predicted = as.character(res[['predictions']][i,3]))
                
            }
            
        }
        
    }
    
    return(list(title = 'Predictions',
                schema = list(fields = fields),
                data = data))
    
}

.DistancesTableClassification <- function(predictors,target, opt, options, res){
    
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
        
        for(i in 1:nrow(res[['Distances']])){	
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

.WeightsTableClassification <- function(predictors, target, opt, options, res){
    
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
        
        for(i in 1:nrow(res[['Weights']])){	
            
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

.PlotErrorVsKClassification <- function(res,opt,options,dataset,formula){
    
    if(options[['noOfNearestNeighbours']] == 'optimized'){
        
        image <- .beginSaveImage(640,480)
        .PlotKoptimizedClassification(res,opt)
        content <- .endSaveImage(image)
        
    } else if (options[['validationLeaveOneOut']]){
        
        image <- .beginSaveImage(640,480)
        .PlotLOOCVClassification(.LOOCVClassification(dataset,options,opt,formula))
        content <- .endSaveImage(image)
        
    }
    
    return(list(title = 'Error vs K',
                width = 640,
                height = 480,
                data = content))
    
}

.LOOCVClassification <- function(dataset,options,opt,formula){
    if(!"kknn" %in% installed.packages()){
        install.packages('kknn', repos="http://cran.rstudio.com/")
    }
    library(kknn)
    knn.fit <- kknn::train.kknn(formula = formula,
                                data = dataset,
                                kmax = options[['maxK']],
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

.PlotLOOCVClassification <- function(res){
    plot(seq_along(res[['error']]), 
         sqrt(res[['error']]), 
         xlab="", 
         main = '',
         type = 'b',
         las = 1,
         ylab = '',
         bty = 'n',
         pch = 19)
    mtext(expression('Model error'), side = 2, line = 2, cex = 1.5, font = 2)
    mtext("K", side = 1, line = 3, cex = 1.5, font = 2)
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

.initKnnClassification <- function(options,results){

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
    
    data_descriptions <- list(list(model = 'k-NN model', nn = ".", rmse = "."))
    
    results[['Descriptions']] <- list(title = 'Evaluation',
                                      schema = list(fields = fields_descriptions),
                                      data = data_descriptions)
    
    if(options[['confusionTable']]){
        
        fields_confusion <- list(list(name = "varname_pred", title = "", type = "string"))
        
        for( i in 1:2){
            
            fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = ".", type = "integer")
            
        }
        
        data_confusion <- list()
        
        for( i in 1:2){
            
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
    
    if(options[["tablePredictions"]]){
        
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
    
    if(options[["tableDistances"]]){
        
        fields_distances <- list(list(name="number", title="Obs. number", type="integer"),
                                 list(name = 'distance', title = "Distance", type = 'integer'))
        
        data_distances <- list(list(number = ".", distance = "."))
        
        results[['Distances']] <- list(title = 'Distances',
                                       schema = list(fields = fields_distances),
                                       data = data_distances)
        
    }
    
    if(options[["tableWeights"]]){
        
        fields_weights <- list(list(name="number", title="Obs. number", type="integer"),
                               list(name = 'weight', title = "Weight", type = 'integer'))
        
        data_weights <- list(list(number = ".", weight = "."))
        
        results[['Weights']] <- list(title = 'Weights',
                                     schema = list(fields = fields_weights),
                                     data = data_weights) 
        
    }
    
    
    return(results)
    
}

.ConfusionTableClassifiaction <- function(res, target, dataset){
    
    if(!is.null(res)){
    
    fields_confusion <- list(list(name = "varname_pred", title = "", type = "string"))
    
    for( i in 1:length(rownames(res[["confusion.table"]]))){
        
        fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = as.character(rownames(res[["confusion.table"]])[i]), type = "integer")
        
    }
    
    data_confusion <- list()
    
    for( i in 1:length(rownames(res[["confusion.table"]]))){
        
        dat_tmp <- list("varname_pred" = as.character(rownames(res[["confusion.table"]]))[i])
        
        for(j in 1:length(rownames(res[["confusion.table"]]))){
            
            dat_tmp[[paste("varname_real",j,sep="")]] <- res[["confusion.table"]][i,j]
            
        }
        
        data_confusion[[length(data_confusion)+1]] <- dat_tmp
        
    }
        
    return(list(title = "Confusion table",
                schema = list(fields = fields_confusion),
                data = data_confusion))
    
    } else if (target != "" & is.null(res)){
        
        fields_confusion <- list(list(name = "varname_pred", title = "", type = "string"))
        
        for( i in 1:length(unique(dataset[,target]))){
            
            fields_confusion[[length(fields_confusion)+1]] <- list(name = paste("varname_real",i, sep = ""), title = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i], type = "integer")
            
        }
        
        data_confusion <- list()
        
        for( i in 1:length(unique(dataset[,target]))){
            
            dat_tmp <- list("varname_pred" = as.character(sort(unique(dataset[,target]), decreasing = FALSE))[i])
            
            for(j in 1:length(unique(dataset[,target]))){
                
                dat_tmp[[paste("varname_real",j,sep="")]] <- "."
                
            }
            
            data_confusion[[length(data_confusion)+1]] <- dat_tmp
            
        }
        
        return(list(title = "Confusion table",
                    schema = list(fields = fields_confusion),
                    data = data_confusion))
        
        
    } else {
        
        fields_confusion <- list(list(name = "varname_pred", title = "", type = "string"),
                                 list(name = "varname_real1", title = ".", type = "integer"),
                                 list(name = "varname_real2", title = ".", type = 'integer'))
        
        data_confusion <- list(list(varname_pred = ".", varname_real1 = "", varname_real2= ""),
                               list(varname_pred = ".", varname_real1= "", varname_real2= ""))
        
        return(list(title = "Confusion table",
                    schema = list(fields = fields_confusion),
                    data = data_confusion))
        
    } 
    
}
