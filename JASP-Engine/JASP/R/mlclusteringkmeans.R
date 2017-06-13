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

MLClusteringKMeans <- function(dataset = NULL, options, state = NULL, perform = "run", callback = function(...) list(status = "ok"), ...) {
    
    # state creation ##
    
    state[["options"]] <- options
    
    stateKey <- list()
    stateKey[["evaluation"]] <- c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo", "criterion")
    stateKey[["clusterinfo"]] <- c("tableClusterInformation","predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo", "tableClusterInfoSize", "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares")
    stateKey[["predictions"]] <- c("tablePredictions", "predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo", "predictionsFrom", "predictionsTo")
    stateKey[['2dplot']] <- c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm")
    stateKey[['criterionvsclusters']] <- c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "robustFrom", "robustTo", "criterion")
    stateKey[["pcaplot"]] <- c("tablePredictions", "predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "clusterSize", "optimizedFrom", "optimizedTo", "robustFrom", "robustTo")
    stateKey[["withinssvsclusters"]] <- c("predictors", "target", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "optimizedFrom", "optimizedTo")
    attr(state, "key") <- stateKey
    
    # read variables ##
    
    predictors <- unlist(options['predictors'])
    predictors <- predictors[predictors != ""]
    
    # read dataset ##
    
    if (is.null(dataset)) {
        
        if (perform == "run") {
            
            dataset <- .readDataSetToEnd(columns.as.numeric=predictors)
            
        } else {
            
            dataset <- .readDataSetHeader(dataset, columns.as.numeric=predictors)
        }
        
    } else {
        
        dataset <- .vdf(columns.as.numeric=predictors)
    }
    
    # error handling & code variable names in base64 ##
    
    if(length(predictors[predictors!='']) > 0){
        
        for(i in 1:length(predictors)){
            
            errors <- .hasErrors(dataset, perform, type = c('infinity', 'observations'),
                                 all.target = predictors[i],
                                 observations.amount = "< 2",
                                 exitAnalysisIfErrors = TRUE)
            
        }
        
        predictors <- .v(predictors)
        
    }
    
    # set the seed so that every time the same set is chosen (to prevent random results) ##
    
    set.seed(1)
    
    # create results bundle ##
    
    results <- list()
    results[['title']] <- 'k-means clustering'
    
    # Provide the Meta to the results bundle ## 
    
    meta <- list(list(name = 'k-means clustering', type = 'title'),
                 list(name = 'evaluation', type = 'table'),
                 list(name = "clusterinfo", type = 'table'),
                 list(name = 'predictions', type = 'table'),
                 list(name = '2dplot', type = 'image'),
                 list(name = 'criterionvsclusters', type = 'image'),
                 list(name = 'pcaplot', type = 'image'),
                 list(name = 'withinssvsclusters', type = 'image'))
    results[['.meta']] <- meta
    
    # init state ##
    
    if(perform == 'init'){
        
        results <- .initKmeans(options = options, results = results, state = state)
        
        return(list(results = results, status = "inited", state = state))
        
    } 
    
    # run state ##
    
    else {
        
        # Set the right options for the analysis ##
        
        opt <- .OptionsSet(dataset = dataset,options = options)
        
        # Run the analysis and save the results ##
        
        res <- .DoKmeansAnalysis(dataset = dataset,options = options,opt = opt,predictors = predictors)
        
        # create the summary table ##
        
        results[['evaluation']] <- .EvaluationTableKmeans(res = res,options = options)
        state[["evaluation"]] <- results[["evaluation"]]
        
        # create the cluster information table ##
        
        if (options[['tableClusterInformation']]){
            
            results[["clusterinfo"]] <- .clusterInfoTable(options = options, res = res, predictors = predictors)
            state[["clusterinfo"]] <- results[["clusterinfo"]]
            
        }
        
        # Create the predictions table ##
        
        if(options[['tablePredictions']]){
            results[['predictions']] <- .PredictionsTableKmeans(res = res,options = options)
            state[["predictions"]] <- results[["predictions"]]
        }
        
        # Create the PCA cluster plot ##
        
        if(options[['plotPCACluster']]){
            results[['pcaplot']] <- .PCAplot(options = options,res = res,dataset = dataset,opt = opt,predictors = predictors)
            state[["pcaplot"]] <- results[["pcaplot"]]
        }
        
        # Create the 2-d clusterplot ##
        
        if(options[['plot2dCluster']]){
            results[['2dplot']] <- .plot2d(dataset = dataset,res = res,opt = opt,options = options,predictors = predictors)
            state[["2dplot"]] <- results[['2dplot']]
        }
        
        # Create the within ss vs cluster plot ##
        
        if(options[['plotPCAClusterSquares']] && options[["noOfClusters"]] == "optimized"){
            results[['withinssvsclusters']] <- .WSSplot(options = options,res = res)
            state[["withinssvsclusters"]] <- results[['withinssvsclusters']]
        }
        
        # Create the criterion vs cluster plot ##
        
        if(options[['plotCriterionVsClusters']] && options[["noOfClusters"]] == "robust"){
            results[['criterionvsclusters']] <- .criterionplot(options = options,res = res)
            state[["criterionvsclusters"]] <- results[['criterionvsclusters']]
        }
        
        # return the results bundle and state ##
        
        return(list(results = results, status = "complete", state = state))
        
    } 
    
}

.OptionsSet <- function(dataset,options){
    opt <- list()
    # set iterations
    if(options[['noOfIterations']] == 'auto'){
        opt[['iter']] <- 15
    } else if (options[['noOfIterations']] == 'manual'){
        opt[['iter']] <- options[['iterationsCount']]
    }
    # set random sets
    if(options[['noOfRandomSets']] == 'auto'){
        opt[['sets']] <- 20
    } else if (options[['noOfRandomSets']] == 'manual'){
        opt[['sets']] <- options[['randomSetCount']]
    }
    # set algorithm
    if(options[['algorithm']] == 'hartiganWong'){
        opt[['algorithm']] <- 'Hartigan-Wong'
    } else if (options[['algorithm']] == 'lloyd'){
        opt[['algorithm']] <- 'Lloyd'
    } else if (options[['algorithm']] == 'macQueen'){
        opt[['algorithm']] <- 'MacQueen'
    }
    # set clusters
    if(options[['noOfClusters']] == 'auto'){ 
        
        ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) <= 1000,
               yes = opt[['clusters']] <- 2,
               no = ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) <= 20000,
                           yes = opt[['clusters']] <- 2*round(((nrow(dataset)*0.001)+1)/2), 
                           no = ifelse(test = options[['noOfClusters']] == 'auto' & nrow(dataset) > 21000, 
                                       yes = opt[['clusters']] <- 21,
                                       no = opt[['clusters']] <- 0)))
    }
    if(options[['noOfClusters']] == 'manual'){
        opt[['clusters']] <- options[['clusterSize']]
    }
    if(options[['noOfClusters']] == 'optimized'){
        opt[['clusters']] <- options[['optimizedFrom']]:options[['optimizedTo']]
    }
    if(options[['noOfClusters']] == 'robust'){
        opt[['clusters']] <- options[['robustFrom']]:options[['robustTo']]
        if (nrow(dataset)>2000){
            opt[['Pam']] <- FALSE
        } else {
            opt[['Pam']] <- TRUE
        }
        if(options[['criterion']] == 'silhoutteLength'){
            opt[['criterion']] <- 'asw'
        } else if (options[['criterion']] == 'Multiasw'){
            opt[['criterion']] <- 'multiasw'
        } else if (options[['criterion']] == 'Calinski-Harabasz'){
            opt[['criterion']] <- 'ch'
        }
    }
    return(opt)
}

.DoKmeansAnalysis <- function(dataset,options,opt,predictors){
    
    if(length(predictors) > 0){
        if(options[['noOfClusters']] == 'auto' | options[['noOfClusters']] == 'manual'){
            
            res <- .Kmeansclusterspecified(dataset, options, opt, predictors)
            
        } else if (options[["noOfClusters"]] == 'robust'){
            
            res <- .robustclustering(dataset, options, opt, predictors)
            
        } else if (options[['noOfClusters']] == 'optimized'){
            
            res <- .Kmeansoptimized(dataset, options, opt, predictors)
        }
        
    }
    
    else {
        
        res <- NULL
        
    }
    
    return(res)
    
}

.Kmeansclusterspecified <- function(dataset,options,opt,predictors){
    kfit <- kmeans(dataset[,predictors],
                   centers = opt[['clusters']],
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res <- list(
        'Predictions' = NULL,
        'clusters' = NULL,
        'centroids' = NULL,
        'size' = NULL,
        'WSS' = NULL,
        'BSS' = NULL,
        'TSS' = NULL
    )
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['clusters']] <- opt[['clusters']]
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    m = ncol(kfit$centers)
    n = length(kfit$cluster)
    k = nrow(kfit$centers)
    D = kfit$tot.withinss
    res[['AIC']] <- D + 2*m*k
    res[['BIC']] <- D + log(n)*m*k
    return(res)
}

.robustclustering <- function(dataset,options,opt,predictors){
    # install.packages('fpc', repos="http://cran.rstudio.com/")
    # library(fpc)
    fit <- fpc::pamk(dataset[,predictors],
                     krange = opt[['clusters']],
                     criterion = opt[['criterion']],
                     usepam = opt[['Pam']],
                     scaling = FALSE,
                     alpha = 0.001,
                     critout = FALSE,
                     ns = 10)
    res <- list()
    res[['clusters']] <- fit$nc
    res[['criterion.krange']] <- fit$crit[which(fit$crit!=0)]
    res[['clusterrange']] <- opt[['clusters']]
    for(i in 1:length(opt[["clusters"]])){
        kfit_tmp <- kmeans(dataset[,predictors],
                           centers = opt[['clusters']][[i]],
                           iter.max = opt[['iter']],
                           nstart = opt[['sets']],
                           algorithm = opt[['algorithm']])
        res[['TSS_tmp']][i] <- kfit_tmp$totss
        res[['BSS_tmp']][i] <- kfit_tmp$betweenss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC']][i] <- D + 2*m*k
        res[['BIC']][i] <- D + log(n)*m*k
    }
    kfit <- kmeans(dataset[,predictors],
                   centers = fit$nc,
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    dAIC <- res[["AIC"]]-min(res[['AIC']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC']] - min(res[['BIC']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    return(res)
}

.Kmeansoptimized <- function(dataset,options,opt, predictors){
    WSS <- numeric(options[['optimizedTo']] - options[['optimizedFrom']])
    clusters <- opt[['clusters']]
    res<-list()
    res[['clusterrange']] <- clusters
    for(i in seq_along(clusters)){
        index <- clusters[i]
        kfit_tmp <- kmeans(dataset[,predictors],
                           centers = index,
                           iter.max = opt[['iter']],
                           nstart = opt[['sets']],
                           algorithm = opt[['algorithm']])
        res[['WithinSumSquares']][i] <- kfit_tmp$tot.withinss
        m = ncol(kfit_tmp$centers)
        n = length(kfit_tmp$cluster)
        k = nrow(kfit_tmp$centers)
        D = kfit_tmp$tot.withinss
        res[['AIC']][i] <- D + 2*m*k
        res[['BIC']][i] <- D + log(n)*m*k
        res[['TSS_tmp']][i] <- kfit_tmp$totss
        res[['BSS_tmp']][i] <- kfit_tmp$betweenss
        
    }
    res[['clusters']] <- .determineoptimum(res, by = "AIC")
    # predictions for best model.
    kfit <- kmeans(dataset[,predictors],
                   centers = res[['clusters']],
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    res[['Predictions']] <- data.frame(
        'Observation' = 1:nrow(dataset),
        'Cluster' = kfit$cluster
    )
    res[['size']] <- kfit$size
    res[['centroids']] <- kfit$centers
    res[['WSS']] <- kfit$withinss
    res[['TSS']] <- kfit$totss
    res[['BSS']] <- kfit$betweenss
    dAIC <- res[["AIC"]]-min(res[['AIC']])
    res[['AICweights']] <- exp((-.5)*dAIC)/sum(exp((-.5)*dAIC))
    dBIC <- res[['BIC']] - min(res[['BIC']])
    res[["BICweights"]] <- exp((-.5)*dBIC)/sum(exp((-.5)*dBIC))
    return(res)
}

.determineoptimum <- function(res,by = "AIC"){
    if(by == "AIC"){
        optimum <- res[["clusterrange"]][which.min(res[['AIC']])]
    } else if (by == "BIC"){
        optimum <- res[["clusterrange"]][which.min(res[['BIC']])]
    }
    return(optimum)
}

.EvaluationTableKmeans <- function(res,options){
    
    if(options[["noOfClusters"]] == "auto" | options[["noOfClusters"]] == 'manual'){
        
        fields_evaluation <- list(list(name = 'title', title = "", type = 'string'),
                                  list(name = 'clusters', title = 'No. clusters', type = 'integer'),
                                  list(name = 'measure', title = 'R-squared', type = 'number', format = 'dp:2'),
                                  list(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1'),
                                  list(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1'))
        
    } else {
        
        fields_evaluation <- list(list(name = 'title', title = "", type = 'string'),
                                  list(name = 'clusters', title = 'No. clusters', type = 'integer'),
                                  list(name = 'measure', title = 'R-squared', type = 'number', format = 'dp:2'),
                                  list(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1'),
                                  list(name = "aicweights", title = "AIC Weights", type = "number", format = "dp:2"),
                                  list(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1'),
                                  list(name = "bicweights", title = "BIC Weights", type = "number", format = "dp:2"))
        
    }
    
    if(is.null(res)){
        
        data_evaluation <- list(list(title = 'k-means model', clusters = ".", measure = ".", aic = ".", bic = "."))
        footnotes_N <- .newFootnotes()
        .addFootnote(footnotes_N,paste("The model has not been applied to any data yet"), symbol = "")
        footnotes_N <- as.list(footnotes_N)
        
    } else {
        
        if(options[['noOfClusters']] == "auto" | options[['noOfClusters']] == "manual"){
            
            data_evaluation <- list(list(title = 'k-means model', clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']], bic = res[['BIC']]))
            
            
        } else if (options[["noOfClusters"]]=="optimized" | options[['noOfClusters']] == "robust"){
            
            data_evaluation <- list()
            index_best_model <- which(res[['clusterrange']] == res[['clusters']])
            for(i in 1:length(res[['clusterrange']])){
                
                if(i == index_best_model){
                    
                    data_evaluation[[1]] <- list(title = "Best model", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                    
                } else if (i > index_best_model){
                    
                    data_evaluation[[i]] <- list(title = "", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                    
                } else if (i < index_best_model){
                    
                    data_evaluation[[i+1]] <- list(title = "", clusters = res[['clusterrange']][[i]], measure = res[["BSS_tmp"]][[i]]/res[["TSS_tmp"]][[i]], aic = res[["AIC"]][[i]], aicweights = res[["AICweights"]][[i]], bic = res[["BIC"]][[i]], bicweights = res[['BICweights']][[i]])
                    
                }
            }
            
        }
        
        footnotes_N <- .newFootnotes()
        .addFootnote(footnotes_N,paste('The model is tested on ',nrow(res[["Predictions"]]), "observations"), symbol = "")
        footnotes_N <- as.list(footnotes_N)
        
    }
    
    citation <- c("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.","Wagenmakers, E. J., & Farrell, S. (2004). AIC model selection using Akaike weights. Psychonomic bulletin & review, 11(1), 192-196.")
    
    return(list(title = 'Summary',
                schema = list(fields = fields_evaluation),
                data = data_evaluation,
                footnotes = footnotes_N,
                citation = citation))
    
}

.PredictionsTableKmeans <- function(res, options){
    
    from <- ifelse(test = options[["predictionsFrom"]] > nrow(res[["Predictions"]]),yes = 1,no = options[["predictionsFrom"]])
    
    to <- ifelse(test = options[['predictionsTo']] > nrow(res[["Predictions"]]),yes = nrow(res[["Predictions"]]), no = options[["predictionsTo"]])
    
    fields_predictions <- list(list(name = 'number', title = "Obs. number", type = 'integer'),
                               list(name = 'prediction', title = 'Prediction', type = 'integer'))
    
    data_predictions <- list()
    
    if(is.null(res)){
        
        data_predictions <- list(list(number = ".", prediction = ".")) 
        
    } else {
        
        for(i in from:to){
            data_predictions[[i]] <- list(number = i, prediction = res[['Predictions']][[i,2]])
        }
        
    }
    
    data_predictions <- data_predictions[!sapply(data_predictions,is.null)]
    
    if (!is.null(res) && (options[["predictionsFrom"]] > options[["predictionsTo"]])){
        
        error <- list(errorType = "badData",
                      errorMessage = "Please specify a valid range of observation values")
        
        return(list(title = 'Predictions',
                    schema = list(fields = fields_predictions),
                    data = data_predictions,
                    error = error))
    } else {
        
    return(list(title = 'Predictions',
                schema = list(fields = fields_predictions),
                data = data_predictions))
        
    }
    
}

.PCAplot <- function(options,res,dataset,opt,predictors){
    
    if(options[['plotPCACluster']] & !is.null(res)){
        image <- .beginSaveImage(640,480)
        .PCAclusplot(dataset,res,options,opt,predictors)
        content <- .endSaveImage(image)
    }
    
    return(list(title = 'PCA cluster plot',
                width = 640,
                height = 480,
                data = content))
    
}

.PCAclusplot <- function(dataset,res,options,opt,predictors){
    # install.packages('cluster',repos="http://cran.rstudio.com/")
    # library(cluster)
    if(nrow(dataset) <= ncol(dataset)){
        stop('Should have more observations than variables')
    }
    
    if(options[['noOfClusters']] == 'auto' | options[['noOfClusters']] == 'manual'){
        clusters <- res[['clusters']]
    }  else if (options[['noOfClusters']] == 'optimized' | options[['noOfClusters']] == 'robust'){
        clusters <- res[['clusters']]
    }
    kfit <- kmeans(dataset[,predictors],
                   centers = clusters,
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    cluster::clusplot(dataset, 
                      kfit$cluster, 
                      color=TRUE, 
                      shade=TRUE, 
                      labels=5, 
                      lines=0,
                      main = '',
                      bty='n',
                      las=1)
}

.plot2d <- function(dataset,res,opt,options,predictors){
    
    if(options[['plot2dCluster']] && length(predictors) == 2){
        im <- .beginSaveImage(640,480)
        .TwodClusterplot(dataset,res,opt,options,predictors)
        cont <- .endSaveImage(im)
    
    return(list(title = '2-D cluster plot',
                width = 640,
                height = 480,
                data = cont))
        
    } else {
        
        return(list(title = "2-D cluster plot",
                    width = 640,
                    height = 480,
                    data = "",
                    error = list(error="badData", errorMessage="2 variables must be specified")))
        
    }
    
}

.TwodClusterplot <- function(dataset,res,opt,options,predictors){
    # install.packages('vegan',repos="http://cran.rstudio.com/")
    # library(vegan)
    if(length(predictors)!= 2){
        return("")
    }
    if(options[['noOfClusters']] == 'auto' | options[['noOfClusters']] == 'manual'){
        clusters <- res[['clusters']]
    }  else if (options[['noOfClusters']] == 'optimized' | options[['noOfClusters']] == 'robust'){
        clusters <- res[['clusters']]
    }
    kfit <- kmeans(dataset[,predictors],
                   centers = clusters,
                   iter.max = opt[['iter']],
                   nstart = opt[['sets']],
                   algorithm = opt[['algorithm']])
    dat <- data.frame(
        'X' = dataset[,predictors[1]],
        'Y' = dataset[,predictors[2]]
    )
    groups <- levels(factor(kfit$cluster))
    plot(dat, 
         type = "n",
         las = 1,
         xlab = .unv(predictors[1]),
         ylab = .unv(predictors[2]),
         bty='n')
    cols <- rainbow(length(groups))
    for(i in seq_along(groups)){
        points(dat[factor(kfit$cluster) == groups[i], 1],
               dat[factor(kfit$cluster) == groups[i], 2],
               col = cols[i], pch = 19)
    }
    if(options[['plot2dClusterCentroids']]){
        vegan::ordispider(dat, factor(kfit$cluster), label = FALSE,lwd = 0.1,col= 'black') # grey48 # ,lty = 'dotted'
    }
}

.WSSplot <- function(options,res){
    
    if(options[['plotPCAClusterSquares']] & !is.null(res)){

        g <- .PlotOptimizedkClustering(res)

        imgObj <- .writeImage(width = options$plotWidth, 
                              height = options$plotHeight, 
                              plot = g)
        
    }
    
    plot <- list()
    
    plot[["title"]] <- "Within sum of squares vs. Cluster plot"
    plot[["data"]] <- imgObj[["png"]]
    plot[["obj"]] <- imgObj[["obj"]]
    plot[["convertible"]] <- TRUE
    plot[["status"]] <- "complete"
    
    return(plot)
}

.PlotOptimizedkClustering <- function(res){
    
    library(JASPgraphs) # remove later
    
    xName = "No. Clusters"
    yName = "Total within sum of squares"
    x <- res[['clusterrange']]
    y <- res[['WithinSumSquares']]
    toPlot = data.frame(x = x, y = y)
    
    g <- drawCanvas(xName = xName, yName = yName, dat = toPlot, xBreaks = seq(min(x), max(x), 1), xLabels = seq(min(x), max(x), 1))
    g <- drawLines(g, dat = toPlot, alpha = .25)
    g <- drawPoints(g, dat = toPlot, size = 5, alpha = .65)
    g <- drawPoints(g, dat = data.frame(x = res[['clusters']], y = res[['WithinSumSquares']][which(res[['clusterrange']]==res[['clusters']])]),fill = "red", size = 5)
    g <- themeJasp(g)
    
    return(g)
}

.criterionplot<-function(options,res){
    
    if(options[['plotCriterionVsClusters']] & !is.null(res) & options[["robustFrom"]] != 1){

        g <- .PlotRobustOptimize(res, options)
        
        imgObj <- .writeImage(width = options$plotWidth, 
                              height = options$plotHeight, 
                              plot = g)
        
    }
    
    plot <- list()
    
    plot[["title"]] <- "Criterion vs. Cluster plot"
    plot[["convertible"]] <- TRUE
    
    if(options[["robustFrom"]] == 1){
        
    plot[["error"]] <- list(error="badData", errorMessage="The minimum amount of clusters to plot is 2") 
    plot[["data"]] <- ""
    plot[["obj"]] <- ""
        
    } else {
        
    plot[["data"]] <- imgObj[["png"]]
    plot[["obj"]] <- imgObj[["obj"]]
        
    }
    
    plot[["status"]] <- "complete"
    
    return(plot)
    
}

.PlotRobustOptimize <- function(res, options){
    
    library(JASPgraphs) # remove later
    
    if(options[["robustFrom"]] != 1){
        
        xName = "No. Clusters"
        yName = "Criterion"
        x <- res[['clusterrange']]
        y <- res[['criterion.krange']]
        toPlot = data.frame(x = x, y = y)
        
        g <- drawCanvas(xName = xName, yName = yName, dat = toPlot, xBreaks = seq(min(x), max(x), 1), xLabels = seq(min(x), max(x), 1))
        g <- drawLines(g, dat = toPlot, alpha = .25)
        g <- drawPoints(g, dat = toPlot, size = 5, alpha = .65)
        g <- drawPoints(g, dat = data.frame(x = res[['clusterrange']][which.max(res[['criterion.krange']])], y = max(res[['criterion.krange']])),fill = "red", size = 5)
        g <- themeJasp(g)
        
        return(g)
        
    }
    
}

.clusterInfoTable <- function(options, res, predictors){
    
    fields_clusterinfo <- list(list(name = 'cluster', title = 'Cluster', type = 'integer'))
    if (options[['tableClusterInfoSize']]){
        
        fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'size', title = 'Size', type = 'integer')
        
    }
    if (options[['tableClusterInfoSumSquares']]){
        
        fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'withinss', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')
        
    }
    
    if(options[['tableClusterInfoCentroids']] & length(predictors) > 0){
        
        for( i in 1:length(predictors)){
            
            fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = paste('centroid',i,sep = ''), title = paste('Centroid',.unv(predictors[i])), type = 'number', format = 'dp:3')	
            
        } 
        
    } else if(options[['tableClusterInfoCentroids']] & length(predictors) == 0){
        
        fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = "centroid", title = "Centroid", type = 'number', format = 'dp:3')
        
    }
    
    footnotes_BSS <- .newFootnotes()
    
    if(!is.null(res)){
        
        if(options[['tableClusterInfoBetweenSumSquares']]){
            
            .addFootnote(footnotes_BSS,paste('The Between Sum of Squares of this model is', round(res[['BSS']],2)))
            
        }
        
        if(options[['tableClusterInfoTotalSumSquares']]){
            
            .addFootnote(footnotes_BSS,paste('The Total Sum of Squares of this model is', round(res[['TSS']],2)))
            
        }
        
    } else if (is.null(res)){
        
        if(options[['tableClusterInfoBetweenSumSquares']]){
            
            .addFootnote(footnotes_BSS,'The Between Sum of Squares of this model is not yet computed')

        }
        
        if(options[['tableClusterInfoTotalSumSquares']]){
            
            .addFootnote(footnotes_BSS,'The Total Sum of Squares of this model is not yet computed')

        }
        
    }
    
    footnotes_BSS <- as.list(footnotes_BSS)
    
    if(is.null(res)){
        
        data_clusterinfo <- list(list(cluster = ".", size = ".", withinss = ".", centroid = "."))
        
    } else {
        
        data_clusterinfo <- list()
        
        for(i in 1:res[['clusters']]){
            data_clusterinfo[[i]] <- list(cluster = i, size = res[['size']][i], withinss = res[['WSS']][[i]])
            for( j in 1:length(predictors)){
                data_clusterinfo[[i]][[paste('centroid',j,sep='')]] <- res[['centroids']][[i,j]]
            }
        }
        
    }
    
    results <- list(title = 'Cluster information',
                    schema = list(fields = fields_clusterinfo),
                    data = data_clusterinfo,
                    footnotes = footnotes_BSS)
    
    return(results)
}

.initKmeans <- function(options, results, state){
    
    # init evaluation table ##
    
    if(!is.null(state[["evaluation"]])){
        
        results[["evaluation"]] <- state[["evaluation"]]
        
    } else {
        
        if(options[["noOfClusters"]] == "auto" | options[["noOfClusters"]] == 'manual'){
            
            fields_evaluation <- list(list(name = 'title', title = "", type = 'string'),
                                      list(name = 'clusters', title = 'No. clusters', type = 'string'),
                                      list(name = 'measure', title = 'R-squared', type = 'string'),
                                      list(name = 'aic', title = 'AIC', type = 'string'),
                                      list(name = 'bic', title = 'BIC', type = 'string'))
            
        } else {
            
            fields_evaluation <- list(list(name = 'title', title = "", type = 'string'),
                                      list(name = 'clusters', title = 'No. clusters', type = 'integer'),
                                      list(name = 'measure', title = 'R-squared', type = 'number', format = 'dp:2'),
                                      list(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1'),
                                      list(name = "aicweights", title = "AIC Weights", type = "number", format = "dp:2"),
                                      list(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1'),
                                      list(name = "bicweights", title = "BIC Weights", type = "number", format = "dp:2"))
            
        }
        
        if(options[['noOfClusters']]=='auto' | options[['noOfClusters']] == 'manual'){
            
            data_evaluation <- list(list(title = 'k-means model', clusters = ".", measure = ".", aic = ".", bic = "."))
            
        } else if (options[["noOfClusters"]] == 'optimized'){
            
            data_evaluation <- list()
            
            for(i in 1:length(options[["optimizedFrom"]]:options[["optimizedTo"]])){
                
                if (i == 1){
                    
                    data_evaluation[[1]] <- list(title = 'Best model', clusters = ".", measure = ".", aic = ".", aicweights = ".", bic = ".", bicweights = ".")
                    
                } else {
                    
                    data_evaluation[[i]] <- list(title = '', clusters = ".", measure = ".", aic = ".", aicweights = ".", bic = ".", bicweights = ".")
                }
                
            }
            
        } else {
            
            data_evaluation <- list()
            
            for(i in 1:length(options[["robustFrom"]]:options[["robustTo"]])){
                
                if (i == 1){
                    
                    data_evaluation[[1]] <- list(title = 'Best model', clusters = ".", measure = ".", aic = ".", aicweights = ".", bic = ".", bicweights = ".")
                    
                } else {
                    
                    data_evaluation[[i]] <- list(title = '', clusters = ".", measure = ".", aic = ".", aicweights = ".", bic = ".", bicweights = ".")
                }
                
            }
            
        }
        results[['evaluation']] <- list(title = 'Summary',
                                        schema = list(fields = fields_evaluation),
                                        data = data_evaluation)
        
    }
    
    # init prediction table ##
    
    if(options[["tablePredictions"]]){
        
        if(!is.null(state[["predictions"]])){
            
            results[["predictions"]] <- state[["predictions"]]
            
        } else {
            
            
            fields_predictions <- list(list(name = 'number', title = "Obs. number", type = 'integer'),
                                       list(name = 'prediction', title = 'Prediction', type = 'integer'))
            
            data_predictions <- list(list(number = ".", prediction = "."))
            
            results[['predictions']] <- list(title = 'Predictions',
                                             schema = list(fields = fields_predictions),
                                             data = data_predictions)
        }
        
    }
    
    # init clusterinfo table ##
    if(options[['tableClusterInformation']]){
        
        if(!is.null(state[["clusterinfo"]])){
            
            results[["clusterinfo"]] <- state[["clusterinfo"]]
            
        } else {
            
            fields_clusterinfo <- list(list(name = 'cluster', title = 'Cluster', type = 'integer'))
            if (options[['tableClusterInfoSize']]){
                
                fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'size', title = 'Size', type = 'integer')
                
            }
            if (options[['tableClusterInfoSumSquares']]){
                
                fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'withinss', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')
                
            }
            
            if(options[['tableClusterInfoCentroids']]){
                
                fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = paste('centroid',1,sep = ''), title = "Centroid", type = 'number', format = 'dp:3')
                
            }
            
            footnotes_BSS <- .newFootnotes()
            
            if(options[['tableClusterInfoBetweenSumSquares']]){
                
                .addFootnote(footnotes_BSS,'The Between Sum of Squares of this model is not yet computed')
                
            }
            
            if(options[['tableClusterInfoTotalSumSquares']]){
                
                .addFootnote(footnotes_BSS,'The Total Sum of Squares of this model is not yet computed')
                
            }
            
            footnotes_BSS <- as.list(footnotes_BSS)
            
            data_clusterinfo <- list(list(cluster = ".", size = ".", withinss = ".", centroid1 = "."))
            
            results[['clusterinfo']] <- list(title = 'Cluster information',
                                             schema = list(fields = fields_clusterinfo),
                                             data = data_clusterinfo,
                                             footnotes = footnotes_BSS)
            
        }
        
    }
    
    return(results)
}


