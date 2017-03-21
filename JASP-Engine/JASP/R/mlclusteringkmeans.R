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

MLClusteringKMeans <- function(dataset = NULL, options, perform = "run", callback = function(...) list(status = "ok"), ...) {
	
	perform = "run"
	
	# read variables
	predictors <- unlist(options['predictors'])
	predictors <- predictors[predictors != ""]
	
	# read dataset
	if (is.null(dataset)) {
		
		if (perform == "run") {
			
			dataset <- .readDataSetToEnd(columns.as.numeric=predictors)
			
		} else {
			
			dataset <- .readDataSetHeader(dataset, columns.as.numeric=predictors)
		}
		
	} else {
		
		dataset <- .vdf(columns.as.numeric=predictors)
	}
	
	
	# code in base64
	if(length(predictors[predictors!='']) > 0){
		predictors <- .v(predictors)
	}
	
	set.seed(1)
	
	# create results bundle
	results <- list()
	
	# Provide the Meta to the results bundle
	meta <- list(list(name = 'K-means clustering', type = 'title'),
				 list(name = 'evaluation', type = 'table'),
				 list(name = "clusterinfo", type = 'table'),
				 list(name = 'predictions', type = 'table'),
				 list(name = '2dplot', type = 'image'),
				 list(name = 'criterionvsclusters', type = 'image'),
				 list(name = 'pcaplot', type = 'image'),
				 list(name = 'withinssvsclusters', type = 'image'))
	results[['.meta']] <- meta
	
	results[['title']] <- 'K-means clustering'
	
	# Set the right options for the analysis
	opt <- .OptionsSet(dataset,options)
	
	# do the analysis
	res <- .DoKmeansAnalysis(dataset,options,opt,predictors)
	
	# create the evaluation table
	results[['evaluation']] <- .EvaluationTableKmeans(res)
	
	# create the cluster information table
	if (options[['tableClusterInformation']] & !is.null(res)){
		
		fields_clusterinfo <- list(list(name = 'cluster', title = 'Cluster', type = 'integer'))
		if (options[['tableClusterInfoSize']]){
			
			fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'size', title = 'Size', type = 'integer')
			
		}
		if (options[['tableClusterInfoSumSquares']]){
			
			fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = 'withinss', title = 'Within Sum of Squares', type = 'number', format = 'dp:2')
			
		}
		
		if(options[['tableClusterInfoCentroids']]){
			
			for( i in 1:length(predictors)){
				fields_clusterinfo[[length(fields_clusterinfo)+1]] <- list(name = paste('centroid',i,sep = ''), title = paste('Centroid',i), type = 'number', format = 'dp:3')	
			}
			
		}
		
		if(options[['tableClusterInfoBetweenSumSquares']]){
			footnotes_BSS <- .newFootnotes()
			.addFootnote(footnotes_BSS,paste('The Between Sum of Squares of this model is', round(res[['BSS']],2)))
			footnotes_BSS <- as.list(footnotes_BSS)
		}
		
		if(options[['tableClusterInfoTotalSumSquares']]){
			footnotes_TSS <- .newFootnotes()
			.addFootnote(footnotes_TSS,paste('The Total Sum of Squares of this model is', round(res[['TSS']],2)))
			footnotes_TSS <- as.list(footnotes_TSS)
		}
		
		data_clusterinfo <- list()
		
		for(i in 1:res[['clusters']]){
			data_clusterinfo[[i]] <- list(cluster = i, size = res[['size']][i], withinss = res[['WSS']][[i]])
			for( j in 1:length(predictors)){
				data_clusterinfo[[i]][[paste('centroid',j,sep='')]] <- res[['centroids']][[i,j]]
			}
		}
		
		results[['clusterinfo']] <- list(title = 'Cluster information',
										 schema = list(fields = fields_clusterinfo),
										 data = data_clusterinfo)
		
		if(options[['tableClusterInfoBetweenSumSquares']] & !options[['tableClusterInfoTotalSumSquares']]){
			
			results[['clusterinfo']][['footnotes']] <- footnotes_BSS
			
		} else if (!options[['tableClusterInfoBetweenSumSquares']] & options[['tableClusterInfoTotalSumSquares']]){
			
			results[['clusterinfo']][['footnotes']] <- footnotes_TSS
			
		} else if (options[['tableClusterInfoBetweenSumSquares']] & options[['tableClusterInfoTotalSumSquares']]){
			
			results[['clusterinfo']][['footnotes']] <- as.list(footnotes_BSS, footnotes_TSS)
			
		}
		
	}
	
	# Predictions table
	if(options[['tablePredictions']] & !is.null(res)){
		results[['predictions']] <- .PredictionsTableKmeans(res,options)
	}
	
	# PCA cluster plot
	if(options[['plotPCACluster']]){
		results[['pcaplot']] <- .PCAplot(options,res,dataset,opt,predictors)
	}
	
	# 2-d clusterplot
	if(options[['plot2dCluster']]){
		results[['2dplot']] <- .plot2d(dataset,res,opt,options,predictors)
	}
	
	# within ss vs cluster plot	
	if(options[['plotPCAClusterSquares']]){
		results[['withinssvsclusters']] <- .WSSplot(options,res)
	}
	
	# criterion vs cluster plot	
	if(options[['plotCriterionVsClusters']]){
		results[['criterionvsclusters']] <- .criterionplot(options,res)
	}
	
	return(list(results = results, status = "complete"))
	
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
	install.packages('fpc', repos="http://cran.rstudio.com/")
	library(fpc)
	fit <- fpc::pamk(dataset[,predictors],
					 krange = opt[['clusters']],
					 criterion = opt[['criterion']],
					 usepam = opt[['Pam']],
					 scaling = FALSE,
					 alpha = 0.001,
					 critout = FALSE,
					 ns = 10)
	res <- list(
		'Predictions' = NULL,
		'clusters' = NULL,
		'criterion.krange' = NULL,
		'clusterrange' = NULL,
		'size' = NULL,
		'centroids' = NULL,
		'WSS' = NULL,
		'BSS' = NULL,
		'TSS' = NULL
	)
	res[['clusters']] <- fit$nc
	res[['criterion.krange']] <- fit$crit[which(fit$crit!=0)]
	kfit <- kmeans(dataset[,predictors],
				   centers = fit$nc,
				   iter.max = opt[['iter']],
				   nstart = opt[['sets']],
				   algorithm = opt[['algorithm']])
	
	res[['Predictions']] <- data.frame(
		'Observation' = 1:nrow(dataset),
		'Cluster' = kfit$cluster
	)
	res[['clusterrange']] <- opt[['clusters']]
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

.Kmeansoptimized <- function(dataset,options,opt, predictors){
	WSS <- numeric(options[['optimizedTo']] - options[['optimizedFrom']])
	clusters <- opt[['clusters']]
	for(i in seq_along(clusters)){
		index <- clusters[i]
		kfit <- kmeans(dataset[,predictors],
					   centers = index,
					   iter.max = opt[['iter']],
					   nstart = opt[['sets']],
					   algorithm = opt[['algorithm']])
		WSS[i] <- kfit$tot.withinss
	}
	res<-list(
		'Predictions' = NULL,
		'WithinSumSquares' = NULL,
		'optimalclusters' = NULL,
		'clusters' = NULL,
		'centroids' = NULL,
		'size' = NULL,
		'WSS' = NULL,
		'BSS' = NULL,
		'TSS' = NULL
	)
	res[['WithinSumSquares']] <- WSS
	res[['clusters']] <- .determineoptimum(WSS,opt)
	kfit <- kmeans(dataset[,predictors],
				   centers = res[['clusters']],
				   iter.max = opt[['iter']],
				   nstart = opt[['sets']],
				   algorithm = opt[['algorithm']])
	res[['Predictions']] <- data.frame(
		'Observation' = 1:nrow(dataset),
		'Cluster' = kfit$cluster
	)
	res[['clusterrange']] <- clusters
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

.determineoptimum <- function(WSS,opt){
	dif <- numeric(length(WSS)-1)
	slope <- logical(length(dif)-1)
	for(i in 1:(length(WSS)-1)){
		dif[i] <- WSS[i+1] - WSS[i]
	}
	for( j in 1:(length(dif)-1)){
		if(abs(dif[j+1]) < abs(dif[j]*0.5)){
			slope[j]<-TRUE
		}
	}
	optimum <- which(slope==TRUE)[1]+1
	if(is.na(optimum)){
		optimum <- opt[['clusters']][1]
	}
	return(optimum)
}

.EvaluationTableKmeans <- function(res){
	
	fields_evaluation <- list(list(name = 'title', title = "", type = 'string'),
							  list(name = 'clusters', title = 'No. clusters', type = 'integer'),
							  list(name = 'measure', title = 'R-squared', type = 'number', format = 'dp:2'),
							  list(name = 'aic', title = 'AIC', type = 'number', format = 'dp:1'),
							  list(name = 'bic', title = 'BIC', type = 'number', format = 'dp:1'))
	if(is.null(res)){
		
		data_evaluation <- list(list(title = 'K-means model', clusters = 0, measure = 0, aic = 0, bic = 0))
		
	} else {
		
		data_evaluation <- list(list(title = 'K-means model', clusters = res[['clusters']], measure = res[['BSS']]/res[['TSS']], aic = res[['AIC']], bic = res[['BIC']]))
		
	}
	
	return(list(title = 'Evaluation',
				schema = list(fields = fields_evaluation),
				data = data_evaluation))
	
}

.PredictionsTableKmeans <- function(res, options){
	
	fields_predictions <- list(list(name = 'number', title = "Obs. number", type = 'integer'),
							   list(name = 'prediction', title = 'Prediction', type = 'integer'))
	
	data_predictions <- list()
	
	for(i in 1:nrow(res[['Predictions']])){
		data_predictions[[i]] <- list(number = i, prediction = res[['Predictions']][[i,2]])
	}
	
	return(list(title = 'Predictions',
				schema = list(fields = fields_predictions),
				data = data_predictions))
	
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
	install.packages('cluster',repos="http://cran.rstudio.com/")
	library(cluster)
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
	
	if(options[['plot2dCluster']]){
		im <- .beginSaveImage(640,480)
		.TwodClusterplot(dataset,res,opt,options,predictors)
		cont <- .endSaveImage(im)
	}
	
	return(list(title = '2-D cluster plot',
				width = 640,
				height = 480,
				data = cont))
	
}

.TwodClusterplot <- function(dataset,res,opt,options,predictors){
	install.packages('vegan',repos="http://cran.rstudio.com/")
	library(vegan)
	if(length(predictors)!= 2){
		stop('Two variables must be specified')
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
		im2 <-.beginSaveImage(640,480)
		.PlotOptimizedkClustering(res)
		cont2<-.endSaveImage(im2)
		
	}
	
	return(list(title = 'Within Sum of Squares vs cluster plot',
				width = 640,
				height = 480,
				data = cont2))
}

.PlotOptimizedkClustering <- function(res){
	plot(res[['clusterrange']],
		 res[['WithinSumSquares']],
		 main = '',
		 type = 'b',
		 las = 1,
		 xlab = '',
		 ylab = '',
		 bty = 'n',
		 pch = 19)
	mtext(expression('Total within sum of squares'), side = 2, line = 2, cex = 1.5, font = 2)
	mtext("Clusters", side = 1, line = 3, cex = 1.5, font = 2)
	points(res[['clusters']],
		   res[['WithinSumSquares']][which(res[['clusterrange']]==res[['clusters']])],
		   col='red',
		   pch=19)
}

.criterionplot<-function(options,res){
	
	if(options[['plotCriterionVsClusters']] & !is.null(res)){
		im3 <- .beginSaveImage(640,480)
		.PlotRobustOptimize(res)
		cont3 <- .endSaveImage(im3)
		
	}
	
	return(list(title ='Criterion vs cluster plot',
				width = 640,
				height = 480,
				data = cont3))
	
}

.PlotRobustOptimize <- function(res){
	plot(res[['clusterrange']],
		 res[['criterion.krange']],
		 type='b',
		 pch=19,
		 xlab = '',
		 ylab = '',
		 bty = 'n',
		 main = '',
		 las = 1)
	mtext(expression('Criterion'), side = 2, line = 2, cex = 1.5, font = 2)
	mtext("Clusters", side = 1, line = 3, cex = 1.5, font = 2)
	points(res[['clusterrange']][which.max(res[['criterion.krange']])],
		   max(res[['criterion.krange']]),
		   pch = 19,
		   col= 'red')
}