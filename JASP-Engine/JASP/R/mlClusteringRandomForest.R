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

mlClusteringRandomForest <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)

  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "randomForest")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(options, jaspResults, ready)

  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "randomForest")

  # Create the variable importance table
  .randomForestClusteringVarImpTable(options, jaspResults, ready, position = 3)

  # Create the within sum of squares plot
  .elbowCurvePlot(dataset, options, jaspResults, ready, position = 4)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 5, type = "randomForest")
  
}

.randomForestClustering <- function(dataset, options, jaspResults){

if(options[["modelOpt"]] == "validationManual"){
      
    rfit <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
										y = NULL, 
										ntree = options[["noOfTrees"]], 
										proximity = TRUE, 
										oob.prox = TRUE)

    clusters <- options[['noOfClusters']]

  } else {

    avg_silh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <-  numeric(options[["maxClusters"]] - 1)
    bicStore <-  numeric(options[["maxClusters"]] - 1)

    startProgressbar(length(clusterRange))

	rfit_tmp <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
									y = NULL, 
									ntree = options[["noOfTrees"]], 
									proximity = TRUE, 
									oob.prox = TRUE)
	hrfit_tmp <- hclust(as.dist(1 - rfit_tmp$proximity), method = "ward.D2")

    for (i in clusterRange) {

  	  pred.values <- cutree(hrfit_tmp, k = i)
      silh <- summary(cluster::silhouette(pred.values, dist(dataset[, .v(options[["predictors"]])])))
      avg_silh[i - 1] <- silh[["avg.width"]]

	  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
    wss_tmp <- numeric(i)
    for(j in 1:i) {
      if (m == 1) {
        wss_tmp[j] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == j])
      } else {
        wss_tmp[j] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == j,])
      }
    }

	wssStore[i - 1] <- sum(wss_tmp)

	n <- length(pred.values)
	k <- i
	D <- sum(wss_tmp)
	aicStore[i - 1] <- D + 2*m*k
	bicStore[i - 1] <- D + log(n)*m*k
      
    progressbarTick()
  }

  clusters <- base::switch(options[["optimizationCriterion"]],
                            "validationSilh" = clusterRange[which.max(avg_silh)],
                            "validationAIC" = clusterRange[which.min(aicStore)],
                            "validationBIC" = clusterRange[which.min(bicStore)])

	rfit <- randomForest::randomForest(x = dataset[, .v(options[["predictors"]])], 
										y = NULL, 
										ntree = options[["noOfTrees"]], 
										proximity = TRUE, 
										oob.prox = TRUE)

  }

  hrfit <- hclust(as.dist(1 - rfit$proximity), method = "ward.D2")
  pred.values <- cutree(hrfit, k = clusters)

  clusters <- clusters
  size <- as.numeric(table(pred.values))

  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  wss <- numeric(clusters)
  for(i in 1:clusters) {
    if (m == 1) {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == i])
    } else {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][pred.values == i,])
    }
  }

  tss <- .tss(dist(dataset[, .v(options[["predictors"]])]))

  n <- length(pred.values)
  k <- clusters
  D <- sum(wss)
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  silhouettes <- summary(cluster::silhouette(pred.values, dist(dataset[, .v(options[["predictors"]])])))
  Silh_score <- silhouettes[["avg.width"]]
  silh_scores <- silhouettes[["clus.avg.widths"]]

  clusterResult <- list()
  clusterResult[["pred.values"]] <- pred.values
  clusterResult[['clusters']] <- clusters
  clusterResult[["N"]] <- nrow(dataset)
  clusterResult[['size']] <- size
  clusterResult[['WSS']] <- wss
  clusterResult[['TSS']] <- tss
  clusterResult[['BSS']] <- clusterResult[['TSS']] - sum(clusterResult[['WSS']])
  clusterResult[['AIC']] <- aic
  clusterResult[['BIC']] <- bic
  clusterResult[['Silh_score']] <- Silh_score
  clusterResult[['silh_scores']] <- silh_scores
  clusterResult[["fit"]] <- rfit

  if(options[["modelOpt"]] != "validationManual"){
    clusterResult[['silhStore']] <- avg_silh
    clusterResult[["aicStore"]] <- aicStore
    clusterResult[["bicStore"]] <- bicStore
    clusterResult[["wssStore"]] <- wssStore
  }

  return(clusterResult)
}

.randomForestClusteringVarImpTable <- function(options, jaspResults, ready, position){

  if (!is.null(jaspResults[["importanceTable"]]) || !options[["importanceTable"]]) return()
  
  # Create table
  importanceTable <- createJaspTable(title = "Variable Importance")
  importanceTable$position <- position
  importanceTable$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", "optimizationCriterion",
                                                      "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "importanceTable"))
  
  # Add column info
  importanceTable$addColumnInfo(name = "variable",  title = " ", type = "string")
  importanceTable$addColumnInfo(name = "measure",  title = "Mean decrease in Gini Index", type = "number", format = "sf:4")

  jaspResults[["importanceTable"]] <- importanceTable

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object
  fit <- clusterResult[["fit"]]
  varImp <- fit[["importance"]]
  ord <- order(varImp, decreasing = TRUE)
  name <- .unv(rownames(varImp)[ord])
  values <- as.numeric(varImp[ord])
  
  # Add data per column
  row <- data.frame(variable = name, measure = values)
  importanceTable$addRows(row)

}

