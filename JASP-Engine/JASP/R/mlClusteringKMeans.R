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

mlClusteringKMeans <- function(jaspResults, dataset, options, ...) {
    
  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "kmeans")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(options, jaspResults, ready)
  
  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "kmeans")

  # Create the within sum of squares plot
  .elbowCurvePlot(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 4, type = "kmeans")
  
}

.kMeansClustering <- function(dataset, options, jaspResults, ready){

  if(options[["modelOpt"]] == "validationManual"){
    
    kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                   centers = options[['noOfClusters']],
                   iter.max = options[['noOfIterations']],
                   nstart = options[['noOfRandomSets']],
                   algorithm = options[['algorithm']])

    clusters <- options[['noOfClusters']]

  } else {

    avg_silh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <-  numeric(options[["maxClusters"]] - 1)
    bicStore <-  numeric(options[["maxClusters"]] - 1)

    startProgressbar(length(clusterRange))

    for (i in clusterRange) {
      kfit_tmp <- kmeans(dataset[, .v(options[["predictors"]])],
                   centers = i,
                   iter.max = options[['noOfIterations']],
                   nstart = options[['noOfRandomSets']],
                   algorithm = options[['algorithm']])
      silh <- summary(cluster::silhouette(kfit_tmp$cluster, dist(dataset[, .v(options[["predictors"]])])))
      avg_silh[i - 1] <- silh[["avg.width"]]

      v_tmp <- kfit_tmp$centers
      clabels_tmp <- kfit_tmp$cluster
      csumsqrs_tmp <- .sumsqr(dataset[, .v(options[["predictors"]])], v_tmp, clabels_tmp) 
      wssStore[i - 1] <- csumsqrs_tmp$tot.within.ss

      m <- ncol(kfit_tmp$centers)
      n <- length(kfit_tmp$cluster)
      k <- nrow(kfit_tmp$centers)
      D <- kfit_tmp$tot.withinss
      aicStore[i - 1] <- D + 2*m*k
      bicStore[i - 1] <- D + log(n)*m*k
      
      progressbarTick()
  }

  clusters <- base::switch(options[["optimizationCriterion"]],
                            "validationSilh" = clusterRange[which.max(avg_silh)],
                            "validationAIC" = clusterRange[which.min(aicStore)],
                            "validationBIC" = clusterRange[which.min(bicStore)])
  
  kfit <- kmeans(dataset[, .v(options[["predictors"]])],
                 centers = clusters,
                 iter.max = options[['noOfIterations']],
                 nstart = options[['noOfRandomSets']],
                 algorithm = options[['algorithm']])

  }

  pred.values <- kfit$cluster
  clusters <- clusters
  size <- kfit$size
  centroids <- kfit$centers
  wss <- kfit$withinss
  tss <- kfit$totss
  bss <- kfit$betweenss

  m <- ncol(kfit$centers)
  n <- length(kfit$cluster)
  k <- nrow(kfit$centers)
  D <- kfit$tot.withinss
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  silhouettes <- summary(cluster::silhouette(kfit$cluster, dist(dataset[, .v(options[["predictors"]])])))
  Silh_score <- silhouettes[["avg.width"]]
  silh_scores <- silhouettes[["clus.avg.widths"]]

  clusterResult <- list()
  clusterResult[["pred.values"]] <- pred.values
  clusterResult[['clusters']] <- clusters
  clusterResult[["N"]] <- nrow(dataset)
  clusterResult[['size']] <- size
  clusterResult[['centroids']] <- centroids
  clusterResult[['WSS']] <- wss
  clusterResult[['TSS']] <- tss
  clusterResult[['BSS']] <- bss
  clusterResult[['AIC']] <- aic
  clusterResult[['BIC']] <- bic
  clusterResult[['Silh_score']] <- Silh_score
  clusterResult[['silh_scores']] <- silh_scores

  if(options[["modelOpt"]] != "validationManual"){
    clusterResult[['silhStore']] <- avg_silh
    clusterResult[["aicStore"]] <- aicStore
    clusterResult[["bicStore"]] <- bicStore
    clusterResult[["wssStore"]] <- wssStore
  }

  return(clusterResult)
}
