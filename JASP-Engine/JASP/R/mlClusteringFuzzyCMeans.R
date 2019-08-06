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

mlClusteringFuzzyCMeans <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "cmeans")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(options, jaspResults, ready)
  
  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "cmeans")

  # Create the within sum of squares plot
  .elbowCurvePlot(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 4, type = "cmeans")
  
}

.cMeansClustering <- function(dataset, options, jaspResults, ready){
  
  if(options[["modelOpt"]] == "validationManual"){
      
    cfit <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                            centers = options[['noOfClusters']],
                            iter.max = options[['noOfIterations']],
                            m = options[["m"]])

    clusters <- options[['noOfClusters']]

  } else {

    avg_silh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <-  numeric(options[["maxClusters"]] - 1)
    bicStore <-  numeric(options[["maxClusters"]] - 1)

    startProgressbar(length(clusterRange))

    for (i in clusterRange) {
      cfit_tmp <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                              centers = i,
                              iter.max = options[['noOfIterations']],
                              m = options[["m"]])
      silh <- summary(cluster::silhouette(cfit_tmp$cluster, dist(dataset[, .v(options[["predictors"]])])))
      avg_silh[i - 1] <- silh[["avg.width"]]

      v_tmp <- cfit_tmp$centers
      clabels_tmp <- cfit_tmp$cluster
      csumsqrs_tmp <- .sumsqr(dataset[, .v(options[["predictors"]])], v_tmp, clabels_tmp) 
      wssStore[i - 1] <- csumsqrs_tmp$tot.within.ss

      m <- ncol(cfit_tmp$centers)
      n <- length(cfit_tmp$cluster)
      k <- nrow(cfit_tmp$centers)
      D <- csumsqrs_tmp$tot.within.ss
      aicStore[i - 1] <- D + 2*m*k
      bicStore[i - 1] <- D + log(n)*m*k
      
      progressbarTick()
  }

  clusters <- base::switch(options[["optimizationCriterion"]],
                            "validationSilh" = clusterRange[which.max(avg_silh)],
                            "validationAIC" = clusterRange[which.min(aicStore)],
                            "validationBIC" = clusterRange[which.min(bicStore)])
  
  cfit <- e1071::cmeans(dataset[, .v(options[["predictors"]])],
                          centers = clusters,
                          iter.max = options[['noOfIterations']],
                          m = options[["m"]])

  }

  v <- cfit$centers
  clabels <- cfit$cluster
  csumsqrs <- .sumsqr(dataset[, .v(options[["predictors"]])], v, clabels)

  pred.values <- cfit$cluster
  clusters <- clusters
  size <- cfit$size
  centroids <- cfit$centers
  wss <- csumsqrs$wss
  tss <- csumsqrs$tss
  bss <- csumsqrs$bss

  m <- ncol(cfit$centers)
  n <- length(cfit$cluster)
  k <- nrow(cfit$centers)
  D <- csumsqrs$tot.within.ss
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  silhouettes <- summary(cluster::silhouette(cfit$cluster, dist(dataset[, .v(options[["predictors"]])])))
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