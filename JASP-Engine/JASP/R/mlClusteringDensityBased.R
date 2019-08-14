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

mlClusteringDensityBased <- function(jaspResults, dataset, options, ...) {

  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "densitybased")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(options, jaspResults, ready)
  
  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "densitybased")
  
  # Create the k-distance plot
  .densityBasedClusteringKdistPlot(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 4, type = "densitybased")

}

.densityBasedClustering <- function(dataset, options, jaspResults){
 
  if (options[["distance"]] == "Correlated densities") {
    dfit <- dbscan::dbscan(as.dist(1-cor(t(as.data.frame(dataset[, .v(options[["predictors"]])])), method = "pearson")), eps = options[['eps']], minPts = options[['minPts']])
  } else {
    dfit <- dbscan::dbscan(as.data.frame(dataset[, .v(options[["predictors"]])]), eps = options[['eps']], minPts = options[['minPts']])
  }

  noisePoints <- length(dfit$cluster[dfit$cluster == 0])
  clusters <- ifelse(noisePoints > 0, yes = length(table(dfit$cluster)) - 1, no = length(table(dfit$cluster)))

  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]
  
  wss <- numeric(clusters)
  for(i in 1:clusters) {
    if (m == 1) {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][dfit$cluster == i])
    } else {
      wss[i] <- .ss(dataset[, .v(options[["predictors"]])][dfit$cluster == i,])
    }
  }

  if(noisePoints > 0) {
    tss <- .tss(dist(dataset[, .v(options[["predictors"]])][dfit$cluster != 0, ]))
  } else {
    tss <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  }

  pred.values <- dfit$cluster
  clusters <- clusters
  size <- as.data.frame(table(dfit$cluster))[,2]

  n <- length(dfit$cluster)
  k <- clusters
  D <- sum(wss)
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  nullClusters <- oneClusters <- 0 
  for (i in 1:length(dfit$cluster)) {
    if (dfit$cluster[i] == 0) {
      nullClusters <- nullClusters + 1
    } else if(dfit$cluster[i] == 1){
      oneClusters <- oneClusters + 1
    }
  }
  
  zeroMark <- ifelse(nullClusters == length(dfit$cluster), yes = 1, no = 0)
  oneMark <- ifelse(oneClusters == length(dfit$cluster), yes = 1, no = 0)
  
  if (oneMark == 0 && zeroMark == 0){
    if(options[["distance"]] == "Normal densities"){
      silhouettes <- summary(cluster::silhouette(dfit$cluster, dist(dataset[, .v(options[["predictors"]])])))
    } else if(options[["distance"]] == "Correlated densities"){
      silhouettes <- summary(cluster::silhouette(dfit$cluster, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))
    }
  } else {
    silhouettes <- list("avg.width" = 0, "clus.avg.widths" = rep(0, clusters))
  }

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
  clusterResult[["zeroMark"]] <- zeroMark
  clusterResult[["oneMark"]] <- oneMark
  clusterResult[["noisePoints"]] <- noisePoints

  return(clusterResult)
}

.densityBasedClusteringKdistPlot <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["kdistPlot"]]) || !options[["k-distplot"]]) return()

  kdistPlot <- createJaspPlot(plot = NULL, title = "K-Distance Plot", width = 500, height = 300)
  kdistPlot$position <- position
  kdistPlot$dependOn(options = c("predictors", "eps", "minPts", "modelOpt", "seed", "scaleEqualSD", "ready", "k-distplot", "distance"))
  jaspResults[["kdistPlot"]] <- kdistPlot

  if(!ready) return()

  unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
  data <- dataset[unique.rows, .v(options[["predictors"]])]
  if(options[["distance"]] == "Correlated densities"){
    knnDist <- dbscan::kNNdist(as.dist(1-cor(t(as.data.frame(data)), method = "pearson")), k = options[['minPts']])
  } else {
    knnDist <- dbscan::kNNdist(data , k = options[['minPts']])
  }
  knnDims <- dim(knnDist)
  knnValues <- seq(from = 1, to = knnDims[1] * (knnDims[2]-1), by = 1)

  d <- data.frame(x = knnValues, y = sort(knnDist[,2:options[['minPts']]]))

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  lineData <- data.frame(xstart = xBreaks[1], xend = xBreaks[length(xBreaks)], ystart = options[["eps"]], yend = options[["eps"]])

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) + 
        JASPgraphs::geom_line() +
        ggplot2::scale_x_continuous(name = "Points sorted by distance", breaks = xBreaks, limits = range(xBreaks)) + 
        ggplot2::scale_y_continuous(name = paste0(options[['minPts']], '-nearest neighbors \ndistance'), breaks = yBreaks, limits = range(yBreaks)) +
        ggplot2::geom_segment(ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), data = lineData, linetype = 2, color = "darkgray")
  p <- JASPgraphs::themeJasp(p)

  kdistPlot$plotObject <- p
}





