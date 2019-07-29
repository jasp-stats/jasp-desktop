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

mlClusteringHierarchical <- function(jaspResults, dataset, options, ...) {
  
  # Preparatory work
  dataset <- .readDataClusteringAnalyses(dataset, options)
  .errorHandlingClusteringAnalyses(dataset, options)
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "hierarchical")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(options, jaspResults, ready)
  
  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "hierarchical")

  # Create the within sum of squares plot
  .elbowCurvePlot(dataset, options, jaspResults, ready, position = 3)
  
  # Create dendrogram
  .hierarchicalClusteringDendogram(dataset, options, jaspResults, ready, position = 4)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 5, type = "hierarchical")
  
}

.hierarchicalClustering <- function(dataset, options, jaspResults){
  
  if(options[["modelOpt"]] == "validationManual"){
        
    if (options[["distance"]] == "Pearson correlation") {
      hfit <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]), method="pearson")),
                                      method = options[["linkage"]]), k = options[['noOfClusters']])
    } else {
      hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                                  method = options[["linkage"]]), k = options[['noOfClusters']])
    }

    clusters <- options[['noOfClusters']]

  } else {

    avg_silh <- numeric(options[["maxClusters"]] - 1)
    wssStore <- numeric(options[["maxClusters"]] - 1)
    clusterRange <- 2:options[["maxClusters"]]
    aicStore <-  numeric(options[["maxClusters"]] - 1)
    bicStore <-  numeric(options[["maxClusters"]] - 1)

    startProgressbar(length(clusterRange))

    for (i in clusterRange){

      if(options[["distance"]] == "Pearson correlation") {
        hfit_tmp <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]), method="pearson")),
                                        method = options[["linkage"]]), k = i)
      } else {
        hfit_tmp <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                                    method = options[["linkage"]]), k = i)
      }
      silh <- summary(cluster::silhouette(hfit_tmp, dist(dataset[, .v(options[["predictors"]])])))
      avg_silh[i - 1] <- silh[["avg.width"]]

      m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]

      wss <- numeric(length(table(hfit_tmp)))
      for (j in 1:length(table(hfit_tmp))) {
        if (m == 1) {
          wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit_tmp == j])
        } else {
          wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit_tmp == j,])
        }
      }
      wssStore[i - 1] <- sum(wss)

      n = length(hfit_tmp)
      k = length(table(hfit_tmp))
      D = sum(wss)
      aic <- D + 2*m*k
      bic <- D + log(n)*m*k
      aicStore[i - 1] <- D + 2*m*k
      bicStore[i - 1] <- D + log(n)*m*k
      
      progressbarTick()
    }

    clusters <- base::switch(options[["optimizationCriterion"]],
                              "validationSilh" = clusterRange[which.max(avg_silh)],
                              "validationAIC" = clusterRange[which.min(aicStore)],
                              "validationBIC" = clusterRange[which.min(bicStore)])
    
    if (options[["distance"]] == "Pearson correlation") {
        hfit <- cutree(hclust(as.dist(1-cor(t(dataset[, .v(options[["predictors"]])]), method="pearson")),
                                        method = options[["linkage"]]), k = clusters)
    } else {
        hfit <- cutree(hclust(dist(dataset[, .v(options[["predictors"]])]),
                                    method = options[["linkage"]]), k = clusters)
    }

  }
  
  pred.values <- hfit
  clusters <- clusters
  size <- as.data.frame(table(hfit))[,2]

  m <- dim(as.data.frame(dataset[, .v(options[["predictors"]])]))[2]

  wss <- numeric(length(table(hfit)))
  for (j in 1:length(table(hfit))) {
    if (m == 1) {
      wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit == j])
    } else {
      wss[j] <- .ss(dataset[, .v(options[["predictors"]])][hfit == j,])
    }
  }

  n = length(hfit)
  k = length(table(hfit))
  D = sum(wss)
  aic <- D + 2*m*k
  bic <- D + log(n)*m*k

  if(options[["distance"]] == "Pearson correlation") {
    silhouettes <- summary(cluster::silhouette(hfit, as.dist(1-cor(t(dataset[, .v(options[["predictors"]])])))))
  } else {
    silhouettes <- summary(cluster::silhouette(hfit, dist(dataset[, .v(options[["predictors"]])])))
  }
  Silh_score <- silhouettes[["avg.width"]]
  silh_scores <- silhouettes[["clus.avg.widths"]]

  clusterResult <- list()
  clusterResult[["pred.values"]] <- pred.values
  clusterResult[['clusters']] <- clusters
  clusterResult[["N"]] <- nrow(dataset)
  clusterResult[['size']] <- size
  clusterResult[['WSS']] <- wss
  clusterResult[['TSS']] <- .tss(dist(dataset[, .v(options[["predictors"]])]))
  clusterResult[['BSS']] <- clusterResult[['TSS']] - sum(clusterResult[['WSS']])
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

.hierarchicalClusteringDendogram <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["dendrogram"]]) || !options[["dendrogram"]]) return()

  dendrogram <- createJaspPlot(plot = NULL, title = "Dendogram", width = 500, height = 300)
  dendrogram$position <- position
  dendrogram$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "dendrogram", "optimizationCriterion"))
  jaspResults[["dendrogram"]] <- dendrogram

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(options[["seedBox"]])  set.seed(options[["seed"]])

  unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
  data <- dataset[unique.rows, .v(options[["predictors"]])]

  if(options[["distance"]] == "Pearson correlation") {
    hc <- hclust(as.dist(1-cor(t(data), method="pearson")), method = options[["linkage"]])
  } else {
    hc <- hclust(dist(data), method = options[["linkage"]])
  }
  
  p <- ggdendro::ggdendrogram(hc)
  p <- JASPgraphs::themeJasp(p) + ggdendro::theme_dendro()
  dendrogram$plotObject <- p
}

