#
# Copyright (C) 2019 University of Amsterdam
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

.readDataClusteringAnalyses <- function(dataset, options){
  predictors <- unlist(options[['predictors']])
  predictors <- predictors[predictors != ""]
  if (is.null(dataset)){
    dataset <- .readAndAddCompleteRowIndices(dataset, predictors)
  }
  if(options[["scaleEqualSD"]] && length(unlist(options[["predictors"]])) > 0)
    dataset <- .scaleNumericData(dataset)

  return(dataset)
}

.errorHandlingClusteringAnalyses <- function(dataset, options, type){
  predictors <- unlist(options$predictors)

  customChecks <- .getCustomErrorChecksClustering(dataset, options, type)
  if(length(predictors[predictors != ""]) > 0L)
    .hasErrors(dataset, type = c('infinity', 'observations'), custom=customChecks, 
                all.target = predictors, observations.amount = "< 2", exitAnalysisIfErrors = TRUE)

  return()
}

.getCustomErrorChecksClustering <- function(dataset, options, type){

  checkClusters <- function() {
    if (type != "densitybased") {
      clusters  <- base::switch(options[["modelOpt"]], "validationManual" = options[["noOfClusters"]], "validationOptimized" = options[["maxClusters"]])
      if (clusters > (nrow(dataset) - 1))
        return(gettextf("You have specified more clusters than distinct data points. Please choose a number lower than %s.", nrow(dataset)))
    }
  }

  checkDataHierarchicalClustering <- function() {
    if(type != "hierarchical")
      return()

    if(nrow(dataset) >= 65536L)
      return(gettext("R package error: The hclust clustering algorithm from the stats R package cannot handle data that has 65536 or more rows. We are working on a solution for this problem. Please try another algorithm in the meantime."))
  }

  return(list(checkClusters, checkDataHierarchicalClustering))
}

.clusterAnalysesReady <- function(options){
  ready <- length(options[["predictors"]][options[["predictors"]] != ""]) >= 2
  return(ready)
}

.sumsqr <- function(x, v, clusters){
  sumsqr <- function(x) sum(scale(x, scale = FALSE)^2)
  bwss <- sumsqr(v[clusters,])
  wss <- sapply(split(as.data.frame(x), clusters), sumsqr)
  twss <- sum(wss)
  tss <- bwss + twss
  ss <- list(bwss, wss, twss, tss)
  names(ss) <- c("bss", "wss", "tot.within.ss", "tss")
  return(ss)
}

.disttovar <- function(x) {
  mean(x**2)/2
}

.tss <- function(x) {
  n <- nrow(as.matrix(x))
  .disttovar(x)*(n-1)
}

.ss <- function(x) {
  sum(scale(x, scale = FALSE)^2)
}

.clustering <- function(dataset, options, jaspResults, ready, type){

  if(!is.null(jaspResults[["clusterResult"]])) return()

  # set the seed so that every time the same set is chosen (to prevent random results) ##
  if(options[["seedBox"]]) set.seed(options[["seed"]])

  if(ready){
    if(type == "kmeans"){
      clusterResult <- .kMeansClustering(dataset, options, jaspResults)
    } else if(type == "cmeans"){
      clusterResult <- .cMeansClustering(dataset, options, jaspResults)
    } else if(type == "hierarchical"){
      clusterResult <- .hierarchicalClustering(dataset, options, jaspResults)
    } else if(type == "densitybased"){
      clusterResult <- .densityBasedClustering(dataset, options, jaspResults)
    } else if(type == "randomForest"){
      clusterResult <- .randomForestClustering(dataset, options, jaspResults)
    }

  jaspResults[["clusterResult"]] <- createJaspState(clusterResult)
  jaspResults[["clusterResult"]]$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed",
                                                    "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "maxTrees", "optimizationCriterion"))
  }
}

.clusteringTable <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["clusteringTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                        "kmeans" = gettext("K-Means Clustering"),
                        "cmeans" = gettext("Fuzzy C-Means Clustering"),
                        "hierarchical" = gettext("Hierarchical Clustering"),
                        "densitybased" = gettext("Density-Based Clustering"),
                        "randomForest" = gettext("Random Forest Clustering"))

  clusteringTable                       <- createJaspTable(title)
  clusteringTable$position <- position
  clusteringTable$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", "optimizationCriterion",
                                                      "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "maxTrees"))

  clusteringTable$addColumnInfo(name = 'clusters', title = gettext('Clusters'), type = 'integer')
  clusteringTable$addColumnInfo(name = 'n', title = gettext('N'), type = 'integer')
  clusteringTable$addColumnInfo(name = 'measure', title = gettextf('R%s', "\u00B2"), type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'aic', title = gettext('AIC'), type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'bic', title = gettext('BIC'), type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')

  clusteringTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")

  if(!ready) clusteringTable$addFootnote(gettext("Please provide at least 2 variables."))

  jaspResults[["clusteringTable"]]      <- clusteringTable

  if(!ready) return()

  .clustering(dataset, options, jaspResults, ready, type = type)

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(options[["modelOpt"]] != "validationManual"){
    criterion <- base::switch(options[["optimizationCriterion"]], "validationAIC" = gettext("AIC"), "validationBIC" = gettext("BIC"), "validationSilh" = gettext("silhouette"))
    clusteringTable$addFootnote(gettextf("The model is optimized with respect to the <i>%s</i> value.", criterion))
  }

  if(clusterResult[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- gettext("The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization.")
    clusteringTable$addFootnote(message)
  }

  if(type=="densitybased"){
    if(clusterResult[["zeroMark"]] == 1)
      clusteringTable$addFootnote(gettext("Your cluster model contains 0 clusters and only Noisepoints, we advise to change the Eps and MinPts parameters."), colNames = "clusters")
    if(clusterResult[["oneMark"]] == 1)
      clusteringTable$addFootnote(gettext("Your cluster model contains 1 cluster and 0 Noisepoints. You could change the Eps and MinPts parameters."), colNames = "clusters")
  }

  if(!options[["scaleEqualSD"]])
    clusteringTable$addFootnote(gettext("The variables in the model are <b>unstandardized</b>."))

  row <- data.frame(clusters = clusterResult[['clusters']], measure = clusterResult[['BSS']]/clusterResult[['TSS']], aic = round(clusterResult[['AIC']], 2),
                    bic = round(clusterResult[['BIC']], 2), Silh = round(clusterResult[['Silh_score']], 2), n = clusterResult[["N"]])
  clusteringTable$addRows(row)
}

.clusterInformationTable <- function(options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["clusterInfoTable"]]) || !options[["tableClusterInformation"]]) return()

    clusterInfoTable                        <- createJaspTable(gettext("Cluster Information"))
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt", "noOfIterations",
                                        "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette", "optimizationCriterion",
                                        "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS", "minPts", "eps",
                                        "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m", "linkage", "distance", "noOfTrees", "maxTrees"))
  clusterInfoTable$position               <- position
  clusterInfoTable$transpose              <- TRUE

  clusterInfoTable$addColumnInfo(name = 'cluster', title = gettext('Cluster'), type = 'integer')
  clusterInfoTable$addColumnInfo(name = 'size', title = gettext('Size'), type = 'integer')
  clusterInfoTable$addColumnInfo(name = 'percentage', title = gettext('Explained proportion within-cluster heterogeneity'), type = 'number')
  if(options[["tableClusterInfoWSS"]])
    clusterInfoTable$addColumnInfo(name = 'withinss', title = gettext('Within sum of squares'), type = 'number')
  if(options[["tableClusterInfoSilhouette"]])
    clusterInfoTable$addColumnInfo(name = 'silh_scores', title = gettext('Silhouette score'), type = 'number')

  jaspResults[["clusterInfoTable"]]       <- clusterInfoTable

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(type == "kmeans" || type == "cmeans"){
    if(options[['tableClusterInfoCentroids']]){
      for( i in 1:length(options[["predictors"]])){
          clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = gettextf('Centroid %s', options[["predictors"]][i]), type = 'number', format = 'dp:3')
      }
    }
  }

  size <- clusterResult[["size"]]
  cluster <- 1:clusterResult[["clusters"]]
  withinss <- clusterResult[["WSS"]]
  silh_scores <- clusterResult[['silh_scores']]

  if(type == "densitybased"){
    if(sum(size) == clusterResult[["noisePoints"]]) {
      cluster <- gettext("Noisepoints")
      withinss <- 0
    } else if(clusterResult[["noisePoints"]] > 0) {
      cluster <- c(gettext("Noisepoints"), 1:(clusterResult[["clusters"]]))
      withinss <- c(0, withinss)
      silh_scores[1] <- 0
    }
  }

  row <- data.frame(cluster = cluster, 
                    size = size, 
                    percentage = withinss / sum(withinss))

  if(options[["tableClusterInfoWSS"]])
    row <- cbind(row, withinss = withinss)
  if(options[["tableClusterInfoSilhouette"]])
    row <- cbind(row, silh_scores = silh_scores)

  if(type == "kmeans" || type == "cmeans"){
    if(options[['tableClusterInfoCentroids']]){
        for( i in 1:length(options[["predictors"]])){
            row <- cbind(row, "tmp" = clusterResult[['centroids']][ ,i])
            colnames(row)[length(colnames(row))] <- paste0("centroid", i)
        }
    }
  }

  clusterInfoTable$addRows(row)

  if(options[['tableClusterInfoBetweenSumSquares']]){
      message <- gettextf('The Between Sum of Squares of the %1$s cluster model is %2$s', clusterResult[["clusters"]], round(clusterResult[['BSS']], 2))
      clusterInfoTable$addFootnote(message)
  }
  if(options[['tableClusterInfoTotalSumSquares']]){
      message <- gettextf('The Total Sum of Squares of the %1$s cluster model is %2$s', clusterResult[["clusters"]], round(clusterResult[['TSS']], 2))
      clusterInfoTable$addFootnote(message)
  }
}

.clusterEvaluationMetrics <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["clusterEvaluationMetrics"]]) || !options[["clusterEvaluationMetrics"]]) return()

  clusterEvaluationMetrics                        <- createJaspTable(gettext("Evaluation Metrics"))
  clusterEvaluationMetrics$dependOn(options =c("clusterEvaluationMetrics","predictors", "modelOpt", "noOfIterations",
                                      "noOfClusters","noOfRandomSets", "optimizationCriterion", "scaleEqualSD", "minPts", "eps",
                                      "maxClusters", "m", "linkage", "distance", "noOfTrees", "maxTrees"))
  clusterEvaluationMetrics$position               <- position

  clusterEvaluationMetrics$addColumnInfo(name = 'metric', title = '', type = 'string')
  clusterEvaluationMetrics$addColumnInfo(name = 'value', title = gettext('Value'), type = 'number')

  jaspResults[["clusterEvaluationMetrics"]]       <- clusterEvaluationMetrics

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object
  clustering <- clusterResult[["pred.values"]]

  distance <- dist(dataset)
  metrics <- fpc::cluster.stats(distance, clustering, silhouette = FALSE, sepindex = FALSE, sepwithnoise = FALSE)

  clusterEvaluationMetrics[["metric"]] <- c(gettext("Maximum diameter"), 
                                            gettext("Minimum separation"), 
                                            gettextf("Pearson's %s", "\u03B3"), 
                                            gettext("Dunn index"), 
                                            gettext("Entropy"), 
                                            gettext("Calinski-Harabasz index"))

  if(length(unique(clustering)) != 1){

    clusterEvaluationMetrics[["value"]] <- c(metrics[["max.diameter"]], metrics[["min.separation"]], metrics[["pearsongamma"]], metrics[["dunn"]], metrics[["entropy"]], metrics[["ch"]])
    clusterEvaluationMetrics$addFootnote(gettext("All metrics are based on the <i>euclidean</i> distance."))

  } else {

    clusterEvaluationMetrics$addFootnote(gettext("Evaluation metrics cannot be computed when there is only 1 cluster."))

  }

}

.tsneClusterPlot <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["plot2dCluster"]]) || !options[["plot2dCluster"]]) return()

  clusterPlot <- createJaspPlot(plot = NULL, title = gettext("t-SNE Cluster Plot"), width = 500, height = 300)
  clusterPlot$position <- position
  clusterPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "labels", "noOfTrees", "maxTrees", "legend", "optimizationCriterion"))
  jaspResults[["plot2dCluster"]] <- clusterPlot

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(options[["seedBox"]]) set.seed(options[["seed"]])

  startProgressbar(2)
  progressbarTick()

  unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
  if(is.null(jaspResults[["tsneOutput"]])){
    tsne_out <- Rtsne::Rtsne(as.matrix(dataset), perplexity = nrow(dataset) / 4, check_duplicates = FALSE)
    jaspResults[["tsneOutput"]] <- createJaspState(tsne_out)
    jaspResults[["tsneOutput"]]$dependOn(options = c("predictors", "seedBox", "seed"))
  } else {
    tsne_out <- jaspResults[["tsneOutput"]]$object
  }

  if(type == "kmeans" || type == "cmeans"){
    fit <- base::switch(type,
                        "kmeans" = kmeans(dataset, centers = clusterResult[["clusters"]], iter.max = options[['noOfIterations']], nstart = options[['noOfRandomSets']], algorithm = options[['algorithm']]),
                        "cmeans" = e1071::cmeans(dataset, centers = clusterResult[["clusters"]], iter.max = options[['noOfIterations']], m = options[['m']]))
    pred.values <- fit$cluster
    colSize <- clusterResult[["clusters"]]
  } else if(type == "hierarchical"){
      if (options[["distance"]] == "Pearson correlation") {
        hfit <- cutree(hclust(as.dist(1-cor(t(dataset), method="pearson")), method = options[["linkage"]]), k = clusterResult[['clusters']])
      } else {
        hfit <- cutree(hclust(dist(dataset), method = options[["linkage"]]), k = clusterResult[['clusters']])
      }
      pred.values <- hfit
      colSize <- clusterResult[["clusters"]]
  } else if (type == "densitybased"){
      if (options[["distance"]] == "Correlated densities") {
        fit <- dbscan::dbscan(as.dist(1-cor(t(dataset), method = "pearson")), eps = options[['eps']], minPts = options[['minPts']])
      } else {
        fit <- dbscan::dbscan(dataset, eps = options[['eps']], minPts = options[['minPts']])
      }
      pred.values <- fit$cluster
      colSize <- clusterResult[["clusters"]] + 1
  } else if(type == "randomForest"){
    fit <- randomForest::randomForest(x = dataset, y = NULL, ntree = options[["noOfTrees"]],
                                      proximity = TRUE, oob.prox = TRUE)
    hrfit <- hclust(as.dist(1 - fit$proximity), method = "ward.D2")
    pred.values <- cutree(hrfit, k = clusterResult[["clusters"]])
    colSize <- clusterResult[["clusters"]]
  }

  clusterAssignment <- factor(pred.values, levels = sort(unique(pred.values), decreasing = FALSE))
  if(type=="densitybased")
    levels(clusterAssignment)[levels(clusterAssignment)=="0"] <- gettext("Noisepoint")
  tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], Cluster = clusterAssignment)
  p <- ggplot2::ggplot(tsne_plot) +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y, fill = Cluster), size = 4, stroke = 1, shape = 21, color = "black") +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL) +
        ggplot2::scale_fill_manual(values = colorspace::qualitative_hcl(n = colSize))
  if(options[["legend"]]){
    p <- JASPgraphs::themeJasp(p, legend.position = "right")
  } else {
    p <- JASPgraphs::themeJasp(p)
  }
  p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  if(options[["labels"]])
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label=rownames(dataset), x = x, y = y), hjust=-1, vjust=1, data = tsne_plot) # of (rownames(data))

  progressbarTick()
  clusterPlot$plotObject <- p

}

.elbowCurvePlot <- function(dataset, options, jaspResults, ready, position){

if(!is.null(jaspResults[["optimPlot"]]) || !options[["withinssPlot"]] || options[["modelOpt"]] == "validationManual") return()

  optimPlot <- createJaspPlot(plot = NULL, title = gettext("Elbow Method Plot"), width = 600, height = 400)
  optimPlot$position <- position
  optimPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "withinssPlot", "noOfTrees", "maxTrees", "optimizationCriterion"))
  jaspResults[["optimPlot"]] <- optimPlot

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  wss <- clusterResult[['wssStore']]
  aic <- clusterResult[['aicStore']]
  bic <- clusterResult[['bicStore']]

  values <- c(wss, aic, bic)
  type <- rep(c(gettext("WSS"), gettext("AIC"), gettext("BIC")), each = length(wss))

  requiredPoint <- base::switch(options[["optimizationCriterion"]],
                                  "validationAIC" = gettext("AIC"),
                                  "validationBIC" = gettext("BIC"),
                                  "validationSilh" = "")

  d <- data.frame(x = rep(2:options[["maxClusters"]], 3), y = values, type = type)

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  yVals <- values[type == requiredPoint]
  pointData <- data.frame(x = clusterResult[['clusters']],
                          y = yVals[clusterResult[['clusters']] - 1],
                          type = requiredPoint)

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type)) +
        JASPgraphs::geom_line() +
        ggplot2::scale_x_continuous(name = gettext("Number of Clusters"), breaks = xBreaks, labels = xBreaks) +
        ggplot2::scale_y_continuous(name = "", breaks = yBreaks, labels = yBreaks) +
        ggplot2::scale_linetype_manual(values = c(3, 2, 1)) +
        ggplot2::labs(linetype = "")
  
  if(options[["optimizationCriterion"]] != "validationSilh"){
    p <- p + JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, linetype = type, fill = "red")) +
              ggplot2::scale_fill_manual(labels = gettextf("Lowest %s value", requiredPoint), values = "red") +
              ggplot2::labs(fill = "") 
  }
  
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  optimPlot$plotObject <- p

}

.clusteringAddClustersToData <- function(dataset, options, jaspResults, ready){
  if(!ready || !options[["addClusters"]] || options[["clusterColumn"]] == "")  return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(is.null(jaspResults[["clusterColumn"]])){
    predictions <- clusterResult[["pred.values"]]
    clusterColumn <- rep(NA, max(as.numeric(rownames(dataset))))
    clusterColumn[as.numeric(rownames(dataset))] <- predictions
    jaspResults[["clusterColumn"]] <- createJaspColumn(columnName=options[["clusterColumn"]])
    jaspResults[["clusterColumn"]]$dependOn(options = c("clusterColumn", "predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed",
                                                        "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "maxTrees", "optimizationCriterion"))
    jaspResults[["clusterColumn"]]$setNominal(clusterColumn)
  }
}

.clusterMeansTable <- function(dataset, options, jaspResults, ready, position){

  if(!is.null(jaspResults[["clusterMeansTable"]]) || !options[["tableClusterMeans"]]) return()

  clusterMeansTable <- createJaspTable(gettext("Cluster Means"))
  clusterMeansTable$dependOn(options = c("tableClusterMeans","predictors", "modelOpt", "noOfIterations",
                                      "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette", "optimizationCriterion",
                                      "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS", "minPts", "eps",
                                      "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m", "linkage", "distance", "noOfTrees", "maxTrees"))
  clusterMeansTable$position               <- position

  jaspResults[["clusterMeansTable"]] <- clusterMeansTable

  if(!ready) return()

  clusterMeansTable$addColumnInfo(name = "cluster", title = "", type = 'number')
  if(options[["predictors"]] != ""){
    for(i in 1:length(unlist(options[["predictors"]]))){
      columnName <- as.character(options[["predictors"]][i])
      clusterMeansTable$addColumnInfo(name = columnName, title = columnName, type = 'number')
    }
  }

  clusterResult <- jaspResults[["clusterResult"]]$object

  clusters <- as.factor(clusterResult[["pred.values"]])
  clusterLevels <- as.numeric(levels(clusters))
  clusterTitles <- gettextf("Cluster %s", clusterLevels)
  clusterMeans <- NULL
  
  for(i in clusterLevels){
    clusterSubset <- subset(dataset, clusters == i)
    clusterMeans <- rbind(clusterMeans, colMeans(clusterSubset))
  }

  clusterMeans <- cbind(cluster = clusterTitles, data.frame(clusterMeans))
  colnames(clusterMeans) <- c("cluster", as.character(options[["predictors"]]))

  clusterMeansTable$setData(clusterMeans)
}

.clusterDensitiesPlot <- function(dataset, options, jaspResults, ready, position){

  if (!is.null(jaspResults[["clusterDensities"]]) || !options[["plotClusterDensities"]]) return()

	clusterDensities <- createJaspContainer(gettext("Cluster Density Plots"))
  clusterDensities$dependOn(options = c("plotClusterDensities","predictors", "modelOpt", "noOfIterations",
                                      "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette", "optimizationCriterion",
                                      "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS", "minPts", "eps",
                                      "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m", "linkage", "distance", "noOfTrees", "maxTrees"))
  clusterDensities$position <- position

  jaspResults[["clusterDensities"]] <- clusterDensities

	if (!ready)
		return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  for (variable in unlist(options[["predictors"]])) {

    clusters <- as.factor(clusterResult[["pred.values"]])
    colorPalette <- colorspace::qualitative_hcl(n = length(unique(clusters)))

    xBreaks <- JASPgraphs::getPrettyAxisBreaks(dataset[[.v(variable)]], min.n = 4)

    plotData <- data.frame(Cluster = clusters, 
                           value = dataset[[.v(variable)]])

    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = value, fill = Cluster)) +
          ggplot2::geom_density(color = "transparent", alpha = 0.6) +
          ggplot2::ylab(gettext("Density")) +
          ggplot2::scale_x_continuous(name = variable, breaks = xBreaks, labels = xBreaks, limits = range(xBreaks))
    p <- JASPgraphs::themeJasp(p, legend.position = "right") + 
          ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), 
                         axis.text.y = ggplot2::element_blank())
    
    clusterDensities[[variable]] <- createJaspPlot(plot = p, title = variable, height = 300, width = 500)
    clusterDensities[[variable]]$dependOn(optionContainsValue=list("predictors" = variable))
  }
}

.clusterMeansPlot <- function(dataset, options, jaspResults, ready, position){

  if (!is.null(jaspResults[["clusterMeans"]]) || !options[["plotClusterMeans"]]) return()

	clusterMeans <- createJaspContainer(gettext("Cluster Mean Plots"))
  clusterMeans$dependOn(options = c("plotClusterMeans", "showBars", "oneFigure", "predictors", "modelOpt", "noOfIterations",
                                      "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette", "optimizationCriterion",
                                      "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS", "minPts", "eps",
                                      "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m", "linkage", "distance", "noOfTrees", "maxTrees"))
  clusterMeans$position <- position

  jaspResults[["clusterMeans"]] <- clusterMeans

	if (!ready)
		return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(options[["oneFigure"]]){

    clusters <- as.factor(clusterResult[["pred.values"]])
    xBreaks <- c(1, (as.numeric(levels(clusters)) + 1) * length(options[["predictors"]]))
    
    clusterMeansData <- aggregate(dataset, list(clusters), mean)
    clusterSdData <- aggregate(dataset, list(clusters), sd)
    clusterLengthData <- aggregate(dataset, list(clusters), length)
    
    clusterCoord <- rep(clusterMeansData[, 1], length(options[["predictors"]]))

    xCoord <- numeric()
    for(i in 1:length(options[["predictors"]])){
      if(i == 1){
        xCoord <- c(xCoord, (length(xCoord) + 1):(length(xCoord) + length(clusterMeansData[, 1]))) 
      } else {
        xCoord <- c(xCoord, (max(xCoord) + 3):(max(xCoord) + 2 + length(clusterMeansData[, 1])))
      }
    }

    values <- as.numeric(unlist(clusterMeansData[, -1]))
    lowerValues <- as.numeric(unlist(clusterMeansData[, -1])) - qnorm(0.95) * as.numeric(unlist(clusterSdData[, -1])) / sqrt(as.numeric(unlist(clusterLengthData[, -1])))
    upperValues <- as.numeric(unlist(clusterMeansData[, -1])) + qnorm(0.95) * as.numeric(unlist(clusterSdData[, -1])) / sqrt(as.numeric(unlist(clusterLengthData[, -1])))

    plotData <- data.frame(xCoord = xCoord,
                           Cluster = clusterCoord, 
                           value = values,
                           lower = lowerValues,
                           upper = upperValues)
    
    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, unlist(plotData[complete.cases(plotData), -c(1, 2)])), min.n = 4)
    
    xBreaks <- (1:length(options[["predictors"]])) * (length(levels(clusters)) + 2)
    xBreaks <- xBreaks - (length(levels(clusters)) + 2)
    xBreaks <- xBreaks + 0.5 * length(levels(clusters))
    
    xLabels <- options[["predictors"]]
    
    plotData <- plotData[complete.cases(plotData), ]

    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = xCoord, y = value, fill = Cluster))
    
    if(options[["showBars"]]){
      p <- p + ggplot2::geom_bar(color = "black", stat = "identity") + 
        ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin=lower, ymax=upper), width = 0.2, size = 1)
    } else {
      p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = max(plotData[["xCoord"]]), y = 0, yend = 0), linetype = 2) +
                ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = xCoord, ymin=lower, ymax=upper), width = 0.2, size = 1) + 
                JASPgraphs::geom_point(color = "black")
    }
    
    p <- p + ggplot2::scale_x_continuous(name = "", breaks = xBreaks, labels = xLabels) +
      ggplot2::scale_y_continuous(name = gettext("Value"), breaks = yBreaks, labels = yBreaks, limits = range(yBreaks))
    p <- JASPgraphs::themeJasp(p, legend.position = "right", sides = "l") + 
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_text(angle = 20))
    
    clusterMeans[["oneFigure"]] <- createJaspPlot(plot = p, title = gettext("All predictors"), height = 400, width = 200 * length(options[["predictors"]]))


  } else {

    for (variable in unlist(options[["predictors"]])){

      clusters <- as.factor(clusterResult[["pred.values"]])
      xBreaks <- as.numeric(levels(clusters))
      
      clusterMeansData <- aggregate(dataset[[.v(variable)]], list(clusters), mean)
      clusterSdData <- aggregate(dataset[[.v(variable)]], list(clusters), sd)
      clusterLengthData <- aggregate(dataset[[.v(variable)]], list(clusters), length)

      plotData <- data.frame(Cluster = clusterMeansData[, 1], 
                            value = clusterMeansData[, 2],
                            lower = clusterMeansData[, 2] - qnorm(0.95) * clusterSdData[, 2] / sqrt(clusterLengthData[, 2]),
                            upper = clusterMeansData[, 2] + qnorm(0.95) * clusterSdData[, 2] / sqrt(clusterLengthData[, 2]))
      
      plotData <- plotData[complete.cases(plotData), ]

      yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, unlist(plotData[, -1])), min.n = 4)

      p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Cluster, y = value, fill = Cluster))

      if(options[["showBars"]]){
        p <- p + ggplot2::geom_bar(color = "black", stat = "identity") + 
                  ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin=lower, ymax=upper), width = 0.2, size = 1)
      } else {
        p <- p + ggplot2::geom_errorbar(data = plotData, ggplot2::aes(x = Cluster, ymin=lower, ymax=upper), width = 0.2, size = 1) + 
                  JASPgraphs::geom_point(color = "black")
      }

      p <- p + ggplot2::scale_x_discrete(name = gettext("Cluster"), breaks = xBreaks, labels = xBreaks) +
                ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, labels = yBreaks, limits = range(yBreaks))
      p <- JASPgraphs::themeJasp(p, sides = "l") + 
            ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
      
      clusterMeans[[variable]] <- createJaspPlot(plot = p, title = variable, height = 300, width = 500)
      clusterMeans[[variable]]$dependOn(optionContainsValue=list("predictors" = variable))
    }

  }
}
