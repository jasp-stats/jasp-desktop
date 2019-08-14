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
  if (is.null(dataset))
    dataset <- .readDataSetToEnd(columns.as.numeric = predictors, exclude.na.listwise = predictors)

  if(options[["scaleEqualSD"]] && length(unlist(options[["predictors"]])) > 0)
    dataset <- .scaleNumericData(dataset)

  return(dataset)
}

.errorHandlingClusteringAnalyses <- function(dataset, options){
  predictors <- unlist(options$predictors)
  
  if(length(predictors[predictors != ""]) > 0L)
    .hasErrors(dataset, perform, type = c('infinity', 'observations'), all.target = predictors, 
               observations.amount = "< 2", exitAnalysisIfErrors = TRUE)
  return()
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
                                                      "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "optimizationCriterion"))
  }
}

.clusteringTable <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["clusteringTable"]])) return() #The options for this table didn't change so we don't need to rebuild it

  title <- base::switch(type,
                        "kmeans" = "K-Means Clustering",
                        "cmeans" = "Fuzzy C-Means Clustering",
                        "hierarchical" = "Hierarchical Clustering",
                        "densitybased" = "Density-Based Clustering",
                        "randomForest" = "Random Forest Clustering")

  clusteringTable                       <- createJaspTable(title)
  clusteringTable$position <- position
  clusteringTable$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", "optimizationCriterion",
                                                      "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees"))

  clusteringTable$addColumnInfo(name = 'clusters', title = 'Clusters', type = 'integer')
  clusteringTable$addColumnInfo(name = 'n', title = 'N', type = 'integer')
  clusteringTable$addColumnInfo(name = 'measure', title = 'R\u00B2', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'aic', title = 'AIC', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'bic', title = 'BIC', type = 'number', format = 'dp:2')
  clusteringTable$addColumnInfo(name = 'Silh', title = 'Silhouette', type = 'number', format = 'dp:2')

  clusteringTable$addCitation("Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(1), 100-108.")

  if(!ready) clusteringTable$addFootnote(message="Please provide at least 2 variables.", symbol="<i>Note.</i>")

  jaspResults[["clusteringTable"]]      <- clusteringTable

  if(!ready) return()

  .clustering(dataset, options, jaspResults, ready, type = type)

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(options[["modelOpt"]] != "validationManual"){
    criterion <- base::switch(options[["optimizationCriterion"]], "validationAIC" = "AIC", "validationBIC" = "BIC", "validationSilh" = "silhouette")
    clusteringTable$addFootnote(message = paste0("The model is optimized with respect to the <i>", criterion, "</i> value."), symbol="<i>Note.</i>")
  }
    
  if(clusterResult[["clusters"]] == options[["maxClusters"]] && options[["modelOpt"]] != "validationManual"){
    message <- "The optimum number of clusters is the maximum number of clusters. You might want to adjust the range of optimization."
    clusteringTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }

  if(type=="densitybased"){
    if(clusterResult[["zeroMark"]] == 1)
      clusteringTable$addFootnote(message = "Your cluster model contains 0 clusters and only Noisepoints, we advise to change the Eps and MinPts parameters.", symbol="<i>Note.</i>") 
    if(clusterResult[["oneMark"]] == 1)
      clusteringTable$addFootnote(message = "Your cluster model contains 1 cluster and 0 Noisepoints. You could change the Eps and MinPts parameters.", symbol="<i>Note.</i>") 
  }

  if(!options[["scaleEqualSD"]])
    clusteringTable$addFootnote(message = "The variables in the model are <b>unstandardized</b>.", symbol="<i>Note.</i>")
  
  row <- data.frame(clusters = clusterResult[['clusters']], measure = clusterResult[['BSS']]/clusterResult[['TSS']], aic = round(clusterResult[['AIC']], 2),
                    bic = round(clusterResult[['BIC']], 2), Silh = round(clusterResult[['Silh_score']], 2), n = clusterResult[["N"]])
  clusteringTable$addRows(row)
}

.clusterInformationTable <- function(options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["clusterInfoTable"]]) || !options[["tableClusterInformation"]]) return()

    clusterInfoTable                        <- createJaspTable("Cluster Information")
    clusterInfoTable$dependOn(options =c("tableClusterInformation","predictors", "modelOpt", "noOfIterations",
                                        "noOfClusters","noOfRandomSets", "tableClusterInfoSize", "tableClusterInfoSilhouette", "optimizationCriterion",
                                        "tableClusterInfoSumSquares", "tableClusterInfoCentroids", "scaleEqualSD", "tableClusterInfoWSS", "minPts", "eps",
                                        "tableClusterInfoBetweenSumSquares", "tableClusterInfoTotalSumSquares", "maxClusters", "m", "linkage", "distance", "noOfTrees"))
  clusterInfoTable$position               <- position
  clusterInfoTable$transpose              <- TRUE

  clusterInfoTable$addColumnInfo(name = 'cluster', title = 'Cluster', type = 'integer')
  clusterInfoTable$addColumnInfo(name = 'size', title = 'Size', type = 'integer')
  if(options[["tableClusterInfoWSS"]])
    clusterInfoTable$addColumnInfo(name = 'withinss', title = 'Within sum of squares', type = 'number', format = 'dp:2')
  if(options[["tableClusterInfoSilhouette"]])
    clusterInfoTable$addColumnInfo(name = 'silh_scores', title = 'Silhouette score', type = 'number', format = 'dp:2')

  jaspResults[["clusterInfoTable"]]       <- clusterInfoTable
    
  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(type == "kmeans" || type == "cmeans"){
    if(options[['tableClusterInfoCentroids']]){
      for( i in 1:length(options[["predictors"]])){
          clusterInfoTable$addColumnInfo(name = paste0('centroid', i), title = paste0('Centroid ', options[["predictors"]][i]), type = 'number', format = 'dp:3')
      }
    }
  }

  size <- clusterResult[["size"]]
  cluster <- 1:clusterResult[["clusters"]]
  withinss <- clusterResult[["WSS"]]
  silh_scores <- clusterResult[['silh_scores']]

  if(type == "densitybased"){
    if(sum(size) == clusterResult[["noisePoints"]]) {
      cluster <- "Noisepoints"
      withinss <- 0
    } else if(clusterResult[["noisePoints"]] > 0) {
      cluster <- c("Noisepoints", 1:(clusterResult[["clusters"]]))
      withinss <- c(0, withinss)
      silh_scores[1] <- 0
    }
  }

  row <- data.frame(cluster = cluster, size = size)
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
      message <- paste0('The Between Sum of Squares of the ', clusterResult[["clusters"]], ' cluster model is ', round(clusterResult[['BSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
  if(options[['tableClusterInfoTotalSumSquares']]){
      message <- paste0('The Total Sum of Squares of the ', clusterResult[["clusters"]], ' cluster model is ', round(clusterResult[['TSS']],2))
      clusterInfoTable$addFootnote(message=message, symbol="<i>Note.</i>")
  }
}

.tsneClusterPlot <- function(dataset, options, jaspResults, ready, position, type){

  if(!is.null(jaspResults[["plot2dCluster"]]) || !options[["plot2dCluster"]]) return()

  clusterPlot <- createJaspPlot(plot = NULL, title = "T-sne Cluster Plot", width = 500, height = 300)
  clusterPlot$position <- position
  clusterPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "labels", "noOfTrees", "legend", "optimizationCriterion"))
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

  clusterAssignment <- factor(pred.values)
  if(type=="densitybased")
    levels(clusterAssignment)[levels(clusterAssignment)=="0"] <- "Noisepoint"  
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

  optimPlot <- createJaspPlot(plot = NULL, title = "Elbow Method Plot", width = 500, height = 300)
  optimPlot$position <- position
  optimPlot$dependOn(options = c("predictors", "noOfClusters","noOfRandomSets", "algorithm", "eps", "minPts", "distance",
                                          "noOfIterations", "modelOpt", "ready", "seed", "plot2dCluster", "maxClusters", "scaleEqualSD", "seedBox",
                                          "linkage", "m", "withinssPlot", "noOfTrees", "optimizationCriterion"))
  jaspResults[["optimPlot"]] <- optimPlot

  if(!ready) return()

  clusterResult <- jaspResults[["clusterResult"]]$object  

  wss <- clusterResult[['wssStore']]
  aic <- clusterResult[['aicStore']]
  bic <- clusterResult[['bicStore']]

  values <- c(wss, aic, bic)
  type <- rep(c("Within Sum of Squares", "AIC", "BIC"), each = length(wss))

  requiredPoint <- base::switch(options[["optimizationCriterion"]], 
                                  "validationAIC" = "AIC", 
                                  "validationBIC" = "BIC", 
                                  "validationSilh" = "Within Sum of Squares")

  d <- data.frame(x = rep(2:options[["maxClusters"]], 3), y = values, type = type)
       
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  yVals <- values[type == requiredPoint]
  pointData <- data.frame(x = clusterResult[['clusters']], 
                          y = yVals[clusterResult[['clusters']] - 1],
                          type = requiredPoint)
   
  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y, linetype = type)) +
        JASPgraphs::geom_line() + 
        ggplot2::scale_x_continuous(name = "Number of Clusters", breaks = xBreaks, labels = xBreaks) +
        ggplot2::scale_y_continuous(name = "", breaks = yBreaks, labels = yBreaks) +
        ggplot2::labs(linetype = "") +
        ggplot2::scale_linetype_manual(values = c(3, 2, 1)) + 
        JASPgraphs::geom_point(data = pointData, ggplot2::aes(x = x, y = y, linetype = type), fill = "red")
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  optimPlot$plotObject <- p

}

.clusteringAddClustersToData <- function(options, jaspResults, ready){
  if(!ready || !options[["addClusters"]] || options[["clusterColumn"]] == "")  return()

  clusterResult <- jaspResults[["clusterResult"]]$object

  if(is.null(jaspResults[["clusterColumn"]])){
    clusterColumn <- clusterResult[["pred.values"]]
    jaspResults[["clusterColumn"]] <- createJaspColumn(columnName=options[["clusterColumn"]])
    jaspResults[["clusterColumn"]]$dependOn(options = c("clusterColumn", "predictors", "noOfClusters","noOfRandomSets", "noOfIterations", "algorithm", "modelOpt", "seed", 
                                                        "maxClusters", "seedBox", "scaleEqualSD", "m", "distance", "linkage", "eps", "minPts", "noOfTrees", "optimizationCriterion"))
    jaspResults[["clusterColumn"]]$setNominal(clusterColumn)
  }  
}