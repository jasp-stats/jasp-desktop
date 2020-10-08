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
  .errorHandlingClusteringAnalyses(dataset, options, type = "densitybased")
  
  # Check if analysis is ready to run
  ready  <- .clusterAnalysesReady(options)

  # Compute results and create the model summary table
  .clusteringTable(dataset, options, jaspResults, ready, position = 1, type = "densitybased")

  # If the user wants to add the clusters to the data set
  .clusteringAddClustersToData(dataset, options, jaspResults, ready)
  
  # Create the cluster information table
  .clusterInformationTable(options, jaspResults, ready, position = 2, type = "densitybased")

  # Create the cluster means table
  .clusterMeansTable(dataset, options, jaspResults, ready, position = 3)

  # Create the cluster evaluation metrics table
  .clusterEvaluationMetrics(dataset, options, jaspResults, ready, position = 4)
  
  # Create the k-distance plot
  .densityBasedClusteringKdistPlot(dataset, options, jaspResults, ready, position = 5)

  # Create the cluster means plot
  .clusterMeansPlot(dataset, options, jaspResults, ready, position = 6)

  # Create the cluster densities plot
  .clusterDensitiesPlot(dataset, options, jaspResults, ready, position = 7)

  # Create the cluster plot
  .tsneClusterPlot(dataset, options, jaspResults, ready, position = 8, type = "densitybased")

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
    silhouettes <- list("avg.width" = 0, "clus.avg.widths" = rep(0, max(1, clusters)))
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

  kdistPlot <- createJaspPlot(plot = NULL, title = gettext("K-Distance Plot"), width = 500, height = 300)
  kdistPlot$position <- position
  kdistPlot$dependOn(options = c("predictors", "eps", "minPts", "modelOpt", "seed", "scaleEqualSD", "ready", "k-distplot", "distance"))
  jaspResults[["kdistPlot"]] <- kdistPlot

  if(!ready) return()

  unique.rows <- which(!duplicated(dataset[, .v(options[["predictors"]])]))
  data <- dataset[unique.rows, .v(options[["predictors"]])]
  if(options[["distance"]] == "Correlated densities"){
    knnDist <- dbscan::kNNdist(as.dist(1-cor(t(as.data.frame(data)), method = "pearson")), k = options[['minPts']])
  } else {
    knnDist <- dbscan::kNNdist(data, k = options[['minPts']])
  }

  knnValues <- seq(from = 1, to = length(knnDist), by = 1)
  knnDistances <- sort(knnDist)

  d <- data.frame(x = knnValues, y = knnDistances)

  xBreaks <- JASPgraphs::getPrettyAxisBreaks(d$x, min.n = 4)
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, d$y, options[["eps"]]), min.n = 4)

  yKnee <- try(findCutoff(knnValues, knnDistances, method = "curvature")[["y"]])
  if (inherits(yKnee, "try-error")) # this can cause a stackoverflow, in which case we abort and don't add it
    suggestedLine <- NULL
  else
    suggestedLine <- data.frame(xstart = xBreaks[1], xend = xBreaks[length(xBreaks)], ystart = yKnee, yend = yKnee)
  
  lineData <- data.frame(xstart = xBreaks[1], xend = xBreaks[length(xBreaks)], ystart = options[["eps"]], yend = options[["eps"]])
 
  p <-  ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) + 
        ggplot2::scale_x_continuous(name = gettext("Points sorted by distance"), breaks = xBreaks) +
        ggplot2::scale_y_continuous(name = gettextf('%s-nearest neighbors \ndistance', options[['minPts']]), breaks = yBreaks)
  
  if (!is.null(suggestedLine)) {
        p <-  p + ggplot2::geom_segment(ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), data = suggestedLine, linetype = 2, color = "darkred") +
                  ggrepel::geom_text_repel(data = suggestedLine, ggplot2::aes(label= gettextf("Maximum curvature = %s", round(yend, 2)), x = xstart, y = yend), hjust = 0, vjust = -0.5, color = "darkred")
  }
  
  p <-  p + ggplot2::geom_segment(ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend), data = lineData, linetype = 2, color = "darkgray") +
        JASPgraphs::geom_line()
  p <-  JASPgraphs::themeJasp(p)

  kdistPlot$plotObject <- p
}

# this function is a direct replicate of KneeArrower::findCutoff(), with added stackoverflow protection
# the algorithm should probably be rewritten in a way that does not require recursion
findCutoff <- function (x, y, method = "first", frac.of.steepest.slope = 0.5) {
  stack <- Cstack_info()[names(Cstack_info()) == "eval_depth"] * 6 # each run adds 6 to the stack
  if (getOption("expressions") <= (stack + 6)) 
    stop(gettext("End of recursion reached without converging"))

  is.invalid <- function(x) {
      any((!is.numeric(x)) | is.infinite(x))
  }
  if (is.invalid(x) || is.invalid(y)) {
      stop(gettext("x and y must be numeric and finite. Missing values not allowed."))
  }
  if (length(x) != length(y)) {
      stop(gettext("x and y must be of equal length."))
  }
  
  new.x <- seq(from = min(x), to = max(x), length.out = length(x))
  sp <- smooth.spline(x, y)
  new.y <- predict(sp, new.x)$y
  largest.odd.num.lte <- function(x) {
      x.int <- floor(x)
      if (x.int%%2 == 0) {
          x.int - 1
      }
      else {
          x.int
      }
  }
  smoothen <- function(y, p = p, filt.length = NULL, ...) {
      ts <- (max(new.x) - min(new.x))/length(new.x)
      p <- 3
      if (is.null(filt.length)) {
          filt.length <- min(largest.odd.num.lte(length(new.x)), 
              7)
      }
      if (filt.length <= p) {
          stop(gettext("Need more points to find cutoff."))
      }
      signal::sgolayfilt(y, p = p, n = filt.length, ts = ts, 
          ...)
  }
  first.deriv <- smoothen(new.y, m = 1)
  second.deriv <- smoothen(new.y, m = 2)
  pick.sign <- function(x) {
      most.extreme <- which(abs(x) == max(abs(x), na.rm = TRUE))[1]
      sign(x[most.extreme])
  }
  first.deriv.sign <- pick.sign(first.deriv)
  second.deriv.sign <- pick.sign(second.deriv)
  x.sign <- 1
  y.sign <- 1
  if ((first.deriv.sign == -1) && (second.deriv.sign == -1)) {
      x.sign <- -1
  }
  else if ((first.deriv.sign == -1) && (second.deriv.sign == 
      1)) {
      y.sign <- -1
  }
  else if ((first.deriv.sign == 1) && (second.deriv.sign == 
      1)) {
      x.sign <- -1
      y.sign <- -1
  }
  if ((x.sign == -1) || (y.sign == -1)) {
      results <- findCutoff(x.sign * x, y.sign * y, method = method, 
          frac.of.steepest.slope = frac.of.steepest.slope)
      return(list(x = x.sign * results$x, y = y.sign * results$y))
  }
  cutoff.x <- NA
  if (method == "first") {
      if (is.invalid(frac.of.steepest.slope)) {
          stop(gettext("Need to specify fraction of maximum slope."))
      }
      if (frac.of.steepest.slope <= 0 || frac.of.steepest.slope > 
          1) {
          stop(gettext("Fraction of maximum slope must be positive and be less than or equal to 1."))
      }
      slope.cutoff <- frac.of.steepest.slope * max(first.deriv)
      cutoff.x <- findInverse(new.x, first.deriv, slope.cutoff)
  }
  else if (method == "curvature") {
      curvature <- abs(second.deriv)/(1 + first.deriv^2)^(3/2)
      cutoff.x <- findInverse(new.x, curvature, max(curvature))
  }
  else {
      stop(gettext("Method must be either 'first' or 'curvature'."))
  }
  if (is.na(cutoff.x)) {
      warning(gettext("Cutoff point is beyond range. Returning NA."))
      list(x = NA, y = NA)
  }
  else {
      approx(new.x, new.y, cutoff.x)
  }
}

findInverse <- function (x, y, y0) 
{
    if (y0 < min(y) | max(y) < y0) {
        return(NA)
    }
    else {
        f <- approxfun(x, y, rule = 1)
        return(optimize(function(x) abs(f(x) - y0), range(x))$minimum)
    }
}