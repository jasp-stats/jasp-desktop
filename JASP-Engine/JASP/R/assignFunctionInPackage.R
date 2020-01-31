assignFunctionInPackage <- function(fun, name, package) {
  #'@title Overwrite functions inside R packages
  #' 
  #'@param fun    : function you want to replace
  #'@param name   : the name of the function in a package
  #'@param package: the name of the package
  #'  
  #' example usage:
  #' avoid a parallel backend from being registered which triggers a firewall message
  ns <- getNamespace(package)
  unlockBinding(name, ns)
  assign(name, fun, ns)
  lockBinding(name, ns)
}

# NOTE: let's make it a custom to start the new function names with "fake" followed by the name of the original 
# function. Also, let's put all "modified" versions of functions in this file. Note that thise file probably needs
# to be resolved completely for R syntax.

# mlRegressionBoosting ----
fakeGbmCrossValModelBuild <- function(cv.folds, cv.group, n.cores, i.train, x, y, offset, 
                                      distribution, w, var.monotone, n.trees, interaction.depth, 
                                      n.minobsinnode, shrinkage, bag.fraction, var.names, response.name, 
                                      group) {
  # the first two lines create a parallel backend and trigger a firewall message
  # cluster <- gbmCluster(n.cores)
  # on.exit(parallel::stopCluster(cluster))
  seeds <- as.integer(runif(cv.folds, -(2^31 - 1), 2^31))
  # parallel::parLapply(cl = NULL, X = 1:cv.folds, fun = gbmDoFold, 
  #                     i.train, x, y, offset, distribution, w, var.monotone, 
  #                     n.trees, interaction.depth, n.minobsinnode, shrinkage, 
  #                     bag.fraction, cv.group, var.names, response.name, group, 
  #                     seeds)
  
  # NOTE: gbm::gbmDoFold calls library(gbm, silent = TRUE) so we make another fake function
  fakeGbmDoFold <- function(X, i.train, x, y, offset, distribution, w, var.monotone,
                            n.trees, interaction.depth, n.minobsinnode, shrinkage, bag.fraction,
                            cv.group, var.names, response.name, group, s) {
    set.seed(s[[X]])
    i <- order(cv.group == X)
    x <- x[i.train, , drop = TRUE][i, , drop = FALSE]
    y <- y[i.train][i]
    offset <- offset[i.train][i]
    nTrain <- length(which(cv.group != X))
    group <- group[i.train][i]
    res <- gbm::gbm.fit(x = x, y = y, offset = offset, distribution = distribution, 
                        w = w, var.monotone = var.monotone, n.trees = n.trees, 
                        interaction.depth = interaction.depth, n.minobsinnode = n.minobsinnode, 
                        shrinkage = shrinkage, bag.fraction = bag.fraction, nTrain = nTrain, 
                        keep.data = FALSE, verbose = FALSE, response.name = response.name, 
                        group = group)
    res
  }
  lapply(X = 1:cv.folds, FUN = fakeGbmDoFold, i.train, x, y, offset, distribution, w, var.monotone, n.trees, 
         interaction.depth, n.minobsinnode, shrinkage, bag.fraction, cv.group, var.names, response.name, group, seeds)
}

fakeGbmCrossValErr <- function(cv.models, cv.folds, cv.group, nTrain, n.trees) {
  in.group <- tabulate(cv.group, nbins = cv.folds)
  cv.error <- vapply(1:cv.folds, function(index) {
    model <- cv.models[[index]]
    model$valid.error * in.group[[index]]
  }, double(n.trees))
  return(rowSums(as.matrix(cv.error))/nTrain)
}

# cowplot, used in flexplot (and other analyses that open pdf devices on Windows) ----
fakeGrDevicesPdf <- function(file = if(onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                             width, height, onefile, family, title, fonts, version,
                             paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
                             useDingbats, useKerning, fillOddEven, compress) { 
  args <- as.list(match.call())
  matchedIdx <- which(names(args) %in% c("file", "width", "height", "oneFile", "family", "bg", "pointsize"))
  matchedArgs <- args[matchedIdx]
  
  matchedArgs[["filename"]] <- matchedArgs[["file"]]
  matchedArgs[["file"]] <- NULL
  
  do.call(grDevices::cairo_pdf, matchedArgs, envir = parent.frame())
}
