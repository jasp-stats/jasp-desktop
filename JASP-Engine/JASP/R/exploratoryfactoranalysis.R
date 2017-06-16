#
# Copyright (C) 2013-2015 University of Amsterdam
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


.DotIfNULL <- function(x){

  # if (is.null(x) || any(is.na(x)) || !is.finite(x)){
  if (is.null(x) || is.na(x) || !is.finite(x)){
    return(".")
  } else {
    return(x)
  }
}


ExploratoryFactorAnalysis <- function(dataset = NULL, options, perform = "run",
                                      callback = function(...) list(status="ok"), state = NULL, ...) {
  

  ## call the common initialization function
init <- .initialize(dataset, options, perform)
  
  
  results <- init[["results"]]
  dataset <- init[["dataset"]]


  ## initialize result ## ----
  results[["title"]] = "Exploratory Factor Analysis"
  meta <- list(
    list(name="title", type="title"),
    list(name="factorLoadings", type = "table"),
    list(name="factorCorrelations", type = "table"),
    list(name="goodnessOfFit", type="table"),
    list(name="fitMeasures", type="table"),
    list(name="pathDiagram", type="image"),
    list(name="screePlot", type="image"))

  results[[".meta"]] = meta

  ## initialize state ## ----

  stateKey <- list(
    analysisResults = c(
      'rotationMethod',
      'orthogonalSelector',
      'obliqueSelector',
      'variables', 
      'factorMethod',
      'eigenValuesBox',
      'numberOfFactors'),
    pathDiagram = c(
      'rotationMethod',
      'orthogonalSelector',
      'obliqueSelector',
      'variables', 
      'factorMethod',
      'eigenValuesBox',
      'numberOfFactors',
      'highlightSlider', 
      'highlightText',
      # 'incl_pathDiagram', 
      'plotWidthPathDiagram',
      'plotHeightPathDiagram'),
    screePlot = c(
      'rotationMethod',
      'orthogonalSelector',
      'obliqueSelector',
      'variables', 
      'factorMethod',
      'eigenValuesBox',
      'numberOfFactors',
      # 'incl_screePlot',
      'plotWidthScreePlot',
      'plotHeightScreePlot')
    )
  keep <- NULL

  # # States:
  # newAnalysis <- TRUE
  # state <- .retrieveState()

  # if (!is.null(state) && !is.null(state[["analysisResults"]])) {  # is there state?
  
  # # if ( !is.null(state)) {  # is there state?
    
  #   # nVariable <- length(options$variables)
  #   diff <- .diff(options,state$options)
    
  #   if (is.list(diff) && !diff[['rotationMethod']] && !diff[['orthogonalSelector']] && !diff[['obliqueSelector']] && !diff[['variables']] && !diff[['factorMethod']] && 
  #       !diff[['eigenValuesBox']] && !diff[['numberOfFactors']]) {

  #     # old results can be used
  #     newAnalysis <- FALSE
  #   }
  # }

  if(is.null(state[["analysisResults"]])){

    state <- NULL
    analysisResults <- NULL

    if (perform == "run" && !is.null(dataset) && nrow(dataset) > 1){

      error0 <- .hasErrors(dataset=dataset, perform=perform, type=c("infinity", "variance"), exitAnalysisIfErrors=TRUE)

      customChecks <- list(
        function() {
          if (length(options$variables) > 0 && options$numberOfFactors > length(options$variables)) {
            return(paste0("Too many factors requested (", options$numberOfFactors, ") for the amount of included variables"))
          }
        },
        function(){
          if(length(options$variables) <= 1){
            return(paste0("Not enough variables (", length(options$variables), ") for analysis."))
          }
        },
        function(){
          if(is.null(dataset)){
            return(paste0("Dataset empty"))
          }else if(nrow(dataset) < 3){
            return(paste0("Not enough valid cases (", nrow(dataset),") for analysis"))
          }
        },
        # check whether all row variance == 0
        function(){
          if(!is.null(dataset) && nrow(dataset)>1){
            varianceZero <- 0
            for (i in 1:nrow(dataset)){
              if(sd(dataset[i,], na.rm = TRUE) == 0) varianceZero <- varianceZero + 1
            }
            if(varianceZero == nrow(dataset)){
              return("Data not valid: variance is zero in each row")
            }
          }
        },
        # check whether all variables correlates to each other
        function(){
          if(!is.null(dataset) && nrow(dataset)>1){
            allCorr <- 0
            nVar <- ncol(dataset)
            for (i in 1:(nVar-1)){
              for (j in (i+1):nVar){
                if(cor(dataset[,i],dataset[,j]) > 0.98) allCorr <- allCorr + 1
              }
            }
            if(allCorr == nVar*(nVar-1)/2){
              return("Data not valid: all variables correlate with each other")
            }
          }
        }
      )
    
      error <- .hasErrors(dataset=dataset, perform=perform, type=c("infinity", "variance"), custom=customChecks, exitAnalysisIfErrors=TRUE)
      analysisResults <- try(silent = FALSE, expr = {
        .estimateEFA(dataset, options, perform)
        })

    }


  }else{
    analysisResults <- state[["analysisResults"]]
  }


    # Output table
    # Create fit measures tables:
    results[["goodnessOfFit"]] <- .goodnessOfFit(analysisResults, options, perform)
    # Create factor correlation table:
    results[["factorLoadings"]] <- .getLoadings(analysisResults, dataset, options, perform, "efa")

    if(options$incl_correlations){
      results[["factorCorrelations"]] <- .getFactorCorrelations(analysisResults, options, perform, "efa")
    }

    if(options$incl_fitIndices){
      results[["fitMeasures"]] <- .fitMeasures(analysisResults, options, perform)
    }

  # Output Plot

  # Create path diagram:

  if(isTRUE(options$incl_pathDiagram)){
    p <- try(silent = FALSE, expr = {
      .pathDiagramEFA(analysisResults, options, perform, oldPlot = state[["pathDiagram"]])
     })

    if(isTryError(p)){
      errorMessage <- .extractErrorMessage(p)
      results[["pathDiagram"]][["error"]] <- list(error="badData", errorMessage=errorMessage)
    }else{
      results[["pathDiagram"]] <- p
      keep <- c(keep, results[["pathDiagram"]][["data"]])
    }
    
  }

  # Scree plot:
  if(isTRUE(options$incl_screePlot)){
    p <- try(silent = FALSE, expr = {
      .screePlot(dataset, options, perform, oldPlot = state[["screePlot"]],"efa")
     })

    if(isTryError(p)){
      errorMessage <- .extractErrorMessage(p)
      results[["screePlot"]][["error"]] <- list(error="badData", errorMessage=errorMessage)
    }else{
      results[["screePlot"]] <- p
      keep <- c(keep, results[["screePlot"]][["data"]])
    }
  }
  ## TEMP DEBUG THING:
  # save(dataset,results,init,options,perform,callback,...,file = "/Users/sachaepskamp/Dropbox/work/JASP/Rcodes/JASPinit.RData")

  
  # # Dummies:
  # status <- list(ready=TRUE, error=error)

  #save state
  state <- list(
    options = options,
    analysisResults = analysisResults,
    pathDiagram = results[["pathDiagram"]],
    screePlot = results[["screePlot"]]
  )

  attr(state, "key") <- stateKey

  if (perform == "run") {

    return(list(results = results, status = "complete", state = state, keep = keep))

  } else {

    return(list(results = results, status = "inited", state = state, keep = keep))
  }
}

.estimateNFactorEFA <- function(dataset, options, perform){

  # Number of factors:
  nVariable <- length(options$variables)

  # get nFactor
  nFactor <- 1
  message <- NULL

  if (options$factorMethod == "parallelAnalysis"){
    parallelAnalysis <- try(silent = FALSE, expr = {
            image <- .beginSaveImage()
            pa <- psych::fa.parallel(dataset)   
            .endSaveImage(image)
    })

    if (class(parallelAnalysis) == "try-error"){
      errorMessageTmp <- .extractErrorMessage(parallelAnalysis)
      message <- paste0("Parallel analysis not possible: ", errorMessageTmp)
      nFactor <- 1
    }else{
      if (is.na(pa$nfact)) pa$nfact <- 1
      nFactor <- max(1,pa$nfact)
      message <- sprintf("Parallel analysis suggests that the number of factors =  %d  and the number of components =  %d.",
      as.integer(pa$nfact), as.integer(pa$ncomp))
    }

  }else if(options$factorMethod == "eigenValues"){
      # Compute ev:
      eigenValues <- try(silent = FALSE, expr = {
              image <- .beginSaveImage()
              pa <- psych::fa.parallel(dataset)
              .endSaveImage(image)
      })

      if (class(eigenValues) == "try-error"){
        errorMessageTmp <- .extractErrorMessage(eigenValues)
        message <- paste0("Eigen values not possible: ", errorMessageTmp)
        nFactor <- 1
      }else{
        nFactor <- sum(pa$fa.values > options$eigenValuesBox)
        message <- sprintf("Eigen values suggests that the number of factors =  %d  and the number of components =  %d.",
         as.integer(pa$nfact), as.integer(pa$ncomp))
      }

  }else if(options$factorMethod == "manual"){
    nFactor <- options$numberOfFactors
  }
  # }

  return(list(nFactor = nFactor, message = message))
}


### Inner functions ###
# Estimate EFA:
.estimateEFA <- function(dataset, options, perform) {

  if (options$rotationMethod == "orthogonal"){
      Rotation <- options$orthogonalSelector
  } else {

      Rotation <- options$obliqueSelector
  }

  res <- .estimateNFactorEFA(dataset, options, perform)
  nFactor <- res$nFactor
  message <- res$message

  Results <- psych::fa(dataset, nFactor, rotate = Rotation)

  return(list(Results = Results, nFactor = nFactor, message = message))

}




# Path diagram:
.pathDiagramEFA <- function(analysisResults, options, perform, oldPlot=NULL){
  
  if (!is.null(oldPlot) && !identical(oldPlot[["data"]], "")){ #&& !is.null(oldPlot[["data"]])
    return(oldPlot)
  }

  pathDiagram <- list()
  pathDiagram$title <- "Path Diagram"
  pathDiagram$width <- options$plotWidthPathDiagram
  pathDiagram$height <- options$plotHeightPathDiagram
  if (pathDiagram$height==0){
    pathDiagram$height <- 1 + 299 * (length(options$variables)/5)
  }
  pathDiagram$custom <- list(width="plotWidthPathDiagram", height="plotHeightPathDiagram")
  
  
  #   filename <- .requestTempFileNameNative("svg")
  #   grDevices::svg(filename=filename, width=width/72, height=height/72, bg="transparent")
  #   
  #   relativePath <- filename$relativePath
  #   base::Encoding(filename) <- "UTF-8"
  #   
  
  # if (perform != "run" | is.null(analysisResults) | !isTRUE(options$incl_pathDiagram)){
  
  if (is.null(analysisResults) || isTryError(analysisResults)){ # || !isTRUE(options$incl_pathDiagram || perform == "init"
    
    pathDiagram$data <- NULL
    
  } else {
  # if(perform == "run"){
    
    
    # image <- .beginSaveImage(pathDiagram$width,pathDiagram$height)
#     Lambda <- loadings(analysisResults)
#     labels <- .unv(rownames(Lambda))
#     Lambda <- matrix(c(Lambda),nrow(Lambda),ncol(Lambda))
#     Theta <- analysisResults$uniquenesses
#     Psi <- analysisResults$r.scores
#     
#     qgraph::qgraph.loadings(Lambda, factorCors = Psi, resid = Theta,model="reflective",
#                             cut = options$highlightText, residSize = 0.25,labels = 
#                               labels)
#     
    
    # Via qgraph:
    # Model matrices:
    analysisResults <- analysisResults$Results

    LY <- as.matrix(loadings(analysisResults)) #labels <- .unv(rownames(LY))
    TE <- diag(analysisResults$uniqueness)
    PS <- analysisResults$r.scores
    
    if (options$rotationMethod == "orthogonal"){
      Rotation <- options$orthogonalSelector
    } else {
      Rotation <- options$obliqueSelector
    }
    
    
    # Variable names:
    labels <- .unv(rownames(LY))
    factors <- paste0("F",seq_len(ncol(LY)))
    
    # Number of variables:
    nFactor <- length(factors)
    nIndicator <- length(labels)
    nTotal <- nFactor + nIndicator
    
    # Make layout:
    # For each manifest, find strongest loading:
    strongest <- apply(abs(LY),1,which.max)
    ord <- order(strongest)
    
    # Reshuffle labels and LY:
    labels <- labels[ord]
    LY <- LY[ord,]
    
    # Edgelist:
    # Factor loadings
    E_loadings <- data.frame(from = rep(factors,each=nIndicator), to = rep(labels,times=nFactor),weight=c(LY),
                             stringsAsFactors = FALSE)
    
    # Residuals:
    E_resid <- data.frame(from=labels,to=labels,weight=diag(TE))
    E_resid$weight <- 0
    
    # Factor correlations:
    E_cor <- data.frame(from = c(factors[col(PS)]), to =  c(factors[row(PS)]),weight=c(PS),
                        stringsAsFactors = FALSE)
    E_cor <- E_cor[E_cor$from != E_cor$to,]
    
    # Total:
    E <- rbind(E_loadings,E_resid,E_cor)
    
    # Make the layout:
    sq <- function(x){
      seq(-1,1,length.out = x+2)[-c(1,x+2)]
    }
    
    L <- cbind(
      c(rep(-1,nFactor),rep(1,nIndicator)),
      c(sq(nFactor),sq(nIndicator))
    )
    
    # Compute curvature of correlations:
    # Numeric edgelist:
    E_cor_numeric <- cbind(match(E_cor$from,factors),match(E_cor$to,factors))
    
    # Compute distance:
    dist <- abs(L[E_cor_numeric[,1],2] - L[E_cor_numeric[,2],2])
    
    
    
    # Max has curvature of 8!
    # Min has curvature of 4!
    
    min <- 2
    max <- 8
    # Scale to max:
    dist <- min + dist/(max(dist))*(max - min)
    if (length(unique(dist))==1){
      dist[] <- mean(c(max,min))
    }
    
    # Scale such that this is true when nfactor = 2 and nIndicator == 5:
    # dist <- dist/((nFactor/2)*(nIndicator/2))
    
    # Scale to plot width:
    Scale <- sqrt(pathDiagram$width^2 + pathDiagram$height^2)/sqrt(480^2 + 300^2)
    dist <- 1/Scale * dist
#     
    
    # Curvature:
    curve <- c(rep(0,nrow(E_loadings)),
               rep(0,nrow(E_resid)),
               dist)
    
    # Edge connectpoints:
    ECP <- matrix(NA,nrow(E),2)
    ECP[nrow(E_loadings) + nrow(E_resid) + seq_len(nrow(E_cor)),1:2] <- 1.5*pi
    ECP[seq_len(nrow(E_loadings)),2] <- 1.5*pi
    
    # Loop rotation:
    loopRotation <- 0.5*pi
    
    # bidirectional:
    bidir <- c(rep(FALSE,nrow(E_loadings) + nrow(E_resid)), rep(TRUE, nrow(E_cor)))
    
    # Shape:
    shape <- c(rep("circle",nFactor), rep("rectangle",nIndicator))
    
    
    
    # Just some observations:
    # 10 indicators, height of 6 works well
    # 50 indicators, height of 1 works well
    # Good function: exp(2.25 - 0.05 * nIndicator)
    # OIther idea: keep size constant, just increase plot size in JASP!
    
    # Size:
    size1 <- c(
      rep(12,nFactor),
      rep(30,nIndicator)
    )
    size2 <- c(
      rep(12,nFactor),
      rep( 7,nIndicator)
    )

    # image <- .beginSaveImage(pathDiagram$width,pathDiagram$height)
    # Plot:
    label.scale.equal <- c(rep(1,nFactor),rep(2,nIndicator))
 
    # Run once without plotting to obtain the scaled label sizes:
    .plotFunc <- function(){
      qgraph::qgraph(E, layout = L, directed=TRUE, bidirectional=bidir, residuals = TRUE, residScale  = 10,
                     labels = c(factors,labels), curve = curve, curveScale = FALSE, edgeConnectPoints = ECP,
                     loopRotation=loopRotation, shape = shape, vsize = size1, vsize2 = size2,label.scale.equal=label.scale.equal,
                     residScale = 2, mar = c(5,10,5,12), normalize = FALSE, label.fill.vertical = 0.75, cut = options$highlightText,
                     bg = "transparent"
      )
    }

    content <- .writeImage(width = pathDiagram$width, 
      height = pathDiagram$height, plot = .plotFunc, obj = TRUE)
    pathDiagram[["convertible"]] <- TRUE
    pathDiagram[["obj"]] <- content[["obj"]]
    pathDiagram[["data"]] <- content[["png"]]
    pathDiagram[["status"]] <- "complete"
    
  }
  
  
  return(pathDiagram)
}
