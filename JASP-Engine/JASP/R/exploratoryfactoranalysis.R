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
  
  if (is.null(x) || any(is.na(x)) || !is.finite(x)){
    return(".")
  } else {
    return(x)
  }
}


ExploratoryFactorAnalysis <- function(dataset = NULL, options, perform = "run",
                                      callback = function(...) 0, ...) {
  
  
  
  ## call the common initialization function
  init <- .initializeEFA(dataset, options, perform)
  
  
  results <- init[["results"]]
  dataset <- init[["dataset"]]
  
  # States:
  state <- .retrieveState()
  analysisResults <- NULL
  newAnalysis <- TRUE
  # if ( ! is.null(state) && state$complete) {  # is there state?
  
  if ( !is.null(state)) {  # is there state?
    
    diff <- .diff(options, state$options)  # compare old and new options
    
    nVariable <- length(options$variables)
    
    if (is.list(diff) && !diff[['rotationMethod']] && !diff[['orthogonalSelector']] && !diff[['obliqueSelector']] && !diff[['variables']] && !diff[['factorMethod']] && 
        !diff[['eigenValuesBox']] && !diff[['numberOfFactors']]) {
      
      # old results can be used
      newAnalysis <- FALSE
      analysisResults <- state$analysisResults
      
    }
    
  }
  
  
  # Number of factors:
  nVariable <- length(options$variables)
  
  # Rotation method:
  if (options$rotationMethod == "orthogonal"){
    Rotation <- options$orthogonalSelector
  } else {
    Rotation <- options$obliqueSelector
  }
  
  
  # Number of factors:
  
  if (options$factorMethod == "parallelAnalysis"){
    
    if (nrow(dataset)>0 && nVariable > 0){
      image <- .beginSaveImage()
      pa <- psych::fa.parallel(dataset)   
      .endSaveImage(image)
      if (is.na(pa$nfact)) pa$nfact <- 1
      nFactor <- max(1,pa$nfact)
      
    } else {
      if (is.null(state$nFactor)){
        nFactor <- 1          
      } else {
        nFactor <- state$nFactor
      }
    }
    
  } else if (options$factorMethod == "eigenValues"){
    if (nrow(dataset)>0 && nVariable > 0){
      # Compute ev:
      image <- .beginSaveImage()
      pa <- psych::fa.parallel(dataset)   
      .endSaveImage(image)
      
      # Number of factors:
      nFactor <- sum(pa$fa.values > options$eigenValuesBox)
    } else {
      if (is.null(state$nFactor)){
        nFactor <- 1          
      } else {
        nFactor <- state$nFactor
      }
      
    }
  } else if (options$factorMethod == "manual"){
    nFactor <- options$numberOfFactors
  }
  
  
  # Check if number of factors is correct:
  if (length(options$variables) > 0 && nFactor > length(options$variables)){
    error <- TRUE
    errorMessage <- "Too many factors requested"
  } else {
    error <- FALSE
    errorMessage <- ""
  }
  
  
  if (perform == "run" && nrow(dataset) > 0 && is.null(analysisResults) && length(options$variables) > 1 && !error){
    
    analysisResults <- .estimateEFA(dataset, options, perform,nFactor)
    
  } else {
    # Otherwise just keep the state
    # analysisResults <- NULL
  }
  
  # Make factor loadings table:
  # if (newAnalysis){
  results[["factorLoadings"]] <- .getLoadingsEFA(analysisResults, options,perform,nFactor,dataset)    
  #   } else {
  #     results[["factorLoadings"]]  <- state$results[["factorLoadings"]] 
  #   }
  
  
  # Create factor correlation table:
  # if (newAnalysis || (is.list(diff) && diff[['incl_correlations']] && options$incl_correlations)){
  results[["factorCorrelations"]] <- .getFactorCorrelationsEFA(analysisResults, options,perform)    
  #   } else {
  #     results[["factorCorrelations"]] <- state$results[["factorCorrelations"]] 
  #   }
  
  
  # Create fit measures tables:
  results[["goodnessOfFit"]] <- .goodnessOfFitEFA(analysisResults, options,perform)
  results[["fitMeasures"]] <- .fitMeasuresEFA(analysisResults, options,perform)
  
  # Create path diagram:
  results[["pathDiagram"]] <- .pathDiagramEFA(analysisResults, options,perform)
  
  # Scree plot:
  results[["screePlot"]] <- .screePlotFA(dataset, options,perform)
  
  ## TEMP DEBUG THING:
  # save(dataset,results,init,options,perform,callback,...,file = "/Users/sachaepskamp/Dropbox/work/JASP/Rcodes/JASPinit.RData")
  
  
  #### META
  meta <- list(
    list(name = "title", type = "title")
  )
  
  if (isTRUE(options$incl_loadings)){
    meta[[length(meta)+1]] <- list(name = "factorLoadings", type = "table")
    
  }
  
  if (isTRUE(options$incl_correlations)){
    meta[[length(meta)+1]] <- list(name = "factorCorrelations", type = "table")
  }
  
  if (isTRUE(options$incl_GoF)){
    meta[[length(meta)+1]] <- list(name = "goodnessOfFit", type = "table")
  }
  if (isTRUE(options$incl_fitIndices)){
    meta[[length(meta)+1]] <- list(name = "fitMeasures", type = "table")
  }
  if (isTRUE(options$incl_pathDiagram)){
    meta[[length(meta)+1]] <- list(name = "pathDiagram", type = "image")
  }
  if (isTRUE(options$incl_screePlot)){
    meta[[length(meta)+1]] <- list(name = "screePlot", type = "image")
  }
  
  
  
  results[[".meta"]] <- meta
  
  # Dummies:
  status <- list(ready=TRUE, error=error,errorMessage=errorMessage)
  
  if (status$error == TRUE){
    results[["factorLoadings"]][["error"]] <-  list(errorType="badData", errorMessage=status$errorMessage)
    results[["factorCorrelations"]][["error"]] <-  list(errorType="badData")
    results[["goodnessOfFit"]][["error"]] <-  list(errorType="badData")
    results[["fitMeasures"]][["error"]] <-  list(errorType="badData")
    results[["pathDiagram"]][["error"]] <-  list(errorType="badData")
    results[["screePlot"]][["error"]] <-  list(errorType="badData")
  } else {
    results[["factorLoadings"]][["error"]] <- NULL
    results[["factorCorrelations"]][["error"]] <-  NULL
    results[["goodnessOfFit"]][["error"]] <-  NULL
    results[["fitMeasures"]][["error"]] <-  NULL
    results[["pathDiagram"]][["error"]] <-  NULL
    results[["screePlot"]][["error"]] <-  NULL
  }
  
  if (perform == "run" && status$ready) {
    state <- list(options=options,analysisResults=analysisResults,nFactor=nFactor,results=results,complete=TRUE)
    retList <- list(results=results, status="complete", state=state)

  } else {
    state <- list(options=options,nFactor=nFactor,analysisResults=analysisResults,results=results,complete=FALSE)
    
    retList <- list(results=results, status="inited",state=state)

  }

  return(retList)
}

### Inner functions ###
# Estimate EFA:
.estimateEFA <- function(dataset, options, perform, nFactor=1) {
  
  nVariable <- length(options$variables)
  
  # Rotation method:
  if (options$rotationMethod == "orthogonal"){
    Rotation <- options$orthogonalSelector
  } else {
    Rotation <- options$obliqueSelector
  }
  
  
  #   # Number of factors:
  #   if (options$factorMethod == "eigenValues"){
  #     # Covariance matrix:
  #     corMatrix <- cor(dataset, use = "pairwise.complete.obs") ### ADD MISSING OPTION
  #     
  #     # Eigenvalues:
  #     EV <- eigen(corMatrix,only.values = TRUE)$values
  #     
  #     # Number of factors:
  #     nFactor <- sum(EV > options$eigenValuesBox)
  #   } else if (options$factorMethod == "manual"){
  #     nFactor <- options$numberOfFactors
  #   }
  
  if (nFactor == 0) stop("Number of factors must be > 0")
  if (nFactor > nVariable){
    stop("Too many factors requested")
  }
  
  Results <- psych::fa(dataset,nFactor, rotate = Rotation)
  # Results <- factanal(dataset, nFactor, rotation = Rotation)
  
  return(Results)
}

# Get loadings matrix:
.getLoadingsEFA <- function(analysisResults, options,perform, nFactor=1,dataset){
  
  # Extract loadings:
  # if (!is.null(analysisResults) & perform == "run"){
  if (!is.null(analysisResults)){
    
    loadingsMatrix <- as.matrix(loadings(analysisResults))    
    # loadingsMatrix <- matrix(unlist(loadingsMatrix),nrow(loadingsMatrix),ncol(loadingsMatrix))
    
    
    # loadingsMatrix <- matrix(1,12,1)
    
    
    if (ncol(loadingsMatrix) > 0){
      colnames(loadingsMatrix) <- paste("Factor",seq_len(ncol(loadingsMatrix)))
    }
    
    # Add uniqueness:
    
    loadingsMatrix <- cbind(loadingsMatrix, Uniqueness = unname(analysisResults$uniquenesses))
    
  } else {
    
    if (is.null(options$numberOfFactors)){
      nFact <- 0
    } else {
      nFact <- options$numberOfFactors
    }
    
    loadingsMatrix <- matrix(NA,length(options$variables),nFactor+1)
    colnames(loadingsMatrix) <- c( paste("Factor",seq_len(nFactor)),"Uniqueness")
    rownames(loadingsMatrix) <- colnames(dataset)
    
  }
  
  # Create JASP table:
  Loadings <- list()
  Loadings[["title"]] <- "Factor Loadings"
  Loadings[["schema"]] <- list(fields = list(
    #     list(name="model", title = "", type="text"),
    #     list(name="cn_05", title = "Hoelter Critical N (CN) alpha=0.05", type="number", format = "dp:3"),
    #     list(name="cn_01", title = "Hoelter Critical N (CN) alpha=0.01", type="number", format = "dp:3"),
    #     list(name="gfi", title = "Goodness of Fit Index (GFI)", type="number", format = "dp:3"),
    #     list(name="agfi", title = "Parsimony Goodness of Fit Index (GFI)", type="number", format = "dp:3"),
    #     list(name="mfi", title = "McDonald Fit Index (MFI)", type="number", format = "dp:3"),
    #     list(name="ecvi", title = "Expected Cross-Validation Index (ECVI)", type="number", format = "dp:3")
  ))
  
  # Add columns:
  Loadings[["schema"]][["fields"]][[1]] <- list(name = "VAR", title = "", type="string")
  
  for (j in seq_len(ncol(loadingsMatrix))){
    Loadings[["schema"]][["fields"]][[j+1]] <- list(name = colnames(loadingsMatrix)[j], title = colnames(loadingsMatrix)[j], type="number", format = "dp:3")
  }
  
  Loadings[["data"]] <- list()
  
  # Add rows:
  if (nrow(loadingsMatrix)==0){
    Loadings[["data"]][[1]] <- as.list(rep(".",ncol(loadingsMatrix)+1))
  }
  
  for (i in seq_len(nrow(loadingsMatrix))){
    
    dat <- loadingsMatrix[i,]
    
    Loadings[["data"]][[i]] <- list(VAR = .unv(rownames(loadingsMatrix)[i]))
    for (j in seq_along(dat)){
      if (is.na(dat[j])){
        Loadings[["data"]][[i]][[j+1]] <- "."
      } else {
        Loadings[["data"]][[i]][[j+1]] <- unname(ifelse(abs(dat[j]) < options$highlightText & j != length(dat),".",dat[j]))        
      }
      
    }
    
    names( Loadings[["data"]][[i]]) <- sapply(Loadings[["schema"]][["fields"]],"[[",'name')
  }
  
  return(Loadings)
}

# Init:
.initializeEFA <- function(dataset, options, perform) {
  
  groups <- options$groupingVariable
  depvars <- unlist(options$variables)
  
  
  if (!is.null(groups) && groups == "") groups <- NULL
  
  if (is.null(dataset)) {
    ## if we are ready to run, read in the dataset
    if (perform == "run" && length(options$variables) > 1) {
      #
      #       if (options$missingValues == "excludeListwise") {
      #         exclude <- depvars
      #       } else {
      #         exclude <- NULL
      #       }
      #
      dataset <- .readDataSetToEnd(columns.as.numeric = depvars,
                                   columns.as.factor = groups)
      
      ## else just read in the headers (and create an empty table)
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric = depvars,
                                    columns.as.factor = groups)
    }
  }
  
  ## this is the main object; we add stuff to it and return it
  results <- list(
    "title" = "Exploratory Factor Analysis",
    "citation" = list("Revelle, W. (2015) psych: Procedures for Personality and Psychological Research, Northwestern University,
  Evanston, Illinois, USA, http://CRAN.R-project.org/package=psych Version = 1.5.8.")
  )
  
  #### META
  # meta <- list()
  # meta[[1]] <- list(name = "title", type = "title")
  #   meta[[2]] <- list(name = "ttest", type = "table")
  #   meta[[3]] <- list(name="assumptionChecks", type="object", meta=list(list(name="shapiroWilk", type="table"), list(name="levene", type="table")))
  #   meta[[4]] <- list(name="descriptives", type="object", meta=list(list(name="descriptivesTable", type="table"), list(name = "descriptivesPlots", type = "collection", meta="image")))
  #
  # results[[".meta"]] <- meta
  list("results" = results, "dataset" = dataset)
}


# Path diagram:
.pathDiagramEFA <- function(analysisResults, options, perform){
  
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
  
  if (is.null(analysisResults) || !isTRUE(options$incl_pathDiagram) || perform == "init"){
    
    pathDiagram$data <- NULL
    
  } else {
    
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
    LY <- as.matrix(loadings(analysisResults))
    TE <- diag(analysisResults$uniqueness)
    PS <- analysisResults$r.scores
    
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
    
    # Plot:
    label.scale.equal <- c(rep(1,nFactor),rep(2,nIndicator))
    
    .plotFunc <- function() {
    # Run once without plotting to obtain the scaled label sizes:
    qgraph::qgraph(E, layout = L, directed=TRUE, bidirectional=bidir, residuals = TRUE, residScale  = 10,
                   labels = c(factors,labels), curve = curve, curveScale = FALSE, edgeConnectPoints = ECP,
                   loopRotation=loopRotation, shape = shape, vsize = size1, vsize2 = size2,label.scale.equal=label.scale.equal,
                   residScale = 2, mar = c(5,10,5,12), normalize = FALSE, label.fill.vertical = 0.75, cut = options$highlightText,
                   bg = "transparent"
    )
    }
    
    content <- .writeImage(width = pathDiagram$width, height = pathDiagram$height, plot = .plotFunc, obj = TRUE)
    pathDiagram[["convertible"]] <- TRUE
	pathDiagram[["obj"]] <- content[["obj"]]
	pathDiagram[["data"]] <- content[["png"]]
    
    # pathDiagram$data <- .endSaveImage(image)
    pathDiagram$status <- "complete"
    
  }
  


  # grDevices::dev.off()
  
  return(pathDiagram)
}

# Factor correlations:
.getFactorCorrelationsEFA <- function(analysisResults, options,perform){
  
  # Extract loadings:
  if (!is.null(analysisResults)){
    
    corMatrix <- as.matrix(analysisResults$r.scores)
    # loadingsMatrix <- matrix(unlist(loadingsMatrix),nrow(loadingsMatrix),ncol(loadingsMatrix))
    
    
    # loadingsMatrix <- matrix(1,12,1)
    
  } else {
    
    if (is.null(options$numberOfFactors)){
      nFact <- 0
    } else {
      nFact <- options$numberOfFactors
    }
    
    corMatrix <- matrix(,0,0)
    
  }
  
  if (ncol(corMatrix) > 0){
    colnames(corMatrix) <- rownames(corMatrix) <- paste("Factor",seq_len(ncol(corMatrix)))
  }
  
  # Create JASP table:
  FactorCorrelations <- list()
  FactorCorrelations[["title"]] <- "Factor Correlations"
  FactorCorrelations[["schema"]] <- list(fields = list(
    #     list(name="model", title = "", type="text"),
    #     list(name="cn_05", title = "Hoelter Critical N (CN) alpha=0.05", type="number", format = "dp:3"),
    #     list(name="cn_01", title = "Hoelter Critical N (CN) alpha=0.01", type="number", format = "dp:3"),
    #     list(name="gfi", title = "Goodness of Fit Index (GFI)", type="number", format = "dp:3"),
    #     list(name="agfi", title = "Parsimony Goodness of Fit Index (GFI)", type="number", format = "dp:3"),
    #     list(name="mfi", title = "McDonald Fit Index (MFI)", type="number", format = "dp:3"),
    #     list(name="ecvi", title = "Expected Cross-Validation Index (ECVI)", type="number", format = "dp:3")
  ))
  
  # Add columns:
  FactorCorrelations[["schema"]][["fields"]][[1]] <- list(name = "VAR", title = "", type="string")
  
  for (j in seq_len(ncol(corMatrix))){
    FactorCorrelations[["schema"]][["fields"]][[j+1]] <- list(name = colnames(corMatrix)[j], title = colnames(corMatrix)[j], type="number", format = "dp:3")
  }
  
  FactorCorrelations[["data"]] <- list()
  
  # Add rows:
  for (i in seq_len(nrow(corMatrix))){
    
    dat <- corMatrix[i,]
    FactorCorrelations[["data"]][[i]] <- list(VAR = rownames(corMatrix)[i])
    for (j in 1:i){
      FactorCorrelations[["data"]][[i]][[j+1]] <- unname(dat[j])
    }
    
    names(FactorCorrelations[["data"]][[i]]) <- sapply(FactorCorrelations[["schema"]][["fields"]],"[[",'name')[seq_along(FactorCorrelations[["data"]][[i]])]
  }
  
  return(FactorCorrelations)
}

# Goodness of fit
.goodnessOfFitEFA <- function(analysisResults, options,perform){
  
  # Extract loadings:
  if (!is.null(analysisResults)){
    
    #     print(analysisResults)
    #     print(analysisResults$RMSEA)
    #     
    #     Fits <- list(
    #       CHI = analysisResults$STATISTIC,
    #       PVAL = analysisResults$PVAL,
    #       DF = analysisResults$dof,
    #       RMSEA = analysisResults$RMSEA['RMSEA'],
    #       RMSEAlower = analysisResults$RMSEA['lower'],      
    #       RMSEAupper = analysisResults$RMSEA['upper'],      
    #       TLI = analysisResults$TLI,
    #       RMS = analysisResults$rms,
    #       CRMS = analysisResults$crms,
    #       BIC = analysisResults$BIC
    #     )
    
    
    Fits <- list(
      CHI = .DotIfNULL(analysisResults$STATISTIC),
      PVAL = .DotIfNULL(analysisResults$PVAL),
      DF = .DotIfNULL(analysisResults$dof),
      RMSEA = .DotIfNULL(unname(analysisResults$RMSEA['RMSEA'])),
      RMSEAlower = .DotIfNULL(unname(analysisResults$RMSEA['lower'])),
      RMSEAupper = .DotIfNULL(unname(analysisResults$RMSEA['upper'])),      
      TLI = .DotIfNULL(analysisResults$TLI),
      RMS = .DotIfNULL(analysisResults$rms),
      CRMS = .DotIfNULL(analysisResults$crms),
      BIC = .DotIfNULL(analysisResults$BIC)
    )
    
  } else {
    
    Fits <- list(
      CHI = ".",
      PVAL = ".",
      DF = ".",
      RMSEA = ".",
      RMSEAlower = ".",      
      RMSEAupper = ".",
      TLI = ".",
      RMS = ".",
      CRMS = ".",
      BIC = "."
    )
    
  }
  
  
  # Create JASP table:
  goodnessOfFit <- list()
  goodnessOfFit[["title"]] <- "Chi-squared Test"
  
  # Create the columns:
  goodnessOfFit[["schema"]] <- list(fields = list(
    list(name = "model", title="", type = "string"),
    list(name = "chisq", title = "Value", type="number", format = "dp:3"),
    list(name = "df", title = "df", type="integer"),
    list(name = "p", title = "p", type="number", format = "dp:3;p:.001")
  ))
  
  # Create and fill the row(s):
  goodnessOfFit[["data"]] <- list(
    list(
      model = "Model", 
      chisq = ifelse(Fits$DF>0,Fits$CHI,"."),
      df = ifelse(Fits$DF>0,Fits$DF,"."),
      p = ifelse(Fits$DF>0,Fits$PVAL,".")
    )
  )
  
  
  return(goodnessOfFit)
}


# fitMeasures
.fitMeasuresEFA <- function(analysisResults, options,perform){
  
  # browser()
  
  # Extract loadings:
  if (!is.null(analysisResults)){
    
    Fits <- list(
      CHI = .DotIfNULL(analysisResults$STATISTIC),
      PVAL = .DotIfNULL(analysisResults$PVAL),
      DF = .DotIfNULL(analysisResults$dof),
      RMSEA = .DotIfNULL(unname(analysisResults$RMSEA['RMSEA'])),
      RMSEAlower = .DotIfNULL(unname(analysisResults$RMSEA['lower'])),
      RMSEAupper = .DotIfNULL(unname(analysisResults$RMSEA['upper'])),      
      TLI = .DotIfNULL(analysisResults$TLI),
      RMS = .DotIfNULL(analysisResults$rms),
      CRMS = .DotIfNULL(analysisResults$crms),
      BIC = .DotIfNULL(analysisResults$BIC)
    )
    
  } else {
    
    Fits <- list(
      CHI = ".",
      PVAL = ".",
      DF = ".",
      RMSEA = ".",
      RMSEAlower = ".",      
      RMSEAupper = ".",
      TLI = ".",
      RMS = ".",
      CRMS = ".",
      BIC = "."
    )
    
  }
  
  
  # Create JASP table:
  FitMeasures <- list()
  FitMeasures[["title"]] <- "Additional fit indices"
  
  # Create the columns:
  FitMeasures[["schema"]] <- list(fields = list(
    list(name = "model", title="", type = "string"),
    list(name = "RMSEA", title = "RMSEA", type="number", format = "dp:3"),
    list(name = "RMSEAci", title = "RMSEA 90% confidence", type="string"),
    list(name = "TLI", title = "TLI", type="number", format = "dp:3"),
    list(name = "BIC", title = "BIC", type="number", format = "dp:3")
  ))
  
  # Create and fill the row(s):
  if (is.numeric(Fits$RMSEAlower))
  {
    Fits$RMSEAlower <- round(Fits$RMSEAlower,3)
  }
  
  if (is.numeric(Fits$RMSEAupper))
  {
    Fits$RMSEAupper <- round(Fits$RMSEAupper,3)
  }
  
  FitMeasures[["data"]] <- list(
    list(
      model = "Model", 
      RMSEA = Fits$RMSEA,
      RMSEAci = paste(Fits$RMSEAlower,"-",Fits$RMSEAupper),
      TLI = Fits$TLI,
      BIC = Fits$BIC
    )
  )
  
  
  return(FitMeasures)
}


### Screeplot:
.screePlotFA <- function(dataset, options, perform) {
  
  screePlot <- list()
  
  #   if (perform == "run" && status$ready && !status$error && !is.null(model)) {
  #    
  if (perform == "run" && ncol(dataset) > 0 && nrow(dataset)> 0 && length(options$variables) > 1) {
    
    screePlot$title <- "Scree Plot"
    screePlot$width <- options$plotWidthScreePlot
    screePlot$height <- options$plotHeightScreePlot
    screePlot$custom <- list(width="plotWidthScreePlot", height="plotHeightScreePlot")
    
    # Compute ev:
    image <- .beginSaveImage()
    pa <- psych::fa.parallel(dataset)   
    .endSaveImage(image)
    
    # Eigenvalues:
    EV <- data.frame(
      id = seq_len(ncol(dataset)),
      ev = pa$fa.values,
      type = "Data"
    )
    
    # Parallel analysis:
    PA <- data.frame(
      id = seq_len(ncol(dataset)),
      ev = pa$fa.sim,
      type = "Simulated (95th quantile)"
    )
    
    combined <- rbind(EV,PA)
    
    p <- ggplot2::ggplot(combined, ggplot2::aes_string(x="id",y="ev",lty="type",pch="type")) + ggplot2::geom_point(na.rm = TRUE, size=3) +
      ggplot2::xlab("") + ggplot2::ylab("Eigenvalue")+ ggplot2::xlab("Factors") +ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::ggtitle("") + ggplot2::theme_bw() + ggplot2::geom_hline(yintercept = options$eigenValuesBox) +
      ggplot2::theme(panel.grid.minor=ggplot2::element_blank(), plot.title = ggplot2::element_text(size=18),
                     panel.grid.major=ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black", size=1.2),
                     axis.title.x = ggplot2::element_text(size=18,vjust=-.2), axis.title.y = ggplot2::element_text(size=18,vjust=1.2),
                     axis.text.x = ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15),
                     panel.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                     plot.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
                     panel.border = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_line(size = 0.5),
                     axis.ticks.margin = grid::unit(1,"mm"),
                     axis.ticks.length = grid::unit(3, "mm"),
                     plot.margin = grid::unit(c(0,0,.5,.5), "cm")) + 
      ggplot2::scale_linetype_discrete("") +  ggplot2::scale_shape_discrete("") +
      ggplot2::theme(legend.position = c(0.99,0.99),legend.justification = c(1,1),
                     legend.text=ggplot2::element_text(size=12.5),
                     panel.background=ggplot2::element_rect(fill="transparent",colour=NA),
                     plot.background=ggplot2::element_rect(fill="transparent",colour=NA),
                     legend.key = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
                     legend.background=ggplot2::element_rect(fill="transparent",colour=NA))
    
    # image <- .beginSaveImage(options$plotWidthScreePlot, options$plotHeightScreePlot)
    # print(p)
    # content <- .endSaveImage(image)
    
    content <- .writeImage(width = options$plotWidthScreePlot, 
    					   height = options$plotHeightScreePlot,
    					   plot = p, obj = TRUE)
    screePlot$data <- content[["png"]]
    screePlot[["convertible"]] <- TRUE
	screePlot[["obj"]] <- content[["obj"]]
    
    # screePlot$data <- content
    screePlot$status <- "complete"
    
    statescreePlot <- screePlot
    
  } else {
    
    screePlot$title <- "Scree Plot"
    screePlot$width <- options$plotWidthScreePlot
    screePlot$height <- options$plotHeightScreePlot
    screePlot$custom <- list(width="plotWidthScreePlot", height="plotHeightScreePlot")
    
    screePlot$data <- NULL
    
    statescreePlot <- NULL
    
    #     if (status$error)
    #       screePlot$error <- list(errorType="badData")
    
  } 
  
  return(screePlot)
}
