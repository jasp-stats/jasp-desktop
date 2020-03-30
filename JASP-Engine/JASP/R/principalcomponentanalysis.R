#
# Copyright (C) 2013-2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <http://www.gnu.org/licenses/>.
#

PrincipalComponentAnalysis <- function(jaspResults, dataset, options, ...) {
  jaspResults$addCitation("Revelle, W. (2018) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, https://CRAN.R-project.org/package=psych Version = 1.8.12.")

  # Read dataset
  dataset <- .pcaReadData(dataset, options)
  ready   <- length(options$variables) > 1
  
  if (ready)
    .pcaCheckErrors(dataset, options)

  modelContainer <- .pcaModelContainer(jaspResults)

  # output functions
  .pcaGoFTable(     modelContainer, dataset, options, ready)
  .pcaLoadingsTable(modelContainer, dataset, options, ready)
  .pcaEigenTable(   modelContainer, dataset, options, ready)
  .pcaCorrTable(    modelContainer, dataset, options, ready)
  .pcaScreePlot(    modelContainer, dataset, options, ready)
  .pcaPathDiagram(  modelContainer, dataset, options, ready)

  # data saving
  .pcaAddComponentsToData(jaspResults, modelContainer, options, ready)
}

# Preprocessing functions ----
.pcaReadData <- function(dataset, options) {
  if (!is.null(dataset)) return(dataset)

  if (options[["missingValues"]] == "listwise") {
    return(.readDataSetToEnd(columns = unlist(options$variables), exclude.na.listwise = unlist(options$variables)))
  } else {
    return(.readDataSetToEnd(columns = unlist(options$variables)))
  }
}

.pcaCheckErrors <- function(dataset, options) {
  customChecksPCAEFA <- list(
    function() {
      if (length(options$variables) > 0 && options$factorMethod == "manual" &&
          options$numberOfFactors > length(options$variables)) {
        return(gettextf("Too many factors requested (%i) for the amount of included variables", options$numberOfFactors))
      }
    },
    function() {
      if(nrow(dataset) < 3){
        return(gettextf("Not enough valid cases (%i) to run this analysis", nrow(dataset)))
      }
    },
    # check whether all row variance == 0
    function() {
      varianceZero <- 0
      for (i in 1:nrow(dataset)){
        if(sd(dataset[i,], na.rm = TRUE) == 0) varianceZero <- varianceZero + 1
      }
      if(varianceZero == nrow(dataset)){
        return(gettext("Data not valid: variance is zero in each row"))
      }
    },
    # Check for correlation anomalies
    function() {
      P <- ncol(dataset)
      
      # check whether a variable has too many missing values to compute the correlations
      Np <- colSums(!is.na(dataset))
      error_variables <- .unv(names(Np)[Np < P])
      if (length(error_variables) > 0) {
        return(gettextf("Data not valid: too many missing values in variable(s) %s.",
                        paste(error_variables, collapse = ", ")))
      }
      
      S <- cor(dataset)
      if (all(S == 1)) {
        return(gettext("Data not valid: all variables are collinear"))
      }
    }
  )
  error <- .hasErrors(dataset = dataset, type = c("infinity", "variance"), custom = customChecksPCAEFA,
                      exitAnalysisIfErrors = TRUE)
  return()
}

.pcaModelContainer <- function(jaspResults, options) {
  if (!is.null(jaspResults[["modelContainer"]])) {
    modelContainer <- jaspResults[["modelContainer"]]
  } else {
    modelContainer <- createJaspContainer()
    modelContainer$dependOn(c("rotationMethod", "orthogonalSelector", "obliqueSelector", "variables", "factorMethod",
                              "eigenValuesBox", "numberOfFactors", "missingValues", "basedOn"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  return(modelContainer)
}


# Results functions ----
.pcaComputeResults <- function(modelContainer, dataset, options, ready) {
  pcaResult <- try(
    psych::principal(
      r        = dataset,
      nfactors = .pcaGetNComp(dataset, options),
      rotate   = ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector),
      scores   = TRUE,
      covar    = options$basedOn == "covariance"
    )
  )

  if (inherits(pcaResult, "try-error")) {
    errmsg <- gettextf("Estimation failed. \nInternal error message: %s", attr(pcaResult, "condition")$message)
    modelContainer$setError(.decodeVarsInMessage(names(dataset), errmsg))
  }

  modelContainer[["model"]] <- createJaspState(pcaResult)
  return(pcaResult)
}

.pcaGetNComp <- function(dataset, options) {
  if (options$factorMethod == "manual")           return(options$numberOfFactors)
  fa <- try(psych::fa.parallel(dataset, plot = FALSE))
  if (inherits(fa, "try-error"))                  return(1)
  if (options$factorMethod == "parallelAnalysis") return(max(1, fa$ncomp))
  if (options$factorMethod == "eigenValues") {
    ncomp <- sum(fa$pc.values > options$eigenValuesBox)
    # I can use stop() because it's caught by the try and the message is put on
    # on the modelcontainer.
    if (ncomp == 0)
      stop(
        gettext("No components with an eigenvalue > "), options$eigenValuesBox, ". ",
        gettext("Maximum observed eigenvalue: "), round(max(fa$pc.values), 3)
      )
    return(ncomp)
  }
}


# Output functions ----
.pcaGoFTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["goftab"]])) return()

  goftab <- createJaspTable(title = "Chi-squared Test")
  goftab$addColumnInfo(name = "model", title = "",               type = "string")
  goftab$addColumnInfo(name = "chisq", title = gettext("Value"), type = "number", format = "dp:3")
  goftab$addColumnInfo(name = "df",    title = gettext("df"),    type = "integer")
  goftab$addColumnInfo(name = "p",     title = gettext("p"),     type = "number", format = "dp:3;p:.001")
  goftab$position <- 1

  modelContainer[["goftab"]] <- goftab

  if (!ready) return()

  pcaResults <- .pcaComputeResults(modelContainer, dataset, options)
  if (modelContainer$getError()) return()

  goftab[["model"]] <- "Model"
  goftab[["chisq"]] <- pcaResults$STATISTIC
  goftab[["df"]]    <- pcaResults$dof
  goftab[["p"]]     <- pcaResults$PVAL

  if (pcaResults$dof < 0)
    goftab$addFootnote(message = gettext("Degrees of freedom below 0, model is unidentified."), symbol = gettext("<em>Warning:</em>"))
}

.pcaLoadingsTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["loatab"]])) return()
  loatab <- createJaspTable(gettext("Component Loadings"))
  loatab$dependOn("highlightText")
  loatab$position <- 2
  loatab$addColumnInfo(name = "var", title = "", type = "string")
  modelContainer[["loatab"]] <- loatab

  if (!ready || modelContainer$getError()) return()

  pcaResults <- modelContainer[["model"]][["object"]]

  coltitle <- ifelse(options$rotationMethod == "orthogonal", "PC", "RC")
  if (options$rotationMethod == "orthogonal" && options$orthogonalSelector == "none") {
    loatab$addFootnote(message = gettext("No rotation method applied."))
  } else {
    loatab$addFootnote(
      message = gettextf("Applied rotation method is %s.", ifelse(options$rotationMethod == "orthogonal", options$orthogonalSelector, options$obliqueSelector))
    )
  }

  loads <- loadings(pcaResults)
  loatab[["var"]] <- .unv(rownames(loads))

  for (i in 1:ncol(loads)) {
    # fix weird "all true" issue
    if (all(abs(loads[, i]) < options$highlightText)) {
      loatab$addColumnInfo(name = paste0("c", i), title = paste0(coltitle, i), type = "string")
      loatab[[paste0("c", i)]] <- rep("", nrow(loads))
    } else {
      loatab$addColumnInfo(name = paste0("c", i), title = paste0(coltitle, i), type = "number", format = "dp:3")
      loatab[[paste0("c", i)]] <- ifelse(abs(loads[, i]) < options$highlightText, NA, loads[ ,i])
    }
  }

  loatab$addColumnInfo(name = "uni", title = gettext("Uniqueness"), type = "number", format = "dp:3")
  loatab[["uni"]] <- pcaResults$uniquenesses
}

.pcaEigenTable <- function(modelContainer, dataset, options, ready) {
  if (!is.null(modelContainer[["eigtab"]])) return()
  coltitle <- ifelse(options$rotationMethod == "orthogonal", "PC", "RC")

  eigtab <- createJaspTable("Component Characteristics")
  eigtab$addColumnInfo(name = "comp", title = "",                type = "string")
  eigtab$addColumnInfo(name = "eigv", title = gettext("Eigenvalue"),      type = "number", format = "sf:4;dp:3")
  eigtab$addColumnInfo(name = "prop", title = gettext("Proportion var."), type = "number", format = "sf:4;dp:3")
  eigtab$addColumnInfo(name = "cump", title = gettext("Cumulative"),      type = "number", format = "sf:4;dp:3")

  eigtab$position <- 3

  modelContainer[["eigtab"]] <- eigtab

  if (!ready || modelContainer$getError()) return()

  pcaResults <- modelContainer[["model"]][["object"]]

  eigv <- pcaResults$values
  eigtab[["comp"]] <- paste0(coltitle, 1:pcaResults$factors)
  eigtab[["eigv"]] <- eigv[1:pcaResults$factors]
  eigtab[["prop"]] <- eigv[1:pcaResults$factors] / sum(eigv)
  eigtab[["cump"]] <- cumsum(eigv)[1:pcaResults$factors] / sum(eigv)
}

.pcaCorrTable <- function(modelContainer, dataset, options, ready) {
  if (!options[["incl_correlations"]] || !is.null(modelContainer[["cortab"]])) return()
  cortab <- createJaspTable(gettext("Component Correlations"))
  cortab$dependOn("incl_correlations")
  cortab$addColumnInfo(name = "col", title = "", type = "string")
  cortab$position <- 4
  modelContainer[["cortab"]] <- cortab

  if (!ready || modelContainer$getError()) return()

  coltitle <- ifelse(options$rotationMethod == "orthogonal", "PC", "RC")
  cors <- zapsmall(modelContainer[["model"]][["object"]][["r.scores"]])
  dims <- ncol(cors)


  cortab[["col"]] <- paste0(coltitle, 1:dims)

  for (i in 1:dims) {
    thisname <- paste0(coltitle, i)
    cortab$addColumnInfo(name = thisname, title = thisname, type = "number", format = "dp:3")
    cortab[[thisname]] <- cors[,i]
  }

}

.pcaScreePlot <- function(modelContainer, dataset, options, ready) {
  if (!options[["incl_screePlot"]] || !is.null(modelContainer[["scree"]])) return()

  scree <- createJaspPlot(title = gettext("Scree plot"), width = 480, height = 320)
  scree$dependOn("incl_screePlot")
  modelContainer[["scree"]] <- scree

  if (!ready || modelContainer$getError()) return()

  fa <- try(psych::fa.parallel(dataset, plot = FALSE))
  if (inherits(fa, "try-error")) {
    errmsg <- gettextf("Screeplot not available. \nInternal error message: %s", attr(pcaResult, "condition")$message)
    scree$setError(.decodeVarsInMessage(names(dataset), errmsg))
    return()
  }

  n_col <- ncol(dataset)
  df <- data.frame(
    id   = rep(seq_len(n_col), 2),
    ev   = c(fa$pc.values, fa$pc.sim),
    type = rep(c(gettext("Data"), gettext("Simulated (95th quantile)")), each = n_col)
  )

  # basic scree plot
  plt <-
    ggplot2::ggplot(df, ggplot2::aes(x = id, y = ev, linetype = type, shape = type)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = gettext("Component"), y = gettext("Eigenvalue"))
  
 
  # dynamic function for point size:
  # the plot looks good with size 3 when there are 10 points (3 + log(10) - log(10) = 3)
  # with more points, the size will become logarithmically smaller until a minimum of 
  # 3 + log(10) - log(200) = 0.004267726
  # with fewer points, they become bigger to a maximum of 3 + log(10) - log(2) = 4.609438
  pointsize <- 3 + log(10) - log(n_col)
  if (pointsize > 0) {
    plt <- plt + ggplot2::geom_point(na.rm = TRUE, size = max(0, 3 + log(10) - log(n_col)))
  }

  # optionally add an eigenvalue cutoff line
  if (options$factorMethod == "eigenValues") {
    plt <- plt + ggplot2::geom_hline(yintercept = options$eigenValuesBox)
  }

  # theming with special legend thingy
  plt <-
    JASPgraphs::themeJasp(plt) +
    ggplot2::theme(
      legend.position      = c(0.99, 0.95),
      legend.justification = c(1, 1),
      legend.text          = ggplot2::element_text(size = 12.5),
      legend.title         = ggplot2::element_blank(),
      legend.key.size      = ggplot2::unit(18, "pt")
    )

  scree$plotObject <- plt
  modelContainer[["scree"]] <- scree
}

.pcaPathDiagram <- function(modelContainer, dataset, options, ready){
  if (!options[["incl_pathDiagram"]] || !is.null(modelContainer[["path"]])) return()

  # Create plot object
  n_var <- length(options$variables)
  path <- createJaspPlot(title = gettext("Path Diagram"), width = 480, height = ifelse(n_var < 2, 300, 1 + 299 * (n_var / 5)))
  path$dependOn(c("incl_pathDiagram", "highlightText"))
  modelContainer[["path"]] <- path
  if (!ready || modelContainer$getError()) return()

  # Get result info
  pcaResult <- modelContainer[["model"]][["object"]]
  LY <- as.matrix(loadings(pcaResult))
  TE <- diag(pcaResult$uniqueness)
  PS <- pcaResult$r.scores

  # Variable names
  xName   <- ifelse(options$rotationMethod == "orthogonal" && options$orthogonalSelector == "none", "PC", "RC")
  factors <- paste0(xName, seq_len(ncol(LY)))
  labels  <- .unv(rownames(LY))

  # Number of variables:
  nFactor    <- length(factors)
  nIndicator <- length(labels)
  nTotal     <- nFactor + nIndicator

  # Make layout:
  # For each manifest, find strongest loading:
  strongest <- apply(abs(LY), 1, which.max)
  ord       <- order(strongest)

  # Reshuffle labels and LY:
  labels <- labels[ord]
  LY     <- LY[ord,]

  # Edgelist:
  # Factor loadings
  E_loadings <- data.frame(
    from   = rep(labels, nFactor),
    to     = rep(factors, each = nIndicator),
    weight = c(LY),
    stringsAsFactors = FALSE
  )

  # Residuals:
  E_resid <- data.frame(
    from   = labels,
    to     = labels,
    weight = diag(TE)
  )

  # Factor correlations:
  E_cor <- data.frame(
    from   = c(factors[col(PS)]),
    to     = c(factors[row(PS)]),
    weight = c(PS),
    stringsAsFactors = FALSE
  )
  E_cor <- E_cor[E_cor$from != E_cor$to, ]

  # Combine everything:
  edge_df <- rbind(E_loadings, E_resid, E_cor)

  # Make the layout:
  sq <- function(x) seq(-1, 1, length.out = x + 2)[-c(1, x + 2)]

  layout_mat <- cbind(
    c(rep(-1, nFactor), rep(1, nIndicator)),
    c(sq(nFactor),      sq(nIndicator))
  )

  # Compute curvature of correlations:
  # Numeric edgelist:
  E_cor_numeric <- cbind(match(E_cor$from, factors), match(E_cor$to, factors))

  # Compute distance:
  dist <- abs(layout_mat[E_cor_numeric[,1], 2] - layout_mat[E_cor_numeric[,2], 2])
  min <- 2
  max <- 8

  # Scale to max:
  dist <- min + dist / (max(dist)) * (max - min)
  if (length(unique(dist)) == 1) {
    dist[] <- mean(c(max, min))
  }

  # Scale to plot width:
  Scale <- sqrt(path$width^2 + path$height^2) / sqrt(480^2 + 300^2)
  dist <- 1 / Scale * dist

  # Curvature:
  curve <- c(rep(0, nrow(E_loadings)), rep(0, nrow(E_resid)), dist)

  # Edge connectpoints:
  ECP <- matrix(NA, nrow(edge_df), 2)
  ECP[nrow(E_loadings) + nrow(E_resid) + seq_len(nrow(E_cor)), 1:2] <- 1.5 * pi
  ECP[seq_len(nrow(E_loadings)), 1] <- 1.5 * pi

  # Loop rotation:
  loopRotation <- 0.5*pi

  # bidirectional:
  bidir <- c(rep(FALSE, nrow(E_loadings) + nrow(E_resid)), rep(TRUE, nrow(E_cor)))

  # Shape:
  shape <- c(rep("circle", nFactor), rep("rectangle", nIndicator))

  # Size:
  size1 <- c(rep(12, nFactor), rep(30, nIndicator))
  size2 <- c(rep(12, nFactor), rep( 7, nIndicator))

  # Plot:
  label.scale.equal <- c(rep(1, nFactor),rep(2, nIndicator))

  path$plotObject <- .suppressGrDevice(qgraph::qgraph(
    input               = edge_df,
    layout              = layout_mat,
    directed            = TRUE,
    bidirectional       = bidir,
    residuals           = TRUE,
    residScale	        = 10,
    labels              = c(factors,labels),
    curve               = curve,
    curveScale          = FALSE,
    edgeConnectPoints   = ECP,
    loopRotation        = loopRotation,
    shape               = shape,
    vsize               = size1,
    vsize2              = size2,
    label.scale.equal   = label.scale.equal,
    residScale          = 2,
    mar                 = c(5,10,5,12),
    normalize           = FALSE,
    label.fill.vertical = 0.75,
    cut                 = options$highlightText,
    bg                  = "transparent"
  ))

}

.pcaAddComponentsToData <- function(jaspResults, modelContainer, options, ready) {
  if(!ready || !options[["addPC"]] || options[["PCPrefix"]] == "" || modelContainer$getError()) return()

  scores <- modelContainer[["model"]][["object"]][["scores"]]

  for (i in 1:ncol(scores)) {
    scorename <- paste0(options[["PCPrefix"]], "_", i)
    if (is.null(jaspResults[[scorename]])) {
      jaspResults[[scorename]] <- createJaspColumn(scorename)
      jaspResults[[scorename]]$dependOn(optionsFromObject = modelContainer)
      jaspResults[[scorename]]$setScale(scores[, i])
    }
  }
}
