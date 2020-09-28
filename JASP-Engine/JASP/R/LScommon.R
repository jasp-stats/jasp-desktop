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

# general functions
.evaluatePriors       <- function(priors){
  for(p in 1:length(priors)){
    for(i in 1:length(priors[[p]])){
      tempP <- priors[[p]][[i]]
      if (names(priors[[p]])[i] %in% c("parAlpha", "parBeta", "parPoint", "parMu", "parSigma", "PH")){
        priors[[p]][[paste0(names(priors[[p]])[i],"Inp")]] <- priors[[p]][[i]]
        priors[[p]][[i]] <- eval(parse(text = priors[[p]][[i]]))
      }
    }
    if (priors[[p]][["name"]] == ""){
      priors[[p]][["name"]] <- gettextf(
        "%s %i",
        ifelse(any(names(priors[[p]]) %in% c("PH")), "Hypothesis", "Model"),
        p
      )
    }
  }
  
  if (anyDuplicated(sapply(priors, function(p)p$name)) != 0){
    JASP:::.quitAnalysis(gettextf(
      "Please remove duplicates from the %s names.",
      ifelse(any(names(priors[[p]]) %in% c("PH")), "Hypotheses", "Models")
    ))
  }
  
  return(priors)
}
.scalePriors          <- function(priors){
  unscaled <- sapply(priors, function(x)x$PH)
  scaled   <- unscaled/sum(unscaled)
  for(i in 1:length(priors)){
    priors[[i]]$PH <- scaled[i]
  }
  return(priors)
}
.aproximateSupportLS   <- function(xSeq, seqTF){
  xStart <- NULL
  xEnd   <- NULL
  
  r <- rle(seqTF)
  
  if (length(r$values) > 0){
    for(i in 1:length(r$values)){
      if (r$values[i]){
        if (i == 1){
          xStart <- c(xStart, 1)
          xEnd   <- c(xEnd,   r$lengths[1])
        } else {
          xStart <- c(xStart, sum(r$lengths[1:(i-1)])+1)
          xEnd   <- c(xEnd,   sum(r$lengths[1:i]))
        }
      } 
    }
  } else {
    xStart <- NA
    xEnd   <- NA
  }
  
  return(cbind.data.frame("lCI" = xSeq[xStart], "uCI" = xSeq[xEnd]))
}
.cleanSequence        <- function(sequence){
  sequence <- gsub(",", "\n", sequence)
  sequence <- gsub(";", "\n", sequence)
  sequence <- unlist(strsplit(sequence, split = "\n"))
  sequence <- trimws(sequence, which = c("both"))
  sequence <- sequence[sequence != ""]
  return(sequence)
}

hdi.function   <- function(object, credMass=0.95, tol, ...)  {
  # adapted from HDIinterval:::hdi.function
  if(missing(tol))
    tol <- 1e-8
  if(class(try(object(0.5, ...), TRUE)) == "try-error")
    stop(paste("Incorrect arguments for the inverse cumulative density function",
               substitute(object)))
  # cf. code in Kruschke 2011 p630
  intervalWidth <- function( lowTailPr , ICDF , credMass , ... ) {
    ICDF( credMass + lowTailPr , ... ) - ICDF( lowTailPr , ... )
  }
  optInfo <- optimize( intervalWidth , c( 0 , 1.0 - credMass) , ICDF=object ,
                       credMass=credMass , tol=tol , ... )
  HDIlowTailPr <- optInfo$minimum
  result <- c(lower = object( HDIlowTailPr , ... ) ,
              upper = object( credMass + HDIlowTailPr , ... ) )
  attr(result, "credMass") <- credMass
  return(result)
}
hdi.density    <- function(object, credMass=0.95, allowSplit=FALSE, ...) {
  # adapted from HDIinterval:::hdi.density
  sorted = sort( object$y , decreasing=TRUE )
  heightIdx = min( which( cumsum( sorted) >= sum(object$y) * credMass ) )
  height = sorted[heightIdx]
  indices = which( object$y >= height )
  # HDImass = sum( object$y[indices] ) / sum(object$y)
  gaps <- which(diff(indices) > 1)
  if(length(gaps) > 0 && !allowSplit) {
    # In this case, return shortest 95% CrI
    warning("The HDI is discontinuous but allowSplit = FALSE;
    the result is a valid CrI but not HDI.")
    cumul <- cumsum(object$y) / sum(object$y)
    upp.poss <- low.poss <- which(cumul < 1 - credMass)
    for (i in low.poss)
      upp.poss[i] <- min(which(cumul > cumul[i] + credMass))
    # all(cumul[upp.poss] - cumul[low.poss] > credMass) # check
    width <- upp.poss - low.poss
    best <- which(width == min(width))  # usually > 1 value due to ties
    result <- c(lower = mean(object$x[low.poss[best]]),
                upper = mean(object$x[upp.poss[best]]))
  } else {
    begs <- indices[c(1, gaps + 1)]
    ends <- indices[c(gaps, length(indices))]
    result <- cbind(begin = object$x[begs], end = object$x[ends])
    if(!allowSplit)  {
      result <- as.vector(result)
      names(result) <- c("lower", "upper")
    }
  }
  attr(result, "credMass") <- credMass
  attr(result, "height") <- height
  return(result)
}

# plotting functions
.plotPriorPosteriorLS  <- function(allLines, allArrows, dfPoints = NULL, xName = NULL, xRange = c(0,1)){
  
  mappingArrow <- ggplot2::aes(x = x, xend = x, y = yStart, yend = yEnd, color = g)
  mappingLines <- ggplot2::aes(x = x, y = y, color = g)
  mappingPoint <- ggplot2::aes(x = x, y = y, color = g)
  
  if (!is.null(allLines))allLines   <- do.call("rbind", allLines)
  if (!is.null(allArrows))allArrows <- do.call("rbind", allArrows)
  
  # get the y_axis max
  yMax <- .getYMax(allLines, allArrows)
  
  g <- ggplot2::ggplot() 
  
  if (!is.null(allArrows)){
    
    for(i in nrow(allArrows):1){
      
      tempArrow       <- allArrows[i,]
      tempArrow$yEnd <- tempArrow$yEnd * .scalingSpikes(allLines, allArrows)
      
      g <- g + ggplot2::geom_segment(
        data        = tempArrow,
        mapping     = mappingArrow,
        size        = 1,
        linetype    = 1,
        arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
        show.legend = F) +
        ggplot2::geom_segment(
          data    = tempArrow,
          mapping = mappingArrow,
          size    = 1)
    }
    
    xHigh <- max(allArrows$x)
  }
  
  if (!is.null(allLines)){
    
    for(i in 1:length(unique(allLines$g))){
      
      tempLine <- allLines[allLines$g == unique(allLines$g)[i], ]
      tempType <- i
      
      g <- g + ggplot2::geom_line(
        data     = tempLine,
        mapping  = mappingLines,
        size     = 1,
        linetype = tempType)
    }
    
    xHigh <- allLines$x[which.max(allLines$y)]
  }
  
  g <- g + .plotXAxis(xName, xRange, FALSE)
  g <- g + .plotYAxis(allLines, allArrows, NULL)
  
  if (!is.null(dfPoints)){
    
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
    
    if (!is.null(allArrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c(c("black", "gray")[1:length(unique(allArrows$g))], "black"),
                                           breaks  = c(as.character(allArrows$g), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if (length(unique(allArrows$g)) == 1) c(1, NA) else c(1, 2, NA),
                                             shape    = if (length(unique(allArrows$g)) == 1) c(NA, 4) else c(NA, NA, 4)
                                           )))  
    } else {
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray", "black")[c(1:length(unique(allLines$g)), 3)],
                                           breaks  = c(unique(as.character(allLines$g)), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2, NA)[c(1:length(unique(allLines$g)), 3)],
                                             shape    = c(NA, NA,  4)[c(1:length(unique(allLines$g)), 3)]
                                           ))) 
    }
    
  } else {
    
    if (!is.null(allArrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray")[1:length(unique(allArrows$g))],
                                           breaks  = as.character(allArrows$g),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if (length(unique(allArrows$g)) == 1) c(1) else c(1, 2),
                                             shape    = if (length(unique(allArrows$g)) == 1) c(NA) else c(NA, NA)
                                           ))) 
    } else {
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c( "black", "gray")[1:length(unique(allLines$g))],
                                           breaks  = c(unique(as.character(allLines$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2)[1:length(unique(allLines$g))],
                                             shape    = c(NA, NA)[1:length(unique(allLines$g))]
                                           ))) 
    }
    
  }
  
  
  if (xHigh > .5) {
    legend.position = c(0.25, 1)
  } else {
    legend.position = c(0.75, 1)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  g <- g + JASPgraphs::geom_rangeframe(sides = if (!is.null(allLines) & !is.null(allArrows)) "lbr" else "lb") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(allLines, allArrows)
  
  plot <- g
  
  return(plot)
}
.plotOverlyingLS       <- function(allLines, allArrows, dfPoints = NULL, pointEstimate = NULL, CI = NULL, xName = NULL, yName = NULL,
                                   xRange = c(0,1), palette = "colorblind", noLegend = FALSE, nRound = 3, discrete = FALSE,
                                   proportions = FALSE){
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = yStart, yend = yEnd, group = g, color = g)
  mappingArrows1 <- ggplot2::aes(x = xStart , xend = xEnd, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = xEnd , xend = xStart, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  if (!is.null(allLines))allLines   <- do.call("rbind", allLines)
  if (!is.null(allArrows))allArrows <- do.call("rbind", allArrows)
  
  # get the y_axis max
  yMax <- .getYMax(allLines, allArrows)
  
  # set the CI text
  # set the CI text
  if (!is.null(CI) || !is.null(pointEstimate)){
    # text for the interval
    tempLabel <- .CIlabelLS(CI, nRound, pointEstimate)
  } else {
    tempLabel <- NULL
  }
  
  if (!is.null(CI)){
    CI         <- cbind.data.frame(CI, "y" = yMax * 1.05)
  }
  
  if (discrete){
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(ceiling(xRange[1]),floor(xRange[2])))
    xBreaks[length(xBreaks)] <- floor(xRange[2])
    if (!proportions){
      xBreaks <- round(xBreaks)
    }
  } else {
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xRange)
  }
  
  g <- ggplot2::ggplot()
  
  if (!is.null(allArrows)){
    
    allArrowsScaled       <- allArrows
    allArrowsScaled$yEnd  <- allArrowsScaled$yEnd * .scalingSpikes(allLines, allArrows)
    
    if (!is.null(pointEstimate)){
      if (pointEstimate$spike[1]){
        pointEstimate$y <- pointEstimate$y  * .scalingSpikes(allLines, allArrows)
      }
    }
    
    g <- g + ggplot2::geom_segment(
      data        = allArrowsScaled,
      mapping     = mappingArrows,
      size        = 1,
      arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      show.legend = F)
    g <- g + ggplot2::geom_segment(
      data    = allArrowsScaled,
      mapping = mappingArrows,
      size    = 1)
  }
  
  if (!is.null(allLines)){
    g <- g + ggplot2::geom_line(data = allLines, mapping = mappingLines, size = 1,)
  }
  
  if (!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  if (!is.null(pointEstimate)){
    if (!anyNA(pointEstimate$x)){
      g <- g + ggplot2::geom_point(data = pointEstimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  if (noLegend == TRUE){
    g <- g + ggplot2::scale_colour_manual(values = "black")
  } else {
    g <- g + JASPgraphs::scale_JASPcolor_discrete(palette)
  }
  
  # axes
  g <- g + .plotXAxis(xName, xRange, discrete)
  g <- g + .plotYAxis(allLines, allArrows, if (!is.null(CI) || !is.null(pointEstimate)) "notNull" else NULL)
  
  # legend
  if (!is.null(allLines)){
    xr   <- range(allLines$x)
    idx  <- which.max(allLines$y)
    xmax <- allLines$x[idx]
  } else {
    xr   <- range(allArrows$x)
    idx  <- which.max(allArrows$yEnd)
    xmax <- allArrows$x[idx]
  }
  
  if (!is.null(CI)){
    if (!is.na(CI$xStart[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
  }
  
  if (!is.null(CI) || !is.null(pointEstimate)){
    labelY    <- if (length(tempLabel) == 1) 1.10 else 1.25 - .07 * c(1:length(tempLabel)) 
    for(i in 1:length(tempLabel)){
      
      tempText <- data.frame(
        label = tempLabel[i],
        x = (xRange[1] + xRange[2])/2,
        y = yMax * labelY[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = tempText,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
    }
  }
  
  
  if (xmax > mean(xr)) {
    legend.position = c(0.2, 0.8)
  } else {
    legend.position = c(0.8, 0.8)
  }
  
  if (noLegend == FALSE){
    g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  } else {
    g <- g + JASPgraphs::themeJaspRaw()
  }
  g <- g + JASPgraphs::geom_rangeframe(sides = if (!is.null(allLines) & !is.null(allArrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(allLines, allArrows)
  
  plot <- g
  
  return(plot)
}
.plotStackedLS         <- function(allLines, allArrows, legend, dfPoints = NULL, xName = NULL, yName = gettext("Density"),
                                   xRange = c(0,1), lCI = NULL, uCI = NULL, discrete = FALSE, proportions = FALSE){
  
  mappingLines  <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows <- ggplot2::aes(x = x , xend = x, y = yStart, yend = yEnd, group = g, color = g)
  mappingLegend <- ggplot2::aes(x = x, y = y, label = name)
  mappingPoint  <- ggplot2::aes(x = x, y = y)
  
  
  if (!is.null(allLines)){
    
    allLinesD <- allLines
    allLinesL <- allLines
    
    for(i in 1:length(allLinesD)){
      if (is.null(lCI) & is.null(uCI)){
        allLinesD[[i]] <- rbind.data.frame(
          data.frame(x = xRange[1], y = 0, g = allLinesD[[i]]$g[1]),
          allLinesD[[i]],
          data.frame(x = xRange[2], y = 0, g = allLinesD[[i]]$g[1])     
        )
      } else {
        allLinesD[[i]] <- rbind.data.frame(
          data.frame(x = lCI, y = 0, g = allLinesD[[i]]$g[1]),
          allLinesD[[i]][allLinesD[[i]]$x > lCI & allLinesD[[i]]$x < uCI,],
          data.frame(x = uCI, y = 0, g = allLinesD[[i]]$g[1])     
        )
      }
      allLinesL[[i]] <- data.frame(x = xRange, y = rep(0, 2), g = allLinesD[[i]]$g[1] )
    }
    
    allLines  <- do.call("rbind", allLines)
    allLinesD <- do.call("rbind", allLinesD)
    allLinesL <- do.call("rbind", allLinesL)
  }
  
  if (!is.null(allArrows)){
    
    allArrowsL <- list()
    for(i in 1:length(allArrows)){
      allArrowsL[[i]] <- data.frame(y = rep(allArrows[[i]]$yStart, 2), x = xRange,
                                     g = rep(allArrows[[i]]$g, 2))
    }
    
    allArrows <- do.call("rbind", allArrows)
    allArrowsL<- do.call("rbind", allArrowsL)
  }
  
  legend      <- data.frame(legend)
  colnames(legend) <- c("type", "name")
  legend$type <- as.character(legend$type)
  legend$name <- as.character(legend$name)
  
  if (!is.null(allLines)){
    obsYmax <- max(allLines$y)
    if (!is.null(allArrows)){
      allArrows$yEnd <- obsYmax
    }
  } else {
    obsYmax <- max(allArrows$yEnd)    
  }
  yBreak  <- obsYmax/3 
  newymax <- obsYmax + yBreak*nrow(legend)
  
  legend$y <- yBreak*(0:(nrow(legend)-1))
  legend$x <- xRange[1]
  
  # changing y-coordinates to "stack" the plots
  for(i in 1:nrow(legend)){
    if (legend$type[i] == "spike"){
      allArrows[allArrows$g == legend[i,2], "yStart"] <- allArrows[allArrows$g == legend[i,2], "yStart"] + yBreak*(i-1)
      allArrows[allArrows$g == legend[i,2], "yEnd"]   <- allArrows[allArrows$g == legend[i,2], "yEnd"]   + yBreak*(i-1)
      allArrowsL[allArrowsL$g == legend[i,2], "y"]     <- allArrowsL[allArrowsL$g == legend[i,2], "y"]     + yBreak*(i-1)
    } else if (legend$type[i] %in% c("beta", "normal")){
      allLines[allLines$g == legend[i,2], "y"]   <- allLines[allLines$g == legend[i,2], "y"]   + yBreak*(i-1)
      allLinesD[allLinesD$g == legend[i,2], "y"] <- allLinesD[allLinesD$g == legend[i,2], "y"] + yBreak*(i-1)
      allLinesL[allLinesL$g == legend[i,2], "y"] <- allLinesL[allLinesL$g == legend[i,2], "y"] + yBreak*(i-1)
    }
  }
  
  g <- ggplot2::ggplot()
  
  for(i in nrow(legend):1){
    if (legend$type[i] == "spike"){
      g <- g + ggplot2::geom_segment(
        data = allArrows[allArrows$g == legend$name[i],],
        mapping = mappingArrows, size = 1,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
        ggplot2::geom_line(
          data = allArrowsL[allArrowsL$g == legend$name[i],],
          mapping = mappingLines)
    }
    if (legend$type[i] %in% c("beta", "normal")){
      g <- g + ggplot2::geom_line(
        data = allLines[allLines$g == legend$name[i],],
        mapping = mappingLines, size = 1) + 
        ggplot2::geom_polygon(
          data = allLinesD[allLinesD$g == legend$name[i],],
          mapping = mappingLines, fill = "grey60", alpha = .8)
      
      if (!is.null(lCI) & !is.null(uCI)){
        g <- g + ggplot2::geom_line(
          data = allLinesL[allLinesL$g == legend$name[i],],
          mapping = mappingLines
        )
      }
      
    }
  }
  
  if (!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  #legend$name <- sapply(legend$name, function(x)paste(c(x, "    "), collapse = ""))
  #g <- g + ggplot2::geom_text(data = legend, mapping = mappingLegend,
  #                            size = 8, hjust = 1, vjust = 0)
  
  g <- g + ggplot2::scale_colour_manual(values = rep("black", nrow(legend))) +
    .plotXAxis(xName, xRange, discrete) +
    ggplot2::scale_y_continuous(yName, limits = c(0, newymax),breaks = legend$y, labels = legend$name) + 
    ggplot2::coord_cartesian(clip = 'off')
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'b') + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
      
      axis.line.y  = ggplot2::element_blank(),
      #axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
      #plot.margin  = ggplot2::unit(c(0,0,0,max(sapply(legend$name,nchar))/60), "npc")
    )
  
  plot <- g
  
  return(plot)
}
.plotIterativeLS       <- function(allLines, allCI, xName = "Observations", yName = NULL, xStart = 0,
                                   palette = "colorblind", BFlog = NULL, yRange = NULL){
  
  allLines      <- do.call("rbind", allLines)
  allLines$name <- factor(allLines$name, levels = sort(levels(allLines$name)))
  
  obsXmax    <- max(allLines$x)
  newXmax    <- obsXmax
  if (obsXmax > 10){
    xBreaks <- round(seq(xStart, obsXmax, length.out = 7))
  } else {
    xBreaks <- xStart:obsXmax
  }
  
  if (is.null(yRange)){
    if (is.null(BFlog)){
      yRange <- c(0, 1)
    } else if (BFlog){
      yRange <- range(c(allLines$y, 0))
    } else if (!BFlog){
      yRange <- range(c(allLines$y, 1))
    }    
  }
  
  
  mappingLines   <- ggplot2::aes(x = x, y = y, 
                                 group = name, color = name)
  mappinglCI     <- ggplot2::aes(x = x, y = y1, 
                                 group = name, color = name)
  mappinguCI     <- ggplot2::aes(x = x, y = y2, 
                                 group = name, color = name)
  mappingPolygon <- ggplot2::aes(x = x, y = y, group = name, fill = name)
  
  clr  <- scales::gradient_n_pal(JASPgraphs::JASPcolors(palette))(seq(0, 1, length.out = length(unique(allLines$name))))
  #clr  <- JASPgraphs::colorBrewerJasp(n = length(unique(allLines$name)))
  if (length(allCI) > 0){
    namesCI <- NULL
    for(i in 1:length(allCI)){
      namesCI <- c(namesCI, as.character(unique(allCI[[i]]$name)))
    }
    clr1 <- clr[order(order(namesCI))]
  }
  
  
  g <- ggplot2::ggplot()
  
  if (length(allCI) > 0){
    for(i in length(allCI):1){
      if (is.null(allCI[[i]]))next
      tempData <- allCI[[i]]
      tempPoly <- data.frame(
        x = c(tempData$x, rev(tempData$x)),
        y = c(tempData$y1, rev(tempData$y2)),
        name = rep(tempData$name,2)
      )
      
      g <- g + 
        ggplot2::geom_polygon(
          data    = tempPoly,
          mapping = mappingPolygon, fill = clr1[i], alpha = .3) +
        ggplot2::geom_path(
          data    = tempData,
          mapping = mappinguCI, size = 1, linetype = 2) +
        ggplot2::geom_path(
          data    = tempData,
          mapping = mappinglCI, size = 1, linetype = 2)
    }
  }
  
  if (!is.null(BFlog)){
    if (BFlog){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(0, 0)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    } else if (!BFlog){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(1, 1)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    }
  }
  
  g <- g +
    ggplot2::geom_line(
      data    = allLines,
      mapping = mappingLines, size = 1)
  
  g <- g + .plotXAxis(xName, c(xStart, newXmax), TRUE)
  g <- g + .plotYAxis2(yName, yRange)
  g <- g + ggplot2::scale_colour_manual(values = clr)
  
  
  if (mean(allLines$y[allLines$x == max(allLines$x)]) > .5) {
    legend.position = c(0.8, 0.03 + length(unique(allLines$name))/10)
  } else {
    legend.position = c(0.8, 1.03)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position) + 
    JASPgraphs::geom_rangeframe(sides = 'lb') + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 2, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
    )
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotIndividualLS      <- function(allLines, allArrows, pointEstimate, CI, CIallLines, dfPoints = NULL, xRange, xName, yName = NULL, nRound = 3){ 
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g,)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = yStart, yend = yEnd, group = g)
  mappingArrows1 <- ggplot2::aes(x = xStart , xend = xEnd, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = xEnd , xend = xStart, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  # get the y_axis max
  yMax <- .getYMax(allLines, allArrows)
  
  # set the CI text
  if (!is.null(CI) || !is.null(pointEstimate)){
    # text for the interval
    tempLabel <- .CIlabelLS(CI, nRound, pointEstimate)
  } else {
    tempLabel <- NULL
  }
  if (!is.null(CI))
    CI <- cbind.data.frame(CI, "y" = yMax * 1.05)
  
  g <- ggplot2::ggplot()
  
  if (!is.null(allArrows)){
    
    tempArrows        <- allArrows
    tempArrows$yEnd  <- tempArrows$yEnd * .scalingSpikes(allLines, allArrows)
    
    g <- g + ggplot2::geom_segment(
      data    = allArrows,
      mapping = mappingArrows, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black")
  }
  
  if (!is.null(allLines)){
    if (!is.null(CIallLines)){
      g <- g + ggplot2::geom_polygon(
        data = CIallLines,
        mapping = mappingLines, fill = "grey60", alpha = .8)
    }
    g <- g + ggplot2::geom_line(
      data    = allLines,
      mapping = mappingLines, size = 1, color = "black") 
  }
  
  if (!is.null(CI)){
    if (!is.na(CI$xStart[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
  }
  
  if (!is.null(CI) || !is.null(pointEstimate)){
    labelY    <- if (length(tempLabel) == 1) 1.10 else 1.25 - .07 * c(1:length(tempLabel)) 
    for(i in 1:length(tempLabel)){
      
      tempText <- data.frame(
        label = tempLabel[i],
        x = (xRange[1] + xRange[2])/2,
        y = yMax * labelY[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = tempText,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
    }
  }
  
  if (!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  if (!is.null(pointEstimate)){
    if (!anyNA(pointEstimate$x)){
      g <- g + ggplot2::geom_point(data = pointEstimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  # x-axes
  g <- g + .plotXAxis(xName, xRange, FALSE)
  g <- g + .plotYAxis(allLines, allArrows, tempLabel)
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = if (!is.null(allLines) & !is.null(allArrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(allLines, allArrows)
  
  plot <- g
  return(plot)
}
.plotPredictionLS      <- function(dfHist, pointEstimate, CI, xRange, xName, yName, nRound = 0, xBlacked = NULL,
                                   proportions = FALSE, predictionN = NULL){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  mappingArrows1    <- ggplot2::aes(x = xStartAdj , xend = xEndAdj, y = y, yend = y, group = g)
  mappingArrows2    <- ggplot2::aes(x = xEndAdj,  xend = xStartAdj, y = y, yend = y, group = g)
  mappingText       <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint      <- ggplot2::aes(x = x, y = y)
  
  if (!is.null(CI) || !is.null(pointEstimate)){
    # text for the interval
    tempLabel <- .CIlabelLS(CI, nRound, pointEstimate)
    yMaxMultiplier <- ifelse(length(tempLabel) == 1, 1.15, 1.25)
  } else {
    tempLabel <- NULL
  }
  
  if (proportions){
    xBreaks    <- JASPgraphs::getPrettyAxisBreaks(xRange)
    xBreaks[1] <- 0
    xBreaks[length(xBreaks)] <- 1
  } else {
    xBreaks  <- round(JASPgraphs::getPrettyAxisBreaks(xRange))
    xBreaks[length(xBreaks)] <- predictionN
  }
  
  
  if (xBreaks[length(xBreaks)] > xRange[2])xBreaks[length(xBreaks)] <- xRange[2]
  
  obsYmax    <- max(dfHist$y)
  if (all(round(dfHist$y[1], 5) == round(dfHist$y, 5)))
    obsYmax <- obsYmax * 1.2
  yBreaks    <- JASPgraphs::getPrettyAxisBreaks(c(0, obsYmax))
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(ifelse(!is.null(CI) || !is.null(pointEstimate), yMaxMultiplier + .05, 1.10) * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  if (!is.null(CI)){
    
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.10)
    
    if (!proportions){
      CI$xStartAdj <- CI$xStart - .5
      CI$xEndAdj   <- CI$xEnd   + .5
    } else {
      CI$xStartAdj <- CI$xStart - .5/(predictionN + 1)
      CI$xEndAdj   <- CI$xEnd   + .5/(predictionN + 1)
    }
    
    for(i in 1:nrow(CI)){
      dfHist$col[dfHist$x >= CI$xStart[i] & dfHist$x <= CI$xEnd[i]] <- "b"
    }
  }
  if (!is.null(xBlacked))
    dfHist[dfHist$x == xBlacked,"col"] <- "c"
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_bar(
    data     = dfHist,
    mapping  = mappingHistogram,
    #fill     = "grey",
    col      = "black",
    stat     = "identity"
  )
  
  
  if (!is.null(CI)){
    if (!is.na(CI$xStart[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
  }
  
  if (!is.null(CI) || !is.null(pointEstimate)){
    r <- 0
    for(i in 1:length(tempLabel)){
      
      tempText <- data.frame(
        label = tempLabel[i],
        x = (xRange[1] + xRange[2])/2,
        y = obsYmax * (yMaxMultiplier-r)
      )
      
      g <- g + ggplot2::geom_text(
        data    = tempText,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
      
      r <- r + .10
    }
  }
  
  if (!is.null(pointEstimate)){
    if (!anyNA(pointEstimate$x)){
      g <- g + ggplot2::geom_point(data = pointEstimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  # control fill
  if (is.null(CI)){
    fillColor <- c("grey90") 
  } else {
    if (nrow(CI) == 1){
      if (all(xRange[1]:xRange[2] %in% CI$xStart:CI$xEnd)){
        fillColor <- c("grey50") 
      } else {
        fillColor <- c("grey90", "grey50") 
      }
    } else {
      if (all(xRange[1]:xRange[2] %in% c(unlist(sapply(1:nrow(CI), function(i)CI$xStart[i]:CI$xEnd[i]))))){
        fillColor <- c("grey50") 
      } else {
        fillColor <- c("grey90", "grey50") 
      }
    }
  }
  if (!is.null(xBlacked))
    fillColor <- c(fillColor, "steelblue")
  
  
  if (!proportions){
    g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5, xRange[2] + .5))
  } else {
    g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5/(predictionN+1), xRange[2] + .5/(predictionN+1)))
  }
  g <- g + ggplot2::scale_y_continuous(yName, breaks = yBreaks, limits = c(0, newymax)) 
  g <- g + ggplot2::scale_colour_manual(values = fillColor, aesthetics = "fill")
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'lb') +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"))
  
  plot <- g
  return(plot)
}
.plotAccuracyLS        <- function(dfHist, xName = xName, yName = yName){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  
  yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, dfHist$y))
  xBreaks  <- 1:nrow(dfHist)
  xRange   <- c(.5, nrow(dfHist) + .5)
  
  obsYmax  <- max(dfHist$y)
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(1.10 * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_bar(
    data     = dfHist,
    mapping  = mappingHistogram,
    #fill     = "grey",
    col      = "black",
    stat     = "identity"
  )
  
  
  g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, labels = dfHist$g, limits = xRange)
  g <- g + ggplot2::scale_y_continuous(yName, breaks = yBreaks, limits = c(0, newymax)) 
  g <- g + ggplot2::scale_colour_manual(values = "grey90", aesthetics = "fill")
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'lb') +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
      axis.text.x       = if (nrow(dfHist) > 3) ggplot2::element_text(angle = 45))
  
  plot <- g
  return(plot)
  
}

# support functions
.CIlabelLS            <- function(CI, nRound, PE = NULL){
  
  if (!is.null(CI)){
    tempInt <- sapply(1:nrow(CI), function(i){
      if (is.na(CI$xStart[i])){
        x <- "none"
        #x <- "~symbol(\\306)"
      } else if (CI$xStart[i] == CI$xEnd[i]){
        x <- paste(c(
          "[",format(round(CI$xStart[i], nRound), nsmall = nRound),"]"
        ), collapse = "")
      } else {
        x <- paste(c(
          "[",format(round(CI$xStart[i], nRound), nsmall = nRound),", ",format(round(CI$xEnd[i], nRound), nsmall = nRound),"]"
        ), collapse = "")
      }
      return(x)
    }
    )
    tempInt <- paste(tempInt, collapse = " and " )
    tempInt <- paste("'",tempInt,"'")
    
    # text for the coverage
    tempCov <- paste0(c("'",round(CI$coverage[1]*100), "% CI'"), collapse = "")
    
    
    if (CI$g[1] == "HPD"){
      tempLabel <- paste(c(tempCov,"['HPD']:",tempInt), collapse = "")
    } else if (CI$g[1] == "custom"){
      tempLabel  <- paste(c("P({",format(round(CI$xStart, nRound), nsmall = nRound),"<=",if (CI$parameter == "theta") "theta" else if (CI$parameter == "mu") "mu","}<=",
                             (format(round(CI$xEnd, nRound), nsmall = nRound)),")","=='",round(CI$coverage[1]*100),"%'"), collapse = "")
    } else if (CI$g[1] == "support"){
      tempLabel <- paste(c("SI['[BF = ",CI$BF[1],"]']:",tempInt), collapse = "")
    } else if (CI$g[1] == "central"){
      tempLabel <- paste(c(tempCov,":",tempInt), collapse = "")
    }
    
  } else {
    tempLabel <- NULL
  }
  
  if (!is.null(PE)){
    if (nrow(PE) > 1)
      PE <- PE[1,]
    PEl <- PE$l
    if (is.numeric(PE$l))
      PEl <- format(round(PEl, ifelse(PE$estimate == "mean", 3, nRound)), nsmall = ifelse(PE$estimate == "mean", 3, nRound))
    tempPe    <- paste0("'", PE$estimate,"'",  "==", "' ", PEl, ifelse(is.null(tempLabel), " '", "; '"))
    if (!is.null(tempLabel)){
      tempLabel <- paste(tempPe, tempLabel, sep = "~")
    } else {
      tempLabel <- tempPe
    }
  }
  
  if (nchar(tempLabel) > 75){
    tempO <- gregexpr(" and", substr(tempLabel, 1, 65))
    tempLabel <- c(paste(substr(tempLabel, 1, tempO[[1]][length(tempO[[1]])]-1), "'", sep = ""), 
                    paste("'",substr(tempLabel, tempO[[1]][length(tempO[[1]])], nchar(tempLabel)), sep = ""))
  }
  
  return(tempLabel)
}
.getYMax               <- function(allLines, allArrows){
  if (!is.null(allLines)){
    maxXLines <- max(allLines$y)
    if (all(round(allLines$y[1], 5) == round(allLines$y, 5)))
        maxXLines <- maxXLines * 1.2
  }
    
  
  if (!is.null(allLines) & !is.null(allArrows)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, maxXLines))
    yMax     <- max(c(allLines$y, yBreaks))
  } else if (!is.null(allLines)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, maxXLines))
    yMax     <- max(c(allLines$y, yBreaks))
  } else {
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(allArrows$yEnd)))
    yMax     <- max(c(allArrows$yEnd, yBreaks))
  }
  return(yMax)
}
.scalingSpikes         <- function(allLines, allArrows){
  if (!is.null(allLines) & !is.null(allArrows)){
    yMax     <- .getYMax(allLines, allArrows)
    yBreaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(allArrows$yEnd)))
    return(yMax/max(yBreaks2))
  } else {
    return(1)
  }
}
.plotXAxis             <- function(xName, xRange, discrete){
  
  xBreaks <- JASPgraphs::getPrettyAxisBreaks(xRange)
  
  if (discrete){
    xBreaks <- round(xBreaks)
    xBreaks <- unique(xBreaks[xBreaks >= xRange[1] &  xBreaks <= xRange[2]])
    if (xBreaks[1] > ceiling(xRange[1]))
      xBreaks <- c(ceiling(xRange[1]), xBreaks)
    if (xBreaks[length(xBreaks)] < floor(xRange[2]))
      xBreaks <- c(xBreaks, floor(xRange[2]))
  }
  xRange <- range(c(xRange, xBreaks))
  
  return(ggplot2::scale_x_continuous(xName, limits = xRange, breaks = xBreaks))
}
.plotYAxis             <- function(allLines, allArrows, CI){
  
  yMax <- .getYMax(allLines, allArrows)
  
  if (!is.null(allLines) & !is.null(allArrows)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, yMax))
    yBreaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(allArrows$yEnd)))
    yPos2    <- yBreaks2/(max(yBreaks2)/yMax)
  } else if (!is.null(allLines)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, yMax))
  } else {
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, yMax))
  }
  
  # set the y-scale plotting range
  if (!is.null(CI)){
    yRange    <- c(0, yMax * 1.20) 
  } else {
    yRange    <- c(0, yMax) 
  }
  
  if (!is.null(allLines) & !is.null(allArrows)){
    return(ggplot2::scale_y_continuous(
      gettext("Density"),
      breaks = yBreaks,
      limits = yRange,
      sec.axis = ggplot2::sec_axis(
        ~ .,
        name   = gettext("Probability"),
        breaks = yPos2,
        labels = yBreaks2)
    ))
  } else {
    return(ggplot2::scale_y_continuous(
      ifelse(is.null(allLines), gettext("Probability"), gettext("Density")),
      breaks = yBreaks,
      limits = yRange
    ))
  }
}
.plotYAxis2            <- function(yName, yRange){
  
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(yRange)
  yRange  <- range(c(yRange, yBreaks))
  
  return(ggplot2::scale_y_continuous(yName, limits = yRange, breaks = yBreaks))
}
.plotThemePlus         <- function(allLines, allArrows){
  if (!is.null(allLines) & !is.null(allArrows)){
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3),
        plot.margin = ggplot2::margin(t = 3, r = 10, b = 0, l = 1))
    )    
  } else {
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3.5),
        plot.margin = ggplot2::margin(t = 3, r = 0, b = 0, l = 1))
    )
  }
}

# containers and common outputs
.introductoryTextLS            <- function(jaspResults, options, analysis){
  
  if (!is.null(jaspResults[['introText']])) return()
  
  intro <- createJaspHtml()
  intro$dependOn(c("introText"))
  intro$position <- 0
  
  intro[['text']] <- .explanatoryTextLS("main", NULL, analysis)
  
  jaspResults[['introText']] <- intro
  
  return()  
}



.estimatesContainerLS          <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["estimatesContainer"]])){
    estimatesContainer <- createJaspContainer("Model")
    estimatesContainer$position <- 2
    estimatesContainer$dependOn("pointEstimate")
    jaspResults[["estimatesContainer"]] <- estimatesContainer 
  } else {
    estimatesContainer <- jaspResults[["estimatesContainer"]]
  }
  
  
  if (options[["introText"]] && is.null(estimatesContainer[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("estimates", options, analysis)
    
    estimatesContainer[['introText']] <- introText    
  }
  
  return(estimatesContainer)
}
.containerPlotsLS              <- function(jaspResults, options, analysis, type){
  
  if (is.null(jaspResults[[paste0("containerPlots", type)]])){
    containerPlots <- createJaspContainer(title = gettextf(
      "%1$s %2$s Plots", 
      switch(
        options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]],
        "overlying" = gettext("All"),
        "stacked"   = gettext("Stacked"),
        "individual"= gettext("Individual")
      ),
      type))
    containerPlots$dependOn(c(
      ifelse(type == "Prior", "plotsPrior", "plotsPosterior"),
      ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")
    ))
    containerPlots$position <- ifelse(type == "Prior", 3, 4)
    jaspResults[[paste0("containerPlots", type)]] <- containerPlots 
  } else {
    containerPlots <- jaspResults[[paste0("containerPlots", type)]]
  }
  
  
  if (options[["introText"]] && is.null(containerPlots[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("parameter_plots", options, analysis, type)
    
    containerPlots[['introText']] <- introText    
  }
  
  return(containerPlots)
}
.containerPlotsBothLS          <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerBoth"]])){
    containerBoth <- createJaspContainer(title = gettext("Prior and Posterior Plots"))
    containerBoth$position <- 5
    containerBoth$dependOn("plotsBoth")
    
    jaspResults[["containerBoth"]] <- containerBoth
  } else {
    containerBoth <- jaspResults[["containerBoth"]]
  }
  
  
  if (options[["introText"]] && is.null(containerBoth[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("both_plots", options, analysis)
    
    containerBoth[['introText']] <- introText    
  }
  
  return(containerBoth)
}
.containerSequentialPointLS    <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerIterative"]])){
    containerIterative <- createJaspContainer(title = gettextf(
      "%s Sequential Analysis: Point Estimate",
      switch(
        options[["plotsIterativeType"]],
        "overlying" = gettext("All"),
        "stacked"   = gettext("Stacked"),
        "individual"= gettext("Individual")
      )))
    containerIterative$position <- 6
    containerIterative$dependOn(c("plotsIterative", "plotsIterativeType", "plotsIterativeEstimateType"))
    
    jaspResults[["containerIterative"]] <- containerIterative
  } else {
    containerIterative <- jaspResults[["containerIterative"]]
  }
  
  
  if (options[["introText"]] && is.null(containerIterative[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("sequential_point", options, analysis)
    
    containerIterative[['introText']] <- introText    
  }
  
  return(containerIterative)
}
.containerSequentialIntervalLS <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerIterativeInterval"]])){
    containerIterativeInterval <- createJaspContainer(title = gettextf(
      "%s Sequential Analysis: Interval",
      switch(
        options[["plotsIterativeIntervalType"]],
        "overlying" = gettext("All"),
        "stacked"   = gettext("Stacked"),
        "individual"= gettext("Individual")
      )))
    containerIterativeInterval$position <- 7
    containerIterativeInterval$dependOn(c("plotsIterativeInterval", "plotsIterativeIntervalType"))
    
    jaspResults[["containerIterativeInterval"]] <- containerIterativeInterval
  } else {
    containerIterativeInterval <- jaspResults[["containerIterativeInterval"]]
  }
  
  
  if (options[["introText"]] && is.null(containerIterativeInterval[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn(c("introText", "plotsIterativeIntervalType"))
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("sequential_interval", options, analysis)
    
    containerIterativeInterval[['introText']] <- introText    
  }
  
  return(containerIterativeInterval)
}
.containerSequentialUpdatingLS <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerIterativeUpdating"]])){
    containerIterativeUpdating <- createJaspContainer(title = gettext("Sequential Posterior Updating"))
    containerIterativeUpdating$position <- 8
    containerIterativeUpdating$dependOn("doIterative")
    
    jaspResults[["containerIterativeUpdating"]] <- containerIterativeUpdating
  } else {
    containerIterativeUpdating <- jaspResults[["containerIterativeUpdating"]]
  }
  
  
  if (options[["introText"]] && is.null(containerIterativeUpdating[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("sequential_updating", options, analysis)
    
    containerIterativeUpdating[['introText']] <- introText    
  }
  
  return(containerIterativeUpdating)
}
.containerPredictionsLS        <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerPredictions"]])){
    containerPredictions <- createJaspContainer(title = gettext("Prediction Summary"))
    containerPredictions$position <- 9
    containerPredictions$dependOn(c("predictionTable", "predictionTableEstimate"))
    
    jaspResults[["containerPredictions"]] <- containerPredictions
  } else {
    containerPredictions <- jaspResults[["containerPredictions"]]
  }
  
  
  if (options[["introText"]] && is.null(containerPredictions[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("predictions", options, analysis)
    
    containerPredictions[['introText']] <- introText    
  }
  
  return(containerPredictions)
}
.containerPredictionPlotsLS    <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerPredictionPlots"]])){
    containerPredictionPlots <- createJaspContainer(title = gettextf(
      "%s Prediction Plots",
      switch(
        options[["predictionPlotType"]],
        "overlying" = gettext("All"),
        "stacked"   = gettext("Stacked"),
        "individual"= gettext("Individual")
      )))
    containerPredictionPlots$position <- 10
    containerPredictionPlots$dependOn(c("plotsPredictions", "predictionPlotType"))
    
    jaspResults[["containerPredictionPlots"]] <- containerPredictionPlots
  } else {
    containerPredictionPlots <- jaspResults[["containerPredictionPlots"]]
  }
  
  
  if (options[["introText"]] && is.null(containerPredictionPlots[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("prediction_plots", options, analysis)
    
    containerPredictionPlots[['introText']] <- introText    
  }
  
  return(containerPredictionPlots)
}
.containerPlots2LS             <- function(jaspResults, options, analysis, type){
  
  if (is.null(jaspResults[[paste0("containerPlots", type)]])){
    containerPlots <- createJaspContainer(title = gettextf(
      "%1$s %2$s Plots",
      switch(
        options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]],
        "conditional" = gettext("Conditional"),
        "joint"       = gettext("Joint"),
        "marginal"    = gettext("Marginal")
      ),
      type))
    containerPlots$dependOn(c(
      ifelse(type == "Prior", "plotsPrior", "plotsPosterior"),
      ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")
    ))
    containerPlots$position <- ifelse(type == "Prior", 3, 6)
    jaspResults[[paste0("containerPlots", type)]] <- containerPlots 
  } else {
    containerPlots <- jaspResults[[paste0("containerPlots", type)]]
  }
  
  
  if (options[["introText"]] && is.null(containerPlots[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("parameter_plots", options, analysis, type)
    
    containerPlots[['introText']] <- introText    
  }
  
  return(containerPlots)
}
.containerPrediction2PlotsLS   <- function(jaspResults, options, analysis, type){
  
  if (is.null(jaspResults[[paste0("containerPlotsPrediction", type)]])){
    containerPlots <- createJaspContainer(title = gettextf(
      "%1$s %2$s Prediction Plots",
      switch(
        options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]],
        "conditional" = gettext("Conditional"),
        "joint"       = gettext("Joint"),
        "marginal"    = gettext("Marginal")
      ),
      type))
    containerPlots$dependOn(c(
      ifelse(type == "Prior", "plotsPredictions",       "plotsPredictionsPost"),
      ifelse(type == "Prior", "plotsPredictionType",    "plotsPredictionPostType"),
      if (type == "Posterior") "predictionN"
    ))
    containerPlots$position <- ifelse(type == "Prior", 4, 10)
    jaspResults[[paste0("containerPlotsPrediction", type)]] <- containerPlots 
  } else {
    containerPlots <- jaspResults[[paste0("containerPlotsPrediction", type)]]
  }
  
  
  if (options[["introText"]] && is.null(containerPlots[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn(c("introText", ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")))
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("prediction_plots", options, analysis, type)
    
    containerPlots[['introText']] <- introText    
  }
  
  return(containerPlots)
}
.containerPlotsBoth2LS         <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerBoth"]])){
    containerBoth <- createJaspContainer(title = gettextf(
      "%s Prior and Posterior Plots",
      switch(
        options[["plotsBothType"]],
        "conditional" = gettext("Conditional"),
        "joint"       = gettext("Joint"),
        "marginal"    = gettext("Marginal")
      )))
    containerBoth$position <- 7
    containerBoth$dependOn(c("plotsBoth", "plotsBothType"))
    
    jaspResults[["containerBoth"]] <- containerBoth
  } else {
    containerBoth <- jaspResults[["containerBoth"]]
  }
  
  
  if (options[["introText"]] && is.null(containerBoth[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("both_plots", options, analysis)
    
    containerBoth[['introText']] <- introText    
  }
  
  return(containerBoth)
}
.containerPredictiveAccuracyLS <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerPredictiveAccuracy"]])){
    containerPredictiveAccuracy <- createJaspContainer(title = gettextf(
      "%s Predictive Accuracy Plot", 
      switch(
        options[["plotsPredictiveAccuracyType"]],
        "conditional" = gettext("Conditional"),
        "joint"       = gettext("Joint"),
        "marginal"    = gettext("Normalized")
      )))
    containerPredictiveAccuracy$position <- 5
    containerPredictiveAccuracy$dependOn(c("plotsPredictiveAccuracy", "plotsPredictiveAccuracyType"))
    
    jaspResults[["containerPredictiveAccuracy"]] <- containerPredictiveAccuracy
  } else {
    containerPredictiveAccuracy <- jaspResults[["containerPredictiveAccuracy"]]
  }
  
  
  if (options[["introText"]] && is.null(containerPredictiveAccuracy[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("predictive_accuracy", options, analysis)
    
    containerPredictiveAccuracy[['introText']] <- introText    
  }
  
  return(containerPredictiveAccuracy)
}
.containerSequentialTestsLS    <- function(jaspResults, options, analysis){
  
  if (is.null(jaspResults[["containerSequentialTests"]])){
    containerSequentialTests <- createJaspContainer(title = gettextf(
      "%s Sequential Analysis",
      switch(
        options[["plotsIterativeType"]],
        "conditional" = gettext("Conditional"),
        "joint"       = gettext("Joint"),
        "marginal"    = gettext("Normalized"),
        "BF"          = gettext("Bayes Factor")
      )))
    containerSequentialTests$position <- 8
    containerSequentialTests$dependOn(c("plotsIterative", "plotsIterativeType"))
    
    jaspResults[["containerSequentialTests"]] <- containerSequentialTests
  } else {
    containerSequentialTests <- jaspResults[["containerSequentialTests"]]
  }
  
  
  if (options[["introText"]] && is.null(containerSequentialTests[['introText']])){
    
    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1
    
    introText[['text']] <- .explanatoryTextLS("sequential_tests", options, analysis)
    
    containerSequentialTests[['introText']] <- introText    
  }
  
  return(containerSequentialTests)
}

# explanatory text
.explanatoryTextLS  <- function(text, options = NULL, analysis = NULL, type = NULL){
  
  estimation <- grepl("Est", analysis, fixed = TRUE)
  binomial   <- grepl("bin", analysis, fixed = TRUE)
  
  if (text == "main"){
    
    introText <- gettextf(
      "Welcome to the %s analysis from the Learn Bayes module. This analysis illustrates Bayesian %s using %s examples.",
      switch(
        analysis,
        "binEst"    = gettext("Binomial Estimation"),
        "binTest"   = gettext("Binomial Testing"),
        "gaussEst"  = gettext("Gaussian Estimation"),
        "gaussTest" = gettext("Gaussian Testing")
      ),
      ifelse(estimation, gettext("estimation"), gettext("testing")),
      ifelse(binomial,   gettext("binomial"),   gettext("Gaussian"))
    )
    
    parameterInfo <- ifelse(
      binomial,
      gettextf("%s (the population proportion of successes)", "\u03B8"),
      gettextf("%s (the population mean, variance is assumed to be known)", "\u03BC")
    )
    
    overviewText <- gettextf(
      "The analysis is split into 5 sections:
      <ol> <li> Data - for specifying the data input for the computations. You can either use a variable from a dataset loaded into JASP, specify an aggregated overview of the data, or enter the observations one by one (and update them during the analysis). </li>
           <li> %s </li>
           <li> %s </li>
           <li> %s </li>
           <li> %s </li> </ol>
      ",
      ifelse(estimation,
             gettextf("Model - for specifying models that will be used for estimation. You can specify the model name, the prior distribution for the parameter %s, and parameters that define the distribution.", parameterInfo),
             gettextf("Hypothesis - for setting hypotheses that will be used for testing. You can specify the hypothesis name, the prior probability, the prior distribution for the parameter %s, and parameters that define the distribution.", parameterInfo)),
      ifelse(estimation,
             gettext("Inference - for drawing conclusions from the specified models. The options include visualizing prior, posterior, and prior and posterior distributions. In addition, the posterior plots of the multiple models can be shown in one figure (All), with a depth effect (Stacked), or as separate figures (Individual)."),
             gettext("Inference - for drawing conclusions based on the specified hypothesis. The options include visualizing prior distributions, prior predictive distributions, posterior distributions, prior and posterior distributions, and predictive accuracy. In addition, the multiple hypotheses can be shown when considered alone (Conditional), after multiplication by the prior (Joint), and combined together (Marginal)." )),
      ifelse(estimation,
             gettext("Sequential analysis - for displaying the results of Bayesian estimation sequentially, one observation at the time (available only with non-aggregated data). The options include the updating of the point estimate, specified interval, and the resulting distributions."),
             gettext("Sequential analysis - for displaying the results of Bayesian evidence accumulation sequentially, one observation at the time (available only with non-aggregated data).")),
      ifelse(estimation,
             gettext("Posterior prediction - for assesing the predictions from the updated models. In addition, multiple models can be shown in one figure (All), with a depth effect (Stacked), or in individual figures (Individual)."),
             gettext("Posterior prediction - for assesing the predictions from the updated hypotheses. In addition, predictions from multiple hypotheses can be shown when considered alone (Conditional), after multiplication by the prior (Joint), and combined together (Marginal)."))
    )
    
    out <- paste0(introText, "\n\n", overviewText)
    
  } else if (text == "data"){
    
    mainText   <- gettext("The 'Data' section allows you to specify data input for the analysis.")
    optionText <- switch(
      options[["dataType"]],
      "dataVariable"  = gettextf(
        "The 'Select variable' option allows the selection of a variable ('Selected') from a dataset loaded into JASP. %s",
        ifelse(binomial,
               gettext("In addition, the variable levels need to be classified into successes ('Successes') and failures ('Failures')."),
               gettext("In addition, the standard deviation of the normal distribution of the population ('SD') needs to be specified.")
        )
      ),
      "dataCounts"    = gettextf(
        "The 'Specify counts' option allows the use of aggregated data. %1$s This means that the %2$s are updated only once, with the complete data. The lack of information about the order of observations precludes the use of sequential analysis.",
        ifelse(binomial,
               gettext("The necessary information is the number of successes ('Successes') and the number of failures ('Failures')."),
               gettext("The necessary information is the number observed mean ('Mean'), the standard deviation of the normal distribution of the population ('SD'), and the number of observations ('Observations').")
        ),
        ifelse(estimation, gettext("models"), gettext("hypotheses"))
      ),
      "dataSequence"  = gettextf(
        "The 'Enter sequence' option allows manual input of a specific order of observations. %s",
        ifelse(binomial,
               gettext("The necessary information are the individual observed outcomes written into 'Comma-separated sequence of observations', and classification of the observation types into the ones that represent success ('Successes') and failures ('Failures')."),
               gettext("The necessary information are the individual numeric outcomes written into 'Comma-separated sequence of observations' and the standard deviation of the normal distribution of the population ('SD').")
        )
      )
    )
    
    out <- paste0(mainText, " ", optionText)
    
  } else if (text == "estimates"){
    
    estimationFormulas <- switch(
      analysis,
      "binEst"   = gettextf(
        "The 'Binomial Estimation' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population proportion of successes: 
        <ul><li>'Spike(%2$s)' - for concentrating all probability mass at one point (%2$s). This represents the prior belief that the population proportion is %2$s with certainty. This conviction is so strong that no data can move this prior belief. Hence, the posterior is also a spike at %2$s. The prior and the posterior %5$s then corresponds to the location of the spike.</li><li>'Beta(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a beta distribution with parameters %3$s and %4$s. The prior %6$s. After observing 'S' successes and 'F' failures, the posterior distribution updates to beta(%3$s + S, %4$s + F) with a %5$s computed correspondingly.</li></ul>",
        "\u03B8", "\u03B8\u2080", "\u03B1", "\u03B2",
        options[["pointEstimate"]],
        switch(
          options[["pointEstimate"]],
          "mean"   = gettextf("mean can be computed as %1$s / (%1$s + %2$s)", "\u03B1", "\u03B2"),
          "median" = gettextf("median can be approximated as (%1$s - 1/3) / (%1$s + %2$s - 2/3) if%1$s, %2$s > 1", "\u03B1", "\u03B2"),
          "mode"   = gettextf("mode can be computed as (%1$s - 1) / (%1$s + %2$s - 2) if%1$s, %2$s > 1", "\u03B1", "\u03B2")
        )),
      "gaussEst" = gettextf(
        "The 'Gaussian Estimation' analysis offers two types of prior distributions for parameter %1$s of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability mass at one point (%3$s). This represents the prior belief that the population proportion is %3$s with certainty. This conviction is so strong that no data can move this prior belief. Hence, the posterior is also a spike at %3$s. The prior and the posterior %8$s then corresponds to the location of the spike.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with parameters mean %3$s and standard deviation %4$s. The prior %8$s corresponds to the mean parameter of the normal distribution %3$s. After seeing 'N' observations with mean %5$s, the posterior distribution updates to normal( (%4$s%6$s*%5$s)/( (%2$s%6$s/N) + %4$s%6$s) + (%2$s%6$s*%3$s)/( (%2$s%6$s/N) + %4$s%6$s), 1/%7$s(1/%4$s%6$s + N/%2$s%6$s) ) with %8$s corresponding to the mean of posterior distribution.</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A", options[["pointEstimate"]]),
    )
    
    tableDescription <- gettextf(
      "The 'Estimation Summary' table displays numerical summaries for the individual models. The displayed point estimate can be changed using the 'Point estimate' option. The table is composed of the following columns:
    <ul><li>'Model' - the specified model names</li><li>'Prior (%1$s)' - the specified prior distribution for parameter %1$s</li><li>'Prior %2$s' - the %3$s of the specified prior distribution</li><li>'Posterior (%1$s)' - the estimated posterior distribution for the parameter %1$s (i.e., the prior distribution updated with data)</li><li>'Posterior %2$s' - the %3$s of the posterior distribution</li></ul>", 
      ifelse(binomial, "\u03B8", "\u03BC"),
      .estimateTextLS(options[["pointEstimate"]]),
      options[["pointEstimate"]]
    )
    
    out <- paste0(estimationFormulas, "\n", tableDescription)
    
  } else if (text == "tests"){
    
    testsFormulas <- switch(
      analysis,
      "binTest"   = gettextf(
        "The 'Binomial Testing' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population proportion of successes: 
        <ul><li>'Spike(%2$s)' - for concentrating all probability density at one location (%2$s). The marginal likelihood corresponds to a binomial density evaluated at the observed number of successes, with size equal to the number of observations, and with the chance parameter equal to the location of the spike.</li><li>'Beta(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a beta distribution with parameters %3$s and %4$s. The marginal likelihood corresponds to a beta-binomial density evaluated at the observed number of successes, with size equal to the number of observations, and with parameters %3$s and %4$s corresponding to the shape parameters of the prior beta distribution.</li></ul>",
        "\u03B8", "\u03B8\u2080", "\u03B1", "\u03B2"),
      "gaussTest" = gettextf(
        "The 'Gaussian Testing' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population mean of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability density at one location (%3$s). The marginal likelihood corresponds to a normal density evaluated at the observed mean, with mean equal to the location of the spike, and standard deviation as %2$s/%7$sN, where 'N' stands for the number of observations.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with mean %3$s and standard deviation %4$s. The marginal likelihood corresponds to a normal density evaluated at the observed mean, with mean equal to the mean of the prior distribution, and standard deviation of %7$s( %2$s%6$s/N + %4$s%6$s ).</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A"),
    )
    
    testsFormulas2 <- gettextf(
      "The posterior probabilities 'P(H|data)' are then computed using Bayes formula,
        <center>P(H|data) = P(H) * P(data|H) / P(data),</center> where 'P(H)' represents the prior probability of the hypothesis, 'P(data|H)' the marginal likelihood of the data given the hypothesis, and 'P(data)' the probability of the data. The factor 'P(data)' equals the sum of marginal likelihoods multiplied by the prior probabilities, P(data) = %1$s P(data|H%2$s)*P(H%2$s).",
      "\u2211", "\u1D62"
    )
    
    testsFormulas3 <- gettextf(
      "The Bayes factors 'BF' can be obtained by dividing the posterior odds of two models by their prior odds,
        <center>BF%1$s%2$s = ( P(H%1$s|data)/P(H%2$s|data) ) / ( P(H%1$s)/P(H%2$s) ),</center> where 'BF%1$s%2$s' stands for the Bayes factor in favor of the 1st model in comparison to the 2nd model and can be interpreted as a natural measure of relative predictive performance. It is also possible to compute a Bayes factor that compares the hypothesis to the remaining hypotheses 'vs. all' exchange the second hypothesis for the sum of the remaining hypotheses, which can be simplified into,
        <center>BF%1$s%2$s = ( P(H%1$s|data)/(1-P(H%1$s|data)) ) / ( P(H%1$s)/(1-P(H%1$s)) ).</center>The numerator and denominator of the Bayes factors can be reversed by choosing the 'BF%2$s%1$s' option (quantifying the evidence in favor of the second hypothesis), or transformed to a log scale by choosing the 'log(BF%1$s%2$s)' option.",
      "\u2081", "\u2080"
    )
    
    tableDescription <- gettextf(
      "The 'Testing Summary' table displays numerical summaries for the hypotheses. It is composed of the following columns:
    <ul><li>'Hypothesis' - the specified hypothesis names</li><li>'P(H)' - the prior probability of the hypothesis</li><li>'log(likelihood)' - the log of the marginal likelihood of the hypothesis</li><li>'P(H|data)' - the posterior probability of the hypothesis (after updating with the data)</li><li>%s</li></ul>", 
      ifelse(options[["bfType"]] == "inclusion",
             gettext("'Inclusion BF' - the inclusion Bayes factor for the hypothesis (change from prior to posterior odds for including the hypothesis)"),
             gettextf("'BF' - the Bayes factor comparing the predictive performance of the current hypothesis to the %s",
                      ifelse(options[["bfType"]] == "best",
                             "best performing hypothesis",
                             "to the hypothesis specified in 'vs.' Dropdown menu"))
      )
    )
    
    out <- paste0(testsFormulas, "\n", testsFormulas2, "\n\n", testsFormulas3, "\n\n", tableDescription)
    
  } else if (text == "parameter_plots"){
    
    generalText <- gettextf(
      "The '%1$s' option displays the %2$s plots for parameter %3$s. Spike prior distributions are visualized as arrows (signifying that their density is infinite) and %4$s distributions are visualized as continuous lines.",
      ifelse(type == "Prior", gettext("Prior distribution"), gettext("Posterior distribution")),
      ifelse(type == "Prior", gettext("prior distribution"), gettext("posterior distribution")),
      ifelse(binomial, "\u03B8", "\u03BC"),
      ifelse(binomial, gettext("beta"), gettext("normal"))
    )
    
    if (estimation){
      
      specificText <- switch(
        options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]],
        "overlying"    = gettextf(
          "The 'All' option shows all %1$s for parameter %2$s on top of each other, allowing for easier comparison with a common density and probability scale on the y-axis.",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(binomial, "\u03B8", "\u03BC")),
        "stacked"      = gettextf(
          "The 'Stacked' option shows all %1$s for parameter %2$s in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(binomial, "\u03B8", "\u03BC")),
        "individual"   = gettextf(
          "The 'Individual' option shows %1$s for parameter %2$s individually in separate figures. It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%3$s",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(binomial, "\u03B8", "\u03BC"),
          .CIsTextLS(type == "Posterior"))
      )
      
      out <- paste0(generalText, " ", specificText)
      
    } else {
      
      generalText2 <- switch(
        text,
        "Prior"     = "",
        "Posterior" = gettextf(
          " Furthermore, the 'Observed data' checkbox allows the visualization of the observed %s as a black cross in the figure.",
          ifelse(binomial, gettext("proportion of successes"), gettext("mean"))
        )
      )
      
      specificText <- switch(
        options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]],
        "conditional" = gettextf(
          "The 'Conditional' option shows all %1$s for parameter %2$s independently, as ifthey were considered as individual models (without the existence of other hypotheses). It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%3$s",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(binomial, "\u03B8", "\u03BC"),
          .CIsTextLS(type == "Posterior")),
        "joint"       = gettextf(
          "The 'Joint' option shows all %1$s for parameter %2$s when considered together in light of the other hypotheses (by multiplying the density of the %1$s over the parameter by the %3$s probability of the hypothesis). In addition, the 'Overlying' option allows the visualization of all %1$s on top of each other, allowing for easier comparison with a common density and probability scale on the y-axis and the 'Stacked' option shows all %1$s in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(binomial, "\u03B8", "\u03BC"),
          ifelse(type == "Prior", gettext("prior"), gettext("posterior"))),
        "marginal"    = gettextf(
          "The 'Marginal' option collapses across all individual %1$s, weighting them by their %2$s probability.%3$s It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%4$s",
          ifelse(type == "Prior", gettext("prior distributions"), gettext("posterior distributions")),
          ifelse(type == "Prior", gettext("prior"), gettext("posterior")),
          ifelse(type == "Prior", gettext(""), gettext("The result represents the best estimate given all hypotheses and our prior beliefs about them together.")),
          .CIsTextLS(type == "Posterior"))
      )
      
      out <- paste0(generalText, generalText2, " ", specificText)
      
    }
    
  } else if (text == "both_plots"){
    
    generalText <- gettextf(
      "The 'Prior and posterior distribution' option displays the prior and posterior distribution of the parameter %1$s for the individual %2$s. Spike prior distributions are visualized as arrows (signifying that their density is infinite) and %3$s distributions are visualized as continuous lines. Prior distributions are visualized as dashed grey lines and the posterior distribution as solid black lines. In addition, the observed data summary can be visualized as a black cross by selecting the '%4$s' checkbox.",
      ifelse(binomial,   "\u03B8", "\u03BC"),
      ifelse(estimation, gettext("models"), gettext("hypotheses")),
      ifelse(binomial,   gettext("beta"), gettext("normal")),
      ifelse(binomial,   gettext("Observed proportion"), gettext("Observed data"))
    )
    
    if (estimation){
      
      out <- generalText
      
    } else {
      
      specificText <- switch(
        options[["plotsBothType"]],
        "conditional" = gettextf(
          "The 'Conditional' option shows all prior and posterior distributions for parameter %1$s independently, as ifthey were considered as individual models (without the existence of other hypotheses).",
          ifelse(binomial, "\u03B8", "\u03BC")),
        "joint"       = gettextf(
          "The 'Joint' option shows all prior and posterior distributions for parameter %1$s when considered together in light of the other hypotheses. In addition, the 'Overlying' option allows the visualization all %1$s on top of each other, allowing for easier comparison with a common density and probability scale on the y-axis and the 'Stacked' option shows all %1$s in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.",
          ifelse(binomial, "\u03B8", "\u03BC")),
        "marginal"    = gettext("The 'Marginal' option collapses across all individual prior and posterior distributions, weighting them by their prior and posterior probability.")
      )
      
      out <- paste0(generalText, " ", specificText)
      
    }
    
  } else if (text == "sequential_point"){
    
    generalText <- gettextf(
      "The 'Point estimate' option displays a plot with the sequential updating of the point estimate %s (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).",
      ifelse(binomial, "\u03B8", "\u03BC")
    )
    
    specificText <- switch(
      options[["plotsIterativeType"]],
      "overlying"    = gettextf("The 'All' option shows either the mean ('Mean') or the median ('Median') for all specified models in one figure, allowing for easier comparison. It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%s", .CIsTextLS()),
      "stacked"      = gettext("The 'Stacked' option shows all of the prior distribution updates for each model within one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.")
    )
    
    tableText <- gettext("The 'Updating table' option generates a numerical summary of the displayed figures.")
    
    out <- paste0(generalText, " ", specificText, " ", tableText)
    
  } else if (text == "sequential_interval"){
    
    generalText <- gettextf(
      "The 'Interval' option displays a sequential plot with the probability of parameter %s lying inside of the interval ranging from ('lower') to ('upper'), (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).",
      ifelse(binomial, "\u03B8", "\u03BC")
    )
    
    specificText <- switch(
      options[["plotsIterativeIntervalType"]],
      "overlying"    = gettextf(
        "The 'All' option shows the probability of parameter %s lying inside of the specified range for all models in one figure, allowing for easier comparison.",
        ifelse(binomial, "\u03B8", "\u03BC")
      ),
      "stacked"      = gettext("The 'Stacked' option visualizes the interval for all prior distribution updates for each model within one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.")
    )
    
    tableText <- gettext("The 'Updating table' option generates a numerical summary of the displayed figures.")
    
    out <- paste0(generalText, " ", specificText, " ", tableText)
    
  } else if (text == "sequential_updating"){
    
    out <- gettextf(
      "The 'Posterior updating table' option generates a numerical summary of the updating process for parameter %s. The 'Observation' column tracks how many observations have been already encountered for the corresponding posterior distribution. The first row (observation = 0) corresponds to the prior distribution and the last row corresponds to the final posterior distribution after the prior distribution was updated with all observations.",
      ifelse(binomial, "\u03B8", "\u03BC"))
    
    
    estimationFormulas <- switch(
      analysis,
      "binEst"   = gettextf(
        "The 'Binomial Estimation' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population proportion of successes: 
        <ul><li>'Spike(%2$s)' - for concentrating all probability density at one location (%2$s). The prior %5$s corresponds to the location of the spike. The posterior distribution is again a spike at the same location and corresponding %5$s</li><li>'Beta(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a beta distribution with parameters %3$s and %4$s. The prior %6$s. After observing 'S' successes and 'F' failures, the posterior distribution updates to beta(%3$s + S, %4$s + F) with a %5$s computed correspondingly.</li></ul>",
        "\u03B8", "\u03B8\u2080", "\u03B1", "\u03B2",
        options[["pointEstimate"]],
        switch(
          options[["pointEstimate"]],
          "mean"   = gettextf("mean can be computed as %1$s / (%1$s + %2$s)", "\u03B1", "\u03B2"),
          "median" = gettextf("median can be approximated as (%1$s - 1/3) / (%1$s + %2$s - 2/3) if%1$s, %2$s > 1", "\u03B1", "\u03B2"),
          "mode"   = gettextf("mode can be computed as (%1$s - 1) / (%1$s + %2$s - 2) if%1$s, %2$s > 1", "\u03B1", "\u03B2")
        )),
      "gaussEst" = gettextf(
        "The 'Gaussian Estimation' analysis offers two types of prior distributions for parameter %1$s of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability density at one location (%3$s). The prior %8$s corresponds to the location of the spike. The posterior distribution is again a spike at the same location and corresponding %8$s.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with parameters mean %3$s and standard deviation %4$s. The prior %8$s corresponds to the mean parameter of the normal distribution %3$s. After seeing 'N' observations with mean %5$s, the posterior distribution updates to normal( (%4$s%6$s*%5$s)/( (%2$s%6$s/N) + %4$s%6$s) + (%2$s%6$s*%3$s)/( (%2$s%6$s/N) + %4$s%6$s), 1/%7$s(1/%4$s%6$s + N/%2$s%6$s) ) with %8$s corresponding to the mean of posterior distribution.</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A", options[["pointEstimate"]]),
    )
    
  } else if (text == "predictions"){
    
    predictionsText <- gettextf(
      "The 'Posterior prediction' section allows the prediction of future data based on the estimated models%s",
      ifelse(estimation,
             gettext(". When prior-based predictions are desired, the data can be removed from the 'Data' section (the posterior after seeing no data equals the prior)."),
             gettext(" and the marginal prediction based on the hypotheses weighted by the posterior probabilities.")
      )
    )
    
    if (binomial){
      
      predictionsFormulas <- gettextf(
        "For a model with a spike prior distribution for parameter %1$s, predictions for 'N' future observation ('Future observations') follow a binomial distribution with size N and chance parameter %2$s equal to the location of the prior distribution. %5$s For a model with a beta prior distribution for parameter %1$s, the predictive distribution is a beta-binomial distribution with size N and posterior beta distribution parameters %3$s and %4$s. %6$s",
        "\u03B8", "\u03B8\u2080", "\u03B1", "\u03B2",
        switch(
          options[["predictionTableEstimate"]],
          "mean"   = gettextf("The mean prediction can be computed as N*%s", "\u03B8\u2080"),
          "median" = "", # there is no simple solution
          "mode"   = gettextf("The mode prediction can be usually computed as (N + 1)*%1$s rounded down to the closest integer if0 < %1$s < 1", "\u03B8\u2080")
        ),
        switch(
          options[["predictionTableEstimate"]],
          "mean"   = gettextf("The mean of the predictions can be computed as N*%1$s/( %1$s + %2$s ).", "\u03B1", "\u03B2"),
          "median" = "", # there is no simple solution
          "mode"   = ""  # and I couldn't find analytical solution for this at all
        )
      )
      
    } else {
      
      predictionsFormulas <- gettextf(
        "For a model with a spike prior distribution for parameter %1$s, predictions for 'N' future observation ('Future observations') with standard deviation %2$s ('SD') follow a normal distribution with mean parameter equal to the location of the prior distribution %3$s and standard deviation %2$s/%4$sN. For a model with a normal prior distribution for parameter %1$s, the predictive distribution is a normal distribution with mean equal to the mean of the posterior distribution and standard deviation based on the standard deviation of the posterior distribution (%3$s) and expected standard deviation of the new data (%2$s/%4$sN), %4$s( %3$s%5$s + (%2$s/%4$sN)%5$s ). In both cases, the %6$s prediction is equal to the mean parameter of the distribution for predictions.",
        "\u03BC", "\u03C3", "\u03C3\u209A", "\u221A", "\u00B2", options[["predictionTableEstimate"]]
      )
      
    }
    
    tableDescription <- gettextf(
      "The 'Prediction Summary' table displays numerical summaries for the individual models. The displayed point estimate can be changed using the 'Point estimate' option. The table is composed of the following columns:
    <ul><li>'Model' - the specified model names</li><li>'Posterior (%1$s)' - the estimated posterior distribution for parameter %1$s (used for prediction)</li>%2$s<li>'Posterior %4$s' - the %5$s of the specified posterior distribution</li><li>'Prediction%3$s' - the predictive distribution for new data</li><li>'Prediction %4$s' - the %5$s of predicted data</li></ul>", 
      ifelse(binomial, "\u03B8", "\u03BC"),
      ifelse(estimation, "", "<li>'P(H|data)' - the posterior probability of the hypothesis (after updating with the data)</li>"),
      ifelse(binomial, gettext(" (Successes)"), ""),
      switch(
        options$predictionTableEstimate,
        "mean"   = gettext("Mean"),
        "median" = gettext("Median"),
        "mode"   = gettext("Mode")
      ),
      switch(
        options$predictionTableEstimate,
        "mean"   = gettext("mean"),
        "median" = gettext("median"),
        "mode"   = gettext("mode")
      )
    )
    
    out <- paste0(predictionsText, " ", predictionsFormulas, "\n\n", tableDescription)
    
  } else if (text == "prediction_plots"){
    
    if (estimation){
      
      # only posterior prediction plots are available for estimation
      generalText <- gettextf(
        "The 'Posterior predictive distribution' option displays figures of predictive distributions based on posterior distributions of the individual models. The '%1$s' checkbox transforms the figures with predicted data%2$s into figures with %3$s.",
        ifelse(binomial, gettext("Show sample proportions"), gettext("Show sample means")),
        ifelse(binomial, gettext(" (number of successes)"), ""),
        ifelse(binomial, gettext("sample proportions"), gettext("sample means"))
      )
      
      specificText <- switch(
        options[["plotsPredictionType"]],
        "overlying"    = gettext("The 'All' option shows all posterior predictive distributions on top of each other, allowing for easier comparison with a common density scale on the y-axis."),
        "stacked"      = gettext("The 'Stacked' option shows all posterior predictive distributions in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis."),
        "individual"   = gettextf("The 'Individual' option shows posterior predictive distributions for each model individually in separate figures. It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%s",.CIsTextLS())
      )
      
      out <- paste0(generalText, " ", specificText)
      
    } else {
      
      generalText <- gettextf(
        "The '%1$s' option displays figures of predictive distributions based on posterior distributions of the individual models. %2$s",
        ifelse(type == "Prior", gettext("Prior predictive distribution"), gettext("Posterior predictive distribution")),
        ifelse(type == "Prior",
               gettextf(
                 "Furthermore, the '%1$s' checkbox allows the visualization of the observed %2$s as a black cross in the figure.",
                 ifelse(binomial, gettext("Observed proportion"), gettext("Observed data")),
                 ifelse(binomial, gettext("number of successes"), gettext("mean"))),
               gettextf(
                 "Furthermore, The '%1$s' checkbox transforms the figures with predicted data%2$s into figures with %3$s%4$s",
                 ifelse(binomial, gettext("Show sample proportions"), gettext("Show sample means")),
                 ifelse(binomial, gettext(" (number of successes)"), ""),
                 ifelse(binomial, gettext("sample proportions"), gettext("sample means")),
                 ifelse(type == "Posterior" && binomial, gettext(" and the 'Predictions table' checkbox shows exact probabilities for every predicted number of successes."), ".")
               ))
      )
      
      specificText <- switch(
        options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]],
        "conditional" = gettextf(
          "The 'Conditional' option shows all %1$s for parameter %2$s independently, as ifthey were considered as individual models (without the existence of other hypotheses). It is possible to visualize different of credible intervals ('CI'):%3$s",
          ifelse(type == "Prior", gettext("prior predictive distributions"), gettext("posterior predictive distributions")),
          ifelse(binomial, "\u03B8", "\u03BC"),
          .CIsTextLS(FALSE)),
        "joint"       = gettextf(
          "The 'Joint' option shows all %1$s for parameter %2$s when considered together in light of the other hypotheses (by multiplying the density of the %1$s by the %3$s probability of the hypothesis). In addition, the 'Overlying' option allows the  visualization of all %1$s on top of each other, allowing for easier comparison with a common density and probability scale on the y-axis and the 'Stacked' option shows all %1$s in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.",
          ifelse(type == "Prior", gettext("prior predictive distributions"), gettext("posterior predictive distributions")),
          ifelse(binomial, "\u03B8", "\u03BC"),
          ifelse(type == "Prior", gettext("prior"), gettext("posterior"))),
        "marginal"    = gettextf(
          "The 'Marginal' option collapses across all individual %1$s, weighting them by their %2$s probability.%3$s It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%4$s",
          ifelse(type == "Prior", gettext("prior predictive distributions"), gettext("posterior predictive distributions")),
          ifelse(type == "Prior", gettext("prior"), gettext("posterior")),
          ifelse(type == "Prior", gettext(""), gettext("The result represents the best estimate given all hypotheses and our prior beliefs about them together.")),
          .CIsTextLS(FALSE))
      )
      
      out <- paste0(generalText, " ", specificText)
      
    }
    
  } else if (text == "predictive_accuracy"){
    
    generalText <- gettext("The 'Predictive accuracy' option allows a comparison of the predictive accuracy across all hypotheses. Predictive accuracy refers to how likely the data are given the hypotheses.")
    
    specificText <- switch(
      options[["plotsPredictiveAccuracyType"]],
      "conditional" = gettext("The 'Conditional' option shows all predictive accuracies independently, as ifthey were considered as individual models (without the existence of other hypotheses)."),
      "joint"       = gettext("The 'Joint' option shows all predictive accuracies when taking the prior probabilities of hypotheses into account (by multiplying conditional predictive accuracies by prior probabilities of the hypotheses)."),
      "marginal"    = gettext("The 'Normalized' option shows all predictive accuracies considered together in light of the other hypotheses (by normalizing the joint predictive accuracies by the probability of the data, which equals to the posterior probability of the hypotheses).")
    )
    
    out <- paste0(generalText, " ", specificText)
    
  } else if (text == "sequential_tests"){
    
    generalText <- gettext("The 'Test results' option displays a plot with the sequential change in the predictive accuracy of all hypotheses (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).")
    
    specificText <- switch(
      options[["plotsIterativeType"]],
      "conditional" = gettext("The 'Conditional' option shows all predictive accuracies independently, as ifthey were considered as individual models (without the existence of other hypotheses)."),
      "joint"       = gettext("The 'Joint' option shows all predictive accuracies when taking the prior probabilities of hypotheses into account (by multiplying conditional predictive accuracies by prior probabilities of the hypotheses)."),
      "marginal"    = gettext("The 'Normalized' option shows all predictive accuracies considered together in light of the other hypotheses (by normalizing the joint predictive accuracies by the probability of the data, which equals to the posterior probability of the hypotheses at the given time point)."),
      "BF"          = gettextf("The 'Bayes factor' option can compare the predictive accuracies of the hypotheses to the rest of the hypotheses ('vs. All'), the best hypothesis ('vs. best'), or a specific hypothesis selected in the 'vs.' dropdown. The nominator and denominator of the Bayes factors can be reversed by choosing the 'BF%2$s%1$s' option (quantifying the evidence in favor of the second hypothesis), or transformed to a log scale by choosing the 'log(BF%1$s%2$s)' option.", "\u2081", "\u2080")
    )
    
    tableText <- gettext("The 'Updating table' option generates a numerical summary of the displayed figure.")
    
    out <- paste0(generalText, " ", specificText, " ", tableText)
    
  }
  
  return(out)
}
.CIsTextLS          <- function(SI = FALSE){
  return(gettextf(
    "<ul><li>'central' - a central interval (or quantile) that covers the central 'mass'%% area of the distribution</li><li>'HPD' - a highest posterior density interval that covers 'mass'%% area with the shortest range</li><li>'custom' - an interval defined by a 'lower' and 'upper' bound. It returns the posterior mass of the parameter falling inside the custom interval.</li>%s</ul>",
    ifelse(SI, gettext("<li>'support' - a support interval that covers a range of all parameter values which BF is higher than 'BF'</li>"),"")
  ))
}
.estimateTextLS     <- function(estimate){
  return( switch(
    estimate,
    "mean"   = gettext("Mean"),
    "median" = gettext("Median"),
    "mode"   = gettext("Mode")
  ))
}