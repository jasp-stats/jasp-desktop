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

.ldGetPlotContainer <- function(jaspResults, options, name, title, position){
  if(!is.null(jaspResults[[name]])){
    plotsContainer <- jaspResults[[name]]
  } else{
    plotsContainer <- createJaspContainer(title = gettext(title))
    plotsContainer$position <- position
    
    if("parametrization" %in% names(options)){
      plotsContainer$dependOn(c(options$parValNames, "parametrization"))
    } else{
      plotsContainer$dependOn(c(options$parValNames))
    }
    
    jaspResults[[name]] <- plotsContainer
  }
  
  return(plotsContainer)
}

.ldFormulaPlot <- function(container, options, formulaText = NULL, depend = NULL){
  if(!options$formulas) return()
  if(!is.null(container[['formula']])) return()  
  
  formula <- createJaspHtml(title = gettext("Formula"), elementType = "h1")
  formula$position <- 3
  formula$dependOn(c("formulas", depend))
  
  if(is.function(formulaText)){
    formula[['text']] <- formulaText(options)
  } else if(is.character(formulaText)){
    formula[['text']] <- formulaText
  }
  
  container[['formula']] <- formula
}

### Plot PDF ----
.ldFillPDFContainer <- function(pdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotPDF) return()
  
  .ldExplanationPDF(pdfContainer, options, explanationText)
  .ldPlotPDF(pdfContainer, options)
  .ldFormulaPlot(pdfContainer, options, formulaText, "plotPDF")
  
  return()
}

.ldExplanationPDF <- function(pdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(pdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$pdf
  }
  
  explanation[['text']] <- explanationText
  pdfContainer[['explanation']] <- explanation

}

.ldPlotPDF <- function(pdfContainer, options){
  if(!is.null(pdfContainer[['pdfPlot']])) return()
  
  pdfPlot <- createJaspPlot(title = gettext("Density Plot"), width = 600, height = 320)
  pdfPlot$position <- 2 # after explanation, before formula
  pdfPlot$dependOn(c('plotPDF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  pdfContainer[['pdfPlot']] <- pdfPlot
  
  .ldFillPlotPDF(pdfPlot, options)

  return()
}

.ldFillPlotPDF <- function(pdfPlot, options){
  # basic density curve
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], size = 1.25)
  
  # highlight probability
  if(options$highlightProbability){
    # determine plotting region
    args <- options[['pars']]
    argsPDF <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['q']] <- c(-Inf, options[['highlightmax']])
    } else if(options[['highlightType']] == "upper"){
      args[['q']] <- c(options[['highlightmin']], Inf)
    }
    
    # calculate value under the curve
    cdfValue <- do.call(options[['cdfFun']], args)
    cdfValue <- cdfValue[2] - cdfValue[1]
    
    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    if(c(0, 1) %in% cdfValueRound){
      cdfValueRound <- round(cdfValue, 3)
    }
    
    # calculate position of the geom_text
    args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
    argsPDF[['x']] <- seq(args[['q']][1], args[['q']][2], length.out = 20)
    x <- weighted.mean(argsPDF[['x']], do.call(options[['pdfFun']], argsPDF))
    argsPDF[['x']] <- x
    y <- do.call(options[['pdfFun']], argsPDF)/3
    
    plot <- plot + 
      ggplot2::stat_function(fun = options[['pdfFun']], n = 101, args = options[['pars']], geom = "area", 
                             xlim = args[['q']], fill = "steelblue") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
  }
  
  # highlight density
  if(options$highlightDensity){
    # determine plotting region
    args <- options[['pars']]
    if(options[['highlightType']] == "minmax"){
      args[['x']] <- c(options[['highlightmin']], options[['highlightmax']])
    } else if(options[['highlightType']] == "lower"){
      args[['x']] <- options[['highlightmax']]
    } else if(options[['highlightType']] == "upper"){
      args[['x']] <- options[['highlightmin']]
    }
    
    
    pdfValue <- do.call(options[['pdfFun']], args)
    
    segment_data <- data.frame(x = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               xend = args[['x']], y = pdfValue)
    
    # plot density
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data, 
                            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = data.frame(x = options[['range_x']][1], y = pdfValue, label = round(pdfValue, 2)),
                         ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['x']], ymin = 0, ymax = pdfValue, linetype = 2) +
      JASPgraphs::geom_point(x = args[['x']], y = pdfValue, size = 5)
  }
  
  plot <- plot + ggplot2::ylab(gettext("Density")) + 
    ggplot2::scale_x_continuous(limits = options[['range_x']], breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']]))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  pdfPlot[['plotObject']] <- plot
}

### Plot CDF ----
.ldFillCDFContainer <- function(cdfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotCDF) return()
  
  .ldExplanationCDF(cdfContainer, options, explanationText)
  .ldPlotCDF(cdfContainer, options)
  .ldFormulaPlot(cdfContainer, options, formulaText, "plotCDF")
}

.ldExplanationCDF <- function(cdfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(cdfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCDF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  cdfContainer[['explanation']] <- explanation
}

.ldPlotCDF <- function(cdfContainer, options){
  if(!is.null(cdfContainer[['cdfPlot']])) return()
  
  cdfPlot <- createJaspPlot(title = gettext("Cumulative Probability Plot"), width = 600, height = 320)
  cdfPlot$position <- 2 # after explanation, before formula
  cdfPlot$dependOn(c('plotCDF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  cdfContainer[['cdfPlot']] <- cdfPlot
  
  .ldFillPlotCDF(cdfPlot, options)
  
  return()
}

.ldFillPlotCDF <- function(cdfPlot, options){
  
  plot <- ggplot2::ggplot(data = data.frame(x = options[['range_x']]), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['cdfFun']], n = 101, args = options[['pars']], size = 1.25)

  
  args <- options[['pars']]
  if(options[['highlightType']] == "minmax"){
    args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
  } else if(options[['highlightType']] == "lower"){
    args[['q']] <- options[['highlightmax']]
  } else if(options[['highlightType']] == "upper"){
    args[['q']] <- options[['highlightmin']]
  }
  
  cdfValue <- do.call(options[['cdfFun']], args)
  point_data <- data.frame(x = args[['q']], y = cdfValue, col = as.factor(seq_along(args[['q']])))
  
  if(options$highlightProbability){ 
    # round value for plotting as text
    cdfValueRound <- round(cdfValue, 2)
    
    segment_data <- data.frame(xoffset = options[['range_x']][1] + (options[['range_x']][2]-options[['range_x']][1])/15,
                               x = options[['range_x']][1], xend = args[['q']], y = cdfValue, label = cdfValueRound)
    
    
    plot <- plot + 
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xoffset, xend = xend, y = y, yend = y), linetype = 2) +
      ggplot2::geom_text(data = segment_data, ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = args[['q']], ymin = 0, ymax = cdfValue, linetype = 2) + 
      JASPgraphs::geom_point(data = point_data, ggplot2::aes(x = x, y = y), size = 5)
  }
  
  if(options$highlightDensity){
    # determine plotting region
    pdfArgs <- args
    pdfArgs[['x']] <- pdfArgs[['q']]
    pdfArgs[['q']] <- NULL
    
    pdfValue <- do.call(options[['pdfFun']], pdfArgs)
    intercept <- cdfValue - args[['q']]*pdfValue
    slopeText <-  round(pdfValue, 2)
    
    line_data <- data.frame(slope = pdfValue, intercept = intercept, col = as.factor(1:length(pdfArgs[['x']])))
    
    plot <- plot + 
      ggplot2::geom_abline(data = line_data, ggplot2::aes(slope = slope, intercept = intercept, col = col), size = 1) +
      JASPgraphs::geom_point (data = point_data, ggplot2::aes(x = x, y = y, col = col), size = 5) + 
      JASPgraphs::scale_JASPcolor_discrete(name = gettext("Slope"), labels = as.character(slopeText))
  }
  
  
  plot <- plot + 
    ggplot2::ylab(gettext("Probability (X \u2264 x)")) +
    ggplot2::scale_x_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']])) +
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  plot <- JASPgraphs::themeJasp(plot, legend.position = c(0.85, 0.4))
  
  cdfPlot[['plotObject']] <- plot
}

### Plot QF ----
.ldFillQFContainer <- function(qfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotQF) return()
  
  .ldExplanationQF(qfContainer, options, explanationText)
  .ldPlotQF(qfContainer, options)
  .ldFormulaPlot(qfContainer, options, formulaText, "plotQF")
}

.ldExplanationQF <- function(qfContainer, options, explanationText){
  if(!options$explanatoryText) return()
  if(!is.null(qfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotQF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  qfContainer[['explanation']] <- explanation
}

.ldPlotQF <- function(qfContainer, options){
  if(!is.null(qfContainer[['qfPlot']])) return()
  
  qfPlot <- createJaspPlot(title = gettext("Quantile Plot"), width = 600, height = 320)
  qfPlot$position <- 2 # after explanation, before formula
  qfPlot$dependOn(c('plotQF', 'range'))
  qfContainer[['qfPlot']] <- qfPlot
  
  .ldFillPlotQF(qfPlot, options)
  
  return()

}

.ldFillPlotQF <- function(qfPlot, options){
  args <- options[['pars']]
  args[['q']] <- options[['range_x']]
  prange <- do.call(options[['cdfFun']], args)
  args[['q']] <- NULL
  
  plot <- ggplot2::ggplot(data = data.frame(x = prange), ggplot2::aes(x = x)) +
    ggplot2::stat_function(fun = options[['qFun']], n = 151, args = args, size = 1.25)  +
    ggplot2::ylab("x") + ggplot2::xlab(gettext("Probability(X \u2264 x)")) +
    ggplot2::scale_x_continuous(limits = 0:1) +
    ggplot2::scale_y_continuous(limits = options[['range_x']], 
                                breaks = JASPgraphs::getPrettyAxisBreaks(options[['range_x']]))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  qfPlot[['plotObject']] <- plot
}

### Plot PMF ----
.ldFillPMFContainer <- function(pmfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotPMF) return()
  
  .ldExplanationPMF(pmfContainer, options, explanationText)
  .ldPlotPMF(pmfContainer, options)
  .ldFormulaPlot(pmfContainer, options, formulaText, "plotPMF")
  
  return()
}

.ldExplanationPMF <- function(pmfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(pmfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotPMF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$pmf
  }
  
  explanation[['text']] <- explanationText
  pmfContainer[['explanation']] <- explanation
  
}

.ldPlotPMF <- function(pmfContainer, options){
  if(!is.null(pmfContainer[['pmfPlot']])) return()
  
  pmfPlot <- createJaspPlot(title = gettext("Probability Mass Plot"), width = 600, height = 320)
  pmfPlot$position <- 2 # after explanation, before formula
  pmfPlot$dependOn(c('plotPMF', 
                     'min_x', 'max_x',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max'))
  pmfContainer[['pmfPlot']] <- pmfPlot
  
  .ldFillPlotPMF(pmfPlot, options)
  
  return()
}

.ldFillPlotPMF <- function(pmfPlot, options){
  args <- options[['pars']]
  args[['x']] <- options[['range_x']][1]:options[['range_x']][2]
  
  # make a room next to the y-axis
  xlim <- options[['range_x']] + c(-0.1, 0.1) * diff(options[['range_x']]) + c(-0.8, 0.8)
  
  # if(diff(options[['range_x']]) <= 2){
  #   xlim[1] <- floor(xlim[1]) - 1
  #   xlim[2] <- ceiling(xlim[2]) + 1
  # }
  dat <- data.frame(x = args[['x']], y = do.call(options[['pdfFun']], args))
  
  # basic plot
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black", width = 0.8)
  
  # highlight probability
  if(options$highlightProbability){
    # determine plotting region
    args[['x']] <- options[['highlightmin']]:options[['highlightmax']]
    datHigh <- data.frame(x = args[['x']], pmf = do.call(options[['pdfFun']], args))
    
    # calculate value under the curve
    cdfValue <- sum(datHigh$pmf)
    
    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    if(c(0, 1) %in% cdfValueRound){
      cdfValueRound <- round(cdfValue, 3)
    }
    
    # calculate position of the geom_text
    datShown <- datHigh[datHigh$x %in% dat$x, ]
    if(ncol(datShown) > 0) {
      x <- datShown$x[which.max(datShown$pmf)]
      y <- datShown$pmf[which.max(datShown$pmf)] + 0.1 * max(dat$y)
    } else{ # the entire highlight region is outside of displayed range
      x <- NA
      y <- NA
      cdfValueRound <- NA
    }
    
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = x, y = pmf), 
                        data = datShown, width = 0.8,
                        fill = "steelblue", stat = "identity") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
  }
  
  # highlight density
  if(options$highlightDensity){
    # determine plotting region
    args <- options[['pars']]
    args[['x']] <- c(options[['highlightmin']], options[['highlightmax']])
    
    segment_data <- subset(dat, x %in% args[['x']])
    segment_data$xend <- segment_data$x
    segment_data$x    <- xlim[1] + 0.05 * diff(options[['range_x']])
    segment_data$xseg <- xlim[1] + 0.1 * diff(options[['range_x']])
    segment_data$label <- round(segment_data$y, 2)
    
    # make 10% margin to write values along axis
    #xlim <- xlim + c(-0.1, 0) * diff(options[['range_x']]) 
    # plot density
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = xend, y = y), stat = "identity",
                        data = segment_data, 
                        alpha = 0, colour = "black", size = 1.5, width = 0.8) +
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xseg, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = segment_data,
                         ggplot2::aes(x = x, y = y, label = label), size = 6)
  }
  
  breaks <- JASPgraphs::getPrettyAxisBreaks(options[['range_x']])
  # display only pretty integers
  breaks <- breaks[breaks %% 1 == 0]
  plot <- plot + 
    ggplot2::ylab(gettext("Probability (X = x)")) + 
    ggplot2::scale_x_continuous(limits = xlim,
                                breaks = breaks,
                                labels = breaks,
                                expand = c(0, 0))
    
  plot <- JASPgraphs::themeJasp(plot)
  
  pmfPlot[['plotObject']] <- plot
}

### Plot CMF ----
.ldFillCMFContainer <- function(cmfContainer, options, formulaText = NULL, explanationText = NULL){
  if(!options$plotCMF) return()
  
  .ldExplanationCMF(cmfContainer, options, explanationText)
  .ldPlotCMF(cmfContainer, options)
  .ldFormulaPlot(cmfContainer, options, formulaText, "plotCMF")
}

.ldExplanationCMF <- function(cmfContainer, options, explanationText = NULL){
  if(!options$explanatoryText) return()
  if(!is.null(cmfContainer[['explanation']])) return()
  
  explanation <- createJaspHtml()
  explanation$dependOn(c("plotCMF", "explanatoryText"))
  explanation$position <- 1
  
  if(is.null(explanationText)){
    explanationText <- .ldAllTextsList()$explanations$cdf
  }
  
  explanation[['text']] <- explanationText
  cmfContainer[['explanation']] <- explanation
}

.ldPlotCMF <- function(cmfContainer, options){
  if(!is.null(cmfContainer[['cmfPlot']])) return()
  
  cmfPlot <- createJaspPlot(title = gettext("Cumulative Probability Plot"), width = 600, height = 320)
  cmfPlot$position <- 2 # after explanation, before formula
  cmfPlot$dependOn(c('plotCMF', 'min_x', 'max_x', 'highlightType',
                     'highlightDensity', 'highlightProbability', 
                     'min', 'max', 'lower_max', 'upper_min'))
  cmfContainer[['cmfPlot']] <- cmfPlot
  
  .ldFillPlotCMF(cmfPlot, options)
  
  return()
}

.ldFillPlotCMF <- function(cmfPlot, options){
  args <- options[['pars']]
  args[['q']] <- options[['range_x']][1]:options[['range_x']][2]
  
  dat <- data.frame(x = args[['q']], y = do.call(options[['cdfFun']], args))
  
  # make a room next to the y-axis
  xlim <- options[['range_x']] + c(-0.1, 0.1) * diff(options[['range_x']]) + c(-0.8, 0.8)
  
  # basic plot
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black", width = 0.8)
  
  
  # determine plotting region
  args <- options[['pars']]
  args[['q']] <- c(options[['highlightmin']], options[['highlightmax']])
  
  pdfArgs <- args
  pdfArgs[['x']] <- pdfArgs[['q']]
  pdfArgs[['q']] <- NULL
    
  pdfValue <- do.call(options[['pdfFun']], pdfArgs)
  cmfValue <- do.call(options[['cdfFun']], args)

  pdfValueRound <-  round(pdfValue, 2)
  cmfValueRound <- round(cmfValue, 2)
  
  # is there anything to highlight on the plot? (i.e., is the highlighted region inside the range of x)
  highlights <- any(args[['q']] %in% dat$x)
  
  if(options$highlightDensity && highlights){
    # redraw the bars with tip colored highlighting the increment at the selected x
    datDens <- dat
    datDens$col <- "grey"
    if(options$highlightmin %in% datDens$x) {
      datDens[dat$x == options$highlightmin, "y"] <- cmfValue[1] - pdfValue[1]
      datDens <- rbind(datDens, data.frame(x = options$highlightmin, y = pdfValue[1], col = "blue"))
    }
    if(options$highlightmax %in% datDens$x){
      datDens[datDens$x == options$highlightmax, "y"] <- cmfValue[2] - pdfValue[2]
      datDens <- rbind(datDens, data.frame(x = options$highlightmax, y = pdfValue[2], col = "blue"))
    }
    datDens$col <- factor(datDens$col, levels = c("blue","grey"))
    segment_data <- data.frame(xseg = args[['q']], 
                               xend = xlim[2] - 0.1*diff(options[['range_x']]),
                               x    = xlim[2] - 0.05*diff(options[['range_x']]),
                               ymin = cmfValue - pdfValue, ymax = cmfValue, ymid = cmfValue - 0.5*pdfValue,
                               labelDensity = pdfValueRound)
    
    plot <- plot +
      ggplot2::geom_bar(data = datDens, ggplot2::aes(x = x, y = y, fill = col), width = 0.8, stat = "identity", color = "black") +
      ggplot2::geom_segment(data = segment_data, ggplot2::aes(x = xseg, xend = xend, y = ymid, yend = ymid), linetype = 2) + 
      ggplot2::geom_text(data = segment_data, ggplot2::aes(x = x, y = ymid, label = labelDensity), size = 6) +
      ggplot2::scale_fill_manual(values = c("steelblue", "grey"), guide = FALSE)
  }
  
  if(options$highlightProbability && highlights){
    segment_data <- subset(dat, x %in% args[['q']])
    segment_data$xend <- segment_data$x
    
    segment_data$x    <- xlim[1] + 0.05 * diff(options[['range_x']])
    segment_data$xseg <- xlim[1] + 0.1 * diff(options[['range_x']])
    segment_data$label <- round(segment_data$y, 2)
    
    plot <- plot + 
      ggplot2::geom_bar(ggplot2::aes(x = xend, y = y), stat = "identity",
                        data = segment_data, 
                        alpha = 0, colour = "black", size = 1.5, width = 0.8) +
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = xseg, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = segment_data,
                         ggplot2::aes(x = x, y = y, label = label), size = 6)
  }
  
  breaks <- JASPgraphs::getPrettyAxisBreaks(options[['range_x']])
  # display only pretty integers
  breaks <- breaks[breaks %% 1 == 0]
  plot <- plot + 
    ggplot2::ylab(gettext("Probability (X \u2264 x)")) + 
    ggplot2::scale_x_continuous(limits = xlim,
                                breaks = breaks,
                                labels = breaks,
                                expand = c(0, 0)) + 
    ggplot2::scale_y_continuous(limits = c(0, 1))
  
  plot <- JASPgraphs::themeJasp(plot)
  
  cmfPlot[['plotObject']] <- plot
}

#### Plot empirical ----
.ldPlotHistogram <- function(dataContainer, variable, options, ready, as = "scale"){
  if(!options$histogram) return()
  if(!is.null(dataContainer[['histogram']])) return()
  
  title <- switch(as, scale = gettext("Histogram"), gettext("Bar plot"))
  histPlot <- createJaspPlot(title = title, width = 500, height = 320)
  
  if(as != "scale"){
    histPlot$dependOn(c("histogram"))
  } else{
    histPlot$dependOn(c("histogramBins", "histogram"))
  }
  histPlot$position <- 3
  
  dataContainer[['histogram']] <- histPlot
  
  if(!ready) return()
  
  .ldFillPlotHistogram(histPlot, options, variable, as)
  
}

.ldFillPlotHistogram <- function(histPlot, options, variable, as = "scale"){
  if(as == "scale"){
    range <- range(variable)
    histData <- hist(variable, 
                     breaks = seq(range[1], range[2], length.out = options[['histogramBins']]+1), 
                     plot = FALSE)
    dat <- data.frame(counts = histData$counts, density = histData$density, mids = histData$mids)
  } else if(as == "discrete"){
    range <- range(variable)
    mids <- range[1]:range[2]
    counts <- sapply(mids, function(i) sum(variable == i))
    dat  <- data.frame(counts = counts, mids = mids)
  } else if(as == "factor"){
    levs <- levels(variable)
    mids <- seq_along(levs)
    range <- range(mids)
    counts <- sapply(levs, function(i) sum(variable == i))
    dat <- data.frame(counts = counts, mids = mids, labs = levs)
  }
  
  plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = mids, y = counts/sum(counts))) +
    ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black") +
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(gettextf("Rel. Freq (%s in bin)", options[['variable']]))
  
  if(as == "scale"){
    plot <- plot + ggplot2::scale_x_continuous(limits = range, 
                                               expand = c(0.05, 0),
                                               breaks = JASPgraphs::getPrettyAxisBreaks(range))
  } else if (as == "discrete"){
    breaks <- pretty(range)
    breaks <- breaks[breaks %% 1 == 0]
    plot <- plot + ggplot2::scale_x_continuous(limits = range + c(-1, 1),
                                               breaks = breaks,
                                               labels = breaks,
                                               expand = c(0, 0))
  } else if (as == "factor"){
    plot <- plot + ggplot2::scale_x_continuous(limits = range + c(-1, 1),
                                               labels = dat$labs,
                                               breaks = dat$mids,
                                               expand = c(0, 0))
  }
  
  plot <- JASPgraphs::themeJasp(plot)
  histPlot[['plotObject']] <- plot
}

.ldPlotECDF <- function(dataContainer, variable, options, ready){
  if(!options[['ecdf']]) return()
  if(!is.null(dataContainer[['ecdf']])) return()
  
  ecdfPlot <- createJaspPlot(title = gettext("Empirical Cumulative Distribution"), width = 500, height = 320)
  
  ecdfPlot$dependOn(c("ecdf"))
  ecdfPlot$position <- 4
  
  dataContainer[['ecdf']] <- ecdfPlot
  
  if(!ready) return()
  
  .ldFillPlotECDF(ecdfPlot, options, variable)
}

.ldFillPlotECDF <- function(plot, options, variable){
  p <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = variable)) +
    ggplot2::stat_ecdf(geom = "step", size = 1.5) +
    ggplot2::geom_rug() +
    ggplot2::scale_x_continuous(limits = range(variable)*1.1) +
    ggplot2::xlab(options$variable) +
    ggplot2::ylab(gettextf("Freq (%s \u2264 x)", options[['variable']]))
  
  p <- JASPgraphs::themeJasp(p)
  plot[['plotObject']] <- p
  

}