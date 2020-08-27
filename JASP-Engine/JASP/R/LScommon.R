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
.evaluate_priors       <- function(priors){
  for(p in 1:length(priors)){
    for(i in 1:length(priors[[p]])){
      temp_p <- priors[[p]][[i]]
      if (names(priors[[p]])[i] %in% c("parAlpha", "parBeta", "parPoint", "parMu", "parSigma", "PH")){
        priors[[p]][[paste(names(priors[[p]])[i],"inp", sep = "_")]] <- priors[[p]][[i]]
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
.scale_priors          <- function(priors){
  unscaled <- sapply(priors, function(x)x$PH)
  scaled   <- unscaled/sum(unscaled)
  for(i in 1:length(priors)){
    priors[[i]]$PH <- scaled[i]
  }
  return(priors)
}
.aproximateSupportLS   <- function(x_seq, TF_seq){
  x_start <- NULL
  x_end   <- NULL
  
  r <- rle(TF_seq)
  
  if (length(r$values) > 0){
    for(i in 1:length(r$values)){
      if (r$values[i]){
        if (i == 1){
          x_start <- c(x_start, 1)
          x_end   <- c(x_end,   r$lengths[1])
        } else {
          x_start <- c(x_start, sum(r$lengths[1:(i-1)])+1)
          x_end   <- c(x_end,   sum(r$lengths[1:i]))
        }
      } 
    }
  } else {
    x_start <- NA
    x_end   <- NA
  }
  
  return(cbind.data.frame("lCI" = x_seq[x_start], "uCI" = x_seq[x_end]))
}
.clean_sequence        <- function(sequence){
  sequence <- gsub(",", "\n", sequence)
  sequence <- gsub(";", "\n", sequence)
  sequence <- unlist(strsplit(sequence, split = "\n"))
  sequence <- trimws(sequence, which = c("both"))
  sequence <- sequence[sequence != ""]
  return(sequence)
}

# plotting functions
.plotPriorPosteriorLS  <- function(all_lines, all_arrows, dfPoints = NULL, xName = NULL, xRange = c(0,1)){
  
  mappingArrow <- ggplot2::aes(x = x, xend = x, y = y_start, yend = y_end, color = g)
  mappingLines <- ggplot2::aes(x = x, y = y, color = g)
  mappingPoint <- ggplot2::aes(x = x, y = y, color = g)
  
  if (!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if (!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  g <- ggplot2::ggplot() 
  
  if (!is.null(all_arrows)){
    
    for(i in nrow(all_arrows):1){
      
      temp_arrow       <- all_arrows[i,]
      temp_arrow$y_end <- temp_arrow$y_end * .scalingSpikes(all_lines, all_arrows)
      
      g <- g + ggplot2::geom_segment(
        data        = temp_arrow,
        mapping     = mappingArrow,
        size        = 1,
        linetype    = 1,
        arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
        show.legend = F) +
        ggplot2::geom_segment(
          data    = temp_arrow,
          mapping = mappingArrow,
          size    = 1)
    }
    
    x_high <- max(all_arrows$x)
  }
  
  if (!is.null(all_lines)){
    
    for(i in 1:length(unique(all_lines$g))){
      
      temp_line <- all_lines[all_lines$g == unique(all_lines$g)[i], ]
      temp_type <- i
      
      g <- g + ggplot2::geom_line(
        data     = temp_line,
        mapping  = mappingLines,
        size     = 1,
        linetype = temp_type)
    }
    
    x_high <- all_lines$x[which.max(all_lines$y)]
  }
  
  g <- g + .plotXAxis(xName, xRange, FALSE)
  g <- g + .plotYAxis(all_lines, all_arrows, NULL)
  
  if (!is.null(dfPoints)){
    
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
    
    if (!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c(c("black", "gray")[1:length(unique(all_arrows$g))], "black"),
                                           breaks  = c(as.character(all_arrows$g), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if (length(unique(all_arrows$g)) == 1) c(1, NA) else c(1, 2, NA),
                                             shape    = if (length(unique(all_arrows$g)) == 1) c(NA, 4) else c(NA, NA, 4)
                                           )))  
    } else {
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray", "black")[c(1:length(unique(all_lines$g)), 3)],
                                           breaks  = c(unique(as.character(all_lines$g)), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2, NA)[c(1:length(unique(all_lines$g)), 3)],
                                             shape    = c(NA, NA,  4)[c(1:length(unique(all_lines$g)), 3)]
                                           ))) 
    }
    
  } else {
    
    if (!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray")[1:length(unique(all_arrows$g))],
                                           breaks  = as.character(all_arrows$g),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if (length(unique(all_arrows$g)) == 1) c(1) else c(1, 2),
                                             shape    = if (length(unique(all_arrows$g)) == 1) c(NA) else c(NA, NA)
                                           ))) 
    } else {
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c( "black", "gray")[1:length(unique(all_lines$g))],
                                           breaks  = c(unique(as.character(all_lines$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2)[1:length(unique(all_lines$g))],
                                             shape    = c(NA, NA)[1:length(unique(all_lines$g))]
                                           ))) 
    }
    
  }
  
  
  if (x_high > .5) {
    legend.position = c(0.25, 1)
  } else {
    legend.position = c(0.75, 1)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  g <- g + JASPgraphs::geom_rangeframe(sides = if (!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  
  return(plot)
}
.plotOverlyingLS       <- function(all_lines, all_arrows, dfPoints = NULL, point_estimate = NULL, CI = NULL, xName = NULL, yName = NULL,
                                   xRange = c(0,1), palette = "colorblind", no_legend = FALSE, nRound = 3, discrete = FALSE,
                                   proportions = FALSE){
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  if (!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if (!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  # set the CI text
  # set the CI text
  if (!is.null(CI) || !is.null(point_estimate)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound, point_estimate)
  } else {
    temp_label <- NULL
  }
  
  if (!is.null(CI)){
    CI         <- cbind.data.frame(CI, "y" = y_max * 1.05)
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
  
  if (!is.null(all_arrows)){
    
    all_arrows_scaled        <- all_arrows
    all_arrows_scaled$y_end  <- all_arrows_scaled$y_end * .scalingSpikes(all_lines, all_arrows)
    
    if (!is.null(point_estimate)){
      if (point_estimate$spike[1]){
        point_estimate$y <- point_estimate$y  * .scalingSpikes(all_lines, all_arrows)
      }
    }
    
    g <- g + ggplot2::geom_segment(
      data        = all_arrows_scaled,
      mapping     = mappingArrows,
      size        = 1,
      arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      show.legend = F)
    g <- g + ggplot2::geom_segment(
      data    = all_arrows_scaled,
      mapping = mappingArrows,
      size    = 1)
  }
  
  if (!is.null(all_lines)){
    g <- g + ggplot2::geom_line(data = all_lines, mapping = mappingLines, size = 1,)
  }
  
  if (!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  if (!is.null(point_estimate)){
    if (!anyNA(point_estimate$x)){
      g <- g + ggplot2::geom_point(data = point_estimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  if (no_legend == TRUE){
    g <- g + ggplot2::scale_colour_manual(values = "black")
  } else {
    g <- g + JASPgraphs::scale_JASPcolor_discrete(palette)
  }
  
  # axes
  g <- g + .plotXAxis(xName, xRange, discrete)
  g <- g + .plotYAxis(all_lines, all_arrows, if (!is.null(CI) || !is.null(point_estimate)) "notNull" else NULL)
  
  # legend
  if (!is.null(all_lines)){
    xr   <- range(all_lines$x)
    idx  <- which.max(all_lines$y)
    xmax <- all_lines$x[idx]
  } else {
    xr   <- range(all_arrows$x)
    idx  <- which.max(all_arrows$y_end)
    xmax <- all_arrows$x[idx]
  }
  
  if (!is.null(CI)){
    if (!is.na(CI$x_start[1])){
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
  
  if (!is.null(CI) || !is.null(point_estimate)){
    label_y    <- if (length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = y_max * label_y[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
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
  
  if (no_legend == FALSE){
    g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  } else {
    g <- g + JASPgraphs::themeJaspRaw()
  }
  g <- g + JASPgraphs::geom_rangeframe(sides = if (!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  
  return(plot)
}
.plotStackedLS         <- function(all_lines, all_arrows, legend, dfPoints = NULL, xName = NULL, yName = gettext("Density"),
                                   xRange = c(0,1), lCI = NULL, uCI = NULL, discrete = FALSE, proportions = FALSE){
  
  mappingLines  <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingLegend <- ggplot2::aes(x = x, y = y, label = name)
  mappingPoint  <- ggplot2::aes(x = x, y = y)
  
  
  if (!is.null(all_lines)){
    
    all_linesD <- all_lines
    all_linesL <- all_lines
    
    for(i in 1:length(all_linesD)){
      if (is.null(lCI) & is.null(uCI)){
        all_linesD[[i]] <- rbind.data.frame(
          data.frame(x = xRange[1], y = 0, g = all_linesD[[i]]$g[1]),
          all_linesD[[i]],
          data.frame(x = xRange[2], y = 0, g = all_linesD[[i]]$g[1])     
        )
      } else {
        all_linesD[[i]] <- rbind.data.frame(
          data.frame(x = lCI, y = 0, g = all_linesD[[i]]$g[1]),
          all_linesD[[i]][all_linesD[[i]]$x > lCI & all_linesD[[i]]$x < uCI,],
          data.frame(x = uCI, y = 0, g = all_linesD[[i]]$g[1])     
        )
      }
      all_linesL[[i]] <- data.frame(x = xRange, y = rep(0, 2), g = all_linesD[[i]]$g[1] )
    }
    
    all_lines  <- do.call("rbind", all_lines)
    all_linesD <- do.call("rbind", all_linesD)
    all_linesL <- do.call("rbind", all_linesL)
  }
  
  if (!is.null(all_arrows)){
    
    all_arrowsL <- list()
    for(i in 1:length(all_arrows)){
      all_arrowsL[[i]] <- data.frame(y = rep(all_arrows[[i]]$y_start, 2), x = xRange,
                                     g = rep(all_arrows[[i]]$g, 2))
    }
    
    all_arrows <- do.call("rbind", all_arrows)
    all_arrowsL<- do.call("rbind", all_arrowsL)
  }
  
  legend      <- data.frame(legend)
  colnames(legend) <- c("type", "name")
  legend$type <- as.character(legend$type)
  legend$name <- as.character(legend$name)
  
  if (!is.null(all_lines)){
    obsYmax <- max(all_lines$y)
    if (!is.null(all_arrows)){
      all_arrows$y_end <- obsYmax
    }
  } else {
    obsYmax <- max(all_arrows$y_end)    
  }
  yBreak  <- obsYmax/3 
  newymax <- obsYmax + yBreak*nrow(legend)
  
  legend$y <- yBreak*(0:(nrow(legend)-1))
  legend$x <- xRange[1]
  
  # changing y-coordinates to "stack" the plots
  for(i in 1:nrow(legend)){
    if (legend$type[i] == "spike"){
      all_arrows[all_arrows$g == legend[i,2], "y_start"] <- all_arrows[all_arrows$g == legend[i,2], "y_start"] + yBreak*(i-1)
      all_arrows[all_arrows$g == legend[i,2], "y_end"]   <- all_arrows[all_arrows$g == legend[i,2], "y_end"]   + yBreak*(i-1)
      all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     <- all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     + yBreak*(i-1)
    } else if (legend$type[i] %in% c("beta", "normal")){
      all_lines[all_lines$g == legend[i,2], "y"]   <- all_lines[all_lines$g == legend[i,2], "y"]   + yBreak*(i-1)
      all_linesD[all_linesD$g == legend[i,2], "y"] <- all_linesD[all_linesD$g == legend[i,2], "y"] + yBreak*(i-1)
      all_linesL[all_linesL$g == legend[i,2], "y"] <- all_linesL[all_linesL$g == legend[i,2], "y"] + yBreak*(i-1)
    }
  }
  
  g <- ggplot2::ggplot()
  
  for(i in nrow(legend):1){
    if (legend$type[i] == "spike"){
      g <- g + ggplot2::geom_segment(
        data = all_arrows[all_arrows$g == legend$name[i],],
        mapping = mappingArrows, size = 1,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
        ggplot2::geom_line(
          data = all_arrowsL[all_arrowsL$g == legend$name[i],],
          mapping = mappingLines)
    }
    if (legend$type[i] %in% c("beta", "normal")){
      g <- g + ggplot2::geom_line(
        data = all_lines[all_lines$g == legend$name[i],],
        mapping = mappingLines, size = 1) + 
        ggplot2::geom_polygon(
          data = all_linesD[all_linesD$g == legend$name[i],],
          mapping = mappingLines, fill = "grey60", alpha = .8)
      
      if (!is.null(lCI) & !is.null(uCI)){
        g <- g + ggplot2::geom_line(
          data = all_linesL[all_linesL$g == legend$name[i],],
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
.plotIterativeLS       <- function(all_lines, all_CI, xName = "Observations", yName = NULL, x_start = 0,
                                   palette = "colorblind", BF_log = NULL, yRange = NULL){
  
  all_lines      <- do.call("rbind", all_lines)
  all_lines$name <- factor(all_lines$name, levels = sort(levels(all_lines$name)))
  
  obsXmax    <- max(all_lines$x)
  newXmax    <- obsXmax
  if (obsXmax > 10){
    xBreaks <- round(seq(x_start, obsXmax, length.out = 7))
  } else {
    xBreaks <- x_start:obsXmax
  }
  
  if (is.null(yRange)){
    if (is.null(BF_log)){
      yRange <- c(0, 1)
    } else if (BF_log){
      yRange <- range(c(all_lines$y, 0))
    } else if (!BF_log){
      yRange <- range(c(all_lines$y, 1))
    }    
  }
  
  
  mappingLines   <- ggplot2::aes(x = x, y = y, 
                                 group = name, color = name)
  mappinglCI     <- ggplot2::aes(x = x, y = y1, 
                                 group = name, color = name)
  mappinguCI     <- ggplot2::aes(x = x, y = y2, 
                                 group = name, color = name)
  mappingPolygon <- ggplot2::aes(x = x, y = y, group = name, fill = name)
  
  clr  <- scales::gradient_n_pal(JASPgraphs::JASPcolors(palette))(seq(0, 1, length.out = length(unique(all_lines$name))))
  #clr  <- JASPgraphs::colorBrewerJasp(n = length(unique(all_lines$name)))
  if (length(all_CI) > 0){
    names_CI <- NULL
    for(i in 1:length(all_CI)){
      names_CI <- c(names_CI, as.character(unique(all_CI[[i]]$name)))
    }
    clr1 <- clr[order(order(names_CI))]
  }
  
  
  g <- ggplot2::ggplot()
  
  if (length(all_CI) > 0){
    for(i in length(all_CI):1){
      if (is.null(all_CI[[i]]))next
      temp_data <- all_CI[[i]]
      temp_poly <- data.frame(
        x = c(temp_data$x, rev(temp_data$x)),
        y = c(temp_data$y1, rev(temp_data$y2)),
        name = rep(temp_data$name,2)
      )
      
      g <- g + 
        ggplot2::geom_polygon(
          data    = temp_poly,
          mapping = mappingPolygon, fill = clr1[i], alpha = .3) +
        ggplot2::geom_path(
          data    = temp_data,
          mapping = mappinguCI, size = 1, linetype = 2) +
        ggplot2::geom_path(
          data    = temp_data,
          mapping = mappinglCI, size = 1, linetype = 2)
    }
  }
  
  if (!is.null(BF_log)){
    if (BF_log){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(0, 0)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    } else if (!BF_log){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(1, 1)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    }
  }
  
  g <- g +
    ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1)
  
  g <- g + .plotXAxis(xName, c(x_start, newXmax), TRUE)
  g <- g + .plotYAxis2(yName, yRange)
  g <- g + ggplot2::scale_colour_manual(values = clr)
  
  
  if (mean(all_lines$y[all_lines$x == max(all_lines$x)]) > .5) {
    legend.position = c(0.8, 0.03 + length(unique(all_lines$name))/10)
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
.plotIndividualLS      <- function(all_lines, all_arrows, point_estimate, CI, CIall_lines, dfPoints = NULL, xRange, xName, yName = NULL, nRound = 3){ 
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g,)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  # set the CI text
  if (!is.null(CI) || !is.null(point_estimate)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound, point_estimate)
  } else {
    temp_label <- NULL
  }
  if (!is.null(CI))
    CI <- cbind.data.frame(CI, "y" = y_max * 1.05)
  
  g <- ggplot2::ggplot()
  
  if (!is.null(all_arrows)){
    
    temp_arrows        <- all_arrows
    temp_arrows$y_end  <- temp_arrows$y_end * .scalingSpikes(all_lines, all_arrows)
    
    g <- g + ggplot2::geom_segment(
      data    = all_arrows,
      mapping = mappingArrows, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black")
  }
  
  if (!is.null(all_lines)){
    if (!is.null(CIall_lines)){
      g <- g + ggplot2::geom_polygon(
        data = CIall_lines,
        mapping = mappingLines, fill = "grey60", alpha = .8)
    }
    g <- g + ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1, color = "black") 
  }
  
  if (!is.null(CI)){
    if (!is.na(CI$x_start[1])){
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
  
  if (!is.null(CI) || !is.null(point_estimate)){
    label_y    <- if (length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = y_max * label_y[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
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
  
  if (!is.null(point_estimate)){
    if (!anyNA(point_estimate$x)){
      g <- g + ggplot2::geom_point(data = point_estimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  # x-axes
  g <- g + .plotXAxis(xName, xRange, FALSE)
  g <- g + .plotYAxis(all_lines, all_arrows, temp_label)
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = if (!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  return(plot)
}
.plotPredictionLS      <- function(dfHist, point_estimate, CI, xRange, xName, yName, nRound = 0, xBlacked = NULL,
                                   proportions = FALSE, predictionN = NULL){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  mappingArrows1    <- ggplot2::aes(x = x_start_adj , xend = x_end_adj, y = y, yend = y, group = g)
  mappingArrows2    <- ggplot2::aes(x = x_end_adj,  xend = x_start_adj, y = y, yend = y, group = g)
  mappingText       <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint      <- ggplot2::aes(x = x, y = y)
  
  if (!is.null(CI) || !is.null(point_estimate)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound, point_estimate)
    y_max_multiplier <- ifelse(length(temp_label) == 1, 1.15, 1.25)
  } else {
    temp_label <- NULL
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
  newymax    <- max(ifelse(!is.null(CI) || !is.null(point_estimate), y_max_multiplier + .05, 1.10) * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  if (!is.null(CI)){
    
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.10)
    
    if (!proportions){
      CI$x_start_adj <- CI$x_start - .5
      CI$x_end_adj   <- CI$x_end   + .5
    } else {
      CI$x_start_adj <- CI$x_start - .5/(predictionN + 1)
      CI$x_end_adj   <- CI$x_end   + .5/(predictionN + 1)
    }
    
    for(i in 1:nrow(CI)){
      dfHist$col[dfHist$x >= CI$x_start[i] & dfHist$x <= CI$x_end[i]] <- "b"
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
    if (!is.na(CI$x_start[1])){
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
  
  if (!is.null(CI) || !is.null(point_estimate)){
    r <- 0
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = obsYmax * (y_max_multiplier-r)
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
      
      r <- r + .10
    }
  }
  
  if (!is.null(point_estimate)){
    if (!anyNA(point_estimate$x)){
      g <- g + ggplot2::geom_point(data = point_estimate, mapping = mappingPoint, show.legend = FALSE,
                                   inherit.aes = FALSE, size = 4, shape = 21, 
                                   stroke = 1.25, fill = "grey") 
    }
  }
  
  # control fill
  if (is.null(CI)){
    fillColor <- c("grey90") 
  } else {
    if (nrow(CI) == 1){
      if (all(xRange[1]:xRange[2] %in% CI$x_start:CI$x_end)){
        fillColor <- c("grey50") 
      } else {
        fillColor <- c("grey90", "grey50") 
      }
    } else {
      if (all(xRange[1]:xRange[2] %in% c(unlist(sapply(1:nrow(CI), function(i)CI$x_start[i]:CI$x_end[i]))))){
        fillColor <- c("grey50") 
      } else {
        fillColor <- c("grey90", "grey50") 
      }
    }
  }
  if (!is.null(xBlacked))
    fillColor <- c(fillColor, "black")
  
  
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
      axis.text.x       = ggplot2::element_text(angle = 45))
  
  plot <- g
  return(plot)
  
}

# support functions
.CI_labelLS            <- function(CI, nRound, PE = NULL){
  
  if (!is.null(CI)){
    temp_int <- sapply(1:nrow(CI), function(i){
      if (is.na(CI$x_start[i])){
        x <- "none"
        #x <- "~symbol(\\306)"
      } else if (CI$x_start[i] == CI$x_end[i]){
        x <- paste(c(
          "[",format(round(CI$x_start[i], nRound), nsmall = nRound),"]"
        ), collapse = "")
      } else {
        x <- paste(c(
          "[",format(round(CI$x_start[i], nRound), nsmall = nRound),", ",format(round(CI$x_end[i], nRound), nsmall = nRound),"]"
        ), collapse = "")
      }
      return(x)
    }
    )
    temp_int <- paste(temp_int, collapse = " and " )
    temp_int <- paste("'",temp_int,"'")
    
    # text for the coverage
    temp_cov <- paste(c("'",round(CI$coverage[1]*100), "% CI'"), collapse = "")
    
    
    if (CI$g[1] == "HPD"){
      temp_label <- paste(c(temp_cov,"['HPD']:",temp_int), collapse = "")
    } else if (CI$g[1] == "custom"){
      temp_label  <- paste(c("P({",format(round(CI$x_start, nRound), nsmall = nRound),"<=",if (CI$parameter == "theta") "theta" else if (parameter == "mu") "mu","}<=",
                             (format(round(CI$x_end, nRound), nsmall = nRound)),")","=='",round(CI$coverage[1]*100)," %'"), collapse = "")
    } else if (CI$g[1] == "support"){
      temp_label <- paste(c("SI['[BF = ",CI$BF[1],"]']:",temp_int), collapse = "")
    } else if (CI$g[1] == "central"){
      temp_label <- paste(c(temp_cov,":",temp_int), collapse = "")
    }
    
  } else {
    temp_label <- NULL
  }
  
  if (!is.null(PE)){
    if (nrow(PE) > 1)
      PE <- PE[1,]
    PEl <- PE$l
    if (is.numeric(PE$l))
      PEl <- format(round(PEl, ifelse(PE$estimate == "mean", 3, nRound)), nsmall = ifelse(PE$estimate == "mean", 3, nRound))
    temp_pe    <- paste0("'", PE$estimate,"'",  ":", "' ", PEl, ifelse(is.null(temp_label), " '", "; '"))
    if (!is.null(temp_label)){
      temp_label <- paste(temp_pe, temp_label, sep = "~")
    } else {
      temp_label <- temp_pe
    }
  }
  
  if (nchar(temp_label) > 75){
    temp_o <- gregexpr(" and", substr(temp_label, 1, 65))
    temp_label <- c(paste(substr(temp_label, 1, temp_o[[1]][length(temp_o[[1]])]-1), "'", sep = ""), 
                    paste("'",substr(temp_label, temp_o[[1]][length(temp_o[[1]])], nchar(temp_label)), sep = ""))
  }
  
  return(temp_label)
}
.getYMax               <- function(all_lines, all_arrows){
  if (!is.null(all_lines)){
    max_x_lines <- max(all_lines$y)
    if (all(round(all_lines$y[1], 5) == round(all_lines$y, 5)))
        max_x_lines <- max_x_lines * 1.2
  }
    
  
  if (!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max_x_lines))
    y_max     <- max(c(all_lines$y, y_breaks))
  } else if (!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max_x_lines))
    y_max     <- max(c(all_lines$y, y_breaks))
  } else {
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_max     <- max(c(all_arrows$y_end, y_breaks))
  }
  return(y_max)
}
.scalingSpikes         <- function(all_lines, all_arrows){
  if (!is.null(all_lines) & !is.null(all_arrows)){
    y_max     <- .getYMax(all_lines, all_arrows)
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    return(y_max/max(y_breaks2))
  } else {
    return(1)
  }
}
.plotXAxis             <- function(x_name, x_range, discrete){
  
  x_breaks <- JASPgraphs::getPrettyAxisBreaks(x_range)
  
  if (discrete){
    x_breaks <- round(x_breaks)
    x_breaks <- unique(x_breaks[x_breaks >= x_range[1] &  x_breaks <= x_range[2]])
    if (x_breaks[1] > ceiling(x_range[1]))
      x_breaks <- c(ceiling(x_range[1]), x_breaks)
    if (x_breaks[length(x_breaks)] < floor(x_range[2]))
      x_breaks <- c(x_breaks, floor(x_range[2]))
  }
  x_range <- range(c(x_range, x_breaks))
  
  return(ggplot2::scale_x_continuous(x_name, limits = x_range, breaks = x_breaks))
}
.plotYAxis             <- function(all_lines, all_arrows, CI){
  
  y_max <- .getYMax(all_lines, all_arrows)
  
  if (!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, y_max))
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_pos2    <- y_breaks2/(max(y_breaks2)/y_max)
  } else if (!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, y_max))
  } else {
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, y_max))
  }
  
  # set the y-scale plotting range
  if (!is.null(CI)){
    y_range    <- c(0, y_max * 1.20) 
  } else {
    y_range    <- c(0, y_max) 
  }
  
  if (!is.null(all_lines) & !is.null(all_arrows)){
    return(ggplot2::scale_y_continuous(
      gettext("Density"),
      breaks = y_breaks,
      limits = y_range,
      sec.axis = ggplot2::sec_axis(
        ~ .,
        name   = gettext("Probability"),
        breaks = y_pos2,
        labels = y_breaks2)
    ))
  } else {
    return(ggplot2::scale_y_continuous(
      ifelse(is.null(all_lines), gettext("Probability"), gettext("Density")),
      breaks = y_breaks,
      limits = y_range
    ))
  }
}
.plotYAxis2            <- function(y_name, y_range){
  
  y_breaks <- JASPgraphs::getPrettyAxisBreaks(y_range)
  y_range <- range(c(y_range, y_breaks))
  
  return(ggplot2::scale_y_continuous(y_name, limits = y_range, breaks = y_breaks))
}
.plotThemePlus         <- function(all_lines, all_arrows){
  if (!is.null(all_lines) & !is.null(all_arrows)){
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
  
  estimation <- grepl("_est", analysis, fixed = TRUE)
  binomial   <- grepl("bin", analysis, fixed = TRUE)
  
  if (text == "main"){
    
    intro_text <- gettextf(
      "Welcome to the %s analysis from the Learn Bayes module. This analysis illustrates Bayesian %s using %s examples.",
      switch(
        analysis,
        "bin_est"    = gettext("Binomial Estimation"),
        "bin_test"   = gettext("Binomial Testing"),
        "gauss_est"  = gettext("Gaussian Estimation"),
        "gauss_test" = gettext("Gaussian Testing")
      ),
      ifelse(estimation, gettext("estimation"), gettext("testing")),
      ifelse(binomial,   gettext("binomial"),   gettext("Gaussian"))
    )
    
    parameter_info <- ifelse(
      binomial,
      gettextf("%s (the population proportion of successes)", "\u03B8"),
      gettextf("%s (the population mean, variance is assumed to be known)", "\u03BC")
    )
    
    overview_text <- gettextf(
      "The analysis is split into 5 sections:
      <ol> <li> Data - for specifying the data input for the computations. You can either use a variable from a dataset loaded into JASP, specify an aggregated overview of the data, or enter the observations one by one (and update them during the analysis). </li>
           <li> %s </li>
           <li> %s </li>
           <li> %s </li>
           <li> %s </li> </ol>
      ",
      ifelse(estimation,
             gettextf("Model - for specifying models that will be used for estimation. You can specify the model name, the prior distribution for the parameter %s, and parameters that define the distribution.", parameter_info),
             gettextf("Hypothesis - for setting hypotheses that will be used for testing. You can specify the hypothesis name, the prior probability, the prior distribution for the parameter %s, and parameters that define the distribution.", parameter_info)),
      ifelse(estimation,
             gettext("Inference - for visualizing the specified models. The individual options show prior, posterior, and prior and posterior distributions. In addition, the multiple models can be shown in one figure (All), with a depth effect (Stacked), or in individual figures (Individual)."),
             gettext("Inference - for visualizing the specified hypothesis. The individual options show prior distributions, prior predictive distributions, posterior distributions, prior and posterior distributions, and predictive accuracy. In addition, the multiple hypotheses can be shown when considered alone (Conditional), after multiplication by the prior (Joint), and combined together (Marginal)." )),
      ifelse(estimation,
             gettext("Sequential analysis - for displaying the Bayesian estimation updating with additional data (available only with non-aggregated data). The individual options show updating of the point estimate, specified interval, and the resulting distributions."),
             gettext("Sequential analysis - for displaying Bayesian evidence accumulation as data come in (available only with non-aggregated data).")),
      ifelse(estimation,
             gettext("Posterior prediction - for visualizing the predictions from the updated models. In addition, multiple models can be shown in one figure (All), with a depth effect (Stacked), or in individual figures (Individual)."),
             gettext("Posterior prediction - for visualizing the predictions from the updated hypotheses. In addition, predictions from multiple hypotheses can be shown when considered alone (Conditional), after multiplication by the prior (Joint), and combined together (Marginal)."))
    )
    
    out <- paste0(intro_text, "\n\n", overview_text)
    
  } else if (text == "data"){
    
    main_text   <- gettext("The 'Data' section allows you to specify data input for the analysis.")
    option_text <- switch(
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
    
    out <- paste0(main_text, " ", option_text)
    
  } else if (text == "estimates"){
    
    estimation_formulas <- switch(
      analysis,
      "bin_est"   = gettextf(
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
      "gauss_est" = gettextf(
        "The 'Gaussian Estimation' analysis offers two types of prior distributions for parameter %1$s of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability density at one location (%3$s). The prior %8$s corresponds to the location of the spike. The posterior distribution is again a spike at the same location and corresponding %8$s.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with parameters mean %3$s and standard deviation %4$s. The prior %8$s corresponds to the mean parameter of the normal distribution %3$s. After seeing 'N' observations with mean %5$s, the posterior distribution updates to normal( (%4$s%6$s*%5$s)/( (%2$s%6$s/N) + %4$s%6$s) + (%2$s%6$s*%3$s)/( (%2$s%6$s/N) + %4$s%6$s), 1/%7$s(1/%4$s%6$s + N/%2$s%6$s) ) with %8$s corresponding to the mean of posterior distribution.</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A", options[["pointEstimate"]]),
    )
    
    table_description <- gettextf(
      "The 'Estimation Summary' table displays numerical summaries for the individual models. The displayed point estimate can be changed using the 'Point estimate' option. The table is composed of the following columns:
    <ul><li>'Model' - the specified model names</li><li>'Prior (%1$s)' - the specified prior distribution for parameter %1$s</li><li>'Prior %2$s' - the %3$s of the specified prior distribution</li><li>'Posterior (%1$s)' - the estimated posterior distribution for the parameter %1$s (i.e., the prior distribution updated with data)</li><li>'Posterior %2$s' - the %3$s of the posterior distribution</li></ul>", 
      ifelse(binomial, "\u03B8", "\u03BC"),
      .estimateTextLS(options[["pointEstimate"]]),
      options[["pointEstimate"]]
    )
    
    out <- paste0(estimation_formulas, "\n", table_description)
    
  } else if (text == "tests"){
    
    tests_formulas <- switch(
      analysis,
      "bin_test"   = gettextf(
        "The 'Binomial Testing' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population proportion of successes: 
        <ul><li>'Spike(%2$s)' - for concentrating all probability density at one location (%2$s). The marginal likelihood corresponds to a binomial density evaluated at the observed number of successes, with size equal to the number of observations, and with the chance parameter equal to the location of the spike.</li><li>'Beta(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a beta distribution with parameters %3$s and %4$s. The marginal likelihood corresponds to a beta-binomial density evaluated at the observed number of successes, with size equal to the number of observations, and with parameters %3$s and %4$s corresponding to the shape parameters of the prior beta distribution.</li></ul>",
        "\u03B8", "\u03B8\u2080", "\u03B1", "\u03B2"),
      "gauss_test" = gettextf(
        "The 'Gaussian Testing' analysis offers two types of prior distributions for parameter %1$s that represents the underlying population mean of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability density at one location (%3$s). The marginal likelihood corresponds to a normal density evaluated at the observed mean, with mean equal to the location of the spike, and standard deviation as %2$s/%7$sN, where 'N' stands for the number of observations.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with mean %3$s and standard deviation %4$s. The marginal likelihood corresponds to a normal density evaluated at the observed mean, with mean equal to the mean of the prior distribution, and standard deviation of %7$s( %2$s%6$s/N + %4$s%6$s ).</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A"),
    )
    
    tests_formulas2 <- gettextf(
      "The posterior probabilities 'P(H|data)' are then computed using Bayes formula,
        <center>P(H|data) = P(H) * P(data|H) / P(data),</center> where 'P(H)' represents the prior probability of the hypothesis, 'P(data|H)' the marginal likelihood of the data given the hypothesis, and 'P(data)' the probability of the data. The factor 'P(data)' equals the sum of marginal likelihoods multiplied by the prior probabilities, P(data) = %1$s P(data|H%2$s)*P(H%2$s).",
      "\u2211", "\u1D62"
    )
    
    tests_formulas3 <- gettextf(
      "The Bayes factors 'BF' can be obtained by dividing the posterior odds of two models by their prior odds,
        <center>BF%1$s%2$s = ( P(H%1$s|data)/P(H%2$s|data) ) / ( P(H%1$s)/P(H%2$s) ),</center> where 'BF%1$s%2$s' stands for the Bayes factor in favor of the 1st model in comparison to the 2nd model and can be interpreted as a natural measure of relative predictive performance. It is also possible to compute a Bayes factor that compares the hypothesis to the remaining hypotheses 'vs. all' exchange the second hypothesis for the sum of the remaining hypotheses, which can be simplified into,
        <center>BF%1$s%2$s = ( P(H%1$s|data)/(1-P(H%1$s|data)) ) / ( P(H%1$s)/(1-P(H%1$s)) ).</center>The numerator and denominator of the Bayes factors can be reversed by choosing the 'BF%2$s%1$s' option (quantifying the evidence in favor of the second hypothesis), or transformed to a log scale by choosing the 'log(BF%1$s%2$s)' option.",
      "\u2081", "\u2080"
    )
    
    table_description <- gettextf(
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
    
    out <- paste0(tests_formulas, "\n", tests_formulas2, "\n\n", tests_formulas3, "\n\n", table_description)
    
  } else if (text == "parameter_plots"){
    
    general_text <- gettextf(
      "The '%1$s' option displays the %2$s plots for parameter %3$s. Spike prior distributions are visualized as arrows (signifying that their density is infinite) and %4$s distributions are visualized as continuous lines.",
      ifelse(type == "Prior", gettext("Prior distribution"), gettext("Posterior distribution")),
      ifelse(type == "Prior", gettext("prior distribution"), gettext("posterior distribution")),
      ifelse(binomial, "\u03B8", "\u03BC"),
      ifelse(binomial, gettext("beta"), gettext("normal"))
    )
    
    if (estimation){
      
      specific_text <- switch(
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
      
      out <- paste0(general_text, " ", specific_text)
      
    } else {
      
      general_text2 <- switch(
        text,
        "Prior"     = "",
        "Posterior" = gettextf(
          " Furthermore, the 'Observed data' checkbox allows the visualization of the observed %s as a black cross in the figure.",
          ifelse(binomial, gettext("proportion of successes"), gettext("mean"))
        )
      )
      
      specific_text <- switch(
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
      
      out <- paste0(general_text, general_text2, " ", specific_text)
      
    }
    
  } else if (text == "both_plots"){
    
    general_text <- gettextf(
      "The 'Prior and posterior distribution' option displays the prior and posterior distribution of the parameter %1$s for the individual %2$s. Spike prior distributions are visualized as arrows (signifying that their density is infinite) and %3$s distributions are visualized as continuous lines. Prior distributions are visualized as dashed grey lines and the posterior distribution as solid black lines. In addition, the observed data summary can be visualized as a black cross by selecting the '%4$s' checkbox.",
      ifelse(binomial,   "\u03B8", "\u03BC"),
      ifelse(estimation, gettext("models"), gettext("hypotheses")),
      ifelse(binomial,   gettext("beta"), gettext("normal")),
      ifelse(binomial,   gettext("Observed proportion"), gettext("Observed data"))
    )
    
    if (estimation){
      
      out <- general_text
      
    } else {
      
      specific_text <- switch(
        options[["plotsBothType"]],
        "conditional" = gettextf(
          "The 'Conditional' option shows all prior and posterior distributions for parameter %1$s independently, as ifthey were considered as individual models (without the existence of other hypotheses).",
          ifelse(binomial, "\u03B8", "\u03BC")),
        "joint"       = gettextf(
          "The 'Joint' option shows all prior and posterior distributions for parameter %1$s when considered together in light of the other hypotheses. In addition, the 'Overlying' option allows the visualization all %1$s on top of each other, allowing for easier comparison with a common density and probability scale on the y-axis and the 'Stacked' option shows all %1$s in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.",
          ifelse(binomial, "\u03B8", "\u03BC")),
        "marginal"    = gettext("The 'Marginal' option collapses across all individual prior and posterior distributions, weighting them by their prior and posterior probability.")
      )
      
      out <- paste0(general_text, " ", specific_text)
      
    }
    
  } else if (text == "sequential_point"){
    
    general_text <- gettextf(
      "The 'Point estimate' option displays a plot with the sequential updating of the point estimate %s (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).",
      ifelse(binomial, "\u03B8", "\u03BC")
    )
    
    specific_text <- switch(
      options[["plotsIterativeType"]],
      "overlying"    = gettextf("The 'All' option shows either the mean ('Mean') or the median ('Median') for all specified models in one figure, allowing for easier comparison. It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%s", .CIsTextLS()),
      "stacked"      = gettext("The 'Stacked' option shows all of the prior distribution updates for each model within one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.")
    )
    
    table_text <- gettext("The 'Updating table' option generates a numerical summary of the displayed figures.")
    
    out <- paste0(general_text, " ", specific_text, " ", table_text)
    
  } else if (text == "sequential_interval"){
    
    general_text <- gettextf(
      "The 'Interval' option displays a sequential plot with the probability of parameter %s lying inside of the interval ranging from ('lower') to ('upper'), (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).",
      ifelse(binomial, "\u03B8", "\u03BC")
    )
    
    specific_text <- switch(
      options[["plotsIterativeIntervalType"]],
      "overlying"    = gettextf(
        "The 'All' option shows the probability of parameter %s lying inside of the specified range for all models in one figure, allowing for easier comparison.",
        ifelse(binomial, "\u03B8", "\u03BC")
      ),
      "stacked"      = gettext("The 'Stacked' option visualizes the interval for all prior distribution updates for each model within one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis.")
    )
    
    table_text <- gettext("The 'Updating table' option generates a numerical summary of the displayed figures.")
    
    out <- paste0(general_text, " ", specific_text, " ", table_text)
    
  } else if (text == "sequential_updating"){
    
    out <- gettextf(
      "The 'Posterior updating table' option generates a numerical summary of the updating process for parameter %s. The 'Observation' column tracks how many observations have been already encountered for the corresponding posterior distribution. The first row (observation = 0) corresponds to the prior distribution and the last row corresponds to the final posterior distribution after the prior distribution was updated with all observations.",
      ifelse(binomial, "\u03B8", "\u03BC"))
    
    
    estimation_formulas <- switch(
      analysis,
      "bin_est"   = gettextf(
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
      "gauss_est" = gettextf(
        "The 'Gaussian Estimation' analysis offers two types of prior distributions for parameter %1$s of a normal distribution, Normal(%1$s, %2$s), with known standard deviation %2$s: 
        <ul><li>'Spike(%3$s)' - for concentrating all probability density at one location (%3$s). The prior %8$s corresponds to the location of the spike. The posterior distribution is again a spike at the same location and corresponding %8$s.</li><li>'Normal(%3$s, %4$s)' - for allocating probability density across all values of parameter %1$s according to a normal distribution with parameters mean %3$s and standard deviation %4$s. The prior %8$s corresponds to the mean parameter of the normal distribution %3$s. After seeing 'N' observations with mean %5$s, the posterior distribution updates to normal( (%4$s%6$s*%5$s)/( (%2$s%6$s/N) + %4$s%6$s) + (%2$s%6$s*%3$s)/( (%2$s%6$s/N) + %4$s%6$s), 1/%7$s(1/%4$s%6$s + N/%2$s%6$s) ) with %8$s corresponding to the mean of posterior distribution.</li></ul>",
        "\u03BC", "\u03C3", "\u03BC\u2080", "\u03C3\u2080", "x&#772", "\u00B2", "\u221A", options[["pointEstimate"]]),
    )
    
  } else if (text == "predictions"){
    
    predictions_text <- gettextf(
      "The 'Posterior prediction' section allows the prediction of future data based on the estimated models%s",
      ifelse(estimation,
             gettext(". When prior-based predictions are desired, the data can be removed from the 'Data' section (the posterior after seeing no data equals the prior)."),
             gettext(" and the marginal prediction based on the hypotheses weighted by the posterior probabilities.")
      )
    )
    
    if (binomial){
      
      predictions_formulas <- gettextf(
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
      
      predictions_formulas <- gettextf(
        "For a model with a spike prior distribution for parameter %1$s, predictions for 'N' future observation ('Future observations') with standard deviation %2$s ('SD') follow a normal distribution with mean parameter equal to the location of the prior distribution %3$s and standard deviation %2$s/%4$sN. For a model with a normal prior distribution for parameter %1$s, the predictive distribution is a normal distribution with mean equal to the mean of the posterior distribution and standard deviation based on the standard deviation of the posterior distribution (%3$s) and expected standard deviation of the new data (%2$s/%4$sN), %4$s( %3$s%5$s + (%2$s/%4$sN)%5$s ). In both cases, the %6$s prediction is equal to the mean parameter of the distribution for predictions.",
        "\u03BC", "\u03C3", "\u03C3\u209A", "\u221A", "\u00B2", options[["predictionTableEstimate"]]
      )
      
    }
    
    table_description <- gettextf(
      "The 'Prediction Summary' table displays numerical summaries for the individual models. The displayed point estimate can be changed using the 'Point estimate' option. The table is composed of the following columns:
    <ul><li>'Model' - the specified model names</li><li>'Posterior (%1$s)' - the estimated posterior distribution for parameter %1$s (used for prediction)</li>%2$s<li>'Posterior Mean' - the mean of the specified posterior distribution</li><li>'Prediction%3$s' - the predictive distribution for new data</li><li>'Prediction Mean' - the mean of predicted data</li></ul>", 
      ifelse(binomial, "\u03B8", "\u03BC"),
      ifelse(estimation, "", "<li>'P(H|data)' - the posterior probability of the hypothesis (after updating with the data)</li>"),
      ifelse(binomial, gettext(" (Successes)"), "")
    )
    
    out <- paste0(predictions_text, " ", predictions_formulas, "\n\n", table_description)
    
  } else if (text == "prediction_plots"){
    
    if (estimation){
      
      # only posterior prediction plots are available for estimation
      general_text <- gettextf(
        "The 'Posterior predictive distribution' option displays figures of predictive distributions based on posterior distributions of the individual models. The '%1$s' checkbox transforms the figures with predicted data%2$s into figures with %3$s.",
        ifelse(binomial, gettext("Show sample proportions"), gettext("Show sample means")),
        ifelse(binomial, gettext(" (number of successes)"), ""),
        ifelse(binomial, gettext("sample proportions"), gettext("sample means"))
      )
      
      specific_text <- switch(
        options[["plotsPredictionType"]],
        "overlying"    = gettext("The 'All' option shows all posterior predictive distributions on top of each other, allowing for easier comparison with a common density scale on the y-axis."),
        "stacked"      = gettext("The 'Stacked' option shows all posterior predictive distributions in one figure with a depth effect induced by plotting the additional distributions 'further' on the z-axis."),
        "individual"   = gettextf("The 'Individual' option shows posterior predictive distributions for each model individually in separate figures. It is possible to visualize different types of point estimates ('Point estimate') and credible intervals ('CI'):%s",.CIsTextLS())
      )
      
      out <- paste0(general_text, " ", specific_text)
      
    } else {
      
      general_text <- gettextf(
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
      
      specific_text <- switch(
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
      
      out <- paste0(general_text, " ", specific_text)
      
    }
    
  } else if (text == "predictive_accuracy"){
    
    general_text <- gettext("The 'Predictive accuracy' option allows a comparison of the predictive accuracy across all hypotheses. Predictive accuracy refers to how likely the data are given the hypotheses.")
    
    specific_text <- switch(
      options[["plotsPredictiveAccuracyType"]],
      "conditional" = gettext("The 'Conditional' option shows all predictive accuracies independently, as ifthey were considered as individual models (without the existence of other hypotheses)."),
      "joint"       = gettext("The 'Joint' option shows all predictive accuracies when taking the prior probabilities of hypotheses into account (by multiplying conditional predictive accuracies by prior probabilities of the hypotheses)."),
      "marginal"    = gettext("The 'Normalized' option shows all predictive accuracies considered together in light of the other hypotheses (by normalizing the joint predictive accuracies by the probability of the data, which equals to the posterior probability of the hypotheses).")
    )
    
    out <- paste0(general_text, " ", specific_text)
    
  } else if (text == "sequential_tests"){
    
    general_text <- gettext("The 'Test results' option displays a plot with the sequential change in the predictive accuracy of all hypotheses (y-axis). The figure visualizes the updating process as ifthe individual data points were arriving one after another (x-axis).")
    
    specific_text <- switch(
      options[["plotsIterativeType"]],
      "conditional" = gettext("The 'Conditional' option shows all predictive accuracies independently, as ifthey were considered as individual models (without the existence of other hypotheses)."),
      "joint"       = gettext("The 'Joint' option shows all predictive accuracies when taking the prior probabilities of hypotheses into account (by multiplying conditional predictive accuracies by prior probabilities of the hypotheses)."),
      "marginal"    = gettext("The 'Normalized' option shows all predictive accuracies considered together in light of the other hypotheses (by normalizing the joint predictive accuracies by the probability of the data, which equals to the posterior probability of the hypotheses at the given time point)."),
      "BF"          = gettextf("The 'Bayes factor' option can compare the predictive accuracies of the hypotheses to the rest of the hypotheses ('vs. All'), the best hypothesis ('vs. best'), or a specific hypothesis selected in the 'vs.' dropdown. The nominator and denominator of the Bayes factors can be reversed by choosing the 'BF%2$s%1$s' option (quantifying the evidence in favor of the second hypothesis), or transformed to a log scale by choosing the 'log(BF%1$s%2$s)' option.", "\u2081", "\u2080")
    )
    
    table_text <- gettext("The 'Updating table' option generates a numerical summary of the displayed figure.")
    
    out <- paste0(general_text, " ", specific_text, " ", table_text)
    
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