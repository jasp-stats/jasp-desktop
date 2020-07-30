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

# developmental stuff
RDEBUG <- function(message){
  if(file.exists("D:/Projects/jasp/jasp-R-debug/RDEBUG.txt")){
    sink(file = "D:/Projects/jasp/jasp-R-debug/RDEBUG.txt", append = TRUE)
    cat(message)
    cat("\n")
    sink(file = NULL) 
  }
}
saveOptions <- function(options){
  if(file.exists("D:/Projects/jasp/jasp-R-debug/options.RDS"))
    saveRDS(options, file = "D:/Projects/jasp/jasp-R-debug/options.RDS")
}


# general functions
.evaluate_priors       <- function(priors){
  for(p in 1:length(priors)){
    for(i in 1:length(priors[[p]])){
      temp_p <- priors[[p]][[i]]
      if(names(priors[[p]])[i] %in% c("parAlpha", "parBeta", "parPoint", "parMu", "parSigma", "PH")){
        priors[[p]][[paste(names(priors[[p]])[i],"inp", sep = "_")]] <- priors[[p]][[i]]
        priors[[p]][[i]] <- eval(parse(text = priors[[p]][[i]]))
      }
    }
    if(priors[[p]][["name"]] == ""){
      priors[[p]][["name"]] <- gettextf(
        "%s %i",
        ifelse(any(names(priors[[p]]) %in% c("PH")), "Hypothesis", "Model"),
        p
      )
    }
  }

  if(anyDuplicated(sapply(priors, function(p)p$name)) != 0){
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
  
  if(length(r$values) > 0){
    for(i in 1:length(r$values)){
      if(r$values[i]){
        if(i == 1){
          x_start <- c(x_start, 1)
          x_end   <- c(x_end,   r$lengths[1])
        }else{
          x_start <- c(x_start, sum(r$lengths[1:(i-1)])+1)
          x_end   <- c(x_end,   sum(r$lengths[1:i]))
        }
      } 
    }
  }else{
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
  
  if(!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if(!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  g <- ggplot2::ggplot() 
  
  if(!is.null(all_arrows)){
    
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
  
  if(!is.null(all_lines)){
    
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
  
  if(!is.null(dfPoints)){
    
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
    
    if(!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c(c("black", "gray")[1:length(unique(all_arrows$g))], "black"),
                                           breaks  = c(as.character(all_arrows$g), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if(length(unique(all_arrows$g)) == 1) c(1, NA) else c(1, 2, NA),
                                             shape    = if(length(unique(all_arrows$g)) == 1) c(NA, 4) else c(NA, NA, 4)
                                           )))  
    }else{
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray", "black")[c(1:length(unique(all_lines$g)), 3)],
                                           breaks  = c(unique(as.character(all_lines$g)), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2, NA)[c(1:length(unique(all_lines$g)), 3)],
                                             shape    = c(NA, NA,  4)[c(1:length(unique(all_lines$g)), 3)]
                                           ))) 
    }
    
  }else{
    
    if(!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray")[1:length(unique(all_arrows$g))],
                                           breaks  = as.character(all_arrows$g),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if(length(unique(all_arrows$g)) == 1) c(1) else c(1, 2),
                                             shape    = if(length(unique(all_arrows$g)) == 1) c(NA) else c(NA, NA)
                                           ))) 
    }else{
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
  }else {
    legend.position = c(0.75, 1)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  g <- g + JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  
  return(plot)
}
.plotOverlyingLS       <- function(all_lines, all_arrows, dfPoints = NULL, CI = NULL, xName = NULL, yName = NULL,
                                   xRange = c(0,1), palette = "colorblind", no_legend = FALSE, nRound = 3, discrete = FALSE,
                                   proportions = FALSE){
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  if(!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if(!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  # set the CI text
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    CI         <- cbind.data.frame(CI, "y" = y_max * 1.05)
  }
  
  if(discrete){
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(ceiling(xRange[1]),floor(xRange[2])))
    xBreaks[length(xBreaks)] <- floor(xRange[2])
    if(!proportions){
      xBreaks <- round(xBreaks)
    }
  }else{
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xRange)
  }
  
  g <- ggplot2::ggplot()
  
  if(!is.null(all_arrows)){
    
    all_arrows_scaled        <- all_arrows
    all_arrows_scaled$y_end  <- all_arrows_scaled$y_end * .scalingSpikes(all_lines, all_arrows)
    
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
  if(!is.null(all_lines)){
    g <- g + ggplot2::geom_line(data = all_lines, mapping = mappingLines, size = 1,)
  }
  
  if(!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  if(no_legend == TRUE){
    g <- g + ggplot2::scale_colour_manual(values = "black")
  }else{
    g <- g + JASPgraphs::scale_JASPcolor_discrete(palette)
  }
  
  # axes
  g <- g + .plotXAxis(xName, xRange, discrete)
  g <- g + .plotYAxis(all_lines, all_arrows, CI)
  
  # legend
  if(!is.null(all_lines)){
    xr   <- range(all_lines$x)
    idx  <- which.max(all_lines$y)
    xmax <- all_lines$x[idx]
  }else{
    xr   <- range(all_arrows$x)
    idx  <- which.max(all_arrows$y_end)
    xmax <- all_arrows$x[idx]
  }
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
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
    
    label_y    <- if(length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
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
    legend.position = c(0.2, 0.875)
  }else{
    legend.position = c(0.8, 0.875)
  }
  
  if(no_legend == FALSE){
    g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  }else{
    g <- g + JASPgraphs::themeJaspRaw()
  }
  g <- g + JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
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
  
  
  if(!is.null(all_lines)){
    
    all_linesD <- all_lines
    all_linesL <- all_lines
    
    for(i in 1:length(all_linesD)){
      if(is.null(lCI) & is.null(uCI)){
        all_linesD[[i]] <- rbind.data.frame(
          data.frame(x = xRange[1], y = 0, g = all_linesD[[i]]$g[1]),
          all_linesD[[i]],
          data.frame(x = xRange[2], y = 0, g = all_linesD[[i]]$g[1])     
        )
      }else{
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
  
  if(!is.null(all_arrows)){
    
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
  
  if(!is.null(all_lines)){
    obsYmax <- max(all_lines$y)
    if(!is.null(all_arrows)){
      all_arrows$y_end <- obsYmax
    }
  }else{
    obsYmax <- max(all_arrows$y_end)    
  }
  yBreak  <- obsYmax/3 
  newymax <- obsYmax + yBreak*nrow(legend)
  
  legend$y <- yBreak*(0:(nrow(legend)-1))
  legend$x <- xRange[1]
  
  # changing y-coordinates to "stack" the plots
  for(i in 1:nrow(legend)){
    if(legend$type[i] == "spike"){
      all_arrows[all_arrows$g == legend[i,2], "y_start"] <- all_arrows[all_arrows$g == legend[i,2], "y_start"] + yBreak*(i-1)
      all_arrows[all_arrows$g == legend[i,2], "y_end"]   <- all_arrows[all_arrows$g == legend[i,2], "y_end"]   + yBreak*(i-1)
      all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     <- all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     + yBreak*(i-1)
    }else if(legend$type[i] %in% c("beta", "normal")){
      all_lines[all_lines$g == legend[i,2], "y"]   <- all_lines[all_lines$g == legend[i,2], "y"]   + yBreak*(i-1)
      all_linesD[all_linesD$g == legend[i,2], "y"] <- all_linesD[all_linesD$g == legend[i,2], "y"] + yBreak*(i-1)
      all_linesL[all_linesL$g == legend[i,2], "y"] <- all_linesL[all_linesL$g == legend[i,2], "y"] + yBreak*(i-1)
    }
  }
  
  g <- ggplot2::ggplot()
  
  for(i in nrow(legend):1){
    if(legend$type[i] == "spike"){
      g <- g + ggplot2::geom_segment(
        data = all_arrows[all_arrows$g == legend$name[i],],
        mapping = mappingArrows, size = 1,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
        ggplot2::geom_line(
          data = all_arrowsL[all_arrowsL$g == legend$name[i],],
          mapping = mappingLines)
    }
    if(legend$type[i] %in% c("beta", "normal")){
      g <- g + ggplot2::geom_line(
        data = all_lines[all_lines$g == legend$name[i],],
        mapping = mappingLines, size = 1) + 
        ggplot2::geom_polygon(
          data = all_linesD[all_linesD$g == legend$name[i],],
          mapping = mappingLines, fill = "grey60", alpha = .8)
      
      if(!is.null(lCI) & !is.null(uCI)){
        g <- g + ggplot2::geom_line(
          data = all_linesL[all_linesL$g == legend$name[i],],
          mapping = mappingLines
        )
      }
      
    }
  }
  
  if(!is.null(dfPoints)){
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
  if(obsXmax > 10){
    xBreaks <- round(seq(x_start, obsXmax, length.out = 7))
  }else{
    xBreaks <- x_start:obsXmax
  }
  
  if(is.null(yRange)){
    if(is.null(BF_log)){
      yRange <- c(0, 1)
    }else if(BF_log){
      yRange <- range(c(all_lines$y, 0))
    }else if(!BF_log){
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
  if(length(all_CI) > 0){
    names_CI <- NULL
    for(i in 1:length(all_CI)){
      names_CI <- c(names_CI, as.character(unique(all_CI[[i]]$name)))
    }
    clr1 <- clr[order(order(names_CI))]
  }
  
  
  g <- ggplot2::ggplot()
  
  if(length(all_CI) > 0){
    for(i in length(all_CI):1){
      if(is.null(all_CI[[i]]))next
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
  
  if(!is.null(BF_log)){
    if(BF_log){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(0, 0)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    }else if(!BF_log){
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
  }else{
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
.plotIndividualLS      <- function(all_lines, all_arrows, CI, CIall_lines, dfPoints = NULL, xRange, xName, yName = NULL, nRound = 3){ 
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g,)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)

  # set the CI text
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    CI         <- cbind.data.frame(CI, "y" = y_max * 1.05)
  }
  
  g <- ggplot2::ggplot()
  
  if(!is.null(all_arrows)){
    
    temp_arrows        <- all_arrows
    temp_arrows$y_end  <- temp_arrows$y_end * .scalingSpikes(all_lines, all_arrows)
    
    g <- g + ggplot2::geom_segment(
      data    = all_arrows,
      mapping = mappingArrows, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black")
  }
  
  if(!is.null(all_lines)){
    if(!is.null(CIall_lines)){
      g <- g + ggplot2::geom_polygon(
        data = CIall_lines,
        mapping = mappingLines, fill = "grey60", alpha = .8)
    }
    g <- g + ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1, color = "black") 
  }
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
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
    
    
    label_y    <- if(length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
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
  
  if(!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  # x-axes
  g <- g + .plotXAxis(xName, xRange, FALSE)
  g <- g + .plotYAxis(all_lines, all_arrows, CI)
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  return(plot)
}
.plotPredictionLS      <- function(dfHist, CI, xRange, xName, yName, nRound = 0, xBlacked = NULL,
                                   proportions = FALSE, predictionN = NULL){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  mappingArrows1    <- ggplot2::aes(x = x_start_adj , xend = x_end_adj, y = y, yend = y, group = g)
  mappingArrows2    <- ggplot2::aes(x = x_end_adj,  xend = x_start_adj, y = y, yend = y, group = g)
  mappingText       <- ggplot2::aes(x = x, y = y, label = label)
  
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    
    y_max_multiplier <- ifelse(length(temp_label) == 1, 1.15, 1.25)
  }
  
  yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, dfHist$y))
  if(proportions){
    xBreaks    <- JASPgraphs::getPrettyAxisBreaks(xRange)
    xBreaks[1] <- 0
    xBreaks[length(xBreaks)] <- 1
  }else{
    xBreaks  <- round(JASPgraphs::getPrettyAxisBreaks(xRange))
    xBreaks[length(xBreaks)] <- predictionN
  }
  
  
  if(xBreaks[length(xBreaks)] > xRange[2])xBreaks[length(xBreaks)] <- xRange[2]
  
  obsYmax    <- max(dfHist$y)
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(ifelse(!is.null(CI), y_max_multiplier + .05, 1.10) * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  if(!is.null(CI)){
    
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.10)
    
    if(!proportions){
      CI$x_start_adj <- CI$x_start - .5
      CI$x_end_adj   <- CI$x_end   + .5
    }else{
      CI$x_start_adj <- CI$x_start - .5/(predictionN + 1)
      CI$x_end_adj   <- CI$x_end   + .5/(predictionN + 1)
    }
    
    for(i in 1:nrow(CI)){
      dfHist$col[dfHist$x >= CI$x_start[i] & dfHist$x <= CI$x_end[i]] <- "b"
    }
  }
  if(!is.null(xBlacked)){
    dfHist[dfHist$x == xBlacked,"col"] <- "c"
  }
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_bar(
    data     = dfHist,
    mapping  = mappingHistogram,
    #fill     = "grey",
    col      = "black",
    stat     = "identity"
  )
  
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
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
  
  # control fill
  if(is.null(CI)){
    fillColor <- c("grey90") 
  }else{
    if(nrow(CI) == 1){
      if(all(xRange[1]:xRange[2] %in% CI$x_start:CI$x_end)){
        fillColor <- c("grey50") 
      }else{
        fillColor <- c("grey90", "grey50") 
      }
    }else{
      if(all(xRange[1]:xRange[2] %in% c(unlist(sapply(1:nrow(CI), function(i)CI$x_start[i]:CI$x_end[i]))))){
        fillColor <- c("grey50") 
      }else{
        fillColor <- c("grey90", "grey50") 
      }
    }
  }
  if(!is.null(xBlacked)){
    fillColor <- c(fillColor, "black")
  }
  
  
  if(!proportions){
    g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5, xRange[2] + .5))
  }else{
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
.CI_labelLS            <- function(CI, nRound){
  temp_int <- sapply(1:nrow(CI), function(i){
    if(is.na(CI$x_start[i])){
      x <- "none"
      #x <- "~symbol(\\306)"
    }else if(CI$x_start[i] == CI$x_end[i]){
      x <- paste(c(
        "[",format(round(CI$x_start[i], nRound), nsmall = nRound),"]"
      ), collapse = "")
    }else{
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
  
  
  if(CI$g[1] == "HPD"){
    temp_label <- paste(c(temp_cov,"['HPD']:",temp_int), collapse = "")
  }else if(CI$g[1] == "custom"){
    temp_label  <- paste(c("P({",format(round(CI$x_start, nRound), nsmall = nRound),"<=x}<=",
                           (format(round(CI$x_end, nRound), nsmall = nRound)),")","=='",round(CI$coverage[1]*100)," %'"), collapse = "")
  }else if(CI$g[1] == "support"){
    temp_label <- paste(c("SI['[BF = ",CI$BF[1],"]']:",temp_int), collapse = "")
  }else if(CI$g[1] == "central"){
    temp_label <- paste(c(temp_cov,":",temp_int), collapse = "")
  }
  
  if(nchar(temp_label) > 75){
    temp_o <- gregexpr(" and", substr(temp_label, 1, 65))
    temp_label <- c(paste(substr(temp_label, 1, temp_o[[1]][length(temp_o[[1]])]-1), "'", sep = ""), 
                    paste("'",substr(temp_label, temp_o[[1]][length(temp_o[[1]])], nchar(temp_label)), sep = ""))
  }
  
  return(temp_label)
}
.getYMax               <- function(all_lines, all_arrows){
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_max     <- max(c(all_lines$y, y_breaks))
  }else if(!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_max     <- max(c(all_lines$y, y_breaks))
  }else{
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_max     <- max(c(all_arrows$y_end, y_breaks))
  }
  return(y_max)
}
.scalingSpikes         <- function(all_lines, all_arrows){
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_max     <- .getYMax(all_lines, all_arrows)
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    return(y_max/max(y_breaks2))
  }else{
    return(1)
  }
}
.plotXAxis             <- function(x_name, x_range, discrete){
  
  x_breaks <- JASPgraphs::getPrettyAxisBreaks(x_range)
  
  if(discrete){
    x_breaks <- round(x_breaks)
    x_breaks <- unique(x_breaks[x_breaks >= x_range[1] &  x_breaks <= x_range[2]])
  }
  x_range <- range(c(x_range, x_breaks))
  
  return(ggplot2::scale_x_continuous(x_name, limits = x_range, breaks = x_breaks))
}
.plotYAxis             <- function(all_lines, all_arrows, CI){
  
  y_max <- .getYMax(all_lines, all_arrows)
  
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_pos2    <- y_breaks2/(max(y_breaks2)/y_max)
  }else if(!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
  }else{
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
  }
  
  # set the y-scale plotting range
  if(!is.null(CI)){
    y_range    <- c(0, y_max * 1.20) 
  }else{
    y_range    <- c(0, y_max) 
  }
  
  if(!is.null(all_lines) & !is.null(all_arrows)){
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
  }else{
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
  if(!is.null(all_lines) & !is.null(all_arrows)){
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3),
        plot.margin = ggplot2::margin(t = 3, r = 10, b = 0, l = 1))
    )    
  }else{
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3.5),
        plot.margin = ggplot2::margin(t = 3, r = 0, b = 0, l = 1))
    )
  }
}
