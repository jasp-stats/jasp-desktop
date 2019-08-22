#
# Copyright (C) 2013-2018 University of Amsterdam
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

TTestPairedSamples <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$pairs) > 0  #at least one variable pair
  type  <- "paired"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestMainTable(  jaspResults, dataset, options, ready, type)
  .ttestNormalTable(jaspResults, dataset, options, ready, type)
  
  # Descriptives
  .ttestDescriptivesTable(    jaspResults, dataset, options, ready, type)
  .ttestPairDescriptivesPlot( jaspResults, dataset, options, ready)
  
  return()
}

# Plot
.ttestPairDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(pair in options$pairs) {
    title <- paste(pair, collapse=" - ")
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(pairs = pair))
    subcontainer[[title]] <- descriptivesPlot
    
    if(ready){
      p <- try(.ttestPairDescriptivesPlotFill(jaspResults, dataset, options, ready, pair))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestPairDescriptivesPlotFill <- function(jaspResults, dataset, options, ready, pair){
  base_breaks_x <- function(x) {
    b <- unique(as.numeric(x))
    d <- data.frame(y = -Inf, yend = -Inf, x = min(b), xend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1))
  }
  
  base_breaks_y <- function(x) {
    ci.pos <- c(x[, "dependent"] - x[, "ci"], x[, "dependent"] + x[, "ci"])
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1), ggplot2::scale_y_continuous(breaks = c(min(b),
                                                                                                                                           max(b))))
  }
  
  c1 <- dataset[[ .v(pair[[1]]) ]]
  c2 <- dataset[[ .v(pair[[2]]) ]]
  ####
  data <- data.frame(id = rep(1:length(c1), 2), dependent = c(c1, c2),
                     groupingVariable = c(rep(paste("1.", pair[[1]], sep = ""), length(c1)),
                                          rep(paste("2.", pair[[2]], sep = ""), length(c2))))
  
  summaryStat <- .summarySEwithin(data, measurevar = "dependent", withinvars = "groupingVariable",
                                  idvar = "id", conf.interval = options$descriptivesPlotsConfidenceInterval,
                                  na.rm = TRUE, .drop = FALSE)
  
  pd <- ggplot2::position_dodge(0.2)
  
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable,
                                                 y = dependent, group = 1)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,
                                                                                                                  ymax = ciUpper), colour = "black", width = 0.2, position = pd) +
    ggplot2::geom_line(position = pd, size = 0.7) + ggplot2::geom_point(position = pd,
                                                                        size = 4) + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat) +
    base_breaks_x(summaryStat$groupingVariable) + ggplot2::scale_x_discrete(labels = c(pair[[1]], pair[[2]]))
  
  p <- JASPgraphs::themeJasp(p)
  
  return(p)
}
