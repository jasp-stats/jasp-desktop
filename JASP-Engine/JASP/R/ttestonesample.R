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

TTestOneSample <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$variables) > 0
  type  <- "one-sample"
  if(ready) {
    dataset <- .ttestReadData(dataset, options, type)
    .ttestCheckErrors(        dataset, options, type)
  }
  # Output tables (each calls its own results function)
  .ttestMainTable(  jaspResults, dataset, options, ready, type)
  .ttestNormalTable(jaspResults, dataset, options, ready, type)
  
  # Descriptives
  .ttestDescriptivesTable(    jaspResults, dataset, options, ready, type)
  .ttestOneDescriptivesPlot(  jaspResults, dataset, options, ready)
  
  return()
}

# Plot
.ttestOneDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlots)
    return()
  .ttestDescriptivesContainer(jaspResults, options)
  container <- jaspResults[["ttestDescriptives"]]
  container[["title"]] <- createJaspContainer("Descriptives Plot")
  subcontainer <- container[["title"]]
  for(variable in options$variables) {
    title <- variable
    descriptivesPlot      <- createJaspPlot(title = title, width = 480, height = 320)
    descriptivesPlot$dependOn(optionContainsValue = list(variables = variable))
    subcontainer[[title]] <- descriptivesPlot
    
    if(ready){
      p <- try(.ttestOneDescriptivesPlotFill(jaspResults, dataset, options, ready, variable))
      if(isTryError(p))
        descriptivesPlot$setError(.extractErrorMessage(p))
      else
        descriptivesPlot$plotObject <- p
    }
  }
  return()
}

.ttestOneDescriptivesPlotFill <- function(jaspResults, dataset, options, ready, variable){
  base_breaks_y <- function(x, options) {
    
    values <- c(options$testValue, x[, "dependent"] - x[, "ci"],
                x[, "dependent"] + x[, "ci"])
    ci.pos <- c(min(values), max(values))
    b <- pretty(ci.pos)
    d <- data.frame(x = -Inf, xend = -Inf, y = min(b), yend = max(b))
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend), inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(min(b),  options$testValue, max(b))))
  }
  
  dataSubset <- data.frame(dependent = dataset[[.v(variable)]],
                           groupingVariable = rep(variable, length(dataset[[.v(variable)]])))
  
  ci <- options$descriptivesPlotsConfidenceInterval
  summaryStat <- .summarySE(dataSubset, measurevar = "dependent",
                            groupvars = "groupingVariable",
                            conf.interval = ci, na.rm = TRUE, .drop = FALSE)
  testValue <- data.frame(testValue = options$testValue)
  pd <- ggplot2::position_dodge(0.2)
  p <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = groupingVariable, y = dependent, group = 1)) 
  p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = ciLower,  ymax = ciUpper), colour = "black", width = 0.2, position = pd)
  p <- p + ggplot2::geom_line(position = pd, size = 0.7) #gives geom_path warning
  p <- p + ggplot2::geom_point(position = pd, size = 4) 
  p <- p + ggplot2::geom_hline(data = testValue, ggplot2::aes(yintercept = testValue), linetype = "dashed") 
  p <- p + ggplot2::ylab(NULL) + ggplot2::xlab(NULL) + base_breaks_y(summaryStat, options) 
  p <- JASPgraphs::themeJasp(p) + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  
  return(p)
}
