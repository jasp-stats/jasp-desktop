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

# When making changes to this file always mention @koenderks as a reviewer in the Pull Request

.auditRiskModel <- function(options, jaspResults){

  if(!is.null(jaspResults[["ARMcontainer"]])) return()

  ARMcontainer <- createJaspContainer(title= "<u>Audit Risk Model</u>")
  ARMcontainer$position <- 2
  ARMcontainer$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "materiality", "explanatoryText", "valuta"))

  #  Audit Risk Model formula
  .ARMformula(options, jaspResults, position = 2, ARMcontainer)
  DR                          <- jaspResults[["DR"]]$object
  
  if(!is.null(ARMcontainer[["AuditRiskModelParagraph"]])){
    return()
  } else {
    if(options[["explanatoryText"]]){
      materialityLevelLabel <- base::switch(options[["materiality"]], "materialityRelative" = paste0(round(options[["materialityPercentage"]], 10) * 100, "%"), "materialityAbsolute" = paste(jaspResults[["valutaTitle"]]$object, format(options[["materialityValue"]], scientific = FALSE)))
      auditRiskLabel        <- paste0(round((1 - options[["confidence"]]) * 100, 2), "%")
      dectectionRiskLabel   <- paste0(round(DR * 100, 2), "%")
  
      ARMcontainer[["AuditRiskModelParagraph"]] <- createJaspHtml(paste0("Prior to the substantive testing phase, the inherent risk was determined to be <b>", options[["IR"]] ,"</b>. The internal control risk was determined
                                                                      to be <b>", options[["CR"]] ,"</b>. According to the Audit Risk Model, the required detection risk to maintain an audit risk of <b>", auditRiskLabel, "</b> for a materiality
                                                                      of <b>", materialityLevelLabel ,"</b> should be <b>", dectectionRiskLabel , "</b>. The translation of High, Medium and Low to probabilities is done according to <b>IODAD (2007)</b>."), "p")
      ARMcontainer[["AuditRiskModelParagraph"]]$position <- 1
      ARMcontainer[["AuditRiskModelParagraph"]]$dependOn(options = c("confidence", "IR", "CR", "materialityPercentage", "materialityValue", "valuta"))
    }
  }
  jaspResults[["ARMcontainer"]] <- ARMcontainer
}

.ARMformula <- function(options, jaspResults, position = 2, ARMcontainer){

    if(!is.null(ARMcontainer[["ARMformula"]])) return()

    AR                      <- 1 - options[["confidence"]]
    IR                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    CR                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
    DR                      <- AR / IR / CR

    jaspResults[["DR"]]     <- createJaspState(DR)
    jaspResults[["DR"]]     $dependOn(options = c("IR", "CR", "confidence"))

    text <- paste0("Audit risk (", round(AR * 100, 2),"%) = Inherent risk (", round(IR * 100, 2), "%) x Control risk (", round(CR * 100, 2), "%) x Detection risk (", round(DR * 100, 2), "%)")

    ARMcontainer[["ARMformula"]] <- createJaspHtml(text, "h3")
    ARMcontainer[["ARMformula"]]$position <- position
    ARMcontainer[["ARMformula"]]$dependOn(options = c("IR", "CR", "confidence"))
}

.bookValueDescriptives <- function(dataset, options, jaspResults, position, procedureContainer){

  if(!is.null(procedureContainer[["bookValueDescriptives"]])) return() #The options for this table didn't change so we don't need to rebuild it

  dataTable                                                 <- createJaspTable("Book Value Descriptives")
  dataTable$position                                        <- position
  dataTable$dependOn(options = c("monetaryVariable", "recordNumberVariable", "bookValueDescriptives"))

  dataTable$addColumnInfo(name = 'popSize',     title = "Population size",        type = 'string')
  dataTable$addColumnInfo(name = 'value',       title = "Total value",            type = 'string')
  dataTable$addColumnInfo(name = 'mean',        title = "Mean",                   type = 'string')
  dataTable$addColumnInfo(name = 'sd',          title = "Std. deviation",         type = 'string')
  dataTable$addColumnInfo(name = 'p1',          title = "25%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p2',          title = "50%",                    type = 'string', overtitle = "Percentile")
  dataTable$addColumnInfo(name = 'p3',          title = "75%",                    type = 'string', overtitle = "Percentile")

  procedureContainer[["bookValueDescriptives"]]        <- dataTable

  if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "")
    return()

  popSize                           <- jaspResults[["N"]]$object
  values                            <- dataset[, .v(options[["monetaryVariable"]])]
  total.value                       <- paste(jaspResults[["valutaTitle"]]$object, round(sum(abs(values)), 2))
  mean.value                        <- paste(jaspResults[["valutaTitle"]]$object, round(mean(values), 2))
  sd.value                          <- paste(jaspResults[["valutaTitle"]]$object, round(sd(values), 2))
  Q                                 <- paste(jaspResults[["valutaTitle"]]$object, round(as.numeric(quantile(values, c(0.25, 0.50, 0.75))), 2))

  row <- data.frame(popSize = popSize, value = total.value, mean = mean.value, sd = sd.value, p1 = Q[1], p2 = Q[2], p3 = Q[3])
  dataTable$addRows(row)
}

.bookValueDistribution <- function(dataset, options, jaspResults, position, procedureContainer){

  if(!is.null(procedureContainer[["bookValueDistribution"]])) return()

  bookValuePlot <- createJaspPlot(plot = NULL, title = "Book Value Distribution", width = 600, height = 300)
  bookValuePlot$position <- position
  bookValuePlot$dependOn(options = c("bookValueDistribution", "monetaryVariable", "valuta"))

  procedureContainer[["bookValueDistribution"]] <- bookValuePlot

  if(options[["monetaryVariable"]] == "" || options[["recordNumberVariable"]] == "") return()

  values  <- dataset[, .v(options[["monetaryVariable"]])]
  meanx   <- mean(values)
  sdx     <- sd(values)
  q       <- as.numeric(quantile(values, c(0.25, 0.5, 0.75)))
  minx    <- min(q[1], meanx - sdx)
  maxx    <- max(q[3], meanx + sdx)

  p <- .plotMarginalJfA(values, options[["monetaryVariable"]], jaspResults)

  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[1], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[2], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = q[3], y = 0), shape = 21, fill = "orange", stroke = 2, size = 3)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx, y = 0), shape = 21, fill = "red", stroke = 2, size = 5)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx + sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)
  p <- p + ggplot2::geom_point(ggplot2::aes(x = meanx - sdx, y = 0), shape = 21, fill = "dodgerblue1", stroke = 2, size = 4)

  pdata <- data.frame(x = c(0,0,0), y = c(0,0,0), l = c("1","2","3"))
  p <- p + ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, shape = l), size = 0, color = c(rgb(0,1,0,0))) +
            ggplot2::scale_shape_manual(name = "", values = c(21,21,21), labels = c("Mean", "Mean \u00B1 sd", "Quartile")) +
            ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = c(5, 4, 3), shape = 21, fill = c("red","dodgerblue1", "orange"), stroke = 2, color = "black")), order = 1) +
            ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -10, r = 50))) +
            ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
  
  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  bookValuePlot$plotObject <- p

  if(options[["explanatoryText"]]){
      figure1 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> The distribution of book values in the audit population. The red and blue dots respectively represent the mean
                                                                                        and the values exactly one standard deviation from the mean. The orange dots represent the 25th, 50th (median) and
                                                                                        75th percentile of the book values."), "p")
      figure1$position <- position + 1
      figure1$dependOn(optionsFromObject= bookValuePlot)
      procedureContainer[["figure1"]] <- figure1
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
      jaspResults[["figNumber"]]$dependOn(options = c("bookValueDistribution", "decisionPlot"))
  }
}

.plotMarginalJfA <- function(column, variableName, jaspResults, rugs = FALSE, displayDensity = FALSE) {
  column <- as.numeric(column)
  variable <- na.omit(column)

  if(length(variable) == 0)
    return(NULL)

  h <- hist(variable, plot = FALSE)

  if (!displayDensity)
    yhigh <- max(h$counts)
  else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  ylow <- 0

  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity)
    p <-
      JASPgraphs::drawAxis(
        xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = "Counts", xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  else
    p <-
      JASPgraphs::drawAxis(
        xName = variableName, yName = "Density", xBreaks = xticks,
        yBreaks = c(0,  1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )

  if (displayDensity)
    p <- p +
      ggplot2::geom_histogram(
        data = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill = "grey",
        col = "black",
        size = .7,
        center = ((h$breaks[2] - h$breaks[1])/2)
      ) +
      ggplot2::geom_line(
        data = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd = 1,
        col = "black"
      )
  else
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        binwidth = (h$breaks[2] - h$breaks[1]),
        fill     = "grey",
        col      = "black",
        size     = .7,
        center    = ((h$breaks[2] - h$breaks[1])/2)
      )

  # JASP theme
  p <- JASPgraphs::themeJasp(p,
                             axisTickWidth = .7,
                             bty = list(type = "n", ldwX = .7, lwdY = 1))
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.evaluationInformation <- function(options, evaluationResult, jaspResults, position, evaluationContainer){

  if(!is.null(evaluationContainer[["evaluationInformation"]])) return()

  evaluationInformation <- createJaspPlot(plot = NULL, title = "Evaluation Information", width = 600, height = 300)
  evaluationInformation$position <- position
  evaluationInformation$dependOn(options = c("IR", "CR", "confidence", "auditResult", "evaluationInformation", "materialityPercentage", "estimator", "materialityValue", "valuta", "performAudit"))

  evaluationContainer[["evaluationInformation"]] <- evaluationInformation

  if(!jaspResults[["runEvaluation"]]$object) return()

  materiality       <- jaspResults[["materiality"]]$object
  bound             <- evaluationResult[["bound"]]
  proj.misstatement <- bound * jaspResults[["total_data_value"]]$object
  if(options[["variableType"]] == "variableTypeCorrect"){
    if(options[["estimator"]] == "gammaBound" || options[["estimator"]] == "binomialBound" || options[["estimator"]] == "hyperBound"){
      mle <- evaluationResult[["k"]] / evaluationResult[["n"]]
    } else {
      mle <- (evaluationResult[["posteriorA"]] - 1) / (evaluationResult[["posteriorA"]] + evaluationResult[["posteriorB"]] - 2)
    }
  } else {
    if(options[["estimator"]] == "stringerBound"){
      mle <- sum(evaluationResult[["z"]]) / evaluationResult[["n"]]
    } else if(options[["estimator"]] == "coxAndSnellBound") {
      mle <- evaluationResult[["mf"]] * ( (evaluationResult[["df1"]] - 2)  / evaluationResult[["df1"]] ) * ( evaluationResult[["df2"]] / (evaluationResult[["df2"]] + 2) ) 
    } else {
      mle <- abs(evaluationResult[["mleTable"]])
    }
  }
 
  label             <- rev(c("Materiality", "Maximum error", "Most likely error"))
  values            <- rev(c(materiality, bound, mle))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute")
    values          <- values * jaspResults[["total_data_value"]]$object
  
  boundColor        <- ifelse(bound < materiality, yes = rgb(0,1,.7,1), no = rgb(1,0,0,1))
  fillUp            <- rev(c("#1380A1", boundColor, "#1380A1"))
  yBreaks           <- as.numeric(JASPgraphs::getPrettyAxisBreaks(c(0, values), min.n = 4))
  
  if(options[["variableType"]] == "variableTypeAuditValues" && options[["materiality"]] == "materialityAbsolute"){
    x.labels        <- format(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4), scientific = FALSE)
    values.labels   <- paste(jaspResults[["valutaTitle"]]$object, ceiling(values))
    x.title         <- ""
  } else {
    x.labels        <- paste0(round(JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4) * 100, 4), "%")
    values.labels   <- paste0(round(values * 100, 2), "%")
    x.title         <- ""
  }

  tb                <- data.frame(x = label, values = values)
  tb$x              <- factor(tb$x, levels = tb$x)
  p                 <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
                        ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fillUp) +
                        ggplot2::coord_flip() +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(x.title) +
                        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
                        ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"))+
                        ggplot2::annotate("text", y = values, x = c(1, 2, 3), label = values.labels, size = 6, vjust = 0.5, hjust = -0.3) + 
                        ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(seq(0, 1.1*max(values), length.out = 100), min.n = 4), limits = c(0, 1.1*max(values)), labels = x.labels)
  p                 <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE)

  evaluationInformation$plotObject <- p

  if(options[["explanatoryText"]]){
      figure4 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Evaluation information regarding the evaluation of the selection. The materiality is compared with the 
                                        maximum misstatement and the most likely error. The most likely error (MLE) is an estimate of the true misstatement 
                                        in the population. The maximum error is an estimate of the maximum error in the population."), "p")
      figure4$position <- position + 1
      figure4$dependOn(optionsFromObject = evaluationInformation)
      evaluationContainer[["figure4"]] <- figure4
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}

.correlationPlot <- function(dataset, options, jaspResults, position, evaluationContainer) {

  if(!is.null(evaluationContainer[["correlationPlot"]])) return()

  correlationPlot <- createJaspPlot(plot = NULL, title = "Correlation Plot", width = 500, height = 400)
  correlationPlot$position <- position
  correlationPlot$dependOn(options = c("auditResult", "correlationPlot", "monetaryVariable", "valuta", "performAudit"))

  evaluationContainer[["correlationPlot"]] <- correlationPlot

  if(!jaspResults[["runEvaluation"]]$object) return()

  d <- data.frame(xx= dataset[,.v(options[["monetaryVariable"]])], yy= dataset[,.v(options[["auditResult"]])])
  co <- cor(d$xx, d$yy, method = "pearson")
  d <- na.omit(d)
  d <- ceiling(d)
  xVar <- d$xx
  yVar <- d$yy

  fit <- vector("list", 1)# vector("list", 4)
  fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), data = d)

  bestModel <- 1 # which.min(Bic)

  # format x labels
  xlow <- min(pretty(xVar))
  xhigh <- max(pretty(xVar))
  xticks <- pretty(c(xlow, xhigh))
  xLabs <- vector("character", length(xticks))
  xLabs <- format(xticks, digits= 3, scientific = FALSE)

  # Format y labels
  yticks <- xticks
  yLabs <- vector("character", length(yticks))
  yLabs <- format(yticks, digits= 3, scientific = FALSE)

  co <- round(co, 3)

  cols <- rep("gray", nrow(d))
  cols[which(d$xx != d$yy)] <- "red"

  p <- JASPgraphs::drawAxis(xName = paste0("Book values (", jaspResults[["valutaTitle"]]$object, ")"), yName = paste0("Audit values (", jaspResults[["valutaTitle"]]$object, ")"), xBreaks = xticks, yBreaks = yticks, yLabels = yLabs, xLabels = xLabs, force = TRUE)
  p <- JASPgraphs::drawPoints(p, dat = d, size = 3, fill = cols)
  p <- .poly.predJfA(fit[[bestModel]], plot = p, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd = 1)
  p <- p + ggplot2::annotate("text", x = xticks[1], y = (yticks[length(yticks)] - ((yticks[length(yticks)] - yticks[length(yticks) - 1]) / 2)),
                              label = paste0("italic(r) == ", co), size = 8, parse = TRUE, hjust = -0.5, vjust = 0.5)
  p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"), panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"))
  
  p <- JASPgraphs::themeJasp(p)

  correlationPlot$plotObject <- p

  if(options[["explanatoryText"]]){
      figure6 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Scatterplot of the book values in the selection and their audit values. Red dots indicate observations that 
                                        did not match their original book value. If these red dots lie in the bottom part of the graph, the observations are overstated. 
                                        If these red dots lie in the upper part of the graph, they are understated. The value <i>r</i> is the Pearson correlation coefficient 
                                        of the book values and the audit values, an indicator of the strengh of the linear relationship between the two variables."), "p")
      figure6$position <- position + 1
      figure6$dependOn(optionsFromObject = correlationPlot)
      evaluationContainer[["figure6"]] <- figure6
      jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}

.poly.predJfA <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)

  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste(f, temp, sep="")
    }

    if (i > 1) {
      temp <- paste("(", coef(fit)[[i]], ")*", "x^", i-1, sep="")
      f <- paste(f, temp, sep="+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text=f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd, lty = 1)
    return(plot)
  }
}

.readDataProcedure <- function(options, jaspResults){
  
  recordNumberVariable                    <- options[["recordNumberVariable"]]
  if(recordNumberVariable == "")          recordNumberVariable <- NULL 
  monetaryVariable                        <- options[["monetaryVariable"]]
  if(monetaryVariable == "")              monetaryVariable <- NULL 
  
  if(!is.null(recordNumberVariable)){
    variables                             <- recordNumberVariable
    if(!is.null(monetaryVariable)){
      variables <- c(variables, monetaryVariable)
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["uniqueN"]]            <- createJaspState(length(unique(dataset[, .v(options[["recordNumberVariable"]])])))
      jaspResults[["total_data_value"]]   <- createJaspState( ceiling(sum(dataset[, .v(monetaryVariable)])))
      jaspResults[["ready"]]              <- createJaspState(TRUE) # Ready for analysis
    } else {
      dataset <- .readDataSetToEnd(columns.as.numeric = variables)
      jaspResults[["N"]]                  <- createJaspState(nrow(dataset))
      jaspResults[["uniqueN"]]            <- createJaspState(length(unique(dataset[, .v(options[["recordNumberVariable"]])])))
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      if(options[["materiality"]] == "materialityRelative"){
        jaspResults[["ready"]]            <- createJaspState(TRUE) # Ready for analysis
      } else {
        jaspResults[["ready"]]            <- createJaspState(FALSE) # Ready for analysis
      }
    }
  } else {
      dataset                             <- NULL
      jaspResults[["N"]]                  <- createJaspState(0)
      jaspResults[["uniqueN"]]            <- createJaspState(0)
      jaspResults[["total_data_value"]]   <- createJaspState(0.01)
      jaspResults[["ready"]]              <- createJaspState(FALSE)
  }
  materialityReady <- ifelse(options[["materiality"]] == "materialityRelative", yes = options[["materialityPercentage"]], no = options[["materialityValue"]])
  if(materialityReady == 0)
    jaspResults[["ready"]]              <- createJaspState(FALSE)

  jaspResults[["N"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["uniqueN"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["total_data_value"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable"))
  jaspResults[["ready"]]$dependOn(options = c("recordNumberVariable", "monetaryVariable", "materiality"))
  return(dataset)
}

.readDataSelection <- function(options){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  rankingVariable                 <- unlist(options[["rankingVariable"]])
  if(rankingVariable == "")       rankingVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  variables                       <- unlist(options[["additionalVariables"]])
  variables.to.read               <- c(recordVariable, variables, rankingVariable, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)
  return(dataset)
}

.execution <- function(options, jaspResults){
  if(options[["pasteVariables"]]){  
    dataset                       <- .readDataSetToEnd(columns.as.numeric = options[["recordNumberVariable"]])
    sampleFilter                  <- rep(0, jaspResults[["N"]]$object)
    rowNumber                     <- which(dataset[, .v(options[["recordNumberVariable"]])] %in% jaspResults[["sample"]]$object[, .v(options[["recordNumberVariable"]])])
    noOfTimesInSample             <- table(jaspResults[["sampleVector"]]$object)
    sampleFilter[rowNumber]       <- 1 * noOfTimesInSample
    sampleFilter                  <- as.numeric(sampleFilter)
    auditDataVariable             <- rep(NA, jaspResults[["N"]]$object)

    auditDataVariable[options[["performAudit"]][[1]]$rowIndices] <- options[["performAudit"]][[1]]$values

    if(is.null(jaspResults[["sampleFilter"]]))  jaspResults[["sampleFilter"]] <- createJaspColumn(columnName=options[["sampleFilter"]], dependencies="sampleFilter")
    if(is.null(jaspResults[["variableName"]]))  jaspResults[["variableName"]] <- createJaspColumn(columnName=options[["variableName"]], dependencies="variableName")

    jaspResults[["sampleFilter"]]$setScale(sampleFilter)
    jaspResults[["variableName"]]$setScale(auditDataVariable)
  }
}

.readDataEvaluation <- function(options, jaspResults){
  recordVariable                  <- unlist(options[["recordNumberVariable"]])
  if(recordVariable == "")        recordVariable <- NULL
  monetaryVariable                <- unlist(options[["monetaryVariable"]])
  if(monetaryVariable == "")      monetaryVariable <- NULL
  sampleFilter                    <- unlist(options[["sampleFilter"]])
  if(sampleFilter == "")          sampleFilter <- NULL
  auditResult                     <- unlist(options[["auditResult"]])
  if(auditResult == "")           auditResult <- NULL
  variables.to.read               <- c(recordVariable, auditResult, sampleFilter, monetaryVariable)
  dataset                         <- .readDataSetToEnd(columns.as.numeric = variables.to.read)

  jaspResults[["runEvaluation"]] <- createJaspState( (!is.null(auditResult) && !is.null(sampleFilter)) )
  jaspResults[["runEvaluation"]]$dependOn(options = c("auditResult", "sampleFilter", "performAudit"))
  return(dataset)
}

.errorHandlingProcedure <- function(options, dataset){
  variables <- NULL
  if(options[["recordNumberVariable"]] != "")
    variables <- c(variables, options[["recordNumberVariable"]])
  if(options[["monetaryVariable"]] != "")
    variables <- c(variables, options[["monetaryVariable"]])
  n <- nrow(dataset)

    .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
            all.target = variables, message="short", observations.amount= paste0("< ", n),
            exitAnalysisIfErrors = TRUE)
}

.decisionAnalysis <- function(options, jaspResults, position, planningContainer, type){

  if(!is.null(planningContainer[["decisionPlot"]])) return()

  decisionPlot <- createJaspPlot(plot = NULL, title = "Decision Analysis", width = 600, height = 300)
  decisionPlot$position <- position
  decisionPlot$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", "decisionPlot", "materialityValue", "explanatoryText"))

  planningContainer[["decisionPlot"]] <- decisionPlot

  if(!jaspResults[["ready"]]$object || planningContainer$getError()) return()

  ar                      <- 1 - options[["confidence"]]
  ir                      <- base::switch(options[["IR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  cr                      <- base::switch(options[["CR"]], "Low" = 0.50, "Medium" = 0.60, "High" = 1)
  alpha                   <- ar / ir / cr

  if(type == "frequentist"){

      n <- c(.calc.n.poisson(options, alpha, jaspResults),
          .calc.n.binomial(options, alpha, jaspResults),
          .calc.n.hypergeometric(options, alpha, jaspResults))

      kpois <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(options[["expectedPercentage"]] * n[1], 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[1], 2))
      kbinom <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(options[["expectedPercentage"]] * n[2]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[2]))
      khyper <- base::switch(options[["expectedErrors"]], "expectedRelative" = ceiling(options[["expectedPercentage"]] * n[3]), "expectedAbsolute" = ceiling(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n[3]))

      k <- c(round(kpois, 2), kbinom, khyper)

      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Poisson", "Binomial", "Hypergeometric"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), each = 3))
      d$dist = factor(d$dist,levels(d$dist)[c(2,1,3)])
      d$nature = factor(d$nature,levels(d$nature)[c(1,2)])
      
      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
          ggplot2::coord_flip() +
          ggplot2::xlab("") +
          ggplot2::ylab("Required sample size") +
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb")) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
          ggplot2::annotate("text", y = k, x = c(3, 2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.3) + 
          ggplot2::annotate("text", y = n, x = c(3, 2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.5) + 
          ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1*max(n))), min.n = 4), limits = c(0, ceiling(1.1*max(n)))) +
          ggplot2::ylim(0, ceiling(1.2*max(n)))
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

      optN <- base::switch(which.min(n), "1" = "Poisson", "2" = "binomial", "3" = "hypergeometric")
      jaspResults[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
      jaspResults[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                                                          "decisionPlot", "materialityValue"))

  } else if(type == "bayesian"){

      n <- c(.calc.n.beta(options, alpha, jaspResults), .calc.n.betabinom(options, alpha, jaspResults))
      k <- base::switch(options[["expectedErrors"]], "expectedRelative" = round(options[["expectedPercentage"]] * n, 2), "expectedAbsolute" = round(options[["expectedNumber"]] / jaspResults[["total_data_value"]]$object * n, 2))
      
      d <- data.frame(y = c(n, k), 
                      dist = rep(c("Beta", "Beta-binomial"), 2),
                      nature = rep(c("Expected error-free", "Expected errors"), each = 2))
      d$dist = factor(d$dist,levels(d$dist)[c(2,1)])
      d$nature = factor(d$nature,levels(d$nature)[c(1,2)])
      
      p <- ggplot2::ggplot(data = d, ggplot2::aes(x = dist, y = y, fill = nature)) +
          ggplot2::geom_bar(stat = "identity", col = "black", size = 1) +
          ggplot2::coord_flip() +
          ggplot2::xlab("") +
          ggplot2::ylab("Required sample size") +
          ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(hjust = 0)) +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb")) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
          ggplot2::annotate("text", y = k, x = c(2, 1), label = k, size = 6, vjust = 0.5, hjust = -0.3) + 
          ggplot2::annotate("text", y = n, x = c(2, 1), label = n, size = 6, vjust = 0.5, hjust = -0.5) + 
          ggplot2::scale_y_continuous(breaks = JASPgraphs::getPrettyAxisBreaks(0:(ceiling(1.1*max(n))), min.n = 4), limits = c(0, ceiling(1.1*max(n)))) +
          ggplot2::ylim(0, ceiling(1.2*max(n)))
      p <- JASPgraphs::themeJasp(p, xAxis = FALSE, yAxis = FALSE, legend.position = "top")

      optN <- base::switch(which.min(n), "1" = "beta", "2" = "beta-binomial")
      jaspResults[["mostEfficientPlanningDistribution"]] <- createJaspState(optN)
      jaspResults[["mostEfficientPlanningDistribution"]]$dependOn(options = c("IR", "CR", "confidence", "materialityPercentage", "expectedErrors", "expectedPercentage", "expectedNumber", 
                                                                            "decisionPlot", "materialityValue"))
  }

  decisionPlot$plotObject <- p

  if(options[["explanatoryText"]]){
        figure2 <- createJaspHtml(paste0("<b>Figure ", jaspResults[["figNumber"]]$object ,".</b> Decision analysis for the current options. The bars represent the sample size that is required under different planning distributions.
                                                                                    The the number of expected errors in the selection is colored in red and the number of expected error-free observations is colored in green. 
                                                                                    The most efficient distribution for these options is the <b>", jaspResults[["mostEfficientPlanningDistribution"]]$object ,"</b> distribution."), "p")
        figure2$position <- position + 1
        figure2$dependOn(optionsFromObject = decisionPlot)
        planningContainer[["figure2"]] <- figure2
        jaspResults[["figNumber"]] <- createJaspState(jaspResults[["figNumber"]]$object + 1)
  }
}
