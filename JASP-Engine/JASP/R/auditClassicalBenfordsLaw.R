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

# When making changes to this file always mention @koenderks as a 
# reviewer in the Pull Request

auditClassicalBenfordsLaw <- function(jaspResults, dataset, options, ...){

  # Create the procedure paragraph
  .auditBenfordsLawProcedureParagraph(options, 
                                      jaspResults,
                                      position = 1)
  
  # Read in the data 
  dataset <- .auditReadDataBenfordsLaw(dataset, 
                                       options)

  # Perform early error checks
  .auditClassicalBenfordsLawErrorCheck(dataset, 
                                       options)

  # Ready for analysis
  ready <- .auditClassicalBenfordsLawReady(options)

  benfordsLawContainer <- .auditBenfordsLawGetContainer(options,
                                                        jaspResults, 
                                                        position = 2)

  # --- TABLES

  .auditCreateTableNumber(jaspResults) # Initialize table numbers
  
  # Create the goodness-of-fit table
  .auditClassicalBenfordsLawTestTable(dataset, 
                                    options, 
                                    benfordsLawContainer, 
                                    jaspResults,
                                    ready, 
                                    positionInContainer = 1)

  # Create the observed and predicted probabilities table                                  
  .auditClassicalBenfordsLawSummaryTable(dataset, 
                                         options, 
                                         benfordsLawContainer, 
                                         jaspResults,
                                         ready, 
                                         positionInContainer = 2)

  # ---

  # --- PLOTS

  .auditCreateFigureNumber(jaspResults) # Initialize figure numbers
  
  # Create the observed and predicted probabilities plot
  .benfordsLawPlot(dataset, 
                   options, 
                   benfordsLawContainer, 
                   jaspResults,
                   ready, 
                   positionInContainer = 3)

  # ---

  # Create the conclusion paragraph
  .auditBenfordsLawConclusionParagraph(options,
                                       benfordsLawContainer,
                                       jaspResults,
                                       ready,
                                       position = 3)

  # ---
}

.auditReadDataBenfordsLaw <- function(dataset, 
                                     options){

  if (!is.null(dataset)) return(dataset)

  values <- options[["values"]]
  if(values == "")  
    values <- NULL

  dataset <- .readDataSetToEnd(columns.as.numeric = values, 
                               exclude.na.listwise = values)
  return(dataset)
}

.auditBenfordsLawProcedureParagraph <- function(options,
                                                jaspResults,
                                                position){

  if(options[["explanatoryText"]] && 
     is.null(jaspResults[["procedureContainer"]])){

    procedureContainer <- createJaspContainer(title = gettext("<u>Procedure</u>"))
    procedureContainer$position <- position

    confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

    if(options[["distribution"]] == "benford"){

      procedureText <- base::switch(options[["digits"]],
                                    "first" = gettextf("Benford's law states that in many naturally occurring collections of numerical observations, the leading significant digit is likely to be small. The goal of this procedure is to determine to what extent the first leading digits in the data set follow Benford's law, and to test this relation with <b>%1$s</b> confidence. Data that are not distributed according to Benford's law might need further investigation.", confidenceLabel),
                                    "firstSecond" = gettextf("Benford's law states that in many naturally occurring collections of numerical observations, the leading significant digit is likely to be small. The goal of this procedure is to determine to what extent the first two leading digits in the data set follow Benford's law, and to test this relation with <b>%1$s</b> confidence. Data that are not distributed according to Benford's law might need further investigation.", confidenceLabel),
                                    "last" = gettextf("Benford's law states that in many naturally occurring collections of numerical observations, the leading significant digit is likely to be small. The goal of this procedure is to determine to what extent the last digits in the data set follow Benford's law, and to test this relation with <b>%1$s</b> confidence. Data that are not distributed according to Benford's law might need further investigation.", confidenceLabel))
    
    } else if (options[["distribution"]] == "uniform"){

      procedureText <- base::switch(options[["digits"]],
                                    "first" = gettextf("The uniform distribution assigns equal probability to all values that may occur. The goal of this procedure is to determine to what extent the first leading digits in the data set follow the uniform distribution, and to test this relation with <b>%1$s</b> confidence. Supposedly random data that do are not uniformly distributed might need further investigation.", confidenceLabel),
                                    "firstSecond" = gettextf("The uniform distribution assigns equal probability to all values that may occur. The goal of this procedure is to determine to what extent the first two leading digits in the data set follow the uniform distribution, and to test this relation with <b>%1$s</b> confidence. Supposedly random data that do are not uniformly distributed might need further investigation.", confidenceLabel),
                                    "last" = gettextf("The uniform distribution assigns equal probability to all values that may occur. The goal of this procedure is to determine to what extent the last digits in the data set follow the uniform distribution, and to test this relation with <b>%1$s</b> confidence. Supposedly random data that do are not uniformly distributed might need further investigation.", confidenceLabel))

    }
    procedureContainer[["procedureParagraph"]] <- createJaspHtml(procedureText, "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer$dependOn(options = c("explanatoryText", 
                                            "confidence",
                                            "digits",
                                            "distribution"))

    jaspResults[["procedureContainer"]] <- procedureContainer
  }
}

.auditClassicalBenfordsLawErrorCheck <- function(dataset, 
                                                 options){
  
  values <- NULL
  if(options[["values"]] != "")
    values <- c(values, options[["values"]])
    
  .hasErrors(dataset, 
              type=c("infinity", "variance", "observations"),
              all.target = values, 
              message = "short", 
              observations.amount= "< 2",
              exitAnalysisIfErrors = TRUE)
}

.auditClassicalBenfordsLawReady <- function(options){

    ready <- options[["values"]] != "" 
    return(ready)

}

.auditBenfordsLawGetContainer <- function(options,
                                          jaspResults, 
                                          position){

  containerTitle <- base::switch(options[["distribution"]],
                                  "benford" = gettext("<u>Assessing Benford's Law</u>"),
                                  "uniform" = gettext("<u>Assessing the Uniform Distribution</u>"))                                       
  benfordsLawContainer <- createJaspContainer(title = containerTitle)
  benfordsLawContainer$position <- position
  benfordsLawContainer$dependOn(options = c("values",
                                            "confidence",
                                            "digits",
                                            "distribution"))

  jaspResults[["benfordsLawContainer"]] <- benfordsLawContainer

  return(benfordsLawContainer)
}

.auditClassicalBenfordsLawGetResults <- function(dataset, 
                                                 options, 
                                                 benfordsLawContainer,
                                                 ready){

  if(!is.null(benfordsLawContainer[["result"]])){

    return(benfordsLawContainer[["result"]]$object)

  } else if(ready){

    totalObs <- nrow(dataset)
    obs <- dataset[[.v(options[["values"]])]]

    if(options[["digits"]] == "first"){

      leadingDigits <- table(as.numeric(substring(format(abs(obs), scientific = TRUE), 1, 1)))
      digits <- 1:9

    } else if(options[["digits"]] == "firstSecond"){

      leadingDigits <- table(as.numeric(substring(format(abs(obs), scientific = TRUE), 1, 3)) * 10)
      digits <- 10:99

    } else if(options[["digits"]] == "last"){

      stringedObs <- as.character(abs(obs))
      leadingDigits <- table(as.numeric(substring(stringedObs, nchar(stringedObs), nchar(stringedObs))))
      digits <- 1:9

    }

    counts <- rep(0, length(digits))
    percentages <- rep(0, length(digits))

    includedNumbers <- as.numeric(names(leadingDigits))

    if(options[["digits"]] == "first" || options[["digits"]] == "last"){
      counts[includedNumbers] <- as.numeric(leadingDigits)
    } else if(options[["digits"]] == "firstSecond"){
      counts[includedNumbers - 9] <- as.numeric(leadingDigits)
    }
    
    percentages <- counts / totalObs
    percentagesLabel <- paste0(round(percentages * 100, 2), "%")

    if(options[["distribution"]] == "benford"){
      inBenford <- log10(1 + 1 / digits) # Benfords law: log_10(1 + 1 / d)
      inBenfordLabel <- paste0(round(inBenford * 100, 2), "%")
    } else if(options[["distribution"]] == "uniform"){
      inBenford <- rep(1 / length(digits), length(digits))
      inBenfordLabel <- paste0(round(inBenford * 100, 2), "%")
    }

    observed <- totalObs * percentages
    expected <- totalObs * inBenford
    chiSquare <- sum( (observed - expected)^2 / expected )
    df <- length(digits) - 1
    pvalue <- pchisq(q = chiSquare, df = df, lower.tail = FALSE)

    result <- list(digits = digits,
                  counts = counts, 
                  percentages = percentages,
                  inBenford = inBenford,
                  N = totalObs,
                  observed = observed,
                  expected = expected,
                  chiSquare = chiSquare,
                  df = df,
                  pvalue = pvalue)

    benfordsLawContainer[["result"]] <- createJaspState(result)
    benfordsLawContainer[["result"]]$dependOn(options = c("values", 
                                                          "confidence",
                                                          "digits",
                                                          "distribution"))
    return(benfordsLawContainer[["result"]]$object)

  } else {
    return(list())
  }
}

.auditClassicalBenfordsLawTestTable <- function(dataset, 
                                                options, 
                                                benfordsLawContainer, 
                                                jaspResults,
                                                ready, 
                                                positionInContainer){
 
  .updateTabNumber(jaspResults)

  if(!is.null(benfordsLawContainer[["benfordsLawTestTable"]])) 
    return()

  tableTitle <- gettextf("<b>Table %i.</b> Goodness-of-fit Test", 
                          jaspResults[["tabNumber"]]$object)

  benfordsLawTestTable <- createJaspTable(tableTitle)
  benfordsLawTestTable$position <- positionInContainer

  benfordsLawTestTable$addColumnInfo(name = 'test', 
                                     title = gettext(''), 
                                     type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'measure', 
                                     title = gettext('Statistic'), 
                                     type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'value',  
                                     title = gettext('Value'), 
                                     type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'df',  
                                     title = gettext('df'), 
                                     type = 'integer')
  benfordsLawTestTable$addColumnInfo(name = 'pvalue', 
                                     title = gettext("<i>p</i> value"), 
                                     type = 'pvalue')
  benfordsLawTestTable$addColumnInfo(name = 'N', 
                                     title = gettext("N"), 
                                     type = 'integer')

  distribution <- base::switch(options[["distribution"]],
                                "benford" = "Benford's law",
                                "uniform" = "the uniform distribution")

  message <- base::switch(options[["digits"]],
                          "first" = gettextf("The null hypothesis specifies that the first digits (1 - 9) in the data set are distributed according to %1$s.", distribution),
                          "firstSecond" = gettextf("The null hypothesis specifies that the first two digits (10 - 99) in the data set are distributed according to %1$s.", distribution),
                          "last" = gettextf("The null hypothesis specifies that the last digits (1 - 9) in the data set are distributed according to %1$s." ,distribution))
  benfordsLawTestTable$addFootnote(message)

  benfordsLawContainer[["benfordsLawTestTable"]] <- benfordsLawTestTable

  df <- ifelse(options[["digits"]] == "first" || options[["digits"]] == "last", 
              yes = 8,
              no = 89)

  if(!ready){
    
    row <- data.frame(test = gettext("Chi-square"), 
                      measure = gettextf("X%1$s", "\u00B2"), 
                      df = df, 
                      value = ".", 
                      pvalue = ".",
                      N = ".")
    benfordsLawTestTable$addRows(row)
    return()
  }

  state <- .auditClassicalBenfordsLawGetResults(dataset, 
                                                options, 
                                                benfordsLawContainer,
                                                ready)

  row <- data.frame(test = gettext("Chi-square"), 
                    measure = gettextf("X%1$s", "\u00B2"), 
                    df = state[["df"]], 
                    value = round(state[["chiSquare"]], 3), 
                    pvalue = state[["pvalue"]],
                    N = state[["N"]])
  benfordsLawTestTable$addRows(row)
}

.auditClassicalBenfordsLawSummaryTable <- function(dataset, 
                                                   options, 
                                                   benfordsLawContainer, 
                                                   jaspResults,
                                                   ready, 
                                                   positionInContainer){

  if(!options[["summaryTable"]])
    return()

  .updateTabNumber(jaspResults)

  if(is.null(benfordsLawContainer[["benfordsLawTable"]])){

    tableTitle <- gettextf("<b>Table %i.</b> Descriptive Statistics",
                           jaspResults[["tabNumber"]]$object)

    benfordsLawTable <- createJaspTable(tableTitle)
    benfordsLawTable$position <- positionInContainer
    columnTitle <- base::switch(options[["distribution"]], 
                                "benford" = gettext("Benford's law"),
                                "uniform" = gettext("Uniform distribution"))

    benfordsLawTable$dependOn(options = "summaryTable")

    whichDigit <- base::switch(options[["digits"]],
                                "first" = gettext('Leading digit'),
                                "firstSecond" = gettext('Leading digits'),
                                "last" = gettext('Last digit'))
                                
    benfordsLawTable$addColumnInfo(name = 'digit', 
                                  title = whichDigit, 
                                  type = 'integer')
    benfordsLawTable$addColumnInfo(name = 'count', 
                                  title = gettext('Count'), 
                                  type = 'integer')
    benfordsLawTable$addColumnInfo(name = 'percentage',  
                                  title = gettext('Percentage'), 
                                  type = 'string')
    benfordsLawTable$addColumnInfo(name = 'inBenford', 
                                  title = columnTitle, 
                                  type = 'string')

    benfordsLawContainer[["benfordsLawTable"]] <- benfordsLawTable

    if(options[["digits"]] == "first" || options[["digits"]] == "last"){
      digits <- 1:9
    } else {
      digits <- 10:99
    }

    if(!ready){

      inBenford <- base::switch(options[["distribution"]],
                                "benford" = log10(1 + 1 / digits),
                                "uniform" = 1 / length(digits))

      row <- data.frame(digit = digits, 
                        count = rep(".", length(digits)),
                        percentage = rep(".", length(digits)),
                        inBenford = paste0(round(inBenford * 100, 2), "%"))
      benfordsLawTable$addRows(row)
      return()
    } 

    state <- .auditClassicalBenfordsLawGetResults(dataset, 
                                                  options, 
                                                  benfordsLawContainer,
                                                  ready)

    percentagesLabel <- paste0(round(state[["percentages"]] * 100, 2), "%")
    inBenfordLabel <- paste0(round(state[["inBenford"]] * 100, 2), "%")

    row <- data.frame(digit = state[["digits"]], 
                      count = state[["counts"]], 
                      percentage = percentagesLabel, 
                      inBenford = inBenfordLabel)

    benfordsLawTable$addRows(row)
  }
}

.benfordsLawPlot <- function(dataset, 
                             options, 
                             benfordsLawContainer, 
                             jaspResults,
                             ready, 
                             positionInContainer){

  if(!options[["benfordsLawPlot"]])
    return()

  .updateFigNumber(jaspResults)

  if(is.null(benfordsLawContainer[["benfordsLawPlot"]])){

    benfordsLawPlot <- createJaspPlot(plot = NULL, 
                                      title = gettext("Observed Percentages vs. Expected Percentages"), 
                                      width = 600, 
                                      height = 400)

    benfordsLawPlot$position <- positionInContainer
    benfordsLawPlot$dependOn(options = c("benfordsLawPlot"))

    benfordsLawContainer[["benfordsLawPlot"]] <- benfordsLawPlot

    if(!ready) 
      return()

    if(options[["digits"]] == "first" || options[["digits"]] == "last"){
      pointSize     <- 5
      lineSize      <- 1.5
    } else if(options[["digits"]] == "firstSecond"){
      pointSize     <- 2
      lineSize      <- 1.2
    }

    state <- .auditClassicalBenfordsLawGetResults(dataset, 
                                                  options, 
                                                  benfordsLawContainer,
                                                  ready)

    legendName <- base::switch(options[["distribution"]], "benford" = gettext("Benford's law"), "uniform" = gettext("Uniform distribution"))

    d <- data.frame(x = c(state[["digits"]], state[["digits"]]),
                    y = c(state[["percentages"]], state[["inBenford"]]),
                    type = c(rep(gettext("Observed"), length(state[["digits"]])), 
                            rep(legendName, length(state[["digits"]]))))

    yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, d$y), min.n = 4)

    if(options[["digits"]] == "first" || options[["digits"]] == "last"){
      xBreaks <- state[["digits"]]
      xLabels <- state[["digits"]]
    } else {
      xBreaks <- state[["digits"]]
      xLabels <- c(10, rep("", 9), 
                  20, rep("", 9), 
                  30, rep("", 9),
                  40, rep("", 9),
                  50, rep("", 9),
                  60, rep("", 9),
                  70, rep("", 9),
                  80, rep("", 9),
                  90, rep("", 8),
                  99)
    }

    axisName <- base::switch(options[["digits"]],
                              "first" = gettext("Leading digit"),
                              "firstSecond" = gettext("Leading digits"),
                              "last" = gettext("Last digit"))

    plotData <- data.frame(x = c(0, 0), y = c(0, 1), type = c(gettext("Observed"), legendName))
    plotData$type <- base::switch(options[["distribution"]],
                                    "benford" = factor(x = plotData$type, levels = levels(plotData$type)[c(1,2)]),
                                    "uniform" = factor(x = plotData$type, levels = levels(plotData$type)[c(2,1)]))

    p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(alpha = 0) +
          ggplot2::geom_bar(data = subset(d,d$type == legendName), 
                            mapping = ggplot2::aes(x = x, y = y), 
                            fill = "darkgray", 
                            stat = "identity", 
                            color = "black",
                            size = 1.2) +
          JASPgraphs::geom_line(data = subset(d,d$type == gettext('Observed')), 
                                mapping = ggplot2::aes(x = x, y = y), 
                                color = "dodgerblue", 
                                size = lineSize) +
          JASPgraphs::geom_point(data = subset(d,d$type == gettext('Observed')), 
                                mapping = ggplot2::aes(x = x, y = y), 
                                fill = "dodgerblue", 
                                size = pointSize, 
                                stroke = 1.5) +
          ggplot2::scale_x_continuous(name = axisName,
                                      breaks = xBreaks, 
                                      labels = xLabels,
                                      limits = c(min(state[["digits"]]) - 0.5, 
                                                max(state[["digits"]]) + 0.5),) +
          ggplot2::scale_y_continuous(name = "",
                                    breaks = yBreaks, 
                                    labels = paste0(round(yBreaks * 100, 2), "%"),
                                    limits = c(0, max(yBreaks))) +
          ggplot2::labs(fill = "") +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -5, r = 50)),
                         panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", size = 0.5)) +
          ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE,
                                                      override.aes = list(size = c(7, 10), 
                                                                          shape = c(21, 22), 
                                                                          fill = c("dodgerblue", "darkgray"), 
                                                                          stroke = 2, 
                                                                          color = "black",
                                                                          alpha = 1)))

    p <- JASPgraphs::themeJasp(p, legend.position = "top")

    benfordsLawPlot$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    distribution <- base::switch(options[["distribution"]], "benford" = "Benford's law", "uniform" = "the uniform distribution")
    benfordsLawPlotText <- createJaspHtml(gettextf("<b>Figure %i:</b> The observed percentages of each digit in the data set compared to the expected percentage under %2$s. For data sets distributed according %2$s the blue dots will lie near the top of the grey bars.", jaspResults[["figNumber"]]$object, distribution), "p")
    
    benfordsLawPlotText$position <- positionInContainer + 1
    benfordsLawPlotText$dependOn(optionsFromObject = benfordsLawContainer[["benfordsLawPlot"]])
    benfordsLawContainer[["benfordsLawPlotText"]] <- benfordsLawPlotText
  }
}

.auditBenfordsLawConclusionParagraph <- function(options,
                                                 benfordsLawContainer,
                                                 jaspResults,
                                                 ready,
                                                 position){

  if(!is.null(jaspResults[["conclusionContainer"]]) || 
     !ready ||
     !options[["explanatoryText"]])
  return()

  conclusionContainer <- createJaspContainer(title= gettext("<u>Conclusion</u>"))
  conclusionContainer$position <- position
  conclusionContainer$dependOn(options = c("values",
                                           "confidence",
                                           "digits",
                                           "explanatoryText",
                                           "distribution"))

  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

  state <- .auditClassicalBenfordsLawGetResults(dataset, 
                                                options, 
                                                benfordsLawContainer,
                                                ready)

  approve <- state[["pvalue"]] >= (1 - options[["confidence"]])

  conclusion <- ifelse(approve, no = gettext("is rejected"), yes = gettext("is not rejected"))
  
  pvalue <- round(state[["pvalue"]], 4)
  if(pvalue < 0.01)
    pvalue <- "< .01"
  pvalue <- ifelse(approve, no = paste0(pvalue, " < \u03B1"), yes = paste0(pvalue, " >= \u03B1"))

  distribution <- base::switch(options[["distribution"]], "benford" = "Benford's law", "uniform" = "the uniform distribution")

  conclusionText <- base::switch(options[["digits"]],
                                  "first" = gettextf("The <i>p</i> value is %1$s and the null hypothesis that the first digits in the data set are distributed according to %2$s <b>%3$s</b>.", pvalue, distribution, conclusion),
                                  "firstSecond" = gettextf("The <i>p</i> value is %1$s and the null hypothesis that the first two digits in the data set are distributed according to %2$s <b>%3$s</b>.", pvalue, distribution, conclusion),
                                  "last" = gettextf("The <i>p</i> value is %1$s and the null hypothesis that the last digits in the data set are distributed according to %2$s <b>%3$s</b>.", pvalue, distribution, conclusion))

  conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(conclusionText, "p")
  conclusionContainer[["conclusionParagraph"]]$position <- 1
  conclusionContainer$dependOn(options = c("explanatoryText", 
                                          "confidence",
                                          "values",
                                          "digits",
                                          "distribution"))

  jaspResults[["conclusionContainer"]] <- conclusionContainer
}
