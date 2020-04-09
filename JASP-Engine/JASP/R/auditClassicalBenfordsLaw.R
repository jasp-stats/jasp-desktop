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

  benfordsLawContainer <- .auditBenfordsLawGetContainer(jaspResults, 
                                                        position = 2)

  # --- TABLES

  # Create a state to keep track of table numbers
  .auditCreateTableNumber(jaspResults)
  
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

  .auditCreateFigureNumber(jaspResults)
  
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
  
  # --- BADGES

  # Provide the analysis badges
  .auditBadgeSection(options,
                     type = "benfordsLaw",
                     stateContainer = benfordsLawContainer,
                     jaspResults, 
                     ready, 
                     position = 4)

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

    procedureText <- gettextf("BBenford's law states that in many naturally occurring collections of numbers, the leading significant number is likely to be small. The goal of this procedure is to determine to which extent the leading numbers in the population follow Benford's law, and to test this relation with <b>%1$s</b> confidence. Data that do not conform to Benford's law might need further verification.", confidenceLabel)

    procedureContainer[["procedureParagraph"]] <- createJaspHtml(procedureText, "p")
    procedureContainer[["procedureParagraph"]]$position <- 1
    procedureContainer$dependOn(options = c("explanatoryText", 
                                            "confidence"))

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
              observations.amount= "< 10",
              exitAnalysisIfErrors = TRUE)
}

.auditClassicalBenfordsLawReady <- function(options){

    ready <- options[["values"]] != "" 
    return(ready)

}

.auditBenfordsLawGetContainer <- function(jaspResults, 
                                          position){
                                         
  benfordsLawContainer <- createJaspContainer(title= gettext("<u>Assessing Benford's Law</u>"))
  benfordsLawContainer$position <- position
  benfordsLawContainer$dependOn(options = c("values",
                                            "confidence",
                                            "digits"))

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

    }

    counts <- rep(0, length(digits))
    percentages <- rep(0, length(digits))

    includedNumbers <- as.numeric(names(leadingDigits))

    if(options[["digits"]] == "first"){
      counts[includedNumbers] <- as.numeric(leadingDigits)
    } else if(options[["digits"]] == "firstSecond"){
      counts[includedNumbers - 9] <- as.numeric(leadingDigits)
    }
    
    percentages <- counts / totalObs
    percentagesLabel <- paste0(round(percentages * 100, 2), "%")

    inBenford <- log10(1 + 1 / digits) # Benfords law: log_10(1 + 1 / d)
    inBenfordLabel <- paste0(round(inBenford * 100, 2), "%")

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
                                                          "confidence"))
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

  digits <- ifelse(options[["digits"]] == "first",
                   yes = gettext(" (1 - 9) "),
                   no = gettext(" (10 - 99) "))

  message <- gettextf("The null hypothesis specifies that the distribution of first numbers %1$s in the population conforms to Benford’s law.", digits)
  benfordsLawTestTable$addFootnote(message)

  benfordsLawContainer[["benfordsLawTestTable"]] <- benfordsLawTestTable

  df <- ifelse(options[["digits"]] == "first", 
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

    benfordsLawTable$dependOn(options = "summaryTable")

    benfordsLawTable$addColumnInfo(name = 'digit', 
                                  title = gettext('Leading digit'), 
                                  type = 'integer')
    benfordsLawTable$addColumnInfo(name = 'count', 
                                  title = gettext('Count'), 
                                  type = 'integer')
    benfordsLawTable$addColumnInfo(name = 'percentage',  
                                  title = gettext('Percentage'), 
                                  type = 'string')
    benfordsLawTable$addColumnInfo(name = 'inBenford', 
                                  title = gettext("Benford's law"), 
                                  type = 'string')

    benfordsLawContainer[["benfordsLawTable"]] <- benfordsLawTable

    if(options[["digits"]] == "first"){
      digits <- 1:9
    } else {
      digits <- 10:99
    }

    if(!ready){

      row <- data.frame(digit = digits, 
                        count = rep(".", length(digits)),
                        percentage = rep(".", length(digits)),
                        inBenford = paste0(round(log10(1 + 1 / digits) * 100, 2), 
                                            "%"))
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
                                      title = gettext("Observed Percentages vs. Benford's Law"), 
                                      width = 600, 
                                      height = 400)

    benfordsLawPlot$position <- positionInContainer
    benfordsLawPlot$dependOn(options = c("benfordsLawPlot"))

    benfordsLawContainer[["benfordsLawPlot"]] <- benfordsLawPlot

    if(!ready) 
      return()

    if(options[["digits"]] == "first"){
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

    d <- data.frame(x = c(state[["digits"]], state[["digits"]]),
                    y = c(state[["percentages"]], state[["inBenford"]]),
                    type = c(rep(gettext("Observed"), length(state[["digits"]])), 
                            rep(gettext("Benford's law"), length(state[["digits"]]))))

    yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

    if(options[["digits"]] == "first"){
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

    p <- ggplot2::ggplot(data = data.frame(x = c(0,0), y = c(0,1), type = c(gettext("Observed"), gettext("Benford's law"))), 
                        mapping = ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(alpha = 0) +
          ggplot2::geom_bar(data = subset(d,d$type == gettext("Benford's law")), 
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
          ggplot2::scale_x_continuous(name = gettext("Leading digit"),
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

    benfordsLawPlotText <- createJaspHtml(gettextf("<b>Figure %i:</b> The observed percentages of each leading number in the population compared to the expected percentageunder Benford's law. The more the blue dots lie near the top of the grey bars, the more the population conforms to Benford's law.", jaspResults[["figNumber"]]$object), "p")
    
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
                                           "explanatoryText"))

  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

  state <- .auditClassicalBenfordsLawGetResults(dataset, 
                                                options, 
                                                benfordsLawContainer,
                                                ready)

  approve <- state[["pvalue"]] >= (1 - options[["confidence"]])

  conclusion <- ifelse(approve, no = gettext("can be rejected"), yes = gettext("can not be rejected"))
  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")
  
  pvalue <- round(state[["pvalue"]], 4)
  if(pvalue < 0.01)
    pvalue <- "< .01"

  conclusionText <- gettextf("The <i>p</i> value is determined to be %1$s. Therefore, the null hypothesis that the distribution of first numbers in the population conforms to Benford's law <b>%2$s</b> with <b>%3$s</b> confidence.", pvalue, conclusion, confidenceLabel)

  conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(conclusionText, "p")
  conclusionContainer[["conclusionParagraph"]]$position <- 1
  conclusionContainer$dependOn(options = c("explanatoryText", 
                                          "confidence",
                                          "values",
                                          "digits"))

  jaspResults[["conclusionContainer"]] <- conclusionContainer
}
