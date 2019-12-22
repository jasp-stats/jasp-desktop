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

classicalBenfordsLaw <- function(jaspResults, dataset, options, ...){

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
  
  # Create the goodness-of-fit table
  .auditClassicalBenfordsLawTestTable(dataset, 
                                    options, 
                                    benfordsLawContainer, 
                                    ready, 
                                    positionInContainer = 1)

  # Create the observed and predicted probabilities table                                  
  .auditClassicalBenfordsLawSummaryTable(dataset, 
                                         options, 
                                         benfordsLawContainer, 
                                         ready, 
                                         positionInContainer = 2)

  # ---

  # --- PLOTS
  
  # Create the observed and predicted probabilities plot
  .benfordsLawPlot(dataset, 
                   options, 
                   benfordsLawContainer, 
                   ready, 
                   positionInContainer = 3)

  # ---

  # Create the conclusion paragraph
  .auditBenfordsLawConclusionParagraph(options,
                                       benfordsLawContainer,
                                       jaspResults,
                                       ready,
                                       position = 4)
  
  # --- BADGES

  # Provide the analysis badges
  .auditBadgeSection(options,
                     type = "benfordsLaw",
                     stateContainer = benfordsLawContainer,
                     jaspResults, 
                     ready, 
                     position = 5)

  # ---
}

.auditReadDataBenfordsLaw <- function(dataset, 
                                     options){

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

    procedureContainer <- createJaspContainer(title= "<u>Procedure</u>")
    procedureContainer$position <- position

    confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

    procedureText <- paste0("Benford's law states that in many naturally occurring collections of numbers, the leading significant digit 
                             is likely to be small. The goal of this procedure is to determine to which extend the leading digits in the 
                             population follow Benford's law, and to test this relation with <b>", confidenceLabel, "</b> confidence. Data that do not conform 
                             to Benford's law might need further verification.")

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
              message="short", 
              observations.amount= "< 10",
              exitAnalysisIfErrors = TRUE)
}

.auditClassicalBenfordsLawReady <- function(options){

    ready <- options[["values"]] != "" 
    return(ready)

}

.auditBenfordsLawGetContainer <- function(jaspResults, 
                                          position){
                                         
  benfordsLawContainer <- createJaspContainer(title= "<u>Assessing Benford's Law</u>")
  benfordsLawContainer$position <- position
  benfordsLawContainer$dependOn(options = c("values",
                                            "confidence"))

  jaspResults[["benfordsLawContainer"]] <- benfordsLawContainer

  return(benfordsLawContainer)
}

.auditClassicalBenfordsLawState <- function(dataset, 
                                            options, 
                                            benfordsLawContainer,
                                            ready){

  if(!is.null(benfordsLawContainer[["result"]])){

    return(benfordsLawContainer[["result"]]$object)

  } else if(ready){

    totalObs <- nrow(dataset)
    obs <- dataset[[.v(options[["values"]])]]

    extractFirstNumber <- function(x){
      as.numeric(strsplit(as.character(abs(x)), "")[[1]][1])
    }
    firstDigitofEveryObs <- sapply(obs, extractFirstNumber)

    digits <- 1:9
    counts <- rep(0, length(digits))
    percentages <- rep(0, length(digits))

    includedNumbers <- as.numeric(names(table(firstDigitofEveryObs)))

    counts[includedNumbers] <- as.numeric(table(firstDigitofEveryObs))
    percentages <- counts / totalObs
    percentagesLabel <- paste0(round(percentages * 100, 2), "%")

    inBenford <- log10(1 + 1 / digits)
    inBenfordLabel <- paste0(round(inBenford * 100, 2), "%")

    observed <- totalObs * percentages
    expected <- totalObs * inBenford
    chiSquare <- sum( (observed - expected)^2 / expected )
    df <- 8
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
    benfordsLawContainer[["result"]]$dependOn(options = c("values", "confidence"))
    return(benfordsLawContainer[["result"]]$object)

  } else {
    return(list())
  }
}

.auditClassicalBenfordsLawTestTable <- function(dataset, 
                                                options, 
                                                benfordsLawContainer, 
                                                ready, 
                                                positionInContainer){

  if(!is.null(benfordsLawContainer[["benfordsLawTestTable"]])) 
    return()

  benfordsLawTestTable <- createJaspTable("Benford's Law Goodness-of-fit Table")
  benfordsLawTestTable$position <- positionInContainer

  benfordsLawTestTable$dependOn(options = c("values",
                                             "confidence"))

  benfordsLawTestTable$addColumnInfo(name = 'test', 
                                title = '', 
                                type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'measure', 
                                title = 'Statistic', 
                                type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'value',  
                                title = 'Value', 
                                type = 'string')
  benfordsLawTestTable$addColumnInfo(name = 'df',  
                                title = 'df', 
                                type = 'integer')
  benfordsLawTestTable$addColumnInfo(name = 'pvalue', 
                                title = "<i>p</i> value", 
                                type = 'pvalue')

  message <- "The null hypothesis specifies that the distribution of 
              first digits in the population conforms to Benford’s Law."
  benfordsLawTestTable$addFootnote(message = message, symbol="<i>Note.</i>")

  benfordsLawContainer[["benfordsLawTestTable"]] <- benfordsLawTestTable

  if(!ready){

    row <- data.frame(test = "Chi-squared", 
                      measure = "X\u00B2", 
                      df = 8, 
                      value = ".", 
                      pvalue = ".")
    benfordsLawTestTable$addRows(row)
    return()
  }

  state <- .auditClassicalBenfordsLawState(dataset, 
                                           options, 
                                           benfordsLawContainer,
                                           ready)

  row <- data.frame(test = "Chi-square", 
                    measure = "X\u00B2", 
                    df = state[["df"]], 
                    value = round(state[["chiSquare"]], 3), 
                    pvalue = state[["pvalue"]])
  benfordsLawTestTable$addRows(row)
}

.auditClassicalBenfordsLawSummaryTable <- function(dataset, 
                                                   options, 
                                                   benfordsLawContainer, 
                                                   ready, 
                                                   positionInContainer){

  if(!is.null(benfordsLawContainer[["benfordsLawTable"]]) || 
      !options[["summaryTable"]])
    return()

  benfordsLawTable <- createJaspTable("Descriptive Statistics")
  benfordsLawTable$position <- positionInContainer

  benfordsLawTable$dependOn(options = c("values", 
                                        "summaryTable"))

  benfordsLawTable$addColumnInfo(name = 'digit', 
                                 title = 'Leading digit', 
                                 type = 'integer')
  benfordsLawTable$addColumnInfo(name = 'count', 
                                 title = 'Count', 
                                 type = 'integer')
  benfordsLawTable$addColumnInfo(name = 'percentage',  
                                 title = 'Observed %', 
                                 type = 'string')
  benfordsLawTable$addColumnInfo(name = 'inBenford', 
                                 title = "Benford's law", 
                                 type = 'string')

  benfordsLawContainer[["benfordsLawTable"]] <- benfordsLawTable

  if(!ready){

    row <- data.frame(digit = 1:9, 
                      count = rep(".", 9),
                      percentage = rep(".", 9),
                      inBenford = paste0(round(log10(1 + 1 / 1:9) * 100, 2), "%"))
    benfordsLawTable$addRows(row)
    return()
  } 

  state <- .auditClassicalBenfordsLawState(dataset, 
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

.benfordsLawPlot <- function(dataset, 
                             options, 
                             benfordsLawContainer, 
                             ready, 
                             positionInContainer){

  if(!is.null(benfordsLawContainer[["benfordsLawPlot"]]) || 
     !options[["benfordsLawPlot"]])
    return()

  benfordsLawPlot <- createJaspPlot(plot = NULL, 
                                    title = "Observed Percentages vs. Benford's Law", 
                                    width = 600, 
                                    height = 400)

  benfordsLawPlot$position <- positionInContainer
  benfordsLawPlot$dependOn(options = c("benfordsLawPlot", 
                                       "values"))

  benfordsLawContainer[["benfordsLawPlot"]] <- benfordsLawPlot

  if(!ready) 
    return()

  state <- .auditClassicalBenfordsLawState(dataset, 
                                          options, 
                                          benfordsLawContainer,
                                          ready)

  d <- data.frame(x = c(1:9, 1:9),
                  y = c(state[["percentages"]], state[["inBenford"]]),
                  type = c(rep("Observed", 9), rep("Benford's law", 9)))

  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  p <- ggplot2::ggplot(data = data.frame(x = c(0,0), y = c(0,1), type = c("Observed", "Benford's law")), 
                       mapping = ggplot2::aes(x = x, y = y, fill = type)) +
        ggplot2::geom_point(alpha = 0) +
        ggplot2::geom_bar(data = subset(d,d$type == "Benford's law"), 
                          mapping = ggplot2::aes(x = x, y = y), 
                          fill = "darkgray", 
                          stat = "identity", 
                          color = "black",
                          size = 1.2) +
        JASPgraphs::geom_line(data = subset(d,d$type == 'Observed'), 
                               mapping = ggplot2::aes(x = x, y = y), 
                               color = "dodgerblue", 
                               size = 2) +
        JASPgraphs::geom_point(data = subset(d,d$type == 'Observed'), 
                               mapping = ggplot2::aes(x = x, y = y), 
                               fill = "dodgerblue", 
                               size = 5, 
                               stroke = 1.5) +
        ggplot2::scale_x_continuous(name = "Leading digit",
                                    breaks = 1:9, 
                                    labels = 1:9,
                                    limits = c(0.5, 9.5)) +
        ggplot2::scale_y_continuous(name = "",
                                  breaks = yBreaks, 
                                  labels = paste0(round(yBreaks * 100, 2), "%"),
                                  limits = c(0, max(yBreaks))) +
        ggplot2::labs(fill = "") +
        ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -5, r = 50))) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE,
                                                     override.aes = list(size = c(7, 10), 
                                                                         shape = c(21, 22), 
                                                                         fill = c("dodgerblue", "darkgray"), 
                                                                         stroke = 2, 
                                                                         color = "black",
                                                                         alpha = 1)))

  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  benfordsLawPlot$plotObject <- p

  if(options[["explanatoryText"]]){

    benfordsLawPlotText <- createJaspHtml(paste0("<b>Figure 1:</b> The observed percentages of each leading digit in the population compared to the expected percentage
                                                  under Benford's law. The more the blue dots lie near the top of the grey bars, the more the population conforms to 
                                                  Benford's law."), "p")
    
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

  conclusionContainer <- createJaspContainer(title= "<u>Conclusion</u>")
  conclusionContainer$position <- position
  conclusionContainer$dependOn(options = c("values",
                                           "confidence",
                                           "explanatoryText"))

  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

  state <- .auditClassicalBenfordsLawState(dataset, 
                                           options, 
                                           benfordsLawContainer,
                                           ready)

  approve <- state[["pvalue"]] >= (1 - options[["confidence"]])

  conclusion <- ifelse(approve, no = "can be rejected", yes = "can not be rejected")
  confidenceLabel <- paste0(round(options[["confidence"]] * 100, 2), "%")

  conclusionText <- paste0("The <i>p</i> value is determined to be ", round(state[["pvalue"]], 4), ". Therefore, the null hypothesis that the distribution of first digits in the 
                            population conforms to Benford's law <b>", conclusion, "</b> with <b>", confidenceLabel, "</b> confidence.")

  conclusionContainer[["conclusionParagraph"]] <- createJaspHtml(conclusionText, "p")
  conclusionContainer[["conclusionParagraph"]]$position <- 1
  conclusionContainer$dependOn(options = c("explanatoryText", 
                                          "confidence",
                                          "values"))

  jaspResults[["conclusionContainer"]] <- conclusionContainer
}