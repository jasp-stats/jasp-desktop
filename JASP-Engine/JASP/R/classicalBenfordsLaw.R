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

  # Read in the data 
  dataset <- .auditReadDataBenfordsLaw(dataset, 
                                       options)

  # Perform early error checks
  .auditClassicalBenfordsLawErrorCheck(dataset, 
                                       options)

  # Ready for analysis
  ready <- .auditClassicalBenfordsLawReady(options)

  # Create explanatory text
  # .auditClassicalEstimation(dataset, 
  #                           options, 
  #                           jaspResults, 
  #                           ready, 
  #                           position = 1)

  # Create results table
  .auditClassicalBenfordsLawSummaryTable(dataset, 
                                         options, 
                                         jaspResults, 
                                         ready, 
                                         position = 2)

  # Benfords Law plot
  .benfordsLawPlot(dataset, 
                   options, 
                   jaspResults, 
                   ready, 
                   position = 4)
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

.auditClassicalBenfordsLawSummaryTable <- function(dataset, 
                                                   options, 
                                                   jaspResults, 
                                                   ready, 
                                                   position){

  if(!is.null(jaspResults[["benfordsLawTable"]])) 
    return() #The options for this table didn't change so we don't need to rebuild it

  benfordsLawTable <- createJaspTable("Benford's Law Summary Table")
  benfordsLawTable$position <- position

  benfordsLawTable$dependOn(options = c("values"))

  benfordsLawTable$addColumnInfo(name = 'digit', 
                                 title = 'Leading digit', 
                                 type = 'string')
  benfordsLawTable$addColumnInfo(name = 'count', 
                                 title = 'Count', 
                                 type = 'string')
  benfordsLawTable$addColumnInfo(name = 'percentage',  
                                 title = 'Observed %', 
                                 type = 'string')
  benfordsLawTable$addColumnInfo(name = 'inBenford', 
                                 title = "Benford's law", 
                                 type = 'string')

  jaspResults[["benfordsLawTable"]] <- benfordsLawTable

  if(!ready)
    return()

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

  result <- list(digits = digits,
                 counts = counts, 
                 percentages = percentages,
                 inBenford = inBenford)

  row <- data.frame(digit = digits, 
                    count = counts, 
                    percentage = percentagesLabel, 
                    inBenford = inBenfordLabel)

  benfordsLawTable$addRows(row)

  jaspResults[["result"]] <- createJaspState(result)
}

.benfordsLawPlot <- function(dataset, 
                             options, 
                             jaspResults, 
                             ready, 
                             position){

  if(!is.null(jaspResults[["benfordsLawPlot"]]) || !options[["benfordsLawPlot"]])
    return()

  benfordsLawPlot <- createJaspPlot(plot = NULL, 
                                    title = "Observed and predicted Probabilities", 
                                    width = 500, 
                                    height = 400)

  benfordsLawPlot$position <- position
  benfordsLawPlot$dependOn(options = c("benfordsLawPlot", 
                                       "values"))

  jaspResults[["benfordsLawPlot"]] <- benfordsLawPlot

  if(!ready) 
    return()

  state <- jaspResults[["result"]]$object

  d <- data.frame(x = c(1:9, 1:9),
                  y = c(state[["percentages"]], state[["inBenford"]]),
                  type = c(rep("Observed", 9), rep("In Benford's Law", 9)))

  yBreaks <- JASPgraphs::getPrettyAxisBreaks(d$y, min.n = 4)

  p <- ggplot2::ggplot(data = data.frame(x = c(0,0), y = c(0,1), type = c("Observed", "In Benford's Law")), 
                       mapping = ggplot2::aes(x = x, y = y, fill = type)) +
        ggplot2::geom_point(alpha = 0) +
        ggplot2::geom_bar(data = subset(d,d$type == "In Benford's Law"), 
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
        ggplot2::scale_x_continuous(name = "Digit",
                                    breaks = 1:9, 
                                    labels = 1:9,
                                    limits = c(0.5, 9.5)) +
        ggplot2::scale_y_continuous(name = "Probability",
                                  breaks = yBreaks, 
                                  labels = yBreaks,
                                  limits = c(0, max(yBreaks))) +
        ggplot2::labs(fill = "") +
        ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -5, r = 50))) +
        ggplot2::guides(fill = ggplot2::guide_legend(
                                                  override.aes = list(size = c(10, 8), 
                                                                      shape = c(22, 21), 
                                                                      fill = c("darkgray", "dodgerblue"), 
                                                                      stroke = 2, 
                                                                      color = "black",
                                                                      alpha = 1)))

  p <- JASPgraphs::themeJasp(p, legend.position = "top")

  benfordsLawPlot$plotObject <- p
}