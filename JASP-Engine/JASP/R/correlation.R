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

Correlation <- function(dataset=NULL, options, perform="run", callback=function(...) 0, state=NULL, ...) {
  #
  stateKey <- list(
    correlationPlot = c("plotCorrelationMatrix", "plotDensities", "plotStatistics",
                        "variables", "pearson", "kendallsTauB", "spearman",
                        "confidenceIntervals", "confidenceIntervalsInterval", "hypothesis", "missingValues"),
    tableData = c("hypothesis", "missingValues", "confidenceIntervalsInterval")
  )

  if (is.null(dataset)) {
    if (perform == "run") {
      if (options$missingValues == "excludeListwise") {
        dataset <- .readDataSetToEnd(columns.as.numeric=options$variables, exclude.na.listwise=options$variables)
      } else {
        dataset <- .readDataSetToEnd(columns.as.numeric=options$variables)
      }
    } else {
      dataset <- .readDataSetHeader(columns.as.numeric=options$variables)
    }
  }

  correlation.plot <- state$correlationPlot
  stateTableData <- state$tableData

  if (options$plotCorrelationMatrix && is.null(correlation.plot)) {
    correlation.plot <- .plotCorrelations(dataset, perform, options)
  }

  tableData <- .calculateCorrelations(dataset, perform, options, stateTableData,
                                      variables=options$variables, pearson=options$pearson,
                                      kendallsTauB=options$kendallsTauB, spearman=options$spearman,
                                      hypothesis=options$hypothesis, CI=options$confidenceIntervalsInterval)

  correlation.table <- .fillCorrelationTable(tableData, displayPairwise=options$displayPairwise,
                                             reportSignificance=options$reportSignificance,
                                             reportCI=options$confidenceIntervals,
                                             flagSignificant=options$flagSignificant, reportVovkSellkeMPR=options$VovkSellkeMPR)

  results <- list()

  meta <- list(
    list(name="title", type="title"),
    list(name="correlations", type="table"),
    list(name="plot", type="image")
  )

  results[[".meta"]] <- meta

  results[["title"]] <- "Correlation Matrix"
  results[["plot"]] <- correlation.plot
  results[["correlations"]] <- correlation.table

  keep <- NULL

  if (length(correlation.plot) > 0) {
    keep <- correlation.plot$data
  }

  if (perform == "init") {
    if (length(options$variables) < 2) {
      return(list(results=results, status="complete", keep=keep))
    } else {
      return(list(results=results, status="inited", state=state, keep=keep))
    }
  } else { # run
    state <- list(options=options, correlationPlot=correlation.plot, tableData=tableData)
    attr(state, "key") <- stateKey

    return(list(results=results, status="complete", state=state, keep=keep))
  }
}

.calculateCorrelations <- function(dataset, perform, options, stateTableData, variables=c(), pearson=TRUE, kendallsTauB=FALSE,
  spearman=FALSE, hypothesis="correlated", CI=0.95) {
  #
  ready <- FALSE
  variables <- unlist(variables)

  if (length(variables) == 0) {
    variables <- c(variables, "...", "... ") # we need this trailing space so 1 != 2
  } else if (length(variables) == 1) {
    variables <- c(variables, "...")
  } else {
    ready <- TRUE
  }

  tests <- c()
  if (pearson)
    tests <- c(tests, "pearson")
  if (spearman)
    tests <- c(tests, "spearman")
  if (kendallsTauB)
    tests <- c(tests, "kendall")

  results <- list(tests=tests, variables=variables, hypothesis=hypothesis, CI=CI)

  if (length(tests) == 0) {
    return(results)
  }

  pairs <- combn(variables, 2, simplify=FALSE)

  for (i in seq_along(pairs)) {
    #
    var1 <- pairs[[i]][1]
    var2 <- pairs[[i]][2]
    pairName <- paste(sort(c(var1, var2)), collapse="-")

    for (test in tests) {
      errorMessage <- NULL
      estimate <- p.value <- MPR <- upperCI <- lowerCI <- "."

      if (! is.null(stateTableData) && ! is.null(stateTableData[["result"]][[pairName]][[test]])) {
        # If state exists, then fill up table
        #
        resultsPair <- stateTableData[["result"]][[pairName]][[test]]
        estimate <- resultsPair$estimate
        p.value <- resultsPair$p.value
        MPR <- resultsPair$MPR
        upperCI <- resultsPair$upperCI
        lowerCI <- resultsPair$lowerCI
        errorMessage <- resultsPair$errorMessage
      } else if (perform == "run" && ready) {
        # here do calculations
        #
        errors <- .hasErrors(dataset, perform = perform, message = 'short', type = c('variance', 'infinity', 'observations'),
                             all.target = c(var1, var2), observations.amount = "< 3")

        if (! identical(errors, FALSE)) {
          estimate <- p.value <- MPR <- upperCI <- lowerCI <- "NaN"
          errorMessage <- errors$message
        } else {
          obs1 <- dataset[[ .v(var1) ]]
          obs2 <- dataset[[ .v(var2) ]]

          if (hypothesis == "correlated") {
            result <- cor.test(obs1, obs2, method = test, alternative = "two.sided", conf.level = CI)
          } else if (hypothesis == "correlatedPositively") {
            result <- cor.test(obs1, obs2, method = test, alternative = "greater", conf.level = CI)
          } else {
            result <- cor.test(obs1, obs2, method = test, alternative = "less", conf.level = CI)
          }

          estimate <- as.numeric(result$estimate)
          p.value  <- as.numeric(result$p.value)
          MPR <- .VovkSellkeMPR(p.value)

          if (test == "pearson") {
            upperCI <- as.numeric(result$conf.int[2])
            lowerCI <- as.numeric(result$conf.int[1])
          } else if (test == "spearman") {
            spearCI <- .createNonparametricConfidenceIntervals(obs1, obs2, obsCor = estimate, method = "spearman", hypothesis = hypothesis, confLevel = CI)

            upperCI <- as.numeric(spearCI[2])
            lowerCI <- as.numeric(spearCI[1])
          } else if (test == "kendall") {
            kendallCI <- .createNonparametricConfidenceIntervals(obs1, obs2, obsCor = estimate, method = "kendall", hypothesis = hypothesis, confLevel = CI)

            upperCI <- as.numeric(kendallCI[2])
            lowerCI <- as.numeric(kendallCI[1])
          }
        }
      }

      results[["result"]][[pairName]][[test]] <- list(
          estimate = .clean(estimate),
          p.value = .clean(p.value),
          MPR = .clean(MPR),
          upperCI = .clean(upperCI),
          lowerCI = .clean(lowerCI),
          errorMessage = errorMessage
      )
    }
  }
  return(results)
}

.fillCorrelationTable <- function(tableData, displayPairwise=FALSE, reportSignificance=FALSE,
  reportCI=FALSE, reportVovkSellkeMPR=FALSE, flagSignificant=FALSE) {

  correlation.table <- list()

  tests <- tableData$tests
  if (length(tests) != 1) {
    correlation.table[["title"]] <- "Correlation Table"
  } else if (tests == "pearson") {
    correlation.table[["title"]] <- "Pearson Correlations"
  } else if (tests == "spearman") {
    correlation.table[["title"]] <- "Spearman Correlations"
  } else if (tests == "kendall") {
    correlation.table[["title"]] <- "Kendall's Tau Correlations"
  } else {
    correlation.table[["title"]] <- "Correlation Table"
  }

  hypothesis <- tableData$hypothesis
  footnotes <- .newFootnotes()
  if (flagSignificant || reportSignificance) {
    if (hypothesis == "correlatedPositively") {
      .addFootnote(footnotes, "all tests one-tailed, for positive correlation", symbol="<i>Note</i>.")
    } else if (hypothesis == "correlatedNegatively") {
      .addFootnote(footnotes, "all tests one-tailed, for negative correlation", symbol="<i>Note</i>.")
    }
  }

  if (flagSignificant) {
    if (hypothesis == "correlated") {
      .addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001", symbol="*")
    } else {
      .addFootnote(footnotes, "p < .05, ** p < .01, *** p < .001, one-tailed", symbol="*")
    }
  }

  if (reportVovkSellkeMPR) {
    if (flagSignificant) {
      .addFootnote(footnotes, symbol = "\u207A", text = "Vovk-Sellke Maximum
      <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
      possible odds in favor of H\u2081 over H\u2080 equals
      1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
      (Sellke, Bayarri, & Berger, 2001).")
    } else {
      .addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
      <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum
      possible odds in favor of H\u2081 over H\u2080 equals
      1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
      (Sellke, Bayarri, & Berger, 2001).")
    }
  }

  variables <- tableData$variables
  CI <- tableData$CI
  fields <- list()
  test.names <- list(pearson="Pearson's r", spearman="Spearman's rho", kendall="Kendall's tau B")
  overTitles <- list(pearson="Pearson", spearman="Spearman", kendall="Kendall")

  if (displayPairwise) {
    fields[[1]] <- list(name=".variable1", title="", type="string")
    fields[[2]] <- list(name=".separator", title="", type="separator")
    fields[[3]] <- list(name=".variable2", title="", type="string")

    for (test in tests) {
      overTitle <- NULL

      if (length(tests) > 1) {
        test.names <- list(pearson="r", spearman="rho", kendall="tau B")
        overTitle <- overTitles[[test]]
      }

      fields[[length(fields)+1]] <- list(name=test, title=test.names[[test]], overTitle=overTitle, type="number", format="sf:4;dp:3")

      if (reportSignificance) {
        fields[[length(fields)+1]] <- list(name=paste0(test, "-p"), title="p", overTitle=overTitle, type="number", format="dp:3;p:.001")
      }

      if (reportVovkSellkeMPR) {
        if (flagSignificant){
          title <- "VS-MPR\u207A"
        } else {
          title <- "VS-MPR\u002A"
        }
        fields[[length(fields)+1]] <- list(name=paste0(test, "-MPR"), title=title, overTitle=overTitle, type="number", format="sf:4;dp:3")
      }

      if (reportCI) {
        fields[[length(fields)+1]] <- list(name=paste0(test, "-lowerCI"), title=paste0("Lower ", 100 * CI, "% CI"), overTitle=overTitle, type="number", format="sf:4;dp:3")
        fields[[length(fields)+1]] <- list(name=paste0(test, "-upperCI"), title=paste0("Upper ", 100 * CI, "% CI"), overTitle=overTitle, type="number", format="sf:4;dp:3")
      }

    }
  } else {
    fields[[1]] <- list(name=".variable", title="", type="string")

    for (test in tests) {
      if (length(tests) > 1 || (length(tests) == 1 && (reportSignificance || reportCI || reportVovkSellkeMPR))) {
        fields[[length(fields)+1]] <- list(name=paste0(".test[", test, "]"), title="", type="string")
      }

      for (variable.name in variables) {
        fields[[length(fields)+1]] <- list(name=paste0(variable.name, "[", test, "]"), title=variable.name, type="number", format="dp:3")
      }

      if (reportSignificance) {
        fields[[length(fields)+1]] <- list(name=paste0(".test[", test, "-p]"), title="", type="string")

        for (variable.name in variables) {
          fields[[length(fields)+1]] <- list(name=paste0(variable.name, "[", test, "-p]"), title=variable.name, type="number", format="dp:3;p:.001")
        }
      }

      if (reportVovkSellkeMPR){
        fields[[length(fields)+1]] <- list(name=paste0(".test[", test, "-MPR]"), title="", type="string")

        for (variable.name in variables) {
          fields[[length(fields)+1]] <- list(name=paste0(variable.name, "[", test, "-MPR]"), title=variable.name, type="number", format="sf:4;dp:3")
        }
      }

      if (reportCI) {
        fields[[length(fields)+1]] <- list(name=paste0(".test[", test, "-upperCI]"), title="", type="string")
        fields[[length(fields)+1]] <- list(name=paste0(".test[", test, "-lowerCI]"), title="", type="string")

        for (variable.name in variables) {
          fields[[length(fields)+1]] <- list(name=paste0(variable.name, "[", test, "-upperCI]"), title=variable.name, type="number", format="dp:3")
          fields[[length(fields)+1]] <- list(name=paste0(variable.name, "[", test, "-lowerCI]"), title=variable.name, type="number", format="dp:3")
        }
      }
    }
  }

  correlation.table[["schema"]] <- list(fields=fields)

  template <- list()

  if (displayPairwise) {
    pairs <- combn(variables, 2, simplify=FALSE)
    template <- lapply(pairs, list)
  } else {
    for (i in seq_along(variables)) {
      row.var <- variables[i]
      template[[row.var]] <- list()
      for (j in seq_along(variables)) {
        col.var <- variables[j]
        if (i < j) { # above diagonal
          template[[row.var]][[col.var]] <- ""
        } else if (i == j) { # on diagonal
          template[[row.var]][[col.var]] <- "-"
        } else { # below diagonal
          template[[row.var]][[col.var]] <- c(row.var, col.var)
        }
      }
    }
  }

  rows <- list()

  for (i in seq_along(template)) { # loop over row variables
    row <- list()
    row.footnotes <- list()
    row.pairs <- template[[i]]

    for (j in seq_along(row.pairs)) { # loop over pairs containing that row var
      pair <- row.pairs[[j]]

      for (test in tests) {
        col.est <- test
        col.p <- paste0(test, "-p")
        col.mpr <- paste0(test, "-MPR")
        col.upperCI <- paste0(test, "-upperCI")
        col.lowerCI <- paste0(test, "-lowerCI")

        if ( ! displayPairwise) {
          col.var <- variables[j]
          col.est <- paste0(col.var, "[", col.est, "]") # contNormal[pearson]
          col.p <- paste0(col.var, "[", col.p, "]") # contNormal[pearson-p]
          col.mpr <- paste0(col.var, "[", col.mpr, "]") # contNormal[pearson-MPR]
          col.upperCI <- paste0(col.var, "[", col.upperCI, "]") # contNormal[pearson-upperCI]
          col.lowerCI <- paste0(col.var, "[", col.lowerCI, "]") # contNormal[pearson-lowerCI]

          if (length(tests) > 1 || (length(tests) == 1 && (reportSignificance || reportCI || reportVovkSellkeMPR))) {
            row[[paste0(".test[", test, "]")]] <- test.names[[test]]
          }

          if (reportSignificance)
            row[[paste0(".test", "[", test, "-p]")]] <- "p-value"

          if (reportVovkSellkeMPR){
            if (flagSignificant){
              row[[paste0(".test", "[", test, "-MPR]")]] <- "VS-MPR\u207A"
            } else {
              row[[paste0(".test", "[", test, "-MPR]")]] <- "VS-MPR\u002A"
            }
          }

          if (reportCI) {
            row[[paste0(".test", "[", test, "-upperCI]")]] <- paste("Upper ", 100 * CI, "% CI", sep="")
            row[[paste0(".test", "[", test, "-lowerCI]")]] <- paste("Lower ", 100 * CI, "% CI", sep="")
          }

          if (identical(pair, "-")) { # auto correlation
            row[[col.est]] <- row[[col.p]] <- row[[col.mpr]] <- row[[col.upperCI]] <- row[[col.lowerCI]] <- "\u2014" # em-dash
            next
          } else if (identical(pair, "")) { # above the diagonal
            row[[col.est]] <- row[[col.p]] <- row[[col.mpr]] <- row[[col.upperCI]] <- row[[col.lowerCI]] <- ""
            next
          }

        }

        pairName <- paste(sort(pair), collapse="-")
        result <- tableData$result[[pairName]][[test]]

        if (flagSignificant && is.numeric(result$p.value)) {
          if (result$p.value < .001) {
            row.footnotes[[col.est]] <- list("***")
          } else if (result$p.value < .01) {
            row.footnotes[[col.est]] <- list("**")
          } else if (result$p.value < .05) {
            row.footnotes[[col.est]] <- list("*")
          }
        }

        row[[col.est]] <- result$estimate

        if (reportSignificance)
          row[[col.p]] <- result$p.value

        if (reportVovkSellkeMPR)
          row[[col.mpr]] <- result$MPR

        if (reportCI) {
          row[[col.upperCI]] <- result$upperCI
          row[[col.lowerCI]] <- result$lowerCI
        }

        if (! is.null(result$errorMessage)) {
          index <- .addFootnote(footnotes, result$errorMessage)
          row.footnotes[[col.est]] <- c(row.footnotes[[col.est]], list(index))
        }

      } # end loop over columns
    } # end loop over tests

    if (displayPairwise) {
      row[[".variable1"]] <- pair[1]
      row[[".separator"]] <- "-"
      row[[".variable2"]] <- pair[2]
    } else {
      row[[".variable"]] <- variables[i]
    }

    if (length(row.footnotes) > 0)
      row[[".footnotes"]] <- row.footnotes

    rows[[i]] <- row
  } # end loop over rows

  correlation.table[["data"]] <- rows
  correlation.table[["footnotes"]] <- as.list(footnotes)
  return(correlation.table)
}

.plotCorrelations <- function(dataset, perform, options) {
  variables <- unlist(options$variables)
  correlation.plot <- NULL

   if (perform == "init" && length(variables) > 1) {
    l <- length(variables)

    if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
      width <- 580
      height <- 580
    } else if (l <= 2) {
      width <- 400
      height <- 400
    } else {
      width <- 250 * l + 20
      height <- 250 * l + 20
    }

    plot <- list()

    plot[["title"]] <- "Correlation Plot"
    plot[["width"]]  <- width
    plot[["height"]] <- height

    correlation.plot <- plot
  } else if (perform == "run" && length(variables) > 1) {
    variable.statuses <- vector("list", length(variables))

    for (i in seq_along(variables)) {
      variable.statuses[[i]]$unplotable <- FALSE
      variable.statuses[[i]]$plottingError <- NULL

      errors <- .hasErrors(dataset, perform, type=c("infinity", "variance", "observations"),
                           all.target=variables[i], message="short", observations.amount="< 3")
      if (! identical(errors, FALSE)) {
        variable.statuses[[i]]$unplotable <- TRUE
        variable.statuses[[i]]$plottingError <- errors$message
      }
    }

    variables <- .v(variables)
    l <- length(variables)

    if (l <= 2 && (options$plotDensities || options$plotStatistics)) {
      width <- 580
      height <- 580
    } else if (l <= 2) {
      width <- 400
      height <- 400
    } else {
      width <- 250 * l
      height <- 250 * l
    }

    plot <- list()

    plot[["title"]] <- "Correlation Plot"
    plot[["width"]]  <- width
    plot[["height"]] <- height

    correlation.plot <- plot
    cexText <- 1.6

    .plotFunc <- function() {

        plotList <- list()

            for (row in seq_len(l)) {
                for (col in seq_len(l)) {
                    if (row == col) {
                        if (options$plotDensities) {
                            if ( ! variable.statuses[[row]]$unplotable) {
                                plotList[[length(plotList)+1]] <- .plotMarginalCor(dataset[[variables[row]]]) # plot marginal (histogram with density estimator)
                            } else {
                                plotList[[length(plotList)+1]] <- .displayError(variable.statuses[[row]]$plottingError, cexText=cexText)
                            }
                        } else {
                            
                            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE)
                            p <- p + ggplot2::xlab("")
                            p <- p + ggplot2::ylab("")
                            p <- JASPgraphs::themeJasp(p)

                            plotList[[length(plotList)+1]] <- p
                        }
                    }

                    if (col > row) {
                        if (options$plotCorrelationMatrix) {
                            if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                plotList[[length(plotList)+1]] <- .plotScatter(dataset[[variables[col]]], dataset[[variables[row]]]) # plot scatterplot
                            } else {
                                errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
                                plotList[[length(plotList)+1]] <- .displayError(errorMessagePlot, cexText=cexText)
                            }
                        } else {
                            
                            p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE)
                            p <- p + ggplot2::xlab("")
                            p <- p + ggplot2::ylab("")
                            p <- JASPgraphs::themeJasp(p)
                            
                            plotList[[length(plotList)+1]] <- p
                        }
                    }

                    if (col < row) {
                        if (l < 7) {
                            if (options$plotStatistics) {
                                if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                    plotList[[length(plotList)+1]] <- .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], hypothesis= options$hypothesis,
                                                  pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval) # plot r= ...
                                } else {
                                    errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                    errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
                                    plotList[[length(plotList)+1]] <- .displayError(errorMessagePlot, cexText=cexText)
                                }
                            } else {
                                
                                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE)
                                p <- p + ggplot2::xlab("")
                                p <- p + ggplot2::ylab("")
                                p <- JASPgraphs::themeJasp(p)
                                
                                plotList[[length(plotList)+1]] <- p
                            }
                        }

                        if (l >= 7) {
                            if (options$plotStatistics) {
                                if ( ! variable.statuses[[col]]$unplotable && ! variable.statuses[[row]]$unplotable) {
                                    plotList[[length(plotList)+1]] <- .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], cexCI= 1.2, hypothesis= options$hypothesis,
                                                  pearson=options$pearson, kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=options$confidenceIntervalsInterval)
                                                  # if(col == 1){
                                                  #     plotList[[length(plotList)]] <- plotList[[length(plotList)]] + ggplot2::annotate("text", x = 0, y = 1.5, label = .unv(variables)[row], angle = 90, size = 6, fontface = 2)
                                                  # }
                                } else {
                                    errorMessages <- c(variable.statuses[[row]]$plottingError, variable.statuses[[col]]$plottingError)
                                    errorMessagePlot <- paste0("Correlation coefficient undefined:", "\n", errorMessages[1])
                                    plotList[[length(plotList)+1]] <- .displayError(errorMessagePlot, cexText=cexText)
                                }
                            } else {
                                p <- JASPgraphs::drawAxis(xName = "", yName = "", force = TRUE)
                                p <- p + ggplot2::xlab("")
                                p <- p + ggplot2::ylab("")
                                p <- JASPgraphs::themeJasp(p)

                                plotList[[length(plotList)+1]] <- p
                            }
                        }
                    }
                }
            }
        
        plotMat <- matrix(plotList, ncol = length(variables), nrow = length(variables))

        p <- JASPgraphs::ggMatrixPlot(plotList = plotMat, leftLabels = .unv(variables), topLabels = .unv(variables))

        return(p)
    }

    obj <- .plotFunc()

    content <- .writeImage(width = width, height = height, plot = obj, obj = TRUE)

    plot <- correlation.plot
    plot[["convertible"]] <- TRUE
    plot[["obj"]] <- content[["obj"]]
    plot[["data"]] <- content[["png"]]

    correlation.plot <- plot
  }

  return(correlation.plot)
}

#### histogram with density estimator ####
.plotMarginalCor <- function(variable, cexYlab= 1.3, lwd= 2, rugs= FALSE) {
    
    variable <- na.omit(as.numeric(variable))
    
    density <- density(variable)
    
    h <- hist(variable, plot = FALSE)

    dens <- h$density
    yhigh <- 1.2*max(h$density)
    ylow <- 0
    xticks <- base::pretty(c(variable, h$breaks), min.n= 3)

    p <- JASPgraphs::drawAxis(xName = "", yName = "Density", xBreaks = xticks, yBreaks = c(0,max(density$y)+.1), force = TRUE, yLabels = NULL, xLabels = xticks)
    p <- p + ggplot2::geom_histogram(data = data.frame(variable), mapping = ggplot2::aes(x = variable, y = ..density..),
            binwidth = (h$breaks[2] - h$breaks[1]),
            fill = "grey",
            col = "black",
            size = .3,
            center = ((h$breaks[2] - h$breaks[1])/2))
    p <- p + ggplot2::geom_line(data = data.frame(x = density$x, y = density$y), mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black")
    
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
    p <- p + ggplot2::xlab("")
    
    # JASP theme
    p <- JASPgraphs::themeJasp(p)
    
    return(p)
    
}


#### scatterplots ####

# predictions of fitted model
.poly.pred <- function(fit, plot = NULL, line=FALSE, xMin, xMax, lwd) {
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
        plot <- plot + ggplot2::geom_line(data = data.frame(x, predY),mapping = ggplot2::aes(x = x, y = predY), size=lwd)
        return(plot)
    }
}


.plotScatter <- function(xVar, yVar, cexPoints= 1.3, cexXAxis= 1.3, cexYAxis= 1.3, lwd= 2) {
        
    d <- data.frame(xx= xVar, yy= yVar)
    d <- na.omit(d)
    xVar <- d$xx
    yVar <- d$yy

    # fit different types of regression
    fit <- vector("list", 1)# vector("list", 4)
    fit[[1]] <- lm(yy ~ poly(xx, 1, raw= TRUE), d)
    # fit[[2]] <- lm(yy ~ poly(xx, 2, raw= TRUE), d)
    # fit[[3]] <- lm(yy ~ poly(xx, 3, raw= TRUE), d)
    # fit[[4]] <- lm(yy ~ poly(xx, 4, raw= TRUE), d)

    # find parsimonioust, best fitting regression model
    # Bic <- vector("numeric", 4)
    # for(i in 1:4){

    #	Bic[i] <- BIC(fit[[i]])
    # }

    bestModel <- 1 # which.min(Bic)

    xlow <- min(pretty(xVar))
    xhigh <- max(pretty(xVar))
    xticks <- pretty(c(xlow, xhigh))
    ylow <- min(min(pretty(yVar)), min(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))
    yhigh <- max(max(pretty(yVar)), max(.poly.pred(fit[[bestModel]], line= FALSE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd=lwd)))


    yticks <- pretty(c(ylow, yhigh))
    yLabs <- vector("character", length(yticks))
    

    for (i in seq_along(yticks)) {
        if (yticks[i] < 10^6 && yticks[i] > 10^-6) {
            yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
        } else {
            yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
        }
    }
    
    xLabs <- vector("character", length(xticks))
    
    for (i in seq_along(xticks)) {
        if (xticks[i] < 10^6 && xticks[i] > 10^-6) {
            xLabs[i] <- format(xticks[i], digits= 3, scientific = FALSE)
        } else {
            xLabs[i] <- format(xticks[i], digits= 3, scientific = TRUE)
        }
    }
    
    p <- JASPgraphs::drawAxis(xName = "", yName = "", xBreaks = xticks, yBreaks = yticks, yLabels = yLabs, xLabels = xLabs, force = TRUE)
    p <- JASPgraphs::drawPoints(p, dat = d, size = 3)
    p <- .poly.pred(fit[[bestModel]], plot = p, line= TRUE, xMin= xticks[1], xMax= xticks[length(xticks)], lwd = 1)    

    # JASP theme
    p <- JASPgraphs::themeJasp(p)

    return(p)
}

#### display correlation value ####
.plotCorValue <- function(xVar, yVar, cexText= 2.5, cexCI= 1.7, hypothesis = "correlated", pearson=options$pearson,
                          kendallsTauB=options$kendallsTauB, spearman=options$spearman, confidenceInterval=0.95) {

    CIPossible <- TRUE

    tests <- c()

    if (pearson)
        tests <- c(tests, "pearson")

    if (spearman)
        tests <- c(tests, "spearman")

    if (kendallsTauB)
        tests <- c(tests, "kendall")


    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = grid::unit(c(1,1,1,1), "cm")
            ) + ggplot2::xlim(0,2) + ggplot2::ylim(0,2)

    lab <- vector("list")
    
    if (length(tests) == 1) {
        ypos <- 1.5
    }

    if (length(tests) == 2) {
        ypos <- c(1.6, 1.2)
    }

    if (length(tests) == 3) {
        ypos <- c(1.7, 1.2, .7)
    }

    for (i in seq_along(tests)) {
        if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == 1){
            CIPossible <- FALSE
    
            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '1.000'")
            }
    
            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '1.000'")
            }
    
            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '1.000'")
            }
        } else if (round(cor.test(xVar, yVar, method=tests[i])$estimate, 8) == -1){
            CIPossible <- FALSE
    
            if(tests[i] == "pearson"){
                lab[[i]] <- paste("italic(r) == '-1.000'")
            }
    
            if(tests[i] == "spearman"){
                lab[[i]] <- paste("italic(rho) == '-1.000'")
            }
    
            if(tests[i] == "kendall"){
                lab[[i]] <- paste("italic(tau) == '-1.000'")
            }
        } else {
            if(tests[i] == "pearson"){
                #lab[[i]] <- paste0("italic(r) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3))[1])
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "pearson", 3) # fix for rounding off decimals
            }
    
            if(tests[i] == "spearman"){
                #lab[[i]] <- paste0("italic(rho) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "spearman", 3) # fix for rounding off decimals
            }
    
            if(tests[i] == "kendall"){
                #lab[[i]] <- paste0("italic(tau) == ",as.character(formatC(round(cor.test(xVar, yVar, method=tests[i])$estimate,3), format="f", digits= 3)))
                lab[[i]] <- .corValueString(cor.test(xVar, yVar, method=tests[i])$estimate, "kendall", 3) # fix for rounding off decimals
            }
        }
    }
        
    for(i in seq_along(tests)){
        p <- p + ggplot2::geom_text(data = data.frame(x = rep(1, length(ypos)), y = ypos), mapping = ggplot2::aes(x = x, y = y, label = unlist(lab)), size = 7, parse = TRUE)
    }


    if (hypothesis == "correlated" & length(tests) == 1 & any(tests == "pearson")) {
        alternative <- "two.sided"
        ctest <- cor.test(xVar, yVar, method= tests, conf.level=confidenceInterval)
    }

    if (hypothesis != "correlated" & length(tests) == 1 & any(tests == "pearson")) {
        if (hypothesis == "correlatedPositively") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="greater", conf.level=confidenceInterval)
        } else if (hypothesis == "correlatedNegatively") {
            ctest <- cor.test(xVar, yVar, method=tests, alternative="less", conf.level=confidenceInterval)
        }
    }

    if (any(tests == "pearson")& length(tests) == 1 && CIPossible) {
        CIlow <- formatC(round(ctest$conf.int[1],3), format = "f", digits = 3)
        CIhigh <- formatC(round(ctest$conf.int[2],3), format = "f", digits = 3)

        if(length(p)>0){
            p <- p + ggplot2::geom_text(data = data.frame(x = 1, y = 1.2), mapping = ggplot2::aes(x = x, y = y, label = paste(100 * confidenceInterval, "% CI: [", CIlow, ", ", CIhigh, "]", sep="")), size = 5)
        }

    }

    return(p)

}

### empty Plot with error message ###
.displayError <- function(errorMessage=NULL, cexText=1.6, lwdAxis= 1.2) {
    p <- ggplot2::ggplot(data = data.frame(), ggplot2::aes(x = seq_along(data.frame()),y = summary(data.frame()))) +
        ggplot2::theme(
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            plot.margin = grid::unit(c(2,1,1,2), "cm"),
            axis.text.x =ggplot2::element_blank(),
            axis.title = ggplot2::element_blank()) +
        ggplot2::annotate("text", x = 4, y = 25, label = errorMessage, size = 9)
    return(p)
}

### Utility functions for nonparametric confidence intervals ###
.concordanceFunction <- function(i, j) {
  concordanceIndicator <- 0
  ij <- (j[2] - i[2]) * (j[1] - i[1])
  if (ij > 0) concordanceIndicator <- 1
  if (ij < 0) concordanceIndicator <- -1
  return(concordanceIndicator)
}

.addConcordances <- function(x, y, i) {
  concordanceIndex <- 0
  for (k in 1:length(x)) {
    if (k != i) {
      concordanceIndex <- concordanceIndex + .concordanceFunction(c(x[i], y[i]), c(x[k], y[k]))
    }
  }
  return(concordanceIndex)
}

.createNonparametricConfidenceIntervals <- function(x, y, obsCor, hypothesis = "two-sided", confLevel = 0.95, method = "kendall"){
  # Based on sections 8.3 and 8.4 of Hollander, Wolfe & Chicken, Nonparametric Statistical Methods, 3e.
  alpha <- 1 - confLevel
  missingIndices <- as.logical(is.na(x) + is.na(y)) # index those values that are missing
  x <- x[!missingIndices] # remove those values
  y <- y[!missingIndices]
  n <- length(x)

 if (method == "kendall") {
   concordanceSumsVector <- numeric(n)
    for (i in 1:n) {
      concordanceSumsVector[i] <- .addConcordances(x, y, i)
    }
    sigmaHatSq <- 2 * (n-2) * var(concordanceSumsVector) / (n*(n-1))
    sigmaHatSq <- sigmaHatSq + 1 - (obsCor)^2
    sigmaHatSq <- sigmaHatSq * 2 / (n*(n-1))

    if (hypothesis=="correlated"){
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (hypothesis!="correlated") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }
    ciLow <- obsCor - z * sqrt(sigmaHatSq)
    ciUp <- obsCor + z * sqrt(sigmaHatSq)
    if (hypothesis=="correlatedPositively") {
      ciUp <- 1
    } else if (hypothesis=="correlatedNegatively") {
      ciLow <- -1
    }
  } else if (method == "spearman") {
    stdErr = 1/sqrt(n-3)
    if (hypothesis=="correlated") {
      z <- qnorm(alpha/2, lower.tail = FALSE)
    } else if (hypothesis!="correlated") {
      z <- qnorm(alpha, lower.tail = FALSE)
    }

    ciLow = tanh(atanh(obsCor) - z * stdErr)
    ciUp = tanh(atanh(obsCor) + z * stdErr)

    if (hypothesis=="correlatedPositively") {
      ciUp <- 1
    } else if (hypothesis=="correlatedNegatively") {
      ciLow <- -1
    }
  }
  return(c(ciLow,ciUp))
}

.corValueString <- function(corValue = NULL, testType = NULL, decimals = 3){
    
    if (testType == "pearson"){
        string <- as.character(as.expression(substitute(italic(r)~"="~r2, list(r2 = formatC(round(corValue,decimals), format = "f", digits = decimals)))))
    } else if (testType == "spearman"){
        string <- as.character(as.expression(substitute(italic(rho)~"="~r2, list(r2 = formatC(round(corValue,decimals), format = "f", digits = decimals)))))
    } else if (testType == "kendall"){
        string <- as.character(as.expression(substitute(italic(tau)~"="~r2, list(r2 = formatC(round(corValue,decimals), format = "f", digits = decimals)))))
    }
    
    return(string)
    
}
