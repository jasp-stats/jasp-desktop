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

classicalPlanning <- function(jaspResults, dataset, options, ...){

  # Frequentist analysis
  type <- "frequentist"

  # Set the nessecary options before the analysis
  planningOptions <- .auditPlanningOptions(options)

  # Explanatory text for the procedure paragraph
  .auditExplanatoryTextProcedure(options, planningOptions, 
                                  jaspResults, position = 1)

  # Explanatory text for the audit risk model paragraph
  .auditRiskModelParagraph(options, planningOptions, jaspResults, position = 2)

  # Check if analysis can be run
  ready <- .auditPlanningReady(options, planningOptions)

  # Create the planning container
  planningContainer <- .auditPlanningGetContainer(jaspResults, position = 3)

  # Perform early error checks
  .auditPlanningErrorChecks(options, planningOptions, planningContainer, ready)

  # Get the planningResult object
  planningState <- .auditPlanningState(options, planningOptions, 
                                        planningContainer, ready, type)

  # Explanatory text for the planning paragraph
  .auditExplanatoryTextPlanning(options, planningOptions, planningState, 
                                  planningContainer, ready, type, 
                                  positionInContainer = 1)
  
  # Fill the planning summary table
  .auditPlanningSummaryTable(options, planningOptions, planningState, 
                              planningContainer, ready, type, 
                              positionInContainer = 2)
  
  # Create an index for figure numbers
  planningContainer[["figNumber"]] <- createJaspState(1)

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, planningOptions, planningState, 
                          planningContainer, ready, type, 
                          positionInContainer = 3)

  # Create the implied sampling distribution plot
  .samplingDistributionPlot(options, planningOptions, planningState, 
                              planningContainer, ready, 
                              positionInContainer = 4)
}

.samplingDistributionPlot <- function(options, planningOptions, planningState, 
                                      planningContainer, ready, 
                                      positionInContainer){

  if(!options[["samplingDistribution"]]) return()

  if(is.null(planningContainer[["samplingDistribution"]])){

    likelihood <- base::switch(options[["planningModel"]], 
                                "Poisson" = "Poisson", 
                                "binomial" = "Binomial", 
                                "hypergeometric" = "Hypergeometric")

    plotTitle <- paste0("Implied ", likelihood, " Sampling Distribution")
    samplingDistribution <- createJaspPlot(plot = NULL, title = plotTitle, 
                                            width = 600, height = 300)

    samplingDistribution$position <- positionInContainer
    samplingDistribution$dependOn(options = c("planningModel", 
                                              "samplingDistribution"))
    planningContainer[["samplingDistribution"]] <- samplingDistribution

    if(!ready || planningContainer$getError()) 
      return()

    limx <- length(0:planningState[["sampleSize"]])
    if(limx > 31) {
        limx <- 31
    }

    xVals <- (0:planningState[["sampleSize"]])[1:limx]
    if(planningState[["likelihood"]] == "poisson"){
      d0 <- stats::dpois(x = xVals, lambda = planningState[["materiality"]] * planningState[["sampleSize"]])
      d1 <- stats::dpois(x = 0:planningState[["expectedSampleError"]], lambda = planningState[["materiality"]] * planningState[["sampleSize"]])
    } else if(planningState[["likelihood"]] == "binomial"){
      d0 <- stats::dbinom(x = xVals, size = planningState[["sampleSize"]], prob = planningState[["materiality"]])
      d1 <- stats::dbinom(x = 0:planningState[["expectedSampleError"]], size = planningState[["sampleSize"]], prob = planningState[["materiality"]])
    } else if(planningState[["likelihood"]] == "hypergeometric"){
      d0 <- stats::dhyper(x = xVals, m = planningState[["populationK"]], n = planningState[["N"]] - planningState[["populationK"]], k = planningState[["sampleSize"]])
      d1 <- stats::dhyper(x = 0:planningState[["expectedSampleError"]], m = planningState[["populationK"]], n = planningState[["N"]] - planningState[["populationK"]], k = planningState[["sampleSize"]])
    }

    data0 <- data.frame(x = xVals, y = d0)
    data1 <- data.frame(x = 0:planningState[["expectedSampleError"]], y = d1)

    xTicks <- JASPgraphs::getPrettyAxisBreaks(xVals)
    yTicks <- JASPgraphs::getPrettyAxisBreaks(data0$y)

    pdata <- data.frame(x = c(0, 0), y = c(0, 0), type = c("Expected error-free", "Expected errors"))
    pdata$type <- factor(pdata$type, levels(pdata$type)[c(2,1)])

    p <- ggplot2::ggplot(data = pdata, ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(shape = 2, alpha = 0) +
          ggplot2::labs(fill = "") +
          ggplot2::scale_x_continuous(name = "n", labels = xTicks, breaks = xTicks) +
          ggplot2::scale_y_continuous(name = "Probability", labels = yTicks, breaks = yTicks) +
          ggplot2::geom_bar(data = data0, mapping = ggplot2::aes(x = x, y = y), stat = "identity", fill = "#7FE58B", size = 0.5, color = "black") +
          ggplot2::geom_bar(data = data1, mapping = ggplot2::aes(x = x, y = y), stat = "identity", fill = "#FF6666", size = 0.5, color = "black") +
          ggplot2::geom_point(data = pdata, mapping = ggplot2::aes(x = x, y = y, fill = type), size = 0) +
          ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = 0, r = 30))) +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 12, shape = 22, fill = c("#FF6666", "#7FE58B"), stroke = 1.5, color = "black")))
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"))

    samplingDistribution$plotObject <- p

  }

  if(options[["explanatoryText"]]){
    figure3 <- createJaspHtml(paste0("<b>Figure ", planningContainer[["figNumber"]]$object ,".</b> The implied <b>", options[["planningModel"]], "</b> sampling distribution. The number of expected errors in the selection is colored in 
                                      red and the number of expected error-free observations is colored in green. The total probability of the errors does 
                                      not exceed the detection risk."), "p")
    figure3$position <- positionInContainer + 1
    figure3$dependOn(optionsFromObject = planningContainer[["samplingDistribution"]])
    planningContainer[["figure3"]] <- figure3
    planningContainer[["figNumber"]] <- createJaspState(planningContainer[["figNumber"]]$object + 1)
  }
}