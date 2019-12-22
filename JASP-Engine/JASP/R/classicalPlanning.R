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

  # We're doing a frequentist analysis
  type <- "frequentist"

  # Deduct the nessecary values from the input options
  planningOptions <- .auditPlanningOptions(options)

  # Create the procedure paragraph
  .auditExplanatoryTextProcedure(options, 
                                 planningOptions, 
                                 jaspResults, 
                                 position = 1)

  # Create the audit risk model paragraph
  .auditRiskModelParagraph(options, 
                           planningOptions, 
                           jaspResults, 
                           position = 2)

  # Check if the options have valid values for running the analysis
  ready <- .auditPlanningReady(options, 
                               planningOptions)

  # Create the container that holds the planning output
  planningContainer <- .auditPlanningGetContainer(jaspResults, 
                                                  position = 3)

  # Perfrom early error checks
  .auditPlanningErrorChecks(options, 
                            planningOptions, 
                            planningContainer, 
                            ready)

  # Get the planning state if it exists, otherwise make one
  planningState <- .auditPlanningState(options, 
                                       planningOptions, 
                                       planningContainer, 
                                       ready, 
                                       type)

  # Create explanatory text for the planning
  .auditExplanatoryTextPlanning(options, 
                                planningOptions, 
                                planningState, 
                                planningContainer, 
                                ready, 
                                type, 
                                positionInContainer = 1)

  # --- TABLES
  
  # Create the summary table
  .auditPlanningSummaryTable(options, 
                             planningOptions, 
                             planningState, 
                             planningContainer, 
                             ready, 
                             type, 
                             positionInContainer = 2)
  
  # ---
  
  # --- PLOTS
  
  # Create a state to keep track of figure numbers
  planningContainer[["figNumber"]] <- createJaspState(1)

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, 
                        planningOptions, 
                        planningState, 
                        planningContainer, 
                        ready, 
                        type, 
                        positionInContainer = 3)

  # Create the implied sampling distribution plot
  .samplingDistributionPlot(options, 
                            planningOptions, 
                            planningState, 
                            planningContainer, 
                            ready, 
                            positionInContainer = 4)

  # ---
  
  # --- BADGES

  # Provide the analysis badges
  .auditBadgeSection(options,
                     type = "planning",
                     stateContainer = NULL,
                     jaspResults, 
                     ready, 
                     position = 5)

  # --- 
}

.samplingDistributionPlot <- function(options, 
                                      planningOptions, 
                                      planningState, 
                                      planningContainer, 
                                      ready, 
                                      positionInContainer){

  if(!options[["samplingDistribution"]]) 
    return()

  if(is.null(planningContainer[["samplingDistribution"]])){

    likelihood <- base::switch(options[["planningModel"]], 
                               "Poisson" = "Poisson", 
                               "binomial" = "Binomial", 
                               "hypergeometric" = "Hypergeometric")

    plotTitle <- paste0("Implied ", likelihood, " Sampling Distribution")

    samplingDistribution <- createJaspPlot(plot = NULL, 
                                           title = plotTitle, 
                                           width = 600, 
                                           height = 300)

    samplingDistribution$position <- positionInContainer
    samplingDistribution$dependOn(options = c("planningModel", 
                                              "samplingDistribution"))

    planningContainer[["samplingDistribution"]] <- samplingDistribution

    if(!ready || planningContainer$getError()) 
      return()

    xVals <- 0:planningState[["sampleSize"]]
    limx <- planningState[["sampleSize"]] + 1
    if(limx > 31)
        limx <- 31
    xVals <- xVals[1:limx]

    if(planningState[["likelihood"]] == "poisson"){

      dErrorFree <- stats::dpois(x = xVals, 
                                 lambda = planningState[["materiality"]] * 
                                          planningState[["sampleSize"]])

      dError <- stats::dpois(x = 0:planningState[["expectedSampleError"]], 
                             lambda = planningState[["materiality"]] * 
                                      planningState[["sampleSize"]])

    } else if(planningState[["likelihood"]] == "binomial"){

      dErrorFree <- stats::dbinom(x = xVals, 
                                  size = planningState[["sampleSize"]], 
                                  prob = planningState[["materiality"]])

      dError <- stats::dbinom(x = 0:planningState[["expectedSampleError"]], 
                              size = planningState[["sampleSize"]], 
                              prob = planningState[["materiality"]])

    } else if(planningState[["likelihood"]] == "hypergeometric"){

      dErrorFree <- stats::dhyper(x = xVals, 
                                  m = planningState[["populationK"]], 
                                  n = planningState[["N"]] - 
                                      planningState[["populationK"]], 
                                  k = planningState[["sampleSize"]])

      dError <- stats::dhyper(x = 0:planningState[["expectedSampleError"]], 
                              m = planningState[["populationK"]], 
                              n = planningState[["N"]] - 
                                  planningState[["populationK"]], 
                              k = planningState[["sampleSize"]])

    }

    dataErrorFree <- data.frame(x = xVals, 
                                y = dErrorFree)
    dataError <- data.frame(x = 0:planningState[["expectedSampleError"]], 
                            y = dError)

    dataLegend <- data.frame(x = c(0, 0), 
                             y = c(0, 0), 
                             type = c("Expected error-free", "Expected errors"))
    dataLegend$type <- factor(x = dataLegend[["type"]], 
                              levels = levels(dataLegend[["type"]])[c(2,1)])

    xTicks <- JASPgraphs::getPrettyAxisBreaks(xVals)
    yTicks <- JASPgraphs::getPrettyAxisBreaks(dataErrorFree[["y"]])

    myLegend <- ggplot2::guide_legend(override.aes=list(size = 12, 
                                                       shape = 22, 
                                                       fill = c("#FF6666", 
                                                                "#7FE58B"), 
                                                       stroke = 1.5, 
                                                       color = "black"))
    
    myTheme <- ggplot2::theme(panel.grid.major.y = 
                              ggplot2::element_line(color="#cbcbcb")) +
              ggplot2::theme(legend.text = ggplot2::element_text(margin = 
                                ggplot2::margin(l = 0, r = 30)))

    p <- ggplot2::ggplot(data = dataLegend, 
                         mapping = ggplot2::aes(x = x, y = y, fill = type)) +
          ggplot2::geom_point(shape = 2, 
                              alpha = 0) +
          ggplot2::scale_x_continuous(name = "n", 
                                      labels = xTicks, 
                                      breaks = xTicks) +
          ggplot2::scale_y_continuous(name = "Probability", 
                                      labels = yTicks, 
                                      breaks = yTicks) +
          ggplot2::geom_bar(data = dataErrorFree, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#7FE58B", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_bar(data = dataError, 
                            mapping = ggplot2::aes(x = x, y = y), 
                            stat = "identity", 
                            fill = "#FF6666", 
                            size = 0.5, 
                            color = "black") +
          ggplot2::geom_point(data = dataLegend, 
                              mapping = ggplot2::aes(x = x, y = y, fill = type), 
                              size = 0) +
          ggplot2::scale_fill_manual(values=c("#7FE58B", "#FF6666"), 
                                     guide = ggplot2::guide_legend(reverse = TRUE)) +
          ggplot2::labs(fill = "") +
          ggplot2::guides(fill = myLegend)
    
    p <- JASPgraphs::themeJasp(p, legend.position = "top") + myTheme

    samplingDistribution$plotObject <- p

  }

  if(options[["explanatoryText"]]){

    samplingDistributionText <- createJaspHtml(paste0("<b>Figure ", 
                                                      planningContainer[["figNumber"]]$object ,
                                                      ".</b> The implied <b>", 
                                                      options[["planningModel"]], 
                                                      "</b> sampling distribution. The number of expected errors in the selection is colored in 
                                                      red and the number of expected error-free observations is colored in green. The total probability of the errors does 
                                                      not exceed the detection risk as specified through the audit risk model."), "p")
    
    samplingDistributionText$position <- positionInContainer + 1
    samplingDistributionText$dependOn(optionsFromObject = planningContainer[["samplingDistribution"]])
    
    planningContainer[["samplingDistributionText"]] <- samplingDistributionText
    planningContainer[["figNumber"]] <- createJaspState(planningContainer[["figNumber"]]$object + 1)
    
  }
}