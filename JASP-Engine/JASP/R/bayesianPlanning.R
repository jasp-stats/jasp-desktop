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

bayesianPlanning <- function(jaspResults, dataset, options, ...){

  # We're doing a Bayesian analysis
  type <- "bayesian"

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

  ## TABLES

  # Create the summary table
  .auditPlanningSummaryTable(options, 
                             planningOptions, 
                             planningState, 
                             planningContainer, 
                             ready, 
                             type, 
                             positionInContainer = 2)
  
  # Create the implicit sample table
  .auditImplicitSampleTable(options, 
                            planningState, 
                            planningContainer, 
                            ready, 
                            positionInContainer = 3)

  # Cerate the prior and posterior statistics table
  .auditPriorAndPosterStatisticsTable(options, 
                                      planningState, 
                                      planningContainer, 
                                      ready, 
                                      positionInContainer = 4)

  ## PLOTS

  # Create a state to keep track of figure numbers
  planningContainer[["figNumber"]] <- createJaspState(1)

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, 
                        planningOptions, 
                        planningState, 
                        planningContainer, 
                        ready, 
                        type, 
                        positionInContainer = 5)

  # Create the prior and expected posterior plot
  .auditPlanningPlotPrior(options, 
                          planningOptions, 
                          planningState, 
                          planningContainer, 
                          ready, 
                          positionInContainer = 6)
}