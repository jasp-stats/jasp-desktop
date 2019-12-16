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

  # Bayesian analysis
  type <- "bayesian"

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
  
  # Create the implicit sample table
  .auditImplicitSampleTable(options, planningState, planningContainer, 
                              ready, positionInContainer = 3)

  # Create an index for figure numbers
  planningContainer[["figNumber"]] <- createJaspState(1)

  # Create the decision analysis plot
  .decisionAnalysisPlot(options, planningOptions, planningState, 
                          planningContainer, ready, type, 
                          positionInContainer = 4)

  # Create the prior and expected posterior plot
  .auditPlanningPlotPrior(options, planningOptions, planningState, 
                          planningContainer, ready, positionInContainer = 5)
}