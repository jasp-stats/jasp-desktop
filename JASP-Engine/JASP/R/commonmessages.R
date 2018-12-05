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

.messages <- function(class, type, ...) {  
  m <- list()
  
  ### Error general
  m$error$opening <- 
  "The following problem(s) occurred while running the analysis:"
  m$error$grouping <- 
  "after grouping on {{grouping}}"
  m$error$exception <- 
  "This analysis terminated unexpectedly.<br><br>{{error}}<br><div class=stack-trace-selector><span>Stack trace</span><div class=stack-trace-arrow></div></div><div class=stack-trace>{{stackTrace}}</div><br>To receive assistance with this problem, please report the message above at: https://jasp-stats.org/bug-reports"
  
  ### Error checks
  m$error$infinity <- 
  "Infinity found in {{variables}}"  
  m$error$factorLevels <- 
  "Number of factor levels {{factorLevels.amount}} in {{variables}}"  
  m$error$variance <- 
  "Variance = {{variance.equalTo}} in {{variables}}"  
  m$error$observations <- 
  "Number of observations {{observations.amount}} in {{variables}}"  
  m$error$observationsPairwise <- 
  "Number of pairwise observations <{{observationsPairwise.amount}} in {{variables}}"  
  m$error$levene <- 
  "Cannot compute statistic reliably: number of observations {{observations.amount}} in {{variables}}"
  m$error$limits <-
  "Values in {{variables}} outside interval [{{limits.min}}, {{limits.max}}]"
  m$error$varCovMatrix <- 
  "Dataset is not a proper variance-covariance matrix. Please load only a positive definite symmetrical matrix as your dataset."
  m$error$varCovData <- 
  "The variance-covariance matrix of the supplied data is not positive-definite. Please check if variables have many missings observations or are collinear"
  
  ### Footnotes
  m$footnote$leveneSign <- 
  "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"
  m$footnote$VovkSellkeMPR <-
  "Vovk-Sellke Maximum <em>p</em>-Ratio: Based the <em>p</em>-value, the maximum possible odds in favor of H\u2081 over H\u2080 equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37 (Sellke, Bayarri, & Berger, 2001)."
  m$footnote$binomNeq <- 
  "Proportions tested against value: {{value}}."
  m$footnote$binomLess <- 
  "For all tests, the alternative hypothesis specifies that the proportion is less than {{value}}."
  m$footnote$binomGreater <- 
  "For all tests, the alternative hypothesis specifies that the proportion is greater than {{value}}."
  
  message <- m[[class]][[type]]
  if (is.null(message))
    stop(paste("Could not find message for class", class, "and type", type))
  
  args <- list(...)
  if (length(args) > 0)
    message <- .parseMessage(message, class, ...)
  
  return(message)
}
