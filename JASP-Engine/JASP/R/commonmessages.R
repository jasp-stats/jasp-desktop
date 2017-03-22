#
# Copyright (C) 2013-2017 University of Amsterdam
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

.messages <- function(class, type) {
  
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
  m$error$levene <- 
  "Cannot compute statistic reliably: number of observations {{observations.amount}} in {{variables}}"
  
  ### Footnotes
  m$footnote$leveneSign <- 
  "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"
  
  
  return(m[[class]][[type]])

}