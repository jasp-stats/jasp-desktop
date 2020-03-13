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
    gettext("The following problem(s) occurred while running the analysis:")
  m$error$grouping <- 
    gettextf("after grouping on %s", "{{grouping}}")
  m$error$fatalError <- 
    gettextf("This analysis terminated unexpectedly. %s To receive assistance with this problem, please report the message above at: https://jasp-stats.org/bug-reports",
             "<br><br>{{error}}<br><div class=stack-trace-selector><span>Stack trace</span><div class=stack-trace-arrow></div></div><div class=stack-trace>{{stackTrace}}</div><br>")
  
  ### Error checks
  m$error$infinity <- 
    gettextf("Infinity found in %s", "{{variables}}")
  m$error$factorLevels <- 
    gettextf("Number of factor levels is %s in %s", "{{factorLevels.amount}}", "{{variables}}")
  m$error$variance <- 
    gettextf("The variance in %s is equal to %s", "{{variables}}", "{{variance.equalTo}}")
  m$error$observations <- 
    gettextf("Number of observations is %s in %s", "{{observations.amount}}", "{{variables}}")
  m$error$levene <- 
    gettextf("Cannot compute statistic reliably: number of observations is %s in %s", "{{observations.amount}}", "{{variables}}")
  m$error$limits <-
    gettextf("Values in %s outside interval [%s, %s]", "{{variables}}", "{{limits.min}}", "{{limits.max}}")
  m$error$varCovMatrix <- 
    gettext("Dataset is not a proper variance-covariance matrix. Please load only a positive definite symmetrical matrix as your dataset.")
  m$error$varCovData <- 
    gettext("The variance-covariance matrix of the supplied data is not positive-definite. Please check if variables have many missings observations or are collinear")
  m$error$modelInteractions <-
    gettext("Main effects and lower-order interactions must be included whenever the corresponding higher-order interaction is included")
  m$error$negativeValues <-
    gettextf("Negative numbers found in %s", "{{variables}}")
  m$error$missingValues <-
    gettextf("Missing values encountered in %s", "{{variables}}")
  m$error$duplicateColumns <-
    gettextf("Duplicate variables encountered in %s", "{{variables}}")
  
  ### Footnotes
  m$footnote$leveneSign <- 
    gettext("Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption")
  m$footnote$VovkSellkeMPR <-
    gettextf("Vovk-Sellke Maximum <em>p</em>-Ratio: Based on the <em>p</em>-value, the maximum possible odds in favor of H%1$s over H%2$s equals 1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> %3$s .37 (Sellke, Bayarri, & Berger, 2001).","\u2081","\u2080","\u2264")
  m$footnote$binomNeq <- 
    gettextf("Proportions tested against value: %s", "{{value}}.")
  m$footnote$binomLess <- 
    gettextf("For all tests, the alternative hypothesis specifies that the proportion is less than %s", "{{value}}.")
  m$footnote$binomGreater <- 
    gettextf("For all tests, the alternative hypothesis specifies that the proportion is greater than %s", "{{value}}.")
  
  message <- m[[class]][[type]]
  if (is.null(message))
    stop(gettextf("Could not find message for class %s and type %s", as.character(class), as.character(type)))
  
  args <- list(...)
  if (length(args) > 0)
    message <- .parseMessage(message, class, ...)
  
  return(message)
}
