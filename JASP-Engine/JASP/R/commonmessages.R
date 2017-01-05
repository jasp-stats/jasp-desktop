.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- "The following problem(s) occurred while running the analysis:"
  messages$error$grouping <- 'after grouping on {{groupingVars}}'
  messages$error$infinity <- "Infinity found in {{variables}}"
  messages$error$factorLevels <- "Number of factor levels {{factorLevels.operator}} {{factorLevels.amount}} in {{variables}}"
  messages$error$variance <- "Variance = {{variance.equalTo}} in {{variables}}"
  messages$error$observations <- "Number of observations < {{observations.lessThan}} in {{variables}}"
  messages$error$unexpected <- "This analysis terminated unexpectedly.<br><br>Error in {{error}}<br><br><div class=stack-trace-selector>Stack trace:</div><div class=stack-trace>{{stackTrace}}</div><br>To receive assistence with this problem, please report the message above at: https://jasp-stats.org/bug-reports"
  
  return(messages[[type]][[id]])
}
# TODO: add citations, titles, footnotes, additional error messages