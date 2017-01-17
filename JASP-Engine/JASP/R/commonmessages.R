.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- "The following problem(s) occurred while running the analysis:"
  messages$error$grouping <- 'after grouping on {{grouping}}'
  messages$error$exception <- "This analysis terminated unexpectedly.<br><br>{{error}}<br><div class=stack-trace-selector><span>Stack trace</span><div class=stack-trace-arrow></div></div><div class=stack-trace>{{stackTrace}}</div><br>To receive assistence with this problem, please report the message above at: https://jasp-stats.org/bug-reports"
  messages$error$infinity <- "Infinity found in {{variables}}"
  messages$error$factorLevels <- "Number of factor levels {{factorLevels.amount}} in {{variables}}"
  messages$error$variance <- "Variance = {{variance.equalTo}} in {{variables}}"
  messages$error$observations <- "Number of observations {{observations.amount}} in {{variables}}"
  messages$error$levene <- "Cannot compute statistic reliably: number of observations {{observations.amount}} in {{variables}}"
  
  messages$notes$leveneSign <- "Levene's test is significant (p < .05), suggesting a violation of the equal variance assumption"
  return(messages[[type]][[id]])
}
# TODO: add citations, titles, (foot)notes, additional error messages