.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- 'The following problem(s) occurred while running the analysis:'
  messages$error$infinity <- 'Infinity found in {variables}'
  messages$error$factorLevels <- 'Number of factor levels {factorLevels.operator} {factorLevels.amount} in {variables}'
  messages$error$variance <- 'Variance = {variance.equalTo} in {variables}'
  messages$error$observations <- 'Number of observations < {observations.lessThan} in {variables}'
  
  return(messages[[type]][[id]])
}