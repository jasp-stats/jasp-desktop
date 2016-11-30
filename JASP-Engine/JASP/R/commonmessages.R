.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- 'The following problem(s) occurred while running the analysis:'
  messages$error$infinity <- 'Infinity found in %variables%'
  messages$error$factorLevels <- 'Factor levels %factorLevels.amount% in %variables%'
  messages$error$variance <- 'Variance = %variance.amount% in %variables%'
  messages$error$groupSize <- 'Group size is unequal'
  messages$error$observations <- 'Observations < %observations.amount% in %variables%'
  
  return(messages[[type]][[id]])
}