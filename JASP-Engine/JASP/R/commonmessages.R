.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- 'The following problem(s) occurred while running the analysis:'
  messages$error$infinity <- 'Infinity found in %variables%'
  messages$error$factorLevels <- 'Factor levels %factorLevels.amount% in %variables%'
  messages$error$variance <- 'Variance = %variance.equalTo% in %variables%'
  messages$error$observations <- 'Observations < %observations.lessThan% in %variables%'
  
  return(messages[[type]][[id]])
}