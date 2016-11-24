.messages <- function(type, id) {
  messages <- list()
  # error
  messages$error$opening <- 'The following problem(s) occurred while running the analysis:'
  messages$error$infinity <- list('Infinity found in %variables%')
  messages$error$factorLevels <- list('Factor levels %levels% in %variables%')
  messages$error$variance <- list('Variance = %errVar% in %variables%')
  messages$error$groupSize <- list('Group size is unequal')
  
  return(messages[[type]][[id]])
}