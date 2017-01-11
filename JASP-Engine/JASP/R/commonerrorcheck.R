.quitAnalysis <- function(message) {
  e <- structure(class = c('expectedError', 'error', 'condition'),
                 list(message=message, call=sys.call(-1)))
  stop(e)
}


.addStackTrace <- function(e) {
  stack <- ''
  if (!is.null(sys.calls()) && length(sys.calls()) > 10) {
    
    stack <- sys.calls()  
    stack <- head(stack[7:length(stack)], -2) # The first 6 and last 2 calls provide no information
    if (length(stack) > 10) {
      stack <- tail(stack, 10) # Show at most the final 10 calls before the error
    }
    
  }
  e$stackTrace <- stack
  signalCondition(e) # Signal the modified error object
}


# Generic function to create error message (mostly used in conjunction with .hasErrors()).
# Args:
#   type: String containing check type.
#   variables: Vector of variables or single string indicating what did not pass the check.
#   includeOpening: Boolean, should there be a general opening line (TRUE) or only the specific error message (FALSE).
#   concatenateWith: String, include if you want to append the error message to an already existing message.
#   ...: Each error message can have any number of variables, denoted by {}'s. Add these as arg=val pairs.
#
# Returns:
#   String containing the error message.
.generateErrorMessage <- function(type, variables=NULL, groupingVars=NULL, includeOpening=FALSE, concatenateWith=NULL, ...) {
  
  if (length(type) == 0) {
    stop('Non-valid type argument provided')
  }
  
  replaceInMessage <- list('!=' = '≠')
  args <- list(...)
  
  message <- .messages('error', type)
  if (is.null(message)) {
    stop('Could not find error message for "', type, '"')
  }
  
  # See if we need to add a grouping line
  if (length(groupingVars) > 0) {
    message <- paste(message, .messages('error', 'grouping'))
    message <- gsub('{{groupingVars}}', paste(groupingVars, collapse=', '), message, fixed=TRUE)
  }
  
  # See if we need to specify variables
  if (grepl('{{variables}}', message, fixed=TRUE)) {
    if (is.null(variables)) {
      stop('This error message requires the offending variables to be specified')
    }
    message <- gsub('{{variables}}', paste(variables, collapse=', '), message, fixed=TRUE)
  }
  
  # Find all {string}'s that needs to be replaced by values
  toBeReplaced <- regmatches(message, gregexpr("(?<=\\{{)\\S*?(?=\\}})", message, perl=TRUE))[[1]]
  if (base::identical(toBeReplaced, character(0)) == FALSE) { # were there any {string}'s?
    if (all(toBeReplaced %in% names(args)) == FALSE) {
      missingReplacements <- toBeReplaced[!toBeReplaced %in% names(args)]
      stop('Missing required replacement(s): "', paste(missingReplacements, collapse=','), '"')
    }
    for (i in 1:length(toBeReplaced)) {
      message <- gsub(paste0('{{', toBeReplaced[i], '}}'), args[[ toBeReplaced[i] ]], message, fixed=TRUE)
    }
  }
  
  # Find all values we do not want in the output, e.g. we do not want to show !=
  for (i in 1:length(replaceInMessage)) {
    if (grepl(names(replaceInMessage)[i], message)) {
      message <- gsub(names(replaceInMessage)[i], replaceInMessage[[i]], message)
    }
  }
  
  # Add line indicator
  if (!is.null(concatenateWith) || includeOpening == TRUE) {
    message <- paste0('<li>', message, '</li></ul>')
  }
  
  # See if we should concatenate it with something
  if (!is.null(concatenateWith)) {
    endOfString <- substr(concatenateWith, nchar(concatenateWith)-4, nchar(concatenateWith))
    if (endOfString == '</ul>') { # Check if we need to remove a closing </ul>
      concatenateWith <- substr(concatenateWith, 1, nchar(concatenateWith)-5)
    }
    message <- paste0(concatenateWith, message)
  }
  
  # See if we should add an opening line
  if (includeOpening == TRUE) {
    message <- paste0(.messages('error', 'opening'), '<ul>', message)
  }
  
  return(message)
}


# Generic error checking function.
# Args:
#   dataset: Normal JASP dataset.
#   perform: 'run' or 'init'.
#   type: List/vector/character containing check type(s).
#   message: 'short', 'default' or 'verbose', should only the first failure of a check be reported in single line form ('short'), or should every check be mentioned in multi-line form;
#             in which case, should variables be mentioned multiple times in multiple checks ('verbose'), or only for the first failure ('default'). (In any case a full error list is generated)
#   exitAnalysisIfErrors: Boolean, should the function simply return its results, or abort the entire analysis when a failing check is encountered.
#   ...: Each check may have required and optional arguments, they are specified in the error check subfunctions.
#        To perform the check only on certain variables instead of all, include a target (e.g. infinity.target=options$dependent, variance.target...).
#
# Returns:
#   FALSE if no errors were found or a list specifying for each check which variables violated it as well as a general error message.
.hasErrors <- function(dataset, perform, type, message='default', exitAnalysisIfErrors=FALSE, ...) {
  
  if (is.null(dataset) || perform != 'run' || length(type) == 0) {
    return(FALSE)
  }
  
  if (exitAnalysisIfErrors && message == 'short') {
    message <- 'default'
  }
  
  # Error checks definition
  checks <- list()
  checks[['infinity']] <- list(callback=.checkInfinity, addGrouping=FALSE)
  checks[['factorLevels']] <- list(callback=.checkFactorLevels, addGrouping=FALSE)
  checks[['variance']] <- list(callback=.checkVariance, addGrouping=TRUE)
  checks[['observations']] <- list(callback=.checkObservations, addGrouping=TRUE)
  
  args <- list(...)
  errors <- list(message=NULL)
  
  for (i in 1:length(type)) {
    
    # See if the specified check exists
    check <- checks[[ type[[i]] ]]
    if (is.null(check)) {
      stop('Unknown check type provided: "', type[[i]], '"')
    }
    
    # Obtain the arguments this specific callback uses
    funcArgs <- base::formals(check[['callback']])

    # Fill in the 'all.*' arguments for this check
    # TODO when R version 3.3 is installed we can use: argsWithAll <- args[startsWith(names(args), 'all.')]
    argsAllPrefix <- args[substring(names(args), 1, 4) == 'all.']
    if (length(argsAllPrefix) > 0) {
      for (a in names(argsAllPrefix)) {
        funcArg <- gsub('all', type[[i]], a, fixed=TRUE)
        if (funcArg %in% names(funcArgs)) {
          args[[funcArg]] <- args[[a]]
        }
      }
    }

    # See if this check expects target variables and if they were provided, if not add all variables
    if (paste0(type[[i]], '.target') %in% names(funcArgs) && !paste0(type[[i]], '.target') %in% names(args)) {
      args[[ paste0(type[[i]], '.target') ]] <- .unv(names(dataset))
    } 
    
    # Obtain an overview of required and optional check arguments
    exclude <- c('dataset', '...')
    optArgs <- list()
    reqArgs <- list()
    for (a in 1:length(funcArgs)) {
      if (names(funcArgs)[a] %in% exclude)
        next
      if (is.symbol(funcArgs[[a]])) { # required args' value is symbol
        reqArgs <- c(reqArgs, funcArgs[a])
      } else {
        optArgs <- c(optArgs, funcArgs[a])
      }
    }
    
    # See if all required arguments are provided
    if (length(reqArgs) > 0 && all(names(reqArgs) %in% names(args)) == FALSE) {
      missingArgs <- reqArgs[!names(reqArgs) %in% names(args)]
      stop('Missing required argument(s): "', paste(names(missingArgs), collapse=','), '"')
    }
    
    # See if optional arguments are provided, if not, add default values to args so we can use them in the error message
    if (length(optArgs) > 0 && all(names(optArgs) %in% names(args)) == FALSE) {
      args <- c(args, optArgs[!names(optArgs) %in% names(args)])
    }
    
    checkResult <- base::do.call(check[['callback']], c(list(dataset=dataset), args))
    
    # Are there errors
    if (checkResult[['error']] == TRUE) {
      
      createMessage <- TRUE
      varsToAdd <- checkResult[['errorVars']]
      
      # In case we want a single line message
      if (message == 'short' && length(errors) > 1) {
        createMessage <- FALSE
      } 
      
      # Or multi-line but with single variable mentions
      if (message == 'default' && length(errors) > 1 && !is.null(varsToAdd)) {
        for (e in 2:length(errors)) { # first element is the error message
          varsToAdd <- varsToAdd[ !varsToAdd %in% errors[[e]] ]
          if (length(varsToAdd) == 0) {
            createMessage <- FALSE
            break
          }
        }
      }
      
      # If it's all good create/expand the error message
      if (createMessage == TRUE) {
        opening = FALSE
        if (is.null(errors[['message']]) && message != 'short') {
          opening = TRUE
        }

        grouping <- NULL
        if (!is.null(args[[ paste0(type[[i]], '.grouping') ]]) && check[['addGrouping']] == TRUE) {
          grouping <- args[[ paste0(type[[i]], '.grouping') ]]
        }
        
        errors[['message']] <- base::do.call(.generateErrorMessage, 
                                             c(list(type=type[[i]], variables=varsToAdd, groupingVars=grouping, includeOpening=opening, concatenateWith=errors[['message']]), args))
      }
      
      # Add the error (with any offending variables, otherwise TRUE) to the list
      if (is.null(checkResult[['errorVars']])) {
        errors[[ type[[i]] ]] <- TRUE
      } else {
        errors[[ type[[i]] ]] <- checkResult[['errorVars']]
      }
      
    }
    
  }

  # Done with all the checks, time to return...
  if (length(errors) == 1)  {
    return(FALSE)
  } 
  
  if (exitAnalysisIfErrors == TRUE) {
    .quitAnalysis(errors[['message']])
  }
  
  return(errors) 
}


# Check for infinity, optionally specify grouping with levels (makes no sense to perform a grouped search without levels)
.checkInfinity <- function(dataset, infinity.target, infinity.grouping=NULL, infinity.groupingLevel=NULL, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  for (v in infinity.target) {
    
    if (is.factor(dataset[[.v(v)]])) { # Coerce factor to numeric
      dataset[[.v(v)]] <- as.numeric(as.character(dataset[[.v(v)]]))
    } 
    
    if (length(infinity.grouping) > 0 && length(infinity.groupingLevel) > 0) { # There are specific grouping vars
      expr <- paste(infinity.grouping, infinity.groupingLevel, sep='==', collapse='&') # Create a subset expression
      df <- subset(dataset, eval(parse(text=expr)))
      hasInf <- any(is.infinite(df[[.v(v)]]))
      
    } else { # No specific subgroup to check
      
      hasInf <- any(is.infinite(dataset[[.v(v)]]))
      
    }
    
    if (hasInf) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}


# Check for the amount of factor levels, required arguments are an amount (numeric) to check and its operator (string) (e.g. '!=' 2 or '>' 1)
.checkFactorLevels <- function(dataset, factorLevels.target, factorLevels.amount, factorLevels.operator, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  for (v in factorLevels.target) {
    
    levelsOfVar <- length(unique(dataset[[.v(v)]]))
    expr <- paste(levelsOfVar, factorLevels.operator, factorLevels.amount) # Build the expression to check for
    if (eval(parse(text=expr)) == TRUE) { # See if this expression is true
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}


# Check for variance in the data. Optionally specify the variance amount to check and grouping (possibly with levels)
.checkVariance <- function(dataset, variance.target, variance.grouping=NULL, variance.groupingLevel=NULL, variance.equalTo=0, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  getVariance <- function(df, target) {
    validValues <- df[[target]][is.finite(df[[target]])]
    variance <- -1 # Prevents the function from returning NA's
    if (length(validValues) > 1) {
      variance <- stats::var(validValues)
    }
    return(variance)
  }
  
  for (v in variance.target) {
    
    if (length(variance.grouping) > 0) { # There are grouping vars
      
      if (length(variance.groupingLevel) > 0) { # We only need to check a specific level
        expr <- paste(variance.grouping, variance.groupingLevel, sep='==', collapse='&') # Create a subset expression
        variance <- getVariance(subset(dataset, eval(parse(text=expr))), .v(v))
      } else { # Check all levels
        # ddply groups the dataset and performs a function on each subgroup, returns a dataframe with the subgroups and the function results
        variance <- plyr::ddply(dataset, .v(variance.grouping), getVariance, .v(v))
      }
      
    } else { # no grouping vars
      
      variance <- getVariance(dataset, .v(v))
      
    }
    
    if ((is.data.frame(variance) && any(variance[, ncol(variance)] == variance.equalTo)) || # Check the last column with the function results
        (is.numeric(variance) && variance == variance.equalTo)) { # Or the scalar
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}


# check for the number of observations in the dependent(s). Requires the minimum possible value to be supplied, optionally grouping (possibly with levels)
.checkObservations <- function(dataset, observations.target, observations.grouping=NULL, observations.groupingLevel=NULL, observations.lessThan, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  for (v in observations.target) {
    
    if (length(observations.grouping) > 0) { # there are grouping vars
      
      if (length(observations.groupingLevel) > 0) { # We only need to check a specific level
        expr <- paste(observations.grouping, observations.groupingLevel, sep='==', collapse='&') # Create a subset expression
        df <- subset(dataset, eval(parse(text=expr)))
        obs <- length(na.omit(df[[.v(v)]]))
      } else {
        # ddply groups the dataset and performs a function on each subgroup, returns a dataframe with the subgroups and the function results
        obs <- plyr::ddply(dataset, .v(observations.grouping), function(df, target) length(na.omit(df[[target]])), .v(v))
      }
      
    } else { # no grouping vars
      
      obs <- length(na.omit(dataset[[.v(v)]]))
      
    }
    
    if ((is.data.frame(obs) && any(obs[, ncol(obs)] < observations.lessThan)) || # Check the last column with the function results
        (is.numeric(obs) && obs < observations.lessThan)) { # Or the scalar
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  
  return(result)
}