# Generic function to create error message (mostly used in conjunction with .hasErrors()).
# Args:
#   type: String containing check type.
#   includeOpening: Boolean, should there be a general opening line (TRUE) or only the specific error message (FALSE).
#   concatenateWith: String, include if you want to append the error message to an already existing message.
#   ...: Each error message can have any number of variables, denoted by {}'s. Add these as arg=val pairs.
#
# Returns:
#   String containing the error message.
.generateErrorMessage <- function(type, variables=NULL, includeOpening=TRUE, concatenateWith=NULL, ...) {
  
  if (length(type) == 0) {
    stop('Non-valid type argument provided')
  }
  
  replaceInMessage <- list('!=' = '≠')
  args <- list(...)
  
  message <- .messages('error', type)
  if (is.null(message)) {
    stop('Could not find error message for "', type, '"')
  }
  
  # See if we need to specify variables
  if (grepl('{variables}', message, fixed=TRUE)) {
    if (is.null(variables)) {
      stop('This error message requires the offending variables to be specified')
    }
    message <- gsub('{variables}', paste(variables, collapse=', '), message, fixed=TRUE)
  }
  
  # Find all {string}'s that needs to be replaced by values
  toBeReplaced <- regmatches(message, gregexpr("(?<=\\{)\\S*?(?=\\})", message, perl=TRUE))[[1]]
  if (base::identical(toBeReplaced, character(0)) == FALSE) { # see if there were any {string}'s
    if (all(toBeReplaced %in% names(args)) == FALSE) {
      missingReplacements <- toBeReplaced[!toBeReplaced %in% names(args)]
      stop('Missing required replacement(s): "', paste(missingReplacements, collapse=','), '"')
    }
    for (i in 1:length(toBeReplaced)) {
      message <- gsub(paste0('{', toBeReplaced[i], '}'), args[[ toBeReplaced[i] ]], message, fixed=TRUE)
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
    if (endOfString == '</ul>') {
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
#   ...: Each check may have required and optional arguments, they are specified in the error check subfunctions.
#        To perform the check only on certain variables instead of all, include a target (e.g. infinity.target=options$dependent, variance.target...).
#
# Returns:
#   FALSE if no errors were found or a list specifying for each check which variables violated it as well as a general error message.
.hasErrors <- function(dataset, perform, type, message='default', ...) {
  
  if (is.null(dataset) || perform != 'run' || length(type) == 0) {
    return(FALSE)
  }
  
  # Error checks definition
  checks <- list()
  checks[['infinity']] <- list(target='infinity.target', callback=.checkInfinity)
  checks[['factorLevels']] <- list(target='factorLevels.target', callback=.checkFactorLevels)
  checks[['variance']] <- list(target='variance.target', callback=.checkVariance)
  checks[['observations']] <- list(target='observations.target', callback=.checkObservations)
  
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
    exclude <- c('dataset', 'targetVars', '...')
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
    
    # Find the appropriate variables to perform the check on
    variables <- .unv(names(dataset))
    if (!is.null(check[['target']]) && !is.null(args[[ check[['target']] ]])) {
      variables <- args[[ check[['target']] ]]
    }
    
    checkResult <- base::do.call(check[['callback']], c(list(dataset=dataset, targetVars=variables), args))
    
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
        errors[['message']] <- base::do.call(.generateErrorMessage, c(list(type=type[[i]], variables=varsToAdd, includeOpening=opening, concatenateWith=errors[['message']]), args))
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
  return(errors) 
}


# Check for infinity, requires no additional arguments.
.checkInfinity <- function(dataset, targetVars, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  for (v in targetVars) {
    data <- dataset[[.v(v)]]
    if (is.factor(data)) { # coerce factor to numeric
      data <- as.numeric(as.character(data))
    } 
    if (any(is.infinite(data))) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


# Check for the amount of factor levels, required arguments are an amount (numeric) to check and its operator (string) (e.g. '!=' 2 or '>' 1)
.checkFactorLevels <- function(dataset, targetVars, factorLevels.amount, factorLevels.operator, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  for (v in targetVars) {

    levelsOfVar <- length(unique(dataset[[.v(v)]]))
    expr <- paste(levelsOfVar, factorLevels.operator, factorLevels.amount) # Build the expression to check for
    if (eval(parse(text=expr)) == TRUE) { # See if this expression is true
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


# Check for variance in the data. Optionally specify the variance amount to check and if the dependent should be grouped.
.checkVariance <- function(dataset, targetVars, variance.equalTo=0, variance.grouping=NULL, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  for (v in targetVars) {
    
    if (length(variance.grouping) > 0) { # There are grouping vars
      # ddply groups the dataset and performs a function on each subgroup, returns a dataframe with the subgroups and the function results
      variances <- plyr::ddply(dataset, .v(variance.grouping),
                               function(x, col) {
                                 validValues <- x[[col]][is.finite(x[[col]])]
                                 variance <- -1 # Prevents the function from returning NA's
                                 if (length(validValues) > 1) {
                                   variance <- stats::var(validValues)
                                 }
                                 return(variance)
                               }, .v(v))
      if (any(variances[, ncol(variances)] == variance.equalTo)) { # check the last column with the function results
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
      }
      
    } else { # no grouping vars
      
      validValues <- dataset[[.v(v)]][is.finite(dataset[[.v(v)]])]
      if (length(validValues) > 1) {
        variance <- stats::var(validValues)
        if (variance == variance.equalTo) {
          result$error <- TRUE
          result$errorVars <- c(result$errorVars, v)
        }
      }
      
    }
    
  }
  return(result)
}


# check for the number of observations in the dependent(s). Requires the minimum possible value to be supplied, optionally specify if the dependent should be grouped.
.checkObservations <- function(dataset, targetVars, observations.lessThan, observations.grouping=NULL, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  for (v in targetVars) {
    
    if (length(observations.grouping) > 0) { # there are grouping vars
      # ddply groups the dataset and performs a function on each subgroup, returns a dataframe with the subgroups and the function results
      obs <- plyr::ddply(dataset, .v(observations.grouping),
                         function(x, col) {
                           length(na.omit(x[[col]]))
                         }, .v(v))
      if (any(obs[, ncol(obs)] < observations.lessThan)) { # check the last column with the function results
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
      }
      
    } else if (length(na.omit(dataset[[.v(v)]])) < observations.lessThan) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}