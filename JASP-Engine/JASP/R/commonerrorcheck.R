# Function to gracefully exit an analysis when continuing to run is nonsensical.
# Comparable to stop(message), except does not raise an exception.
.quitAnalysis <- function(message) {
  e <- structure(class = c('expectedError', 'error', 'condition'),
                 list(message=message, call=sys.call(-1)))
  stop(e)
}

# Adds a stacktrace when an exception is encountered.
# Includes up to the latest 10 system calls before the analysis encountered an error.
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


# Generic function to create an error message (mostly used with .hasErrors()).
# Args:
#   type: String containing check type.
#   variables: Vector of dependent variables/single string indicating what did not pass the check.
#   grouping: Vector of grouping variables/single string indicating what the dependents were grouped on.
#   includeOpening: Boolean, should there be a general opening line (TRUE) or only the one-line, specific error message (FALSE).
#   concatenateWith: String, include if you want to append the error message to an already existing message [Should only be used from hasErrors()].
#   ...: Each error message can have any number of variables, denoted by {{}}'s. Add these as arg=val pairs.
#
# Returns:
#   String containing the error message.
.generateErrorMessage <- function(type, variables=NULL, grouping=NULL, includeOpening=FALSE, concatenateWith=NULL, ...) {
  
  if (length(type) == 0) {
    stop('Non-valid type argument provided')
  }
  
  replaceInMessage <- list('!=' = '≠', '==' = '=')
  args <- list(...)
  
  message <- .messages('error', type)
  if (is.null(message)) {
    stop('Could not find error message for "', type, '"')
  }
  
  # See if we need to add a grouping line
  if (length(grouping) > 0) {
    message <- paste(message, .messages('error', 'grouping'))
    message <- gsub('{{grouping}}', paste(grouping, collapse=', '), message, fixed=TRUE)
  }
  
  # See if we need to specify variables
  if (grepl('{{variables}}', message, fixed=TRUE)) {
    if (is.null(variables)) {
      stop('This error message requires the offending variables to be specified')
    }
    message <- gsub('{{variables}}', paste(variables, collapse=', '), message, fixed=TRUE)
  }
  
  # Find all {{string}}'s that needs to be replaced by values
  toBeReplaced <- regmatches(message, gregexpr("(?<=\\{{)\\S*?(?=\\}})", message, perl=TRUE))[[1]]
  if (base::identical(toBeReplaced, character(0)) == FALSE) { # Were there any {{string}}'s?
    if (all(toBeReplaced %in% names(args)) == FALSE) { # Were all replacements provided in the arguments?
      missingReplacements <- toBeReplaced[!toBeReplaced %in% names(args)]
      stop('Missing required replacement(s): "', paste(missingReplacements, collapse=','), '"')
    }
    for (i in 1:length(toBeReplaced)) {
      value <- args[[ toBeReplaced[i] ]]
      if (length(value) > 1) { # Some arguments may have multiple values, e.g. c('< 3', '> 5000')
        value <- paste(value, collapse=' or ')
      }
      message <- gsub(paste0('{{', toBeReplaced[i], '}}'), value, message, fixed=TRUE)
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
#        E.g. to perform a check only on certain variables instead of all, include a target (e.g. variance.target=options$dependent).
#
# Returns:
#   FALSE if no errors were found or a named list specifying for each check which variables violated it as well as a general error message.
.hasErrors <- function(dataset, perform, type, message='default', exitAnalysisIfErrors=FALSE, ...) {
  
  if (is.null(dataset) || perform != 'run' || length(type) == 0) {
    return(FALSE)
  }
  
  if (exitAnalysisIfErrors && message == 'short') {
    message <- 'default'
  }
  
  # Error checks definition
  checks <- list()
  checks[['infinity']] <- list(callback=.checkInfinity, addGroupingMsg=FALSE)
  checks[['factorLevels']] <- list(callback=.checkFactorLevels)
  checks[['variance']] <- list(callback=.checkVariance, addGroupingMsg=TRUE)
  checks[['observations']] <- list(callback=.checkObservations, addGroupingMsg=TRUE)
  
  args <- list(...)
  errors <- list(message=NULL)
  
  for (i in 1:length(type)) {
    
    # See if the specified check exists
    check <- checks[[ type[[i]] ]]
    if (is.null(check)) {
      stop('Unknown check type provided: "', type[[i]], '"')
    }

    # Obtain the arguments this specific callback uses, attach the check specific prefix
    funcArgs <- base::formals(check[['callback']])
    funcArgs[c('dataset', '...')] <- NULL
    names(funcArgs) <- paste0(type[[i]], '.', names(funcArgs))
    
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
    optArgs <- list()
    reqArgs <- list()
    for (a in 1:length(funcArgs)) {
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
    
    # Call the function with arguments sanse the prefix (e.g. variance.target = target)
    callingArgs <- c(list(dataset=dataset), args)
    names(callingArgs) <- gsub(paste0(type[[i]], '.'), '', names(callingArgs), fixed=TRUE)
    checkResult <- base::do.call(check[['callback']], callingArgs)
    
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
        if (!is.null(check[['addGroupingMsg']]) && !is.null(args[[ paste0(type[[i]], '.grouping') ]]) && check[['addGroupingMsg']] == TRUE) {
          grouping <- args[[ paste0(type[[i]], '.grouping') ]]
        }
        
        errors[['message']] <- base::do.call(.generateErrorMessage, 
                                             c(list(type=type[[i]], variables=varsToAdd, grouping=grouping, includeOpening=opening, concatenateWith=errors[['message']]), args))
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

# Convenience function to apply a check on a specific level of the dependent, or on all subgroups.
# Args:
#   func: Function to perform on the subgroup(s).
#   dataset: JASP dataset.
#   target: String: single dependent variable
#   grouping: Vector of strings or single string indicating the grouping variables
#   levels: Vector of strings/numerics or single string/numeric indicating the level of each of the grouping variables
#
# Returns:
#   Result of the func in vector form when no levels were supplied, otherwise as a single value.
.applyOnGroups <- function(func, dataset, target, grouping, levels=NULL) {
  
  if (length(levels) > 0) {
    
    if (length(grouping) != length(levels)) {
      stop('Each grouping variable must have a level specified')
    }
    
    # The levels vector may be a 'mix' of numeric and characters, we need to add additional quotation marks around characters
    if (is.character(levels)) {
      levels <- vapply(levels, function(x) {
        if (suppressWarnings(is.na(as.numeric(x)))) {
          paste0("\"", x, "\"")
        } else {
          x
        }
      }, character(1))
    }
    
    # Subset based on an expression
    expr <- paste(.v(grouping), levels, sep='==', collapse='&')
    dataset <- subset(dataset, eval(parse(text=expr)))
    result <- func(dataset[[.v(target)]])
    
  } else {
    
    result <- plyr::ddply(dataset, .v(grouping), function(data, target) func(data[[.v(target)]]), target)
    result <- result[[ncol(result)]] # The last column holds the func results
    
  }
  
  return(result)
}


# Check for infinity, optionally specify grouping with levels (makes no sense to perform a grouped search without levels)
.checkInfinity <- function(dataset, target, grouping=NULL, groupingLevel=NULL, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  findInf <- function(x) {
    return(any(is.infinite(x)))
  }
  
  for (v in target) {
    
    if (is.factor(dataset[[.v(v)]])) { # Coerce factor to numeric
      dataset[[.v(v)]] <- as.numeric(as.character(dataset[[.v(v)]]))
    } 
    
    if (length(grouping) > 0 && length(groupingLevel) > 0) { # Should the data be grouped?
      hasInf <- .applyOnGroups(findInf, dataset, v, grouping, groupingLevel)
    } else {
      hasInf <- findInf(dataset[[.v(v)]])
    }
    
    if (hasInf) {
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


# Check for the amount of factor levels, required argument is an amount (string) to check (e.g. '!= 2')
.checkFactorLevels <- function(dataset, target, amount, ...) {
  result <- list(error=FALSE, errorVars=NULL)

  for (v in target) {
    
    levelsOfVar <- length(unique(dataset[[.v(v)]]))
    for (amount in amount) {
      expr <- paste(levelsOfVar, amount) # Build the expression to check for
      if (eval(parse(text=expr))) { # See if this expression is true
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
        break
      }
    }
    
  }
  return(result)
}


# Check for variance in the data. Optionally specify the variance amount which equates an error and grouping (possibly with levels)
.checkVariance <- function(dataset, target, grouping=NULL, groupingLevel=NULL, equalTo=0, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  getVariance <- function(x) {
    validValues <- x[is.finite(x)]
    variance <- -1 # Prevents the function from returning NA's
    if (length(validValues) > 1) {
      variance <- stats::var(validValues)
    }
    return(variance)
  }
  
  for (v in target) {
    
    if (length(grouping) > 0) { # There are grouping vars
      variance <- .applyOnGroups(getVariance, dataset, v, grouping, groupingLevel)
    } else {
      variance <- getVariance(dataset[[.v(v)]])
    }
    
    if (any(variance == equalTo)) { # Check the function results
      result$error <- TRUE
      result$errorVars <- c(result$errorVars, v)
    }
    
  }
  return(result)
}


# Check for the number of observations in the dependent(s). Required argument is an amount (string) to check (e.g. '< 1'), optionally grouping (possibly with levels)
.checkObservations <- function(dataset, target, grouping=NULL, groupingLevel=NULL, amount, ...) {
  result <- list(error=FALSE, errorVars=NULL)
  
  getObservations <- function(x) {
    return(length(na.omit(x)))
  }
  
  for (v in target) {
    
    if (length(grouping) > 0) { # There are grouping vars
      obs <- .applyOnGroups(getObservations, dataset, v, grouping, groupingLevel)
    } else {
      obs <- getObservations(dataset[[.v(v)]])
    }
    
    for (amount in amount) {
      expr <- paste(obs, amount) # Build the expression to check for
      if (any(sapply(expr, function(x) eval(parse(text=x))))) { # See if any of the expressions is true
        result$error <- TRUE
        result$errorVars <- c(result$errorVars, v)
        break
      }
    }
    
  }
  return(result)
}


.checkInputs <- function(inputs.group1, inputs.group2=NULL, inputs.operator, ...) {
  result <- list(error=FALSE)
  
  
  return(result)
}